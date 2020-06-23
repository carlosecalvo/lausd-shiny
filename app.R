# LAUSD COVID19 Resource Mapping - RAND Corporation
# Carlos Calvo-Hernandez, ccalvo@rand.org
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(glue)
library(here)
library(shinycssloaders)
library(DT)


## Check github.com/bborgesr/employee-dir
## "cats" has to be a named list

cats <- list("Food" = "Food Resources", 
             "Housing" = "Housing", 
             "Financial" = "Financial", 
             "Physical Health" = c("Health Care", "Hygiene"), 
             "Mental Health" = "Mental Health", 
             "Legal" = c("Legal", "Undocumented", "Domestic Violence Human Trafficking"), 
             "Educational" = c("Educational", "FamilySource"), 
             "Student Enrollment" = "YouthSource",
             "Diversity" = "LGBTQ",
             "Other" = c("District Resources", "Other", "Foster Youth", "Internet and Utility")
)

# Be careful how the categories are structured. Only change the assignment on the object "cats" above.

# To add a new category to the map: Add the new category to the data csv file under the partner_type column and add the following code in lines 37 and 38 to line 51 and 52, where "new_category" is the formatted string name of the new category (i.e. "Food" for food, etc.)

# %>% 
# mutate(category = if_else(partner_type == "new_category", "new_category", category))

resources <- read_tsv(glue(here(),"/data/resources_full.csv")) %>% 
  mutate(category = if_else(partner_type %in% cats$Food, names(cats[1]),
                            if_else(partner_type %in% cats$Housing, names(cats[2]),
                            if_else(partner_type %in% cats$Financial, names(cats[3]),
                            if_else(partner_type %in% cats$`Physical Health`, names(cats[4]),
                            if_else(partner_type %in% cats$`Mental Health`, names(cats[5]),
                            if_else(partner_type %in% cats$Legal, names(cats[6]),
                            if_else(partner_type %in% cats$Educational, names(cats[7]),
                            if_else(partner_type %in% cats$`Student Enrollment`, names(cats[8]),
                            if_else(partner_type %in% cats$Diversity, names(cats[9]),
                            if_else(partner_type %in% cats$Other, names(cats[10]), "Other"))))))))))) %>% 
  mutate(district = if_else(district == "various", "Various", district))

# This subsets the data for mapping the physical resources
resources_map <- resources %>% 
  filter(!(is.na(lat)))

temp <- unique(resources$category) # object to keep track of the category formatted names

# Default information when no category is selected in the "Data Selector" checkbox.
lausd <- tibble(name = "LAUSD Dept. of Student Health and Human Services",
                address = "333 S. Beaudry Ave., 29th Floor",
                city = "Los Angeles",
                zip = 90017,
                contact_link = "https://achieve.lausd.net/Page/11883",
                phone = "(213) 241-3840",
                district = "ALL",
                lon = -118.2573,
                lat = 34.05614,
                category = "All") 

####### UI ######
ui <- fluidPage(theme = shinytheme("cosmo"), # You can change the theme here


  navbarPage("LAUSD Resource Dashboard", id = "nav", 

        # This is the tab page for the "Interactive Map"
        tabPanel("Interactive Map",
                 
                 div(class = "outer",
                     tags$head(
                         includeCSS("www/styles.css"), # this loads the CSS script used for format
                         includeScript("www/gomap.js") # This loads JS script for panning the map
                         ),
          # Show a plot of the generated map
           leafletOutput("map", width = "100%", height = "100%"),
          # Code for the panels in the "Interactive Map" tab
            # First the "Data Selector" checkbox with the categories
           absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                         draggable = TRUE, top = "10%", left = "auto", right = "2%", bottom = "auto",
                         width = "15%", height = "auto",

                         h2("Data Selector"),
                         h5("Select one or more types of resources you wish to see."),
                         h6(HTML("<br/>")),

                         
                         checkboxGroupInput("checkbox", label = NULL, choices = temp[order(temp)], selected = NULL)
                        ),
            # Second, the "Offline-only Resources" table
           absolutePanel(id = "offline", class = "panel panel-default", fixed = FALSE,
                         draggable = TRUE, top = "auto", left = "2%", right = "auto", bottom = "5%",
                         width = "25%", height = "auto", 
                         
                         h2(" Online-only Resources"),
                         DT::dataTableOutput("online")
                        ),
          
            # Bottom left note on map canvas
           tags$div(id="cite",
                    'Data compiled and map created for ', tags$em('Los Angeles Unified School District')#, ' by RAND Corporation. 2020'
                    )
          )
        ),
        # This is the tab page for the "Data Explorer"
        tabPanel("Data Explorer",
                 # Input selectors at the top of the page
                 fluidRow(
                   column(3,
                          selectInput("source", "Category", c("All Categories" = "", structure(temp[order(temp)])), multiple=TRUE)
                   ),
                   column(3,
                          selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
                          
                   ),
                   column(3,
                          
                          selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                          
                   ),
                   column(3,
                          
                          selectInput("district", "Local District", c("All Districts"= ""), multiple = TRUE))
                 ),
                 hr(),
                 withSpinner(DT::dataTableOutput("explorer"))
        ),
        
        conditionalPanel("false", icon("crosshair"))

                 
                 
                 )
    )


####### Server #######
server <- function(input, output, session) {
    
    # Loading modal to keep user out of trouble while map draws...
    showModal(modalDialog(title="MAP LOADING - PLEASE WAIT...","Please wait for map to draw before proceeding.",size="l",footer=NULL))
    
    # Remove modal when app is ready
    observe({
        req(map, resources, resources_map)
        removeModal()
    })

    ####### Leaflet map ##########
    # This section of the server logic controls the functionality of the tab page "Interactive Map"
    output$map <- renderLeaflet({
        leaflet(resources_map) %>%
            setView(lng = -118.31, lat = 34, zoom = 9.5) %>%
            addProviderTiles("CartoDB.VoyagerLabelsUnder", options = providerTileOptions(noWrap = TRUE))%>% 
        addEasyButton(easyButton( # this creates the "locate me" button on the map canvas
          icon="fa-crosshairs", title = "Locate me", 
          onClick=JS("function(btn, map){map.locate({setView: true, enableHighAccuracy: true})}")))
    })
    
    # Pan map 
    optionchosen <- reactive({
        if(is.null(input$map_bounds))
            return(resources_map[FALSE,])
        bounds <- input$map_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(resources,
               lat >= latRng[1] & lat <= latRng[2] &
                lon >= lngRng[1] & lon <= lngRng[2])
    })
    
    # Icons for map markers. See more at https://rstudio.github.io/leaflet/markers.html Section: Awesome Icons
    
    iconSet <- awesomeIconList(
      Food = makeAwesomeIcon(icon= 'glyphicon-cutlery', markerColor = 'lightred', iconColor = 'lightgray', library = 'glyphicon'),
      Housing = makeAwesomeIcon(icon= 'glyphicon-home', markerColor = 'red', iconColor = 'white', library = 'glyphicon'),
      Financial = makeAwesomeIcon(icon= 'glyphicon-usd', markerColor = 'darkgreen', iconColor = 'white', library = 'glyphicon'),
      `Physical Health` = makeAwesomeIcon(icon= 'medkit', markerColor = 'blue', iconColor = 'white', library = 'fa'),
      `Mental Health` = makeAwesomeIcon(icon= 'glyphicon-cloud', markerColor = 'beige', iconColor = 'white', library = 'glyphicon'),
      Legal = makeAwesomeIcon(icon= 'glyphicon-folder-open', markerColor = 'black', iconColor = 'white', library = 'glyphicon'),
      Educational = makeAwesomeIcon(icon= 'glyphicon-blackboard', markerColor = 'purple', iconColor = 'black', library = 'glyphicon'),
      `Student Enrollment` = makeAwesomeIcon(icon= 'glyphicon-education', markerColor = 'cadetblue', iconColor = 'white', library = 'glyphicon'),
      Diversity = makeAwesomeIcon(icon= 'universal-access', markerColor = 'white', iconColor = 'white', library = 'fa'),
      Other = makeAwesomeIcon(icon= 'glyphicon-asterisk', markerColor = 'lightgray', iconColor = 'white', library = 'glyphicon'),
      All = makeAwesomeIcon(icon= 'bullseye', markerColor = 'lightblue', iconColor = 'white', library = 'fa'))
    
    # This controls the behavior of checkbox on "Data Selector"
    observeEvent(input$checkbox, {
      inputData <- if (is.null(input$checkbox)) lausd else {resources_map %>% 
        filter(category %in% input$checkbox) 
      }
      
      
    # Change markers as map pans
    leafletProxy("map", data = inputData) %>% 
        clearMarkers() %>% 
        addAwesomeMarkers(icon = ~iconSet[category], lng = ~lon, lat = ~lat, 
                          popup = ~(glue("<b>{name}</b>", "<br/>", 
                                         "{address}, ", "{city} ", "{zip}", "<br/>", 
                                "<b>Website:</b> <a href={contact_link}>{contact_link}</a>", 
                                "<br/>", "Telephone: {phone}, ", 
                                "Local District: {district}", "<br/>", 
                                "Resource: {inputData$category}")), 
                          label = inputData$name) 
    # Can add clusterOptions = markerClusterOptions() to cluster markers when zooming out
}, ignoreNULL = F)
    
    # Show a popup at the given location
    showZipcodePopup <- function(id, lat, lng) {
      selectedZip <- resources_map[resources_map$id == id,]
      content <- glue("<b>{selectedZip$name}</b>", "<br/>", 
                      "{selectedZip$address}",  "{selectedZip$city}", "{selectedZip$zip}","<br/>", 
                      "<b>Website:</b> <a href={selectedZip$contact_link}>{selectedZip$contact_link}</a>", 
                      "<br/>", "Telephone: {selectedZip$phone}")
      leafletProxy("map") %>% addMarkers(lng, lat, popup = content) %>% 
        addPopups(lng, lat, content)
    }
    
    observe({# No use at this moment. Expected to have a clickable column on "Data Explorer" that could take you to the map and select the popup from the selected observation
      leafletProxy("map") %>% clearPopups()
      event <- input$map_shape_click
      if (is.null(event))
        return()
      
      isolate({
        showZipcodePopup(event$id, event$lat, event$lng)
      })
    })
    
   ######## Data Explorer ##########
    # This section of the server logic controls the functionality of the tab page "Data Explorer"
    observe({# this observer controls the selector "Cities". This is dependent on a "Category" being selected
      cities <- if (is.null(input$source)) character(0) else {
        filter(resources, category %in% input$source) %>%
          `$`('city') %>%
          unique() %>%
          sort()
      }
      stillSelected <- isolate(input$cities[input$cities %in% cities])
      updateSelectizeInput(session, "cities", choices = cities,
                           selected = stillSelected, server = TRUE)
    })
    
    observe({# this observer controls the selector "Zip code". Same as "Cities"
      zipcodes <- if (is.null(input$source)) character(0) else {
        resources %>%
          filter(category %in% input$source,
                 is.null(input$cities) | city %in% input$cities) %>%
          `$`('zip') %>%
          unique() %>%
          sort()
      }
      stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
      updateSelectizeInput(session, "zipcodes", choices = zipcodes,
                           selected = stillSelected, server = TRUE)
    })
    
    observe({# this observer controls the selector "Zip Code". Same as "Cities"
      district <- if (is.null(input$source)) character(0) else {
        resources %>%
          filter(category %in% input$source,
                 is.null(input$cities) | city %in% input$cities) %>%
          `$`('district') %>%
          unique() %>%
          sort()
      }
      stillSelected <- isolate(input$district[input$district %in% district])
      updateSelectizeInput(session, "district", choices = district,
                           selected = stillSelected, server = TRUE)
    })
    
    observe({# Not in use
      if (is.null(input$goto))
        return()
      isolate({
        map <- leafletProxy("map")
        map %>% clearMarkers()
        dist <- 1
        zip <- input$goto$zip
        lat <- input$goto$lat
        lng <- input$goto$lng
        showZipcodePopup(zip, lat, lng)
        map %>% 
          fitBounds(lng - dist, lat - dist, lng + dist, lat + dist) %>% 
          addAwesomeMarkers()
      })
    })
    # Server logic to subset the data according to the selectors at the top of the page
    output$explorer <- DT::renderDataTable({
      df <- resources %>%
        filter(
          is.null(input$source) | category %in% input$source,
          is.null(input$cities) | city %in% input$cities,
          is.null(input$zipcodes) | zip %in% input$zipcodes
        ) %>%
        #mutate(Action = if_else(!(is.na(lat)), paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', lon, '" data-id="', id, '"><i class="fa fa-crosshairs"></i></a>', sep=""), "NA")) %>% 
        mutate(Website = glue("<a href='{contact_link}' target='_blank'>{contact_link}</a>")) %>% 
        select(Category = category,
               Organization = name, 
               `Partner Type` = partner_type,
               Address = address,
               City = city,
               `Zip Code` = zip,
               Telephone = phone,
               Website,
               `Online only` = online)
      
      action <- dataTableAjax(session, df, outputId = "explorer")
      
      datatable(df, options = list(ajax = list(url = action)), escape = FALSE) %>% 
        formatStyle(1:2, fontWeight = 'bold')
    })
    
    ####### Online resources table ####
    # Server logic for "Online-only Resources" table in "Interactive Map" tab panel. This responds to the checkbox selctor on "Data Selector" by subsetting the "resources" dataframe.
    output$online <- DT::renderDataTable({if (is.null(input$checkbox)) lausd %>% 
        mutate(Website = glue("<a href='{contact_link}' target='_blank'>{contact_link}</a>")) %>% 
        select(Organization = name,
               Website,
               Address = address) %>% # The only fields shown for LAUSD data at this point are Organization, Website, and Address. You can add more inside the select function by calling variables from the lausd dataframe from the top of the script.
        datatable(escape = F) %>% 
        formatStyle(1:2, fontWeight = 'bold')
      else { 
      onlineOnly <- resources %>% 
        filter(online == "yes" & category %in% input$checkbox ) %>%
        mutate(Website = glue("<a href='{contact_link}' target='_blank'>{contact_link}</a>")) %>% 
        select(Category = category,
               Organization = name,
               Website 
               ) %>% 
        datatable(escape = F) %>% 
        formatStyle(1:2, fontWeight = 'bold')}
      })
}
# Run the application 
shinyApp(ui = ui, server = server)
