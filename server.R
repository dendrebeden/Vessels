source("modules.R")
source("functions.R")

shinyServer(function(input, output, session) {
  
  # render for the main page grid
  output$mainPage <- renderUI({
    
    req(!is.null(shipsDT()))
    
    gridTemplate <- grid_template(
      default = list(
        areas = rbind(c("header", "header", "header"),
                      c("plot", "plot", "vesselNotes"),
                      c("plot", "plot", "vesselNotes")), 
        rows_height = c("auto", "500px", "auto"),
        cols_width = c("auto", "2fr", "1fr")))
    
    segment(
      grid(gridTemplate, 
           container_style = "padding: 2%;",
           area_styles = list(
             header = "display: absolute;",
             plot = "padding-top: 50px;",
             vesselNotes = "padding-top: 50px;"
           ),
           header = uiOutput("header"),
           vesselNotes = uiOutput("vesselNotes"),
           plot = leafletOutput("plot")))
  })
  
  # reactive value to store ships data, can be declared as not reactive 
  # shipsDT <- {...}) since this variable does not depend on any other
  # reactive value.
  # output:
  #  data table with ships data.
  shipsDT <- reactive({
    DT <- fread("./ships.csv")
    # remove unused columns
    DT[, c("port", "date", "SPEED", "COURSE", "HEADING", "ELAPSED", "LENGTH",
           "ROT", "SHIPTYPE", "WIDTH", "L_FORE", "W_LEFT", "DWT", "GT_SHIPTYPE",
           "LEGEND", "week_nb") := NULL]
    # map columns
    setnames(DT,
             c("ship_type", "SHIPNAME", "is_parked", "DATETIME"),
             c("SHIP_TYPE", "SHIP_NAME", "IS_PARKED", "DATE_TIME"))
    DT
  })
  
  # reactive value to store information on ships names and types.
  # output:
  #   data table with character columns SHIP_NAME and SHIP_TYPE
  shipsTypeNameDT <- reactive({
    shipsDT()[, c("SHIP_NAME", "SHIP_TYPE")] %>% unique()
  })
  
  ### The following code on dropdowns functionality could be wrapped in another
  ### (parent) module, which is more elegant approach in this situation
  ### (for example: dropDownCascadeServer("vesselDropdowns, shipsTypeNameDT). But
  ### unfortunately on shiny 1.5.0 version this functionality does not work
  ### properly (implicit calls to xxxOutput not working inside modules).
  ### See https://github.com/rstudio/shiny/issues/2000
  ###
  ##############################################################################
  ################ dropDownCascade module substitution start ###################
  
  # render for header grid
  output$header <- renderUI({
    grid(
      grid_template(default = list(
        areas = rbind(c("vesselTypeDropdown", "vesselNameDropdown")),
        rows_height = c("auto", "auto"),
        cols_width = c("50%", "50%"))),
      area_styles = list(vesselTypeDropdown = "padding-right: 5%; padding-left: 1%;",
                         vesselNameDropdown = "padding-left: 5%; padding-right: 1%;"),
      vesselTypeDropdown = uiOutput("vesselTypeDropdown"),
      vesselNameDropdown = uiOutput("vesselNameDropdown")
    )
  })
  
  ### modules UI calls are better to store in individual renders
  output$vesselTypeDropdown <- renderUI({
    dropDownUI("vesselType", "Select vessel type:")
  })
  output$vesselNameDropdown <- renderUI({
    dropDownUI("vesselName", "Select vessel name:")
  })
  
  # store selected type in vesselTypeSelected
  vesselTypeSelected <- dropDownServer("vesselType", vesselTypeValues)
  # store selected name in vesselNameSelected
  vesselNameSelected <- dropDownServer("vesselName", vesselNameValues)
  
  # reactive value to store information on available ships types
  # output:
  #  character vector with ships types
  vesselTypeValues <- reactive({
    shipsTypeNameDT()$SHIP_TYPE %>% unique() %>% sort()
  })
  
  # reactive value to store information on available ships names for the chosen
  # vessel type vesselTypeSelected(),
  # output:
  #  character vector with ships names
  #
  # eventReactive is used in order to make the variable reactive only on
  # vesselTypeSelected(), not on shipsTypeNameDT(). However, an reactive()
  # with isolate() on shipsTypeNameDT() might be used, but the first solution
  # looks more elegant for me.
  vesselNameValues <- eventReactive(vesselTypeSelected(), ignoreNULL = FALSE, {
    shipsTypeNameDT()[SHIP_TYPE %in% vesselTypeSelected(), SHIP_NAME] %>% unique() %>% sort()
  })
  
  ################ dropDownCascade module substitution end ####################
  ##############################################################################
  
  
  # reactive value to store information on chosen vessel
  # output:
  #  list which contains:
  #   - routeHistory   - data.table - data table with numerical columns
  #                                   LON, LAT, and POSIXct column DATE_TIME
  #   - routeDistance  - numeric    - longest route distance sailed
  #   - routeDuration  - numeric    - longest route duration
  #   - routeStartTime - POSIXct    - longest route start time
  #   - routeEndTime   - POSIXct    - longest route end time
  #   - id             - character  - ship id
  #   - type           - character  - ship type
  #   - name           - character  - ship name
  vesselInformation <- eventReactive(vesselNameSelected(), ignoreNULL = FALSE,{
    selectedVessel <- shipsDT()[SHIP_NAME %in% vesselNameSelected() &
                                  SHIP_TYPE %in%  vesselTypeSelected()]
    vesselInfo <- fCalculateLongestRoute(selectedVessel[
      , c("LAT", "LON", "IS_PARKED", "DATE_TIME")])
    # choose the first value in case of several values in SHIP_ID column
    vesselInfo[["id"]] <- if (nrow(selectedVessel) > 0) {
      selectedVessel$SHIP_ID  %>% as.character() %>% unique() %>% .[1]
    }
    vesselInfo[["type"]] <- vesselTypeSelected()
    vesselInfo[["name"]] <- vesselNameSelected()
    vesselInfo
  })
  
  ### renderText's for output$vesselNotes
  output$vesselTypeText <- renderText(vesselInformation()$type)
  output$vesselNameText <- renderText(vesselInformation()$name)
  output$vesselIdText <- renderText(vesselInformation()$id)
  output$routeDistanceText <- renderText({
    if (length(vesselInformation()$routeDistance) == 0) {
      ""
    } else if (vesselInformation()$routeDistance < 0) {
      "The vessel has not sailed."
    } else {
      paste0(round(vesselInformation()$routeDistance, digits = 2), " meters")
    }
  })
  output$routeDurationText <- renderText(vesselInformation()$routeDuration %>%
                                           seconds_to_period() %>%
                                           as.character())
  output$routeStartTimeText <- renderText(vesselInformation()$routeStartTime %>%
                                            as.character())
  output$routeEndTimeText <- renderText(vesselInformation()$routeEndTime %>%
                                          as.character())
  
  # render for vessel notes
  output$vesselNotes <- renderUI({
    cards(
      class = "one",
      card(
        div(class="content",
            div(class="header", icon("ship"), "Vessel information:"),
            div(class="meta", style = "padding-top: 10px;", "Type:"),
            div(class="description", style = "padding-bottom: 10px;", textOutput("vesselTypeText")),
            div(class="meta", "Name:"),
            div(class="description", style = "padding-bottom: 10px;", textOutput("vesselNameText")),
            div(class="meta", "Id:"),
            div(class="description", textOutput("vesselIdText"))
        )
      ),
      card(
        div(class="content",
            div(class="header", icon("route"), "Longest route information:"),
            div(class="meta", style = "padding-top: 10px;", "Distance sailed (in meters):"),
            div(class="description", style = "padding-bottom: 10px;", textOutput("routeDistanceText")),
            div(class="meta",  "Duration:"),
            div(class="description", style = "padding-bottom: 10px;", textOutput("routeDurationText")),
            div(class="meta", "Start time:"),
            div(class="description", style = "padding-bottom: 10px;", textOutput("routeStartTimeText")),
            div(class="meta", "End time:"),
            div(class="description", textOutput("routeEndTimeText"))
        )
      )
    )
  })
  
  # render for map plot
  output$plot <- renderLeaflet({
    
    req(!is.null(vesselInformation()))
    
    plot <- leaflet(options = leafletOptions(zoomControl = FALSE))  %>%
      addProviderTiles("CartoDB.Positron")
    
    routeDT <- vesselInformation()[["routeHistory"]]
    
    if(!is.null(routeDT)) {
      plot %>%
        # to show a route on the map
        addPolylines(lat = routeDT$LAT,
                     lng = routeDT$LON) %>%
        # add marker for departure point
        addAwesomeMarkers(lat = first(routeDT)$LAT,
                          lng = first(routeDT)$LON,
                          icon =  makeAwesomeIcon(icon = "home", markerColor = "green",
                                                  library = "ion")) %>%
        # add marker for destination point
        addAwesomeMarkers(lat = last(routeDT)$LAT, 
                          lng = last(routeDT)$LON,
                          icon = makeAwesomeIcon(icon = "flag", markerColor = "red", library = "fa",
                                                 iconColor = "black"))
    } else {
      plot %>%
        # set map location on Baltic Sea if no routes where found
        setView(18, 56, zoom = 7)
    }
  })
})
