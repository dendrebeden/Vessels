# dropDownCascadeUI <- function(id) {
#   ns <- NS(id)
#   uiOutput(ns("vesselNameDropdown"))
# }
# 
# dropDownCascadeServer <- function(id, vessel_name_type_dt) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       ns <- session$ns
# 
#       output$vesselNameDropdown <- renderUI({
#         grid(
#           grid_template(default = list(
#             areas = rbind(c("vesselTypeDropdown", "vesselNameDropdown")),
#             rows_height = c("auto", "auto"),
#             cols_width = c("50%", "50%"))),
#           area_styles = list(vesselTypeDropdown = "padding-right: 5%; padding-left: 1%;",
#                              vesselNameDropdown = "padding-left: 5%; padding-right: 1%;"),
#           vesselTypeDropdown = uiOutput("vesselTypeDropdown"),
#           vesselNameDropdown = uiOutput("vesselNameDropdown")
#         )
#       })
#       
#       output$vesselTypeDropdown <- renderUI({
#         dropDownUI("vesselType", "Select vessel type:")
#       })
#       output$vesselNameDropdown <- renderUI({
#         dropDownUI("vesselName", "Select vessel name:")
#       })
#       
#       vesselTypeSelected <- dropDownServer("vesselType", vesselTypeValues)
#       vesselNameSelected <- dropDownServer("vesselName", vesselNameValues)
#       
#       vesselTypeValues <- reactive({
#         shipsTypeNameDT()$SHIP_TYPE %>% unique() %>% sort()
#       })
#     }
#   )
# }

# dropdown with header
dropDownUI <- function(id, header) {
  ns <- NS(id)
  div(
    class = "content",
    strong(header),
    uiOutput(ns("dropdown"))
  )
}

# dropDown module server
# input:
#  - choices   - character   - character vector with values for dropdown
#
# output:
#  selected dropdown value wrapped in reactive(...)
dropDownServer<- function(id, choices) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$dropdown <- renderUI({
        dropdown_input(input_id = ns("dropdown"),
                       choices = if(length(choices()) > 0) choices() else character(),
                       value = isolate(if(!is.null(input$dropdown)) input$dropdown)
        )
      })
      return(reactive(input$dropdown))
    })
}
