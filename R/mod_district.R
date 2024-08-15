#' district_mod UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS selectizeInput
#' 

district_ui<- function(id){
  ns<-NS(id)
    selectizeInput(ns('districtInput'),
    label="District Choice",
    choices = NULL,
    selected = NULL,
    multiple = F,
    options=list(placeholder = 'Select a District', 
                 maxOptions = 40)
  )
}
    
#' district_input_mod Server Functions
#'
#' @noRd 
#' 
# district_server <- function(id, org){
#   moduleServer(id, function(input, output, session){
#     num <- reactive({
#       data = org
#       return(data) 
#     }) 
#     observe({
#       updateSelectizeInput(session,inputID='districtInput',
#                            choices =c("", sort(unique(num()))), 
#                            options=list(maxOptions = 40
#                                         ,server = TRUE,placeholder = 'Select a District' ))
#     })
#   })
# }
#     




## To be copied in the UI
# district_ui("district_input_mod_1")
    
## To be copied in the server
# district_server("district_input_mod_1")
