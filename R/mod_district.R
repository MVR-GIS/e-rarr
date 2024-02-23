#' district_mod UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
district_ui<- function(id){
  ns <- NS(id)
  tagList(
    selectizeInput(
    inputID=ns('districtInput'),
    label="District Choice",
    choices = NULL,
    selected = NULL,
    multiple = F,
    options=list(placeholder = 'Select a District', 
                 maxOptions = 40)
  )
  )
}
    
#' district_input_mod Server Functions
#'
#' @noRd 
district_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    num <- reactive({
      data = RiskImpactTable$USACE_ORGANIZATION
      return(data)
    })
    observe({
      updateSelectizeInput(session=session,inputID='districtInput',
                           choices =c("", sort(unique(num()))), 
                           options=list(maxOptions = 40
                                        ,server = TRUE,placeholder = 'Select a District' ))
    })
    
  })
}
    




## To be copied in the UI
# mod_district_input_mod_ui("district_input_mod_1")
    
## To be copied in the server
# mod_district_input_mod_server("district_input_mod_1")
