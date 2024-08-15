#' Sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Sidebar_ui <- function(id){
  ns <- NS(id
           )
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        ns('districtInput'),
        "District",
        choices = NULL,
        selected = NULL,
        multiple = F,
        options=list(placeholder = 'Select a District', 
                     maxOptions = 40)
      ),
      selectizeInput(
        ns("projectInput"), 
                     "Project", 
                     choices = NULL,
                     selected = NULL,
                     multiple = F,
                     options=list(placeholder = 'Select a Project')),
      h6("or"),
      selectizeInput(
        ns("P2Input"),
        "P2 Number",
        choices = NULL,
        selected = NULL,
        multiple = F,
        options=list(placeholder = 'Enter P2 Number')
      ),
      shinyjs::hidden(
        selectizeInput(
          ns("SubIDInput"),
          "Sub ID",
          choices = NULL,
          selected = NULL,
          multiple = FALSE,
          options =  list(placeholder = "Select SubID")
        )),
      conditionalPanel(condition = "input.reporttabs == 'Reports'",
                       selectizeInput(
                         ns("riskInput"),
                         "Risk",
                         choices = NULL ,
                         selected = NULL,
                         multiple = F,
                         options=list(placeholder = 'Select a Risk Item')
                       )),
      conditionalPanel(condition = "input.reporttabs == 'Explore'",
                       selectizeInput(
                         ns("phaseInput"),
                         "Phase",
                         choices = NULL,
                         selected = NULL,
                         multiple = F,
                         options=list(placeholder = 'Enter Phase')),
                       shinyjs::hidden(
                         selectizeInput(
                           ns("mileInput"),
                           "Milestone",
                           choices = NULL,
                           selected = NULL,
                           multiple = F,
                           options=list(placeholder = 'Enter Milestone'))),
                       selectizeInput(
                         ns("disInput"),
                         "Discipline",
                         choices = NULL,
                         selected = NULL,
                         multiple = F,
                         options=list(placeholder = 'Select a discipline')
                       )
      ), width=2)
  )
  }
  
  

    

#' Sidebar Server Functions
#'
#' @noRd 
mod_Sidebar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Sidebar_ui("Sidebar_1")
    
## To be copied in the server
# mod_Sidebar_server("Sidebar_1")
