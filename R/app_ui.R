#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
#' 
#' @importFrom shiny fluidPage navbarPage conditionalPanel mainPanel 
#'                   sidebarPanel tabPanel tabsetPanel sidebarLayout 
#'                   selectizeInput downloadButton
#'                   tag tags tagList div img h6
#' @importFrom shinyjs useShinyjs hidden
#' @importFrom dplyr select mutate filter
#' @importFrom bslib bs_theme layout_column_wrap card card_header card_body 
#'                   tooltip
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
#' @importFrom stringr str_trim

RiskImpactTable <- risk_item_db |>
  select(
    "PROJECT_NAME",
    "RISK_IDENTIFIER",
    "RISK_NAME",
    "USACE_ORGANIZATION",
    "P2_NUMBER",
    "LIFECYCLEPHASENAME",
    "MILESTONE",
    "RISKCATEGORY",
    "DISCIPLINE",
    "P2_SUB_IDENTIFIER"
  ) |>
  mutate(P2_SUB_IDENTIFIER = ifelse(is.na(P2_SUB_IDENTIFIER), "", 
                                    P2_SUB_IDENTIFIER))|>
  mutate(RISK_NAME_ID = paste(RISK_IDENTIFIER,RISK_NAME))|>
  mutate(RISK_NAME_ID =str_trim(RISK_NAME_ID, side = c("right")))




app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    tags$head(tags$style(HTML("
                          body > nav{
                          margin-bottom:0 !important;}
                          body > div.container-fluid{
                          padding:0;}
                          .popover {--bs-popover-max-width: 100%;
                          data-bs-animation: FALSE;}
                          .action-button {border-radius: 12px;}
                          .navbar-header {padding: 20px;}
                          "))),
    fluidPage(
      shinyjs::useShinyjs(),
      theme = bslib::bs_theme(bootswatch = "cosmo", version=5),
      navbarPage(
        title = div(
          img(
            src = "www/castle.png",
            height = "50px",
            width = "60px"
          ),
          "Risk Analysis Reporting System      "
        ),
        tabPanel("District",
                 sidebarLayout(
                   sidebarPanel(
                     #class=c"fa-spin"
                     h5("Filter Projects",div(style = "display:inline-block; float:right",actionButton("resetBtn","Reset Filters", icon =icon("arrows-rotate")))),
                     selectizeInput('MSCInput',
                                    'MSC',
                                    choices = NULL,
                                    selected = NULL,
                                    multiple = F,
                                    options=list(placeholder = 'Select an MSC')), 
                     selectizeInput(
                       'districtsInput',
                       "District",
                       choices = NULL,
                       selected = NULL,
                       multiple = F,
                       options=list(placeholder = 'Select a District', 
                                    maxOptions = 40)),
                     selectizeInput('ProgramCodeInput',
                                    'Program (Code)',
                                    choices = NULL,
                                    selected = NULL,
                                    options=list(placeholder = 'Program (Code)')),
                     selectizeInput('ProgramTypeInput',
                                    'Program Type',
                                    choices = NULL,
                                    selected = NULL,
                                    options=list(placeholder = 'Program Type')),
                     selectizeInput('MissionInput',
                                    'Primary Mission',
                                    choices = NULL,
                                    selected = NULL,
                                    options=list(placeholder = 'Primary Mission')),
                     h5("Filter Project Risks"),
                     selectizeInput(
                       "projdisInput",
                       "Discipline",
                       choices = NULL,
                       selected = NULL,
                       multiple = F,
                       options=list(placeholder = 'Select a Discipline')
                     ),  selectizeInput(
                       "projcatInput",
                       "Risk Category",
                       choices = NULL,
                       selected = NULL,
                       multiple = F,
                       options=list(placeholder = 'Select a Risk Category')
                     ),
                     selectizeInput(
                       "projphaseInput",
                       "Phase",
                       choices = NULL,
                       selected = NULL,
                       multiple = F,
                       options=list(placeholder = 'Enter Phase')),
                     shinyjs::hidden(
                       selectizeInput(
                         "projmileInput",
                         "Milestone",
                         choices = NULL,
                         selected = NULL,
                         multiple = F,
                         options=list(placeholder = 'Enter Milestone'))),
                     ,
                     width=2
                   ), 
                   mainPanel(tabsetPanel(
                     tabPanel(
                       "Explore Projects",
                       plotly::plotlyOutput("projpies"),
                       DT::DTOutput("projoverview"), value= "Explore Projects"),
                     tabPanel("Explore Risks"),
                     tabPanel("Reports"))))),
        tabPanel("Project",
                 sidebarLayout(
                   sidebarPanel(
                     selectizeInput(
                       'districtInput',
                       "District",
                       choices = NULL,
                       selected = NULL,
                       multiple = F,
                       options=list(placeholder = 'Select a District', 
                                    maxOptions = 40)
                     ),
                     selectizeInput("projectInput", 
                                    "Project", 
                                    choices = NULL,
                                    selected = NULL,
                                    multiple = F,
                                    options=list(placeholder = 'Select a Project')),
                     h6("or"),
                     selectizeInput(
                       "P2Input",
                       "P2 Number",
                       choices = NULL,
                       selected = NULL,
                       multiple = F,
                       options=list(placeholder = 'Enter P2 Number')
                     ),
                     shinyjs::hidden(
                       selectizeInput(
                         "SubIDInput",
                         "Sub ID",
                         choices = NULL,
                         selected = NULL,
                         multiple = FALSE,
                         options =  list(placeholder = "Select SubID")
                       )),
                     conditionalPanel(condition = "input.reporttabs == 'Reports'",
                                      selectizeInput(
                                        "riskInput",
                                        "Risk",
                                        choices = NULL ,
                                        selected = NULL,
                                        multiple = F,
                                        options=list(placeholder = 'Select a Risk Item')
                                      )),
                     conditionalPanel(condition = "input.reporttabs == 'Explore'",
                                      selectizeInput(
                                        "phaseInput",
                                        "Phase",
                                        choices = NULL,
                                        selected = NULL,
                                        multiple = F,
                                        options=list(placeholder = 'Enter Phase')),
                                      shinyjs::hidden(
                                        selectizeInput(
                                          "mileInput",
                                          "Milestone",
                                          choices = NULL,
                                          selected = NULL,
                                          multiple = F,
                                          options=list(placeholder = 'Enter Milestone'))),
                                      selectizeInput(
                                        "disInput",
                                        "Discipline",
                                        choices = NULL,
                                        selected = NULL,
                                        multiple = F,
                                        options=list(placeholder = 'Select a discipline')
                                      )
                     ), width=2),
                   
                   mainPanel(
                     tabsetPanel(
                       tabPanel(
                         "Explore Risks",
                         plotly::plotlyOutput("pie"),
                         DT::DTOutput("overviewtab"), value= "Explore"
                       ),
                       tabPanel("Reports",
                                layout_column_wrap(
                                  width = 1/4,
                                  height = 275,
                                  bslib::card(
                                    height = 165,
                                    full_screen = FALSE,
                                    card_header("Project Risks",
                                                shiny::downloadButton(
                                                  outputId="download_Proj",
                                                  label="Download",
                                                  style = "color: #3974db; 
                                                background-color: transparent;
                                                float:right;
                                                border-color: transparent;"
                                                )
                                    ),
                                    card_body(
                                      tags$button(id ="Proj", class="action-button",tags$img(src="www/ProjAllRisk.png", height='165px', max_width='100%')
                                      )
                                    )),
                                  bslib::card(
                                    height = 100,
                                    full_screen = FALSE,
                                    card_header("All Risk",
                                                shiny::downloadButton(
                                                  outputId="download_AllRisk",
                                                  label="Download",
                                                  style = "color: #3974db; 
                                                background-color: transparent;
                                                float:right;
                                                border-color: transparent;"
                                                )),
                                    card_body(
                                      tags$button(id = "AllRisk", class="action-button",tags$img(src="www/AllRiskDetail.png", height='165px', max_width='100%')))
                                    
                                  ),
                                  bslib::card(
                                    height = 165,
                                    full_screen = FALSE,
                                    card_header("Top 4s",
                                                shiny::downloadButton(
                                                  outputId="download_Top4s",
                                                  label="Download",
                                                  style = "color: #3974db; 
                                                background-color: transparent;
                                                float:right;
                                                border-color: transparent;"
                                                )),
                                    card_body(
                                      tags$button(id = "Proj4s", class="action-button",tags$img(src="www/ProjectTop4.png",height = '165px',max_width = '100%',
                                      ),
                                      ),
                                      
                                    )
                                    #d-flex align-items-center gap-1
                                  ),
                                  bslib::card(
                                    height = 165,
                                    full_screen = FALSE,id="RiskItemCard",
                                    card_header("Risk Item Report",
                                                tooltip(bsicons::bs_icon("info-circle"), "Select a Risk Item", placement="right", id="tooltip"),
                                                shiny::downloadButton(
                                                  outputId="download_RiskItem",
                                                  label="Download",
                                                  style = "color: #3974db; 
                                                background-color: transparent;
                                                float:right;
                                                border-color: transparent;"
                                                )),
                                    card_body(
                                      tags$button(id = "RiskItem", class="action-button",tags$img(src="www/RiskItem.png", height='165px', max_width = '100%'))
                                      
                                    ))
                                  ,
                                  
                                )
                       ),
                       id = "reporttabs" )
                   )
                 )
        ), selected = "Project")
    )
  )}