#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import plotly
#' @import shinyjs
#' @noRd

library(shinycssloaders)
library(shinyjs)
library(readr)
library(dplyr)
library(bslib)

tags$head(
  tags$style(
    HTML("
                #download {
                    padding: 0px 5px;
                }")
  )
)

erisk_item <-
  read_csv("./inst/app/data/RISKLIST_FULL_0320245.csv", show_col_types = FALSE, col_types=cols(P2_SUB_IDENTIFIER = col_double()))
risk_item_db <- data.frame(erisk_item)
erisk_project <-
  read_csv("./inst/app/data/PROJECTLIST_FULL_03226024.csv", show_col_types=FALSE)
risk_project_db <- data.frame(erisk_project)
shiny::addResourcePath(prefix = "www", directoryPath = "./inst/app/www")

RiskImpactTable <- risk_item_db |>
  dplyr::select(
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
  mutate(P2_SUB_IDENTIFIER = ifelse(is.na(P2_SUB_IDENTIFIER), "", P2_SUB_IDENTIFIER))|>
  mutate(RISK_NAME_ID = paste(RISK_IDENTIFIER,RISK_NAME))
 
reportlist<-c("All Risk", "All Risk Detail", "Top 4s", "Risk Item Report")
 
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
          "Risk Analysis Reporting System"
        ),
        tabPanel("HQ"),
        tabPanel("Division"),
        tabPanel("District"),
        tabPanel("Project",
                 sidebarLayout(
                   sidebarPanel(
                     conditionalPanel(condition = "input.reporttabs == 'Report'",
                                      selectizeInput(
                                        "reportInput",
                                        "Report",
                                        choices =c("", reportlist),
                                        selected = NULL,
                                        multiple = F,
                                        options = list(placeholder = 'Select a report')
                                      )),
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
                     hidden(
                     selectizeInput(
                       "SubIDInput",
                       "Sub ID",
                       choices = NULL,
                       selected = NULL,
                       multiple = FALSE,
                       options =  list(placeholder = "Select SubID")
                     )),
                       selectizeInput(
                         "riskInput",
                         "Risk",
                         choices = NULL ,
                         selected = NULL,
                         multiple = F,
                         options=list(placeholder = 'Select a Risk Item')
                         ),
                     conditionalPanel(condition = "input.reporttabs == 'Explore'",
                     selectizeInput(
                       "catInput",
                       "Category",
                       choices = NULL,
                       selected = NULL,
                       multiple = F,
                       options = list(placeholder = 'Select a category')
                     ),
                     selectizeInput(
                       "disInput",
                       "Discipline",
                       choices = NULL,
                       selected = NULL,
                       multiple = F,
                       options=list(placeholder = 'Select a discipline')
                     ),
                     selectizeInput(
                       "phaseInput",
                       "Phase",
                       choices = NULL,
                       selected = NULL,
                       multiple = F,
                       options=list(placeholder = 'Enter Phase')),
                     hidden(
                     selectizeInput(
                       "mileInput",
                       "Milestone",
                       choices = NULL,
                       selected = NULL,
                       multiple = F,
                       options=list(placeholder = 'Enter Milestone')))
                     ),
                     downloadButton("report", "Download report"), width=2),
                    
                   
                   mainPanel(
                     tabsetPanel(
                       tabPanel(
                         "Explore Risks",
                         plotly::plotlyOutput("pie"),
                         DT::DTOutput("overviewtab"), value= "Explore"
                       ),
                       tabPanel("Reports", 
                                shinycssloaders::withSpinner(
                                  htmlOutput("ReportRend"), type = 4), value = "Report"
                                ),
                       tabPanel("Cards",
                                layout_column_wrap(
                                  width = 1/2,
                                  height = 700,
                                bslib::card(
                                  height = 250,
                                  full_screen = TRUE,
                                  card_header("Project Risks", 
                                              shiny::downloadButton(
                                    outputId="download",
                                    label="",
                                    icon=shiny::icon("image") # way too large
                                  )),
                                  card_body(shinycssloaders::withSpinner(
                                    htmlOutput("ProjRend"), type = 4))
                                ),
                                bslib::card(
                                  height = 250,
                                  full_screen = TRUE,
                                  card_header("All Risk"),
                                  card_body(shinycssloaders::withSpinner(
                                    htmlOutput("AllRiskRend"), type = 4))
                                ),
                                bslib::card(
                                  height = 250,
                                  full_screen = TRUE,
                                  card_header("Top 4s"),
                                  card_body(shinycssloaders::withSpinner(
                                    htmlOutput("Top4s"), type = 4))
                                ),
                                bslib::card(
                                  height = 250,
                                  full_screen = TRUE,
                                  card_header("Risk Item Report"),
                                  card_body(shinycssloaders::withSpinner(
                                    htmlOutput("riskitem"), type = 4))
                                ))
                       ),
                       id = "reporttabs" )
                   )
                   )
                 ), selected = "Project")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www",
                    app_sys("app/www"))
  
  tags$head(favicon(ext = 'png'),
            bundle_resources(path = app_sys("app/www"),
                             app_title = "erisk")
            # Add here other external resources
            # for example, you can add shinyalert::useShinyalert())
  )
  }
