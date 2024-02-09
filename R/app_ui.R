#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import plotly
#' @import DT
#' @noRd



erisk_item <- read_csv("./inst/app/data/RISKLIST_FULL.csv",show_col_types = FALSE)
risk_item_db<-data.frame(erisk_item)
erisk_project <- read_csv("./inst/app/data/PROJECTLIST_FULL.csv",show_col_types = FALSE)
risk_project_db<-data.frame(erisk_project)
shiny::addResourcePath(prefix = "www", directoryPath = "./inst/app/www")

RiskImpactTable<-risk_item_db[,c("PROJECT_NAME", "RISK_NAME", "USACE_ORGANIZATION","P2_NUMBER")]
riskpies <- risk_item_db |>
  select("P2_NUMBER", "RISK_IDENTIFIER","PROJECT_NAME","RISK_NAME","RISKCATEGORY",
         "DISCIPLINE", "USACE_ORGANIZATION", "COST_RANK_DESC", "SCHEDULE_RANK_DESC", "PERFORMANCE_RANK_DESC")


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(theme = bslib::bs_theme(
      bootswatch = "cosmo"),
      tags$head(tags$style(
        HTML(".shiny-notification {position:fixed;top: 30%;left: 35%;right: 35%;}"))),
      waiter::use_waiter(),
      navbarPage(title=div(img(src="www/castle.png", height="50px", width="60px"),"Risk Analysis Reporting System"),
                 tabPanel("Project",
                          sidebarLayout(
                            sidebarPanel(
                              selectizeInput("districtInput", "Select a District", choices=sort(c("",RiskImpactTable$USACE_ORGANIZATION)),selected = NULL,
                                             multiple = F),
                              selectizeInput("projectInput", "Select a project", choices=NULL ),
                              h6("or"),
                              selectizeInput("P2Input","Enter a P2 Number", choices= NULL,selected = NULL,
                                             multiple = F
                              ),
                              selectizeInput("riskInput", "Select a risk item", choices= NULL ,selected = NULL,
                                             multiple = F),
                              downloadButton("report", "Download report"),width=2),

                            mainPanel(
                              tabsetPanel(id="reporttabs",
                                          tabPanel("Explore Risks", plotly::plotlyOutput("pie"),
                                                   DT::DTOutput("overviewtab")),
                                          tabPanel("Project Report",
                                                   htmlOutput("ProjRend"), value="Project"),
                                          tabPanel("All Risk Items",
                                                   htmlOutput("AllRiskRend"), value="AllRisk"),
                                          tabPanel("Top 4 Risks",
                                                   htmlOutput("Top4s"), value="Top4"),
                                          tabPanel("Risk Item Report",
                                                   htmlOutput("reportrend"), value="RiskItem"),
                              ))))))
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
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "erisk"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
