#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'
#'
#'
library(shiny)
library(readr)
library(dplyr)
library(shinycssloaders)
library(shinyjs)
erisk_item <-
  readr::read_csv("./inst/app/data/RISKLIST_FULL.csv", show_col_types = FALSE)
risk_item_db <- data.frame(erisk_item)
erisk_project <-
  readr::read_csv("./inst/app/data/PROJECTLIST_FULL.csv", show_col_types = FALSE)
risk_project_db <- data.frame(erisk_project)
shiny::addResourcePath(prefix = "www", directoryPath = "./inst/app/www")

RiskImpactTable <-
  risk_item_db[, c("PROJECT_NAME",
                   "RISK_NAME",
                   "USACE_ORGANIZATION",
                   "P2_NUMBER", 
                   "LIFECYCLEPHASENAME",
                   "MILESTONE")]
riskpies <- risk_item_db |>
  dplyr::select(
    "P2_NUMBER",
    "RISK_IDENTIFIER",
    "PROJECT_NAME",
    "RISK_NAME",
    "RISKCATEGORY",
    "DISCIPLINE",
    "USACE_ORGANIZATION",
    "COST_RANK_DESC",
    "SCHEDULE_RANK_DESC",
    "PERFORMANCE_RANK_DESC"
  )|>
  mutate_if(is.character, as.factor)
  





app_server <- function(input, output, session) {

  num <- reactive({
    data = RiskImpactTable$USACE_ORGANIZATION
    return(data)
  })
  
  observe({
    updateSelectizeInput(session,'districtInput',
                         choices =c("", sort(unique(num()))), 
                         options=list(maxOptions = 40
                                      ,server = TRUE,placeholder = 'Select a District' ))
  })
  
  projects <- reactive({
    RiskImpactTable |>
      filter(RiskImpactTable$USACE_ORGANIZATION == input$districtInput)
  })
  
  

  
  
  observeEvent(projects(), {
    choices <- sort(unique(projects()$PROJECT_NAME))
    updateSelectizeInput(
      inputId = "projectInput",
      choices = c("", choices),
      selected = ""
    )
  })
  
  P2s <- reactive({
    RiskImpactTable |>
      filter(
        RiskImpactTable$USACE_ORGANIZATION == input$districtInput |
          RiskImpactTable$PROJECT_NAME == input$projectInput
      )
  })
  
  
  
  observeEvent(P2s(), {
    P2s <- sort(unique(P2s()$P2_NUMBER))
    updateSelectizeInput(
      inputId = "P2Input",
      choices = c("", P2s),
      selected = ""
    )
  })
  
  # observeEvent(projects(), {
  #   if (projects()$SubId != ""){
  #     shinyjs::show("SubIDInput")}
  #   else {
  #     shinyjs::hide("SubIDInput")
  #   }
  # })
  
  
  risks <- reactive({
    RiskImpactTable |>
      filter(
        RiskImpactTable$P2_NUMBER == input$P2Input |
          RiskImpactTable$PROJECT_NAME == input$projectInput
      )
  })
  observeEvent(risks(), {
    risks <- sort(unique(risks()$RISK_NAME))
    updateSelectizeInput(
      inputId = "riskInput",
      choices = c("", risks),
      selected = ""
    )
  })

  conditional <- function(condition, success) {
    if (condition)
      success 
    else
      TRUE
  }
  
   
in_react_frame<-reactiveVal(riskpies)
  
  filtered_frame<-reactive({
    in_react_frame()|>
      filter(conditional(input$districtInput != "",
                         riskpies$USACE_ORGANIZATION == input$districtInput),
             conditional(input$projectInput != "", riskpies$PROJECT_NAME == input$projectInput),
             conditional(input$P2Input != "", riskpies$P2_NUMBER == input$P2Input))|>
#             conditional(input$SubIDInput != "", riskpies$subIDInput == input$SubIdInput))|>
      select(
        RISK_IDENTIFIER,
        USACE_ORGANIZATION,
        PROJECT_NAME,
        RISK_NAME,
        RISKCATEGORY,
        DISCIPLINE,
        COST_RANK_DESC,
        SCHEDULE_RANK_DESC,
        PERFORMANCE_RANK_DESC)
    })
  
  filt_frame <- reactive({  
  frame<-filtered_frame()
    indexes <- req(input$overviewtab_rows_all)
    frame[indexes,]
  })

  
  cost_pie <- reactive(
      pieprep(filt_frame(), "COST_RANK_DESC"))
  
  schedule_pie <- reactive(
      pieprep(filt_frame(), "SCHEDULE_RANK_DESC"))
  perform_pie <- reactive(pieprep(filt_frame(), "PERFORMANCE_RANK_DESC"))
  
  output$overviewtab = DT::renderDT({
    DT::datatable(
      filtered_frame(),
      colnames = c(
        "Risk Identifier",
        "USACE Organization",
        "Project Name",
        "Risk Name ",
        "Risk Category",
        "Discipline",
        "Cost Rank",
        "Schedule Rank",
        "Performance Rank"
      ),
      rownames = FALSE,
      filter = "top"
    )
  })
  
  
  output$pie = plotly::renderPlotly({
    pie_plots(cost_pie(), schedule_pie(), perform_pie())
  })
  
  output$reportrend <- renderUI({
    req(
      isTruthy(input$riskInput),
      isTruthy(input$projectInput) || isTruthy(input$P2Input)
    ) 
    rmarkdown::render(
        "./inst/app/rmd/RiskItemReport.Rmd",
        params = list(
          projID = input$projectInput,
          riskID = input$riskInput,
          p2ID = input$P2Input
        ),output_dir ="./inst/app/www"
        )
    tags$iframe(src="www/RiskItemReport.html", width = '100%',  
                height = 1000,  style = "border:none;")
  })
  output$ProjRend <- renderUI({
    req(isTruthy(input$projectInput) || isTruthy(input$P2Input))
        rmarkdown::render(
          "./inst/app/rmd/ProjectAllRiskReport.Rmd",
          params = list(
            projID = input$projectInput,
            p2ID = input$P2Input
          ),output_dir ="./inst/app/www"
      )
        tags$iframe(src="www/ProjectAllRiskReport.html", width = '100%',  
                    height = 1000,  style = "border:none;")
  })
  output$AllRiskRend <- renderUI({
    req(isTruthy(input$projectInput) || isTruthy(input$P2Input))
      rmarkdown::render(
        "./inst/app/rmd/AllRiskDetailTable.Rmd",
        params = list(
          projID = input$projectInput,
          p2ID = input$P2Input
        ), output_dir ="./inst/app/www"
      )
      tags$iframe(src="www/AllRiskDetailTable.html", width = '100%',  
                  height = 1000,  style = "border:none;")
  })
  
  output$Top4s <- renderUI({
    req(isTruthy(input$projectInput) || isTruthy(input$P2Input))
    rmarkdown::render(
        "./inst/app/rmd/ProjectTop4s.Rmd",
        params = list(
          projID = input$projectInput,
          p2ID = input$P2Input
        ), output_dir ="./inst/app/www"
      )
    tags$iframe(src="www/ProjectTop4s.html", width = '100%',  
                height = 1000,  style = "border:none;")
  })
  
  tabname <- reactive({
    if (input$reporttabs == "Project") {
      "ProjectAllRiskReport"
    }
    else if (input$reporttabs == "AllRisk") {
      "AllRiskDetailTable"
    }
    else if (input$reporttabs == "Top4") {
      "ProjectTop4s"
    }
    else if (input$reporttabs == "RiskItem") {
      "RiskItemReport"
    }
  })
  
  
  params<-reactive({
    if (input$reporttabs == "Project" | input$reporttabs == "AllRisk" | 
        input$reporttabs == "Top4"){
    list(projID = input$projectInput, p2ID = input$P2Input)
  } else if (input$reporttabs == "RiskItem"){
    list(projID = input$projectInput, p2ID = input$P2Input,
         riskID = input$riskInput )
  }
    })
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
      paste0(input$projectInput, " - ", tabname(), ".html")
    },
    content = function(file) {
      rmarkdown::render(
        paste0("./inst/app/rmd/",tabname(), ".Rmd"),
        output_file = file,
        params = params(),
        envir = new.env(),
        intermediates_dir = tempdir()
      )
    }
  )
}
