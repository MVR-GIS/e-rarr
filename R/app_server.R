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
                   "P2_NUMBER")]
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
  )



app_server <- function(input, output, session) {
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
  
  
  
  piedata <- reactive({
    riskpies |>
      filter(
        conditional(
          input$projectInput != "",
          riskpies$PROJECT_NAME == input$projectInput
        )
      )
  })
  
  cost_pie <- reactive(pieprep(piedata(), "COST_RANK_DESC"))
  
  schedule_pie <- reactive(pieprep(piedata(), "SCHEDULE_RANK_DESC"))
  perform_pie <-
    reactive(pieprep(piedata(), "PERFORMANCE_RANK_DESC"))
  
  output$pie = plotly::renderPlotly({
    pie_plots(cost_pie(), schedule_pie(), perform_pie())
  })
  
  
  conditional <- function(condition, success) {
    if (condition)
      success
    else
      TRUE
  }
  
  
  table <- reactive({
    riskpies |>
      filter(
        conditional(
          input$projectInput != "",
          riskpies$PROJECT_NAME == input$projectInput
        )
      ) |>
      select(
        RISK_IDENTIFIER,
        USACE_ORGANIZATION,
        PROJECT_NAME,
        RISK_NAME,
        RISKCATEGORY,
        DISCIPLINE,
        COST_RANK_DESC,
        SCHEDULE_RANK_DESC,
        PERFORMANCE_RANK_DESC
      )
  })
  
  output$overviewtab = DT::renderDT({
    DT::datatable(
      table(),
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
  
  output$reportrend <- renderUI({
    req(
      isTruthy(input$riskInput),
      isTruthy(input$projectInput) || isTruthy(input$P2Input)
    )
    withProgress(message = 'Rendering', {
      includeMarkdown(rmarkdown::render(
        "./inst/app/rmd/RiskItemReport.Rmd",
        params = list(
          projID = input$projectInput,
          riskID = input$riskInput,
          p2ID = input$P2Input,
          shinyrend = TRUE
        )
      ))
    })
  })
  output$ProjRend <- renderUI({
    req(isTruthy(input$projectInput) || isTruthy(input$P2Input))
    withProgress(message = 'Rendering', {
      includeMarkdown(
        rmarkdown::render(
          "./inst/app/rmd/ProjectAllRiskReport.Rmd",
          params = list(
            projID = input$projectInput,
            p2ID = input$P2Input,
            shinyrend = TRUE
          )
        )
      )
    })
  })
  output$AllRiskRend <- renderUI({
    req(isTruthy(input$projectInput) || isTruthy(input$P2Input))
    withProgress(message = 'Rendering', {
      includeMarkdown(rmarkdown::render(
        "./inst/app/rmd/AllRiskDetailTable.Rmd",
        params = list(
          projID = input$projectInput,
          p2ID = input$P2Input,
          shinyrend = TRUE
        )
      ))
    })
  })
  
  output$Top4s <- renderUI({
    req(isTruthy(input$projectInput) || isTruthy(input$P2Input))
    withProgress(message = 'Rendering', {
      includeMarkdown(rmarkdown::render(
        "./inst/app/rmd/ProjectTop4s.Rmd",
        params = list(
          projID = input$projectInput,
          p2ID = input$P2Input,
          shinyrend = TRUE
        )
      ))
    })
  })
  
  filnm <- reactive({
    if (input$reporttabs == "Project") {
      "ProjectAllRiskReport"
    }
    else if (input$reporttabs == "AllRisk") {
      "AllRiskDetailTable"
    }
    else if (input$reporttabs == "Top4") {
      "ProjectTop4s"
    }
  })
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
      paste0(input$projectInput, " - ", filnm(), ".html")
    },
    content = function(file) {
      rmarkdown::render(
        paste0("./inst/app/rmd/", filnm(), ".Rmd"),
        output_file = file,
        params = list(projID = input$projectInput),
        envir = new.env(),
        intermediates_dir = tempdir()
      )
    }
  )
}
