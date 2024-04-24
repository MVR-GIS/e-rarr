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
  read_csv("./inst/app/data/RISKLIST_FULL_0320245.csv", show_col_types = FALSE, col_types=cols(P2_SUB_IDENTIFIER =  col_double()))
risk_item_db <- data.frame(erisk_item)

shiny::addResourcePath(prefix = "www", directoryPath = "./inst/app/www")

RiskImpactTable <-risk_item_db |> dplyr::select("PROJECT_NAME",
                                               "RISK_IDENTIFIER",
                                               "RISK_NAME",
                                               "USACE_ORGANIZATION",
                                               "P2_NUMBER",
                                               "LIFECYCLEPHASENAME",
                                               "MILESTONE",
                                               "RISKCATEGORY",
                                               "DISCIPLINE",
                                               "P2_SUB_IDENTIFIER")|>
  mutate(P2_SUB_IDENTIFIER = ifelse(is.na(P2_SUB_IDENTIFIER), "", P2_SUB_IDENTIFIER))|>
  mutate(RISK_NAME_ID = paste(RISK_IDENTIFIER,RISK_NAME))

RiskImpactTable <- risk_item_db

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
    "PERFORMANCE_RANK_DESC",
    "LIFECYCLEPHASENAME",
    "MILESTONE",
    "P2_SUB_IDENTIFIER"
  )|>
  mutate_if(is.character, as.factor)|>
  mutate(P2_SUB_IDENTIFIER = ifelse(is.na(P2_SUB_IDENTIFIER), "", P2_SUB_IDENTIFIER))|>
  mutate(RISK_NAME_ID = paste(RISK_IDENTIFIER,RISK_NAME))



app_server <- function(input, output, session) {

  num <- reactive({
    data = RiskImpactTable$USACE_ORGANIZATION
    return(data)
  })
  
  observe({
    updateSelectizeInput(session,'districtInput',
                         choices =c("", sort(unique(num()))), 
                         options=list(maxOptions = 40
                                      ,server = TRUE,placeholder = 'Select a District'
                                      ))
  })
  

  
  
  projects <- reactive({
    RiskImpactTable |>
      filter(RiskImpactTable$USACE_ORGANIZATION == input$districtInput)
  })
  
  

  observeEvent(projects(), {
    projs <- sort(unique(projects()$PROJECT_NAME))
    updateSelectizeInput(
      inputId = "projectInput",
      choices = c("", projs),
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
  
  projsub <- RiskImpactTable|>
  filter(P2_SUB_IDENTIFIER != "")|>
  select(PROJECT_NAME, P2_NUMBER)|>
  unique()
  
  observe({
    if (input$projectInput %in% projsub$PROJECT_NAME || input$P2Input %in% projsub$P2_NUMBER){
      shinyjs::show("SubIDInput")
    } else 
      shinyjs::hide("SubIDInput")
  })


  
  
  
  
  observeEvent(risks(), {
    P2sub <- sort(risks()$P2_SUB_IDENTIFIER)
    updateSelectizeInput(
      inputId = "SubIDInput",
      choices =c(P2sub),
      selected =""
    )
  })

  risks <- reactive({
    RiskImpactTable |>
      filter(
        RiskImpactTable$P2_NUMBER == input$P2Input |
          RiskImpactTable$PROJECT_NAME == input$projectInput
      )
  })
  
  riskitems <- reactive({
    RiskImpactTable |>
      filter(
        RiskImpactTable$P2_NUMBER == input$P2Input |
          RiskImpactTable$PROJECT_NAME == input$projectInput,
        conditional(input$SubIDInput != "", RiskImpactTable$P2_SUB_IDENTIFIER == input$SubIDInput)
      )
  })
  

  observeEvent(riskitems(),{
    riskitems <- riskitems()$RISK_NAME_ID
    updateSelectizeInput(
      inputId = "riskInput",
      choices = c("",riskitems),
      selected = "")
  })
  
  
  
  

  observeEvent(risks(), {
    cats <-sort(unique(risks()$RISKCATEGORY))
    discs <-sort(unique(risks()$DISCIPLINE))
    phases <-sort(unique(risks()$LIFECYCLEPHASENAME))
    updateSelectInput(
      inputId = "catInput",
      choices = c("", cats),
      selected = ""
    )
    updateSelectInput(
      inputId = "disInput",
      choices = c("", discs),
      selected = ""
    )
    updateSelectInput(
      inputId = "phaseInput",
      choices = c("", phases),
      selected = ""
    )
  })
  

  milestones <- reactive({
    RiskImpactTable |>
      filter(
        RiskImpactTable$P2_NUMBER == input$P2Input |
          RiskImpactTable$PROJECT_NAME == input$projectInput,
        RiskImpactTable$LIFECYCLEPHASENAME == input$phaseInput
      )
  })
  
  observe({
    if (is.null(input$phaseInput) || input$phaseInput == ""){
      shinyjs::hide("mileInput")
    } else{
      shinyjs::show("mileInput")
    }
  })
  
  
  
  
  observeEvent(milestones(), {
    miles <- sort(unique(milestones()$MILESTONE))
    updateSelectInput(
      inputId = "mileInput",
      choices = c("", miles),
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
      filter(conditional(input$districtInput != "",riskpies$USACE_ORGANIZATION == input$districtInput),
             conditional(input$projectInput != "", riskpies$PROJECT_NAME == input$projectInput),
             conditional(input$SubIDInput != "", riskpies$P2_SUB_IDENTIFIER == input$SubIDInput),
             conditional(input$P2Input != "", riskpies$P2_NUMBER == input$P2Input),
             conditional(input$catInput !="", riskpies$RISKCATEGORY == input$catInput),
             conditional(input$disInput !="", riskpies$DISCIPLINE == input$disInput),
             conditional(input$phaseInput !="", riskpies$LIFECYCLEPHASENAME == input$phaseInput),
             conditional(input$mileInput != "", riskpies$MILESTONE == input$mileInput))|>
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
      ),extensions = 'Buttons',
      options = list(dom='Bfrtip',
                     buttons = c('csv', 'excel','pdf','print')),
      rownames = FALSE,
      filter = "top"
    )
  })
  
  
  output$pie = plotly::renderPlotly({
    pie_plots(cost_pie(), schedule_pie(), perform_pie())
  })
  
 # reportname <- reactive({
 #    if (input$reportInput == "All Risk") {
 #      "ProjectAllRiskReport"
 #    }
 #    else if (input$reportInput == "All Risk Detail") {
 #      "AllRiskDetailTable"
 #    }
 #    else if (input$reportInput == "Top 4s") {
 #      "ProjectTop4s"
 #    }
 #    else if (input$reportInput == "Risk Item Report") {
 #      "RiskItemReport"
 #    }
 #  })
 #  
 
 paramsreport<-reactive({
   if (input$reportInput == "All Risk" | input$reportInput == "All Risk Detail" | 
       input$reportInput == "Top 4s"){
     list(projID = input$projectInput, p2ID = input$P2Input)
   } else if (input$reportInput == "Risk Item Report"){
     list(projID = input$projectInput, p2ID = input$P2Input,
          riskID = input$riskInput)
   }
 })
  
 
 output$riskitem <- renderUI({
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
  
  
  output$download_Proj <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
      paste0(input$projectInput, " - ", "Project Report", ".html")
    },
    content = function(file) {
      rmarkdown::render(
        paste0("./inst/app/rmd/ProjectAllRiskReport.Rmd"),
        output_file = file,
        params = list(projID = input$projectInput, p2ID = input$P2Input),
        envir = new.env(),
        intermediates_dir = tempdir()
      )
    }
  )
  
  output$download_AllRisk <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
      paste0(input$projectInput, " - ", "AllRiskDetailTable", ".html")
    },
    content = function(file) {
      rmarkdown::render(
        paste0("./inst/app/rmd/AllRiskDetailTable.Rmd"),
        output_file = file,
        params = list(projID = input$projectInput, p2ID = input$P2Input),
        envir = new.env(),
        intermediates_dir = tempdir()
      )
    }
  )
  
  output$download_Top4s <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
      paste0(input$projectInput, " - ", "ProjectTop4s", ".html")
    },
    content = function(file) {
      rmarkdown::render(
        paste0("./inst/app/rmd/ProjectTop4s.Rmd"),
        output_file = file,
        params = list(projID = input$projectInput, p2ID = input$P2Input),
        envir = new.env(),
        intermediates_dir = tempdir()
      )
    }
  )
  
  output$download_RiskItem <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
      paste0(input$projectInput,"-",input$riskInput, "-", "RiskItem", ".html")
    },
    content = function(file) {
      rmarkdown::render(
        paste0("./inst/app/rmd/RiskItemReport.Rmd"),
        output_file = file,
        params = list(projID = input$projectInput, p2ID = input$P2Input, 
                      riskID = input$riskInput),
        envir = new.env(),
        intermediates_dir = tempdir()
      )
    }
  )
  
  
  
  
  
}
