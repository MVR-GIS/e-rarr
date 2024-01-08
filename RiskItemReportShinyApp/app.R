library(shiny)
library(readr) 
library(magrittr) 
library(dplyr) 
library(kableExtra) 
library(rlang)
library(dplyr)
library(tidyverse)
library(DT)
library(shinythemes)
library(shiny)
library(bslib)
library(stringr)

erisk_item <- read_csv("C:/workspace/e-rarr/RiskItemReportShinyApp/data/RiskItem_FullView_Update.csv")
risk_item_db<-data.frame(erisk_item)
erisk_project <- read_csv("C:/workspace/e-rarr/RiskItemReportShinyApp/data/RiskProject_FullView.csv")
risk_project_db<-data.frame(erisk_project)
risk_treat <- read_csv("C:/workspace/e-rarr/RiskItemReportShinyApp/data/RiskTreatment_FullView.csv")



erisk_ItemProj<-left_join(risk_item_db, risk_project_db, by = "PROJECT_ID")


RiskImpactTable<-erisk_ItemProj[,c("PROJECT_NAME.x", "RISK_NAME", "USACE_ORGANIZATION","P2_LOOKUP")]





shinyApp(
  ui = fluidPage(theme = bslib::bs_theme(
    bootswatch = "flatly"),
    navbarPage(title="Risk Analysis Reporting System",
               tabPanel("Project",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("districtInput", "Select a District", choices=sort(c("",RiskImpactTable$USACE_ORGANIZATION)),selected = NULL,
                                        multiple = F),
                            selectInput("projectInput", "Select a project", choices=c("",RiskImpactTable$PROJECT_NAME.x)),
                                       h6("or"),
                                      selectInput("P2Input","Enter a P2 Number", choices=c(" ",RiskImpactTable$P2_LOOKUP),selected = NULL,
                                                  multiple = F
                                      ),
                                       selectInput("riskInput", "Select a risk item", choices=sort(c(" ",RiskImpactTable$RISK_NAME)),selected = NULL,
                                                   multiple = F), 
                                       actionButton("render", "View Report"),
                                       downloadButton("report", "Download report"),width=2),
                          
                          mainPanel(
                            tabsetPanel(id="Report Tabs",
                                        tabPanel("Risk Item Report",
                                                 htmlOutput("reportrend")),
                                        tabPanel("Project Report", 
                                                 htmlOutput("ProjRend")),
                                        tabPanel("All Risk Items",
                                                 htmlOutput("AllRiskRend")),
                            )))))),
  server = function(input, output, session) {
    observe({
      projects = RiskImpactTable |>
      filter(RiskImpactTable$USACE_ORGANIZATION == input$districtInput) |>
      pull(PROJECT_NAME.x) |>
      unique() |>
      sort()
    updateSelectInput(
      inputId = "projectInput", 
      choices =c("", projects),
      selected = input$projectInput
    )
    P2s = RiskImpactTable |>
      filter(RiskImpactTable$USACE_ORGANIZATION == input$districtInput) |>
      pull(P2_LOOKUP) |>
      unique() |>
      sort()
    updateSelectInput(
      inputId = "P2Input", 
      choices =c("",P2s), 
      selected = input$P2Input)
    
    risks = RiskImpactTable |>
        filter(RiskImpactTable$PROJECT_NAME.x == input$projectInput)|>
        pull(RISK_NAME) |>
        unique() |>
        sort()
      updateSelectInput(
        inputId = "riskInput", 
        choices = c("",risks), 
        selected = input$riskInput
        )
    })
    
observeEvent(input$render,{
      output$reportrend <- renderUI({
        includeMarkdown(
          rmarkdown::render("RiskItemReport.Rmd", params=list(projID = input$projectInput, riskID = input$riskInput),
         )
        )
      })
      output$AllRiskRend <- renderUI({
        includeMarkdown(
          rmarkdown::render("AllRiskDetailTable.Rmd", params=list(projID = input$projectInput)
          )
        )
      })
      output$ProjRend <- renderUI({
        includeMarkdown(
          rmarkdown::render("ProjectAllRiskReport.Rmd", params=list(projID = input$projectInput,riskID = input$riskInput)
          )
        )
      })
}, ignoreInit=TRUE)

    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = function(){
        paste0(input$projectInput," - ",input$riskInput,"RiskItemReport",".html")
      },
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "RiskItemReport.Rmd")
        file.copy("RiskItemReport.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list( projID = input$projectInput, riskID= input$riskInput)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv()))
      })
  },
  #options=list(height=1500, width=2000)
)
