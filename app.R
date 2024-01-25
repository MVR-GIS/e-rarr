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
library(knitr)
library(stringr)
library(markdown)
library(plotly)

erisk_item <- read_csv("./data/RISKLIST_FULL.csv",show_col_types = FALSE)
risk_item_db<-data.frame(erisk_item)
erisk_project <- read_csv("./data/PROJECTLIST_FULL.csv",show_col_types = FALSE)
risk_project_db<-data.frame(erisk_project)
source("./R/pie_prep.R")
source("./R/pie_plots.R")



erisk_ItemProj<-left_join(risk_item_db, risk_project_db, by = "PROJECT_ID")


RiskImpactTable<-risk_item_db[,c("PROJECT_NAME", "RISK_NAME", "USACE_ORGANIZATION","P2_NUMBER")]
riskpies <- erisk_ItemProj |>
  select("P2_NUMBER.x", "RISK_IDENTIFIER","PROJECT_NAME.x","RISK_NAME","RISKCATEGORY",
         "DISCIPLINE", "USACE_ORGANIZATION.x", "COST_RANK_DESC", "SCHEDULE_RANK_DESC", "PERFORMANCE_RANK_DESC")


shinyApp(
  ui = fluidPage(theme = bslib::bs_theme(
    bootswatch = "cosmo"),
    navbarPage(title=div(img(src="castle.png", height="50px", width="60px"),"Risk Analysis Reporting System"),
               tabPanel("  Project",
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
                            tabsetPanel(id="Report Tabs",
                                        tabPanel("Explore Risks", plotlyOutput("pie"),
                                                 DTOutput("overviewtab")),
                                        tabPanel("Project Report", 
                                                 htmlOutput("ProjRend")),
                                        tabPanel("All Risk Items",
                                                 htmlOutput("AllRiskRend")),
                                        tabPanel("Risk Item Report",
                                                 htmlOutput("reportrend")),
                            )))))),
  

  
  server = function(input, output, session) {
    
    projects <- reactive({RiskImpactTable |>
        filter(RiskImpactTable$USACE_ORGANIZATION == input$districtInput
        )})
    
    observeEvent(projects(), {
       choices <- sort(unique(projects()$PROJECT_NAME))
      updateSelectizeInput(inputId = "projectInput", choices = c("",choices),
                        selected = "") 
    })
 
      P2s <- reactive({RiskImpactTable |>
          filter(RiskImpactTable$USACE_ORGANIZATION == input$districtInput | RiskImpactTable$PROJECT_NAME == input$projectInput)})
      
      observeEvent(P2s(), {
        P2s <- sort(unique(P2s()$P2_NUMBER))
        updateSelectizeInput(
          inputId = "P2Input", 
          choices =c("",P2s), 
          selected = "")
      })
      
      risks <-reactive({RiskImpactTable |>
          filter(RiskImpactTable$P2_NUMBER == input$P2Input | RiskImpactTable$PROJECT_NAME == input$projectInput)})
      observeEvent(risks(),{
        risks <- sort(unique(risks()$RISK_NAME))
        updateSelectizeInput(
          inputId = "riskInput", 
          choices = c("",risks), selected = ""
        )
      })
      
      
    
    piedata <-reactive({riskpies |>
        filter(
          conditional(input$projectInput != "", riskpies$PROJECT_NAME.x == input$projectInput))})
    
      cost_pie<- reactive(pieprep(piedata(), "COST_RANK_DESC"))
      
      schedule_pie<-reactive(pieprep(piedata(), "SCHEDULE_RANK_DESC"))
      perform_pie<- reactive(pieprep(piedata(), "PERFORMANCE_RANK_DESC"))
      
      output$pie = plotly::renderPlotly({pie_plots(cost_pie(),schedule_pie(),perform_pie())})
      
        
    conditional <- function(condition, success){
      if (condition) success else TRUE}
    
        
    table<- reactive({riskpies |>
        filter(
          conditional(input$projectInput != "", riskpies$PROJECT_NAME.x == input$projectInput))|>
        select(RISK_IDENTIFIER, USACE_ORGANIZATION.x, PROJECT_NAME.x, RISK_NAME, RISKCATEGORY, DISCIPLINE, COST_RANK_DESC, SCHEDULE_RANK_DESC, PERFORMANCE_RANK_DESC)
      })
    
    output$overviewtab = DT::renderDataTable({
      DT::datatable(table(), colnames = c("Risk Identifier","USACE Organization","Project Name","Risk Name ","Risk Category","Discipline", 
                                          "Cost Rank", "Schedule Rank", "Performance Rank" ), rownames=FALSE, filter = "top")
    })
    
      output$reportrend <- renderUI({
          includeMarkdown(
          rmarkdown::render("RiskItemReport.Rmd", params=list(projID = input$projectInput, riskID = input$riskInput, p2ID = input$P2Input)
        )
        )
      })
      output$ProjRend <- renderUI({
        includeMarkdown(
          rmarkdown::render("ProjectAllRiskReport.Rmd", params=list(projID = input$projectInput,riskID = input$riskInput, p2ID = input$P2Input))
        )
      })
      output$AllRiskRend <- renderUI({
        includeMarkdown(
          rmarkdown::render("AllRiskDetailTable.Rmd", params=list(projID = input$projectInput)
          )
        )
      })

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
