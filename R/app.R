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
library(shinycssloaders)
RiskApp<- function(...){
  erisk_item <- read_csv("./data/RISKLIST_FULL.csv",show_col_types = FALSE)
  risk_item_db<-data.frame(erisk_item)
  erisk_project <- read_csv("./data/PROJECTLIST_FULL.csv",show_col_types = FALSE)
  risk_project_db<-data.frame(erisk_project)
   addResourcePath(prefix = "www", directoryPath = "./www")
 
  RiskImpactTable<-risk_item_db[,c("PROJECT_NAME", "RISK_NAME", "USACE_ORGANIZATION","P2_NUMBER")]
  riskpies <- risk_item_db |>
    select("P2_NUMBER", "RISK_IDENTIFIER","PROJECT_NAME","RISK_NAME","RISKCATEGORY",
           "DISCIPLINE", "USACE_ORGANIZATION", "COST_RANK_DESC", "SCHEDULE_RANK_DESC", "PERFORMANCE_RANK_DESC")
  
  ui <- fluidPage(theme = bslib::bs_theme(
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
                                        tabPanel("Explore Risks", plotlyOutput("pie"),
                                                 DTOutput("overviewtab")),
                                        tabPanel("Project Report", 
                                                 htmlOutput("ProjRend"), value="Project"),
                                        tabPanel("All Risk Items",
                                                 htmlOutput("AllRiskRend"), value="AllRisk"),
                                        tabPanel("Top 4 Risks",
                                                 htmlOutput("Top4s"), value="Top4"),
                                        tabPanel("Risk Item Report",
                                                 htmlOutput("reportrend"), value="RiskItem"),
                            ))))))
  server <- function(input, output, session) {

    
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
          conditional(input$projectInput != "", riskpies$PROJECT_NAME == input$projectInput))})
    
      cost_pie<- reactive(pieprep(piedata(), "COST_RANK_DESC"))
      
      schedule_pie<-reactive(pieprep(piedata(), "SCHEDULE_RANK_DESC"))
      perform_pie<- reactive(pieprep(piedata(), "PERFORMANCE_RANK_DESC"))
      
      output$pie = plotly::renderPlotly({pie_plots(cost_pie(),schedule_pie(),perform_pie())})
      
        
    conditional <- function(condition, success){
      if (condition) success else TRUE}
    
        
    table<- reactive({riskpies |>
        filter(
          conditional(input$projectInput != "", riskpies$PROJECT_NAME == input$projectInput))|>
        select(RISK_IDENTIFIER, USACE_ORGANIZATION, PROJECT_NAME, RISK_NAME, RISKCATEGORY, DISCIPLINE, COST_RANK_DESC, SCHEDULE_RANK_DESC, PERFORMANCE_RANK_DESC)
      })
    
    output$overviewtab = DT::renderDataTable({
      DT::datatable(table(), colnames = c("Risk Identifier","USACE Organization","Project Name","Risk Name ","Risk Category","Discipline", 
                                          "Cost Rank", "Schedule Rank", "Performance Rank" ), rownames=FALSE, filter = "top")
    })
    
      output$reportrend <- renderUI({
        req(input$projectInput, input$riskInput)
        withProgress(message = 'Rendering',{
          includeMarkdown(
          rmarkdown::render("RiskItemReport.Rmd", params=list(projID = input$projectInput, riskID = input$riskInput, p2ID = input$P2Input,shinyrend = TRUE)
        )
        )})
      })
      output$ProjRend <- renderUI({
        req(input$projectInput)
        withProgress(message = 'Rendering',{
        includeMarkdown(
          rmarkdown::render("ProjectAllRiskReport.Rmd", params=list(projID = input$projectInput, p2ID = input$P2Input,shinyrend = TRUE))
        )
      })
      })
      output$AllRiskRend <- renderUI({
        req(input$projectInput)
        withProgress(message = 'Rendering',{
        includeMarkdown(
          rmarkdown::render("AllRiskDetailTable.Rmd", params=list(projID = input$projectInput,shinyrend = TRUE)
          )
        )
      })
      })
      
      output$Top4s <-renderUI({
        req(input$projectInput)
        withProgress(message = 'Rendering',{
        includeMarkdown(
          rmarkdown::render("ProjectTop4s.Rmd", params=list(projID = input$projectInput,shinyrend = TRUE)
          ))})})

      filnm<-reactive({
       if (input$reporttabs == "Project"){
        "ProjectAllRiskReport"
      } 
        else if (input$reporttabs == "AllRisk"){
          "AllRiskDetailTable"
        } 
        else if (input$reporttabs == "Top4"){
          "ProjectTop4s"
        }
        })
      
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = function(){
        paste0(input$projectInput," - ",filnm(),".html")
      },
      content = function(file) {
        rmarkdown::render(paste0(filnm(),".Rmd"), output_file = file,
                          params =list(projID = input$projectInput),
                          envir = new.env(),
                          intermediates_dir = tempdir())
      })
  }
  shinyApp(ui,server,...)
}
