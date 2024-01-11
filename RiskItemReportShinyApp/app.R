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

erisk_item <- read_csv("C:/workspace/e-rarr/RiskItemReportShinyApp/data/RISKLISTFULL.csv",show_col_types = FALSE)
risk_item_db<-data.frame(erisk_item)
erisk_project <- read_csv("C:/workspace/e-rarr/RiskItemReportShinyApp/data/PROJECTLIST_FULL.csv",show_col_types = FALSE)
risk_project_db<-data.frame(erisk_project)



erisk_ItemProj<-left_join(risk_item_db, risk_project_db, by = "PROJECT_ID")


RiskImpactTable<-risk_item_db[,c("PROJECT_NAME", "RISK_NAME", "USACE_ORGANIZATION","P2_NUMBER")]
riskpies <- erisk_ItemProj |>
  select("P2_NUMBER.x", "RISK_IDENTIFIER","PROJECT_NAME.x","RISK_NAME","RISKCATEGORY",
         "DISCIPLINE", "USACE_ORGANIZATION.x", "COST_RANK_DESC", "SCHEDULE_RANK_DESC", "PERFORMANCE_RANK_DESC")



shinyApp(
  ui = fluidPage(theme = bslib::bs_theme(
    bootswatch = "flatly"),
    navbarPage(title="Risk Analysis Reporting System",
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
                            tabsetPanel(id="Report Tabs",
                                        tabPanel("Home - Project Risk Overview", plotlyOutput("pie"),
                                                 DTOutput("overviewtab")),
                                        tabPanel("Risk Item Report",
                                                 htmlOutput("reportrend")),
                                        tabPanel("Project Report", 
                                                 htmlOutput("ProjRend")),
                                        tabPanel("All Risk Items",
                                                 htmlOutput("AllRiskRend")),
                            )))))),
  

  
  server = function(input, output, session) {
    
    projects <- reactive ({RiskImpactTable |>
        filter(RiskImpactTable$USACE_ORGANIZATION == input$districtInput
        )})
    
    observeEvent(projects(), {
       choices <- sort(unique(projects()$PROJECT_NAME))
      updateSelectizeInput(inputId = "projectInput", choices = c("",choices),
                        selected = "") 
    })
 
      P2s <- reactive({RiskImpactTable |>
          filter(RiskImpactTable$USACE_ORGANIZATION == input$districtInput || RiskImpactTable$PROJECT_NAME == input$projectInput)})
      
      observeEvent(P2s(), {
        P2s <- sort(unique(P2s()$P2_NUMBER))
        updateSelectizeInput(
          inputId = "P2Input", 
          choices =c("",P2s), 
          selected = "")
      })
      
      risks <-reactive({RiskImpactTable |>
          filter(RiskImpactTable$P2_NUMBER == input$P2Input || RiskImpactTable$PROJECT_NAME == input$projectInput)})
      observeEvent(risks(),{
        risks <- sort(unique(risks()$RISK_NAME))
        updateSelectizeInput(
          inputId = "riskInput", 
          choices = c("",risks), selected = ""
        )
      })
      
      
    
    plot<-reactive({riskpies |>
        filter(
          conditional(input$projectInput != "", riskpies$PROJECT_NAME.x == input$projectInput))|>
        group_by(COST_RANK_DESC) |>
        summarize(count = n())|>
        filter(COST_RANK_DESC != "No Risk")|>
        mutate('color' = case_when(COST_RANK_DESC == "Opportunity" ~'rgb(31,120,180)', 
                                   COST_RANK_DESC=='Low'~'rgb(51,160,44)', 
                                   COST_RANK_DESC =='Medium'~ 'rgb(255,127,0)',
                                   COST_RANK_DESC == 'High'~ 'rgb(227,26,28)' ))|>
        plotly::arrange(factor(COST_RANK_DESC,levels=c('Opportunity', 'Low', 'Medium', 'High')))
        })
    
    plot2<-reactive({riskpies |>
        filter(
          conditional(input$projectInput != "", riskpies$PROJECT_NAME.x == input$projectInput))|>
        group_by(SCHEDULE_RANK_DESC) |>
        summarize(count = n())|>
        filter(SCHEDULE_RANK_DESC != "No Risk")|>
        mutate('color' = case_when(SCHEDULE_RANK_DESC == "Opportunity" ~'rgb(31,120,180)', 
                                   SCHEDULE_RANK_DESC=='Low'~'rgb(51,160,44)', 
                                   SCHEDULE_RANK_DESC =='Medium'~ 'rgb(255,127,0)',
                                   SCHEDULE_RANK_DESC == 'High'~ 'rgb(227,26,28)' ))|>
        plotly::arrange(factor(SCHEDULE_RANK_DESC,levels=c('Opportunity', 'Low', 'Medium', 'High')))
    })
    
    plot3<-reactive({riskpies |>
        filter(
          conditional(input$projectInput != "", riskpies$PROJECT_NAME.x == input$projectInput))|>
        group_by(PERFORMANCE_RANK_DESC) |>
        summarize(count = n())|>
        filter(PERFORMANCE_RANK_DESC != "No Risk")|>
        mutate('color' = case_when(PERFORMANCE_RANK_DESC == "Opportunity" ~'rgb(31,120,180)', 
                                   PERFORMANCE_RANK_DESC=='Low'~'rgb(51,160,44)', 
                                   PERFORMANCE_RANK_DESC =='Medium'~ 'rgb(255,127,0)',
                                   PERFORMANCE_RANK_DESC == 'High'~ 'rgb(227,26,28)' ))|>
        plotly::arrange(factor(PERFORMANCE_RANK_DESC,levels=c('Opportunity', 'Low', 'Medium', 'High')))
    })
    
        output$pie = plotly::renderPlotly({
         fig1 <- plot_ly(textfont = list(color = '#FFFFFF')) 
           fig1 <-fig1 |> 
             add_pie(data = plot(),
                  values =  ~ count,
                  labels = ~ COST_RANK_DESC,
                  sort = FALSE,
                  textinfo = 'value',
                  title = "Cost",
                  textfont = list(color = '#FFFFFF'),
                  domain = list(row = 0, column = 0),
                  marker = list(
                    colors = ~ color,
                    line = list(color = '#FFFFFF', width = 1.5))) 
         
           fig1<- fig1 |>
            add_pie(
             data = plot2(),
             values = ~ count,
             labels = ~ SCHEDULE_RANK_DESC,
             textinfo = 'value',
             textfont = list(color = '#FFFFFF'),
             sort = FALSE,
             title = "Schedule",
             domain = list(row = 0, column = 1),
             marker = list(
               colors = ~ color,
               line = list(color = '#FFFFFF', width = 1.5)))
           
           fig1<- fig1 |>
             add_pie(
               data = plot3(),
               values = ~ count,
               labels = ~PERFORMANCE_RANK_DESC,
               textinfo = 'value',
               textfont = list(color = '#FFFFFF'),
               sort = FALSE,
               title = "Performance",
               domain = list(row = 0, column = 2),
               marker = list(
                 colors = ~ color,
                 line = list(color = '#FFFFFF', width = 1.5)))
           
          fig1<- fig1|>
            layout(showlegend = T,
                   grid=list(rows=1, columns=3),legend= list(orientation = 'h'))
          fig1

          })
    
        
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
          rmarkdown::render("RiskItemReport.Rmd", params=list(projID = input$projectInput, riskID = input$riskInput, p2ID = input$P2Input),
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
