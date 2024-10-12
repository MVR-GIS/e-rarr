#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
#'
#' @importFrom dplyr select mutate filter pull
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_wider
#' @importFrom shiny addResourcePath reactive observe observeEvent 
#'                   updateSelectizeInput reactiveVal isTruthy req
#' @importFrom shinyjs show hide disable enable
#' @importFrom DT renderDT datatable 
#' @importFrom shinyalert shinyalert
#' @importFrom formattable currency
#' @importFrom readr read_csv
#' @importFrom purrr reduce
#' @importFrom stats setNames
#'
library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
#library(shinycssloaders)
library(shinyalert)
library(bslib)
library(shinyjs)
library(formattable)
shiny::addResourcePath(prefix = "www", directoryPath = "./inst/app/www")


risk_item_db<-readr::read_csv("./inst/app/data/erisk_program_riskitem.csv",
                              show_col_types = FALSE)

erisk_archive <- read_csv("./inst/app/data/erisk_modeled.csv",
                          show_col_types=FALSE)

risk_project_orgs <- risk_item_db|>
  mutate(P2_SUB_IDENTIFIER = ifelse(is.na(P2_SUB_IDENTIFIER), "", 
                                    P2_SUB_IDENTIFIER),
         RISK_NAME_ID = paste(RISK_IDENTIFIER,RISK_NAME))

last_update <- erisk_archive|>
  mutate(Date_Update = as.POSIXct(DATE_ARCHIVED, format = "%d-%b-%y %I.%M.%OS %p", tz = "UTC"))|>
  arrange(desc(Date_Update))|>
  slice(1)|>
  pull(Date_Update)



conditional <- function(condition, success) {
  if (condition)
    success 
  else
    TRUE
}


condit_title <-function(condition, success) {
  if (condition)
    success 
  else
    ""
}

filter_data <- function(data, ...) {
  conditions <- list(...)
  for (condition in conditions) {
    if (!is.null(condition)) {
      data <- data |> filter(condition)
    }
  }
  return(data)
}

threshold <- as.difftime(24, units = "hours") 

### Server Function
app_server <- function(input, output, session) {

  update_choices <- function(input_id, choices) {
    updateSelectizeInput(session, input_id, choices = c("", choices), 
                         selected = "")}


  observeEvent(input$resetBtn, {
    shinyjs::reset("sidebarPanel")
    lapply(c("MSCInput", "districtsInput", "ProgramCodeInput", "ProgramTypeInput",
             "MissionInput", "phaseInput", "mileInput", "disInput","riskInput",
             "SubIDInput","P2Input","districtInput"),
           function(id) updateSelectizeInput(session, id, selected = ""))
  })
  
### Data Update Script
  # last_updated_time <-  reactiveVal(last_update)
  # 
  # 
  # is_data_out_of_date <- function(last_updated_time, threshold) {
  #   last_updated_time + threshold < Sys.time()  # Compare last update with the threshold
  # }
  # 
  # autoInvalidate <- reactiveTimer(120000)  # Check every 2 minutes
  # 
  # observe({
  #   autoInvalidate()
  #   if (is_data_out_of_date(last_updated_time(), threshold)) {
  #     tryCatch({
  #       erarr::update_data()
  #       erarr::wrangle_riskitem_table()
  #       erarr::wrangle_riskprogram_table()  
  #       # Call to update your data
  #       # Re-extract last update from the data
  #       last_update <- erisk_archive |>
  #         mutate(Date_Update = as.POSIXct(DATE_ARCHIVED, format = "%d-%b-%y %I.%M.%OS %p", tz = "UTC")) |>
  #         arrange(desc(Date_Update)) |>
  #         slice(1) |>
  #         pull(Date_Update)
  #       
  #       last_updated_time(last_update)  # Update reactive value
  #       
  #       output$last_updated <- renderText({
  #         paste("Last updated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  #       })
  #     }, error = function(e) {
  #       message("Error updating data:", e$message)
  #     })
  #   }
  # })

### Division Level Reports
 
  
  MSCs <- reactive({
    setNames(as.list(unique(risk_project_orgs$MSC)), unique(risk_project_orgs$MSC_DESCRIPT))
  })
  
  observe({
    updateSelectizeInput(session, 'MSCInput', choices = MSCs(), selected = "")
  })
  
  
  
  districts <- reactive({
    risk_project_orgs |>
      filter(conditional(input$MSCInput != "",risk_project_orgs$MSC == input$MSCInput))
  })
  
  
  observeEvent(districts(), { update_choices('districtsInput', 
                                             unique(districts()$USACE_ORGANIZATION)) 
    })
  
  
  programcodes <-reactive({
    risk_project_orgs |>
      filter(conditional(input$MSCInput != "",
                         risk_project_orgs$MSC == input$MSCInput),
             conditional(input$districtsInput != "" , 
                         risk_project_orgs$USACE_ORGANIZATION == input$districtsInput))
  })
  
  observeEvent(programcodes(), { update_choices('ProgramCodeInput', 
                                                unique(programcodes()$P2_PROGRAM_CODE)) 
    })
  
  
  programtypes <-reactive({
    risk_project_orgs |>
      filter(conditional(input$MSCInput != "",
                         risk_project_orgs$MSC == input$MSCInput),
             conditional(input$districtsInput != "" , 
                         risk_project_orgs$USACE_ORGANIZATION == input$districtsInput),
             conditional(input$ProgramCodeInput != "" , 
                         risk_project_orgs$P2_PROGRAM_CODE == input$ProgramCodeInput))
  })
  
  
  observeEvent(programtypes(),{
    update_choices('ProgramTypeInput',sort(unique(programtypes()$PROGRAMTYPENAME)))
  })
  
  primarymissions <-reactive({
    risk_project_orgs |>
      filter(conditional(input$MSCInput != "",
                         risk_project_orgs$MSC == input$MSCInput),
             conditional(input$districtsInput != "" , 
                         risk_project_orgs$USACE_ORGANIZATION == input$districtsInput),
             conditional(input$ProgramCodeInput != "" , 
                         risk_project_orgs$P2_PROGRAM_CODE == input$ProgramCodeInput),
             conditional(input$ProgramTypeInput != "" , 
                         risk_project_orgs$PROGRAMTYPENAME == input$ProgramTypeInput))
  })
  
  observeEvent(primarymissions(),{
    update_choices("MissionInput",sort(unique(primarymissions()$PRIMARYMISSION)))
  })
  
  
  proj_disciplines<-reactive({
    risk_project_orgs |>
      filter(conditional(input$MSCInput != "",
                         risk_project_orgs$MSC == input$MSCInput),
             conditional(input$districtsInput != "" , 
                         risk_project_orgs$USACE_ORGANIZATION == input$districtsInput),
             conditional(input$ProgramCodeInput != "" , 
                         risk_project_orgs$P2_PROGRAM_CODE == input$ProgramCodeInput),
             conditional(input$ProgramTypeInput != "" , 
                         risk_project_orgs$PROGRAMTYPENAME == input$ProgramTypeInput),
             conditional(input$MissionInput != "" , 
                         risk_project_orgs$PRIMARYMISSION == input$MissionInput)
             # conditional(input$projcatInput != "", 
             #             risk_project_orgs$RISKCATEGORY == input$projcatInput),
             # conditional(input$projphaseInput !="",
             #             risk_project_orgs$LIFECYCLEPHASENAME == input$phaseInput)
             
      )
  })
  
  observeEvent(proj_disciplines(),{
    update_choices("projdisInput",sort(unique(proj_disciplines()$DISCIPLINE)))
  })
  
  
  projphases <-reactive({
    risk_project_orgs |>
      filter(conditional(input$MSCInput != "",
                         risk_project_orgs$MSC == input$MSCInput),
             conditional(input$districtsInput != "" , 
                         risk_project_orgs$USACE_ORGANIZATION == input$districtsInput),
             conditional(input$ProgramCodeInput != "" , 
                         risk_project_orgs$P2_PROGRAM_CODE == input$ProgramCodeInput),
             conditional(input$ProgramTypeInput != "" , 
                         risk_project_orgs$PROGRAMTYPENAME == input$ProgramTypeInput),
             conditional(input$MissionInput != "" , 
                         risk_project_orgs$PRIMARYMISSION == input$MissionInput),
             conditional(input$projdisInput != "" , 
                         risk_project_orgs$DISCIPLINE == input$projdisInput),
             conditional(input$projcatInput != "", 
                         risk_project_orgs$RISKCATEGORY == input$projcatInput),
      )
  })
  
  observeEvent(projphases(),{
    update_choices("projphaseInput",sort(unique(projphases()$LIFECYCLEPHASENAME)))
  })
  
  
  
  projmilestones <- reactive({
    projphases() |>
      filter(conditional(input$projphaseInput !="",
                         projphases()$LIFECYCLEPHASENAME == input$projphaseInput)
             
      )
  })
  
  observe({
    if (is.null(input$projphaseInput) || input$projphaseInput == ""){
      shinyjs::hide("projmileInput")
    } else {
      shinyjs::show("projmileInput")
    }
  })
  
  observeEvent(projmilestones(), {
    update_choices("projmileInput",sort(unique(projmilestones()$MILESTONE)))
  })
  
  
  proj_cats <- reactive({
    risk_project_orgs |>
      filter(conditional(input$MSCInput != "",
                         risk_project_orgs$MSC == input$MSCInput),
             conditional(input$districtsInput != "" ,
                         risk_project_orgs$USACE_ORGANIZATION == input$districtsInput),
             conditional(input$ProgramCodeInput != "" ,
                         risk_project_orgs$P2_PROGRAM_CODE == input$ProgramCodeInput),
             conditional(input$ProgramTypeInput != "" ,
                         risk_project_orgs$PROGRAMTYPENAME == input$ProgramTypeInput),
             conditional(input$MissionInput != "" , 
                         risk_project_orgs$PRIMARYMISSION == input$MissionInput),
             conditional(input$projdisInput != "" ,
                         risk_project_orgs$DISCIPLINE == input$projdisInput )
             # conditional(input$projphaseInput !="",
             #             risk_project_orgs$LIFECYCLEPHASENAME == input$projphaseInput),
      )
  })
  
  observeEvent(proj_cats(),{
    update_choices("projcatInput",sort(unique(proj_cats()$RISKCATEGORY)))
  })
  
  
  proj_orgs_table <- reactiveVal(risk_project_orgs)
  
  projframe <- reactive({
    proj_orgs_table()|>
      filter(conditional(input$MSCInput != "",
                         proj_orgs_table()$MSC == input$MSCInput),
             conditional(input$districtsInput != "",
                         proj_orgs_table()$USACE_ORGANIZATION == input$districtsInput),
             conditional(input$ProgramCodeInput != "",
                         proj_orgs_table()$P2_PROGRAM_CODE == input$ProgramCodeInput),
             conditional(input$ProgramTypeInput != "",
                         proj_orgs_table()$PROGRAMTYPENAME == input$ProgramTypeInput),
             conditional(input$MissionInput != "",
                         proj_orgs_table()$PRIMARYMISSION == input$MissionInput),
             conditional(input$projdisInput != "" , 
                         proj_orgs_table()$DISCIPLINE == input$projdisInput),
             conditional(input$projcatInput != "", 
                         proj_orgs_table()$RISKCATEGORY == input$projcatInput),
             conditional(input$projphaseInput !="",
                         proj_orgs_table()$LIFECYCLEPHASENAME == input$projphaseInput),
             conditional(input$projmileInput !="",
                         proj_orgs_table()$MILESTONE== input$projmileInput)
      )|>
      select(PROJECT_NAME,PROJECT_ID,P2_NUMBER,DISTRICT_CODE,PRIMARYMISSION,
             PROGRAMTYPENAME,COST_RANK_DESC,SCHEDULE_RANK_DESC,
             PERFORMANCE_RANK_DESC, COST_MEAN,SCHEDULE_MEAN)
  })
  
  
  proj_costs  <-reactive({
    proj_tableprep(projframe(), 'Cost')
  })
  
  
  proj_schedule  <-reactive({
    proj_tableprep(projframe(), 'Schedule')
  })
  
  proj_perform <-reactive({projframe() |>
      group_by(PROJECT_NAME,PROJECT_ID,P2_NUMBER,DISTRICT_CODE,
               PRIMARYMISSION, PROGRAMTYPENAME)|>
      summarise(Count = n(), .groups = 'drop')

  })
  
  
  dfs_combined <- reactive({df_list <-list(proj_costs(), proj_schedule(), 
                                           proj_perform())
  })
  
  projects_comb<-reactive({dfs_combined()|>
      purrr::reduce(left_join, by = join_by(PROJECT_NAME, PROJECT_ID))|>
      select(DISTRICT_CODE, PROJECT_NAME,P2_NUMBER,PRIMARYMISSION,PROGRAMTYPENAME, Potential_Mean_Cost_Impact,
             Potential_Mean_Schedule_Impact)
      
  })
  

  proj_filt_frame <- reactive({  
    frame<-projects_comb()
    indexes <- req(input$projoverview_rows_all)
    frame[indexes,]
  })
  
  output$dynamic_title_program <- renderText({
    title_parts <- c(
      condit_title(input$MSCInput != "", 
                   paste("MSC:", input$MSCInput)),
      condit_title(input$districtsInput != "", 
                   paste("District:", input$districtsInput)),
      condit_title(input$ProgramCodeInput != "", 
                   paste("Program Code:", input$ProgramCodeInput)),
      condit_title(input$ProgramTypeInput != "", 
                   paste("Program Type:", input$ProgramTypeInput)),
      condit_title(input$MissionInput != "", 
                   paste("Mission:", input$MissionInput)),
      condit_title(input$projdisInput !="", 
                   paste("Discipline:", input$projdisInput)),
      condit_title(input$projcatInput !="", 
                   paste("Category:", input$projcatInput)),
      condit_title(input$projphaseInput !="", 
                   paste("Phase:", input$projphaseInput)),
      condit_title(input$projmileInput != "", 
                   paste("Milestone:", input$projmileInput))
    )
    
    # Remove empty strings and collapse into a single string
    paste(title_parts[title_parts != ""], collapse = " | ")
  })
  
 
  proj_cost_pie <- reactive(
    pieprep(projframe(), "COST_RANK_DESC"))
  
  proj_schedule_pie <- reactive(
    pieprep(projframe(), "SCHEDULE_RANK_DESC"))
  
  proj_perform_pie <- reactive(
    pieprep(projframe(), "PERFORMANCE_RANK_DESC"))
  
  
  output$projpies = plotly::renderPlotly({
    pie_plots(proj_cost_pie(), proj_schedule_pie(), proj_perform_pie())
  })
  
  
  
  output$projoverview = DT::renderDT({
    DT::datatable(projects_comb(),
                  colnames = c('District Code','Project Name', 'P2 Number',
                               'Program Type', 'Primary Mission',
                               'Mean Cost Impact', 
                               'Mean Schedule Impact'),
                  rownames = FALSE,
                  extensions = 'Buttons',
                  options = list(dom ='Bfrtip',
                                 buttons = c('csv', 'excel','pdf','print')
                  ))|>
      DT::formatRound("Potential_Mean_Cost_Impact", digits = 0)|>
      DT::formatCurrency("Potential_Mean_Cost_Impact", currency = "$", interval = 3, mark = ",", digits = 0)
       
  })
  
  
### Division Reports
  
  observeEvent(input$Prog4s, {
    rmarkdown::render(
      "./inst/app/rmd/ProgramTop4s.Rmd",
      params = list(
        progID = input$ProgramTypeInput,
        missionID = input$MissionInput,
        MSCID = input$MSCInput,
        districtID = input$districtsInput,
        phaseID = input$projphaseInput,
        disciplineID = input$projdisInput), 
      output_dir ="./inst/app/www"
    )
    shinyalert::shinyalert(html = TRUE, text = tagList(tags$iframe(
      src="www/ProgramTop4s.html", width = 1000, height = 900,  
      style = "border:none;")),
      size = "l",
      confirmButtonText = "Close Report",
      confirmButtonCol = "#9B4121",
      closeOnClickOutside = TRUE
    )
  })
  
  
  output$download_ProgTop4s <- downloadHandler(
    filename = function() {
      paste0("Program_Top_4_Report_", Sys.Date(), ".html")
    },
    content = function(file) {
      # Render the report to a temporary file
      rmarkdown::render(
        "./inst/app/rmd/ProgramTop4s.Rmd",
        params = list(
          progID = input$ProgramTypeInput,
          missionID = input$MissionInput,
          MSCID = input$MSCInput,
          districtID = input$districtsInput,
          phaseID = input$projphaseInput,
          disciplineID = input$projdisInput
        ),
        output_file = file,  # Specify the output file for download
        envir = new.env()
      )
    }
  )
  
  
#### Project level/Risk item reports  
  num <- reactive({
    unique(risk_project_orgs$USACE_ORGANIZATION)
  })
  
  observe({
    updateSelectizeInput(session,'districtInput',
                         choices = c("", sort(num())), 
                         options = list(maxOptions = 40, 
                                        server = TRUE, 
                                        placeholder = 'Select a District')

    )
 

  })
  # Reactive to filter projects based on selected district
  projects <- reactive({
    risk_project_orgs |>
      filter(risk_project_orgs$USACE_ORGANIZATION == input$districtInput,
             conditional(input$P2Input != "" , 
                         risk_project_orgs$P2_NUMBER == input$P2Input))
  })
  

  observeEvent(projects(),{
    update_choices('projectInput',sort(unique(projects()$PROJECT_NAME)))
  })
  
  
  P2s <- reactive({
    risk_project_orgs |>
      filter(risk_project_orgs$USACE_ORGANIZATION == input$districtInput,
             conditional(input$projectInput != "", 
                         risk_project_orgs$PROJECT_NAME == input$projectInput))
  })
  
  
  observeEvent(P2s(),{
    update_choices("P2Input",sort(unique(P2s()$P2_NUMBER)))
  })
  
  
  projsub <- risk_project_orgs|>
  filter(P2_SUB_IDENTIFIER != "")|>
  select(PROJECT_NAME, P2_NUMBER)|>
  unique()
  
  
  observe({
    if (input$projectInput %in% projsub$PROJECT_NAME || 
        input$P2Input %in% projsub$P2_NUMBER){
      shinyjs::show("SubIDInput")
    } else 
      shinyjs::hide("SubIDInput")
  })
 
  observeEvent(risks(),{
    update_choices("SubIDInput",sort(unique(risks()$P2_SUB_IDENTIFIER)))
  })

  risks <- reactive({
    risk_project_orgs |>
      filter(
        risk_project_orgs$P2_NUMBER == input$P2Input |
          risk_project_orgs$PROJECT_NAME == input$projectInput
      )
  })
  
  
  observeEvent(riskitems(),{
    sorted_choices <- riskitems() |>
      separate(RISK_IDENTIFIER, c("code", "rnkorder"), sep = "-", remove = FALSE) |>
      arrange(as.numeric(rnkorder))|>
      pull(RISK_NAME_ID)
    update_choices("riskInput",sorted_choices)
    })
 
  riskitems <- reactive({
    risk_project_orgs |>
      filter(
        risk_project_orgs$P2_NUMBER == input$P2Input |
          risk_project_orgs$PROJECT_NAME == input$projectInput,
             conditional(input$SubIDInput != "",
                         risk_project_orgs$P2_SUB_IDENTIFIER == input$SubIDInput)
      )
  })
  

  observeEvent(riskitems(),
               {
                 update_choices("phaseInput",
                                sort(unique(riskitems()$LIFECYCLEPHASENAME)))
               })
  
  disciplines <- reactive({
    risk_project_orgs |>
      filter(risk_project_orgs$P2_NUMBER == input$P2Input |
               risk_project_orgs$PROJECT_NAME == input$projectInput,
             conditional(input$SubIDInput != "",
                         risk_project_orgs$P2_SUB_IDENTIFIER == input$SubIDInput),


        conditional(input$phaseInput !="",
                    risk_project_orgs$LIFECYCLEPHASENAME == input$phaseInput),
        conditional(input$mileInput !="", 
                    risk_project_orgs$MILESTONE == input$mileInput)
      )
  })

  

  observeEvent(disciplines(),{
    update_choices("disInput",sort(unique(disciplines()$DISCIPLINE)))
  })
  
  milestones <- reactive({
    risk_project_orgs |>
      filter(risk_project_orgs$P2_NUMBER == input$P2Input |
               risk_project_orgs$PROJECT_NAME == input$projectInput,
        conditional(input$SubIDInput != "", 
                    risk_project_orgs$P2_SUB_IDENTIFIER == input$SubIDInput),
        risk_project_orgs$LIFECYCLEPHASENAME == input$phaseInput
      )
  })
  
  observe({
    if (is.null(input$phaseInput) || input$phaseInput == ""){
      shinyjs::hide("mileInput")
    } else {
      shinyjs::show("mileInput")
    }
  })
  
  observeEvent(milestones(), {
      update_choices("mileInput",sort(unique(milestones()$MILESTONE)))
    })
  

  ###Project Level/Risk item reports explore risk page
   
  in_react_frame <- reactiveVal(risk_project_orgs)
  
  filtered_frame <- reactive({
    in_react_frame() |>
      filter(conditional(input$districtInput != "",
                         risk_project_orgs$USACE_ORGANIZATION == input$districtInput),
             conditional(input$projectInput != "", 
                         risk_project_orgs$PROJECT_NAME == input$projectInput),
             conditional(input$SubIDInput != "", 
                         risk_project_orgs$P2_SUB_IDENTIFIER == input$SubIDInput),
             conditional(input$P2Input != "", 
                         risk_project_orgs$P2_NUMBER == input$P2Input),
             conditional(input$phaseInput !="", 
                         risk_project_orgs$LIFECYCLEPHASENAME == input$phaseInput),
             conditional(input$mileInput != "", 
                         risk_project_orgs$MILESTONE == input$mileInput),
             conditional(input$disInput !="", 
                         risk_project_orgs$DISCIPLINE == input$disInput)) |>
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
      colnames = c("Risk Identifier",
                   "USACE Organization",
                   "Project Name",
                   "Risk Name ",
                   "Risk Category",
                   "Discipline",
                   "Cost Rank",
                   "Schedule Rank",
                   "Performance Rank"),
      extensions = 'Buttons',
      options = list(dom ='Bfrtip',
                     buttons = c('csv', 'excel','pdf','print')),
      rownames = FALSE,
      filter = "top"
    )
  })
  
  output$dynamic_title_project <- renderText({
    title_parts <- c(
      condit_title(input$districtInput != "", 
                   paste("District:", input$districtInput)),
      condit_title(input$projectInput != "", 
                   paste("Project:", input$projectInput)),
      condit_title(input$SubIDInput != "", 
                   paste("SubID:", input$SubIDInput)),
      condit_title(input$P2Input != "", 
                   paste("P2 Number:", input$P2Input)),
      condit_title(input$phaseInput !="", 
                  paste("Phase:", input$phaseInput)),
      condit_title(input$mileInput != "", 
                  paste("Milestone:", input$mileInput)),
      condit_title(input$disInput !="", 
                  paste("Discipline:",input$disInput))
    )
    
    # Remove empty strings and collapse into a single string
    paste(title_parts[title_parts != ""], collapse = " | ")
  })
  
  output$pie = plotly::renderPlotly({
    pie_plots(cost_pie(), schedule_pie(), perform_pie())
  })
  
  ####Reporting Cards
  
  observeEvent(input$riskInput,{
    if (input$riskInput ==""){
      shinyjs::disable("RiskItemCard")
    } else{
      shinyjs::enable("RiskItemCard")
    }
  })
  
  observeEvent(input$RiskItem, {
    req(isTruthy(input$riskInput),
        isTruthy(input$projectInput) || isTruthy(input$P2Input)) 
    rmarkdown::render(
      "./inst/app/rmd/RiskItemReport.Rmd",
      params = list(projID = input$projectInput,
                    riskID = input$riskInput,
                    p2ID   = input$P2Input),
      output_dir ="./inst/app/www"
    )
    shinyalert::shinyalert(
      html = TRUE, 
      text = tagList(tags$iframe(src="www/RiskItemReport.html", 
                                 width = 900,  
                                 height = 1000,  
                                 style = "border:none;")), 
      size = "l",
      confirmButtonText = "Close Report",
      confirmButtonCol = "#9B4121",
      closeOnClickOutside = TRUE
    )
  })
  


  observeEvent(input$Proj, {
    req(isTruthy(input$projectInput) || isTruthy(input$P2Input))
    rmarkdown::render(
      "./inst/app/rmd/ProjectAllRiskReport.Rmd",
      params = list(projID = input$projectInput, 
                    p2ID   = input$P2Input,
                    p2sub  = input$SubIDInput),
      output_dir ="./inst/app/www"
    )
    shinyalert::shinyalert(
      html = TRUE, 
      text = tagList(tags$iframe(src="www/ProjectAllRiskReport.html", 
                                 width = 900, 
                                 height = 1000, 
                                 style = "border:none;")),
      size = "l", 
      confirmButtonText = "Close Report", 
      confirmButtonCol = "#9B4121",
      closeOnClickOutside = TRUE
    )
  })

  observeEvent(input$AllRisk, {
    req(isTruthy(input$projectInput) || isTruthy(input$P2Input))
    rmarkdown::render(
      "./inst/app/rmd/AllRiskDetailTable.Rmd",
      params = list(projID = input$projectInput,
                    p2ID   = input$P2Input,
                    p2sub  = input$SubIDInput), 
      output_dir ="./inst/app/www"
    )
    shinyalert::shinyalert(
      html = TRUE, 
      text = tagList(tags$iframe(src = "www/AllRiskDetailTable.html", 
                                 width = 1200, 
                                 height = 1000, 
                                 style = "border:none;")),
      size = "l", 
      confirmButtonText = "Close Report",
      confirmButtonCol = "#9B4121",
      closeOnClickOutside = TRUE
    )
  })
 
  observeEvent(input$Proj4s, {
    req(isTruthy(input$projectInput) || isTruthy(input$P2Input))
    rmarkdown::render(
      "./inst/app/rmd/ProjectTop4s.Rmd",
      params = list(
        projID = input$projectInput,
        p2ID = input$P2Input,
        p2sub= input$SubIDInput), 
      output_dir ="./inst/app/www"
    )
    shinyalert::shinyalert(html = TRUE, text = tagList(tags$iframe(
      src="www/ProjectTop4s.html", width = 1000, height = 900,  
      style = "border:none;")),
      size = "l",
      confirmButtonText = "Close Report",
      confirmButtonCol = "#9B4121",
      closeOnClickOutside = TRUE
    )
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
        params = list(projID = input$projectInput, 
                      p2ID   = input$P2Input,
                      p2sub  = input$SubIDInput),
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
        params = list(projID = input$projectInput, 
                      p2ID   = input$P2Input,
                      p2sub  = input$SubIDInput),
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
        params = list(projID = input$projectInput, 
                      p2ID   = input$P2Input,
                      p2sub  = input$SubIDInput),
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
        params = list(projID = input$projectInput, 
                      p2ID   = input$P2Input,
                      riskID = input$riskInput),
        envir = new.env(),
        intermediates_dir = tempdir()
      )
    }
  )
}
