#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
#'
#' @importFrom dplyr select mutate filter
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_wider
#' @importFrom shiny addResourcePath reactive observe observeEvent 
#'                   updateSelectizeInput reactiveVal isTruthy req
#' @importFrom shinyjs show hide disable enable
#' @importFrom DT renderDT datatable 
#' @importFrom shinyalert shinyalert
#' @importFrom formattable currency
#'
library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(shinycssloaders)
library(shinyalert)
library(bslib)
library(shinyjs)
library(formattable)

erisk_item <-read.csv("./inst/app/data/erisk_item.csv")
risk_item_db <- data.frame(erisk_item)
erisk_project <-read.csv("./inst/app/data/erisk_project.csv")
erisk_orgs <-read.csv("./inst/app/data/erisk_orgs.csv")


erisk_MSC <- erisk_orgs |>
  filter(str_detect(EROC,'0'))

project_orgs <- erisk_project |>
  left_join(erisk_orgs, by=join_by(DISTRICT_CODE))

shiny::addResourcePath(prefix = "www", directoryPath = "./inst/app/www")

RiskImpactTable <- risk_item_db |> 
  dplyr::select("PROJECT_NAME",
                "RISK_IDENTIFIER",
                 "RISK_NAME",
                 "USACE_ORGANIZATION",
                 "P2_NUMBER",
                 "LIFECYCLEPHASENAME",
                 "MILESTONE",
                 "RISKCATEGORY",
                 "DISCIPLINE",
                 "P2_SUB_IDENTIFIER") |>
  mutate(P2_SUB_IDENTIFIER = ifelse(is.na(P2_SUB_IDENTIFIER), "", 
                                    P2_SUB_IDENTIFIER)) |>
  mutate(RISK_NAME_ID = paste(RISK_IDENTIFIER,RISK_NAME))


riskpies <- risk_item_db |>
  dplyr::select("P2_NUMBER",
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
                "P2_SUB_IDENTIFIER") |>
  mutate_if(is.character, as.factor) |>
  mutate(P2_SUB_IDENTIFIER = ifelse(is.na(P2_SUB_IDENTIFIER), "", 
                                    P2_SUB_IDENTIFIER)) |>
  mutate(RISK_NAME_ID = paste(RISK_IDENTIFIER,RISK_NAME))|>
  mutate(RISK_NAME_ID =str_trim(RISK_NAME_ID, side = c("right")))

risk_proj_orgs <- risk_item_db |>
  left_join(project_orgs|>
            select(PROJECT_ID, PRIMARYMISSION,MSC,DISTRICT_NAME, PROGRAMTYPENAME
                   ),  by=join_by(PROJECT_ID))



MVR_items<-risk_item_db |>
  filter(DISTRICT_CODE == "MVR")

conditional <- function(condition, success) {
  if (condition)
    success 
  else
    TRUE
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


### Server Function
app_server <- function(input, output, session) {
  
  
  update_choices <- function(input_id, choices) {
    updateSelectizeInput(session, input_id, choices = c("", choices), selected = "")
  }
  


  observeEvent(input$resetBtn, {
    shinyjs::reset("sidebarPanel")
    lapply(c("MSCInput", "districtsInput", "ProgramCodeInput", "ProgramTypeInput",
             "MissionInput", "phaseInput", "mileInput", "disInput"),
           function(id) updateSelectizeInput(session, id, selected = ""))
  })

  
  
### Division Level Reports
 
  MSCs <- reactive({setNames(as.list(erisk_MSC$MSC), erisk_MSC$DISTRICT_NAME)})
  
  observe({
    updateSelectizeInput(session, 'MSCInput', choices = MSCs(), selected = "")
  })
  
  districts <- reactive({
    project_orgs |>
      filter(conditional(input$MSCInput != "",project_orgs$MSC == input$MSCInput))
  })
  
  
  observeEvent(districts(), { update_choices('districtsInput', 
                                             unique(districts()$USACE_ORGANIZATION)) })
  
  
  programcodes <-reactive({
    project_orgs |>
      filter(conditional(input$MSCInput != "",
                         project_orgs$MSC == input$MSCInput),
             conditional(input$districtsInput != "" , 
                         project_orgs$USACE_ORGANIZATION == input$districtsInput))
  })
  
  observeEvent(programcodes(), { update_choices('ProgramCodeInput', 
                                                unique(programcodes()$P2_PROGRAM_CODE)) })
  
  
  programtypes <-reactive({
    project_orgs |>
      filter(conditional(input$MSCInput != "",
                         project_orgs$MSC == input$MSCInput),
             conditional(input$districtsInput != "" , 
                         project_orgs$USACE_ORGANIZATION == input$districtsInput),
             conditional(input$ProgramCodeInput != "" , 
                         project_orgs$P2_PROGRAM_CODE == input$ProgramCodeInput))
  })
  
  
  observeEvent(programtypes(),{
    update_choices('ProgramTypeInput',sort(unique(programtypes()$PROGRAMTYPENAME)))
  })
  
  primarymissions <-reactive({
    project_orgs |>
      filter(conditional(input$MSCInput != "",
                         project_orgs$MSC == input$MSCInput),
             conditional(input$districtsInput != "" , 
                         project_orgs$USACE_ORGANIZATION == input$districtsInput),
             conditional(input$ProgramCodeInput != "" , 
                         project_orgs$P2_PROGRAM_CODE == input$ProgramCodeInput),
             conditional(input$ProgramTypeInput != "" , 
                         project_orgs$PROGRAMTYPENAME == input$ProgramTypeInput))
  })
  
  observeEvent(primarymissions(),{
    update_choices("MissionInput",sort(unique(primarymissions()$PRIMARYMISSION)))
  })
  
  
  proj_disciplines<-reactive({
    risk_proj_orgs |>
      filter(conditional(input$MSCInput != "",
                         risk_proj_orgs$MSC == input$MSCInput),
             conditional(input$districtsInput != "" , 
                         risk_proj_orgs$USACE_ORGANIZATION == input$districtsInput),
             conditional(input$ProgramCodeInput != "" , 
                         risk_proj_orgs$P2_PROGRAM_CODE == input$ProgramCodeInput),
             conditional(input$ProgramTypeInput != "" , 
                         risk_proj_orgs$PROGRAMTYPENAME == input$ProgramTypeInput),
             conditional(input$MissionInput != "" , 
                         risk_proj_orgs$PRIMARYMISSION == input$MissionInput),
             conditional(input$projcatInput != "", 
                         risk_proj_orgs$RISKCATEGORY == input$projcatInput),
             conditional(input$projphaseInput !="",
                         risk_proj_orgs$LIFECYCLEPHASENAME == input$phaseInput)
             
      )
  })
  
  observeEvent(proj_disciplines(),{
    update_choices("projdisInput",sort(unique(proj_disciplines()$DISCIPLINE)))
  })
  
  
  projphases <-reactive({
    risk_proj_orgs |>
      filter(conditional(input$MSCInput != "",
                         risk_proj_orgs$MSC == input$MSCInput),
             conditional(input$districtsInput != "" , 
                         risk_proj_orgs$USACE_ORGANIZATION == input$districtsInput),
             conditional(input$ProgramCodeInput != "" , 
                         risk_proj_orgs$P2_PROGRAM_CODE == input$ProgramCodeInput),
             conditional(input$ProgramTypeInput != "" , 
                         risk_proj_orgs$PROGRAMTYPENAME == input$ProgramTypeInput),
             conditional(input$MissionInput != "" , 
                         risk_proj_orgs$PRIMARYMISSION == input$MissionInput),
             conditional(input$projdisInput != "" , 
                         risk_proj_orgs$DISCIPLINE == input$projdisInput),
             conditional(input$projcatInput != "", 
                         risk_proj_orgs$RISKCATEGORY == input$projcatInput),
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
    risk_proj_orgs |>
      filter(conditional(input$MSCInput != "",
                         risk_proj_orgs$MSC == input$MSCInput),
             conditional(input$districtsInput != "" ,
                         risk_proj_orgs$USACE_ORGANIZATION == input$districtsInput),
             conditional(input$ProgramCodeInput != "" ,
                         risk_proj_orgs$P2_PROGRAM_CODE == input$ProgramCodeInput),
             conditional(input$ProgramTypeInput != "" ,
                         risk_proj_orgs$PROGRAMTYPENAME == input$ProgramTypeInput),
             conditional(input$MissionInput != "" , 
                         risk_proj_orgs$PRIMARYMISSION == input$MissionInput),
             conditional(input$projdisInput !="",
                         risk_proj_orgs$DISCIPLINE == input$projdisInput),
             conditional(input$projphaseInput !="",
                         risk_proj_orgs$LIFECYCLEPHASENAME == input$projphaseInput),
      )
  })
  
  observeEvent(proj_cats(),{
    update_choices("projcatInput",sort(unique(proj_cats()$RISKCATEGORY)))
  })
  
  
  proj_orgs_table <- reactiveVal(risk_proj_orgs)
  
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
      select(PROJECT_NAME,PROJECT_ID,DISTRICT_CODE,PRIMARYMISSION,COST_RANK_DESC,COST_IMPACT_MOSTLIKELY,SCHEDULE_RANK_DESC,
             SCHEDULE_IMPACT_MOSTLIKELY,PERFORMANCE_RANK_DESC)
  })
  
  
  proj_costs  <-reactive({
    proj_tableprep(projframe(), 'Cost')
  })
  
  
  proj_schedule  <-reactive({
    proj_tableprep(projframe(), 'Schedule')
  })
  
  proj_perform <-reactive({projframe() |>
      group_by(PROJECT_NAME,PROJECT_ID,PERFORMANCE_RANK_DESC)|>
      summarise(Count = n(),.groups = 'drop')|>
      pivot_wider(names_from = PERFORMANCE_RANK_DESC, values_from = Count, values_fill = list(Count = 0))|>
      group_by(PROJECT_NAME, PROJECT_ID)|>
      mutate(
        Performance_Opp = if ("Opportunity" %in% colnames(.data)) Opportunity else 0,
        Performance_Low = if ("Low" %in% colnames(.data)) Low else 0,
        Performance_Med = if ("Medium" %in% colnames(.data)) Medium else 0,
        Performance_High = if ("High" %in% colnames(.data)) High else 0
      )|>
      # Step 2: Summarize the new columns
      summarise(
        Performance_Opp = sum(Opportunity),
        Performance_Low = sum(Low),
        Performance_Med = sum(Medium),
        Performance_High = sum(High),
        .groups = 'drop'
      )})
  
  
  dfs_combined <- reactive({df_list <-list(proj_costs(), proj_schedule(), proj_perform())
  })
  
  projects_comb<-reactive({dfs_combined()|>
      purrr::reduce(left_join, by = join_by(PROJECT_NAME, PROJECT_ID))|>
      select(PROJECT_NAME, PROJECT_ID, Potential_Mean_Cost_Impact,Potential_Mean_Schedule_Impact,
             Cost_Opp,Cost_Low,Cost_Med,Cost_High, Schedule_Opp, Schedule_Low, Schedule_Med, Schedule_High,
             Performance_Opp, Performance_Low, Performance_Med, Performance_High)
  })
  
  proj_filt_frame <- reactive({  
    frame<-projects_comb()
    indexes <- req(input$projoverview_rows_all)
    frame[indexes,]
  })
  
  proj_cost_pie <- reactive({
    proj_pieprep(projects_comb(), "Cost")})
  
  proj_schedule_pie <- reactive({
    proj_pieprep(projects_comb(), "Schedule")})
  
  proj_perform_pie <- reactive({
    proj_pieprep(projects_comb(), "Performance")})
  
  output$projpies = plotly::renderPlotly({
    pie_plots(proj_cost_pie(), proj_schedule_pie(), proj_perform_pie())
  })
  
  mvr_projorgs<- project_orgs |>
    filter(DISTRICT_CODE == "MVR")
  mvr<-risk_proj_orgs|>
    filter(DISTRICT_CODE == "MVR")
  mvr_2 <-erisk_project|>
    filter(DISTRICT_CODE =='MVR')
  
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(colspan = 1, 'Project Name'),
        th(colspan = 1, 'Project ID'),
        th(colspan = 1, 'Potential Mean Cost Impact'),
        th(colspan = 1, 'Potential Mean Schedule Impact'),
        th(colspan = 4,style = "text-align: center;", 'Cost'),
        th(colspan = 4, style = "text-align: center;",'Schedule'),
        th(colspan = 4, style = "text-align: center;",'Performance')
      ),
      tr(
        th(colspan = 4,""),
        lapply(rep(c('O', 'L', 'M', 'H'),3), th),
      )
    )
  ))
  
  
  # proj_costs  <-risk_proj_orgs|>
  #   filter(COST_RANK_DESC != 'No Risk')|>
  #   group_by(PROJECT_NAME,COST_RANK_DESC)|>
  #   summarise(Count = n(),Sum_impact = sum(COST_IMPACT_MOSTLIKELY),.groups = 'drop')|>
  #   pivot_wider(names_from = COST_RANK_DESC, values_from = Count, values_fill = list(Count = 0))|>
  #   group_by(PROJECT_NAME)|>
  #   summarise('Potential Mean Cost Impact'  = sum(Sum_impact), Cost_Opp = sum(Opportunity), Cost_Low = sum(Low),
  #             Cost_med = sum(Medium), Cost_High = sum(High))
  
  
  # 
  # proj_schedule  <-projframe()|>
  #   group_by(PROJECT_NAME,SCHEDULE_RANK_DESC )
  # 
  # proj_perform  <-projframe()|>
  #   group_by(PROJECT_NAME,PERFORMANCE_RANK_DESC )
  
    
  output$projoverview = DT::renderDT({
    DT::datatable(projects_comb(),
                  extensions = 'Buttons',
                  options = list(dom ='Bfrtip',
                                 buttons = c('csv', 'excel','pdf','print'),
                                 columnDefs = list(
                                   list(
                                     targets = 4:15,  # Column indices to disable sorting
                                     orderable = FALSE
                                   )
                                 )
                  ),
                  rownames = FALSE,
                  container = sketch
    )
  })
  
#### Project level/Risk item reports  
  num <- reactive({
    data = RiskImpactTable$USACE_ORGANIZATION
    return(data)
  })
  
  observe({
    updateSelectizeInput(session,'districtInput',
                         choices = c("", sort(unique(num()))), 
                         options = list(maxOptions = 40, 
                                        server = TRUE, 
                                        placeholder = 'Select a District')

    )

  })

  projects <- reactive({
    RiskImpactTable |>
      filter(RiskImpactTable$USACE_ORGANIZATION == input$districtInput,
             conditional(input$P2Input != "" , 
                         RiskImpactTable$P2_NUMBER == input$P2Input))
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
      filter(RiskImpactTable$USACE_ORGANIZATION == input$districtInput,
             conditional(input$projectInput != "", 
                         RiskImpactTable$PROJECT_NAME == input$projectInput))
  })
  
  
  observeEvent(P2s(),{
    update_choices("P2Input",sort(unique(P2s()$P2_NUMBER)))
  })
  
  
  projsub <- RiskImpactTable|>
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
  
  
  observeEvent(riskitems(),{
    update_choices("riskInput",sort(unique(risks()$RISK_NAME_ID)))
  })
  
  riskitems <- reactive({
    RiskImpactTable |>
      filter(
        RiskImpactTable$P2_NUMBER == input$P2Input |
          RiskImpactTable$PROJECT_NAME == input$projectInput,
             conditional(input$SubIDInput != "", RiskImpactTable$P2_SUB_IDENTIFIER == input$SubIDInput)
      )
  })
  


  observeEvent(riskitems(),
               {
                 update_choices("phaseInput",sort(unique(riskitems()$LIFECYCLEPHASENAME)))
               })
  
  disciplines <- reactive({
    RiskImpactTable |>
      filter(RiskImpactTable$P2_NUMBER == input$P2Input |
               RiskImpactTable$PROJECT_NAME == input$projectInput,
             conditional(input$SubIDInput != "",
                         RiskImpactTable$P2_SUB_IDENTIFIER == input$SubIDInput),


        conditional(input$phaseInput !="",
                    RiskImpactTable$LIFECYCLEPHASENAME == input$phaseInput),
        conditional(input$mileInput !="", 
                    RiskImpactTable$MILESTONE == input$mileInput)
      )
  })

  

  observeEvent(disciplines(),{
    update_choices("disInput",sort(unique(disciplines()$DISCIPLINE)))
  })
  
  milestones <- reactive({
    RiskImpactTable |>
      filter(RiskImpactTable$P2_NUMBER == input$P2Input |
               RiskImpactTable$PROJECT_NAME == input$projectInput,
        conditional(input$SubIDInput != "", 
                    RiskImpactTable$P2_SUB_IDENTIFIER == input$SubIDInput),
        RiskImpactTable$LIFECYCLEPHASENAME == input$phaseInput
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
   
  in_react_frame <- reactiveVal(riskpies)
  
  filtered_frame <- reactive({
    in_react_frame() |>
      filter(conditional(input$districtInput != "",
                         riskpies$USACE_ORGANIZATION == input$districtInput),
             conditional(input$projectInput != "", 
                         riskpies$PROJECT_NAME == input$projectInput),
             conditional(input$SubIDInput != "", 
                         riskpies$P2_SUB_IDENTIFIER == input$SubIDInput),
             conditional(input$P2Input != "", 
                         riskpies$P2_NUMBER == input$P2Input),
             conditional(input$phaseInput !="", 
                         riskpies$LIFECYCLEPHASENAME == input$phaseInput),
             conditional(input$mileInput != "", 
                         riskpies$MILESTONE == input$mileInput),
             conditional(input$disInput !="", 
                         riskpies$DISCIPLINE == input$disInput)) |>
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
  
  output$pie = plotly::renderPlotly({
    pie_plots(cost_pie(), schedule_pie(), perform_pie())
  })
  
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
      closeOnClickOutside = TRUE
    )
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
      # showModal(modalDialog(title = "Risk Report",
      #                       tags$iframe(src = "www/AllRiskDetailTable.html", 
      #                                   width = 1200,  
      #                                   height = 1000,  
      #                                   style = "border:none:"), 
      #                       easyClose = TRUE,
      #                       size = "xl")),
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
      size = "l",confirmButtonText = "Close Report",
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
