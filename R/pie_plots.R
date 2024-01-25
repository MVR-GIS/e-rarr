#' @title Pie Plots
#'
#' @description Creates a pie plots from wrangled risk table data
#'
#' @param cost_pie   data frame; A data frame of wrangled cost pie data.
#' @param schedule_pie data frame; A data frame of wrangled schedule pie data.
#' @param perform_pie data frame; A data frame of wrangled performance pie data.
#'
#' @return A plotly plot of pie charts for cost, schedule, and performance risk.
#'
#' @examples
#' erisk_item <- read_csv("C:/workspace/e-rarr/RiskItemReportShinyApp/data/RISKLIST_FULL.csv",show_col_types = FALSE)
#' risk_item_db<-data.frame(erisk_item)
#' erisk_project <- read_csv("C:/workspace/e-rarr/RiskItemReportShinyApp/data/PROJECTLIST_FULL.csv",show_col_types = FALSE)
#' risk_project_db<-data.frame(erisk_project)
#' 
#' 
#'
#' erisk_ItemProj<-left_join(risk_item_db, risk_project_db, by = "PROJECT_ID")
#' 
#' 
#' RiskImpactTable<-risk_item_db[,c("PROJECT_NAME", "RISK_NAME", "USACE_ORGANIZATION","P2_NUMBER")]
#' riskpies <- erisk_ItemProj |>
#'   select("P2_NUMBER.x", "RISK_IDENTIFIER","PROJECT_NAME.x","RISK_NAME","RISKCATEGORY",
#'          "DISCIPLINE", "USACE_ORGANIZATION.x", "COST_RANK_DESC", "SCHEDULE_RANK_DESC", "PERFORMANCE_RANK_DESC")
#' 
#' 
#'
#' #example
#' milestone_plot<-pieplot_prep(riskdf=risk_item, rankcol)
#'
#' @importFrom magrittr |>
#' @importFrom dplyr mutate select arrange
#' @importFrom rlang .data
#' @export
#'
#'
pie_plots<- function(cost_pie,schedule_pie,perform_pie){
fig <- plot_ly(textfont = list(color = '#FFFFFF')) |>
  add_pie(
    data = cost_pie,
    values =  ~ .data$count,
    labels = ~ .data$COST_RANK_DESC,
    sort = FALSE,
    title="Cost",
    textinfo = 'value',
    textfont = list(color = '#FFFFFF'),
    domain = list(row = 0, column = 0),
    marker = list(
      colors = ~ color,
      line = list(color = '#FFFFFF', width = 1.5)
    )
  ) |>
  add_pie(
    data = schedule_pie,
    values = ~ .data$count,
    labels = ~ .data$SCHEDULE_RANK_DESC,
    textinfo = 'value',
    textfont = list(color = '#FFFFFF'),
    sort = FALSE,
    title = "Schedule",
    domain = list(row = 0, column = 1),
    marker = list(
      colors = ~ color,
      line = list(color = '#FFFFFF', width = 1.5)
    )
  ) |>
  add_pie(
    data = perform_pie,
    values = ~ .data$count,
    labels = ~ .data$PERFORMANCE_RANK_DESC,
    textinfo = 'value',
    title = "Performance",
    textfont = list(color = '#FFFFFF'),
    sort = FALSE,
    domain = list(row = 0, column = 2),
    marker = list(
      colors = ~ color,
      line = list(color = '#FFFFFF', width = 1.5)
    )
  )

pies<-fig |>
  layout(title = "", showlegend = T,
         grid=list(rows=1, columns=3),legend= list(orientation = 'h'))
return(pies)
}