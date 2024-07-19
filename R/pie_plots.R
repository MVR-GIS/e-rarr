#' @title Pie Plots
#'
#' @description Preps data for pie charts
#'
#' @param riskitem   data frame; A data frame of a filtered risk item.
#' @param milestonedf data frame; A data frame of phase-milestone pairings
#'
#' @return A ggplot of current milestone on timeline of all project milestones.
#'
#' @examples
#' erisk_item <- read_csv("inst/app/data/RISKLIST_FULL.csv",show_col_types = FALSE)
#' risk_item_db<-data.frame(erisk_item)
#' erisk_project <- read_csv("inst/app/data/PROJECTLIST_FULL.csv",show_col_types = FALSE)
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
#' @importFrom dplyr mutate select arrange
#' @importFrom rlang .data
#' @export
#'
pie_plots<- function(cost_pie,schedule_pie,perform_pie){
  fig <- plotly::plot_ly(textfont = list(color = '#FFFFFF')) |>
    plotly::add_pie(
      data = cost_pie,
      values =  ~ .data$count,
      labels = ~ .data$COST_RANK_DESC,
      title=list(text='<b> Cost <br> </b>', font = list(size = 15)),
      name= " ",
      textinfo='value',
      hoverinfo = 'label+percent',
      sort=FALSE,
      textfont = list(color = '#FFFFFF', size=15),
      domain = list(row = 0, column = 0),
      marker = list(
        colors = ~ color,
        line = list(color = '#FFFFFF', width = 1.5)))|>
    plotly::add_pie(
      data = schedule_pie,
      values = ~ .data$count,
      labels = ~ .data$SCHEDULE_RANK_DESC,
      textfont = list(color = '#FFFFFF',size=15),
      sort = FALSE,
      name= " ",
      textinfo='value',
      hoverinfo = 'label+percent',
      title =list(text='<b> Schedule <br> </b>', font = list(size = 15)),
      domain = list(row = 0, column = 1),
      marker = list(
        colors = ~ color,
        line = list(color = '#FFFFFF', width = 1.5))
    ) |>
    plotly::add_pie(
      data = perform_pie,
      values = ~ .data$count,
      labels = ~ .data$PERFORMANCE_RANK_DESC,
      title = list(text='<b> Performance <br> </b>', font = list(size = 15)),
      name= " ",
      textinfo='value',
      hoverinfo = 'label+percent',
      textfont = list(color = '#FFFFFF', size=15),
      sort = FALSE,
      domain = list(row = 0, column = 2),
      marker = list(
        colors = ~ color,
        line = list(color = '#FFFFFF', width = 1.5)))

  
  pies<-fig |>
    plotly::layout(title = "", showlegend = T,margin=list(l=20,r=20,b=20, t=20, pad=0),
                   grid=list(rows=1, columns=3),legend= list(orientation = 'h',
                  xanchor="center", x=.5, traceorder= "normal", font=list(size=15)))

  return(pies)
}
