#' @title Pie Prep
#'
#' @description Preps data for pie charts
#'
#' @param riskitem   data frame; A data frame of a filtered risk item.
#' @param milestonedf data frame; A data frame of phase-milestone pairings
#'
#' @return A ggplot of current milestone on timeline of all project milestones.
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
#' @importFrom dplyr mutate select arrange
#' @importFrom rlang .data
#' @export
#'
pieprep<- function(riskdf,rankcol){
colName<- as.name(rankcol)
colnameq<-enquo(colName)

  pie_prepped<- riskdf |>
  group_by(!!colnameq) |>
  summarize(count = n())|>
  filter(!!colnameq != "No Risk") |>
  mutate('color' = case_when(!!colnameq == "Opportunity" ~'rgb(31,120,180)', 
                             !!colnameq =='Low'~'rgb(51,160,44)', 
                             !!colnameq =='Medium'~ 'rgb(255,127,0)',
                             !!colnameq == 'High'~ 'rgb(227,26,28)' ))|>
  plotly::arrange(factor(!!colnameq, levels=c('Opportunity', 'Low', 'Medium', 'High')))


return(pie_prepped)

}

