#' @title Milestone Plot
#'
#' @description Creates a milestone chart 
#'
#' @param riskitem   data frame; A data frame of a filtered risk item.
#' @param milestonedf data frame; A data frame of phase-milestone pairings
#'
#' @return A ggplot of current milestone on timeline of all project milestones.
#'
#' @examples
#' #Get milestone data and risk item
#' milestonedf<-read.csv("C:/workspace/e-rarr/RiskItemReportShinyApp/data/PHASEMILESTONE.csv")
#' milestone_df<-data.frame(milestonedf)
#' 
#' 
#'
#' #example
#' milestone_plot<-milestoneplot(riskitem=risk_item, milestonedf = milestone_df)
#'
#' @importFrom magrittr |>
#' @importFrom dplyr mutate select arrange
#' @importFrom rlang .data
#' @export
#'
pieprep<- function(riskdf){
cost_pie<- riskdf |>
  group_by(COST_RANK_DESC) |>
  summarize(count = n())|>
  filter(COST_RANK_DESC != "No Risk") |>
  mutate('color' = case_when(COST_RANK_DESC == "Opportunity" ~'rgb(31,120,180)', 
                             COST_RANK_DESC=='Low'~'rgb(51,160,44)', 
                             COST_RANK_DESC =='Medium'~ 'rgb(255,127,0)',
                             COST_RANK_DESC == 'High'~ 'rgb(227,26,28)' ))|>
  plotly::arrange(factor(COST_RANK_DESC,levels=c('Opportunity', 'Low', 'Medium', 'High')))

schedule_pie<- riskdf |>
  group_by(SCHEDULE_RANK_DESC) |>
  summarize(count = n())|>
  filter(SCHEDULE_RANK_DESC != "No Risk")|>
  mutate('color' = case_when(SCHEDULE_RANK_DESC == "Opportunity" ~ 'rgb(31,120,180)', 
                             SCHEDULE_RANK_DESC=='Low'~'rgb(51,160,44)', 
                             SCHEDULE_RANK_DESC =='Medium'~ 'rgb(255,127,0)',
                             SCHEDULE_RANK_DESC == 'High'~ 'rgb(227,26,28)' ))|>
  plotly::arrange(factor(SCHEDULE_RANK_DESC,levels=c('Opportunity', 'Low', 'Medium', 'High')))

perform_pie<- riskdf |>
  group_by(PERFORMANCE_RANK_DESC) |>
  summarize(count = n())|>
  filter(PERFORMANCE_RANK_DESC != "No Risk")|>
  mutate('color' = case_when(PERFORMANCE_RANK_DESC == "Opportunity" ~ 'rgb(31,120,180)', 
                             PERFORMANCE_RANK_DESC=='Low'~'rgb(51,160,44)', 
                             PERFORMANCE_RANK_DESC =='Medium'~ 'rgb(255,127,0)',
                             PERFORMANCE_RANK_DESC == 'High'~ 'rgb(227,26,28)' ))|>
  plotly::arrange(factor(PERFORMANCE_RANK_DESC,levels=c('Opportunity', 'Low', 'Medium', 'High')))

return(cost_pie, schedule_pie,perform_pie)

}