#' @title Risk Item Proj Table
#'
#' @description Creates the Risk Item Project table used to develop project level reports
#'
#' @param riskitem   data frame; A data frame of all risk items.
#' @param riskproject data frame; A data frame of risks and their associated projects.
#'
#' @return A ggplot of current milestone on timeline of all project milestones.
#'
#' @examples
#' riskdf<- read_csv("./data/RISKLIST_FULL.csv",show_col_types = FALSE)
#' projdf <- read_csv("./data/PROJECTLIST_FULL.csv",show_col_types = FALSE)
#' 
#'
#' #example
#' riskitemproj<-riskitemproj(riskdf, projdf)
#'
#' @importFrom dplyr mutate select left_join   
#' @export
#'
riskitemproj<- function(riskdf,projdf){
  riskdf<-data.frame(riskdf)
  projdf<-data.frame(projdf)
eriskitemproj<-riskdf |>
    left_join(projdf|>
                select(PROJECT_ID, PRIMARYMISSION))
return (eriskitemproj)
}