#' Combine Projects, Means, and Risk Items and only select active risks
#'
#' @param risk data frame; a risk item data frame
#' @param project data frame; a project data frame
#' @param archive data frame; a data frame of modeled and archived risks
#'
#' @return a dataframe of active risks, projects,cost and schedule means
#'
#' @examples
#' #Get test data
#' risk <- erarr::erisk_item
#' project <- erarr::erisk_project
#' archive <- erarr::erisk_modeled
#'
#' #Create items
#' risk_item_db<-create_risk_proj(risk, project, archive)
#'
#' @importFrom dplyr select left_join
#' @importFrom magrittr %>%
#'
create_risk_proj <- function(risk, project, archive){
erisk_mean <- archive|>
  dplyr::select(COST_MEAN, SCHEDULE_MEAN,PROJECT_ID,RISK_ID)

risk_item_db<- risk |>
  dplyr::left_join(project|>
                     dplyr::select(PROJECT_ID, PRIMARYMISSION))|>
  dplyr::left_join(erisk_mean, by = join_by(PROJECT_ID,RISK_ID))|>
  filter(RISKSTATUS =="Active")|>
  mutate(COST_MEAN = formattable::currency(round(COST_MEAN,-3), digits = 0))



return(risk_item_db)
}
