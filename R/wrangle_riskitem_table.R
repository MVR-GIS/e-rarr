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
#' @importFrom dplyr select left_join mutate join_by
#' @importFrom formattable currency
#' @importFrom usethis use_data
#' @importFrom readr write_csv
#' @export
create_risk_proj <- function(risk, project, archive){
  output_path = "../erarr/inst/app/data/"
  erisk_mean <- archive |>
    dplyr::select(COST_MEAN, SCHEDULE_MEAN, PROJECT_ID, RISK_ID)
  
  risk_item_db <- risk |>
    dplyr::left_join(project |>
                       dplyr::select(PROJECT_ID, PRIMARYMISSION, PROGRAMTYPENAME), by = dplyr::join_by(PROJECT_ID)) |>
    dplyr::left_join(erisk_mean, by = dplyr::join_by(PROJECT_ID, RISK_ID)) |>
    dplyr::filter(RISKSTATUS == "Active") |>
    dplyr::mutate(COST_MEAN =  ifelse(
      is.na(COST_MEAN), 
      NA, 
      formattable::currency(round(COST_MEAN, -3), digits = 0)))
  
  readr::write_csv(risk_item_db,
                   paste0(output_path, "erisk_project_riskitem.csv"))
  usethis::use_data(risk_item_db, overwrite = TRUE)
  
return(risk_item_db)
}
