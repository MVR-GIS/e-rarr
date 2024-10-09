#' Combine Organization and Division Information and add it to the project risk table 
#'
#' @param erisk_orgs data frame; an organization data frame 
#' @param erisk_dist data frame; a data frame of districts
#' @param erisk_msc data frame; a data frame of mscs
#' @param risk_item_db data frame; a data frame of combined projects and risks
#'
#' @return a dataframe of active risks, program, projects ,cost and schedule means
#'
#' @examples
#' #Get test data
#' erisk_orgs <- erarr::erisk_orgs
#' erisk_dist <- erarr::erisk_dist
#' erisk_msc <- erarr::erisk_msc
#' risk_item_db <- erarr::risk_item_db
#'
#' #Create items
#' risk_project_orgs<-create_risk_prog(erisk_orgs, erisk_dist, erisk_msc, risk_item_db)
#'
#' @importFrom dplyr select left_join inner_join join_by
#' @importFrom readr write_csv
#' @importFrom usethis use_data
#' @export
create_risk_prog <- function(erisk_orgs, erisk_dist, erisk_msc, risk_item_db){
  output_path = "../erarr/inst/app/data/"
  erisk_orgs_div <- erisk_orgs |>
    dplyr::inner_join(erisk_dist, by = dplyr::join_by(DISTRICT_CODE))
  
  erisk_orgs_div <- erisk_orgs_div |>
    dplyr::select(DISTRICT_CODE,MSC_ID)
 erisk_msc_orgs <- erisk_orgs_div |>
    dplyr::inner_join(erisk_msc, by = dplyr::join_by(MSC_ID), keep = FALSE)
  risk_program_db <- risk_item_db |>
    dplyr::inner_join(erisk_msc_orgs|>
                 dplyr::select(MSC, MSC_DESCRIPT, DISTRICT_CODE), by=dplyr::join_by(DISTRICT_CODE), keep=FALSE)
    
  readr::write_csv(risk_program_db, paste0(output_path, "erisk_program_riskitem.csv"))
  usethis::use_data(risk_program_db, overwrite = TRUE)
return(risk_program_db)
}
