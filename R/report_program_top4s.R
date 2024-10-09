#' @title Program Top 4 Risks Report
#' 
#' @description Creates a report of all project risks. 
#' 
#' @details All package reports are stored in the folder: `./inst/app/rmd/`
#' 
#' @name ProgramTop4Report
#' 
#' @param progID        numeric; program number
#' @param missionID     character; mission ID
#' @param MSCID         character; division ID
#' @param districtID    character; district ID
#' @param phaseID       character; phase ID
#' @param disciplineID  character; discipline id
#' 
#' 
#' @importFrom readr read_csv
#' @importFrom dplyr left_join select inner_join filter group_by
#'              summarise mutate arrange slice_head
#' @importFrom formattable currency
#' @importFrom kableExtra kbl kable_styling
#' 
NULL