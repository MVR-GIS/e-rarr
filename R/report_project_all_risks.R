#' @title Project All Risks Report
#' 
#' @description Creates a report of all project risks. 
#' 
#' @details All package reports are stored in the folder: `./inst/app/rmd/`
#' 
#' @name ProjectAllRiskReport
#' 
#' @param projID   character; erisk Project identifier
#' @param p2ID     numeric; USACE P2 number
#' @param p2sub    numeric; P2 sub identifier
#' 
#' @importFrom readr read_csv
#' @importFrom dplyr select filter arrange mutate
#' @importFrom tidyr separate
#' @importFrom kableExtra kbl kable_styling
#' 
NULL