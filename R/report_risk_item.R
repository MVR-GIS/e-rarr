#' @title Risk Item Report
#' 
#' @description Creates a report of for a specific risk item. 
#' 
#' @details All package reports are stored in the folder: `./inst/app/rmd/`
#' 
#' @name RiskItemReport
#' 
#' @param riskID   character; erisk Risk identifier
#' @param projID   character; erisk Project identifier
#' @param p2ID     numeric; USACE P2 number
#' 
#' @importFrom dplyr mutate select filter left_join case_when slice_max
#' @importFrom knitr include_graphics 
#' @importFrom kableExtra kable_styling
#' @importFrom formattable currency
#' @importFrom stringr str_to_title
#' @importFrom plotly plot_ly add_pie layout 
#' @importFrom stringr str_trim
NULL