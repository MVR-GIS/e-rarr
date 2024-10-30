#' @title Project Top 4 Risks Report
#' 
#' @description Creates a report of top 4 risks. 
#' 
#' @details All package reports are stored in the folder: `./inst/app/rmd/`
#' 
#' @name ProjectTop4s
#' 
#' @param projID   character; erisk Project identifier
#' @param p2ID     numeric; USACE P2 number
#' @param p2sub    numeric; P2 sub identifier
#' 
#' 
#' 
#' @importFrom readr read_csv
#' @importFrom dplyr select mutate filter arrange slice_head
#' @importFrom formattable currency
#' @importFrom kableExtra kbl kable_styling
#' 
NULL