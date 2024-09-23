#' @title Proj Pie Prep
#'
#' @description Preps data for pie charts at project level reports. Counts the values and sets the 
#' color for each of the rank levels in the specified 'rankcol`.
#'
#' @param projdf   data frame; A data frame of wrangled cost schedule and performance 
#' counts from shiny app
#' @param rankcol  character; The column name containing the risk ranks. Ranks
#'                 values must be one of: "High", "Medium", "Low", or 
#'                 "Opportunity".
#'
#' @return A data.frame of colors for each risk rank.
#'
#' @importFrom dplyr group_by summarize filter arrange mutate case_when n
#' @importFrom rlang .data enquo
#' @importFrom stringr str_to_title
#' @export
#'
#'
#'
proj_pieprep <- function(riskdf, colname) {
  upname <- str_to_upper(colname)
  fullname<-paste0(upname,'_RANK_DESC')
  proj_prepped <- riskdf |>
    mutate(
      Opportunity = if (paste0(colname, "_Opp") %in% colnames(riskdf)) {
        rowSums(select(riskdf, all_of(paste0(colname, "_Opp"))), na.rm = TRUE)
      } else {
        0
      },
      Low = if (paste0(colname, "_Low") %in% colnames(riskdf)) {
        rowSums(select(riskdf, all_of(paste0(colname, "_Low"))), na.rm = TRUE)
      } else {
        0
      },
      Medium = if (paste0(colname, "_Med") %in% colnames(riskdf)) {
        rowSums(select(riskdf, all_of(paste0(colname, "_Med"))), na.rm = TRUE)
      } else {
        0
      },
      High = if (paste0(colname, "_High") %in% colnames(riskdf)) {
        rowSums(select(riskdf, all_of(paste0(colname, "_High"))), na.rm = TRUE)
      } else {
        0
      }
    )|>
    pivot_longer(names_to = fullname, values_to = 'count',cols = c(Opportunity, Low, Medium, High))|>
    mutate(
      color = case_when(
        !!sym(fullname) == 'Opportunity' ~ 'rgb(31,120,180)',
        !!sym(fullname) == 'Low' ~ 'rgb(51,160,44)',
        !!sym(fullname) == 'Medium' ~ 'rgb(255,127,0)',
        !!sym(fullname) == 'High' ~ 'rgb(227,26,28)'
      ))
  
  return(proj_prepped)
}
