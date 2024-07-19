#' @title Pie Prep
#'
#' @description Preps data for pie charts. Counts the values and sets the 
#' color for each of the rank levels in the specified 'rankcol`.
#'
#' @param riskdf   data frame; A data frame of a filtered risk item.
#' @param rankcol  character; The column name containing the risk ranks. Ranks
#'                 values must be one of: "High", "Medium", "Low", or 
#'                 "Opportunity".
#'
#' @return A data.frame of colors for each risk rank.
#'
#' @importFrom dplyr group_by summarize filter arrange mutate case_when n
#' @importFrom rlang .data enquo
#' @export
#'
pieprep <- function(riskdf, rankcol) {
  colName <- as.name(rankcol)
  colnameq <- rlang::enquo(colName)
  
  pie_prepped <- riskdf |>
    group_by(!!colnameq) |>
    summarize(count = n()) |>
    filter(!!colnameq != "No Risk") |>
    arrange(factor(!!colnameq, levels = c('Opportunity', 'Low', 'Medium',
                                          'High'))) |>
    mutate(
      'color' = case_when(
        !!colnameq == "Opportunity" ~ 'rgb(31,120,180)',
        !!colnameq == 'Low' ~ 'rgb(51,160,44)',
        !!colnameq == 'Medium' ~ 'rgb(255,127,0)',
        !!colnameq == 'High' ~ 'rgb(227,26,28)'
      )
    )
  
  return(pie_prepped)
}
