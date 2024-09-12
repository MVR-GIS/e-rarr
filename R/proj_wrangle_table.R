#' @title Proj Wrangle Table
#' @description Preps data for table on Project level reports, aggregates cost, schedule, and performance
#' risks per each project
#' @param projdf   data frame; A data frame of cost, schedule, and performance risks for projects
#' @param rankcol  character; The column name containing type of risk, 'Cost", "Schedule', or 'Performance'
#'
#' @return A data.frame of risks summarized for each project
#'
#' @importFrom dplyr group_by summarize filter arrange mutate case_when n
#' @importFrom rlang .data enquo
#' @importFrom stringr str_to_title
#' @export
#'
#'

proj_tableprep <- function(riskdf, colname) {
  upname <- str_to_upper(colname)
  fullname <- paste0(upname,'_RANK_DESC')
  
  # Determine the name for summing impacts based on colname
  if (colname %in% c("Cost", 'Schedule')) {
    sum_name <- paste0(upname, '_IMPACT_MOSTLIKELY')
  } else {
    sum_name <- NULL
  }
  
  # Data wrangling
  wrangled_table <- riskdf %>%    
    group_by(PROJECT_NAME, PROJECT_ID, !!sym(fullname)) %>%
    summarise(Count = n(), Sum_impact = sum(!!sym(sum_name), na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(names_from = !!sym(fullname), values_from = Count, values_fill = list(Count = 0)) %>%
    group_by(PROJECT_NAME, PROJECT_ID) %>%
    mutate(
      !!sym(paste0(colname, '_Opp')) := ifelse("Opportunity" %in% colnames(.data), sum(!!sym('Opportunity')), 0),
      !!sym(paste0(colname, '_Low')) := ifelse("Low" %in% colnames(.data), sum(!!sym('Low')), 0),
      !!sym(paste0(colname, '_Med')) := ifelse("Medium" %in% colnames(.data), sum(!!sym('Medium')), 0),
      !!sym(paste0(colname, '_High')) := ifelse("High" %in% colnames(.data), sum(!!sym('High')), 0)
    ) %>%
    summarise(
      !!sym(paste0('Potential_Mean_', colname, '_Impact')) := sum(Sum_impact, na.rm = TRUE),
      !!sym(paste0(colname, '_Opp')) := sum(Opportunity),
      !!sym(paste0(colname, '_Low')) := sum(Low),
      !!sym(paste0(colname, '_Med')) := sum(Medium),
      !!sym(paste0(colname, '_High')) := sum(High),
      .groups = 'drop'
    )
  
  return(wrangled_table)
}