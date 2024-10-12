#' @title Wrangle Project Tables
#' @description Preps data for table on Project level reports, aggregates cost, schedule, and performance
#' risks per each project
#' @param riskdf   data frame; 
#' @param colname  character; 
#'
#' @return A data.frame of risks summarized for each project
#'
#' @importFrom dplyr group_by summarize filter arrange mutate case_when n
#' @importFrom rlang .data enquo
#' @importFrom stringr str_to_title
#' @importFrom formattable currency 
#' @export
#'

proj_tableprep <- function(riskdf, colname) {
  upname <- str_to_upper(colname)
  fullname <- paste0(upname,'_RANK_DESC')
  
  # Determine the name for summing impacts based on colname
  if (colname %in% c("Cost", 'Schedule')) {
    sum_name <- paste0(upname, '_MEAN')
  } else {
    sum_name <- NULL
  }
  
  # Data wrangling
  wrangled_table <- riskdf %>%    
    group_by(PROJECT_NAME, PROJECT_ID,P2_NUMBER, DISTRICT_CODE,
             PRIMARYMISSION, PROGRAMTYPENAME) %>%
    summarise(Count = n(), !!sym(paste0('Potential_Mean_', 
                                        colname, '_Impact')) := sum(!!sym(sum_name)), .groups = 'drop')
  
  return(wrangled_table)
}