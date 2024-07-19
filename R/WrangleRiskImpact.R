#' @title Wrangle Risk Impact
#'
#' @description Conditionally filters Cost and Schedule risk impacts for 
#' reporting
#'
#' @param risk_item_table   data frame; A data frame of a filtered risk item.
#' @param impacttype column name for impact (Cost, Schedule,Risk)
#' @return A formatted data table of risk impacts.
#'
#' #example
#' milestone_plot<-pieplot_prep(riskdf=risk_item, rankcol)
#'
#' @importFrom rlang enquo
#' @export
#'
impactcolumn <- function(risk_item_table, impacttype) {
  colName <- as.name(impacttype)
  colnameq <- rlang::enquo(colName)
  
  
  
  if (paste("risk_item_table$NO_", colnameq, "_IMPACT") == 1) {
    "No cost impact is anticipated"
  } else if (risk_item_table$COST_IMPACT_DISTTYPE == 299) {
    risk_item_table$COST_IMPACT_MOSTLIKELY
  } else if (risk_item_table$COST_IMPACT_DISTTYPE == 306) {
    c(
      risk_item_table$COST_IMPACT_LOWEST,
      risk_item_table$COST_IMPACT_HIGHEST,
      risk_item_table$COST_IMPACT_EVIDENCE
    )
  } else if (risk_item_table$COST_IMPACT_DISTTYPE == 302) {
    c(
      risk_item_table$COST_IMPACT_LOWEST,
      risk_item_table$COST_IMPACT_MOSTLIKELY,
      risk_item_table$COST_IMPACT_HIGHEST,
      risk_item_table$COST_IMPACT_EVIDENCE
    )
  }
}
