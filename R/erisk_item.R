#' Risk Items
#'
#' A table of risk items and their key attributes for project risk analysis.
#'
#' @docType data
#'
#' @format A data frame of action items with the following attributes:
#' \describe{
#'   \item{RISK_ID}{Identifier that begins with the number 1 for each project. It is unique within each project, numeric}
#'   \item{RISK_IDENTIFIER}{Action item number unique identifier, character}
#'   \item{PROJECT_ID}{Description of the action item, numeric}
#'   \item{PROJECT_NAME}{Start date of the action, character}
#'   \item{P2_SUB_IDENTIFIER}{End date of the action, POSIXct}
#'   \item{USACE_ORGANIZATION}{Technical POC for the action, character}
#'   \item{P2_PROGRAM_CODE}{The last date the technical POC reviewed the action
#'                          item, POSIXct}
#'   \item{P2_NUMBER}{The primary project feature impacted by this action,
#'                  character}
#'   \item{DISTRICT_CODE}{Is the item currently active (Yes/No)? character}
#'   \item{RISK_NAME}{The primary discipline of the item (domain: discipline),
#'                     character}
#'   \item{RISK_CATEGORY}{The current management status of the item
#'                            (domain: management_status, Open/Closed),
#'                            character}
#'   \item{RISKCATEGORYID}{The owner of the item, character}
#'   \item{RISK_STATEMENT}{Current engagement level of the item (domain: Engagement,
#'                    Level, 1-5), character}
#'   \item{PROB_OCCURRENCE}{The date of the current enegement level, numeric}
#'   \item{LIKELIHOOD}{The date the item was last eddited, character}
#'   \item{LIKELIHOOD_DESC}{not used, character}
#'   \item{PROB_OCCURRENCE_EVIDENCE}{not used, character}
#'   \item{OCCURRENCECONFIDENCEID}{not used, character}
#'   \item{RISKOWNERDISCIPLINEID}{not used, numeric}
#'   \item{DISCIPLINE}{not used, character}
#'   \item{DISCIPLINE_ACRONYM}{not used, character}
#'   \item{RISKMANAGERID}{not used, numeric}
#'   \item{RISK_MANAGER}{not used, character}
#'   \item{PERFORMANCEIMPACTTYPEID}{not used, numeric}
#'   \item{PERFORMANCEIMPACT}{not used, character}
#'   \item{PERFORMANCEIMPACT_DESC}{not used, character}
#'   \item{PERFORMANCEIMPACT_IMPCT}{not used, numeric}
#'   \item{PERFORMANCE_IMPACT_DESC}{not used, character}
#'   \item{PERFORMANCEIMPACT_RISK_RANKING}{not used, numeric}
#'   \item{PERFORMANCE_RANK_DESC}{not used, character}
#'   \item{COST_IMPACT_LOWEST}{not used, numeric}
#'   \item{COST_IMPACT_MOSTLIKELY}{not used, numeric}
#'   \item{COST_IMPACT_HIGHEST}{not used, numeric}
#'   \item{COST_IMPACT_EVIDENCE}{not used, character}
#'   \item{COST_RISK_RANKING}{not used, numeric}
#'   \item{COST_RANK_DESC}{not used, numeric}
#'   \item{SCHEDULE_IMPACT_LOWEST}{not used, numeric}
#'   \item{SCHEDULE_IMPACT_MOSTLIKELY}{not used, numeric}
#'   \item{SCHEDULE_IMPACT_HIGHEST}{not used, numeric}
#'   \item{SCHEDULE_IMPACT_EVIDENCE}{not used, numeric}
#'   \item{SCHEDULE_RISK_RANKING}{not used, numeric}
#'   \item{SCHEDULE_RANK_DESC}{not used, numeric}
#'   \item{RISKSTATUSID}{not used, numeric}
#'   \item{RISK_STATUS_ID}{not used, numeric}
#'   \item{RISKSTATUS}{not used, numeric}
#'   \item{PROJECTPHASEID}{not used, numeric}
#'   \item{LIFECYCLEPHASENAME}{not used, numeric}
#'   \item{PROJECTMILESTONEID}{not used, numeric}
#'   \item{MILESTONE}{not used, numeric}
#'   \item{EDITDATE}{not used, numeric}
#'   \item{LOCKSTATUS}{not used, numeric}
#'   \item{USER_ID}{not used, numeric}
#'   \item{ENTERED_BY_USER}{not used, character}
#'   \item{COST_IMPACT_DISTTYPE}{not used, numeric}
#'   \item{COST_IMPACT_DISTDESC}{not used, character}
#'   \item{SCHEDULE_IMPACT_DISTTYPE}{not used, numeric}
#'   \item{SCHEDULE_IMPACT_DISTDESC}{not used, character}
#'   \item{NO_PERFORMANCE_IMPACT}{not used, numeric}
#'   \item{NO_PERFORMANCE_IMPACT_TEXT}{not used, character}
#'   \item{NO_COST_IMPACT}{not used, character}
#'   \item{NO_COST_IMPACT_TEXT}{not used, character}
#'   \item{NO_SCHEDULE_IMPACT}{not used, numeric}
#'   \item{NO_SCHEDULE_IMPACT_TEXT}{not used, character}
#' }
#'
#' @keywords datasets
#'
#' @source \url{https://err.sec.usace.army.mil/}
#'
"erisk_item"
