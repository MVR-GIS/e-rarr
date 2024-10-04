#' Update Data
#'
#' This function connects to an Oracle database, imports specified tables,
#' and saves them as CSV files in the specified output path.
#'
#' @param key_service A string representing the service name in the keyring where the password is stored.
#' @param user_name A string representing the username for the Oracle database.
#' @param host A string representing the host name of the Oracle database.
#' @param port A string representing the port number of the Oracle database.
#' @param sid A string representing the SID of the Oracle database.
#' @param output_path A string representing the directory where the CSV files will be saved.
#'
#' @return None. The function saves the imported tables as CSV files in the specified output path.
#'
#' @examples
#' import_and_save_tables()
#'
#' @export

update_data <- function(key_service = "egis-db-brandonroad",
                                   user_name = "BrandonRoad",
                                   host = "egis-db",
                                   port = "1521",
                                   sid = "B5SDEDP1",
                                   output_path = "C:/workspace/erarr/inst/app/data/") {
  # Set the keyring once on each computer prior to building book
  keyring::key_set(service = key_service, username = user_name)
  
  # Make Oracle connection
  drv <- DBI::dbDriver("Oracle")
  connect_string <- paste0(
    "(DESCRIPTION=",
    "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
    "(CONNECT_DATA=(SID=", sid, ")))"
  )
  
  con_roracle <- ROracle::dbConnect(drv,
                                    username = user_name,
                                    password = keyring::key_get(key_service, user_name),
                                    dbname = connect_string)
  
  # Import tables from Oracle
  erisk_project <- ROracle::dbReadTable(con_roracle, "ERR_PROJECTLIST_FULL")
  phase_milestone <- ROracle::dbReadTable(con_roracle, "ERR_PHASEMILESTONE")
  erisk_item <- ROracle::dbReadTable(con_roracle, "ERR_RISKLIST_FULL")
  risk_transact <- ROracle::dbReadTable(con_roracle, "ERR_RISKTRANSACTIONLIST_FULL")
  risk_treat <- ROracle::dbReadTable(con_roracle, "ERR_RISKTREATMENTLIST_FULL")
  erisk_orgs <- ROracle::dbReadTable(con_roracle, "ERR_USACEORGANIZATION")
  erisk_msc <- ROracle::dbReadTable(con_roracle, "ERR_TBLMSC")
  erisk_dist <- ROracle::dbReadTable(con_roracle, "ERR_TBLDISTRICT")
  risk_modeled <- ROracle::dbReadTable(con_roracle, "ERR_RISK_MODELED")
  
  # Disconnect from the database
  ROracle::dbDisconnect(con_roracle)
  
  # Save the tables as CSV files
  readr::write_csv(erisk_project, paste0(output_path, "erisk_project.csv"))
  readr::write_csv(phase_milestone, paste0(output_path, "phase_milestone.csv"))
  readr::write_csv(erisk_item, paste0(output_path, "erisk_item.csv"))
  readr::write_csv(risk_transact, paste0(output_path, "risk_transact.csv"))
  readr::write_csv(risk_treat, paste0(output_path, "risk_treat.csv"))
  readr::write_csv(erisk_orgs, paste0(output_path, "erisk_orgs.csv"))
  readr::write_csv(erisk_dist, paste0(output_path, "erisk_dist.csv"))
  readr::write_csv(erisk_msc, paste0(output_path, "erisk_msc.csv"))
  readr::write_csv(risk_modeled, paste0(output_path, "erisk_modeled.csv"))
}

