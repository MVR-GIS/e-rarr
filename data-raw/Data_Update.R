## code to prepare `Data_Update` dataset goes here


# Use the `keyring` package to save the database username and password in the 
# system credential store
key_service <- "egis-db-brandonroad"
user_name <- "BrandonRoad"
# Set once on each computer prior to building book
keyring::key_set(service = key_service, username = user_name)

# Make Oracle connection
drv  <- DBI::dbDriver("Oracle")
host <- "egis-db"
port <- "1521"
sid  <- "B5SDEDP1"
connect_string <- paste0(
  "(DESCRIPTION=",
  "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
  "(CONNECT_DATA=(SID=", sid, ")))")

con_roracle <- ROracle::dbConnect(drv, 
                                  username = user_name, 
                                  password = keyring::key_get(key_service, user_name),
                                  dbname   = connect_string)

# Import tables from Oracle

erisk_project            <- ROracle::dbReadTable(con_roracle, 
                                                 "ERR_PROJECTLIST_FULL")
milestonedf      <- ROracle::dbReadTable(con_roracle, 
                                         "ERR_PROJECTPHASES")
erisk_item            <- ROracle::dbReadTable(con_roracle, 
                                              "ERR_RISKLIST_FULL")
risk_transact          <- ROracle::dbReadTable(con_roracle, 
                                               "ERR_RISKTRANSACTIONLIST_FULL")
risk_treat <- ROracle::dbReadTable(con_roracle, 
                                   "ERR_RISKTREATMENTLIST_FULL")
erisk_orgs      <- ROracle::dbReadTable(con_roracle, 
                                        "ERR_TBLUSACEORGS")

# Disconnect from the database
ROracle::dbDisconnect(con_roracle)


usethis::use_data(erisk_project,
                  milestonedf,
                  erisk_item,
                  risk_transact,
                  risk_treat,
                  erisk_orgs,
                  overwrite = TRUE)