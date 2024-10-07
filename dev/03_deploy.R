# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod
devtools::check()
rhub::check_for_cran()

# Deploy

## Local, CRAN or Package Manager ----
## This will build a tar.gz that can be installed locally,
## sent to CRAN, or to a package manager
devtools::build()

## RStudio ----
## If you want to deploy on RStudio related platforms
golem::add_rstudioconnect_file()
golem::add_shinyappsio_file()
golem::add_shinyserver_file()

## Docker ----
## If you want to deploy via a generic Dockerfile


oracle <- 
"
# Install unixODBC packages
  apt-get install -y unixodbc unixodbc-dev

# Install Oracle Instant Client components
  apt-get install -y libaio1 alien
  wget https://download.oracle.com/otn_software/linux/instantclient/2350000/oracle-instantclient-basiclite-23.5.0.24.07-1.el9.x86_64.rpm
  wget https://download.oracle.com/otn_software/linux/instantclient/2350000/oracle-instantclient-sqlplus-23.5.0.24.07-1.el9.x86_64.rpm
  wget https://download.oracle.com/otn_software/linux/instantclient/2350000/oracle-instantclient-odbc-23.5.0.24.07-1.el9.x86_64.rpm
  sudo alien -i --scripts oracle-instantclient*.rpm
  rm -f oracle-instantclient*.rpm  

# Configure ODBC
  cd /opt/oracle/instantclient_23_5
  mkdir etc
  cp /ect/odbcinst.ini etc/.
  cp ~/.odbc.ini etc/odbc.ini
  ./odbc_update_ini.sh .
  sudo cp etc/odbcinst.ini /etc/.
"
golem::add_dockerfile(port = 3838,
                      extra_sysreqs = oracle)

# In Terminal on computer with docker installed, run the following:
# docker build -f Dockerfile --progress=plain -t erarr_base .
# docker build -f Dockerfile --progress=plain -t erarr:latest .
# docker run -p 3838:3838 erarr:latest
# then go to 127.0.0.1:3838


## If you want to deploy to ShinyProxy
golem::add_dockerfile_with_renv_shinyproxy()


# Deploy to Posit Connect or ShinyApps.io
# In command line.
rsconnect::deployApp(
  appName = desc::desc_get_field("Package"),
  appTitle = desc::desc_get_field("Package"),
  appFiles = c(
    # Add any additional files unique to your app here.
    "R/",
    "inst/",
    "data/",
    "NAMESPACE",
    "DESCRIPTION",
    "app.R"
  ),
  appId = rsconnect::deployments(".")$appID,
  lint = FALSE,
  forceUpdate = TRUE
)
