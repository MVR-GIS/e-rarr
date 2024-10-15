FROM rocker/verse:4.4.0
RUN apt-get update && apt-get upgrade -y && apt-get install -y  git gsfonts libcurl4-openssl-dev libfontconfig1-dev libfreetype6-dev libgit2-dev libicu-dev libmagick++-dev libpng-dev libssl-dev libxml2-dev make pandoc zlib1g-dev unixodbc unixodbc-dev libaio1 alien
# RUN wget https://download.oracle.com/otn_software/linux/instantclient/2350000/oracle-instantclient-basiclite-23.5.0.24.07-1.el9.x86_64.rpm
# RUN wget https://download.oracle.com/otn_software/linux/instantclient/2350000/oracle-instantclient-sqlplus-23.5.0.24.07-1.el9.x86_64.rpm
# RUN wget https://download.oracle.com/otn_software/linux/instantclient/2350000/oracle-instantclient-odbc-23.5.0.24.07-1.el9.x86_64.rpm
# RUN sudo alien -i --scripts oracle-instantclient*.rpm
# RUN rm -f oracle-instantclient*.rpm
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/ /etc/shiny-server
RUN echo "sanitize_errors off;\ndisable_protocols xdr-streaming xhr-streaming iframe-eventsource iframe-htmlfile websocket xdr-polling;" | tee /etc/shiny-server/shiny-server.conf
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "1.1.4")' && \
    Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.48")' && \
    Rscript -e 'remotes::install_version("bslib",upgrade="never", version = "0.7.0")' && \
    Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.5.1")' && \
    Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "1.0.2")' && \
    Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.1.4")' && \
    Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.27")' && \
    Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.8.1.1")' && \
    Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.3.1")' && \
    Rscript -e 'remotes::install_version("htmlwidgets",upgrade="never", version = "1.6.4")' && \
    Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.5.1")' && \
    Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.2")' && \
    Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.2.1.1")' && \
    Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.3.1")' && \
    Rscript -e 'remotes::install_version("usethis",upgrade="never", version = "3.0.0")' && \
    Rscript -e 'remotes::install_version("webshot",upgrade="never", version = "0.5.5")' && \
    Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")' && \
    Rscript -e 'remotes::install_version("shinyalert",upgrade="never", version = "3.1.0")' && \
    Rscript -e 'remotes::install_version("readr",upgrade="never", version = "2.1.5")' && \
    Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.10.4")' && \
    Rscript -e 'remotes::install_version("kableExtra",upgrade="never", version = "1.4.0")' && \
    Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.4.1")' && \
    Rscript -e 'remotes::install_version("ggrepel",upgrade="never", version = "0.9.5")' && \
    Rscript -e 'remotes::install_version("formattable",upgrade="never", version = "0.2.1")' && \
    Rscript -e 'remotes::install_version("forcats",upgrade="never", version = "1.0.0")' && \
    Rscript -e 'remotes::install_version("english",upgrade="never", version = "1.2-6")' && \
    Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.33")' && \
    Rscript -e 'remotes::install_version("bsicons",upgrade="never", version = "0.1.2")'

RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 3838
ENV SHINY_LOG_STDERR=1
CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');library(erarr);erarr::run_app()"
