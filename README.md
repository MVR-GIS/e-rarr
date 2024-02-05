<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- rmarkdown::render(input="README.Rmd", output_file = "README.md", output_format = "md_document") -->

# erarr

Enterprise Risk Analysis Reporting in R: A Shiny Application for
producing Risk Analysis Reports in `R`

## Package Status

[![Maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle)
[![Project Status: Active The project has reached a stable, usable state
and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![packageversion](https://img.shields.io/badge/Package%20version-0.1.2-orange.svg?style=flat-square)](commits/master)
[![Last-changedate](https://img.shields.io/badge/last%20change-2024--02--05-yellowgreen.svg)](/commits/master)
[![Licence](https://img.shields.io/badge/licence-CC0-blue.svg)](http://choosealicense.com/licenses/cc0-1.0/)

## Description

This package contains a set of functions that are used to help produce a
Risk Analysis Report. These functions help automate and standardize
report production using the [`bookdown`
package](https://bookdown.org/yihui/bookdown/). These functions can be
grouped into three broad categories.

-   Access Data - These functions are used to extract data from USACE
    enterprise databases where the data is being entered and maintained
    by risk managers.
-   Format Pages - These functions are used to programmatically create
    report links and headers.
-   Produce Figures - These functions are used to create figures needed
    for risk reporting.

<img src="man/figures/HDQLO-03_h120.jpg" width=125 align="right" />

## Funding

Funding for development and maintenance of `erarr` has been provided by
the following US Army Corps of Engineers (USACE) programs:

-   [Navigation and Ecosystem Sustainability Program
    (NESP)](https://www.mvr.usace.army.mil/Rock-Island-District/Programs/NESP/)

------------------------------------------------------------------------

## Latest Updates

Check out the [News](news/index.html) for details on the latest updates.

------------------------------------------------------------------------

## Authors

-   [Barrie Chileen
    Martinez](mailto:barrie.v.chileenmartinez@usace.army.mil),
    Geographer, Rock Island District, U.S. Army Corps of Engineers
    <a itemprop="sameAs" content="https://orcid.org/0000-0002-6960-8167" href="https://0000-0002-6960-8167" target="orcid.widget" rel="me noopener noreferrer" style="vertical-align:top;">
    <img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon">https://orcid.org/0000-0002-6960-8167</a>

------------------------------------------------------------------------

## Install

To install the `erarr` package, install from GitHub using the `remotes`
package:

    remotes::install_github(repo = "MVR-GIS/erarr")

## Bug Reports

If you find any bugs using `erarr`, please open an
[issue](https://github.com/MVR-GIS/erarr/issues).
