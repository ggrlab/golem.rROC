# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()
usethis::use_package("tibble")
usethis::use_package("restrictedROC")
usethis::use_package("data.table")
usethis::use_package("DT")
usethis::use_package("ggplot2")
usethis::use_package("readr")
usethis::use_package("writexl")
# packageurl <- "http://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_3.4.4.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

load("./data-raw/biodata.rda")
load("./data-raw/glehr2023_cd4_cd8_relative.rda")
frontiers110_tcell_relative__permutation_10k <- readRDS(
    "./data-raw/frontiers110_tcell_relative__permutation_10k.rds.rds"
)
for (dv_x in names(frontiers110_tcell_relative__permutation_10k)) {
    for (iv_x in names(frontiers110_tcell_relative__permutation_10k[[dv_x]])) {
        frontiers110_tcell_relative__permutation_10k[[dv_x]][[iv_x]][["plots"]] <- NULL
        frontiers110_tcell_relative__permutation_10k[[dv_x]][[iv_x]][["single_rROC"]] <- NULL
        frontiers110_tcell_relative__permutation_10k[[dv_x]][[iv_x]][["permutation"]][["perm_max_bound"]] <- NULL
        frontiers110_tcell_relative__permutation_10k[[dv_x]][[iv_x]][["permutation"]][["perm_global_bound"]] <- NULL
    }
}
frontiers110_tcell_relative__permutation_10k_small <- frontiers110_tcell_relative__permutation_10k
usethis::use_data(
    biodata,
    glehr2023_cd4_cd8_relative,
    frontiers110_tcell_relative__permutation_10k_small,
    internal = TRUE, overwrite = TRUE
)


## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "name_of_module1", with_test = TRUE) # Name of the module
# golem::add_module(name = "name_of_module2", with_test = TRUE) # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("helpers", with_test = TRUE)
golem::add_utils("helpers", with_test = TRUE)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("golem_rROC")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
