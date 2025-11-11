# ==============================================================================
# 
# FILE NAME:   Build_project.R
# DESCRIPTION: This script builds the complete project from scratch
# 
# AUTHOR:      Daniel Morillo (daniel.morillo@cibersam.es)
# 
# DATE:        27/07/2022
# 
# ==============================================================================


## ---- GLOBAL OPTIONS: --------------------------------------------------------

gc()
rm(list = ls())

# First re-install locally all the project environment:
#   WARNING: Please use R v4.4.1!!
renv::restore(prompt = FALSE)

## ---- INCLUDES: --------------------------------------------------------------

library(rmarkdown)
library(knitr)
library(xfun)

source("R/Constants.R", encoding = 'UTF-8')


## ---- CONSTANTS: -------------------------------------------------------------



## ---- FUNCTIONS: -------------------------------------------------------------


## ---- MAIN: ------------------------------------------------------------------

# Render article:

Rscript_call(render, list(input = MANUSCRIPT_PATH))
Rscript_call(render, list(input = SUPPLEMENT_PATH))
Rscript_call(render, list(input = COVERLETTER_PATH))
Rscript_call(render, list(input = RESPONSELETTER_1_PATH))
Rscript_call(render, list(input = RESPONSELETTER_2_PATH))
Rscript_call(render, list(input = RESPONSE_REVIEWERS_1_PATH))
Rscript_call(render, list(input = RESPONSE_REVIEWERS_2_PATH))
