# ==============================================================================
# 
# FILE NAME:   Constants.R
# DESCRIPTION: Constant objects of the project
# 
# AUTHOR:      Daniel Morillo (daniel.morillo@cibersam.es)
# 
# DATE:        17/12/2021
# 
# ==============================================================================


## ---- INCLUDES: --------------------------------------------------------------

library(glue)
library(flextable)

source("R/Formulae.R", encoding = 'UTF-8')

## ---- CONSTANTS: -------------------------------------------------------------

## File system:

### File names:
MANUSCRIPT_FILENAME               <- "Main_text.Rmd"
SUPPLEMENT_FILENAME               <- "Supplementary_material.Rmd"
TITLEPAGE_FILENAME                <- "Title_page.Rmd"
COVERLETTER_FILENAME              <- "Cover_letter.Rmd"
ITEM_PARAMS_RECKASE_2009_FILENAME <- "Table_6.1_Reckase_2009.csv"
ITEM_PARAMS_TEZZA_2018_FILENAME   <- "Table_5_Tezza_et_al_2018.csv"

### Project directories:
DATA_DIR       <- "dat"
OUTPUT_DIR     <- "output"

### File paths:

MANUSCRIPT_PATH  <- file.path(OUTPUT_DIR, MANUSCRIPT_FILENAME)
SUPPLEMENT_PATH  <- file.path(OUTPUT_DIR, SUPPLEMENT_FILENAME)
TITLEPAGE_PATH   <- file.path(OUTPUT_DIR, TITLEPAGE_FILENAME)
COVERLETTER_PATH <- file.path(OUTPUT_DIR, COVERLETTER_FILENAME)

ITEM_PARAMS_RECKASE_2009_FILEPATH <- file.path(
  DATA_DIR,
  ITEM_PARAMS_RECKASE_2009_FILENAME
)
ITEM_PARAMS_TEZZA_2018_FILEPATH <- file.path(
  DATA_DIR,
  ITEM_PARAMS_TEZZA_2018_FILENAME
)

# Data manipulation objects:
ITEM_COLKEY          <- "item"
DEGREE_DIRTYPE       <- "deg"
COSINE_DIRTYPE       <- "cos"
DISCR_PREFFIX        <- 'a'
SEP_PREFFIX          <- 'sep'
INTERCEPT_COLKEY     <- 'd'
ORTH_SUFFIX          <- "orth"
OBL_SUFFIX           <- "obl"
DIGIT_PATTERN        <- '\\d'
UNDERSCORE           <- '_'
DOT                  <- '.'
DISCR_PARAMS_PATTERN <- glue('^{DISCR_PREFFIX}{DIGIT_PATTERN}$')

# Output formatting objects:

## Title elements:
MULTIDIM_PARAMS_TITLE <- "Multidimensional parameters"
MODEL_ACRONYM         <- "M2PL"
ITEM_TABLE_TITLE      <- "Item"
SPACE_SEP             <- ' '
AGNOSTIC_ABBR         <- glue("{AGNOSTIC_SUBINDEX}{DOT}") |> as.character()

## Footer elements:
FOOTER_PREFFIX        <- as_i("Note. ")
MODEL_EXPLANATION     <- glue(
  MODEL_ACRONYM,
  " = Multidimensional 2-parameter logistic model; "
)
CORR_EXPLANATION      <- list(
  as_equation(as.character(CORR)),
  " = Correlation; "
)
DISCR_EXPLANATION     <- list(
  as_equation(as.character(DISCR_PARAM)),
  " = Item discrimination parameter (in dimension ",
  as_equation(as.character(DIM_INDEX)),
  "); "
)
INTERCEPT_EXPLANATION <- list(
  as_equation(as.character(INTERCEPT_PARAM)),
  " = Item intercept parameter; "
)
MDISC_EXPLANATION     <- list(
  as_equation(as.character(MDISC_ITEM)),
  " = Multidimensional item discrimination parameter; "
)
DISTANCE_EXPLANATION  <- list(
  as_equation(as.character(DISTANCE_PARAM)),
  " = Distance component of the multidimensional item location parameter; "
)
ANGLE_EXPLANATION     <- list(
  as_equation(as.character(ANGLE_TS_ITEM)),
  " = Direction component of the multidimensional item location parameter",
  " (in dimension ", as_equation(as.character(DIM_INDEX)), ")"
)
CORR_VER_EXPLANATION  <-
  "The multidimensional components are the correlation-based version"
AG_VER_EXPLANATION    <- glue("{AGNOSTIC_ABBR} = Agnostic version; ")
COV_VER_EXPLANATION   <- list(
  as_equation(as.character(COV_MATRIX)),
  " = Covariance-based version."
)
