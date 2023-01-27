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


## ---- CONSTANTS: -------------------------------------------------------------

## File system:

### File names:
MANUSCRIPT_FILENAME               <- "paper_M2PLM_parameters.Rmd"
ITEM_PARAMS_RECKASE_2009_FILENAME <- "Table_6.1_Reckase_2009.csv"
ITEM_PARAMS_TEZZA_2018_FILENAME   <- "Table_5_Tezza_et_al_2018.csv"

### Project directories:
DATA_DIR       <- "dat"
OUTPUT_DIR     <- "output"
MANUSCRIPT_DIR <- file.path(OUTPUT_DIR, "paper_draft")

### File paths:

MANUSCRIPT_PATH <- file.path(MANUSCRIPT_DIR, MANUSCRIPT_FILENAME)

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
MULTIDIM_PARAMS_TITLE <- "Multidimensional parameters"
MODEL_ACRONYM         <- "M2PL"
ITEM_TABLE_TITLE      <- "Item"
SPACE_SEP             <- ' '
