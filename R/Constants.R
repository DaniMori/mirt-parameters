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


## ---- CONSTANTS: -------------------------------------------------------------

## File system:

### File names:
MANUSCRIPT_FILENAME <- "paper_M2PLM_parameters.Rmd"

### Project directories:
DATA_DIR       <- "dat"
OUTPUT_DIR     <- "output"
MANUSCRIPT_DIR <- file.path(OUTPUT_DIR, "paper_draft")

### Project paths:
MANUSCRIPT_PATH <- file.path(MANUSCRIPT_DIR, MANUSCRIPT_FILENAME)
