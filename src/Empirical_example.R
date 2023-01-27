# ==============================================================================
# 
# FILE NAME:   Empirical_example.R
# DESCRIPTION: Script for computing the empirical example for the paper.
# 
# AUTHOR:      Mori (danivmorillo@gmail.com)
# 
# DATE:        18/07/2022
# 
# ==============================================================================


## ---- GLOBAL OPTIONS: --------------------------------------------------------

## ---- INCLUDES: --------------------------------------------------------------

## ----libraries----
library(tidyverse)
library(flextable)
library(officer)
library(assertive.numbers)

## ----sources----
source("R/Mirt_toolbox.R", encoding = 'UTF-8')
source("R/Formulae.R",     encoding = 'UTF-8')
source("R/Constants.R",    encoding = 'UTF-8')

## ---- CONSTANTS: -------------------------------------------------------------

## ----general-constants----

# Table manipulation constants:
DISCR_PARAMS_SELECTION <- quo(matches(DISCR_PARAMS_PATTERN))

# Numerical constants for computations:
TOL_LEVEL <- 5e-3 # for comparing computed and given multidimensional parameters

# The original "\\mathbf{\\upSigma}" raises an error in flextable (KaTeX):
COV_MATRIX <- "\\mathbf{\\Sigma}"

## ----set-cov-matrix----
# Version in Reckase, 2009, p. 183; this represents a "close-to-1D case";
#   dimension 2 covariance was changed by +.0001 (.3341 instead of .3340) to
#   avoid negative definiteness of the matrix, which would cause errors
#   computing the multidimensional parameters.
COV_MATRIX_VALUE <- matrix(
  c(
    .1670, .2362, .2892,
    .2362, .3341, .4091,
    .2892, .4091, .5010
  ),
  nrow = 3
)
# (see also Reckase, 2009, p. 153 for the more common 3D case)

## ---- CONFIGURATION: ---------------------------------------------------------

## ----graphical-output-conf----

## ---- MAIN: ------------------------------------------------------------------

## ----read-items----

# Item parameters:
items <- read_csv2(ITEM_PARAMS_RECKASE_2009_FILEPATH)

# # TODO: Delete
# items <- read_csv2(ITEM_PARAMS_TEZZA_2018_FILEPATH)

## ----compute-orthogonal-params----
items_orth <- items |>
  compute_mirt_params(d, !!DISCR_PARAMS_SELECTION, dir_out = DEGREE_DIRTYPE)

## ----compare-orthogonal-params----

cat("\nComputed multidimensional discrimination parameters match the table: ")
items                                                     |>
  pull(MDISC)                                             |>
  is_equal_to(items_orth |> pull(MDISC), tol = TOL_LEVEL) |>
  all()                                                   |>
  cat()

cat("\nComputed multidimensional location parameters match the table: ")
items                                                 |>
  pull(MDIFF)                                         |>
  is_equal_to(items_orth |> pull(D), tol = TOL_LEVEL) |>
  all()                                               |>
  cat()
# Both multidimensional parameters are equal for all items, up to the tolerance
#   level.

## ----compute-oblique-params----

## Compute the parameters:
items_oblique <- items |> compute_mirt_params(
  d, !!DISCR_PARAMS_SELECTION,
  cov_matrix = COV_MATRIX_VALUE,
  dir_out    = DEGREE_DIRTYPE
)

## ----create-params-table----

item_params <- items                                               |>
  # Collapse parameters:
  select(item, !!DISCR_PARAMS_SELECTION, all_of(INTERCEPT_COLKEY)) |>
  full_join(items_orth,    by = ITEM_COLKEY)                       |>
  full_join(
    items_oblique,
    by     = ITEM_COLKEY,
    suffix = paste0(UNDERSCORE, c(ORTH_SUFFIX, OBL_SUFFIX))
  )                                                                |>
  # Reorder parameters:
  select(
    item, !!DISCR_PARAMS_SELECTION, all_of(INTERCEPT_COLKEY),     # M2PL params
    starts_with(c(MDISC_SYM, DISTANCE_SYM), ignore.case = FALSE), # MDISC and D
    starts_with(DEGREE_DIRTYPE |> paste(1:4, sep = UNDERSCORE))   # Dir angles
  )

## ----compose-output-table----

# Add blank columns to format table (separate different types of parameters)
item_params <- item_params                          |>
  add_column(sep_1 = NA, .after = INTERCEPT_COLKEY) |>
  add_column(
    sep_2  = NA,
    .after = glue("{MDISC_SYM}{UNDERSCORE}{OBL_SUFFIX}") |> as.character()
  )                                                 |>
  add_column(
    sep_3  = NA,
    .after = glue("{DISTANCE_SYM}{UNDERSCORE}{OBL_SUFFIX}") |> as.character()
  )

# Table header (for flextable object):
item_headers <- tibble(
  col_keys     = item_params |> colnames(),
  metric       = col_keys %>% {
    case_when(
                 . == ITEM_COLKEY                       ~ ITEM_TABLE_TITLE,
                 . == INTERCEPT_COLKEY                  ~ MODEL_ACRONYM,
      str_detect(., DISCR_PARAMS_PATTERN)               ~ MODEL_ACRONYM,
      str_detect(., glue("{SEP_PREFFIX}{UNDERSCORE}1")) ~ SPACE_SEP,
      TRUE                                              ~ MULTIDIM_PARAMS_TITLE
    )
  },
  metric_param = col_keys %>% {
    case_when(
      str_detect(., MDISC_SYM)      ~ as.character(MDISC_ITEM),
      str_detect(., DISTANCE_SYM)   ~ as.character(DISTANCE_PARAM),
      str_detect(., DEGREE_DIRTYPE) ~ ANGLE_TS_ITEM |> str_replace(
                                        DIM_INDEX,
                                        str_extract(., DIGIT_PATTERN)
                                      ),
      str_detect(., SEP_PREFFIX)    ~ SPACE_SEP,
      TRUE                          ~ metric
    )
  },
  param_space  = col_keys %>% {
    case_when(
                 . == ITEM_COLKEY         ~ ITEM_TABLE_TITLE,
                 . == INTERCEPT_COLKEY    ~ as.character(INTERCEPT_PARAM),
      str_detect(., DISCR_PARAMS_PATTERN) ~ DISCR_PARAM |> str_replace(
                                              DIM_INDEX,
                                              str_extract(., DIGIT_PATTERN)
                                            ),
      str_detect(., ORTH_SUFFIX)          ~ glue("{AGNOSTIC_SUBINDEX}{DOT}") |>
                                              as.character(),
      str_detect(., OBL_SUFFIX)           ~ COV_MATRIX |> as.character(),
      TRUE                                ~ metric_param
    )
  }
)

# Column (logical) indices for indexing the flextable output:
m2pl_params_index        <- item_headers |>
  transmute(metric == MODEL_ACRONYM)     |>
  pull()
cov_based_index          <- item_headers        |>
  transmute(col_keys |> str_detect(OBL_SUFFIX)) |>
  pull()
param_space_eq_index     <- m2pl_params_index | cov_based_index
multidim_scalars_index   <- item_headers                                  |>
  transmute(col_keys |> str_detect(glue("({MDISC_SYM}|{DISTANCE_SYM})"))) |>
  pull()
multidim_angles_index    <- item_headers            |>
  transmute(col_keys |> str_detect(DEGREE_DIRTYPE)) |>
  pull()
multidim_params_index    <- multidim_scalars_index | multidim_angles_index
item_num_index           <- item_headers |>
  transmute(col_keys == ITEM_COLKEY)     |>
  pull()
separator_index          <- item_headers |>
  transmute(metric_param == SPACE_SEP)   |>
  pull()
param_space_header_index <- !item_num_index & !separator_index

# Table formatted as a `flextable` object:
item_params_output <- item_params                          |>
  flextable()                                              |>
  set_header_df(item_headers)                              |>
  merge_v(part = "header")                                 |>
  merge_h(part = "header", i = 1:3)                        |>
  mk_par(
    i = 2, j = multidim_params_index,
    part    = "header",
    value   = as_paragraph(as_equation(.)),
    use_dot = TRUE
  )                                                        |>
  mk_par(
    i = 3, j = param_space_eq_index,
    part    = "header",
    value   = as_paragraph(as_equation(.)),
    use_dot = TRUE
  )                                                        |>
  style(
    i    = 1:2, j = param_space_header_index,
    pr_c = fp_cell(border.bottom = fp_border(width = .5)),
    part = "header"
  )                                                        |>
  theme_apa()                                              |>
  colformat_double(j = item_num_index,         digits = 0) |>
  colformat_double(j = multidim_scalars_index, digits = 3) |>
  colformat_double(j = multidim_angles_index,  digits = 1) |>
  style(
    pr_p = fp_par(
      padding.bottom = 2,
      padding.top    = 2,
      padding.left   = 4,
      padding.right  = 4
    ),
    part = "all"
  )                                                        |>
  valign(valign = "bottom", part = "header")               |>
  align(align = "center",   part = "header")               |>
  align(align = "right",    part = "body")                 |>
  fontsize(size = 12,       part = "header")               |>
  fontsize(size = 10,       part = "body")                 |>
  set_table_properties(layout = "autofit")
