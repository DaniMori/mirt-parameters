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
source("R/Constants.R",    encoding = 'UTF-8')
source("R/Formulae.R")
# source("R/Output.R")

## ---- CONSTANTS: -------------------------------------------------------------

## ----general-constants----

# Table manipulation constants:
DISCR_PARAMS_SELECTION <- quo(matches(DISCR_PARAMS_PATTERN))

# Numerical constants for computations:
TOL_LEVEL <- 5e-3 # for comparing computed and given multidimensional parameters

## ----set-cov-matrix----
# Version in Reckase, 2009, p. 183; this represents a "close-to-1D case";
#   dimension 2 covariance was changed by +.0001 (.3341 instead of .3340) to
#   avoid negative definiteness of the matrix, which would cause errors
#   computing the multidimensional parameters.
COV_MATRIX <- matrix(
  c(
    .1670, .2362, .2892,
    .2362, .3341, .4091,
    .2892, .4091, .5010
  ),
  nrow = 3
)
# (see also Reckase, 2009, p. 153 for the more common 3D case)

# # TODO: Delete
# COV_MATRIX <- diag(4)
# COV_MATRIX[1, 4] <- COV_MATRIX[4, 1] <- .4

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
  cov_matrix = COV_MATRIX,
  dir_out    = DEGREE_DIRTYPE
)

## ----create-params-table----

item_params <- items                                                     |>
  # Collapse parameters:
  select(item, !!DISCR_PARAMS_SELECTION, all_of(INTERCEPT_COLKEY))       |>
  full_join(items_orth,    by = ITEM_COLKEY)                             |>
  full_join(items_oblique, by = ITEM_COLKEY, suffix = c("_orth", "_ob")) |>
  # Reorder parameters:
  select(
    item, !!DISCR_PARAMS_SELECTION, all_of(INTERCEPT_COLKEY), # M2PL parameters
    starts_with(c("MDISC", "D"), ignore.case = FALSE),        # MDISC and D
    starts_with(DEGREE_DIRTYPE |> paste(1:4, sep = '_'))      # Direction angles
  )

## ----compose-output-table----

# Add blank columns to format table (separate different types of parameters)
item_params <- item_params                         |>
  add_column(sep1 = NA, .after = INTERCEPT_COLKEY) |>
  add_column(sep2 = NA, .after = "MDISC_ob")       |>
  add_column(sep3 = NA, .after = "D_ob")

# Table header (for flextable object):
item_headers <- tibble(
  col_keys     = item_params |> colnames(),
  metric       = col_keys %>% {
    case_when(
                 . == ITEM_COLKEY         ~ ITEM_TABLE_TITLE,
                 . == INTERCEPT_COLKEY    ~ MODEL_ACRONYM,
      str_detect(., DISCR_PARAMS_PATTERN) ~ MODEL_ACRONYM,
      str_detect(., "sep1")               ~ ' ',
      TRUE                                ~ MULTIDIM_PARAMS_TITLE
    )
  },
  metric_param = col_keys %>% {
    case_when(
      str_detect(., "MDISC_")       ~ "MDISC_i",
      str_detect(., "D_")           ~ as.character(DISTANCE_PARAM),
      str_detect(., DEGREE_DIRTYPE) ~ paste0(
                                        "\\alpha_{i",
                                        str_extract(., DIGIT_PATTERN),
                                        '}'
                                      ),
      str_detect(., "sep")            ~ ' ',
      TRUE                            ~ metric
    )
  },
  param_space  = col_keys %>% {
    case_when(
                 . == ITEM_COLKEY         ~ ITEM_TABLE_TITLE,
                 . == INTERCEPT_COLKEY    ~ "l_i",
      str_detect(., DISCR_PARAMS_PATTERN) ~ paste0(
                                              "a_{i",
                                              str_extract(., DIGIT_PATTERN),
                                              '}'
                                            ),
      str_detect(., "_orth")   ~ "orth.",
      str_detect(., "_ob")     ~ "obl.",
      str_detect(., "sep\\d+") ~ ' '
    )
  }
)

# Table formatted as a `flextable` object:
item_params_output <- item_params            |>
  flextable()                                |>
  set_header_df(item_headers)                |>
  merge_v(part = "header")                   |>
  merge_h(part = "header", i = 1:3)          |>
  mk_par(
    i = 2,
    j = item_headers |> mutate(metric |> str_detect("parameters")) |> pull(),
    part    = "header",
    value   = as_paragraph(as_equation(.)),
    use_dot = TRUE
  )                                          |>
  mk_par(
    i = 3,
    j = item_headers |> mutate(metric |> str_detect(MODEL_ACRONYM)) |> pull(),
    part    = "header",
    value   = as_paragraph(as_equation(.)),
    use_dot = TRUE
  )                                          |>
  style(
    i    = 1:2, j = c(2:5, 7:8, 10:11, 13:18),
    pr_c = fp_cell(border.bottom = fp_border(width = .5)),
    part = "header"
  )                                          |>
  theme_apa()                                |>
  colformat_double(j =     1, digits = 0)    |>
  colformat_double(j =  7:11, digits = 3)    |>
  colformat_double(j = 13:18, digits = 1)    |>
  style(
    pr_p = fp_par(
      padding.bottom = 2,
      padding.top    = 2,
      padding.left   = 4,
      padding.right  = 4
    ),
    part = "all"
  )                                          |>
  valign(valign = "bottom", part = "header") |>
  align(align = "center",   part = "header") |>
  align(align = "right",    part = "body")   |>
  fontsize(size = 12,       part = "header") |>
  fontsize(size = 10,       part = "body")   |>
  set_table_properties(layout = "autofit")
