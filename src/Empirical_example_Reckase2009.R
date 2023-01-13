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
source("R/Mirt_toolbox.R")
# source("R/Formulae.R")
# source("R/LaTeX_math.R")
# source("R/Output.R")

## ---- CONSTANTS: -------------------------------------------------------------

## ----constants----
COV_MATRIX <- matrix(
  c(
    1.210, .297, 1.232,
     .297, .810,  .252,
    1.232, .252, 1.960
  ),
  nrow = 3
)
# Version in Reckase, 2009, p. 153 (see also p. 183 for the close to 1D case)

DISCR_PARAMS_PATTERN   <- '^a\\d+$'
DISCR_PARAMS_SELECTION <- quo(matches(DISCR_PARAMS_PATTERN))

## ---- CONFIGURATION: ---------------------------------------------------------

## ----graphical-output-conf----

## ---- MAIN: ------------------------------------------------------------------

## ----read-items----

# Item parameters:
items <- read_csv2("dat/Table_6.1_Reckase_2009.csv")

## ----orthogonal-params----
items_orth <- items |>
  compute_mirt_params(d, !!DISCR_PARAMS_SELECTION, dir_out = "deg")

## ----compare-orthogonal-params----

items                                                |>
  pull(MDISC)                                        |>
  is_equal_to(items_orth |> pull(MDISC), tol = 5e-3) |>
  all()

items                                            |>
  pull(MDIFF)                                    |>
  is_equal_to(items_orth |> pull(D), tol = 5e-3) |>
  all()
# Both multidimensional parameters are equal for all items, up to the tolerance
#   level.

## ----oblique-params----

## Compute the parameters:
items_oblique <- items |> compute_mirt_params(
  d, !!DISCR_PARAMS_SELECTION,
  cov_matrix = COV_MATRIX,
  dir_out = "deg"
)

## ----params-table----

# Collapse parameters:
item_params <- items                            |>
  select(item, !!DISCR_PARAMS_SELECTION, l = d) |>
  full_join(items_orth,    by = "item")         |>
  full_join(items_oblique, by = "item", suffix = c("_orth", "_ob"))

# Reorder parameters:
item_params <- item_params |> select(
  item, !!DISCR_PARAMS_SELECTION, l,                 # Linear MIRT parameters
  starts_with(c("MDISC", "D"), ignore.case = FALSE), # MDISC and D
  starts_with('deg_' |> paste0(1:4))                 # direction angles
)

## ----compose-empirical-items-table----

# Add blank columns to format table (separate different types of parameters)
item_params <- item_params                   |>
  add_column(sep1 = NA, .after = "l")        |>
  add_column(sep2 = NA, .after = "MDISC_ob") |>
  add_column(sep3 = NA, .after = "D_ob")

# Table header (for flextable object):
item_headers <- tibble(
  col_keys     = item_params |> colnames(),
  metric       = col_keys %>% {
    case_when(
                 . == "item"              ~ "Item",
                 . == 'l'                 ~ "M2PL",
      str_detect(., DISCR_PARAMS_PATTERN) ~ "M2PL",
      str_detect(., "sep1")               ~ ' ',
      TRUE                                ~ "Multidimensional parameters"
    )
  },
  metric_param = col_keys %>% {
    case_when(
      str_detect(., "MDISC_") ~ "MDISC_i",
      str_detect(., "D_")     ~ "D_i",
      str_detect(., "deg_")   ~ paste0(
                                  "\\alpha_{i",
                                  str_extract(., "\\d+"),
                                  '}'
                                ),
      str_detect(., "sep")    ~ ' ',
      TRUE                    ~ metric
    )
  },
  param_space  = col_keys %>% {
    case_when(
                 . == "item"   ~ "Item",
                 . == 'l'      ~ "l_i",
      str_detect(., DISCR_PARAMS_PATTERN) ~ paste0(
                                              "a_{i",
                                              str_extract(., "\\d+"),
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
    j = item_headers |> mutate(metric |> str_detect("M2PL")) |> pull(),
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
