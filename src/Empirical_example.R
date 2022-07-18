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

## ----sources----
source("R/Mirt_toolbox.R")
# source("R/Formulae.R")
# source("R/LaTeX_math.R")
# source("R/Output.R")

## ---- CONSTANTS: -------------------------------------------------------------

## ----constants----


## ---- CONFIGURATION: ---------------------------------------------------------

## ----graphical-output-conf----

## ---- MAIN: ------------------------------------------------------------------

## ----read-items----

# Item parameters:
items <- read_csv2("dat/Table_5_Tezza_et_al_2018.csv", )

## ----orthogonal-params----
items_orth <- items_mirt_params |>
  compute_mirt_params(d, starts_with('a'), dir_out = "deg")

## ----compare-orthogonal-params----

items                                                |>
  pull(MDISC)                                        |>
  is_equal_to(items_orth |> pull(MDISC), tol = 5e-3) |>
  all()

items                                            |>
  pull(MDIFF)                                    |>
  is_equal_to(items_orth |> pull(D), tol = 5e-3) |>
  all()


## ----oblique-params----

## Compute the correlation matrix:
corr_matrix <- diag(4)
# See Tezza et al., 2018, p. 927:
# > No presente estudo, a correlação entre a dimensão 1 e a dimensão 4
# > foi de aproximadamente 0,4.
corr_matrix[1, 4] <- corr_matrix[4, 1] <- .4

## Compute the parameters:
items_oblique <- items |> compute_mirt_params(
  d, starts_with('a'),
  cov_matrix = corr_matrix,
  dir_out = "deg"
)

## ----params-table----

# Collapse parameters:
item_params <- items                    |>
  select(item, starts_with('a'), l = d) |>
  full_join(items_orth,    by = "item") |>
  full_join(items_oblique, by = "item", suffix = c("_orth", "_ob"))

# Reorder parameters:
item_params <- item_params |> select(
  item, starts_with('a'), l,                         # Linear MIRT parameters
  starts_with(c("MDISC", "D"), ignore.case = FALSE), # MDISC and D
  starts_with('deg_' |> paste0(1:4))                 # direction angles
)

## ----compose-example-items-table----

# Add blank columns to format table (separate different types of parameters)
item_params <- item_params                   |>
  add_column(sep1 = NA, .after = "l")        |>
  add_column(sep2 = NA, .after = "MDISC_ob") |>
  add_column(sep3 = NA, .after = "D_ob")

# Table header (for flextable object):
item_headers <- tibble(
  col_keys = item_params %>% colnames(),
  param    = col_keys %>% {
    case_when(
      . == "item"              ~ "Item",
      str_detect(., "a\\d+")   ~ "Discrimination",
      . == 'l'                 ~ "l",
      str_detect(., "sep\\d+") ~ ' ',
      str_detect(., "MDISC_")  ~ "MDISC",
      str_detect(., "D_")      ~ "D",
      str_detect(., "deg_")    ~ "\\alpha",
    )
  },
  dim      = col_keys %>% {
    case_when(
      . == "item"              ~ "Item",
      . == 'l'                 ~ "l",
      str_detect(., "a\\d+")   ~ "Discrimination",
      str_detect(., "sep\\d+") ~ ' ',
      str_detect(., "\\d+")    ~ str_extract(., "\\d+"),
      str_detect(., "MDISC_")  ~ "MDISC",
      str_detect(., "D_")      ~ "D"
    )
  },
  space    = col_keys %>% {
    case_when(
      . == "item"              ~ "Item",
      . == 'l'                 ~ "l",
      str_detect(., "_orth")   ~ "orth.",
      str_detect(., "_ob")     ~ "obl.",
      str_detect(., "sep\\d+") ~ ' ',
      str_detect(., "\\d+")    ~ str_extract(., "\\d+")
    )
  }
)

# Table formatted as a `flextable` object:
item_params_output <- item_params          |>
  flextable()                              |>
  set_header_df(item_headers)              |>
  merge_v(part = "header")                 |>
  merge_h(part = "header", i = 1:2)        |>
  mk_par(
    i = 1, j = item_headers$col_keys |> str_detect("^deg_"),
    part    = "header",
    value   = as_paragraph(as_equation(.)),
    use_dot = TRUE
  )                                        |>
  colformat_double(digits = 2)             |>
  theme_vanilla()                          |>
  align(align = "center", part = "header") |>
  set_table_properties(layout = "autofit") |>
  fontsize(size = 12, part = "all")        |>
  font(part = "all", fontname = "Times New Roman")
