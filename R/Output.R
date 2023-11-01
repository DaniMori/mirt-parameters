# ==============================================================================
# 
# FILE NAME:   Output.R
# DESCRIPTION: Functions for output formatting
# 
# AUTHOR:      Mori (danivmorillo@gmail.com)
# 
# DATE:        03/06/2021
# 
# ==============================================================================


## ---- GLOBAL OPTIONS: --------------------------------------------------------


## ---- INCLUDES: --------------------------------------------------------------

library(officer)

# source("R/<file>")


## ---- CONSTANTS: -------------------------------------------------------------

### Mathematical constants ----


## ---- FUNCTIONS: -------------------------------------------------------------

insert_anchor <- function(anchor_id, bg_color = "white") {
  
  assertive.strings::assert_is_a_string(anchor_id)
  
  ftext(
    paste0("{#", anchor_id, "}"), prop = fp_text(color = bg_color))
}

format_prop_like <- function(values, sig = 3, drop_0 = TRUE) {
  
  ## Constants: ----
  LEADING_ZERO_PATTERN <- "0(?=\\.)"
  
  ## Argument checking and formatting: ----
  
  assertive.numbers::assert_is_numeric(values)
  assertive.numbers::assert_all_are_in_closed_range(values, -1L, 1L)
  assertive.types::assert_is_scalar(sig)
  assertive.numbers::assert_all_are_whole_numbers(sig)
  assertive.types::assert_is_a_bool(drop_0)
  
  
  ## Main: ----
  
  result <- values      |>
    round(digits = sig) |>
    format(digits = sig, nsmall = sig)
  
  if (drop_0) result <- result |> str_remove(LEADING_ZERO_PATTERN)
  
  result
}
