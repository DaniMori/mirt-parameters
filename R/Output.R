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
library(ggplot2)
library(ggtext)
library(tibble)
library(dplyr)
library(tidyr)

# source("R/<file>", encoding = 'UTF-8')


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
  
  assertive.types::assert_is_numeric(values)
  assertive.numbers::assert_all_are_in_closed_range(values, -1L, 1L)
  assertive.properties::assert_is_scalar(sig)
  assertive.numbers::assert_all_are_whole_numbers(sig)
  assertive.types::assert_is_a_bool(drop_0)
  
  
  ## Main: ----
  
  result <- values      |>
    round(digits = sig) |>
    format(digits = sig, nsmall = sig)
  
  if (drop_0) result <- result |> stringr::str_remove(LEADING_ZERO_PATTERN)
  
  result
}

### Graphical functions ----

create_grid <- function(basis,
                        x_limits = c(-4, 4),
                        y_limits = c(-4, 4),
                        break_step = 2) {
  # Argument checking and formatting: ----
  
  ## Basis vectors tangents and norms
  vec_tan <- basis[2, ] / basis[1, ]
  
  
  # Main: ----
  
  # Define grid limits as a function of the axes limits (in canonical metric):
  grid_box <- expand_grid(x = x_limits, y = y_limits) |> mutate(
    x_lim = if_else(x == x_limits[1], "inf", "sup"),
    y_lim = if_else(y == y_limits[1], "inf", "sup")
  )
  
  # Select delimiting grid vertices, based on the basis vector directions
  grid_bounds <- bind_rows(
    h = grid_box |> mutate(slope = vec_tan[1], limit = y_lim),
    v = grid_box |> mutate(slope = vec_tan[2], limit = x_lim),
    .id = "grid"
  ) |>
    filter(xor(slope > 0, x_lim == y_lim)) |>
    mutate(limit_intercept = y - slope * x) # Compute their intercept
  
  # Transformed (1, 1), expressed in the canonical basis
  unit_point <- basis %*% c(1, 1) |> drop() |>
    as_tibble(.name_repair = "minimal") |>
    add_column(name = c('x', 'y') |> paste0('_unit')) |>
    pivot_wider(values_from = value)
  
  # Grid specifications, in canonical basis
  grid_specs <- grid_bounds |>
    bind_cols(unit_point) |>
    mutate(unit_intercept = y_unit - slope * x_unit) |>
    select(-ends_with("unit")) |>
    mutate(
      n_units  = trunc(limit_intercept / unit_intercept),
      n_breaks = n_units %/% break_step * break_step
    )
  
  # Create vertical grid data, if the second basis vector is completely vertical
  if (is.infinite(vec_tan[2])) {
    
    # Create vertical grid data
    v_grid <- grid_bounds |>
      filter(grid == "v") |>
      mutate(
        unit     = basis[1, 1],
        n_units  = trunc(x / unit),
        n_breaks = n_units %/% break_step * break_step
      ) |>
      select(grid, limit, n_breaks, unit) |>
      pivot_wider(names_from = "limit", values_from = n_breaks) |>
      reframe(coord = seq(from = inf, to = sup, by = break_step) * unit) |>
      mutate(
        grid      = 'v',
        slope     = Inf,
        intercept = NA_real_,
        label_pos = coord,
        ref       = as.character(!coord)
      )
    
    # Filter out non-valid grid values
    grid_specs <- grid_specs |> filter(!is.infinite(slope))
  }
  
  # Create grid data
  grid <- grid_specs |>
    select(grid, limit, slope, unit_intercept, n_breaks) |>
    pivot_wider(names_from = limit, values_from = n_breaks) |>
    group_by(grid) |>
    reframe(
      slope,
      coord = seq(from = inf, to = sup, by = break_step),
      intercept = coord * unit_intercept
    ) |>
    mutate(
      label_pos = if_else(
        grid == "h",
        intercept   + x_limits[1] * vec_tan[1],
        (y_limits[1] - intercept)  / vec_tan[2]
      ),
      ref = as.character(!intercept)
    )
  
  if (is.infinite(vec_tan[2])) return(grid |> bind_rows(v_grid))
  
  grid
}

plot_grid <- function(grid,
                      x_limits = c(-4, 4),
                      y_limits = c(-4, 4),
                      break_step = 2,
                      linetype_axis = 'solid',
                      linetype_grid = 'dashed',
                      linewidth     = .5,
                      axis_ticks    = FALSE) {
  
  # Argument checking and formatting: ----
  
  ## Grid plot linetypes
  linetypes <- c(`TRUE` = linetype_axis, `FALSE` = linetype_grid)
  
  ## Basis vectors tangents and norms
  vec_tan <- grid |> distinct(grid, slope) |> deframe()
  
  ## Axis title positions
  y_axis_titpos <- (y_limits[2] / vec_tan[2] - x_limits[1]) /
    (x_limits[2] - x_limits[1])
  x_axis_titpos <- (x_limits[2] * vec_tan[1] - y_limits[1]) /
    (y_limits[2] - y_limits[1])
  
  
  # Main: ----
  
  # Assign x axis label values and coordinates (to account for the vertical
  #   special case)
  x_labels <- grid |> filter(grid == "v") |> select(coord, label_pos)
  
  # Create vertical grid geometry if the second vector is completely vertical
  if (is.infinite(vec_tan[2])) {
    
    geom_vgrid <- geom_vline(
      mapping = aes(
        xintercept = coord,
        linetype   = ref,
        linewidth  = I(linewidth)
      ),
      data = grid |> filter(grid == "v")
    )
    
    grid <- grid |> filter(grid == "h")
  }
  
  grid_plot <- grid |>
    ggplot() +
    geom_abline(
      mapping = aes(
        intercept = intercept,
        slope     = slope,
        linetype  = ref,
        linewidth = I(linewidth)
      )
    )
  
  if (is.infinite(vec_tan[2])) grid_plot <- grid_plot + geom_vgrid
  
  grid_plot +
    scale_x_continuous(
      minor_breaks = NULL,
      breaks = x_labels |> pull(label_pos),
      labels = x_labels |> pull(coord),
      limits = x_limits,
      expand = expansion(),
      name   = NULL,
      sec.axis = dup_axis(name = "*&theta;*~2~", labels = NULL)
    ) +
    scale_y_continuous(
      minor_breaks = NULL,
      breaks = grid |> filter(grid == "h") |> pull(label_pos),
      labels = grid |> filter(grid == "h") |> pull(coord),
      limits = y_limits,
      expand = expansion(),
      name   = NULL,
      sec.axis = dup_axis(name = "*&theta;*~1~", labels = NULL)
    ) +
    scale_linetype_manual(values = linetypes, guide = NULL) +
    coord_fixed() +
    theme(
      axis.ticks         = if (axis_ticks) element_line() else element_blank(),
      axis.ticks.x.top   = element_blank(),
      axis.ticks.y.right = element_blank(),
      axis.line          = element_blank(),
      axis.title.x       = element_markdown(hjust = y_axis_titpos),
      axis.title.y.right = element_markdown(vjust = x_axis_titpos, angle = 0)
    )
}

transform_grid <- function(basis,
                           x_limits = c(-4, 4),
                           y_limits = c(-4, 4),
                           break_step = 2,
                           linetype_axis = 'solid',
                           linetype_grid = 'dashed',
                           linewidth = .5,
                           axis_ticks = FALSE) {
  
  # Argument checking and formatting: ----
  
  # Main: ----
  basis |>
    create_grid(
      x_limits   = x_limits,
      y_limits   = y_limits,
      break_step = break_step
    ) |>
    plot_grid(
      x_limits   = x_limits,
      y_limits   = y_limits,
      break_step = break_step,
      linetype_axis = linetype_axis,
      linetype_grid = linetype_grid,
      linewidth = linewidth,
      axis_ticks = axis_ticks
    )
}
