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
  
  # Unit "square" (delimited by the lines x = 0, x = 1, y = 0, y = 1) in
  #   transformed basis (generally a rhomboid), expressed in the canonical basis
  unit_square <- expand_grid(x = 0:1, y = 0:1) |>
    rownames_to_column(var = "point") |> # Unit square vertices
    pivot_longer(-point, names_to = "coord") |>
    group_by(point) |>
    mutate(value_transf = basis %*% value |> drop())

  # Transformed (1, 1), expressed in the canonical basis
  unit_point <- unit_square |>
    filter(all(value)) |>
    ungroup() |>
    select(coord, value_transf) |>
    deframe()
    
  # Grid specifications, in canonical basis
  grid_specs <- grid_bounds |> mutate(
    unit_intercept = unit_point[['y']] - slope * unit_point[['x']],
    n_units  = trunc(limit_intercept / unit_intercept),
    n_breaks = n_units %/% break_step * break_step
  )
  
  
  # Create grid data
  grid <- grid_specs |>
    select(grid, limit, slope, unit_intercept, n_breaks) |>
    filter(!is.infinite(slope)) |> # Filter out vertical grid lines
    pivot_wider(names_from = limit, values_from = n_breaks) |>
    group_by(grid, slope) |>
    reframe(
      label     = seq(from = inf, to = sup, by = break_step), # to axis
      intercept = label * unit_intercept # y for `x == 0` (canonical)
    ) |>
    # Compute the position of the axis labels (on the plot margins)
    mutate(
      label_pos = if_else(
        grid == "h",
         intercept   + x_limits[1] * vec_tan[1],
        (y_limits[1] - intercept)  / vec_tan[2]
      )
    )
  
  if (vec_tan |> is.infinite() |> any()) {
    
    # Create vertical grid data, if any of the basis vectors is vertical
    grid <- grid |> bind_rows(
      grid_bounds |>
        filter(is.infinite(slope)) |>
        mutate(
          unit     = basis[1, 1], # NOTE: May misbehave if axis 1 is vertical.
          n_units  = trunc(x / unit),
          n_breaks = n_units %/% break_step * break_step
        ) |>
        select(grid, slope, limit, n_breaks, unit) |>
        pivot_wider(names_from = "limit", values_from = n_breaks) |>
        group_by(across(grid:slope)) |>
        reframe(
          label     = seq(from = inf, to = sup, by = break_step),
          label_pos = label * unit,
        ) |>
        mutate(
          intercept = NA_real_
          # ref       = as.character(!label) # Reference axis (for `coord == 0`)
        )
    )
  }
  
  grid |> mutate(
    ref     = as.character(!label), # Reference axis (when `position == 0`)
    coord_x = if_else(grid == "v", label, 0), # Axis point coordinates
    coord_y = if_else(grid == "h", label, 0)  #   (x and y for both axes)
  ) |>
    # Transform coordinates with the basis matrix
    pivot_longer(coord_x:coord_y, names_to  = "coord") |>
    group_by(across(grid:ref)) |>
    mutate(value = basis %*% value |> drop()) |>
    ungroup() |>
    pivot_wider(names_from = coord, values_from = value)
}

plot_grid <- function(grid,
                      x_limits = c(-4, 4),
                      y_limits = c(-4, 4),
                      linetype_axis = 'solid',
                      linetype_grid = 'dashed',
                      linewidth     = .5,
                      axis_ticks    = c("margin", "axis", "both", "off"),
                      axis_lab_disp = c(.1, -.15)) {
  
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
  
  margin_ticks <- axis_ticks %in% c("margin", "both")
  axis_ticks   <- axis_ticks %in% c("axis",   "both")
  
  
  # Main: ----
  
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
  
  # Create vertical grid geometry if the second vector is completely vertical
  if (is.infinite(vec_tan[2])) {
    
    grid_plot <- grid_plot + geom_vline(
      mapping = aes(
        xintercept = label_pos,
        linetype   = ref,
        linewidth  = I(linewidth)
      ),
      data = grid |> filter(is.infinite(slope))
    )
  }
  
  grid_plot <- grid_plot +
    scale_x_continuous(
      minor_breaks = NULL,
      breaks = grid |> filter(grid == "v") |> pull(label_pos),
      labels = grid |> filter(grid == "v") |> pull(label),
      limits = x_limits,
      expand = expansion(),
      name   = NULL,
      sec.axis = dup_axis(name = "*&theta;*~2~", labels = NULL)
    ) +
    scale_y_continuous(
      minor_breaks = NULL,
      breaks = grid |> filter(grid == "h") |> pull(label_pos),
      labels = grid |> filter(grid == "h") |> pull(label),
      limits = y_limits,
      expand = expansion(),
      name   = NULL,
      sec.axis = dup_axis(name = "*&theta;*~1~", labels = NULL)
    ) +
    scale_linetype_manual(values = linetypes, guide = NULL) +
    coord_fixed() +
    theme(
      axis.ticks.x.top   = element_blank(),
      axis.ticks.y.right = element_blank(),
      axis.line          = element_blank(),
      axis.title.x       = element_markdown(hjust = y_axis_titpos),
      axis.title.y.right = element_markdown(vjust = x_axis_titpos, angle = 0)
    )
  
  if (margin_ticks) {
    
    grid_plot <- grid_plot + theme(
      axis.ticks  = element_line(),
      axis.text   = element_text()
    )
    
  } else {
    
    grid_plot <- grid_plot + theme(
      axis.ticks  = element_blank(),
      axis.text   = element_blank()
    )
  }
  
  if (axis_ticks) {
    
    axis_alignment <- axis_lab_disp[1] |>
      sign() |>
      case_match(-1 ~ "right", 0 ~ "center", 1 ~ "left")
    
    grid_plot <- grid_plot +
      geom_text(
        mapping  = aes(x = coord_x, y = coord_y, label = label),
        position = position_nudge(x = axis_lab_disp[1], y = axis_lab_disp[2]),
        family   = GRAPH_FONT,
        hjust    = axis_alignment
      )
  }
  
  grid_plot
}

transform_grid <- function(basis,
                           x_limits = c(-4, 4),
                           y_limits = c(-4, 4),
                           break_step = 2,
                           linetype_axis = 'solid',
                           linetype_grid = 'dashed',
                           linewidth = .5,
                           axis_ticks = c("margin", "axis", "both", "off"),
                           axis_lab_disp = c(.1, -.15)) {
  
  # Argument checking and formatting: ----
  axis_ticks <- match.arg(axis_ticks)
  
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
      linetype_axis = linetype_axis,
      linetype_grid = linetype_grid,
      linewidth = linewidth,
      axis_ticks = axis_ticks,
      axis_lab_disp = axis_lab_disp
    )
}
