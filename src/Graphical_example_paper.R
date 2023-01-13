# ==============================================================================
# 
# FILE NAME:   Graphical_example_paper.R
# DESCRIPTION: Script for computing and plotting the graphical representation
#              example in the paper.
# 
# AUTHOR:      Mori (danivmorillo@gmail.com)
# 
# DATE:        18/02/2022
# 
# ==============================================================================


## ---- GLOBAL OPTIONS: --------------------------------------------------------

## ---- INCLUDES: --------------------------------------------------------------

## ----libraries----
library(tidyverse)
library(flextable)
library(scales)

## ----sources----
source("R/Mirt_toolbox.R")
source("R/Formulae.R")
source("R/LaTeX_math.R")
source("R/Output.R")

## ---- CONSTANTS: -------------------------------------------------------------

## ----constants----

# Graphical representation parameters:
GRAPH_FONT   <- "serif"
LINE_WIDTH   <- .1
VECTOR_WIDTH <- .3
# POINT_CHAR   <- 19L
PALETTE      <- c("darkred", "darkgoldenrod3", "green3", "cyan3", "blue3")

# Item parameters:
items_MCLM <- tribble(
  ~a_1, ~a_2, ~l  ,
   2  ,  2  ,  0  ,
   2  , -1  ,  1  ,
  -0.5,  1  , -0.5,
   0  ,  1.2,  0.5,
  -0.5, -1  , -0.5
)

# Oblique latent space correlation:
CORR_VALUE   <- .5
CORR_ARC     <- acos(CORR_VALUE)
CORR_ARC_SIN <- sin(CORR_ARC)
CORR_ARC_TAN <- tan(CORR_ARC)

corr_out <- latex_eq(CORR, CORR_VALUE |> format_prop_like(sig = 1))


## ---- CONFIGURATION: ---------------------------------------------------------

## ----graphical-output-conf----
theme_set( # `ggplot` output configuration
  theme_classic(
    base_size      = 12,
    base_family    = GRAPH_FONT,
    base_line_size = LINE_WIDTH
  ) %+replace%
    theme(
      axis.title.x = element_text(hjust = 1),
      axis.title.y = element_text(vjust = 1, angle = 0)
    )
)

## ---- MAIN: ------------------------------------------------------------------

## ----compute-example-items----

# Item parameters:
items_MCLM <- items_MCLM |> rownames_to_column(var = "item")

# Orthogonal case:
items_orth <- items_MCLM |> compute_mirt_params(l, starts_with('a_'))
items_orth <- items_orth |>
  select(-(rad_1:rad_2)) |>
  full_join(
    items_orth |> compute_mirt_coords(
      D, MDISC, starts_with('cos_'),
      original_coords = FALSE
    ),
    by = "item"
  )

# Oblique case:

## Compute the transformation matrix P and the correlation matrix R:
u_1_transf                  <- c(1, - CORR_VALUE / CORR_ARC_SIN)
u_2_transf                  <- c(0,      1 / CORR_ARC_SIN)
transform_matrix            <- cbind(u_1_transf, u_2_transf) |> unname()
transform_matrix_inv        <- solve(transform_matrix)
transform_matrix_transp     <- t(transform_matrix)
transform_matrix_inv_transp <- t(transform_matrix_inv)
inner_prod_matrix           <- transform_matrix_transp %*% transform_matrix
corr_matrix                 <- solve(inner_prod_matrix)
                       # == transform_matrix_inv %*% transform_matrix_inv_transp
sqrt_diag_inner_prod_matrix <- diag(inner_prod_matrix) |> diag() |> sqrt()

## Compute parameters and coordinates:
items_oblique <- items_MCLM |>
  compute_mirt_params(l, starts_with('a_'), cov_matrix = corr_matrix)
items_oblique <- items_oblique |>
  select(-(rad_1:rad_2))       |>
  full_join(
    items_oblique |> compute_mirt_coords(
      D, MDISC, starts_with('cos_'),
      transform       = transform_matrix,
      original_coords = FALSE
    ),
    by = "item"
  )


item_params <- full_join(
  items_orth    |> select(item:deg_2, -starts_with("cos")),
  items_oblique |> select(item:deg_2, -starts_with("cos")),
  by     = "item",
  suffix = c("_orth", "_ob")
)

item_params <- full_join(items_MCLM, item_params, by = "item")


## ----compose-example-items-table----

# Add blank columns to format table (separate different types of parameters)
item_params <- item_params            |>
  add_column(sep1 = NA, .after = "l") |>
  add_column(sep2 = NA, .after = "deg_2_orth")

# Table header (for flextable object):
item_headers <- tibble(
  col_keys = item_params %>% colnames(),
  metric   = c(
    "Item",
    "M2PL"                        |> rep(3),
    " ",
    "Multidimensional parameters" |> rep(9)
  ),
  corr     = c(
    "Item",
    "MCLM"       |> rep(3),
    " ",
    latex_eq(CORR, 0L) |> rep(4),
    " ",
    corr_out |> rep(4)
  ),
  param    = c(
    "Item",
    "a_{i1}",
    "a_{i2}",
    INTERCEPT_PARAM,
    c(" ", "MDISC_i", DISTANCE_PARAM, "\\alpha_{i1}", "\\alpha_{i2}") |> rep(2)
  )
)

# Table formatted as a `flextable` object:
item_params_output <- item_params                 |>
  flextable()                                     |>
  set_header_df(item_headers)                     |>
  merge_v(part = "header")                        |>
  merge_h(part = "header", i = 1:2)               |>
  mk_par(
    i = 2:3, j = c(2:4, 6:9, 11:14),
    part    = "header",
    value   = as_paragraph(as_equation(.)),
    use_dot = TRUE
  )                                               |>
  colformat_double(j = -(1:4), digits = 3)        |>
  colformat_double(j = c(8:9, 13:14), digits = 1) |>
  theme_vanilla()                                 |>
  align(align = "center", part = "header")        |>
  set_table_properties(layout = "autofit")        |>
  fontsize(size = 12, part = "all")               |>
  font(part = "all", fontname = "Times New Roman")

## ----compose-oblique-plot----
plot_oblique <- items_oblique |>
  arrange(desc(item))         |> # To plot them in reverse order
  ggplot(
    aes(
      origin_transf_1, origin_transf_2,
      xend  = end_transf_1, yend = end_transf_2,
      color = item, fill = item
    ),
  )                                                         +
  geom_abline(slope = CORR_ARC_TAN, linewidth = LINE_WIDTH) +
  geom_abline(
    slope     = CORR_ARC_TAN,
    intercept = -CORR_ARC_TAN * (-4:4),
    linewidth = LINE_WIDTH,
    linetype  = "17"
  )                                                         +
  geom_hline(yintercept = 0, linewidth = LINE_WIDTH)        +
  geom_segment(
    arrow     = arrow(angle = 20, length = unit(10, "points"), type = "closed"),
    linejoin  = "mitre",
    linewidth = LINE_WIDTH
  )                                                         +
  scale_x_continuous(
    limits       = c(-2, 3),
    breaks       = (0:4) - 2 / CORR_ARC_TAN,
    labels       = 0:4,
    minor_breaks = NULL,
    name         = NULL,
    oob          = oob_keep
  )                                                         +
  scale_y_continuous(
    limits       = c(-2, 2.8),
    breaks       = -3:3 * cos(CORR_VALUE),
    labels       = function(x) x / cos(CORR_VALUE),
    minor_breaks = 0,
    name         = NULL,
    oob          = oob_keep
  )                                                         +
  scale_color_manual(values = PALETTE, guide = NULL)        +
  coord_fixed(expand = FALSE, clip = "on")                  +
  theme(
    axis.line          = element_blank(),
    panel.grid.major.y = element_line(
      color     = "black",
      linewidth = LINE_WIDTH,
      linetype  = "17"
    )
  )

## ----compose-orthogonal-plot----

plot_orth <- items_orth |>
  arrange(desc(item))   |> # To plot them in reverse order
  ggplot(
    aes(
      origin_transf_1, origin_transf_2,
      xend  = end_transf_1, yend = end_transf_2,
      color = item, fill = item
    ),
  )                                                  +
  geom_vline(xintercept = 0, linewidth = LINE_WIDTH) +
  geom_hline(yintercept = 0, linewidth = LINE_WIDTH) +
  geom_segment(
    arrow     = arrow(angle = 20, length = unit(10, "points"), type = "closed"),
    linejoin  = "mitre",
    linewidth = LINE_WIDTH
  ) +
  scale_x_continuous(
    limits       = c(-2.5, 2.5),
    breaks       = -2:2,
    minor_breaks = NULL,
    name         = NULL,
    oob          = oob_keep
  ) +
  scale_y_continuous(
    limits       = c(-2, 2.8),
    breaks       = -3:3,
    name         = NULL,
    oob          = oob_keep
  ) +
  scale_color_manual(values = PALETTE)               +
  coord_fixed(expand = FALSE, clip = "on")           +
  theme(
    axis.line        = element_blank(),
    panel.grid.major = element_line(
      color     = "black",
      linewidth = LINE_WIDTH,
      linetype  = "17"
    )
  )
