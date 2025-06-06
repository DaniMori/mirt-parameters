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
library(officer)
library(scales)

## ----sources----
source("R/Mirt_toolbox.R", encoding = 'UTF-8')
source("R/Formulae.R",     encoding = 'UTF-8')
source("R/LaTeX_math.R",   encoding = 'UTF-8')
source("R/Output.R",       encoding = 'UTF-8')
source("R/Constants.R",    encoding = 'UTF-8')

## ---- CONSTANTS: -------------------------------------------------------------

## ----constants----

# Graphical representation parameters:
GRAPH_FONT   <- "serif"
FONT_SIZE    <- 12L # Base font size for graphics
AXIS_LAB_POS <- 1   # Axis label position (right or upper extreme)
AXIS_COLOR   <- "black"
LINE_WIDTH   <- .1
VECTOR_WIDTH <- .5
# POINT_CHAR   <- 19L
PALETTE      <- c("darkred", "darkgoldenrod3", "green3", "cyan3", "blue3")

# Item parameters:
items_M2PL <- tribble(
  ~a_1, ~a_2, ~d  ,
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

# Table output objects:
CORR_VALUE_OUT <- CORR_VALUE |> format_prop_like(sig = 1)
CORR_OBL_OUT   <- latex_eq(CORR, CORR_VALUE_OUT) |> as.character()
CORR_ORTH_OUT  <- latex_eq(CORR, 0)              |> as.character()


## ---- CONFIGURATION: ---------------------------------------------------------

## ----graphical-output-conf----
theme_set( # `ggplot` output configuration
  theme_classic(
    base_size      = FONT_SIZE,
    base_family    = GRAPH_FONT,
    base_line_size = LINE_WIDTH
  ) %+replace%
    theme(
      axis.title.x = element_text(hjust = AXIS_LAB_POS),
      axis.title.y = element_text(vjust = AXIS_LAB_POS, angle = 0),
      panel.grid   = element_line(color = AXIS_COLOR)
    )
)

## ---- MAIN: ------------------------------------------------------------------

## ----compute-example-items----

# Item parameters:
items_M2PL <- items_M2PL |> rownames_to_column(var = ITEM_COLKEY)

# Orthogonal case:
items_orth_params <- items_M2PL |> compute_mirt_params(
  all_of(INTERCEPT_COLKEY), starts_with(DISCR_PREFFIX), # Input parameters
  dir_out = c(COSINE_DIRTYPE, DEGREE_DIRTYPE)           # Output configuration
)
items_orth_coords <- items_orth_params |> compute_mirt_coords(
  D, MDISC, starts_with(COSINE_DIRTYPE),
  original_coords = FALSE
)
items_orth  <- full_join(items_orth_params, items_orth_coords, by = ITEM_COLKEY)


# Oblique case:

## Compute the transformation matrix P and the correlation matrix R:
u_1_transf                  <- c(1, - CORR_VALUE / CORR_ARC_SIN)
u_2_transf                  <- c(0,            1 / CORR_ARC_SIN)
transform_matrix            <- cbind(u_1_transf, u_2_transf) |> unname()
transform_matrix_inv        <- solve(transform_matrix)
transform_matrix_transp     <- t(transform_matrix)
transform_matrix_inv_transp <- t(transform_matrix_inv)
inner_prod_matrix           <- transform_matrix_transp %*% transform_matrix
corr_matrix                 <- solve(inner_prod_matrix)
                       # == transform_matrix_inv %*% transform_matrix_inv_transp

## Compute parameters and coordinates:
items_oblique_params <- items_M2PL |> compute_mirt_params(
  all_of(INTERCEPT_COLKEY), starts_with(DISCR_PREFFIX), # Input parameters
  cov_matrix = corr_matrix,                             # Input correlations
  dir_out    = c(COSINE_DIRTYPE, DEGREE_DIRTYPE),       # Output configuration
  version    = "corr"                               # Compute corr-based version
)
items_oblique_coords <- items_oblique_params |> compute_mirt_coords(
  D, MDISC, starts_with(COSINE_DIRTYPE),
  transform       = transform_matrix_inv_transp,
  original_coords = FALSE
)
items_oblique        <- full_join(
  items_oblique_params, items_oblique_coords,
  by = ITEM_COLKEY
)

item_params <- full_join(
  items_orth    |> select(item:deg_2, -starts_with(COSINE_DIRTYPE)),
  items_oblique |> select(item:deg_2, -starts_with(COSINE_DIRTYPE)),
  by     = ITEM_COLKEY,
  suffix = paste0(UNDERSCORE, c(ORTH_SUFFIX, OBL_SUFFIX))
)

item_params <- full_join(items_M2PL, item_params, by = ITEM_COLKEY)


## ----compose-example-items-table----

# Add blank columns to format table (separate different types of parameters)
item_params <- item_params                          |>
  add_column(sep_1 = NA, .after = INTERCEPT_COLKEY) |>
  add_column(
    sep_2  = NA,
    .after = paste(DEGREE_DIRTYPE, 2, ORTH_SUFFIX, sep = UNDERSCORE)
  )

# Table header (for flextable object):
item_headers <- tibble(
  col_keys = item_params |> colnames(),
  metric   = col_keys %>% {
    case_when(
                 . == ITEM_COLKEY                        ~ ITEM_TABLE_TITLE,
                 . == glue("{SEP_PREFFIX}{UNDERSCORE}1") ~ SPACE_SEP,
                 . == INTERCEPT_COLKEY                   ~ MODEL_ACRONYM,
      str_detect(.,   DISCR_PREFFIX)                     ~ MODEL_ACRONYM,
      TRUE                                               ~ MULTIDIM_PARAMS_TITLE
    )
  },
  corr     = col_keys %>% {
    case_when(
      str_detect(., SEP_PREFFIX) ~ ' ',
      str_detect(., ORTH_SUFFIX) ~ CORR_ORTH_OUT,
      str_detect(., OBL_SUFFIX ) ~ CORR_OBL_OUT,
      TRUE                       ~ metric
    )
  },
  param    = col_keys %>% {
    case_when(
                 . == ITEM_COLKEY      ~ ITEM_TABLE_TITLE,
                 . == INTERCEPT_COLKEY ~ as.character(INTERCEPT_PARAM),
      str_detect(., SEP_PREFFIX  )     ~ SPACE_SEP,
      str_detect(., MDISC_SYM    )     ~ as.character(MDISC_ITEM),
      str_detect(., DISTANCE_SYM )     ~ as.character(DISTANCE_PARAM),
      str_detect(., DISCR_PREFFIX)     ~ DISCR_PARAM |> str_replace(
                                           DIM_INDEX,
                                           str_extract(., DIGIT_PATTERN)
                                         ),
      TRUE                             ~ ANGLE_TS_ITEM |> str_replace(
                                           DIM_INDEX,
                                           str_extract(., DIGIT_PATTERN)
                                         )
    )
  }
)

# Column (logical) indices for indexing the flextable output:
corr_header_index      <- item_headers                 |>
  transmute(!corr %in% c(ITEM_TABLE_TITLE, SPACE_SEP)) |>
  pull()
multidim_scalars_index <- item_headers                                  |>
  transmute(param |> str_detect(glue("{MDISC_ITEM}|{DISTANCE_PARAM}"))) |>
  pull()
multidim_angles_index  <- item_headers              |>
  transmute(col_keys |> str_detect(DEGREE_DIRTYPE)) |>
  pull()

# Footer composition:
footer_values <- as_paragraph(
  list_values = list(
    FOOTER_PREFFIX
  ) |>
    c(
      MODEL_EXPLANATION,
      CORR_EXPLANATION,
      DISCR_EXPLANATION,
      INTERCEPT_EXPLANATION,
      MDISC_EXPLANATION,
      DISTANCE_EXPLANATION,
      ANGLE_EXPLANATION, DOT
    )
)

# Table formatted as a `flextable` object:
item_params_output <- item_params                          |>
  flextable()                                              |>
  set_header_df(item_headers)                              |>
  add_footer_lines(values = footer_values)                 |>
  merge_v(part = "header")                                 |>
  merge_h(part = "header", i = 1:2)                        |>
  mk_par(
    i = 2:3, j = corr_header_index,
    part    = "header",
    value   = as_paragraph(as_equation(.)),
    use_dot = TRUE
  )                                                        |>
  style(
    i    = 1:2, j = corr_header_index,
    pr_c = fp_cell(border.bottom = fp_border(width = .5)),
    part = "header"
  )                                                        |>
  theme_apa()                                              |>
  colformat_double(j = multidim_scalars_index, digits = 3) |>
  colformat_double(j = multidim_angles_index,  digits = 1) |>
  style(
    pr_p = fp_par(
      padding.bottom = 3,
      padding.top    = 3,
      padding.left   = 3,
      padding.right  = 3
    ),
    part = "all"
  )                                                        |>
  style(
    pr_p = fp_par(
      line_spacing   = 1.25,
      padding.bottom = 0
    ),
    part = "footer",
  )                                                        |>
  valign(valign = "bottom", part = "header")               |>
  align(align   = "center", part = "header")               |>
  align(align   = "right",  part = "body")                 |>
  fontsize(size = 12,       part = "all")                  |>
  set_table_properties(layout = "autofit")

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
    slope     =  CORR_ARC_TAN,
    intercept = -CORR_ARC_TAN * (-4:4),
    linewidth = LINE_WIDTH,
    linetype  = "17"
  )                                                         +
  geom_hline(yintercept = 0, linewidth = LINE_WIDTH)        +
  geom_segment(
    arrow     = arrow(angle = 20, length = unit(10, "points"), type = "closed"),
    linejoin  = "mitre",
    linewidth = VECTOR_WIDTH
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
    breaks       = -3:3 * CORR_ARC_SIN,
    labels       = function(x) x / CORR_ARC_SIN,
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
    linewidth = VECTOR_WIDTH
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
