# ==============================================================================
# 
# FILE NAME:   Mirt_toolbox.R
# DESCRIPTION: Multidimensional IRT utility functions
# 
# AUTHOR:      Mori (danivmorillo@gmail.com)
# 
# DATE:        18/07/2022
# 
# ==============================================================================


## ---- GLOBAL OPTIONS: --------------------------------------------------------



## ---- INCLUDES: --------------------------------------------------------------


## ---- CONSTANTS: -------------------------------------------------------------

RAD_DEG_FACTOR <- 180 / pi


## ---- FUNCTIONS: -------------------------------------------------------------

### <Section name>: ----


compute_mirt_params <- function(items,
                                intercept,
                                discrimination,
                                cov_matrix,
                                item_id    = item,
                                dir_out    = c("cos", "rad", "deg"),
                                corr_based = FALSE,
                                zero_round = TRUE) {
  ## Argument checking and formatting: ----
  
  # Enquote column selections:
  item_id        <- rlang::enquo(item_id)
  intercept      <- rlang::enquo(intercept)
  discrimination <- rlang::enquo(discrimination)
  
  # Check number of columns selected:
  
  id_col     <- items |> dplyr::select(!!item_id)
  intr_col   <- items |> dplyr::select(!!intercept)
  discr_cols <- items |> dplyr::select(!!discrimination)
  
  n_discr_cols <- discr_cols |> ncol()
  
  assertive.properties::assert_is_of_length(id_col,   1)
  assertive.properties::assert_is_of_length(intr_col, 1)
  
  assertive.extra::assert_is_greater_than_or_equal_to(n_discr_cols, 1)
  
  
  # Check/format covariance matrix:
  if (missing(cov_matrix)) {
    
    cov_matrix <- n_discr_cols |> diag()
    
  } else {
    
    dim_cov <- cov_matrix |> ncol()
    
    assertive.base::assert_are_identical(dim_cov, n_discr_cols)
    assertive.extra::assert_is_covariance_matrix(cov_matrix)
  }
  
  # Direction options:
  dir_out <- dir_out |> match.arg(several.ok = TRUE)
  one_dir <- dir_out                      |>
    assertive.properties::is_of_length(1) |>
    as.logical()
  
  # Flags:
  assertive.types::assert_is_a_bool(corr_based)
  assertive.types::assert_is_a_bool(zero_round)

  ## Main: ----
  
  # Compute transform (and ancillary) matrix:
  innerprod_matrix   <- if (corr_based) cov_matrix |> cov2cor() else cov_matrix
  diag_matrix        <- innerprod_matrix |> diag() |> diag()
  inv_sr_diag_matrix <- diag_matrix      |> sqrt() |> solve()
  
  result <- items                                                            |>
    tidyr::pivot_longer(!!discrimination, names_to = "par", values_to = "a") |>
    dplyr::group_by(!!item_id)                                               |>
    # This renaming is necessary for accessing the variable in `transmute`:
    dplyr::rename(d = !!intercept)                                           |>
    dplyr::transmute(
      !!item_id,
      ## Compute the multidimensional parameters:
      dim   = par |> stringr::str_extract("\\d+"),
      MDISC = (t(a) %*% innerprod_matrix %*% a) |> drop() |> sqrt(),
      D     = - d / MDISC,
      cos   = (inv_sr_diag_matrix %*% innerprod_matrix %*% a / MDISC) |> drop(),
      rad   = acos(cos),
      deg   = rad * RAD_DEG_FACTOR
    )                                                                        |>
    dplyr::ungroup()                                                         |>
    select(!!item_id, dim, MDISC, D, all_of(dir_out))                        |>
    tidyr::pivot_wider(
      names_from  = dim,
      values_from = all_of(dir_out),
      names_glue  = one_dir |> if_else('{.value}_{.name}', '{.name}')
    )
  
  if (zero_round) {
    
    # Avoids "negative zeroes" in rounded figures with
    #   flextable::colformat_double():
    return(
      result |>
        mutate(across(where(is.double), ~if_else(. == 0, 0, .)))
    )
  }
  
  result
}

compute_mirt_coords <- function(items,
                                d,
                                mdisc,
                                dir,
                                transform,
                                item_id         = item,
                                dir_in          = c("cos", "rad", "deg"),
                                original_coords = TRUE) {
  ## Constants: ----
  

  ## Argument checking and formatting: ----

  # Output options:
  assertive.types::assert_is_a_bool(original_coords)
  output_select <- if (original_coords) quo(origin:end_transf)
                   else                 quo(origin_transf:end_transf)
  
  # Enquote column selections:
  item_id <- rlang::enquo(item_id)
  d       <- rlang::enquo(d)
  mdisc   <- rlang::enquo(mdisc)
  dir     <- rlang::enquo(dir)
  
  # Check number of columns selected:
  
  id_col    <- items |> dplyr::select(!!item_id)
  d_col     <- items |> dplyr::select(!!d)
  mdisc_col <- items |> dplyr::select(!!mdisc)
  dir_cols  <- items |> dplyr::select(!!dir)
  
  n_dir_cols <- dir_cols |> ncol()
  
  assertive.properties::assert_is_of_length(id_col,    1)
  assertive.properties::assert_is_of_length(d_col,     1)
  assertive.properties::assert_is_of_length(mdisc_col, 1)
  
  assertive.extra::assert_is_greater_than_or_equal_to(n_dir_cols, 1)
  
  
  # Check/format transformation matrix:
  if (missing(transform)) {
    
    transform <- n_dir_cols |> diag() # Default (no transformation)
    
  } else {
    
    ## Check dimension:
    transf_dim <- transform |> ncol()
    assertive.base::assert_are_identical(transf_dim, n_dir_cols)
    
    ## Check its inverse squared matrix can be considered a covariance matrix:
    cov_matrix <- t(transform) %*% transform |> solve()
    assertive.extra::assert_is_covariance_matrix(cov_matrix)
  }
  
  # Direction options:
  dir_in <- dir_in |> match.arg()
  
  
  ## Main: ----
  
  items                                                             |>
    tidyr::pivot_longer(!!dir, names_to = "par", values_to = "dir") |>
    dplyr::group_by(!!item_id)                                      |>
    dplyr::transmute(
      !!item_id,
      dim    = par |> stringr::str_extract("\\d+"),
      ## Format the direction as cosines:
      cos = dir_in |> switch (
        cos = dir,
        rad = cos(dir),
        deg = cos(dir / RAD_DEG_FACTOR)
      ),
      ## Compute the coordinates:
      origin = D * cos,
      end    = origin + MDISC * cos,
      ## Transform the coordinates:
      across(
        origin:end,
        .fns = list(transf = ~drop(transform %*% .))
      )
    )                                                               |>
    dplyr::ungroup()                                                |>
    tidyr::pivot_wider(
      id_cols     = item,
      names_from  = dim,
      values_from = !!output_select
    )
}
