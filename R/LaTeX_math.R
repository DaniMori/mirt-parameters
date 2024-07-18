# ==============================================================================
# 
# FILE NAME:   LaTeX_math.R
# DESCRIPTION: Functions for outputting LaTeX equations
# 
# AUTHOR:      Mori (danivmorillo@gmail.com)
# 
# DATE:        22/01/2021
# 
# ==============================================================================


## ---- GLOBAL OPTIONS: --------------------------------------------------------

## ---- INCLUDES: --------------------------------------------------------------

library(glue)
library(rlang)


# source("R/<File_name>.R")


## ---- CONSTANTS: -------------------------------------------------------------

EMPTY_STRING      <- ''

TRANSPOSED_SUFFIX <- '^T'
PRIME_SUFFIX      <- "'"

DELIMITER  <- '$'
SEP        <- ' '
EQ_SIGN    <- ' = '
AL_EQ_SIGN <- ' &= '
EQUIV_SIGN <- ' \\equiv '
DEF_SIGN   <- ' \\coloneq '
SIM_SIGN   <- ' \\sim '
IN_SIGN    <- ' \\in '
NO_SEP     <- EMPTY_STRING
SEP_SPACE  <- '\\,'
SEP_COMMA  <- ', '
UNDERSCORE <- '_'

ELLIPSIS      <- '...'
DIAG_ELLIPSIS <- '\\ddots'

INNER_PROD_LEFT  <- "\\langle"
INNER_PROD_RIGHT <- "\\rangle"
INNER_PROD_SEP   <- paste0(SEP_COMMA, SEP_SPACE)

SUMMATION_OP   <- "\\sum"
COORDINATES_OP <- "coord"

LATEX_CLASS <- "laTeR"

ENCLOSING_VALUES <- c(
  "parentheses",
  "sqbrackets",
  "curlybraces",
  "pipes",
  "doublepipes"
)

COLUMN_SEP    <- " & "
LINE_BREAK    <- "\\\\"
EMPTY_OPERAND <- SEP

END_OF_PROOF <- "\\blacksquare"
# TODO: Maybe change by `\rule{2mm}{2mm}`
#       (see https://tex.stackexchange.com/a/302260 )

## ---- FUNCTIONS: -------------------------------------------------------------

### Engine: ----

latex <- function(..., open = DELIMITER, close = DELIMITER, .sep = SEP) {
  
  result <- glue(
    ...,
    .open  = open,
    .close = close,
    .envir = parent.frame(),
    .sep   = .sep
  )
  
  class(result) <- c(LATEX_CLASS, class(result))
  
  result
}


### Notation: ----

latex_bf  <- function(...) latex("\\mathbf{",  latex(...), "}", .sep = NO_SEP)
latex_cal <- function(...) latex("\\mathcal{", latex(...), "}", .sep = NO_SEP)
latex_rm  <- function(...) latex("\\mathrm{",  latex(...), "}", .sep = NO_SEP)

latex_prime <- function(x) latex(x, PRIME_SUFFIX, .sep = NO_SEP)
latex_enum  <- function(...) latex(..., .sep = SEP_COMMA)

latex_sub   <- function(x, sub, .abbr = NA) {

  sub <- latex(sub)
  
  if (is.na(.abbr)) .abbr <- nchar(sub) == 1
  
  if (!.abbr) sub <- latex_enclose(sub)
  
  latex(x, UNDERSCORE, sub, .sep = NO_SEP)
}

### Basic mathematical: ----

latex_frac <- function(num, den, .inline = FALSE, .sep = SEP) {

  frac_fun <- if (.inline) "\\tfrac" else "\\frac"
  
  latex(frac_fun, "{$num$}", "{$den$}", .sep = .sep)
}
latex_ifrac <- function(num, den) latex_frac(num, den, .inline = TRUE)

latex_eq    <- function(..., .align = FALSE) {
  
  latex(..., .sep = if (.align) AL_EQ_SIGN else EQ_SIGN)
}
latex_equiv <- function(...) latex(..., .sep = EQUIV_SIGN)
latex_def   <- function(...) latex(..., .sep = DEF_SIGN)
latex_sim   <- function(...) latex(..., .sep = SIM_SIGN)

latex_sign <- function(..., .par = NA) {
  
  if (is.na(.par)) .par <- length(list(...)) != 1
  
  argument <- latex(...)
  if (.par) argument <- latex_parentheses(argument)
  
  latex(latex_rm("sign"), argument)
}

### Iterated operations: ----

latex_summation <- function(index, ..., from = 1, to = NULL, .par = NA) {
  
  if (is.na(.par)) .par <- length(list(...)) != 1
  
  if (is.null(to)) {
    
    sub <- latex_in(index, from)
    
  } else {
    
    sub <- latex_eq(index, from)
  }
  
  operator <- latex_sub(SUMMATION_OP, sub)
  
  if (!is.null(to)) operator <- latex_raised_to(operator, exp = to)
  
  result <- latex(...)
  if (.par) result <- latex_parentheses(result)
  
  latex(operator, result)
}


### Powers and roots: ----

latex_raised_to <- function(..., exp, .abbr = NA, .par = NA) {
  
  if (is.na(.par)) .par <- length(list(...)) != 1
  
  base <- latex(...)
  if (.par) base <- latex_parentheses(base)
  
  exp <- as.character(exp)
  
  if (is.na(.abbr)) .abbr <- nchar(exp) == 1
  
  if (!.abbr) exp <- latex_enclose(exp)
  
  latex("$base$^$exp$")
}

latex_sqrt    <- function(x) latex("\\sqrt{$latex(x)$}")
latex_inverse <- function(..., .par = NA) {
  
  latex_raised_to(..., exp = -1, .par = .par)
}

latex_squared <- function(..., .par = NA) {
  
  latex_raised_to(..., exp =  2, .par = .par)
}


### Differential calculus: ----

latex_firstdiff <- function(fun, par) {
  
  latex_frac(latex("\\delta $latex(fun)$"), latex("\\delta ", par))
}

latex_seconddiff <- function(fun, par) {
  
  latex_frac(latex("\\delta^2 $latex(fun)$"), latex("\\delta ", par, "^2"))
}


### Exponentials and logistics: ----

latex_exp       <- function(x) latex("\\exp{$latex(x)$}")

latex_log_den   <- function(x) {
  
  latex("1 + ", latex_exp(latex("\\left[-$x$\\right]")))
}

latex_logistic  <- function(x) latex_frac(1, latex_log_den(x))

latex_inv_log   <- function(x) latex_frac(1, latex("1 + ", latex_exp(x)))


### Trigonometric: ----

latex_sin_as_cos <- function(x) latex("\\sqrt{1 - \\cos^2 $latex(x)$}")

latex_cos_as_sin <- function(x) latex("\\sqrt{1 - \\sin^2 $latex(x)$}")

latex_cos <- function(x) { latex("\\cos $x$") }

latex_sin <- function(x) { latex("\\sin $x$") }

latex_tan_def    <- function(x) {
  
  latex_frac(latex_sin(x), latex_cos(x))
}


### Linear algebra: ----

latex_basis_change <- function(init_basis, end_basis) {
  
  latex(
    "{}_{$end_basis$}",
    latex_sub(latex_parentheses('I'), init_basis)
  )
}

latex_coords <- function(vector, basis, transpose = FALSE) {
  
  COORDINATES_OP <- latex_rm(COORDINATES_OP)
  
  result <- latex_sub(COORDINATES_OP, basis, .abbr = TRUE)
  
  if (transpose) result <- latex_transp(result)
  
  latex(result, latex_parentheses(vector, .sep = NO_SEP), .sep = NO_SEP)
}

latex_norm <- function(..., basis = NULL) {
  
  result <- latex_doublepipes(...)
  
  if (!is.null(basis)) return(latex_sub(result, basis))
  
  result
}


### Matrices: ----

latex_matrix <- function(elements,
                         enclosing = ENCLOSING_VALUES,
                         small     = FALSE) {
  
  if (!is.matrix(elements)) stop("`elements` is not a matrix.")

  enclosing_func <- paste0("latex_", match.arg(enclosing))
  
  result <- list()
  
  for (row in seq_len(nrow(elements))) {
    
    result[[row]] <- do.call(
      latex,
      args = append(elements[row, ], list(.sep = COLUMN_SEP))
    )
  }

  result <- do.call(
    latex,
    args = append(result, list(.sep = LINE_BREAK))
  )
  
  envir <- if (small) "smallmatrix" else "matrix"
  
  result <- latex("\\begin{$envir$}\n$result$\n\\end{$envir$}")
  
  do.call(enclosing_func, args = list(result))
}

latex_diagmatrix <- function(elements,
                             enclosing = ENCLOSING_VALUES,
                             small     = FALSE) {
  
  n      <- length(elements)
  mode   <- if (is.list(elements)) "character" else mode(elements)
  
  matrix <- vector(mode = mode, length = n^2)
  if (is.list(elements)) {
    
    matrix <- as.list(matrix)
    matrix[matrix == ''] <- EMPTY_OPERAND
  }
  
  dim(matrix) <- rep(n, 2)
  diag(matrix) <- elements
  
  latex_matrix(matrix, enclosing = enclosing, small = small)
}

latex_enum_diagmatrix <- function(init, end,
                                  enclosing = ENCLOSING_VALUES,
                                  small     = FALSE) {
  
  latex_diagmatrix(list(init, DIAG_ELLIPSIS, end))
}

### Matrix algebra: ----

latex_transp <- function(x) {
  
  latex(x, TRANSPOSED_SUFFIX, .sep = NO_SEP)
}
## TODO: (Wrong!!) Recycle useful parts from here
# latex_transp <- function(x, transp_sym = "'", .bold = FALSE) {
#   
#   transp_sym <- if (.bold) latex_bf(transp_sym)
#                 else if (nchar(transp_sym != 1)) latex('{$x$}')
#                 else transp_sym
#   
#   latex(x, '^', transp_sym, .sep = NO_SEP)
# }

latex_innerprod <- function(x     = EMPTY_OPERAND,
                            y     = EMPTY_OPERAND,
                            basis = NULL) {
  
  result <- latex(INNER_PROD_LEFT, x, INNER_PROD_SEP, y, INNER_PROD_RIGHT)
  
  if (!is.null(basis)) return(latex_sub(result, basis))
  
  result
}



### Sets: ----

latex_in <- function(element, set) latex(element, set, .sep = IN_SIGN)


### Enclosing functions: ----

latex_enclose     <- function(...) latex("{", ..., "}", .sep = NO_SEP)

latex_doublepipes <- function(...) latex("\\left\\|", ..., "\\right\\|")

latex_parentheses <- function(...) latex("\\left(", ..., "\\right)")

latex_sqbrackets  <- function(...) latex("\\left[", ..., "\\right]")

latex_curlybraces <- function(...) latex("\\left\\{", ..., "\\right\\}")
