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


# source("R/<File_name>.R", encoding = 'UTF-8')


## ---- CONSTANTS: -------------------------------------------------------------

DELIMITER  <- '$'
SEP        <- ' '
EQ_SIGN    <- ' = '
EQUIV_SIGN <- ' \\equiv '
DEF_SIGN   <- ' \\coloneq '
SIM_SIGN   <- ' \\sim '
IN_SIGN    <- ' \\in '
NO_SEP     <- ''
SEP_COMMA  <- ', '
UNDERSCORE <- '_'
ELLIPSIS   <- '...'

TRANSPOSED_SUFFIX <- '^T'
PRIME_SUFFIX      <- "'"

INNER_PROD_LEFT  <- "\\langle"
INNER_PROD_RIGHT <- "\\rangle"
INNER_PROD_SEP   <- ",\\,"

LATEX_CLASS <- "laTeR"


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

latex_eq    <- function(...) latex(..., .sep = EQ_SIGN)
latex_equiv <- function(...) latex(..., .sep = EQUIV_SIGN)
latex_def   <- function(...) latex(..., .sep = DEF_SIGN)
latex_sim   <- function(...) latex(..., .sep = SIM_SIGN)

latex_sign <- function(..., .par = NA) {
  
  if (is.na(.par)) .par <- length(list(...)) != 1
  
  argument <- latex(...)
  if (.par) argument <- latex_parentheses(argument)
  
  latex(latex_rm("sign"), argument)
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
latex_log_den   <- function(x) latex("1 + ", latex_exp(latex("\\left[-$x$\\right]")))
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

latex_innerprod <- function(x, y) {
  
  latex(INNER_PROD_LEFT, x, INNER_PROD_SEP, y, INNER_PROD_RIGHT)
}



### Sets: ----

latex_in <- function(element, set) latex(element, set, .sep = IN_SIGN)


### Enclosing functions: ----

latex_enclose     <- function(...) latex("{", ..., "}", .sep = NO_SEP)
latex_norm        <- function(...) latex("\\left\\|", ..., "\\right\\|")
latex_parentheses <- function(...) latex("\\left(", ..., "\\right)")
latex_sqbrackets  <- function(...) latex("\\left[", ..., "\\right]")
latex_curlybraces <- function(...) latex("\\left\\{", ..., "\\right\\}")
