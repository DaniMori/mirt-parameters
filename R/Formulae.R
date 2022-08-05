# ==============================================================================
# 
# FILE NAME:   Formulas.R
# DESCRIPTION: Constants for facilitating the writing of LaTeX equations
# 
# AUTHOR:      Mori (danivmorillo@gmail.com)
# 
# DATE:        22/01/2021
# 
# ==============================================================================


## ---- GLOBAL OPTIONS: --------------------------------------------------------



## ---- INCLUDES: --------------------------------------------------------------

# p_load(tidyverse, magrittr)

source("R/LaTeX_math.R")

## ---- CONSTANTS: -------------------------------------------------------------

### Mathematical constants ----

FRAC_1_2 <- latex_frac(1, 2, .sep = NO_SEP)
FRAC_1_4 <- latex_frac(1, 4, .sep = NO_SEP)

### Latent space and variable definition ----

# Latent space definition:
LATENT_SPACE       <- latex_bf("\\upTheta")
N_DIMS             <- latex('n')
BASIS_VECTOR_EL    <- latex('u')
BASIS_VECTOR       <- latex_bf(BASIS_VECTOR_EL)
BASIS_VECTOR_FIRST <- latex_sub(BASIS_VECTOR, 1)
BASIS_VECTOR_LAST  <- latex_sub(BASIS_VECTOR, N_DIMS)
LS_BASIS           <- latex_cal("B")
LS_BASIS_ELEMENTS  <- latex_enum(
  BASIS_VECTOR_FIRST,
  ELLIPSIS,
  BASIS_VECTOR_LAST
)
LS_BASIS_SET       <- latex_curlybraces(LS_BASIS_ELEMENTS)
LS_BASIS_EQ        <- latex_eq(LS_BASIS, LS_BASIS_SET)

# Latent vector definition:
TRAIT_VECTOR    <- latex_bf("\\uptheta")
TRAIT_VEC_IN_LS <- latex_in(TRAIT_VECTOR, LATENT_SPACE)
MEAN_VECTOR     <- latex_bf("\\upmu")
COV_MATRIX      <- latex_bf("\\upSigma")
NORMAL_DISTR    <- latex_cal('N')
MV_DISTRIBUTION <- latex(
  NORMAL_DISTR,
  latex_parentheses("$MEAN_VECTOR$, $COV_MATRIX$")
)
TRAIT_MV_DEF    <- latex_sim(TRAIT_VECTOR, MV_DISTRIBUTION)

# Covariance matrix definition:
CORR_MATRIX <- latex_bf('R')
SD_MATRIX   <- latex_bf('S')
DIM_INDEX   <- latex('k')
SD_ELEMENT  <- latex_sub('s', "$DIM_INDEX$$DIM_INDEX$")
VAR_ELEMENT <- latex_squared(SD_ELEMENT)
COV_ELEMENT <- latex_squared(latex_sub("\\sigma", "$DIM_INDEX$$DIM_INDEX$"))
VAR_COV_EQ  <- latex_eq(VAR_ELEMENT, COV_ELEMENT)

### M2PL model formulation ----

ITEM_INDEX       <- latex('i')
INTERCEPT_PARAM  <- latex_sub('d', ITEM_INDEX)
DISCR_VECTOR_ANY <- latex_bf('a')
DISCR_VECTOR     <- latex_sub(DISCR_VECTOR_ANY, ITEM_INDEX)
DISCR_PARAM      <- latex_sub('a', "$ITEM_INDEX$$DIM_INDEX$")

DISCR_VECTOR_TRANSP <- latex_transp(DISCR_VECTOR)

IRF_M2PL <- latex(
  'P',
  latex_parentheses(
    latex_eq(latex_sub('X', "$ITEM_INDEX$"), 1),
    '|',
    latex_enum(DISCR_VECTOR, INTERCEPT_PARAM, TRAIT_VECTOR)
  )
)
IRF_ABBR <- latex_sub('P', "$ITEM_INDEX$")

LOGIT_M2PL    <- latex_parentheses(
  DISCR_VECTOR_TRANSP, "$TRAIT_VECTOR$ + $INTERCEPT_PARAM$"
)
EXPFUNC_M2PL  <- latex_exp(latex_sqbrackets("-$LOGIT_M2PL$"))
LOGISTIC_M2PL <- latex_logistic(LOGIT_M2PL)
LOG_INV_M2PL  <- latex_inv_log(LOGIT_M2PL)

M2PL_FORMULATION <- latex_eq(IRF_M2PL, LOGISTIC_M2PL)

### Change of basis in the latent space ----

STD_BASIS_VECTOR_EL   <- latex('e')
STD_BASIS_VECTOR      <- latex_bf(STD_BASIS_VECTOR_EL)
LATENT_SPACE_STD      <- latex_prime(LATENT_SPACE)
TRAIT_VECTOR_STD      <- latex_prime(TRAIT_VECTOR)
LS_STD_BASIS          <- latex_cal('E')
LS_STD_BASIS_ELEMENTS <- latex_enum(
  latex_sub(STD_BASIS_VECTOR, 1),
  ELLIPSIS,
  latex_sub(STD_BASIS_VECTOR, N_DIMS)
)
LS_STD_BASIS_SET      <- latex_curlybraces(LS_STD_BASIS_ELEMENTS)
LS_STD_BASIS_EQ       <- latex_eq(LS_STD_BASIS, LS_STD_BASIS_SET)

TRAIT_NORM   <- latex_norm(TRAIT_VECTOR)
TRAIT_MODULE <- latex("\\theta")
NORM_EQ      <- latex_equiv(TRAIT_MODULE, TRAIT_NORM)

TRANSFORM_MATRIX          <- latex_bf('P')
TRANSFORM_MATRIX_TRANSP   <- latex_transp(TRANSFORM_MATRIX)
BASIS_VECTOR_TRANSF       <- latex_prime(BASIS_VECTOR)
BASIS_VECTOR_FIRST_TRANSF <- latex_prime(BASIS_VECTOR_FIRST)
BASIS_VECTOR_LAST_TRANSF  <- latex_prime(BASIS_VECTOR_LAST)
LS_BASIS_ELEMENTS_TRANSF  <- latex_enum(
  BASIS_VECTOR_FIRST_TRANSF,
  ELLIPSIS,
  BASIS_VECTOR_LAST_TRANSF
)
TRANSFORM_MATRIX_DEF      <- latex_sqbrackets(LS_BASIS_ELEMENTS_TRANSF)
TRANSFORM_MATRIX_EQ       <- latex_eq(TRANSFORM_MATRIX, TRANSFORM_MATRIX_DEF)
BASIS_VECTOR_ANY          <- latex_sub(BASIS_VECTOR, DIM_INDEX)
BASIS_VECTOR_ANY_TRANSF   <- latex_prime(BASIS_VECTOR_ANY)


TRAIT_TRANSFORM <- latex(TRANSFORM_MATRIX, TRAIT_VECTOR)
BASIS_CHANGE    <- latex_def(TRAIT_VECTOR_STD, TRAIT_TRANSFORM)

INNER_PROD_MATRIX     <- latex_bf('M')
INNER_PROD_TRANSF_DEF <- latex(TRANSFORM_MATRIX_TRANSP, TRANSFORM_MATRIX)

TRAIT_VECTOR_STD_SQ <- latex(
  latex_transp(TRAIT_VECTOR_STD),
  TRAIT_VECTOR_STD
)
TRANSF_TRAIT_TRANSP   <- latex_transp(latex_parentheses(TRAIT_TRANSFORM))
TRANSF_TRAIT_SQUARED  <- latex(TRANSF_TRAIT_TRANSP, TRAIT_TRANSFORM)
TRAIT_VECTOR_TRANSP   <- latex_transp(TRAIT_VECTOR)
TRAIT_TRANSF_TRAIT    <- latex(
  TRAIT_VECTOR_TRANSP,
  INNER_PROD_TRANSF_DEF,
  TRAIT_VECTOR
)

AUX_INDEX             <- latex('j')
TRAIT_VECTOR_J        <- latex_sub(TRAIT_VECTOR, AUX_INDEX)
TRAIT_VECTOR_J_TRANSP <- latex_transp(TRAIT_VECTOR_J)
TRAIT_VECTOR_K        <- latex_sub(TRAIT_VECTOR, DIM_INDEX)

INNER_PROD_TRAIT  <- latex_innerprod(TRAIT_VECTOR_J, TRAIT_VECTOR_K)
INNER_PROD_TRANSF <- latex(
  TRAIT_VECTOR_J_TRANSP,
  INNER_PROD_MATRIX,
  TRAIT_VECTOR_K
)

INNER_PROD_MAT_EQ <- latex_eq(INNER_PROD_MATRIX, INNER_PROD_TRANSF_DEF)
INNER_PROD_EQ     <- latex_eq(INNER_PROD_TRAIT,  INNER_PROD_TRANSF)

### Direction cosines ----

ANGLE           <- latex("\\gamma")
ANGLE_VECTORS   <- latex_sub(ANGLE, "$AUX_INDEX$$DIM_INDEX$")
COS_VECTORS     <- latex_cos(ANGLE_VECTORS)
TRAIT_VC_J_NORM <- latex_norm(TRAIT_VECTOR_J)
TRAIT_VC_K_NORM <- latex_norm(TRAIT_VECTOR_K)
COS_VECTORS_DEF <- latex_frac(
  INNER_PROD_TRAIT,
  "$TRAIT_VC_J_NORM$ $TRAIT_VC_K_NORM$"
)
COS_VECTORS_EQ  <- latex_eq(COS_VECTORS, COS_VECTORS_DEF)

DIR_ANGLE_ANY    <- latex_sub(ANGLE, DIM_INDEX)
DIR_COS_ANY      <- latex_cos(DIR_ANGLE_ANY)

INNER_PROD_TRAIT_BASIS_VEC <- latex_innerprod(BASIS_VECTOR_ANY, TRAIT_VECTOR)
BASIS_VECTOR_NORM          <- latex_norm(BASIS_VECTOR_ANY)
DIR_COSINE_DEF             <- latex_frac(
  INNER_PROD_TRAIT_BASIS_VEC,
  "$BASIS_VECTOR_NORM$ $TRAIT_NORM$"
)
BASIS_VECTOR_ANY_MOD       <- latex_sub(BASIS_VECTOR_EL, DIM_INDEX)
TRAIT_MODULE_INV           <- latex_frac(1, TRAIT_MODULE, .sep = NO_SEP)
DIR_COSINE_DEF_EXPANDED    <- latex(
  TRAIT_MODULE_INV, latex_frac(1, BASIS_VECTOR_ANY_MOD, .sep = NO_SEP),# Denoms.
  BASIS_VECTOR_ANY, INNER_PROD_MATRIX, TRAIT_VECTOR,  # Numerators
)
DIR_COSINE_EQ              <- latex_eq(
  DIR_COS_ANY,
  DIR_COSINE_DEF,
  DIR_COSINE_DEF_EXPANDED
)

DIAG_INNER_PROD_MATRIX     <- latex(
  latex_rm("diag"),
  latex_parentheses(INNER_PROD_MATRIX)
)
BASIS_VECTOR_FIRST_MOD     <- latex_sub(BASIS_VECTOR_EL, 1)
BASIS_VECTOR_LAST_MOD      <- latex_sub(BASIS_VECTOR_EL, N_DIMS)
DIAG_INNER_PROD_MATRIX_DEF <- latex_transp(
  latex_sqbrackets(
    latex_enum(
      latex_squared(BASIS_VECTOR_FIRST_MOD),
      ELLIPSIS,
      latex_squared(BASIS_VECTOR_LAST_MOD)
    )
  )
)
DIAG_INNER_PROD_MATRIX_EQ  <- latex_eq(
  DIAG_INNER_PROD_MATRIX,
  DIAG_INNER_PROD_MATRIX_DEF
)

DIR_ANGLE_VEC                 <- latex_bf(ANGLE)
DIR_COS_VEC                   <- latex_cos(DIR_ANGLE_VEC)
DIAG_MATRIX_INNER_PROD        <- latex_bf('D')
DIAG_MATRIX_INNER_PROD_INV_SQ <- latex_raised_to(
  DIAG_MATRIX_INNER_PROD,
  exp = latex("-$FRAC_1_2$")
)
DIR_COS_VEC_DEF               <- latex_frac(
  latex(DIAG_MATRIX_INNER_PROD_INV_SQ, INNER_PROD_MATRIX, TRAIT_VECTOR),
  TRAIT_MODULE
)
DIR_COS_VEC_EQ                <- latex_eq(DIR_COS_VEC, DIR_COS_VEC_DEF)

### Polar coordinates ----

DIAG_MATRIX_INNER_PROD_SQ   <- latex_raised_to(
  DIAG_MATRIX_INNER_PROD,
  exp = FRAC_1_2
)
INNER_PROD_MATRIX_INV       <- latex_inverse(INNER_PROD_MATRIX)
INNER_PROD_INV_DIAG_SQ_PROD <- latex(
  INNER_PROD_MATRIX_INV,
  DIAG_MATRIX_INNER_PROD_SQ
)
TRAIT_VECTOR_POLAR_COEFF    <- latex(INNER_PROD_INV_DIAG_SQ_PROD, DIR_COS_VEC)
TRAIT_VECTOR_POLAR_DEF      <- latex(TRAIT_VECTOR_POLAR_COEFF, TRAIT_MODULE)
TRAIT_VECTOR_POLAR_EQ       <- latex_eq(TRAIT_VECTOR, TRAIT_VECTOR_POLAR_DEF)

LOGIT_M2PL_POLAR     <- latex_parentheses(
  "$DISCR_VECTOR_TRANSP$ $TRAIT_VECTOR_POLAR_DEF$ + $INTERCEPT_PARAM$"
)
EXPFUNC_M2PL_POLAR   <- latex_exp(latex_sqbrackets("-$LOGIT_M2PL_POLAR$"))
LOGISTIC_M2PL_POLAR  <- latex_logistic(LOGIT_M2PL_POLAR)
LOG_INV_M2PL_POLAR   <- latex_inv_log(LOGIT_M2PL)
IRF_POLAR_EQ         <- latex_eq(IRF_ABBR, LOGISTIC_M2PL_POLAR)

### Point and direction of maximum slope ----

# Terms:
IRF_SQUARED           <- latex_squared(IRF_ABBR)
DISCR_VEC_TRAIT_COEFF <- latex(DISCR_VECTOR_TRANSP, TRAIT_VECTOR_POLAR_COEFF)

# Second derivative:
IRF_2ND_DIFF     <- latex_seconddiff(IRF_ABBR, TRAIT_MODULE)
IRF_2ND_DIFF_DEF <- latex(
  latex_squared(DISCR_VEC_TRAIT_COEFF, .par = TRUE),
  IRF_ABBR,
  latex_parentheses("1 - 3$IRF_ABBR$ + 2$IRF_SQUARED$")
)
IRF_2ND_DIFF_EQ  <- latex_eq(IRF_2ND_DIFF, IRF_2ND_DIFF_DEF)
IRF_MAX_SLOPE    <- '.5' ## TODO: Format prop-like!!
IRF_MAX_SLOPE_EQ <- latex_eq(IRF_ABBR, IRF_MAX_SLOPE)

# First derivative:
IRF_1ST_DIFF     <- latex_firstdiff(IRF_ABBR, TRAIT_MODULE)
IRF_1ST_DIFF_DEF <- latex(
  IRF_ABBR,
  latex_parentheses("1 - $IRF_ABBR$"),
  DISCR_VEC_TRAIT_COEFF
)
IRF_1ST_DIFF_EQ  <- latex_eq(IRF_1ST_DIFF, IRF_1ST_DIFF_DEF)

# Maximum slope:
SLOPE_MAX     <- latex_sub("\\left. $IRF_1ST_DIFF$ \\right|", IRF_MAX_SLOPE_EQ)
SLOPE_MAX_DEF <- latex(FRAC_1_4, DISCR_VEC_TRAIT_COEFF)
SLOPE_MAX_EQ  <- latex_eq(SLOPE_MAX, SLOPE_MAX_DEF)

# Standardized direction cosines:
DIR_ANGLE_VEC_STD        <- latex_prime(DIR_ANGLE_VEC)
DIR_COS_VEC_STD          <- latex_cos(DIR_ANGLE_VEC_STD)
DIR_COS_VEC_STD_DEFF     <- latex_frac(TRAIT_VECTOR_STD, TRAIT_MODULE)
DIR_COS_VEC_STD_DEFF_INV <- latex_frac(TRAIT_TRANSFORM, TRAIT_MODULE)
DIR_COS_VEC_STD_EQ       <- latex_eq(
  DIR_COS_VEC_STD,
  DIR_COS_VEC_STD_DEFF,
  DIR_COS_VEC_STD_DEFF_INV
)

# Director cosines and standardized direction cosines equivalence:
TRANSFORM_MATRIX_TRANSP_INV    <- latex_inverse("{$TRANSFORM_MATRIX_TRANSP$}")
DIR_COS_AS_DIR_COS_STD         <- latex(
  TRANSFORM_MATRIX_TRANSP_INV,
  DIAG_MATRIX_INNER_PROD_SQ,
  DIR_COS_VEC,
)
DIR_COS_AS_DIR_COS_STD_EQ      <- latex_eq(
  DIR_COS_VEC_STD_DEFF_INV,
  DIR_COS_AS_DIR_COS_STD,
  DIR_COS_VEC_STD
)
TRANSFORM_MATRIX_INV           <- latex_inverse(TRANSFORM_MATRIX)
TRANF_MATRIX_INV_DIR_COS_STD   <- latex(TRANSFORM_MATRIX_INV, DIR_COS_VEC_STD)
TRAIT_POLAR_COEFF_COS_STD_EQ   <- latex_eq(
  TRAIT_VECTOR_POLAR_COEFF,
  TRANF_MATRIX_INV_DIR_COS_STD
)

# Maximum slope as function of standardized direction cosines:
TRAIT_VECTOR_POLAR_COEFF_COS_STD <- latex(TRANSFORM_MATRIX_INV, DIR_COS_VEC_STD)
SLOPE_MAX_STD_COSINES_DEF        <- latex(
  FRAC_1_4,
  DISCR_VECTOR_TRANSP,
  TRAIT_VECTOR_POLAR_COEFF_COS_STD
)
SLOPE_MAX_STD_COSINES_EQ       <- latex_eq(SLOPE_MAX, SLOPE_MAX_STD_COSINES_DEF)

# Standardized discrimination vector:
DISCR_VECTOR_STD            <- latex_prime(DISCR_VECTOR)
DISCR_VECTOR_STD_DEF        <- latex(TRANSFORM_MATRIX_TRANSP_INV, DISCR_VECTOR)
DISCR_VECTOR_STD_EQ         <- latex_def(DISCR_VECTOR_STD, DISCR_VECTOR_STD_DEF)

# Item direction cosines (in satandardized space):
DIR_ANGLE_ITEM_VEC         <- latex_sub(DIR_ANGLE_VEC, ITEM_INDEX)
DIR_ANGLE_STD_ITEM_VEC     <- latex_prime(DIR_ANGLE_ITEM_VEC)
DIR_ANGLE_STD_SUBSTITUTE   <- latex_eq(
  DIR_ANGLE_STD_ITEM_VEC,
  DIR_ANGLE_VEC_STD
)
DISCR_VECTOR_STD_TRANSP    <- latex_transp(DISCR_VECTOR_STD)
DIR_COS_STD_ITEM_VEC_DEF   <- latex_frac(
  DISCR_VECTOR_STD,
  latex_sqrt("$DISCR_VECTOR_STD_TRANSP$ $DISCR_VECTOR_STD$")
)
DIR_COS_STD_ITEM_VEC       <- latex_cos(DIR_ANGLE_STD_ITEM_VEC)
DIR_COS_STD_ITEM_VEC_EQ    <- latex_eq(
  DIR_COS_STD_ITEM_VEC,
  DIR_COS_STD_ITEM_VEC_DEF
)

# Item direction cosines:
DIR_COS_ITEM_VEC        <- latex_cos(DIR_ANGLE_ITEM_VEC)
DISCR_VECTOR_INNER_PROD <- latex(
  DISCR_VECTOR_TRANSP,
  INNER_PROD_MATRIX_INV,
  DISCR_VECTOR
)
DISCR_VECTOR_MODULE_DEF <- latex_sqrt(DISCR_VECTOR_INNER_PROD)
DIR_COS_ITEM_VEC_DEF    <- latex_frac(
  "$DIAG_MATRIX_INNER_PROD_INV_SQ$ $DISCR_VECTOR$",
  DISCR_VECTOR_MODULE_DEF
)
DIR_COS_ITEM_VEC_EQ     <- latex_eq(DIR_COS_ITEM_VEC, DIR_COS_ITEM_VEC_DEF)

# Item signed distance:
DIR_COS_SUBSTITUTE  <- latex_eq(DIR_COS_VEC, DIR_COS_ITEM_VEC)
DISTANCE_SYM        <- latex('D')
DISTANCE_PARAM      <- latex_sub(DISTANCE_SYM, ITEM_INDEX)
DISTANCE_SUBSTITUTE <- latex_eq(TRAIT_MODULE, DISTANCE_PARAM)
DISTANCE_PARAM_DEF  <- latex_frac("-$INTERCEPT_PARAM$", DISCR_VECTOR_MODULE_DEF)
DISTANCE_PARAM_EQ   <- latex_eq(DISTANCE_PARAM, DISTANCE_PARAM_DEF)

### Maximum slope ----

SLOPE_MAX_PARAM     <- latex_sub('S', ITEM_INDEX)
SLOPE_MAX_PARAM_DEF <- latex(FRAC_1_4, DISCR_VECTOR_MODULE_DEF)
SLOPE_MAX_PARAM_EQ  <- latex_eq(SLOPE_MAX_PARAM, SLOPE_MAX_PARAM_DEF)

### Test space ----

TEST_SPACE <- latex_bf("\\upAlpha")

# Inner product:
DISCR_VECTOR_K       <- latex_sub(DISCR_VECTOR_ANY, DIM_INDEX)
DISCR_VECTOR_TRANSP  <- latex_transp(DISCR_VECTOR)
INNER_PROD_DISCR     <- latex_innerprod(DISCR_VECTOR, DISCR_VECTOR_K)
INNER_PROD_DISCR_DEF <- latex(
  DISCR_VECTOR_TRANSP,
  INNER_PROD_MATRIX_INV,
  DISCR_VECTOR_K
)
INNER_PROD_DISCR_EQ  <- latex_eq(INNER_PROD_DISCR, INNER_PROD_DISCR_DEF)

# Discrimination vector module:
DISCR_VECTOR_MODULE    <- latex_norm(DISCR_VECTOR)
DISCR_VECTOR_MODULE_EQ <- latex_eq(
  latex_squared(DISCR_VECTOR_MODULE),
  DISCR_VECTOR_INNER_PROD
)

# Test space basis:
ITEM_BASIS_VECTOR_EL      <- latex('v')
ITEM_BASIS_VECTOR         <- latex_bf(ITEM_BASIS_VECTOR_EL)
ITEM_BASIS_VECTOR_FIRST   <- latex_sub(ITEM_BASIS_VECTOR, 1)
ITEM_BASIS_VECTOR_LAST    <- latex_sub(ITEM_BASIS_VECTOR, N_DIMS)
ITEM_SPACE_BASIS          <- latex_cal("B^*")
ITEM_SPACE_BASIS_ELEMENTS <- latex_enum(
  ITEM_BASIS_VECTOR_FIRST,
  ELLIPSIS,
  ITEM_BASIS_VECTOR_LAST
)
ITEM_SPACE_BASIS_SET      <- latex_curlybraces(ITEM_SPACE_BASIS_ELEMENTS)
ITEM_SPACE_BASIS_EQ       <- latex_eq(ITEM_SPACE_BASIS, ITEM_SPACE_BASIS_SET)

# Transform matrix:
ITEM_BASIS_VECTOR_FIRST_TRANSF   <- latex_prime(ITEM_BASIS_VECTOR_FIRST)
ITEM_BASIS_VECTOR_LAST_TRANSF    <- latex_prime(ITEM_BASIS_VECTOR_LAST)
ITEM_SPACE_BASIS_ELEMENTS_TRANSF <- latex_enum(
  ITEM_BASIS_VECTOR_FIRST_TRANSF,
  ELLIPSIS,
  ITEM_BASIS_VECTOR_LAST_TRANSF
)
ITEM_TRANSF_MATRIX_DEF           <-
  latex_sqbrackets(ITEM_SPACE_BASIS_ELEMENTS_TRANSF)
ITEM_TRANSF_MATRIX_EQ  <- latex_eq(
  TRANSFORM_MATRIX_TRANSP_INV,
  ITEM_TRANSF_MATRIX_DEF
)

# Item direction cosines (in test space):
ANGLE_TS                          <- latex("\\alpha")
DIR_ANGLE_VEC_TS                  <- latex_bf(ANGLE_TS)
DIR_COS_VEC_TS                    <- latex_cos(DIR_ANGLE_VEC_TS)
DIR_ANGLE_ITEM_VEC_TS             <- latex_sub(DIR_ANGLE_VEC_TS, ITEM_INDEX)
DIR_COS_ITEM_VEC_TS               <- latex_cos(DIR_ANGLE_ITEM_VEC_TS)
DIAG_MATRIX_INNER_PROD_INV        <- latex_prime(DIAG_MATRIX_INNER_PROD)
DIAG_MATRIX_INNER_PROD_INV_SQ_INV <- latex_raised_to(
  DIAG_MATRIX_INNER_PROD_INV,
  exp   = latex("-$FRAC_1_2$"),
  .par  = TRUE
)
DIAG_M_DISCR_PROD                 <- latex(
  DIAG_MATRIX_INNER_PROD_INV_SQ_INV,
  INNER_PROD_MATRIX_INV,
  DISCR_VECTOR
)
DIR_COS_ITEM_VEC_TS_DEF           <- latex_frac(
  DIAG_M_DISCR_PROD,
  DISCR_VECTOR_MODULE_DEF
)
DIR_COS_ITEM_VEC_TS_EQ            <- latex_eq(
  DIR_COS_ITEM_VEC_TS,
  DIR_COS_ITEM_VEC_TS_DEF
)

### Generalized multidimensional parameters ----

# Parameters:
MDISC_PARAM <- latex("MDISC")
MIL_PARAM   <- latex("MIL")

#### Agnostic version of the indices: ----

# Condition to meet:
BASIS_EQ              <- latex_equiv(LS_BASIS, LS_STD_BASIS)
ID_MATRIX             <- latex_bf('I')
INNER_PROD_MAT_STD_EQ <- latex_eq(INNER_PROD_MAT_EQ, ID_MATRIX)

# MDISC:
AGNOSTIC_SUBINDEX          <- latex("ag")
MDISC_AG_PARAM             <- latex_sub(MDISC_PARAM, AGNOSTIC_SUBINDEX)
DISCR_VECTOR_AG_INNER_PROD <- latex(DISCR_VECTOR_TRANSP, DISCR_VECTOR)
DISCR_VECTOR_AG_MODULE     <- latex_sqrt(DISCR_VECTOR_AG_INNER_PROD)
MDISC_AG_PARAM_EQ          <- latex_def(MDISC_AG_PARAM, DISCR_VECTOR_AG_MODULE)

# MIL:
MIL_AG_PARAM            <- latex_sub(MIL_PARAM, AGNOSTIC_SUBINDEX)
DISTANCE_AG_DEF         <- latex_frac("- $INTERCEPT_PARAM$", MDISC_AG_PARAM)
DIR_COS_ITEM_VEC_AG_DEF <- latex_frac(DISCR_VECTOR, MDISC_AG_PARAM)
MIL_AG_PARAM_EQ         <- latex_def(
  MIL_AG_PARAM,
  latex_curlybraces("$DISTANCE_AG_DEF$, $DIR_COS_ITEM_VEC_AG_DEF$")
)

# Transformed latent vector as orthogonal:
MEAN_VECTOR_STD <- latex_prime(MEAN_VECTOR)
COV_MATRIX_STD  <- latex_prime(COV_MATRIX)
MV_DISTR_STD    <- latex(
  NORMAL_DISTR,
  latex_parentheses("$MEAN_VECTOR_STD$, $COV_MATRIX_STD$")
)
TRAIT_MV_STD_EQ <- latex_sim(TRAIT_VECTOR_STD, MV_DISTR_STD)

# Transformed latent vector as orthonormal:
MV_DISTR_STD_NORM    <- latex(
  NORMAL_DISTR,
  latex_parentheses("$MEAN_VECTOR_STD$, $ID_MATRIX$")
)
TRAIT_MV_STD_NORM_EQ <- latex_sim(TRAIT_VECTOR_STD, MV_DISTR_STD_NORM)

#### Correlation-based version of the indices: ----

# Condition to meet:
CORR_MATRIX_INV      <- latex_inverse(CORR_MATRIX)
INNER_PROD_CORR_COND <- latex_eq(INNER_PROD_MATRIX, CORR_MATRIX_INV)

# MDISC:
DISCR_VECTOR_CORR_INNER_PROD <- latex(
  DISCR_VECTOR_TRANSP,
  CORR_MATRIX,
  DISCR_VECTOR
)
DISCR_VECTOR_CORR_MODULE     <- latex_sqrt(DISCR_VECTOR_CORR_INNER_PROD)
MDISC_CORR_PARAM             <- latex_sub(MDISC_PARAM, CORR_MATRIX)
MDISC_CORR_PARAM_EQ          <- latex_def(
  MDISC_CORR_PARAM,
  DISCR_VECTOR_CORR_MODULE
)

# MIL:
MIL_CORR_PARAM            <- latex_sub(MIL_PARAM, CORR_MATRIX)
DISTANCE_CORR_DEF         <- latex_frac("- $INTERCEPT_PARAM$", MDISC_CORR_PARAM)
DIR_COS_ITEM_VEC_CORR_DEF <- latex_frac(
  "$CORR_MATRIX$ $DISCR_VECTOR$",
  MDISC_CORR_PARAM
)
MIL_CORR_PARAM_EQ         <- latex_def(
  MIL_CORR_PARAM,
  latex_curlybraces("$DISTANCE_CORR_DEF$, $DIR_COS_ITEM_VEC_CORR_DEF$")
)

#### Covariance-based version of the indices: ----

# Condition to meet:
COV_MATRIX_INV      <- latex_inverse(COV_MATRIX)
INNER_PROD_COV_COND <- latex_eq(INNER_PROD_MATRIX, COV_MATRIX_INV)

# MDISC:
DISCR_VECTOR_COV_INNER_PROD <- latex(
  DISCR_VECTOR_TRANSP,
  COV_MATRIX,
  DISCR_VECTOR
)
DISCR_VECTOR_COV_MODULE     <- latex_sqrt(DISCR_VECTOR_COV_INNER_PROD)
MDISC_COV_PARAM             <- latex_sub(MDISC_PARAM, COV_MATRIX)
MDISC_COV_PARAM_EQ          <- latex_def(
  MDISC_COV_PARAM,
  DISCR_VECTOR_COV_MODULE
)

# MIL:
MIL_COV_PARAM                 <- latex_sub(MIL_PARAM, COV_MATRIX)
DISTANCE_COV_DEF              <- latex_frac(
  "- $INTERCEPT_PARAM$",
  MDISC_COV_PARAM
)
SD_MATRIX_SQUARED                 <- latex_squared(SD_MATRIX)
DIAG_MATRIX_INNER_PROD_INV_VAR_EQ <- latex_eq(
  DIAG_MATRIX_INNER_PROD_INV,
  SD_MATRIX_SQUARED
)
SD_MATRIX_INV                     <- latex_inverse(SD_MATRIX)
DIR_COS_ITEM_VEC_COV_DEF          <- latex_frac(
  "$SD_MATRIX_INV$ $COV_MATRIX$ $DISCR_VECTOR$",
  MDISC_COV_PARAM
)
MIL_COV_PARAM_EQ                  <- latex_def(
  MIL_COV_PARAM,
  latex_curlybraces("$DISTANCE_COV_DEF$, $DIR_COS_ITEM_VEC_COV_DEF$")
)

### Parameter properties: ----

# Generalization from unidimensional IRT:
MDISC_UNIDIM_EQ    <- latex_eq(MDISC_PARAM, DISCR_PARAM)
DIST_INTERCEPT_REL <- latex_eq(
  INTERCEPT_PARAM,
  "- $DISTANCE_PARAM$ $MDISC_PARAM$"
)
DIAG_KTH_ELEMENT   <- latex(DIM_INDEX, DIM_INDEX, .sep = NO_SEP)

# Director cosines:
ANGLE_VECTORS_ITEM   <- latex_sub(ANGLE, "$ITEM_INDEX$$DIM_INDEX$")
COS_VECTORS_ITEM     <- latex_cos(ANGLE_VECTORS_ITEM)
TRAIT_COMPONENT      <- latex_sub(TRAIT_MODULE, DIM_INDEX)
SIGN_COS_VEC_ITEM    <- latex_sign(COS_VECTORS_ITEM, .par = TRUE)
SIGN_COS_VEC_ITEM_EQ <- latex_eq(SIGN_COS_VEC_ITEM, 0)

### Graphical representation: ----

TRANSF_MATRIX_SQ_CORR_INV_EQ <- latex_eq(INNER_PROD_TRANSF_DEF, CORR_MATRIX_INV)
MDISC_CORR_PARAM_ITEM        <- latex_sub(
  MDISC_PARAM,
  "$CORR_MATRIX$$ITEM_INDEX$"
)
MIL_CORR_PARAM_ITEM          <- latex_sub(
  MIL_PARAM,
  "$CORR_MATRIX$$ITEM_INDEX$"
)

ORIGIN              <- latex_bf('o')
ORIGIN_ITEM         <- latex_sub(ORIGIN, ITEM_INDEX)
DISTANCE_CORR_PARAM <- latex_sub(DISTANCE_SYM, "$CORR_MATRIX$$ITEM_INDEX$")
DIR_CORR_PARAM      <- latex_sub(DIR_COS_VEC_TS,  "$CORR_MATRIX$$ITEM_INDEX$")
ORIGIN_ITEM_COMP    <- latex(DISTANCE_CORR_PARAM, DIR_CORR_PARAM)
ORIGIN_ITEM_EQ      <- latex_eq(ORIGIN_ITEM, ORIGIN_ITEM_COMP)

END           <- latex_bf('e')
END_ITEM      <- latex_sub(END, ITEM_INDEX)
END_ITEM_COMP <- latex(ORIGIN_ITEM, '+', MDISC_CORR_PARAM_ITEM, DIR_CORR_PARAM)
END_ITEM_EQ   <- latex_eq(END_ITEM, END_ITEM_COMP)

ORIGIN_ITEM_TRANSF <- latex_prime(ORIGIN_ITEM)
END_ITEM_TRANSF    <- latex_prime(END_ITEM)

ORIGIN_ITEM_TRANSF_DEF <- latex(TRANSFORM_MATRIX, ORIGIN_ITEM)
ORIGIN_ITEM_TRANSF_EQ  <- latex_eq(ORIGIN_ITEM_TRANSF, ORIGIN_ITEM_TRANSF_DEF)
END_ITEM_TRANSF_DEF    <- latex(TRANSFORM_MATRIX, END_ITEM)
END_ITEM_TRANSF_EQ     <- latex_eq(END_ITEM_TRANSF, END_ITEM_TRANSF_DEF)

#### Graphical representation example: ----

CORR <- latex("\\rho")

TRANSFORM_MATRIX_EXAMPLE    <- latex(
  "\\begin{bmatrix}",
  "  1 & \\rho \\\\",
  "  0 & \\sqrt{{1 - \\rho^2}}",
  "\\end{bmatrix}",
  sep = "\n"
)
TRANSFORM_MATRIX_EXAMPLE_EQ <- latex_def(
  TRANSFORM_MATRIX,
  TRANSFORM_MATRIX_EXAMPLE
)

### Mario's lemmas: ----

TRAIT_VEC_STD_MOD    <- latex_prime(TRAIT_MODULE)
TRAIT_VEC_STD_COMP   <- latex_sub(TRAIT_VEC_STD_MOD, DIM_INDEX)
STD_BASIS_VECTOR_ANY <- latex_sub(STD_BASIS_VECTOR, DIM_INDEX)

TRAIT_VECTOR_DEF_BASIS        <- latex_summation(
  TRAIT_COMPONENT, BASIS_VECTOR_ANY,
  index = DIM_INDEX, from = 1, to = N_DIMS,
  .par = FALSE
)
TRAIT_VECTOR_DEF_BASIS_EQ     <- latex_eq(TRAIT_VECTOR, TRAIT_VECTOR_DEF_BASIS)
TRAIT_VECTOR_DEF_STD_BASIS    <- latex_summation(
  TRAIT_VEC_STD_COMP, BASIS_VECTOR_ANY_TRANSF,
  index = DIM_INDEX, from = 1, to = N_DIMS,
  .par = FALSE
)
TRAIT_VECTOR_DEF_STD_BASIS_EQ <- latex_eq(
  TRAIT_VECTOR,
  TRAIT_VECTOR_DEF_STD_BASIS
)

BASIS_CHANGE_SYM <- latex(
  "{}_{$LS_STD_BASIS$}",
  latex_sub(latex_parentheses('I'), LS_BASIS)
)
BASIS_CHANGE_DEF <- latex_eq(BASIS_CHANGE_SYM, TRANSFORM_MATRIX)

TRAIT_COORDS_DEF <- latex_eq(
  TRAIT_VECTOR,
  latex(
    "coord" |> latex_rm() |> latex_sub(LS_BASIS, .abbr = TRUE),
    latex_parentheses(TRAIT_VECTOR, .sep = NO_SEP)
  )
)

TRAIT_STD_COORDS_DEF <- latex_eq(
  TRAIT_VECTOR_STD,
  latex(
    "coord" |> latex_rm() |> latex_sub(LS_STD_BASIS, .abbr = TRUE),
    latex_parentheses(TRAIT_VECTOR, .sep = NO_SEP)
  )
)

BASIS_CHANGE_EQ <- latex_eq(TRAIT_VECTOR_STD, TRAIT_TRANSFORM)

BASIS_VEC_STD_COORDS_DEF <- latex_eq(
  BASIS_VECTOR_ANY_TRANSF,
  latex(
    "coord" |> latex_rm() |> latex_sub(LS_STD_BASIS, .abbr = TRUE),
    latex_parentheses(BASIS_VECTOR_ANY, .sep = NO_SEP)
  )
)

INNER_PROD_BASIS <- latex_innerprod('\\,', '') |> latex_sub(LS_BASIS)

TRAIT_NORM_BASIS     <- TRAIT_NORM |> latex_sub(LS_BASIS)
TRAIT_NORM_STD_BASIS <- TRAIT_NORM |> latex_sub(LS_STD_BASIS)
TRAIT_NORMS_EQ       <- latex_eq(TRAIT_NORM_BASIS, TRAIT_NORM_STD_BASIS)

TRAIT_NORM_STD_BASIS_SQ <- latex_squared(TRAIT_NORM_STD_BASIS)

TRAIT_NORM_STD_EQ <- latex_eq(
  TRAIT_NORM_STD_BASIS_SQ,
  TRAIT_VECTOR_STD_SQ,
  TRAIT_TRANSF_TRAIT
)

INNER_PROD_MAT_EL    <- latex_sub('m', "$AUX_INDEX$$DIM_INDEX$")
BASIS_VECTOR_AUX     <- latex_sub(BASIS_VECTOR, AUX_INDEX)
BASIS_VEC_INNER_PROD <- latex_innerprod(BASIS_VECTOR_AUX, BASIS_VECTOR_ANY)
INNER_PROD_MAT_EL_EQ <- latex_eq(INNER_PROD_MAT_EL, BASIS_VEC_INNER_PROD)

TRANSFORM_MATRIX_INV_TRANSP <- TRANSFORM_MATRIX_INV |> latex_transp()
INNER_PROD_MATRIX_INV_EQ    <- latex_eq(
  INNER_PROD_MATRIX_INV,
  "$TRANSFORM_MATRIX_INV$ $TRANSFORM_MATRIX_TRANSP_INV$",
  "$TRANSFORM_MATRIX_INV$ $TRANSFORM_MATRIX_INV_TRANSP$"
)

BASIS_VEC_FIRST_NORM <- latex_norm(BASIS_VECTOR_FIRST)
BASIS_VEC_LAST_NORM  <- latex_norm(BASIS_VECTOR_LAST)
DIAG_MATRIX_INNER_PROD_INV_SQ_DEF <- latex(
  "\\begin{pmatrix}",
    latex_frac(1, BASIS_VEC_FIRST_NORM), SEP, ALIGN, SEP, ALIGN, NEWLINE,
    SEP, ALIGN, DIAG_ELLIPSIS, SEP, ALIGN, NEWLINE,
    SEP, ALIGN, SEP, ALIGN, latex_frac(1, BASIS_VEC_LAST_NORM),
  "\\end{pmatrix}"
)
DIAG_MATRIX_INNER_PROD_INV_SQ_EQ <- latex_eq(
  DIAG_MATRIX_INNER_PROD_INV_SQ,
  DIAG_MATRIX_INNER_PROD_INV_SQ_DEF
)

DIM_ENUM                <- latex_enum(1, ELLIPSIS, N_DIMS)
DIM_ENUM_EQ             <- latex_eq(DIM_INDEX, DIM_ENUM)
TRAIT_VECTOR_POLAR_DEF2 <- latex(TRAIT_VECTOR_POLAR_COEFF, TRAIT_NORM)
TRAIT_VECTOR_POLAR_EQ2  <- latex_eq(TRAIT_VECTOR, TRAIT_VECTOR_POLAR_DEF2)
INNER_PROD_MATRIX_ROW   <- INNER_PROD_MATRIX |> latex_sub(DIM_INDEX)
DIR_COS_VEC_DEF2        <- latex_frac(
  latex(DIAG_MATRIX_INNER_PROD_INV_SQ, INNER_PROD_MATRIX, TRAIT_VECTOR),
  TRAIT_NORM
)
DIR_COS_VEC_EQ2          <- latex_eq(DIR_COS_VEC, DIR_COS_VEC_DEF2)


DIR_COSINE_PROOF <- latex_eq(
  DIR_COS_ANY,
  DIR_COSINE_DEF,
  latex_frac(
    latex(
      "coord" |>
        latex_rm() |>
        latex_sub(LS_BASIS, .abbr = TRUE) |>
        latex_transp(),
      latex_parentheses(BASIS_VECTOR_ANY, .sep = NO_SEP),
      INNER_PROD_MATRIX,
      TRAIT_VECTOR
    ),
    "$BASIS_VECTOR_NORM$ $TRAIT_NORM$"
  ),
  latex_frac(
    latex(
      STD_BASIS_VECTOR_ANY |> latex_transp(),
      INNER_PROD_MATRIX,
      TRAIT_VECTOR
    ),
    "$BASIS_VECTOR_NORM$ $TRAIT_NORM$"
  ),
  latex_frac(
    "$INNER_PROD_MATRIX_ROW$ $TRAIT_VECTOR$",
    "$BASIS_VECTOR_NORM$ $TRAIT_NORM$"
  )
)

NEW_BASIS                <- latex_cal("A")
BASIS_VECTOR_EL_NEWBS    <- latex('v')
BASIS_VECTOR_NEWBS       <- latex_bf(BASIS_VECTOR_EL_NEWBS)
BASIS_VECTOR_FIRST_NEWBS <- latex_sub(BASIS_VECTOR_NEWBS, 1)
BASIS_VECTOR_LAST_NEWBS  <- latex_sub(BASIS_VECTOR_NEWBS, N_DIMS)
BASIS_ELEMENTS_NEWBS     <- latex_enum(
  BASIS_VECTOR_FIRST_NEWBS,
  ELLIPSIS,
  BASIS_VECTOR_LAST_NEWBS
)
NEW_BASIS_SET            <- latex_curlybraces(BASIS_ELEMENTS_NEWBS)
NEW_BASIS_EQ             <- latex_eq(NEW_BASIS, NEW_BASIS_SET)
INNER_PROD_NEWBS         <- latex_innerprod('\\,', '') |> latex_sub(NEW_BASIS)

DIR_COS_VEC_NEWBS                   <- DIR_COS_VEC_STD |> latex_prime()
TRANSFORM_MATRIX_NEWBS              <- latex_bf('L')
TRANSFORM_MATRICES_PROD             <- latex(
  TRANSFORM_MATRIX_INV,
  TRANSFORM_MATRIX_NEWBS
)
TRANSFORM_MATRICES_PROD_TRANSP      <- latex_transp(
  latex_parentheses(TRANSFORM_MATRICES_PROD)
)
DIAG_MATRIX_INNER_PROD_NEWBS        <- latex_bf('H')
DIAG_MATRIX_INNER_PROD_NEWBS_INV_SQ <- latex_raised_to(
  DIAG_MATRIX_INNER_PROD_NEWBS,
  exp = latex("-$FRAC_1_2$")
)

DIR_COS_VEC_NEWBS_EQUIV <- latex(
  DIAG_MATRIX_INNER_PROD_NEWBS_INV_SQ,
  TRANSFORM_MATRICES_PROD_TRANSP,
  DIAG_MATRIX_INNER_PROD_SQ,
  DIR_COS_VEC
)
LEMMA_2_EQ              <- latex_eq(DIR_COS_VEC_NEWBS, DIR_COS_VEC_NEWBS_EQUIV)

BASIS_VEC_FIRST_NEWBS_NORM              <- latex_norm(BASIS_VECTOR_FIRST_NEWBS)
BASIS_VEC_LAST_NEWBS_NORM               <- latex_norm(BASIS_VECTOR_LAST_NEWBS)
DIAG_MATRIX_INNER_PROD_NEWBS_INV_SQ_DEF <- latex(
  "\\begin{pmatrix}",
  latex_frac(1, BASIS_VEC_FIRST_NEWBS_NORM), SEP, ALIGN, SEP, ALIGN, NEWLINE,
  SEP, ALIGN, DIAG_ELLIPSIS, SEP, ALIGN, NEWLINE,
  SEP, ALIGN, SEP, ALIGN, latex_frac(1, BASIS_VEC_LAST_NEWBS_NORM),
  "\\end{pmatrix}"
)
DIAG_MATRIX_INNER_PROD_NEWBS_INV_SQ_EQ <- latex_eq(
  DIAG_MATRIX_INNER_PROD_NEWBS_INV_SQ,
  DIAG_MATRIX_INNER_PROD_NEWBS_INV_SQ_DEF
)

BASIS_CHANGE_NEWBS_SYM <- latex(
  "{}_{$LS_STD_BASIS$}",
  latex_sub(latex_parentheses('I'), NEW_BASIS)
)
BASIS_CHANGE_NEWBS_DEF <- latex_eq(
  BASIS_CHANGE_NEWBS_SYM,
  TRANSFORM_MATRIX_NEWBS)

TRAIT_VECTOR_NEWBS     <- latex_prime(TRAIT_VECTOR_STD)
TRAIT_NEWBS_COORDS_DEF <- latex_eq(
  TRAIT_VECTOR_NEWBS,
  latex(
    "coord" |> latex_rm() |> latex_sub(NEW_BASIS, .abbr = TRUE),
    latex_parentheses(TRAIT_VECTOR, .sep = NO_SEP)
  )
)
TRAIT_TRANSF_NEWBS     <- latex(TRANSFORM_MATRIX_NEWBS, TRAIT_VECTOR_NEWBS)
BASIS_CHANGE_NEWBS     <- latex_eq(TRAIT_VECTOR_STD, TRAIT_TRANSF_NEWBS)

BASES_CHANGE_SYM     <- latex(
  "{}_{$LS_BASIS$}",
  latex_sub(latex_parentheses('I'), NEW_BASIS)
)
BASIS_CHANGE_REV_SYM <- latex(
  "{}_{$LS_BASIS$}",
  latex_sub(latex_parentheses('I'), LS_STD_BASIS)
)
BASES_CHANGE_PROD    <- latex(BASIS_CHANGE_REV_SYM, BASIS_CHANGE_NEWBS_SYM)
BASES_CHANGE_EQ      <- latex_eq(
  BASES_CHANGE_SYM,
  BASES_CHANGE_PROD,
  TRANSFORM_MATRICES_PROD
)


TRAIT_J_COORDS_NEWBS_TRANSP   <- latex(
  "coord" |>
    latex_rm() |>
    latex_sub(NEW_BASIS, .abbr = TRUE) |>
    latex_transp(),
  latex_parentheses(TRAIT_VECTOR_J, .sep = NO_SEP)
)
TRAIT_K_COORDS_NEWBS          <- latex(
  "coord" |>
    latex_rm() |>
    latex_sub(NEW_BASIS, .abbr = TRUE),
  latex_parentheses(TRAIT_VECTOR_K, .sep = NO_SEP)
)
TRANSFORM_MATRIX_NEWBS_TRANSP <- latex_transp(TRANSFORM_MATRIX_NEWBS)
TRANSFORM_MATRIX_NEWBS_SQ     <- latex(
  TRANSFORM_MATRIX_NEWBS_TRANSP,
  TRANSFORM_MATRIX_NEWBS
)
INNER_PROD_NEWBS_DEF          <- latex(
  TRAIT_J_COORDS_NEWBS_TRANSP,
  TRANSFORM_MATRIX_NEWBS_SQ,
  TRAIT_K_COORDS_NEWBS
)
INNER_PROD_NEWBS_EQ           <- latex_eq(
  INNER_PROD_TRAIT,
  INNER_PROD_NEWBS_DEF
)
INNER_PROD_MAT_NEWBS          <- latex_bf('K')
INNER_PROD_MAT_NEWBS_EQ       <- latex_eq(
  INNER_PROD_MAT_NEWBS,
  TRANSFORM_MATRIX_NEWBS_SQ
)

TRAIT_VECTOR_POLAR_STD_DEF <- latex(DIR_COS_VEC_STD, TRAIT_NORM)
TRAIT_VECTOR_POLAR_STD_EQ  <- latex_eq(
  TRAIT_VECTOR_STD,
  TRAIT_VECTOR_POLAR_STD_DEF
)

DIAG_MATRIX_INNER_PROD_NEWBS_SQ   <- latex_raised_to(
  DIAG_MATRIX_INNER_PROD_NEWBS,
  exp = FRAC_1_2
)
INNER_PROD_MAT_NEWBS_INV          <- latex_inverse(INNER_PROD_MAT_NEWBS)
INNER_PROD_INV_DIAG_SQ_NEWBS_PROD <- latex(
  INNER_PROD_MAT_NEWBS_INV,
  DIAG_MATRIX_INNER_PROD_NEWBS_SQ
)
TRAIT_VEC_NEWBS_POLAR_COEFF       <- latex(
  INNER_PROD_INV_DIAG_SQ_NEWBS_PROD,
  DIR_COS_VEC_NEWBS
)
TRAIT_VEC_NEWBS_POLAR_DEF         <- latex(
  TRAIT_VEC_NEWBS_POLAR_COEFF,
  TRAIT_NORM
)
TRAIT_VEC_NEWBS_POLAR_EQ          <- latex_eq(
  TRAIT_VECTOR_NEWBS,
  TRAIT_VEC_NEWBS_POLAR_DEF
)

TRAIT_COORDS_EQUIV       <- latex_eq(
  TRAIT_VECTOR_STD,
  TRAIT_TRANSFORM,
  TRAIT_VECTOR_POLAR_STD_DEF
)
TRAIT_COORDS_EQUIV_NEWBS <- latex_eq(
  TRAIT_VECTOR_STD,
  TRAIT_TRANSF_NEWBS,
  TRAIT_VECTOR_POLAR_STD_DEF
)

TRAIT_COORDS_EQUIV_EXPANDED       <- latex_eq(
  latex(TRANSFORM_MATRIX, TRAIT_VECTOR_POLAR_COEFF, TRAIT_NORM),
  TRAIT_VECTOR_POLAR_STD_DEF
)
TRAIT_COORDS_EQUIV_NEWBS_EXPANDED <- latex_eq(
  latex(TRANSFORM_MATRIX_NEWBS, TRAIT_VEC_NEWBS_POLAR_COEFF, TRAIT_NORM),
  TRAIT_VECTOR_POLAR_STD_DEF
)
TRAIT_COORDS_EQUIV_EXPANDED_EQ <- latex_eq(
  latex(TRANSFORM_MATRIX, TRAIT_VECTOR_POLAR_COEFF),
  latex(TRANSFORM_MATRIX_NEWBS, TRAIT_VEC_NEWBS_POLAR_COEFF)
)



## ---- FUNCTIONS: -------------------------------------------------------------

### <Section name>: ----

