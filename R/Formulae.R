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

source("R/LaTeX_math.R", encoding = 'UTF-8')

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
COV_MATRIX      <- latex_bf("\\Sigma")
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

# Model parameters:
ITEM_INDEX          <- latex('i')
INTERCEPT_PARAM     <- latex_sub('d', ITEM_INDEX)
DISCR_VECTOR_ANY    <- latex_bf('a')
DISCR_VECTOR        <- latex_sub(DISCR_VECTOR_ANY, ITEM_INDEX)
DISCR_PARAM         <- latex_sub('a', "$ITEM_INDEX$$DIM_INDEX$")
DISCR_VECTOR_TRANSP <- latex_transp(DISCR_VECTOR)

# IRF denotation:
IRF_M2PL <- latex(
  'P',
  latex_parentheses(
    latex_eq(latex_sub('X', "$ITEM_INDEX$"), 1),
    '|',
    latex_enum(DISCR_VECTOR, INTERCEPT_PARAM, TRAIT_VECTOR)
  )
)
IRF_ABBR <- latex_sub('P', "$ITEM_INDEX$")

# Model formula:
LOGIT_M2PL    <- latex_parentheses(
  DISCR_VECTOR_TRANSP, "$TRAIT_VECTOR$ + $INTERCEPT_PARAM$"
)
EXPFUNC_M2PL     <- latex_exp(latex_sqbrackets("-$LOGIT_M2PL$"))
LOGISTIC_M2PL    <- latex_logistic(LOGIT_M2PL)
LOG_INV_M2PL     <- latex_inv_log(LOGIT_M2PL)
M2PL_FORMULATION <- latex_eq(IRF_M2PL, IRF_ABBR, LOGISTIC_M2PL)

### Change of basis in the latent space ----

# Orthonormal basis definition:
ORTH_BASIS_VECTOR_EL   <- latex('e')
ORTH_BASIS_VECTOR      <- latex_bf(ORTH_BASIS_VECTOR_EL)
LATENT_SPACE_STD       <- latex_prime(LATENT_SPACE)
LS_ORTH_BASIS          <- latex_cal('E')
LS_ORTH_BASIS_ELEMENTS <- latex_enum(
  latex_sub(ORTH_BASIS_VECTOR, 1),
  ELLIPSIS,
  latex_sub(ORTH_BASIS_VECTOR, N_DIMS)
)
LS_ORTH_BASIS_SET      <- latex_curlybraces(LS_ORTH_BASIS_ELEMENTS)
LS_ORTH_BASIS_EQ       <- latex_eq(LS_ORTH_BASIS, LS_ORTH_BASIS_SET)

# Vector definition in original basis:
BASIS_VECTOR_ANY       <- latex_sub(BASIS_VECTOR, DIM_INDEX)
TRAIT_SYMBOL           <- latex("\\theta")
TRAIT_COMPONENT        <- latex_sub(TRAIT_SYMBOL, DIM_INDEX)
TRAIT_VEC_COMP         <- latex_raised_to(TRAIT_COMPONENT, exp = LS_BASIS)
TRAIT_VECTOR_DEF_BASIS <- latex_summation(
  TRAIT_VEC_COMP, BASIS_VECTOR_ANY,
  index = DIM_INDEX, from = 1, to = N_DIMS,
  .par = FALSE
)

# Vector definition in orthonormal basis:
TRAIT_VEC_ORTH_COMP         <- latex_raised_to(
  TRAIT_COMPONENT,
  exp = LS_ORTH_BASIS
)
ORTH_BASIS_VECTOR_ANY       <- latex_sub(ORTH_BASIS_VECTOR, DIM_INDEX)
TRAIT_VECTOR_DEF_ORTH_BASIS <- latex_summation(
  TRAIT_VEC_ORTH_COMP, ORTH_BASIS_VECTOR_ANY,
  index = DIM_INDEX, from = 1, to = N_DIMS,
  .par = FALSE
)

TRAIT_VECTOR_DEF_EQ <- latex_eq(
  TRAIT_VECTOR,
  TRAIT_VECTOR_DEF_BASIS,
  TRAIT_VECTOR_DEF_ORTH_BASIS
)

# Trait coordinates in both original and orthonormal bases:
TRAIT_COORDS          <- latex_raised_to(TRAIT_VECTOR, exp = LS_BASIS)
TRAIT_COORDS_DEF      <- latex_coords(TRAIT_VECTOR, LS_BASIS)
TRAIT_COORDS_EQ       <- latex_eq(TRAIT_COORDS, TRAIT_COORDS_DEF)
TRAIT_ORTH_COORDS     <- latex_raised_to(TRAIT_VECTOR, exp = LS_ORTH_BASIS)
TRAIT_COORDS_ORTH_DEF <- latex_coords(TRAIT_VECTOR, LS_ORTH_BASIS)
TRAIT_COORDS_ORTH_EQ  <- latex_eq(TRAIT_ORTH_COORDS, TRAIT_COORDS_ORTH_DEF)

# Change of basis matrix:
TRANSFORM_MATRIX <- latex_bf('P')
BASIS_CHANGE_SYM <- latex_basis_change(LS_BASIS, LS_ORTH_BASIS)
BASIS_CHANGE_DEF <- latex_eq(TRANSFORM_MATRIX, BASIS_CHANGE_SYM)

# Basis vector coordinates in orthonormal basis:
BASIS_VEC_ANY_ORTH_COORDS     <- latex_raised_to(
  BASIS_VECTOR_ANY,
  exp = LS_ORTH_BASIS
)
BASIS_VECTOR_ANY_TRANSF_DEF <- latex_coords(BASIS_VECTOR_ANY, LS_ORTH_BASIS)
BASIS_VECTOR_ANY_TRANSF_EQ  <- latex_eq(
  BASIS_VEC_ANY_ORTH_COORDS,
  BASIS_VECTOR_ANY_TRANSF_DEF
)

# Latent vector change of basis
TRAIT_TRANSFORM <- latex(TRANSFORM_MATRIX, TRAIT_COORDS)
BASIS_CHANGE    <- latex_eq(TRAIT_ORTH_COORDS, TRAIT_TRANSFORM)

# Change of basis matrix definition:
TRANSFORM_MATRIX_TRANSP   <- latex_transp(TRANSFORM_MATRIX)
BASIS_VECTOR_FIRST_TRANSF <- latex_raised_to(
  BASIS_VECTOR_FIRST,
  exp = LS_ORTH_BASIS
)
BASIS_VECTOR_LAST_TRANSF  <- latex_raised_to(
  BASIS_VECTOR_LAST,
  exp = LS_ORTH_BASIS
)
LS_BASIS_ELEMENTS_TRANSF  <- latex_enum(
  BASIS_VECTOR_FIRST_TRANSF,
  ELLIPSIS,
  BASIS_VECTOR_LAST_TRANSF
)
TRANSFORM_MATRIX_DEF      <- latex_sqbrackets(LS_BASIS_ELEMENTS_TRANSF)
TRANSFORM_MATRIX_EQ       <- latex_eq(TRANSFORM_MATRIX, TRANSFORM_MATRIX_DEF)

# Original basis inner product:
LS_BASIS_INNER_PROD     <- latex_innerprod(basis = LS_BASIS)
TRAIT_NORM_LS_BASIS     <- latex_norm(TRAIT_VECTOR, basis = LS_BASIS)
TRAIT_NORM_LS_ORTH_BASIS <- latex_norm(TRAIT_VECTOR, basis = LS_ORTH_BASIS)
TRAIT_NORM_EQ           <- latex_eq(
  TRAIT_NORM_LS_BASIS,
  TRAIT_NORM_LS_ORTH_BASIS
)

# Latent vector invariance condition:
TRAIT_NORM              <- latex_norm(TRAIT_VECTOR)
TRAIT_NORM_SQ           <- latex_squared(TRAIT_NORM)
TRAIT_VECTOR_ORTH_TRANSP <- latex_raised_to(
  TRAIT_VECTOR, # TODO: Transposing a "vector in basis" may typeset wrongly
  exp = "{$LS_ORTH_BASIS$T}"
)
TRAIT_VECTOR_ORTH_SQ     <- latex(TRAIT_VECTOR_ORTH_TRANSP, TRAIT_ORTH_COORDS)
TRAIT_COORDS_TRANSP     <- latex_raised_to(
  TRAIT_VECTOR, # TODO: Transposing a "vector in basis" may typeset wrongly
  exp = "{$LS_BASIS$T}"
)
INNER_PROD_TRANSF_DEF   <- latex(TRANSFORM_MATRIX_TRANSP, TRANSFORM_MATRIX)
TRAIT_TRANSF_TRAIT      <- latex(
  TRAIT_COORDS_TRANSP,
  INNER_PROD_TRANSF_DEF,
  TRAIT_COORDS
)
TRAIT_NORM_SQ_EQ    <- latex_eq(
  TRAIT_NORM_SQ,
  TRAIT_VECTOR_ORTH_SQ,
  TRAIT_TRANSF_TRAIT
)

# Inner product matrix definition:
INNER_PROD_MATRIX <- latex_bf('M')
INNER_PROD_MAT_EQ <- latex_eq(INNER_PROD_MATRIX, INNER_PROD_TRANSF_DEF)

# Ancillary latent vectors:
AUX_INDEX             <- latex('j')
TRAIT_VECTOR_J        <- latex_sub(TRAIT_VECTOR, AUX_INDEX)
TRAIT_VECTOR_K        <- latex_sub(TRAIT_VECTOR, DIM_INDEX)
TRAIT_COORDS_J        <- latex_sub(TRAIT_COORDS, AUX_INDEX)
TRAIT_COORDS_J_TRANSP <- latex_transp(TRAIT_COORDS_J)
TRAIT_COORDS_K        <- latex_sub(TRAIT_COORDS, DIM_INDEX)

# Inner product definition:
INNER_PROD_TRAIT  <- latex_innerprod(
  TRAIT_VECTOR_J,
  TRAIT_VECTOR_K,
  basis = LS_BASIS
)
INNER_PROD_TRANSF <- latex(
  TRAIT_COORDS_J_TRANSP,
  INNER_PROD_MATRIX,
  TRAIT_COORDS_K
)
INNER_PROD_EQ     <- latex_eq(INNER_PROD_TRAIT, INNER_PROD_TRANSF)

# Inner product matrix element:
INNER_PROD_MAT_ELEMENT     <- latex_sub(
  'm',
  latex(AUX_INDEX, DIM_INDEX, .sep = NO_SEP)
)
BASIS_VECTOR_AUX           <- latex_sub(BASIS_VECTOR, AUX_INDEX)
INNER_PROD_MAT_ELEMENT_DEF <- latex_innerprod(
  BASIS_VECTOR_AUX,
  BASIS_VECTOR_ANY
)
INNER_PROD_MAT_ELEMENT_EQ  <- latex_eq(
  INNER_PROD_MAT_ELEMENT,
  INNER_PROD_MAT_ELEMENT_DEF
)
INNER_PROD_MATRIX_INDEX    <- latex(AUX_INDEX, DIM_INDEX, .sep = SEP_COMMA)

# Inverse inner product matrix:
INNER_PROD_MATRIX_INV       <- latex_inverse(INNER_PROD_MATRIX)
TRANSFORM_MATRIX_INV        <- latex_inverse(TRANSFORM_MATRIX)
TRANSFORM_MATRIX_TRANSP_INV <- latex_transp("{$TRANSFORM_MATRIX_INV$}")
INNER_PROD_MATRIX_INV_DEF   <- latex(
  TRANSFORM_MATRIX_INV,
  TRANSFORM_MATRIX_TRANSP_INV
)
INNER_PROD_MATRIX_INV_EQ    <- latex_eq(
  INNER_PROD_MATRIX_INV,
  INNER_PROD_MATRIX_INV_DEF
)

# Inner product matrix element:
INNER_PROD_MAT_ELEMENT     <- latex_sub(
  'm',
  latex(AUX_INDEX, DIM_INDEX, .sep = NO_SEP)
)
BASIS_VECTOR_AUX           <- latex_sub(BASIS_VECTOR, AUX_INDEX)
BASIS_VECTOR_AUX_TRANSF    <- latex_prime(BASIS_VECTOR_AUX)
INNER_PROD_MAT_ELEMENT_DEF <- latex_innerprod(
  BASIS_VECTOR_AUX_TRANSF,
  BASIS_VECTOR_AUX_TRANSF
)
INNER_PROD_MAT_ELEMENT_EQ  <- latex_eq(
  INNER_PROD_MAT_ELEMENT,
  INNER_PROD_MAT_ELEMENT_DEF
)

# Inverse inner product matrix:
INNER_PROD_MATRIX_INV       <- latex_inverse(INNER_PROD_MATRIX)
TRANSFORM_MATRIX_INV        <- latex_inverse(TRANSFORM_MATRIX)
TRANSFORM_MATRIX_TRANSP_INV <- latex_transp("{$TRANSFORM_MATRIX_INV$}")
INNER_PROD_MATRIX_INV_DEF   <- latex(
  TRANSFORM_MATRIX_INV,
  TRANSFORM_MATRIX_TRANSP_INV
)
INNER_PROD_MATRIX_INV_EQ    <- latex_eq(
  INNER_PROD_MATRIX_INV,
  INNER_PROD_MATRIX_INV_DEF
)

### Direction cosines ----

# Cosine of the angle between two vectors:
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

# Generic direction cosine of a vector:
DIR_ANGLE_ANY              <- latex_sub(ANGLE, DIM_INDEX)
DIR_ANGLE_ANY_ORG          <- latex_raised_to(DIR_ANGLE_ANY, exp = LS_BASIS)
DIR_COS_ANY_ORG            <- latex_cos(DIR_ANGLE_ANY_ORG)
INNER_PROD_TRAIT_BASIS_VEC <- latex_innerprod(
  BASIS_VECTOR_ANY,
  TRAIT_VECTOR,
  basis = LS_BASIS
)
BASIS_VECTOR_ANY_NORM      <- latex_norm(BASIS_VECTOR_ANY)
DIR_COS_DEF_DENOMINATOR    <- latex(BASIS_VECTOR_ANY_NORM, TRAIT_NORM)
DIR_COSINE_DEF             <- latex_frac(
  INNER_PROD_TRAIT_BASIS_VEC, # Numerator
  DIR_COS_DEF_DENOMINATOR
)
TRAIT_NORM_INV             <- latex_frac(1, TRAIT_NORM, .sep = NO_SEP)
BASIS_VECTOR_ANY_NORM_INV <- latex_frac(1, BASIS_VECTOR_ANY_NORM, .sep = NO_SEP)

BASIS_VECTOR_ANY_COORDS    <- latex_raised_to(
  BASIS_VECTOR_ANY,
  exp = LS_BASIS
)
DIR_COSINE_DEF_EXPANDED    <- latex(
  BASIS_VECTOR_ANY_NORM_INV, TRAIT_NORM_INV,                # Denominators
  BASIS_VECTOR_ANY_COORDS, INNER_PROD_MATRIX, TRAIT_COORDS, # Numerators
)
DIR_COSINE_EQ              <- latex_eq(
  DIR_COS_ANY_ORG,
  DIR_COSINE_DEF,
  DIR_COSINE_DEF_EXPANDED
)

# Direction cosine vectors (in original basis):
DIR_ANGLE_VEC     <- latex_bf(ANGLE)
DIR_ANGLE_VEC_ORG <- latex_raised_to(DIR_ANGLE_VEC, exp = LS_BASIS)
DIR_COS_VEC_ORG   <- latex_cos(DIR_ANGLE_VEC_ORG)

### Proposition 1 ----

# Direction cosine vector (in original space):
DIAG_MATRIX_INNER_PROD        <- latex_bf('D')
DIAG_MATRIX_INNER_PROD_INV_SQ <- latex_raised_to(
  DIAG_MATRIX_INNER_PROD,
  exp = latex("-$FRAC_1_2$")
)
DIR_COS_VEC_DEF               <- latex_frac(
  latex(DIAG_MATRIX_INNER_PROD_INV_SQ, INNER_PROD_MATRIX, TRAIT_COORDS),
  TRAIT_NORM
)
DIR_COS_VEC_EQ                <- latex_eq(DIR_COS_VEC_ORG, DIR_COS_VEC_DEF)

# Basis vectors:
BASIS_VECTOR_FIRST_NORM    <- latex_norm(BASIS_VECTOR_FIRST)
BASIS_VECTOR_LAST_NORM     <- latex_norm(BASIS_VECTOR_LAST)
BASIS_VECTOR_FIRST_NORM_SQ <- latex_squared(BASIS_VECTOR_FIRST_NORM)
BASIS_VECTOR_LAST_NORM_SQ  <- latex_squared(BASIS_VECTOR_LAST_NORM)

# Diagonal matrix (with the diagonal of the inner product matrix) definition:
DIAG_MATRIX_INNER_PROD_DEF <- latex_enum_diagmatrix(
  BASIS_VECTOR_FIRST_NORM_SQ,
  BASIS_VECTOR_LAST_NORM_SQ
)
DIAG_MATRIX_INNER_PROD_EQ  <- latex_eq(
  DIAG_MATRIX_INNER_PROD,
  DIAG_MATRIX_INNER_PROD_DEF
)

# Trait vector in polar coordinates:
DIAG_MATRIX_INNER_PROD_SQ   <- latex_raised_to(
  DIAG_MATRIX_INNER_PROD,
  exp = FRAC_1_2
)
INNER_PROD_INV_DIAG_SQ_PROD <- latex(
  INNER_PROD_MATRIX_INV,
  DIAG_MATRIX_INNER_PROD_SQ
)
TRAIT_VECTOR_POLAR_COEFF    <- latex(
  INNER_PROD_INV_DIAG_SQ_PROD,
  DIR_COS_VEC_ORG
)
TRAIT_VECTOR_POLAR_DEF      <- latex(TRAIT_VECTOR_POLAR_COEFF, TRAIT_NORM)
TRAIT_VECTOR_POLAR_EQ       <- latex_eq(TRAIT_COORDS, TRAIT_VECTOR_POLAR_DEF)

#### Proof ----

# Enumeration of dimensions:
DIM_ENUM    <- latex_enum(1, ELLIPSIS, N_DIMS)
DIM_ENUM_EQ <- latex_eq(DIM_INDEX, DIM_ENUM)

# Inner product (defined with coordinates of basis vector):
BASIS_VECTOR_COORD <- latex_coords(BASIS_VECTOR_ANY, LS_BASIS, transpose = TRUE)
DIR_COS_DEF_COORDS <- latex_frac(
  latex(BASIS_VECTOR_COORD, INNER_PROD_MATRIX, TRAIT_COORDS), # Numerator
  DIR_COS_DEF_DENOMINATOR
)

# Inner product (defined with standard basis vector):
STD_BASIS_VECTOR_EL         <- latex('b')
STD_BASIS_VECTOR            <- latex_bf(STD_BASIS_VECTOR_EL)
STD_BASIS_VECTOR_ANY        <- latex_sub(STD_BASIS_VECTOR, sub = DIM_INDEX)
STD_BASIS_VECTOR_ANY_TRANSP <- latex_transp(STD_BASIS_VECTOR_ANY)
DIR_COS_DEF_BASIS_VEC       <- latex_frac(
  latex(STD_BASIS_VECTOR_ANY_TRANSP, INNER_PROD_MATRIX, TRAIT_COORDS), # Numer.
  DIR_COS_DEF_DENOMINATOR
)

# Inner product (defined with inner product matrix row):
INNER_PROD_MATRIX_ROW <- latex_sub(INNER_PROD_MATRIX, DIM_INDEX)
DIR_COS_DEF_IPM_ROW   <- latex_frac(
  latex(INNER_PROD_MATRIX_ROW, TRAIT_COORDS), # Numerator
  DIR_COS_DEF_DENOMINATOR
)

# Cosine definition (proof):
DIR_COSINE_PROOF_EQ <- latex_eq(
  DIR_COS_ANY_ORG,
  DIR_COSINE_DEF,
  DIR_COS_DEF_COORDS,
  DIR_COS_DEF_BASIS_VEC,
  DIR_COS_DEF_IPM_ROW
)

### Proposition 2 ----

# New latent space basis:
ALT_BASIS              <- latex_cal('C')
ALT_BASIS_VECTOR_EL    <- latex('w')
ALT_BASIS_VECTOR       <- latex_bf(ALT_BASIS_VECTOR_EL)
ALT_BASIS_VECTOR_FIRST <- latex_sub(ALT_BASIS_VECTOR, 1)
ALT_BASIS_VECTOR_LAST  <- latex_sub(ALT_BASIS_VECTOR, N_DIMS)
ALT_BASIS_ELEMENTS     <- latex_enum(
  ALT_BASIS_VECTOR_FIRST,
  ELLIPSIS,
  ALT_BASIS_VECTOR_LAST
)
ALT_BASIS_SET       <- latex_curlybraces(ALT_BASIS_ELEMENTS)
ALT_BASIS_EQ        <- latex_eq(ALT_BASIS, ALT_BASIS_SET)

# Inner product in new basis :
ALT_BASIS_INNER_PROD <- latex_innerprod(basis = ALT_BASIS)

# Direction cosine vectors (in orthonormal basis):
DIR_ANGLE_VEC_STD <- latex_raised_to(DIR_ANGLE_VEC, exp = LS_ORTH_BASIS)
DIR_COS_VEC_STD   <- latex_cos(DIR_ANGLE_VEC_STD)

# Direction cosine vectors equivalence (new and original bases):
DIR_ANGLE_VEC_ALT                 <- latex_raised_to(
  DIR_ANGLE_VEC,
  exp = ALT_BASIS
)
DIR_COS_VEC_ALT                   <- latex_cos(DIR_ANGLE_VEC_ALT)
DIAG_MATRIX_INNER_PROD_ALT        <- latex_bf('H')
DIAG_MATRIX_INNER_PROD_ALT_INV_SQ <- latex_raised_to(
  DIAG_MATRIX_INNER_PROD_ALT,
  exp = latex("-$FRAC_1_2$")
)
TRANSFORM_MATRIX_ALT              <- latex_bf('L')
TRANSFORM_MATRICES_PROD     <- latex(TRANSFORM_MATRIX_INV, TRANSFORM_MATRIX_ALT)
DIR_COS_VEC_ALT_DEF               <- latex(
  DIAG_MATRIX_INNER_PROD_ALT_INV_SQ,
  latex_transp(latex_parentheses(TRANSFORM_MATRICES_PROD)),
  DIAG_MATRIX_INNER_PROD_SQ,
  DIR_COS_VEC_ORG
)
DIR_COS_VEC_ALT_EQ             <- latex_eq(DIR_COS_VEC_ALT, DIR_COS_VEC_ALT_DEF)

# New basis vector norms:
ALT_BASIS_VECTOR_FIRST_NORM    <- latex_norm(ALT_BASIS_VECTOR_FIRST)
ALT_BASIS_VECTOR_LAST_NORM     <- latex_norm(ALT_BASIS_VECTOR_LAST)
ALT_BASIS_VECTOR_FIRST_NORM_SQ <- latex_squared(ALT_BASIS_VECTOR_FIRST_NORM)
ALT_BASIS_VECTOR_LAST_NORM_SQ  <- latex_squared(ALT_BASIS_VECTOR_LAST_NORM)

# Diagonal matrix (with the diagonal of the inner product matrix) definition:
DIAG_MATRIX_INNER_PROD_ALT_DEF <- latex_enum_diagmatrix(
  ALT_BASIS_VECTOR_FIRST_NORM_SQ,
  ALT_BASIS_VECTOR_LAST_NORM_SQ
)
DIAG_MATRIX_INNER_PROD_ALT_EQ  <- latex_eq(
  DIAG_MATRIX_INNER_PROD_ALT,
  DIAG_MATRIX_INNER_PROD_ALT_DEF
)

# Transform matrix definition (from the orthonormal basis to the new basis):
BASIS_CHANGE_ALT        <- latex_basis_change(ALT_BASIS, LS_ORTH_BASIS)
TRANSFORM_MATRIX_ALT_EQ <- latex_eq(TRANSFORM_MATRIX_ALT, BASIS_CHANGE_ALT)

#### Proof ----

# Latent trait vector coordinates:
TRAIT_VECTOR_ALT     <- latex_raised_to(TRAIT_VECTOR, exp = ALT_BASIS)
TRAIT_COORDS_ALT_DEF <- latex_coords(TRAIT_VECTOR, ALT_BASIS)
TRAIT_COORDS_ALT_EQ  <- latex_eq(TRAIT_VECTOR_ALT, TRAIT_COORDS_ALT_DEF)

# Transform matrix definition (from the original to the new basis):
BASIS_CHANGE_ORG_ALT     <- latex_basis_change(ALT_BASIS, LS_BASIS)
BASIS_CHANGE_INV         <- latex_basis_change(LS_ORTH_BASIS, LS_BASIS)
BASIS_CHANGE_ORG_ALT_DEF <- latex(BASIS_CHANGE_INV, BASIS_CHANGE_ALT)
BASIS_CHANGE_ORG_ALT_EQ  <- latex_eq(
  BASIS_CHANGE_ORG_ALT,
  BASIS_CHANGE_ORG_ALT_DEF,
  TRANSFORM_MATRICES_PROD
)

# Latent trait vector transformation from orthonormal to new basis:
TRAIT_TRANSFORM_ALT    <- latex(TRANSFORM_MATRIX_ALT, TRAIT_VECTOR_ALT)
TRAIT_TRANSFORM_ALT_EQ <- latex_eq(TRAIT_ORTH_COORDS, TRAIT_TRANSFORM_ALT)

# Inner product in new basis:

TRAIT_VECTOR_J_COORDS_ALT    <- latex_raised_to(TRAIT_VECTOR_J, exp = ALT_BASIS)
TRAIT_VECTOR_J_COORDS_ALT_TRANSP <- latex_transp(TRAIT_VECTOR_J_COORDS_ALT)
TRANSFORM_MATRIX_ALT_TRANSP      <- latex_transp(TRANSFORM_MATRIX_ALT)
INNER_PROD_MATRIX_ALT_DEF        <- latex(
  TRANSFORM_MATRIX_ALT_TRANSP,
  TRANSFORM_MATRIX_ALT
)
TRAIT_VECTOR_K_COORDS_ALT    <- latex_raised_to(TRAIT_VECTOR_K, exp = ALT_BASIS)
INNER_PROD_ALT_DEF               <- latex(
  TRAIT_VECTOR_J_COORDS_ALT_TRANSP,
  INNER_PROD_MATRIX_ALT_DEF,
  TRAIT_VECTOR_K_COORDS_ALT
)
INNER_PROD_TRAIT_ALT_EQ        <- latex_eq(INNER_PROD_TRAIT, INNER_PROD_ALT_DEF)

# Inner product matrix in new basis:
INNER_PROD_MATRIX_ALT    <- latex_bf('K')
INNER_PROD_MATRIX_ALT_EQ <- latex_eq(
  INNER_PROD_MATRIX_ALT,
  INNER_PROD_MATRIX_ALT_DEF
)

# Polar coordinates in orthonormal basis:
TRAIT_VEC_ORTH_POLAR_DEF <- latex(DIR_COS_VEC_STD, TRAIT_NORM)
TRAIT_VEC_ORTH_POLAR_EQ  <- latex_eq(
  TRAIT_ORTH_COORDS,
  TRAIT_VEC_ORTH_POLAR_DEF
)

# Polar coordinates in new basis:
DIAG_MATRIX_INNER_PROD_ALT_SQ   <- latex_raised_to(
  DIAG_MATRIX_INNER_PROD_ALT,
  exp = FRAC_1_2
)
INNER_PROD_MATRIX_INV_ALT       <- latex_inverse(INNER_PROD_MATRIX_ALT)
INNER_PROD_INV_DIAG_ALT_SQ_PROD <- latex(
  INNER_PROD_MATRIX_INV_ALT,
  DIAG_MATRIX_INNER_PROD_ALT_SQ
)
TRAIT_VEC_ALT_POLAR_COEFF       <- latex(
  INNER_PROD_INV_DIAG_ALT_SQ_PROD,
  DIR_COS_VEC_ALT
)
TRAIT_VEC_ALT_POLAR_DEF         <- latex(TRAIT_VEC_ALT_POLAR_COEFF, TRAIT_NORM)
TRAIT_VEC_ALT_POLAR_EQ    <- latex_eq(TRAIT_VECTOR_ALT, TRAIT_VEC_ALT_POLAR_DEF)

# Trait coordinate equivalences:
TRAIT_VEC_ORTH_POLAR_EQ_XPAND    <- latex_eq(
  BASIS_CHANGE,
  TRAIT_VEC_ORTH_POLAR_DEF
)
TRAIT_VEC_ALT_POLAR_ALT_EQ       <- latex_eq(
  TRAIT_TRANSFORM_ALT_EQ,
  TRAIT_VEC_ORTH_POLAR_DEF
)
TRANSF_TRAIT_VECTOR_POLAR        <- latex(
  TRANSFORM_MATRIX,
  TRAIT_VECTOR_POLAR_DEF
)
TRANSF_TRAIT_VEC_ALT_POLAR       <- latex(
  TRANSFORM_MATRIX_ALT,
  TRAIT_VEC_ALT_POLAR_DEF
)
TRAIT_VEC_ORTH_POLAR_ORG_EQ      <- latex_eq(
  TRANSF_TRAIT_VECTOR_POLAR,
  TRAIT_VEC_ORTH_POLAR_DEF
)
TRAIT_VEC_ORTH_POLAR_ALT_EQ      <- latex_eq(
  TRANSF_TRAIT_VEC_ALT_POLAR,
  TRAIT_VEC_ORTH_POLAR_DEF
)
TRANSF_TRAIT_VEC_POLAR_COEFF     <- latex(
  TRANSFORM_MATRIX,
  TRAIT_VECTOR_POLAR_COEFF
)
TRANSF_TRAIT_VEC_ALT_POLAR_COEFF <- latex(
  TRANSFORM_MATRIX_ALT,
  TRAIT_VEC_ALT_POLAR_COEFF
)
TRAIT_VEC_POLAR_ORG_ALT_COEFF_EQ <- latex_eq(
  TRANSF_TRAIT_VEC_POLAR_COEFF,
  TRANSF_TRAIT_VEC_ALT_POLAR_COEFF
)


### M2PL IRT model in polar coordinates ----

LOGIT_M2PL_POLAR     <- latex_parentheses(
  "$DISCR_VECTOR_TRANSP$ $TRAIT_VECTOR_POLAR_DEF$ + $INTERCEPT_PARAM$"
)
EXPFUNC_M2PL_POLAR   <- latex_exp(latex_sqbrackets("-$LOGIT_M2PL_POLAR$"))
LOGISTIC_M2PL_POLAR  <- latex_logistic(LOGIT_M2PL_POLAR)
LOG_INV_M2PL_POLAR   <- latex_inv_log(LOGIT_M2PL)
IRF_POLAR_EQ         <- latex_eq(IRF_ABBR, LOGISTIC_M2PL_POLAR)

### Point and direction of maximum slope ----

# Terms:
DISCR_VEC_TRAIT_COEFF <- latex(DISCR_VECTOR_TRANSP, TRAIT_VECTOR_POLAR_COEFF)

# First derivative:
IRF_1ST_DIFF     <- latex_firstdiff(IRF_ABBR, TRAIT_NORM)
IRF_1ST_DIFF_DEF <- latex(
  IRF_ABBR,
  latex_parentheses("1 - $IRF_ABBR$"),
  DISCR_VEC_TRAIT_COEFF
)
IRF_1ST_DIFF_EQ  <- latex_eq(IRF_1ST_DIFF, IRF_1ST_DIFF_DEF)

# Second derivative:
IRF_2ND_DIFF     <- latex_seconddiff(IRF_ABBR, TRAIT_NORM)
IRF_2ND_DIFF_DEF <- latex(
  latex_squared(DISCR_VEC_TRAIT_COEFF, .par = TRUE),
  IRF_ABBR,
  latex_parentheses("1 -  $IRF_ABBR$"),
  latex_parentheses("1 - 2$IRF_ABBR$")
)
IRF_2ND_DIFF_EQ  <- latex_eq(IRF_2ND_DIFF, IRF_2ND_DIFF_DEF)
IRF_MAX_SLOPE    <- '.5' ## TODO: Format prop-like!!
IRF_MAX_SLOPE_EQ <- latex_eq(IRF_ABBR, IRF_MAX_SLOPE)

# Condition for transformation to orthonormal basis:
ID_MATRIX        <- latex_bf('I')
ALT_MATRIX_AS_ID <- latex_eq(DIAG_MATRIX_INNER_PROD_ALT, ID_MATRIX)

# Director cosines and orthonormalized direction cosines equivalence:
DIR_COS_AS_DIR_COS_STD        <- latex(
  TRANSFORM_MATRIX_TRANSP_INV,
  DIAG_MATRIX_INNER_PROD_SQ,
  DIR_COS_VEC_ORG,
)
DIR_COS_AS_DIR_COS_ORTH_EQ    <- latex_eq(
  DIR_COS_VEC_STD,
  DIR_COS_AS_DIR_COS_STD
)
TRANSF_MATRIX_INV_DIR_COS_STD  <- latex(TRANSFORM_MATRIX_INV, DIR_COS_VEC_STD)
TRAIT_POLAR_COEFF_COS_ORTH_EQ  <- latex_eq(
  TRANSF_MATRIX_INV_DIR_COS_STD,
  TRAIT_VECTOR_POLAR_COEFF
)

# Maximum slope:
SLOPE_MAX            <- latex_sub(
  "\\left. $IRF_1ST_DIFF$ \\right|",
  IRF_MAX_SLOPE_EQ
)
DIR_ANGLE_ITEM_VEC   <- latex_sub(DIR_ANGLE_VEC_ORG, ITEM_INDEX)
DIR_COS_ITEM_VEC     <- latex_cos(DIR_ANGLE_ITEM_VEC)
ITEM_POLAR_COEFF     <- latex(
  INNER_PROD_INV_DIAG_SQ_PROD,
  DIR_COS_ITEM_VEC
)
DISCR_VEC_ITEM_COEFF <- latex(DISCR_VECTOR_TRANSP, ITEM_POLAR_COEFF)
SLOPE_MAX_DEF        <- latex(FRAC_1_4, DISCR_VEC_ITEM_COEFF)
SLOPE_MAX_EQ         <- latex_eq(SLOPE_MAX, SLOPE_MAX_DEF)

# Standardized discrimination vector:
DISCR_VECTOR_STD     <- latex_prime(DISCR_VECTOR)
DISCR_VECTOR_STD_DEF <- latex(TRANSFORM_MATRIX_TRANSP_INV, DISCR_VECTOR)
DISCR_VECTOR_STD_EQ  <- latex_def(DISCR_VECTOR_STD, DISCR_VECTOR_STD_DEF)

# Maximum slope as a function of standardized discrimination vectors:
DISCR_VECTOR_STD_TRANSP  <- latex_transp(DISCR_VECTOR_STD)
DIR_COS_ITEM_VEC_STD     <- DIR_COS_VEC_STD |> latex_sub(ITEM_INDEX)
DISCR_VEC_ITEM_STD_COEFF <- latex(DISCR_VECTOR_STD_TRANSP, DIR_COS_ITEM_VEC_STD)
SLOPE_MAX_STD_DEF        <- latex(FRAC_1_4, DISCR_VEC_ITEM_STD_COEFF)
SLOPE_MAX_STD_EQ         <- latex_eq(SLOPE_MAX, SLOPE_MAX_STD_DEF)

# Maximum slope as function of standardized direction cosines:
ITEM_POLAR_COEFF_COS_STD  <- latex(
  TRANSFORM_MATRIX_INV,
  DIR_COS_ITEM_VEC_STD
)
SLOPE_MAX_STD_COSINES_DEF <- latex(
  FRAC_1_4,
  DISCR_VECTOR_TRANSP,
  ITEM_POLAR_COEFF_COS_STD
)
SLOPE_MAX_STD_COSINES_EQ  <- latex_eq(SLOPE_MAX, SLOPE_MAX_STD_COSINES_DEF)

# Item direction cosines (in satandardized space):
DIR_COS_STD_ITEM_VEC_DEF <- latex_frac(
  DISCR_VECTOR_STD,
  latex_sqrt("$DISCR_VECTOR_STD_TRANSP$ $DISCR_VECTOR_STD$")
)
DIR_COS_VEC_STD_EQ       <- latex_eq(
  DIR_COS_ITEM_VEC_STD,
  DIR_COS_STD_ITEM_VEC_DEF
)

# Item direction cosines:
DIR_ANGLE_ITEM_VEC_EQ   <- latex_eq(DIR_ANGLE_VEC_ORG, DIR_ANGLE_ITEM_VEC)
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
TRAIT_NORM_SOLVED    <- latex(
  "-$INTERCEPT_PARAM$",
  latex_inverse(DISCR_VEC_ITEM_COEFF, .par = TRUE)
)
TRAIT_NORM_SOLVED_EQ <- latex_eq(TRAIT_NORM, TRAIT_NORM_SOLVED)
DISTANCE_SYM         <- latex('D')
DISTANCE_PARAM       <- latex_sub(DISTANCE_SYM, ITEM_INDEX)
DISTANCE_SUBSTITUTE  <- latex_eq(TRAIT_NORM, DISTANCE_PARAM)
DISTANCE_PARAM_DEF  <- latex_frac("-$INTERCEPT_PARAM$", DISCR_VECTOR_MODULE_DEF)
DISTANCE_PARAM_EQ    <- latex_eq(DISTANCE_PARAM, DISTANCE_PARAM_DEF)

# Maximum slope:

SLOPE_MAX_PARAM     <- latex_sub('S', ITEM_INDEX)
SLOPE_MAX_PARAM_DEF <- latex(FRAC_1_4, DISCR_VECTOR_MODULE_DEF)
SLOPE_MAX_PARAM_EQ  <- latex_eq(SLOPE_MAX_PARAM, SLOPE_MAX_PARAM_DEF)

### Test space ----

# Test space:
TEST_SPACE <- latex_bf("A")

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
DISCR_VECTOR_MODULE      <- latex_norm(DISCR_VECTOR)
DISCR_VECTOR_MODULE_SQ   <- latex_squared(DISCR_VECTOR_MODULE)
DISCR_VECTOR_TRANSF_PROD <- latex(
  DISCR_VECTOR_TRANSP,
  TRANSFORM_MATRIX_INV,
  TRANSFORM_MATRIX_TRANSP_INV,
  DISCR_VECTOR
)
DISCR_VECTOR_MODULE_EQ   <- latex_eq(
  DISCR_VECTOR_MODULE_SQ,
  DISCR_VECTOR_TRANSF_PROD,
  DISCR_VECTOR_INNER_PROD
)

# Maximum slope (and location) as a function of item discrimination norm:

## Item direction cosines:
DIR_COS_ITEM_VEC_NORM_DEF    <- latex_frac(
  "$DIAG_MATRIX_INNER_PROD_INV_SQ$ $DISCR_VECTOR$",
  DISCR_VECTOR_MODULE
)
DIR_COS_ITEM_VEC_NORM_EQ     <- latex_eq(
  DIR_COS_ITEM_VEC,
  DIR_COS_ITEM_VEC_NORM_DEF
)

## Item signed distance:
DISTANCE_PARAM_NORM_DEF <- latex_frac("-$INTERCEPT_PARAM$", DISCR_VECTOR_MODULE)
DISTANCE_PARAM_NORM_EQ  <- latex_eq(DISTANCE_PARAM, DISTANCE_PARAM_NORM_DEF)

## Maximum slope:
SLOPE_MAX_PARAM_NORM_DEF <- latex(FRAC_1_4, DISCR_VECTOR_MODULE)
SLOPE_MAX_PARAM_NORM_EQ  <- latex_eq(SLOPE_MAX_PARAM, SLOPE_MAX_PARAM_NORM_DEF)

# Inner product matrix elements:
ITEM_BASIS_VECTOR_ANY         <- latex_sub(ITEM_BASIS_VECTOR, ITEM_INDEX)
ITEM_BASIS_VECTOR_AUX         <- latex_sub(ITEM_BASIS_VECTOR, DIM_INDEX)
ITEM_BASIS_VECTORS_INNER_PROD <- latex_innerprod(
  ITEM_BASIS_VECTOR_ANY,
  ITEM_BASIS_VECTOR_AUX
)
INNER_PROD_INV_MAT_SUBINDEX   <- latex(ITEM_INDEX, DIM_INDEX, .sep = NO_SEP)
INNER_PROD_INV_MAT_ELEMENT    <- latex_sub('m', INNER_PROD_INV_MAT_SUBINDEX) |>
  latex_inverse()
INNER_PROD_INV_MAT_ELEMENT_EQ <- latex_eq(
  ITEM_BASIS_VECTORS_INNER_PROD,
  INNER_PROD_INV_MAT_ELEMENT
)

# Change of basis matrix:
BASIS_CHANGE_TEST_SPACE <- latex_basis_change(ITEM_SPACE_BASIS, LS_ORTH_BASIS) # TODO: Orthogonal basis OK?
BASIS_CHANGE_TS_EQ      <- latex_eq(
  BASIS_CHANGE_TEST_SPACE,
  TRANSFORM_MATRIX_TRANSP_INV
)
DISCR_VECTOR_STD_MODULE <- latex_norm(DISCR_VECTOR_STD)
DISCR_VECTOR_STD_MOD_EQ <- latex_eq(
  DISCR_VECTOR_MODULE,
  DISCR_VECTOR_STD_MODULE
)

# Item direction cosines (in test space):
ANGLE_TS                          <- latex("\\alpha")
ANGLE_TS_ITEM                     <- latex_sub(
  ANGLE_TS,
  "$ITEM_INDEX$$DIM_INDEX$"
)
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
  DISCR_VECTOR_MODULE
)
DIR_COS_ITEM_VEC_TS_EQ            <- latex_eq(
  DIR_COS_ITEM_VEC_TS,
  DIR_COS_ITEM_VEC_TS_DEF
)

# Elements of the diagonal matrix of the inverse inner product matrix:
DIAG_MATRIX_INNER_PROD_INV_EL     <- latex_prime('d')
DIAG_MATRIX_INNER_PROD_INV_EL_ANY <- latex_sub(
  DIAG_MATRIX_INNER_PROD_INV_EL,
  DIM_INDEX
)
INNER_PROD_INV_MAT_DIAG_EL        <- latex_sub('m', '$DIM_INDEX$$DIM_INDEX$') |>
  latex_inverse()
ITEM_BASIS_VECTOR_ANY_NORM        <- latex_norm(ITEM_BASIS_VECTOR_AUX)
ITEM_BASIS_VECTOR_ANY_NORM_SQ     <- latex_squared(ITEM_BASIS_VECTOR_ANY_NORM)
MATRIX_INNER_PROD_INV_EL_DIAG     <- latex_sub(
  DIAG_MATRIX_INNER_PROD_INV_EL,
  DIM_INDEX
)
MATRIX_INNER_PROD_INV_EL_DIAG_EQ  <- latex_eq(
  DIAG_MATRIX_INNER_PROD_INV_EL_ANY,
  INNER_PROD_INV_MAT_DIAG_EL,
  ITEM_BASIS_VECTOR_ANY_NORM_SQ
)


### Generalized multidimensional parameters ----

# Parameters:
MDISC_SYM  <- latex("MDISC")
MDISC_ITEM <- latex_sub(MDISC_SYM, ITEM_INDEX)
MIL_PARAM  <- latex("MIL")

#### Agnostic version of the indices: ----

# Condition to meet:
BASIS_EQ              <- latex_equiv(LS_BASIS, LS_ORTH_BASIS)
INNER_PROD_MAT_STD_EQ <- latex_eq(INNER_PROD_MAT_EQ, ID_MATRIX)

# MDISC:
AGNOSTIC_SUBINDEX          <- latex("ag")
MDISC_AG_PARAM             <- latex_sub(MDISC_SYM, AGNOSTIC_SUBINDEX)
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
TRAIT_MV_STD_EQ <- latex_sim(TRAIT_ORTH_COORDS, MV_DISTR_STD)

# Transformed latent vector as orthonormal:
MV_DISTR_STD_NORM    <- latex(
  NORMAL_DISTR,
  latex_parentheses("$MEAN_VECTOR_STD$, $ID_MATRIX$")
)
TRAIT_MV_STD_NORM_EQ <- latex_sim(TRAIT_ORTH_COORDS, MV_DISTR_STD_NORM)

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
MDISC_CORR_PARAM             <- latex_sub(MDISC_SYM, CORR_MATRIX)
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
MDISC_COV_PARAM             <- latex_sub(MDISC_SYM, COV_MATRIX)
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
MDISC_UNIDIM_EQ    <- latex_eq(MDISC_SYM, DISCR_PARAM)
DIST_INTERCEPT_REL <- latex_eq(
  INTERCEPT_PARAM,
  "- $DISTANCE_PARAM$ $MDISC_SYM$"
)
DIAG_KTH_ELEMENT   <- latex(DIM_INDEX, DIM_INDEX, .sep = NO_SEP)

# Director cosines:
ANGLE_VECTORS_ITEM   <- latex_sub(ANGLE, "$ITEM_INDEX$$DIM_INDEX$")
COS_VECTORS_ITEM     <- latex_cos(ANGLE_VECTORS_ITEM)
SIGN_COS_VEC_ITEM    <- latex_sign(COS_VECTORS_ITEM, .par = TRUE)
SIGN_COS_VEC_ITEM_EQ <- latex_eq(SIGN_COS_VEC_ITEM, 0)

### Graphical representation: ----

TRANSF_MATRIX_SQ_CORR_INV_EQ <- latex_eq(INNER_PROD_TRANSF_DEF, CORR_MATRIX_INV)
MDISC_CORR_PARAM_ITEM        <- latex_sub(
  MDISC_SYM,
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

### Application to examples from the literature: ----

DISTANCE_AG_PARAM <- DISTANCE_SYM |> latex_sub(AGNOSTIC_SUBINDEX)
