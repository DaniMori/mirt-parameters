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

FRAC_1_2 <- latex_frac(1, 2)
FRAC_1_4 <- latex_frac(1, 4)


### MUPP-2PL model maximal slope ----

#### Rectangular coordinates ----

F_THETA_RECT <- "f_2(\\theta_1, \\theta_2)"

EXPONENT_RECT <- "(a_1 \\theta_1 - a_2 \\theta_2 + l)"
LOGISTIC_RECT <- latex_logistic("$EXPONENT_RECT$")


#### Polar coordinates ----

ALPHA <- "\\alpha"

COS_ALPHA <- latex("\\cos $ALPHA$")
SIN_ALPHA <- latex("\\sin $ALPHA$")

COS_SQ_ALPHA <- latex("\\cos^2 $ALPHA$")

THETA_COS_ALPHA <- latex("\\theta $COS_ALPHA$")
THETA_SIN_ALPHA <- latex("\\theta $SIN_ALPHA$")

A1_COS_ALPHA <- latex("a_1 $COS_ALPHA$")
A2_SIN_ALPHA <- latex("a_2 $SIN_ALPHA$")

A_COS_SIN_POLAR  <- latex("($A1_COS_ALPHA$ - $A2_SIN_ALPHA$)")

F_THETA_ALPHA         <- latex("f(\\theta, $ALPHA$)")
F_THETA_ALPHA_1_MINUS <- latex("[1 - $F_THETA_ALPHA$]")

EXPONENT_POLAR  <- latex("(a_1 $THETA_COS_ALPHA$ - a_2 $THETA_SIN_ALPHA$ + l)")
EXPFUNC_POLAR   <- latex_exp("[-$EXPONENT_POLAR$]")
LOG_DENOM_POLAR <- latex_log_den("$EXPONENT_POLAR$")
LOGISTIC_POLAR  <- latex_logistic("$EXPONENT_POLAR$")
LOG_INV_POLAR   <- latex_inv_log("$EXPONENT_POLAR$")

F_THETA_ALPHA_DIFF_THETA  <- latex_firstdiff(F_THETA_ALPHA, "\\theta")
F_THETA_ALPHA_DIFF2_THETA <- latex_seconddiff(F_THETA_ALPHA, "\\theta")

DIFF2_COMMON_FACTOR <- latex(
  F_THETA_ALPHA,
  F_THETA_ALPHA_1_MINUS,
  "$A_COS_SIN_POLAR$^2"
)

DIFF2_COMMON_FACTOR_ALT_ORDER <- latex(
  "$A_COS_SIN_POLAR$^2",
  F_THETA_ALPHA,
  F_THETA_ALPHA_1_MINUS
)


#### Alpha maximization ----

ZETA_ALPHA <- latex("\\zeta($ALPHA$)")

SIN_ALPHA_AS_COS    <- latex_sin_as_cos(ALPHA)
A2_SIN_ALPHA_AS_COS <- latex("a_2 $SIN_ALPHA_AS_COS$")

ZETA_ALPHA_DIFF_COS_ALPHA <- latex_firstdiff(ZETA_ALPHA, "\\cos $ALPHA$")

SQRT_A1_A2_SQUARED <- latex_sqrt("a_1^2 + a_2^2")

MINUS_A1_OVER_SQRT_A_SQ <- latex_frac("- a_1", SQRT_A1_A2_SQUARED)


##### (Alpha maximization, sin alpha version);

SIN_SQ_ALPHA <- latex("\\sin^2 $ALPHA$")

COS_ALPHA_AS_SIN    <- latex_cos_as_sin(ALPHA)
A1_COS_ALPHA_AS_SIN <- latex("a_1 $COS_ALPHA_AS_SIN$")

ZETA_ALPHA_DIFF_SIN_ALPHA <- latex_firstdiff(ZETA_ALPHA, SIN_ALPHA)

MINUS_A2_OVER_SQRT_A_SQ <- latex_frac("- a_2", SQRT_A1_A2_SQUARED)


#### Theta maximization ----

LOG_EXPONENT_MAX_THETA <- latex(
  latex_frac('-a_1^2 \\theta - a_2^2 \\theta', SQRT_A1_A2_SQUARED),
  " + l"
)
LOG_EXPONENT_MAX_THETA_WITH_PAR <- latex(
  "\\left( $LOG_EXPONENT_MAX_THETA$ \\right)"
)
LOGISTIC_MAX_THETA  <- latex_logistic(LOG_EXPONENT_MAX_THETA_WITH_PAR)
LOG_DENOM_MAX_THETA <- latex_log_den(LOG_EXPONENT_MAX_THETA_WITH_PAR)
EXP_MAX_THETA       <- latex_exp(
  "\\left[ -$LOG_EXPONENT_MAX_THETA_WITH_PAR$ \\right]"
)


##### (Possible correction) ----

LOG_EXPONENT_MAX_THETA_CORR <- latex(
  '\\tfrac{ ( -a_1^2 \\theta + a_2^2 \\theta ) }',
  "{$SQRT_A1_A2_SQUARED$}",
  " + l"
)


### MCLM graphical representation ----

#### Matrices and vectors ----

ORTHOGONALIZED <- "^\\mathbf{o}"
INVERSE        <- "^{-1}"

ID_MATRIX <- "\\mathbf{I}"

THETA      <- "\\mathbf{\\uptheta}"
THETA_ORTH <- latex(THETA, ORTHOGONALIZED)
T_MATRIX   <- "\\mathbf{T}"
T_INVERSE  <- latex(T_MATRIX, INVERSE)
R_MATRIX   <- "\\mathbf{R}"
R_INVERSE  <- latex(R_MATRIX, INVERSE)
D_MATRIX   <- "\\mathbf{D}"
SIGMA      <- "\\mathbf{\\upSigma}"

COV <- "\\operatorname{Cov}"

A_VECTOR      <- "\\mathbf{a}_i"
A_VECTOR_ORTH <- latex("$A_VECTOR$^o")


#### MCLM model ----

A_TRANSP_T <- latex("$A_VECTOR$' $T_MATRIX$")
T_TRANSP_A <- latex("$T_MATRIX$' $A_VECTOR$")

A_THETA      <- latex("$A_VECTOR$' $THETA$")
A_THETA_ORTH <- latex("$A_TRANSP_T$ $THETA_ORTH$")

EXPONENT_MCLM      <- latex("($A_THETA$ + l_i)")
EXPONENT_MCLM_ORTH <- latex("($A_THETA_ORTH$ + l_i)")

P_THETA <- latex("P_i($THETA$)")

LOGISTIC_MCLM      <- latex_logistic("[$EXPONENT_MCLM$]")
LOGISTIC_MCLM_ORTH <- latex_logistic("[$EXPONENT_MCLM_ORTH$]")


#### Multidimensional parameters ----


MBL <- "MBL_i"
MBS <- "MBS_i"

MBL_DEF <- latex(
  "-",
  latex_frac("l_i", latex_sqrt("$A_TRANSP_T$ $T_TRANSP_A$"))
)
MBL_DEF_ORTH <- latex(
  "-",
  latex_frac("l_i", latex_sqrt("$A_VECTOR_ORTH$' $A_VECTOR_ORTH$"))
)

MBS_DEF      <- latex("\\left\\| $T_TRANSP_A$ \\right\\|")
MBS_DEF_ORTH <- latex("\\left\\| $A_VECTOR_ORTH$ \\right\\|")


#### Coordinate change ----

THETA_1 <- "\\uptheta_1"
THETA_2 <- "\\uptheta_2"

COORD_SYSTEM      <- latex(THETA_1, THETA_2)
COORD_SYSTEM_ORTH <- latex("$THETA_1$^\\mathrm{o}", "$THETA_2$^\\mathrm{o}")

RHO_COORD_SYSTEM       <- latex("\\rho_{$COORD_SYSTEM$}")
SIN_ALPHA_COORD_SYSTEM <- latex("\\sqrt{1 - $RHO_COORD_SYSTEM$^2}")


#### Change of base ----

V_2_VECTOR       <- "\\mathbf{v}_2"
SIN_ALPHA_AS_RHO <- latex("\\sqrt{1 - \\rho^2}")
TAN_ALPHA_AS_RHO <- latex_frac(SIN_ALPHA_AS_RHO, "\\rho")
DIRECTOR_VECTOR  <- latex("\\left(1, \\, $TAN_ALPHA_AS_RHO$\\right)")
NORM_DIR_VECTOR  <- latex("\\left(\\rho, \\, $SIN_ALPHA_AS_RHO$\\right)")
ORTHONORMAL_BASE <- latex("\\{(1, \\, 0), (0, \\, 1)\\}")
OBLIQUE_BASE     <- latex(
  "\\left\\{(1, \\, 0), $NORM_DIR_VECTOR$ \\right\\}"
)


#### Graphical representation parameters ----

GAMMA_COMPONENT <- "\\gamma_{ik}"
GAMMA_VECTOR    <- "\\mathbf{\\gamma}"

A_COMPONENT_ORTH <- "a_{ik}^o"

DIRECTOR_COSINE <- latex("\\cos $GAMMA_COMPONENT$")
DIR_COS_VECTOR  <- latex("\\cos $GAMMA_VECTOR$")

DIR_COS_VECTOR_DEF <- latex_frac(T_TRANSP_A, MBS)

TAN_BETA_DEF <- latex_tan_def("\\beta")


DIR_VECTOR        <- latex("(1, \\, \\tan \\beta)")
DIR_VECTOR_NORM   <- latex("(\\cos \\beta, \\, \\sin \\beta)")
DIR_VECTOR_NORM_T <- paste(
  "\\begin{pmatrix}",
  "  \\cos \\beta \\\\",
  "  \\sin \\beta",
  "\\end{pmatrix}",
  sep = "\n"
)

X_COORD_Q <- latex("x + l \\, \\cos \\beta")
Y_COORD_Q <- latex("y + l \\, \\sin \\beta")

SIN_BETA_AS_COS <- latex_sin_as_cos("\\beta")

T_INVERSE_DEF <- latex(
  "\\begin{pmatrix}",
  "  1 & \\rho \\\\",
  "  0 & \\sqrt{{1 - \\rho^2}}",
  "\\end{pmatrix}",
  sep = "\n"
)

T_MATRIX_DEF <- latex(
  "\\begin{pmatrix}",
  "  1 & -\\tfrac{\\rho}{$SIN_ALPHA_AS_RHO$} \\\\",
  "  0 &  \\tfrac{1}    {$SIN_ALPHA_AS_RHO$}",
  "\\end{pmatrix}",
  sep = "\n"
)
T_MATRIX_TRANSP_DEF <- latex(
  "\\begin{pmatrix}",
  "                                   1 &                             0 \\\\",
  "  -\\frac{\\rho}{$SIN_ALPHA_AS_RHO$} & \\frac{1}{$SIN_ALPHA_AS_RHO$}",
  "\\end{pmatrix}",
  sep = "\n"
)


#### Example of graphical representation ----

A_1_VECTOR      <- "\\mathbf{a}_1"
A_1_VECTOR_ORTH <- latex("$A_1_VECTOR$^o")

RHO_EXAMPLE      <- "0.5"
RHO_SQ_EXAMPLE   <- "0.25"
A_1_VECTOR_DEF_T <- "(2, \\, -0.5)"
A_1_VEC_DEF_VERT <- latex(
  "\\begin{pmatrix}",
  "   2 \\\\",
  "  -0.5",
  "\\end{pmatrix}",
  sep = "\n"
)
A_1_VEC_DEF_FRAC <- latex(
  "\\begin{pmatrix}",
  "               2 \\\\",
  "  -\\tfrac{1}{2}",
  "\\end{pmatrix}",
  sep = "\n"
)


SIN_ALPHA_AS_RHO_EXAMPLE <- latex("\\sqrt{1 - $RHO_EXAMPLE$^2}")

T_MATRIX_DEF_EXAMPLE <- latex(
  "\\begin{pmatrix}",
  "  1 & -\\tfrac{$RHO_EXAMPLE$}{$SIN_ALPHA_AS_RHO_EXAMPLE$} \\\\",
  "  0 &  \\tfrac{1}            {$SIN_ALPHA_AS_RHO_EXAMPLE$}",
  "\\end{pmatrix}",
  sep = "\n"
)
T_MATRIX_TRANSP_FRAC_DEF <- latex(
  "\\begin{pmatrix}",
  "   1                     &                   0 \\\\",
  "  -\\tfrac{1}{\\sqrt{3}} & \\tfrac{2}{\\sqrt{3}}",
  "\\end{pmatrix}",
  sep = "\n"
)

T_SQ <- latex(
  "\\begin{pmatrix}",
  "    \\tfrac{4}{3} & -\\tfrac{2}{3} \\\\",
  "   -\\tfrac{2}{3} &  \\tfrac{4}{3}",
  "\\end{pmatrix}",
  sep = "\n"
)

PROD_T_A_1    <- latex("$T_MATRIX$' $A_1_VECTOR$")
PROD_SQ_A_1_T <- latex("$A_1_VECTOR$' $T_MATRIX$ $PROD_T_A_1$")

MBL_1     <- "MBL_1"
MBL_1_DEF <- latex(
  "-",
  latex_frac("l_1", latex_sqrt("$PROD_SQ_A_1_T$"))
)


### Parameter derivation in non-orthonormal space ----

D_PARAM <- latex("d_i")

THETA_J      <- latex(THETA, '_j')
THETA_J_NORM <- latex("\\left\\| $THETA_J$ \\right\\|")
THETA_MODULE <- latex("\\theta_j")

S_MATRIX  <- latex("\\mathbf{S}")
S_INVERSE <- latex(S_MATRIX, '^{-1}')

ALPHA_J      <- latex("$ALPHA$_j")
ALPHA_JK     <- latex("$ALPHA$_{jk}")
COS_ALPHA_JK <- latex("\\cos $ALPHA_JK$")

ALPHA_J_VECTOR     <- latex("\\mathbf{$ALPHA$}_j")
COS_ALPHA_J_VECTOR <- latex("\\cos $ALPHA_J_VECTOR$")
R_COS_ALPHA        <- latex("$R_MATRIX$ $COS_ALPHA_J_VECTOR$")
R_COS_ALPHA_THETA  <- latex("$R_COS_ALPHA$ $THETA_MODULE$")

BASIS_VECTOR      <- "\\mathbf{e}_k"
BASIS_VECTOR_NORM <- latex("\\left\\| $BASIS_VECTOR$ \\right\\|")
BASIS_VECTOR_1    <- "\\mathbf{e}_1"
BASIS_VECTOR_N    <- "\\mathbf{e}_n"


P_MCLM_OBL <- latex(
  "P(X_{ij} = 1 | ",
  A_VECTOR, ", ",
  D_PARAM, ", ",
  ALPHA_J_VECTOR, ", ",
  THETA_MODULE, ")"
)

A_R_COS_ALPHA       <- latex("$A_VECTOR$' $R_COS_ALPHA$")
EXPONENT_OBL_POLAR  <- latex("($A_R_COS_ALPHA$ $THETA_MODULE$ + $D_PARAM$)")
EXPFUNC_OBL_POLAR   <- latex_exp("[-$EXPONENT_OBL_POLAR$]")
LOG_DENOM_OBL_POLAR <- latex_log_den("$EXPONENT_OBL_POLAR$")
LOGISTIC_OBL_POLAR  <- latex_logistic("$EXPONENT_OBL_POLAR$")
LOG_INV_OBL_POLAR   <- latex_inv_log("$EXPONENT_OBL_POLAR$")

B_MATRIX  <- latex("\\mathbf{B}")
B_INVERSE <- latex(B_MATRIX, '^{-1}')

SIGMA_ORTH <- latex(SIGMA, ORTHOGONALIZED)

THETA_J_ORTH      <- latex(THETA_J, ORTHOGONALIZED)
THETA_J_ORTH_NORM <- latex("\\left\\|", THETA_J_ORTH, "\\right\\|")


IRF             <- latex(
  "P \\left( x_{ij} = 1 |",
  "$A_VECTOR$, $D_PARAM$, $ALPHA_J_VECTOR$, $THETA_MODULE$",
  "\\right)"
)
IRF_DIFF_THETA  <- latex_firstdiff(IRF, THETA_MODULE)
IRF_DIFF2_THETA <- latex_seconddiff(IRF, THETA_MODULE)


IRF_ABBR            <- latex("P_{ij}")
IRF_DIFF_THETA_DEF  <- latex(IRF_ABBR, "(1 - $IRF_ABBR$)", A_R_COS_ALPHA)
IRF_DIFF2_THETA_DEF <- latex(
  "($A_R_COS_ALPHA$)^2",
  IRF_ABBR,
  "(2 $IRF_ABBR$^2 - 3 $IRF_ABBR$ + 1)"
)


ALPHA_J_ORTH_VECTOR     <- latex(ALPHA_J_VECTOR, ORTHOGONALIZED)
COS_ALPHA_J_ORTH_VECTOR <- latex("\\cos $ALPHA_J_ORTH_VECTOR$")
R_COS_ALPHA_ORTH        <- latex("$R_MATRIX$ $COS_ALPHA_J_ORTH_VECTOR$")
R_COS_ALPHA_ORTH_THETA  <- latex("$R_COS_ALPHA_ORTH$ $THETA_MODULE$")

COS_2_ALPHA_JN <- latex("\\cos^2 $ALPHA$_{jn}")

COS_ALPHA_IK_ORTH       <- latex("\\cos $ALPHA$_{ik}", ORTHOGONALIZED)
ALPHA_I_VECTOR          <- latex("\\mathbf{$ALPHA$}_i")
COS_ALPHA_I_VECTOR      <- latex("\\cos $ALPHA_I_VECTOR$")
ALPHA_I_ORTH_VECTOR     <- latex(ALPHA_I_VECTOR, ORTHOGONALIZED)
COS_ALPHA_I_ORTH_VECTOR <- latex("\\cos $ALPHA_I_ORTH_VECTOR$")

MDIFF <- latex("MDIFF_i")
MDISC <- latex("MDISC_i")


P_MCLM_OBL_MDIFF <- latex(
  "P(X_{ij} = 1 | ",
  A_VECTOR, ", ",
  D_PARAM, ", ",
  "$ALPHA_J_VECTOR$ = $ALPHA_I_VECTOR$,",
  "$THETA_MODULE$ = $MDIFF$)"
)


R_COS_ALPHA_I             <- latex("$R_MATRIX$ $COS_ALPHA_I_VECTOR$")
A_R_COS_ALPHA_I           <- latex("$A_VECTOR$' $R_COS_ALPHA_I$")
EXPONENT_OBL_POLAR_MDIFF  <- latex("($A_R_COS_ALPHA_I$ $MDIFF$ + $D_PARAM$)")
EXPFUNC_OBL_POLAR_MDIFF   <- latex_exp("[-$EXPONENT_OBL_POLAR_MDIFF$]")
LOG_DENOM_OBL_POLAR_MDIFF <- latex_log_den("$EXPONENT_OBL_POLAR_MDIFF$")
LOGISTIC_OBL_POLAR_MDIFF  <- latex_logistic("$EXPONENT_OBL_POLAR_MDIFF$")
LOG_INV_OBL_POLAR_MDIFF   <- latex_inv_log("$EXPONENT_OBL_POLAR_MDIFF$")


## ---- FUNCTIONS: -------------------------------------------------------------

### <Section name>: ----

