# ==============================================================================
# 
# FILE NAME:   Graphical_example_transcriptions_Mario.R
# DESCRIPTION: Constants for using with Mario's graphical representation example
# 
# AUTHOR:      Mori (danivmorillo@gmail.com)
# 
# DATE:        19/02/2021
# 
# ==============================================================================


## ---- GLOBAL OPTIONS: --------------------------------------------------------



## ---- INCLUDES: --------------------------------------------------------------



## ---- CONSTANTS: -------------------------------------------------------------

A_1_1 <-  2
A_1_2 <- 0
L_1   <-  1
RHO   <-   .5

RHO_ARC_RAD <- acos(RHO)
RHO_ARC     <- RHO_ARC_RAD / pi * 180

A_1_VALUE <- as.matrix(c(A_1_1, A_1_2))
T_VALUE   <- matrix(c(1, 0, -RHO/sqrt(1 - RHO^2), 1/sqrt(1 - RHO^2)), nrow = 2)
T_INV_VAL <- solve(T_VALUE)


A_1_ORTH_TRANSP_VALUE <- t(A_1_VALUE) %*% T_VALUE

T_T_TRANSP_VALUE <- T_VALUE %*% t(T_VALUE)

A_1_ORTH_SQ_VALUE <- drop(A_1_ORTH_TRANSP_VALUE %*% t(A_1_ORTH_TRANSP_VALUE))

MBS_1_VALUE <- sqrt(A_1_ORTH_SQ_VALUE)
MBL_1_VALUE <- - L_1 / MBS_1_VALUE

COS_BETA_VALUE <- A_1_ORTH_TRANSP_VALUE %*% c(1, 0) / MBS_1_VALUE
SIN_BETA_VALUE <- sqrt(1 - COS_BETA_VALUE^2)
BETA_VALUE     <- acos(COS_BETA_VALUE)
BETA_VALUE_DEG <- BETA_VALUE / pi * 180

A_1_START_COORDS <- MBL_1_VALUE * c(COS_BETA_VALUE, SIN_BETA_VALUE)
A_1_END_COORDS   <- A_1_START_COORDS -
  c(COS_BETA_VALUE, SIN_BETA_VALUE) * MBS_1_VALUE

A_1_START_COORDS_OBLIQUE <- T_VALUE %*% A_1_START_COORDS
A_1_END_COORDS_OBLIQUE   <- T_VALUE %*% A_1_END_COORDS


# With the corrections:

SIN_BETA_VALUE_CORR   <- A_1_ORTH_TRANSP_VALUE %*% c(0, 1) / MBS_1_VALUE
A_1_START_COORDS_CORR <- MBL_1_VALUE * c(COS_BETA_VALUE, SIN_BETA_VALUE_CORR)
A_1_END_COORDS_CORR   <- A_1_START_COORDS_CORR +
  c(COS_BETA_VALUE, SIN_BETA_VALUE_CORR) * MBS_1_VALUE

A_1_START_COORDS_OBLIQUE_CORR <- T_INV_VAL %*% A_1_START_COORDS_CORR
A_1_END_COORDS_OBLIQUE_CORR   <- T_INV_VAL %*% A_1_END_COORDS_CORR


# With the proper parameter corrections:

CORR_MATRIX <- t(T_INV_VAL) %*% T_INV_VAL

MBS_1_SQ_VALUE_CORRECT <- drop(t(A_1_VALUE) %*% CORR_MATRIX %*% A_1_VALUE)
MBS_1_VALUE_CORRECT  <- sqrt(MBS_1_SQ_VALUE_CORRECT)
MBL_1_VALUE_CORRECT  <- - L_1 / MBS_1_VALUE_CORRECT
DIR_COS_VEC_CORRECT  <- t(A_1_VALUE) %*% CORR_MATRIX / MBS_1_VALUE_CORRECT


A_1_START_COORDS_CORRECT <- MBL_1_VALUE_CORRECT * DIR_COS_VEC_CORRECT
A_1_END_COORDS_CORRECT   <- A_1_START_COORDS_CORRECT +
  MBS_1_VALUE_CORRECT * DIR_COS_VEC_CORRECT

A_1_START_COORDS_ORTH_CORRECT <- t(T_VALUE) %*% t(A_1_START_COORDS_CORRECT)
A_1_END_COORDS_ORTH_CORRECT   <- t(T_VALUE) %*% t(A_1_END_COORDS_CORRECT)
