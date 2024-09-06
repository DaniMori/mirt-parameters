# ==============================================================================
# 
# FILE NAME:   Math.R
# DESCRIPTION: Mathematical functions, created to cleanup the dependency from
#              package `matlib`
# 
# AUTHOR:      Daniel Morillo (daniel.morillo@cibersam.es)
# 
# DATE:        06/09/2024
# 
# ==============================================================================


## ---- INCLUDES: --------------------------------------------------------------

library(glue)
library(flextable)

## ---- FUNCTIONS: -------------------------------------------------------------

# Angle between vectors (in degrees)
angle <- function(x, y) acos(x %*% y / (length(x) * length(y)))

# Length of a vector
len <- function(x) {
  
  if (is.vector(x)) x <- as.matrix(x)
  
  sqrt(colSums(x^2))
}
