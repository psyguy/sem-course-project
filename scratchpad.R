## Example of a Full LISREL model path diagram with the same number of
## exgenous and endogenous variables:

# Lambda matrices:
Loadings <- rbind(diag(1, 2, 2), diag(1, 2, 2), diag(1, 2, 2))

# Phi and Psi matrices:
LatVar <- diag(1, 2, 2)

# Beta matrix:
Beta <- matrix(0, 2, 2)
Beta[1, 2] <- 1

# Theta matrices:
ManVar <- diag(1, nrow(Loadings), nrow(Loadings))

# Gamma matrix:
Gamma <- diag(1, 2, 2)

# Tau matrices:
ManInts <- rep(1, 6)

# Alpha and Kappa matrices:
LatInts <- rep(1, 2)

# Combine model:
mod <- lisrelModel(LY = Loadings, PS = LatVar, BE = Beta, TE = ManVar, LX = Loadings, 
                   PH = LatVar, GA = Gamma, TD = ManVar, TY = ManInts, TX = ManInts, AL = LatInts, 
                   KA = LatInts)

# Plot path diagram:
semPaths(mod,
         as.expression = c("nodes", "edges"), sizeMan = 3, sizeInt = 1, 
         "std", edge.label.cex = 0.5,
         sizeLat = 4)


# mimic -------------------------------------------------------------------

library("lavaan")

# Example 5.8 from mplus user guide:
Data <- read.table("http://www.statmodel.com/usersguide/chap5/ex5.8.dat")
names(Data) <- c(paste("y", 1:6, sep = ""), paste("x", 1:3, sep = ""))

# Model:
model.Lavaan <- "f1 =~ y1 + y2 + y3\nf2 =~ y4 + y5 + y6\nf1 + f2 ~ x1 + x2 + x3 "

# Run Lavaan:
library("lavaan")
fit <- lavaan:::lavaan(model.Lavaan, data = Data, std.lv = TRUE)

# Plot path diagram:
semPaths(fit, title = FALSE,
         curvePivot = TRUE,
         "std", edge.label.cex = 0.5, curvePivot = TRUE)



# my sempath --------------------------------------------------------------


semPaths(f,
         title = FALSE,
         curvePivot = TRUE,
         what = "std",
         # style = "mx",
         layout = "tree2",
         edge.label.cex = 0.5,
         curvePivot = FALSE)



matrix(
  c(
    "y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8",
    "x1", NA, NA, "dem60", NA, NA, "dem65", NA,
    "x2", NA, NA, "ind60", NA, NA, NA, NA,
    "x3", NA, NA, NA, NA , NA, NA, NA),
  ,4 )


