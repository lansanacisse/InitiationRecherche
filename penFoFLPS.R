# Chargement des packages nécessaires
library(penFoFPLS)

library(fda)
# library(fof)
# library(ggplot2)

# verifions s'il n'y a pas de valeurs manquantes
is.na(cycling)

# Visualisation des données


# partageons le jeux de données

# X contient les valeurs independantes
# y contiennt la valeurs dependantes

colonnes_a_extraire <- c("SECS", "KM", "CAD", "KPH", "HR", "ALT", "SLOPE", "TEMP")
X <- lapply(colonnes_a_extraire, function(col) cycling[[col]]) 
y <- cycling$WATTS # la variable dependante
# x <- cycling$TEMP
# Régression fPLS pénalisée
# model <- ffpls_bs(X, y, ncomp = 3)

# y = cycling$WATTS
# x = cycling$SECS

donnees_X <- cycling$SECS
donnees_Y <- cycling$WATTS
argvals_X <- seq(0, 1, length.out = ncol(donnees_X))
argvals_Y <- seq(0, 1, length.out = ncol(donnees_Y))

basisobj_X <- fda::create.bspline.basis(rangeval = range(argvals_X), nbasis = 10)
basisobj_Y <- fda::create.bspline.basis(rangeval = range(argvals_Y), nbasis = 10)

ncomp <- 3
penalty_X <- 0
penalty_Y <- 0

res <- penFoFPLS::ffpls_bs(donnees_X, donnees_Y, center = TRUE,
                    argvals_X = argvals_X,
                    argvals_Y = argvals_Y,
                    ncomp = ncomp,
                    basisobj_X = basisobj_X,
                    basisobj_Y = basisobj_Y,
                    penalty_X = penalty_X,
                    penalty_Y = penalty_Y)


# penFoFPLS::means_plot(res$fitted.values, val_type = "MSE")
mse <- mean(res$mvpls_model$residuals^2) # 459.498