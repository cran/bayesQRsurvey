## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      fig.width = 7, fig.height = 4.5, fig.align = "center")

## ----eval = FALSE-------------------------------------------------------------
# install.packages("bayesQRsurvey")

## ----message = FALSE----------------------------------------------------------
library("bayesQRsurvey")
library("ggplot2")

## -----------------------------------------------------------------------------
theme_set(theme_classic(base_size = 14) +
  theme(axis.text         = element_text(colour = "black"),
        legend.title      = element_blank(),
        legend.background = element_blank()))

scale_tau <- scale_colour_manual(
  values = c("0.1" = "grey70", "0.5" = "grey40", "0.9" = "black"),
  breaks = c("0.9", "0.5", "0.1"),
  labels = c("0.9" = expression(tau == 0.9),
             "0.5" = expression(tau == 0.5),
             "0.1" = expression(tau == 0.1)))

## -----------------------------------------------------------------------------
data("Anthro", package = "bayesQRsurvey")

Anthro$age <- Anthro$age / 12
Anthro$sex <- factor(Anthro$sex, levels = c("1", "0"),
                     labels = c("Boys", "Girls"))

str(Anthro)

## -----------------------------------------------------------------------------
set.seed(50)
fit_ald <- bqr.svy(wgt ~ age + I(age^2) + sex, weights = dweight,
                   data = Anthro, quantile = c(0.1, 0.5, 0.9),
                   niter = 6000, burnin = 3000, thin = 1, verbose = FALSE)

fit_ald

## -----------------------------------------------------------------------------
print(summary(fit_ald), tau = 0.5)

## -----------------------------------------------------------------------------
fit_ald$diagnosis[["tau=0.500"]]

## ----fig.height = 5-----------------------------------------------------------
plot(fit_ald, type = "trace", tau = 0.5,
     color_palette = "grey", theme_style = "none")

## ----fig.height = 5-----------------------------------------------------------
plot(fit_ald, type = "density", tau = 0.5,
     color_palette = "grey", theme_style = "none")

## -----------------------------------------------------------------------------
plot(fit_ald, type = "fit", which = "age", add_points = FALSE,
     color_palette = "none", theme_style = "none") +
  scale_tau + labs(x = "Age (years)", y = "Weight (kg)") +
  theme(legend.position = "inside", legend.position.inside = c(0.85, 0.18))

## ----fig.height = 5-----------------------------------------------------------
set.seed(50)
fit_grid <- bqr.svy(wgt ~ age + I(age^2) + sex, weights = dweight,
                    data = Anthro, quantile = seq(0.1, 0.9, by = 0.2),
                    niter = 6000, burnin = 3000, thin = 1, verbose = FALSE)

plot(fit_grid, type = "quantile", add_ols = TRUE,
     color_palette = "grey", theme_style = "none") + labs(x = "quantile")

## -----------------------------------------------------------------------------
myprior <- prior(beta_x_mean = rep(0, 4), beta_x_cov = 25)

set.seed(50)
fit_prior <- bqr.svy(wgt ~ age + I(age^2) + sex, weights = dweight,
                     data = Anthro, quantile = 0.5,
                     niter = 6000, burnin = 3000, thin = 1,
                     prior = myprior, verbose = FALSE)

## -----------------------------------------------------------------------------
set.seed(50)
fit_score <- bqr.svy(wgt ~ age + I(age^2) + sex, weights = dweight,
                     data = Anthro, method = "score", quantile = 0.5,
                     niter = 20000, burnin = 5000, thin = 1, verbose = FALSE)

summary(fit_score)

## -----------------------------------------------------------------------------
set.seed(50)
fit_mo <- mo.bqr.svy(cbind(wgt, hgt) ~ age + I(age^2) + sex,
                     weights = dweight, data = Anthro,
                     quantile = c(0.05, 0.10, 0.15),
                     n_dir = 20, max_iter = 2000, verbose = FALSE)

fit_mo

## -----------------------------------------------------------------------------
print(summary(fit_mo), coefficients = FALSE)

## ----fig.height = 5.5---------------------------------------------------------
plotQuantileRegion(fit_mo, response = c("wgt", "hgt"),
                   datafile = Anthro, xValue = c(1, 2, 4, 0),
                   ngridpoints = 200, paintedArea = FALSE,
                   color_palette = "grey", theme_style = "none")

