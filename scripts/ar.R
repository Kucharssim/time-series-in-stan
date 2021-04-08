library(forecast)
library(here)
library(knitr)
library(rstan)
source(here("R", "stationary_ar.R"))

ar_stan_model <- rstan::stan_model(here::here("stan", "ar.stan"))

# Simulate AR data ----
true_p <- 3
( true_beta  <- stationary_ar(true_p) )
( true_sigma <- rgamma(1, 1, 1)       )

T_obs <- 200

stan_data <- list(
  P = true_p,
  T = T_obs,
  F = ceiling(T_obs * 0.2),
  y = as.vector(arima.sim(list(ar=true_beta, sd = true_sigma, m = 0), T_obs))
)

plot.ts(stan_data$y)
arima(stan_data$y, order = c(3, 0, 0))

# Fit the data ----
ar_stan_fit <- rstan::sampling(ar_stan_model, stan_data, cores = 4)
knitr::kable(summary(ar_stan_fit, pars = c("alpha", "beta", "sigma"))$summary, digits = 2)

# Plots the forecast
probs <- c(0.025, 0.975)
sum_stats <- c("mean", sprintf("%s%%", probs*100))
confidence <- summary(ar_stan_fit, pars = "mu",    probs = probs)$summary[, sum_stats]
prediction <- summary(ar_stan_fit, pars = "y_rep", probs = probs)$summary[, sum_stats]
colnames(confidence) <- colnames(prediction) <- c("mean", "lower", "upper")
y_range <- range(range(prediction), range(stan_data$y))
confidence <- as.data.frame(confidence)
prediction <- as.data.frame(prediction)

plot_interval <- function(lower, upper, col) {
  x <- seq_along(lower)
  x <- c(x, rev(x))
  y <- c(lower, rev(upper))
  polygon(x, y, col = col, border = col)
}

plot(seq_len(stan_data$T+stan_data$F), type = "n", ylim = y_range)
plot_interval(prediction$lower, prediction$upper, adjustcolor("red", 0.5))
plot_interval(confidence$lower, confidence$upper, adjustcolor("blue", 0.5))
lines(confidence$mean, col = "blue")
lines(stan_data$y, lwd = 2)
