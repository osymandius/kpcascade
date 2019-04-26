setwd("~/Documents/GitHub/kpcascade/src")

library(rstan)

data <- data.frame("prev"= 0.55, "se" = 0.04)

stan_data <- list(
  N = length(data$prev),
  prev = data$prev,
  se = data$se
  # prior_mu = 0.6,
  # prior_tau = 0.03
  # low = 0.2,
  # up = 0.8
)

stanc("combine_estimates.stan")

stan_model <- "combine_estimates.stan"

fit <- stan(file=stan_model, data = stan_data, warmup = 500, iter = 5000, chains = 4, cores = 4, thin =1)
fit

mean(data$prev)

posterior <- extract(fit)

hist(sample(posterior$theta, 100))
hist(posterior$theta)

plot(~data$prev, xlim=c(0.4, 0.7))
