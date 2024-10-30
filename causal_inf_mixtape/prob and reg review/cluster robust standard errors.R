# 2.27 Cluster robust standard errors


# libraries
#-------------------------------------------------------------------------------
library(tidyverse)
library(arm)
library(mvtnorm)
library(lme4)
library(multiwayvcov)
library(clusterSEs)
#-------------------------------------------------------------------------------


# Function to generate clustered data
#-------------------------------------------------------------------------------
gen_cluster <- function(param = c(.1, .5), n = 1000, n_cluster = 50, rho = .5) {
  # individual level
  Sigma_i <- matrix(c(1, 0, 0, 1 - rho), ncol = 2)
  values_i <- rmvnorm(n = n, sigma = Sigma_i)
  
  # cluster level
  cluster_name <- rep(1:n_cluster, each = n / n_cluster)
  Sigma_cl <- matrix(c(1, 0, 0, rho), ncol = 2)
  values_cl <- rmvnorm(n = n_cluster, sigma = Sigma_cl)
  
  # predictor var consists of individual and cluster lvl components
  x <- values_i[, 1] + rep(values_cl[, 1], each = n / n_cluster)
  
  # same with error
  error <- values_i[, 2] + rep(values_cl[, 2], each = n / n_cluster)
  
  # data gen process
  y <- param[1] + param[2]*x + error
  
  df <- data.frame(x, y, cluster = cluster_name)
  return(df)
}
#-------------------------------------------------------------------------------


# Simulate dataset with clusters and fit OLS
# Calculate cluster-robust SE when cluster_robust = TRUE
#-------------------------------------------------------------------------------
cluster_sim <- function(param = c(.1, .5), n = 1000, n_cluster = 50,
                        rho = .5, cluster_robust = FALSE) {
  df <- gen_cluster(param = param, n = n, n_cluster = n_cluster, rho = rho)
  
  fit <- lm(y ~ x, data = df)
  
  b1 <- coef(fit)[2]
  if(!cluster_robust) {
    Sigma <- vcov(fit)
    se <- sqrt(diag(Sigma)[2])
    b1_ci95 <- confint(fit)[2, ]
  } else { # cluster-robust SE
    Sigma <- cluster.vcov(fit, ~ cluster)
    se <- sqrt(diag(Sigma)[2])
    t_critical <- qt(.025, df = n - 2, lower.tail = FALSE)
    lower <- b1 - t_critical*se
    upper <- b1 + t_critical*se
    b1_ci95 <- c(lower, upper)
  }
  return(c(b1, se, b1_ci95))
}
#-------------------------------------------------------------------------------


# Function to iterate the simulation, returns a DF
#-------------------------------------------------------------------------------
run_cluster_sim <- function(n_sims = 1000, param = c(.1, .5), n = 1000,
                            n_cluster = 50, rho = .5, cluster_robust = FALSE) {
  df <- replicate(n_sims, cluster_sim(param = param, n = n, rho = rho,
                                      n_cluster = n_cluster,
                                      cluster_robust = cluster_robust))
  df <- as.data.frame(t(df))
  names(df) <- c('b1', 'se_b1', 'ci95_lower', 'ci95_upper')
  df <- df %>% 
    mutate(id = 1:n(),
           param_caught = ci95_lower <= param[2] & ci95_upper >= param[2])
  return(df)
}
#-------------------------------------------------------------------------------


# Distribution of the estimator and confidence intervals
#-------------------------------------------------------------------------------
sim_params <- c(.4, 0) # beta1 = 0: no effect of x on y
sim_nocluster <- run_cluster_sim(n_sims = 10000, param = sim_params, rho = 0)
hist_nocluster <- ggplot(sim_nocluster, aes(b1)) +
  geom_histogram(color = "black") +
  geom_vline(xintercept = sim_params[2], color = 'red')
#-------------------------------------------------------------------------------


# 95% confidence intervals of the slope
# LOVE LOVE LOVE this plot
#-------------------------------------------------------------------------------
ci95_nocluster <- ggplot(sample_n(sim_nocluster, 100),
                         aes(x = reorder(id, b1), y = b1, 
                             ymin = ci95_lower, ymax = ci95_upper,
                             color = param_caught)) +
  geom_hline(yintercept = sim_params[2], linetype = 'dashed') +
  geom_pointrange() +
  labs(x = 'sim ID', y = 'b1', title = 'Randomly Chosen 100 95% CIs') +
  scale_color_discrete(name = 'True param value', labels = c('missed', 'hit')) +
  coord_flip()
#-------------------------------------------------------------------------------


# What happens when we use OLS with clustered data?
#-------------------------------------------------------------------------------
sim_params <- c(.4, 0)   # beta1 = 0: no effect of x on y
sim_cluster_ols <- run_cluster_sim(n_sims = 10000, param = sim_params)
hist_cluster_ols <- hist_nocluster %+% sim_cluster_ols
# thinner, less variance of the OLS estimator
#-------------------------------------------------------------------------------


# confidence intervals with the clustered OLS
#-------------------------------------------------------------------------------
ci95_cluster_ols <- ci95_nocluster %+% sample_n(sim_cluster_ols, 100)
# data = clustered, but wayyyy more misses
# Stddev shrinks under clustered data, so we reject the null incorrectly
# too often!
#-------------------------------------------------------------------------------


# Clustered robust SE's
#-------------------------------------------------------------------------------
sim_params <- c(.4, 0)
sim_cluster_robust <- run_cluster_sim(n_sims = 10000, param = sim_params,
                                      cluster_robust = TRUE)

hist_cluster_robust <- hist_nocluster %+% sim_cluster_ols

# ci's 
ci95_cluster_robust <- ci95_nocluster %+% sample_n(sim_cluster_robust, 100)






















  
  




































