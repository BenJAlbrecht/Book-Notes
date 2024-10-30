library(tidyverse)

# Data prep
#-------------------------------------------------------------------------------
read_data <- function(df) {
  full_path <- paste0("https://github.com/scunning1975/mixtape/raw/master/",
                      df)
  haven::read_dta(full_path)
}

auto <-
  read_data("auto.dta") %>%
  mutate(length = length - mean(length))
#-------------------------------------------------------------------------------


# Regressions
#-------------------------------------------------------------------------------
# How does length affect price?
lm1 <- lm(price ~ length, auto)

# The LONG regression, how do all covariates affect price?
lm2 <- lm(price ~ length + weight + headroom + mpg, auto)

# Remove the covariates linear relationship with the primary one, length
# Here, we get the residuals (M_x_all)(Length)
# This is length without other covariates, or x1 without x2
lm_aux <- lm(length ~ weight + headroom + mpg, auto)
auto <- auto %>% 
  mutate(length_resid = residuals(lm_aux))

# How does the effect of length without the other covariates affect price?
# This is Price with x1 but x1 such that no x2
lm2_alt <- lm(price ~ length_resid, auto)

#
# Grab coefficients from these...
#

coef_lm1 <- lm1$coefficients # y on x1 
coef_lm2_alt <- lm2_alt$coefficients # y on residuals of lm_aux
resid_lm2 <- residuals(lm2)
#-------------------------------------------------------------------------------


# Plotting stuff
#-------------------------------------------------------------------------------
y_single <- tibble(
  price = coef_lm2_alt[1] + coef_lm1[2]*auto$length_resid,
  length_resid = auto$length_resid
)
y_multi <- tibble(
  price = coef_lm2_alt[1] + coef_lm2_alt[2]*auto$length_resid,
  length_resid = auto$length_resid
)
book<-auto %>%
  ggplot(aes(x=length_resid, y = price)) +
  geom_point() +
  geom_smooth(data = y_multi, color = "blue") +
  geom_smooth(data = y_single, color = "red")
#-------------------------------------------------------------------------------


# Can I recreate this?
#-------------------------------------------------------------------------------
df <- auto %>% 
  select(-length_resid)

# short reg
short <- lm(price ~ length, df)

# Long regression
long <- lm(price ~ length + weight + headroom + mpg, df)

# First reg
first <- lm(price ~ weight + headroom + mpg, df)
first_res <- residuals(first)

# Second reg
second <- lm(length ~ weight + headroom + mpg, df)
second_res <- residuals(second)

# Final residual regression
df <- df %>% 
  mutate(
    first_res = first_res,
    second_res = second_res
  )

res_reg <- lm(first_res ~ second_res, df)

# Replicate ggplot
single_model <- tibble(
  price = short$coefficients[1] + short$coefficients[2]*df$second_res,
  second_res = df$second_res
)
multi_model <- tibble(
  price = short$coefficients[1] + res_reg$coefficients[2]*df$second_res,
  second_res = df$second_res
)


me <- ggplot(df,
       aes(x = second_res, y = price)) +
  geom_point() +
  geom_smooth(data = single_model, color = "lightblue") +
  geom_smooth(data = multi_model, color = "darkred")





book
me





























