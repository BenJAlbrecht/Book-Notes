library(tidyverse)
library(haven)
library(FNN)
library(magrittr)

read_data <- function(df)
{
  full_path <- paste("https://github.com/scunning1975/mixtape/raw/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

train_bias_red <- read_data("training_bias_reduction.dta") %>% 
  as_tibble()

# Make it like the table in the book
df <- train_bias_red %>% 
  mutate(
    Y1 = case_when(
      D == 1 ~ Y,
      TRUE ~ NA
    ),
    Y0 = case_when(
      D == 0 ~ Y,
      Unit == 1 ~ 4,
      Unit == 2 ~ 0,
      Unit == 3 ~ 5,
      Unit == 4 ~ 1
    )
  ) %>% 
  relocate(
    Unit, Y1, Y0, D, X
  )

# Estimating muhat^0(x)
train_reg <- df %>% lm(Y ~ X, .)
df <- df %>% 
  mutate(
    muhat = fitted(train_reg)
  )


# join to find matched values
matched_df <- df %>% 
  filter(D == 1) %>% 
  select(-Y, -D) %>% 
  left_join(
    df %>% filter(D == 0) %>% 
      select(Unit, Y0, muhat),
    by = c("Y0"),
    suffix = c("_t", "_c")
  )

# Get the bias corrected estimator
matched_df <- matched_df %>% 
  mutate(
    diffs = (Y1 - Y0) - (muhat_t - muhat_c)
  ) %>% 
  summarize(
    m = mean(diffs)
  )













