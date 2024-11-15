library(tidyverse)
library(haven)

read_data <- function(df)
{
  full_path <- paste("https://github.com/scunning1975/mixtape/raw/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

training_example <- read_data("training_example.dta") %>% 
  slice(1:20)

view(training_example)

ggplot(training_example, aes(x = age_treat)) +
  stat_bin(bins = 10, na.rm = TRUE)

ggplot(training_example, aes(x=age_control)) +
  geom_histogram(bins = 10, na.rm = TRUE)

outcomes <- training_example %>% 
  select(earnings_treat, earnings_matched) %>% 
  slice(1:10) %>% 
  mutate(
    earnings_treat = as.numeric(earnings_treat),
    unit_treatment_effect = earnings_treat - earnings_matched
  ) %>% pull(unit_treatment_effect) %>% mean()