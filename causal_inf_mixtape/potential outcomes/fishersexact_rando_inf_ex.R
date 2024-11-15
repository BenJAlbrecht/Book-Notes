# Fisher's Exact test example
# Randomization Inference
library(tidyverse)
library(magrittr)
library(haven)

read_data <- function(df)
{
  full_path <- paste("https://github.com/scunning1975/mixtape/raw/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

# True permutation of treatment D_1
ri <- read_data("ri.dta") %>% 
  mutate(id = c(1:8))

treated <- c(1:4)


combo <- ri %$% as_tibble(t(combn(id, 4)))

