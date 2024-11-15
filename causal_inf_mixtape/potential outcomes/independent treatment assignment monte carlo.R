# independence assumption
# simple difference in outcomes (SDO)
# like a randomized control trial
library(tidyverse)

# note ATE = 0.6

gap <- function() {
  sdo <- tibble(
    y1 = c(7,5,5,7,4,10,1,5,3,9),
    y0 = c(1,6,1,8,2,1,10,6,7,8),
    random = rnorm(10)
  ) %>% 
    arrange(random) %>%  # random treatment assignment
    mutate(
      d = c(rep(1, 5), rep(0, 5)), # switching equation
      y = d * y1 + (1 - d) * y0
    ) %>% 
    pull(y)
  
  sdo <- mean(sdo[1:5] - sdo[6:10])
  
  return(sdo)
}


# monte carlo, 10k simulations
sim <- replicate(100000, gap())
mean(sim)


hist(sim)


