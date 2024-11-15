# Propensity Score matching and the NSW job training program
library(tidyverse)
library(haven)

# Read data
#-------------------------------------------------------------------------------
# NSW experimental data
read_data <- function(df)
{
  full_path <- paste("https://github.com/scunning1975/mixtape/raw/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

nsw_dw <- read_data("nsw_mixtape.dta")
#-------------------------------------------------------------------------------


# Estimating ATE from NSW experiment
#-------------------------------------------------------------------------------
# Treated
nsw_dw %>% 
  filter(treat == 1) %>% 
  summary(re78)

mean1 <- nsw_dw %>% 
  filter(treat == 1) %>% 
  pull(re78) %>% 
  mean()

# Control
nsw_dw %>% 
  filter(treat == 0) %>% 
  summary(re78)

mean0 <- nsw_dw %>% 
  filter(treat == 0) %>%
  pull(re78) %>% 
  mean()

# ATE
ate <- mean1 - mean0
#-------------------------------------------------------------------------------


# Now replace control with non-experimental from CPS
#-------------------------------------------------------------------------------
# Drop control
nsw_dw <- nsw_dw %>% 
  filter(treat == 1)

# Get CPS control
nsw_dw_cpscontrol <- read_data("cps_mixtape.dta") %>% 
  bind_rows(nsw_dw) %>% 
  mutate(agesq = age^2,
         agecube = age^3,
         educsq = educ*educ,
         u74 = case_when(re74 == 0 ~ 1, TRUE ~ 0),
         u75 = case_when(re75 == 0 ~ 1, TRUE ~ 0),
         interaction1 = educ*re74,
         re74sq = re74^2,
         re75sq = re75^2,
         interaction2 = u74*hisp)

# estimating prop score
logit_nsw <- glm(treat ~ age + agesq + agecube + educ + educsq + marr +
                   nodegree + black + hisp + re74 + re75 + u74 + u75 +
                   interaction1,
                 family = binomial(link = "logit"),
                 data = nsw_dw_cpscontrol)

# Assign the pscore
nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(pscore = logit_nsw$fitted.values)

pscore_control <- nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  pull(pscore) %>% 
  mean()

pscore_treated <- nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  pull(pscore) %>% 
  mean()

nsw_dw_cpscontrol %>%
  ggplot(aes(x = pscore, fill = factor(treat))) +
  geom_histogram(aes(y = ..density..), # density because the count of group untreated >>> treat
                 color = "#e9ecef", 
                 alpha = 0.3, 
                 position = "identity", 
                 bins = 30) +
  scale_fill_manual(values = c("#69b3a2", "#404080")) +
  labs(fill = "") +
  theme_minimal() +
  theme(legend.position = "top")




