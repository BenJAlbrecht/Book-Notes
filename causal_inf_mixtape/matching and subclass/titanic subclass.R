# subclassification, titanic ex
library(tidyverse)
library(haven)
library(stargazer)


read_data <- function(df)
{
  full_path <- paste("https://github.com/scunning1975/mixtape/raw/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

titanic <- read_data("titanic.dta")

ggplot(titanic, aes(x = class)) +
  geom_bar()

# naive SDO
#-------------------------------------------------------------------------------
# make treatment dummy
titanic <- titanic %>% 
  mutate(d = ifelse(class == 1, 1, 0))

# Naive SDO
y1 <- titanic %>% 
  filter(d == 1) %>% 
  pull(survived) %>% 
  mean()

y0 <- titanic %>% 
  filter(d == 0) %>% 
  pull(survived) %>% 
  mean()

# biased because it doesn't adjust for observable confounders age / gender
sdo <- y1 - y0
#-------------------------------------------------------------------------------


# Subclassification strategy
#-------------------------------------------------------------------------------
# age == 0 -> child
# age == 1 -> adult
# stratifying by age / gender
titanic <- titanic %>% 
  mutate(
    s = case_when(
      sex == 0 & age == 1 ~ 1,
      sex == 0 & age == 0 ~ 2,
      sex == 1 & age == 1 ~ 3,
      sex == 1 & age == 0 ~ 4,
      TRUE ~ 0
    )
  )

# y(strata)(treatment)
y11 <- titanic %>% filter(s == 1, d == 1) %$% mean(survived)
y10 <- titanic %>% filter(s == 1, d == 0) %$% mean(survived)

y21 <- titanic %>% filter(s == 2, d == 1) %$% mean(survived)
y20 <- titanic %>% filter(s == 2, d == 0) %$% mean(survived)

y31 <- titanic %>% filter(s == 3, d == 1) %$% mean(survived)
y30 <- titanic %>% filter(s == 3, d == 0) %$% mean(survived)

y41 <- titanic %>% filter(s == 4, d == 1) %$% mean(survived)
y40 <- titanic %>% filter(s == 4, d == 0) %$% mean(survived)

y_s_d <- titanic %>% group_by(s, d) %>% summarize(y = mean(survived))

# Survival probability by group
diff1 <- y11 - y10
diff2 <- y21 - y20
diff3 <- y31 - y30
diff4 <- y41 - y40

y_s_d <- y_s_d %>% 
  summarize(diff = y[d==1] - y[d==0], .groups = 'drop')

# Strata specific weighting
obs <- nrow(titanic %>% filter(d == 0))

wt1 <- titanic %>% filter(s == 1, d == 0) %$% nrow(.) / obs
wt2 <- titanic %>% filter(s == 2, d == 0) %$% nrow(.) / obs
wt3 <- titanic %>% filter(s == 3, d == 0) %$% nrow(.) / obs
wt4 <- titanic %>% filter(s == 4, d == 0) %$% nrow(.) / obs

wts <- titanic %>% 
  filter(d == 0) %>% 
  group_by(s) %>% 
  summarize(n = n()) %>% 
  mutate(
    tot = sum(n),
    w = n / tot
  ) %>% 
  select(s, w)
  
# weighted ate
wate <- left_join(y_s_d, wts, by = "s") %>% mutate(diffw = diff * w) %$% sum(diffw)


stargazer(wate, sdo, type = "text")


