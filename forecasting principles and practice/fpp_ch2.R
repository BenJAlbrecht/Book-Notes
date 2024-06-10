# FPP Ch 2 #
############
library(tidyverse)
library(fpp3)
library(readr)
library(GGally)


######################
# The Index Variable #
######################

# we can store time series in a tsibble object
y <- tsibble(
  Year = 2015:2019,
  Observation = c(123, 39, 78, 52, 110),
  index = Year
)




#####################
# The key variables #
#####################

# tsibble also allows multiple ts to be stored in one objet.
# suppose you are interested in a dataset containing the fastest run times
# for men and women track races at the olympics
olympic_running
# note the key: length, sex [14]
# telling us there are 14 ts in this object uniquely identifed by the keys
# length and sex

# the distinct() function can be used to show the categories of each var
# or even combinations of vars
olympic_running %>% distinct(Sex)




################################
# Working with tsibble objects #
################################

# sales data on pharmaceutical products in Australia
PBS    # monthly data on medicare australia prescription data from July 1991 to June 2008

# we are interested in the Cost time series (total cost of scripts in aussie dollars)

# extract the A10 scripts, select needed columns
PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) # note that the index var and the keys
                                        # would be returned even if not explicitly
                                        # selected, as they are required for a tsibble

# another useful fcn is summarise() which allows us to combine data across keys
# EX: total cost per month regardless of concession or type keys
PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost))

# we can also use mutate
# EX: change units from dollars to millions
PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost)) %>%
  mutate(Cost = TotalC/1e6)

# save this for future analysis
a10 <- PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost)) %>%
  mutate(Cost = TotalC/1e6)




###########################################
# Read a csv file and conver to a tsibble #
###########################################

# everything in this book is a tsibble. but we often read csvs so we'll need
# to learn how to convert things!!!

# ex: size of prison pop in aus
prison <- read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")

# identify cols which contain time and keys
prison <- prison %>%
  mutate(Quarter = yearquarter(Date)) %>%
  select(-Date) %>%
  as_tsibble(key = c(State, Gender, Legal, Indigenous),
             index = Quarter)




#######################
# The seasonal period #
#######################
# the seasonal period == number of obs before the seasonal pattern repeats
#                        in most cases this is automatically detected using the time index var




##############
# Time plots #
##############
# weekely economy passanger load on Ansett airlines between aus's 2 largest cities
melsyd_economy <- ansett %>%
  filter(Airports == "MEL-SYD", Class == "Economy") %>%
  mutate(Passengers = Passengers / 1000)

autoplot(melsyd_economy, Passengers) +
  labs(title = "Ansett airlines economy class",
       subtitle = "Melbourne-Sydney",
       y = "Passengers ('000)")

# a simpler time series using the a10 data from earlier
autoplot(a10, Cost) +
  labs(y = "$ (millions)",
       title = "Australian antidiabetic drug sales")




##################
# Seasonal Plots #
##################

# seasonal plots allow us to see underlying seasonal patterns more clearly
# also useful in identifying years in which the pattern changes
# ex antidiabetic drug sales
a10 %>%
  gg_season(Cost, labels = "both") +
  labs(y = "$ (millions)",
       title = "Seasonal plot: Antidiabetic drug sales") 
# notice the large jump in sales in january for each year.
# theyre actually sales in late december as customers stockpile but sales
# are not registered until a week or two later

# multiple seasonal periods
# we can use the period arg to select which seasonal plot is requried
# vic_elec data == half-hourly electricity demand for the state of victoria
# period can do like yearly, weekly, daily, etc..
vic_elec %>%
  gg_season(Demand, period = "day") +
  theme(legend.position = "none") +
  labs(y = "MWh",
       title = "Electricity demand: Victoria")

# now weekly
vic_elec %>%
  gg_season(Demand, period = "week") +
  theme(legend.position = "none") +
  labs(y = "MWh",
       title = "Electricity demand: Victoria")

# yearly
vic_elec %>%
  gg_season(Demand, period = "year") +
  labs(y = "MWh",
       title = "Electricity demand: Victoria")




############################ 
# Seasonal subseries plots #
############################
# an alternative plot to identify seasonality is where the data for each season
# are collected together in separate mini time plots
a10 %>%
  gg_subseries(Cost) +
  labs(
    y = "$ (millions)",
    title = "Australian antidiabetic drug sales"
  ) # not super revealing but still useful

# ex: australian holiday tourism
holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))

# time plot shows that there's strong seasonality for most states
# but seasonal peaks don't coincide!
autoplot(holidays, Trips) +
  labs(
    y = "Overnight trips ('000)",
    title = "Australian domestic holidays"
  )

# to see the timing we can use a season plot
# clearly southern states have strongest tourism in Q1 (summer)
# while the northern states have strongest in Q3 (their dry season)
gg_season(holidays, Trips) +
  labs(
    y = "Overnight trips ('000)",
    title = "Australian domestic holidays"
  )
# corresponding subseries
holidays %>%
  gg_subseries(Trips) +
  labs(
    y = "Overnight trips ('000)",
    title = "Australian domestic holidays"
  )




################
# Scatterplots #
################
# we have so far visualized individual series
# now we may want to see relations between series

# EX:
# graph1: half-hour electricity demand in victoria
vic_elec %>%
  filter(year(Time) == 2014) %>%
  autoplot(Demand) +
  geom_line(lwd = 1.25) +
  labs(
    y = "GW",
    title = "Half-hourly electricity demand: Victoria"
  )

# graph2: half-hourly temp in melbourne
vic_elec %>%
  filter(year(Time) == 2014) %>%
  autoplot(Temperature) +
  geom_line(lwd = 1.25) +
  labs(
    y = "Degrees Celsius",
    title = "Half-hourly temperatures: Melbourne, Australia"
  )

# Relationship between demand and temperature, using a scatterplot
vic_elec %>%
  filter(year(Time) == 2014) %>%
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point(size = 1.5, colour = "red") +
  labs(
    x = "Temperature (degrees Celsius)",
    y = "Electricity demand (GW)"
  )




########################
# Scatterplot Matrices #
########################

# if there are other potential predictors, we can plot each
visitors <- tourism %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))

# plot
visitors %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line(lwd = 1.2) +
  facet_grid(vars(State), scales = "free_y") +
  labs(
    title = "Australian domestic tourism",
    y = "Overnight trips ('000)"
  )

# To see the relation between these, we can plot each time series against the
# others.
# Scatterplot matrix with GGally
visitors %>%
  pivot_wider(values_from = Trips, names_from = State) %>%
  ggpairs(columns = 2:9)




#############
# Lag plots #
#############
# scatterplots of quarterly aussie beer production with lagged vals
recent_production <- aus_production %>%
  filter(year(Quarter) >= 2000)
recent_production %>%
  gg_lag(Beer, geom = "point") +
  labs(x = "lag(Beer, k)")




###############
# White noise #
###############
set.seed(30)
y <- tsibble(
  sample = 1:50,
  wn = rnorm(50),
  index = sample
)

# wn time series plot
y %>%
  autoplot(wn) +
  labs(title = "White noise",
       y = "")

# wn acf plot
y %>%
  ACF(wn) %>%
  autoplot() + labs(title = "White noise")

###########################
#  |~#===============#~|  #
#  |~|---Exercises---|~|  #
#  |~#===============#~|  #
###########################

# exercise 1
# drop some NAs
drop_na.tbl_ts <- function(ts) tsibble::as_tsibble(tidyr:::drop_na.data.frame(ts))

bricks_ts <- aus_production %>%
  select(Bricks) %>%
  drop_na()
autoplot(bricks_ts, Bricks)

autoplot(pelt, Lynx)

autoplot(gafa_stock, Close)

autoplot(vic_elec, Demand) +
  geom_line(colour = "blue") +
  labs(title = "Half-hourly electricity demand for Victoria, Australia",
       x = "Time (half-hour)",
       y = "Electricity demand")

# exercise 2
for (stock in unique(gafa_stock$Symbol)) {
  print(gafa_stock %>%
    filter(Symbol == stock) %>%
    slice_max(Close, n = 1))
}

# exercise 3
tute1 <- readr::read_csv("tute1.csv")

tute_ts <- tute1 %>%
  mutate(Quarter = yearquarter(Quarter)) %>%
  as_tsibble(index = Quarter)

tute_ts %>%
  pivot_longer(-Quarter) %>%
  ggplot(aes(x = Quarter,
             y = value)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y")

# exercise 4
library(USgas)
us_gas_ts <- us_total %>%
  as_tsibble(
    key = state,
    index = year
  )

ne_area <- c("Maine", "Vermont", "New Hampshire",
             "Massachusettes", "Connecticut", "Rhode Island")
new_eng <- us_gas_ts %>%
  filter(state %in% ne_area)
ggplot(new_eng,
       aes(x = year, y = y, color = state)) +
  geom_line(lwd = 1.5) +
  labs(title = "Demand for natural gas in New England",
       x = "Year", y = "Demand")

# exercise 5
tourism_exercise <- readxl::read_excel("tourism.xlsx")

tour_ex <- tourism_exercise %>%
  mutate(Quarter = yearquarter(Quarter)) %>%
  as_tsibble(
    key = c(Region, State, Purpose),
    index = Quarter
  )

avg_trips_by_rg_prp <- tour_ex %>%
  group_by(Region, Purpose) %>%
  summarise(mean_value = mean(Trips, na.rm = TRUE), .groups = 'drop')

max_avg_comb <- avg_trips_by_rg_prp %>%
  filter(mean_value == max(mean_value))

tour_by_state <- tour_ex %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))

# exercise 6
aus_arrivals
autoplot(aus_arrivals, Arrivals) +
  geom_line(lwd = 1)

aus_arrivals %>%
  gg_season(Arrivals, period = "year")

aus_arrivals %>%
  gg_subseries(Arrivals)

# exercise 7
set.seed(1234589)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

autoplot(myseries, Turnover) + geom_line(lwd = 1)

myseries %>%
  gg_season(Turnover, period = "year")

myseries %>%
  gg_subseries(Turnover)

myseries %>%
  gg_lag(Turnover, geom = "point")

myseries %>%
  ACF(Turnover) %>%
  autoplot()


# Exercise 8
##############
# TS 1 #
########
us_employment

private_emp <- us_employment %>%
  filter(Title == "Total Private")

# autplot of ts
autoplot(private_emp, Employed) + geom_line(lwd = 1)
# positive trend upwards with some cyclical movements, also some seasonality

# ggszn
private_emp %>%
  gg_season(Employed) # no seasonality actually?

# ggsubser
private_emp %>%
  gg_subseries(Employed)

# gglag
private_emp %>%
  gg_lag(Employed, geom = "point")

# acf
private_emp %>% ACF(Employed) %>% autoplot()

# TS 2 #
########
aus_production

bricks <- aus_production %>%
  select(Bricks)

# autoplot
autoplot(bricks, Bricks) + geom_line(lwd = 1)

# ggszn
bricks %>%
  gg_season(Bricks)

# ggsubser
bricks %>% gg_subseries(Bricks)

# gglag
bricks %>% gg_lag(Bricks, geom = "point")

# acf
bricks %>% ACF(Bricks) %>% autoplot()

# TS 3 #
########
pelt %>% as_tsibble(
  index = Year
)

hare <- pelt %>% select(Hare)

# autoplot
autoplot(hare, Hare) + geom_line(lwd = 1)

# gglag
hare %>% gg_lag(Hare, geom = "point")

# acf
hare %>% ACF(Hare) %>% autoplot()

# TS 4 #
########
PBS

ts4 <- PBS %>% filter(ATC2 == "H02")

# auto
autoplot(ts4, Cost) + geom_line(lwd = 1)

# ggszn
ts4 %>% gg_season(Cost)

# ggsubser
ts4 %>% gg_subseries(Cost)

# ex 10
aus_livestock

piggies <- aus_livestock %>%
  filter(year(Month) >= 1990,
         year(Month) <= 1995) %>%
  filter(Animal == "Pigs") %>%
  filter(State == "Victoria")

autoplot(piggies, Count) + geom_line(lwd = 1)

piggies %>% ACF(Count) %>% autoplot()

pig2 <- aus_livestock %>%
  filter(Animal == "Pigs") %>%
  filter(State == "Victoria")

autoplot(pig2, Count) + geom_line(lwd = 1)

pig2 %>% ACF(Count) %>% autoplot()

# exercise 11
dgoog <- gafa_stock %>%
  filter(Symbol == "GOOG",
         year(Date) >= 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE) %>%
  mutate(diff = difference(Close))

autoplot(dgoog, diff) + geom_line(lwd = 1)

dgoog %>% ACF(diff) %>% autoplot()



















