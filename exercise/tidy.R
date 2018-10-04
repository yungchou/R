install.packages('tidyverse')
library(tidyverse)

## Messy Data Type #1
billboard <- read.csv("n:/dataset/messy.data/billboard.csv")
head(billboard)

# Issues:
# There are multiple observations on a single row.
# We want one observations per row.

head(names(billboard), 10)

# Gather
# Create a new column, week, to gather all observations
billboard2 <- billboard %>%
  gather(key="week", value="rank", wk1:wk76, na.rm = TRUE)
names(billboard2)
head(billboard2)

head(unique(billboard2$week))
head(unique(billboard2$rank))

# The following code works the same
# billboard2 <- billboard %>%
#   gather(key="week", value="rank", -c(1:5), na.rm = TRUE)

billboard3 <- billboard2 %>%
  mutate(
    week = readr::parse_number(week),
    date = readr::parse_date(date.entered) + 7 * (week - 1)) %>%
  select(-date.entered)

head(billboard3)

#============================================================

## Messy Data Type #1
weather <- read.csv("n:/dataset/messy.data/weather.csv")
names(weather)
head(weather,3)

weather2 <- weather %>%
  gather(key=day, rank=value, d1:d31, na.rm = TRUE) %>%
  mutate(day=readr::parse_number(day))
head(weather2)
