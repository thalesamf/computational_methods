###########################
#Time-series
###########################

#Packages----------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)

#Importing data ---------------------------------------------------------------
covid <- read.csv("data/raw/covid19-dd7bc8e57412439098d9b25129ae6f35.csv")


#Converting into date format
# First checking the class
class(covid$date)

# Changing to date format
covid$date <- as_date(covid$date)
class(covid$date)

# Now we can make numeric operations
range(covid$date)

#Plotting a time series with ggplot2
ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal()

#Note that there are negative cases. Thus, we need to substitute per zero
covid$new_confirmed[covid$new_confirmed < 0] <- 0

ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal() +
  labs(x = "Date", y = "New cases")

#Statistical applications ------------------------------------------------------

#Rolling mean
covid$row_mean <- zoo::rollmean(covid$new_confirmed, 14, fill = NA)

head(covid)

ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal() +
  labs(x = "Date", y = "New cases") +
  scale_x_date (breaks = "4 months", date_labels = "%Y-%m") +
  geom_line(aes(x = date, y = row_mean), colour = "red", size = 1.2)
