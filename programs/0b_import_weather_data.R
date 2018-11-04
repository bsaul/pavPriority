#------------------------------------------------------------------------------#
#    TITLE : Import weather data
#     DATE : 2018NOV04
#   AUTHOR : B. Saul
#  PURPOSE : Get weather data to use as covariates
#------------------------------------------------------------------------------#

# Downloaded weather data from https://www.ncdc.noaa.gov/ on 2018-11-03 for daily
# summaries of weather in zip code 27516
# TAVG - Average Temperature.
# TMIN - Minimum temperature
# TMAX - Maximum temperature
# PRCP - Precipitation
# AWND - Average wind speed
weather <- read.csv("inst/extdata/1526196.csv", stringsAsFactors = FALSE) %>%
  select(date = DATE, 
         temp_avg =  TAVG, temp_max = TMAX, temp_min = TMIN, precip = PRCP, wind_avg = AWND) %>%
  group_by(date) %>%
  # TODO: is there a better way to summarize the data?
  summarise_all(
    funs(mean), na.rm = TRUE
  ) %>%
  mutate(date = as.Date(date))

# Weather variables to create
# average hi temp in the X past days prior to date
# total precip in the X past days prior to date

make_weather_variable <- function(.var, .f, .date, .lookback, .wdt){
  ind <- .wdt$date %in% (.date - 1):(.date - .lookback - 1)
  .f(.wdt[[.var]][ind])
}

make_weather_variable <- Vectorize(make_weather_variable, vectorize.args = ".date")


