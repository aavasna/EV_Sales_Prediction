# source for the data
# a complete description of the series (dates, periodicity, units of measurement)
library(lubridate)
library(tidyverse)
library(fpp3)
library(ggplot2)
library(dplyr)
library(readxl)

# importing all data sets
ev_sales <- read_csv("data/EV_Total_Sales.csv")
car_price_cpi <- read_csv("data/car_price_cpi.csv") |>
  rename(car_cpi = CUUR0000SETA01, Month = DATE) |>
  filter(year(Month) >= 2017)

gas_prices <- read_csv("data/gas_prices.csv") |>
  rename(price = GASREGW, Month = DATE)
gas_prices$Month <- yearmonth(gas_prices$Month, "%m-b-%y")
gas_prices <- filter(gas_prices, year(Month) >= 2017)

google_trend <- read_csv("data/google_trend.csv", 
                         skip = 1) |>
  rename(search = 'electric car: (United States)')
PPI <- read_csv("data/PPI.csv") |>
  rename(price = PCU3363233632, Month = DATE)

# formatting the laws data set
laws <- read_csv("data/laws_and_incentives.csv") |>
  filter (State == 'US') |>
  select('State', 'Enacted Date') |>
  rename(Date = 'Enacted Date') |>
  drop_na(Date)
laws <- laws |>
  group_by(State, Date) |>
  mutate(count = n()) |>
  distinct(State, Date, .keep_all = TRUE)
laws$Date <- yearmonth(laws$Date)
laws$Date <- as_date(laws$Date)
monthly_dates <- seq(as.Date("2011-01-01"), as.Date("2023-04-01"), by = "month")
laws <- laws |>
  group_by(State) |>
  mutate(cumulative_count = cumsum(count))
laws$Date <- as.Date(paste(laws$Date, "01", sep = "-"))
laws <- laws |>
  complete(Date = monthly_dates, fill = list(count = 0)) %>%
  ungroup()
laws <- laws |>
  group_by(State) |>
  mutate(law_count = cumsum(count)) |>
  filter(year(Date) >= 2011) |>
  select(State, Date, law_count) |>
  rename(Month = Date) |>
  filter(year(Month) >= 2017)

# Create a tsibble with monthly frequency
ev_sales$Month <- yearmonth(ev_sales$Month, "%y-%b")
ev_ts <- ev_sales %>%
  as_tsibble(index = Month)

#create tsibble for other variables

gas_prices <- gas_prices |>
  as_tsibble(index = Month)
gas_prices |> autoplot(price) +
  labs(title = "Gas Prices",
       x = "Dates",
       y = "Dollars Per Gallon")

google_trend$Month <- yearmonth(google_trend$Month)
google_trend <- google_trend |>
  filter(year(Month) >= 2017) |>
  as_tsibble(index = Month)
google_trend |> autoplot(search) +
  labs(title = 'Google Trends Search for "electric car"',
       x = "Dates",
       y = "Search Count")

car_price_cpi$Month <- yearmonth(car_price_cpi$Month)
car_price_cpi <- car_price_cpi |>
  as_tsibble(index = Month)
car_price_cpi |> autoplot(car_cpi) +
  labs(title = 'CPI for All Urban Consumers, New Vehicles in U.S. City Average, Index 1982 - 1984 = 100',
       x = "Dates",
       y = "CPI")

laws$Month <- yearmonth(laws$Month)
laws <- laws |>
  filter(year(Month) >= 2017) |>
  as_tsibble(index = Month)
  
laws |> autoplot(law_count) +
  labs(title = 'Number of Laws/Incentives Related to EVs in the USA ',
       x = "Dates",
       y = "Number of Laws")
laws <- laws |>
  mutate(after_BBB_act = ifelse(yearmonth(laws$Month) < yearmonth("Nov 2021"), 0, 1))

PPI$Month <- yearmonth(PPI$Month)
PPI <- PPI |>
  as_tsibble(index = Month)

ev_ts_2017 <- ev_ts |>
  filter(year(Month) >= 2017)

# to make data stationary for series, get % change
ev_ts_2017 <- ev_ts_2017 %>%
  mutate(sales = 100 * (BEV / lag(BEV) - 1))
# predictors
gas_prices <- gas_prices %>%
  mutate(price_change = 100 * (price / lag(price) - 1))
gas_prices <-gas_prices %>%
  slice(-c(1,2))
gas_prices%>%
  autoplot(price_change) +
  labs(y="", title="Gas prices (stationary)")
google_trend <- google_trend %>%
  mutate(trend = 100 * (search / lag(search) - 1))
google_trend <-google_trend %>%
  slice(-c(1,2))

# google_trend%>%
#   autoplot(trend) +
#   labs(y="", title="Google Trend (stationary)")

PPI <- PPI %>%
  mutate(ppi_change = 100 * (price / lag(price) - 1))
PPI <- PPI |>
  mutate( ppi_change = difference(ppi_change))
PPI <- PPI %>%
  slice(-c(1,2))
# PPI%>%
#   autoplot(difference(ppi_change)) +
#   labs(y="", title="PPI (stationary)")
ev_ts_2017 <- ev_ts_2017 %>%
  slice(-c(1,2))
laws <- laws |>
  slice(-c(1,2))
ev_ts_2017
# ev_ts_2017 %>%
#   autoplot(sales) +
#   labs(y="", title="Electric Sales (stationary)")

ev_ts_2017 %>%
  features(sales, unitroot_kpss)
# Since the p-value > 0.05: we can say that the sales data are stationary.


#Setting training and test sets
dataset <- ev_ts_2017 |> 
  select(Month, sales)|>
  rename(BEV = sales)|>
  add_column(gas_price = gas_prices$price_change)|>
  add_column(google_trend = google_trend$trend)|>
  add_column(laws = laws$after_BBB_act)|>
  add_column(PPI = PPI$ppi_change)


train <- dataset |>
  filter(year(Month) < 2022)
test <- dataset |>
  filter(year(Month) >= 2022)

autoplot(train, BEV) +
  autolayer(test,BEV,color= "red")

# Finding the model that fits best with your training data. 

fit_sales <-  train |>
  model(
    # seasonal arima
    model1 = decomposition_model(
      STL(BEV, robust = TRUE),
      ARIMA(season_adjust)), 
    # seasonal multiple regression
    model2 = decomposition_model(
      STL(BEV, robust = TRUE),
      TSLM(season_adjust ~ trend() +
             gas_price + 
             google_trend + 
             laws +
             PPI)),
    # seasonal arima-reg
    model3 = decomposition_model(
      STL(BEV, robust = TRUE),
      ARIMA(season_adjust ~ gas_price + 
              google_trend + 
              laws +
              PPI)
  ),
  # arima + multiple reg
  model4 = ARIMA(BEV ~ gas_price + 
                   google_trend + 
                   laws +
                   PPI),
  # auto arima
  model5 = ARIMA(BEV),
  model6 = ARIMA(BEV ~ pdq(2,0,2)),
  model7 = ARIMA(BEV ~ pdq(2,0,2) + laws),
  model8 = ARIMA(BEV ~ pdq(2,0,2) + laws + google_trend + gas_price + PPI),
  model9 = ARIMA(BEV ~ pdq(2,0,2) + google_trend),
)
fit_sales %>% pivot_longer(everything(), names_to = "Model name",
                     values_to = "Specification")

fc_sales <- fit_sales %>% forecast(test)

accuracy(fc_sales, test)

fit_sales %>% select(model7) %>% gg_tsresiduals(lag=36)

augment(fit_sales) %>% features(.innov, ljung_box, lag=24, dof=5)

future_values <- new_data(dataset, 24) %>%
  mutate(laws = 1) |>
  mutate(google_trend = mean(dataset$google_trend))|>
  mutate(gas_price = mean(dataset$gas_price)) |>
  mutate(PPI = mean(dataset$PPI)) |>
  mutate(BEV = mean(dataset$BEV))

fit <- dataset |>
  model(ARIMA(BEV ~ pdq(2, 0, 2) + laws))

forecast(fit, future_values) |>
  autoplot(dataset)

