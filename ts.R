library(tidyverse)
library(fable)
library(tsibble)
library(feasts)
library(lubridate)

df <- read.csv2("https://github.com/KobzevAY/Bank_and_Fin_Stats/raw/main/ipoteka_smr.csv",
                colClasses = c('character', 'numeric'))
ipoteka <- readxl::read_xlsx('E:/СГЭУ/ipoteka_smr.xlsx', col_types = c('date', 'numeric'))
ipoteka_ts <- as_tsibble(ipoteka, index = as.Date(as.character(date), origin  = '%Y-%m-%d'))

ipoteka_ts <- tsibble(df, m = yearmonth(df$п.їdate), index = m)

ipoteka_ts %>%
  gg_season(ipoteka, labels = "both") +
  labs(y = "$ (millions)",
       title = "Seasonal plot: Antidiabetic drug sales")


ipoteka_ts %>%
  gg_subseries(ipoteka) +
  labs(
    y = "$ (millions)",
    title = "Australian antidiabetic drug sales"
  )

ipoteka_ts %>%
  model(
    classical_decomposition(ipoteka, type = "additive")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical additive decomposition of total
                  US retail employment")


x11_dcmp <- ipoteka_ts %>%
  model(x11 = X_13ARIMA_SEATS(ipoteka ~ x11())) %>%
  components()
autoplot(x11_dcmp) +
  labs(title =
         "Decomposition of total US retail employment using X-11.")


x11_dcmp %>%
  ggplot(aes(x = m)) +
  geom_line(aes(y = ipoteka, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")


fit <- ipoteka_ts %>%
  model(trend_model = TSLM(ipoteka ~ trend()))
fit %>% forecast(h = "3 years")

fit %>%
  forecast(h = "3 years") %>%
  autoplot(ipoteka_ts) +
  labs(y = "$US", title = "GDP per capita for Sweden")


fit2 <- ipoteka_ts %>%  
  model(SNAIVE(ipoteka ~ lag("year"))) %>% forecast(h = "3 years")
fit2 %>% autoplot(ipoteka_ts)


fit3 <- ipoteka_ts %>%  
  model(ARIMA(ipoteka)) %>% forecast(h = "3 years")
fit3 %>% autoplot(ipoteka_ts)
