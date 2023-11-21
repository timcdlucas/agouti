## code to prepare `stock_vector` dataset

library(tidyquant)
library(dplyr)
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

prices <- tq_get(symbols$symbol[400:450],
                 from = "2002-01-01",
                 to = "2020-03-01",
                 get = "stock.prices")

ggplot(prices, aes(x = date, y = open, colour = symbol, group = symbol)) +
  geom_line()


ggplot(prices %>% dplyr::filter(symbol == 'AMCR'), aes(x = date, y = open, colour = symbol, group = symbol)) +
  geom_line()

just_growth<-
  prices %>%
  filter(symbol == 'VTRS') %>%
  mutate(growth = close / lag(close) - 1) %>%
  select(growth)

just_growth <- na.omit(just_growth) %>% pull(growth)

stock_vector <- just_growth
usethis::use_data(stock_vector)
