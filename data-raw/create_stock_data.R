
library(tidyquant)
library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)

options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# Downloading Apple price using quantmod


symbols <- tq_index("SP500")



prices <- tq_get(symbols$symbol,
                 from = "2015-01-01",
                 to = "2020-03-01",
                 get = "stock.prices")


prices <- 
    prices %>% 
        filter(date > as.Date('2017-01-01'))

prices2 <- 
    prices %>% 
        group_by(symbol) %>% 
        mutate(growth = close / lag(close) - 1)

ggplot(prices2, aes(date, growth, colour = symbol)) +
    geom_line()

ggplot(prices2, aes(date, growth, colour = symbol)) +
    geom_line() +
    facet_wrap(~ symbol)





lags <- seq(50)
lag_names <- paste("lag", formatC(lags, width = nchar(max(lags)), flag = "0"), 
                   sep = "_")
lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)


prices3 <-
    prices2 %>% 
        group_by(symbol) %>% 
        mutate_at(vars(growth), funs_(lag_functions))

prices4 <- 
    prices3 %>% 
        ungroup %>% 
        filter(grepl('10$', as.character(date))) %>% 
        mutate(ID = as.character(seq(nrow(.)))) %>% 
        na.omit %>% 
        pivot_longer(contains('lag')) %>% 
        mutate(lag = as.numeric(gsub('lag_', '', name))) %>% 
        rename(lag_char = name,
               lagged_growth = value) %>% 
        select(ID, growth, lagged_growth, lag, lag_char, symbol, date)





ggplot(prices4, aes(lagged_growth, growth, colour = as.numeric(ID))) + 
    geom_point() +
    scale_colour_viridis_c()





thresholds_sm <- quantile(prices4$lagged_growth, seq(0.02, 0.98, length.out = 25))

prices4 %>% 
    group_by(ID) %>% 
    summarise(prop = sapply(thresholds_sm, 
                            \(x) weighted.mean(lagged_growth > x, w = 1 / lag)),
              threshold = round(thresholds_sm, 3),
              growth = mean(growth)) %>% 
    ggplot(aes(x = prop, y = growth)) + 
        geom_point(alpha = 0.4) +
        geom_smooth(method = 'lm', colour = 'red') +
        facet_wrap(~ threshold, scale = 'free')




thresholds_sm <- quantile(prices4$lagged_growth, seq(0.95, 0.05, length.out = 100))


thresh_models_summary <- 
    prices4 %>% 
    group_by(ID) %>% 
    summarise(prop = sapply(thresholds_sm, 
                            \(x) weighted.mean(lagged_growth > x, w = 1 / lag)),
              threshold = round(thresholds_sm, 3),
              growth = mean(growth)) %>% 
    group_by(threshold) %>% 
    do(fitThresh = tidy(lm(growth ~ prop, data = .), conf.int = TRUE)) %>% 
    unnest(fitThresh)



thresh_models_summary %>% 
    filter(term == 'prop') %>% 
    ggplot(aes(x = threshold, y = estimate)) + 
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = 'steelblue') +
        geom_line()


usethis::use_data(prices4, overwrite = TRUE)


