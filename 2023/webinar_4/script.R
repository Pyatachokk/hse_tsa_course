library(tidyverse) # обработка данных
library(fpp3) # куча плюшек для рядов
library(rio) # импорт данных

n_sim = 300

set.seed(777)

# Белый шум
data = tibble(y = arima.sim(n = n_sim, model = list()))


data$date = yearmonth(ymd('2000-12-01') + months(1:n_sim))

data = as_tsibble(data, index = date)

gg_tsdisplay(data, y)


ACF(data, y, lag_max=90) %>% autoplot()

PACF(data, y, lag_max=90) %>% autoplot()

Box.test(data$y, lag = 3, type = "Ljung-Box", fitdf = 0)
Box.test(data$y, lag = 10, type = "Ljung-Box", fitdf = 0)
Box.test(data$y, lag = 15, type = "Ljung-Box", fitdf = 0)

autoplot(data)
data_train = filter(data, date < yearmonth('2020-01-01'))

mods = model(data_train,
             naive = MEAN(y),
             ma12 = ARIMA(y ~ 1 + pdq(0, 0, 12) + PDQ(0, 0, 0))
            )

report(mods$naive[[1]])
report(mods$ma12[[1]])


fcst = forecast(mods, h = 60)
accuracy(fcst, data)

autoplot(fcst, data)



# MA(2)
data = tibble(y = arima.sim(n = n_sim, model = list(ma = c(0.2279, 0.2488,0.2279, 0.2488))))
data$date = yearmonth(ymd('2000-12-01') + months(1:n_sim))
data = as_tsibble(data, index = date)

autoplot(data)

# MA(q): ACF обрывается на лаге q, PACF экспоненциально убывает.

ACF(data, y, lag_max) %>% autoplot()

PACF(data, y, lag_max) %>% autoplot()




macro = import("macrodata.csv")

colnames(macro)[1] = "date"

macro$date=yearquarter(macro$date)

macro = tsibble(macro, index=date)
autoplot(macro, realinv)

macro_train = filter(macro, date < yearquarter('1990-09-01'))


macro_train_diff = macro_train %>% mutate(realinv=c(NA, diff(realinv)))

macro_train_diff = na.omit(macro_train_diff)

autoplot(macro_train_diff, realinv)

ACF(macro_train_diff, realinv, lag_max=30) %>% autoplot()
PACF(macro_train_diff, realinv, lag_max=30) %>% autoplot()

mods = model(macro_train_diff,
             mean = MEAN(realinv),
             theta = THETA(realinv),
             ma12 = ARIMA(realinv ~ 1 + pdq(0, 0, 3) + PDQ(0, 0, 0))
)
          
mods

report(mods$ma12[[1]])
report(mods$mean[[1]])

fcst = forecast(mods$mean[[1]],h = nrow(macro)-nrow(macro_train))
fcst$.mean = macro_train$realinv[nrow(macro_train)] + (cumsum(fcst$.mean))


ggplot() + geom_line(mapping = aes(date, .mean), data=fcst)+
  geom_line(mapping=aes(date, realinv), data=macro)
  


fcst = forecast(mods, h = nrow(macro)-nrow(macro_train))
accuracy(fcst, macro)


mods = model(macro_train,
             snaive = SNAIVE(pop),
             mean = SNAIVE(pop),
             theta = THETA(pop),
             ma12 = ARIMA(pop ~ 1 + pdq(0, 1, 12) + PDQ(0, 0, 0)),
             stl_ma = decomposition_model(STL(pop ~ season(window = Inf)),
                                          ARIMA(season_adjust ~ 1 + pdq(0, 1, 12) + PDQ(0, 0, 0)),
                                          SNAIVE(season_year)))

mods

report(mods$ma12[[1]])
report(mods$stl_ma[[1]])


fcst = forecast(mods, h = nrow(macro)-nrow(macro_train))
accuracy(fcst, macro)
