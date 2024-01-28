# Esli russkie bukvi prevratilitis v krakozyabry, to...
# File - Reopen with encoding... - UTF-8 - Set as default - OK

# Временные ряды. Неделя 6. Скрипт 1.
# KPSS и ADF тесты

library(tidyverse) # обработка данных
library(fpp3) # куча плюшек для рядов
library(rio) # импорт данных
library(urca) # тесты

bet0 <- 0.1
bet1 <- 0.2
mu <- 0.3
y_tilde <- arima.sim(model = list(order = c(0,0,0)),
                     n = 200, mean = bet0)
y1 <- y_tilde + bet1*(1:200)
y2 <- arima.sim(model= list(order = c(0,1,0)),
                n = 200, mean = mu)

# trend stationary
plot(y1)

# difference stationary
plot(y2)


m = import('macrodata.csv')
colnames(m)[1] = "date"
glimpse(m)

m2 = mutate(m, date = yearquarter(date))
glimpse(m2)

macro = as_tsibble(m2, index = date)
macro


gg_tsdisplay(macro, pop)

res_kpss = ur.kpss(macro$pop, type = 'mu')
summary(res_kpss)
# d >= 1
# H0: ts = mu + stat
# Ha: ts = mu + stat + rw

# H0 is rejected

gg_tsdisplay(macro, c(NA, diff(pop)))

res_kpss = ur.kpss(diff(macro$pop), type = 'mu')
summary(res_kpss)
# d >=2
# H0: ts = mu + stat
# Ha: ts = mu + stat + rw

# H0 is not rejected

gg_tsdisplay(macro, c(NA, NA, diff(diff(pop))))

res_kpss = ur.kpss(diff(macro$pop, differences = 2), type = 'mu')
summary(res_kpss)

# d = 2
# H0: ts = mu + stat
# Ha: ts = mu + stat + rw

# H0 is not rejected

res_adf = ur.df(macro$realgdp, type = 'trend',
                selectlags = 'AIC')
summary(res_adf)

# H0: ts = ARIMA(p, 1, q) + trend
# Ha: ts = ARIMA(p, 0, q) + const

# H0 is rejected

macro_train = filter(macro, date < yearquarter('1990-09-01'))


macro_train_diff = macro_train %>% mutate(pop=c(NA, NA, diff(pop, differences=2)))

macro_train_diff = na.omit(macro_train_diff)

autoplot(macro_train_diff, pop)



ACF(macro_train_diff, pop, lag_max=40) %>% autoplot()
PACF(macro_train_diff, pop, lag_max=40) %>% autoplot()
# P=1, p <= 2, q <= 2, Q = 0

autoplot(macro_train, c(rep(NA, 4), diff(pop, lag=4)))
autoplot(macro_train, c(rep(NA, 5), diff(diff(pop, lag=4))))

# D=1, d=1, q <= 3, p <= 1 

ACF(macro_train, c(rep(NA, 5), diff(diff(pop, lag=4))), lag_max=30) %>% autoplot()
PACF(macro_train, c(rep(NA, 5), diff(diff(pop, lag=4))), lag_max=30) %>% autoplot()

models = model(macro_train,
               naive = SNAIVE(pop),
               theta = THETA(pop),
               auto = ARIMA(pop),
               sarima111_x11 = ARIMA(pop ~ 0 + pdq(0:1, 1, 0:3) +
                                       PDQ(0, 1, 0)))

report(models$auto[[1]])

report(models$sarima111_x11[[1]])

nrow(macro) - nrow(macro_train)
fcst = forecast(models, h = '5 years')
accuracy(fcst, macro)


nrow(macro)

macro_slide = slide_tsibble(macro,
                           .size = 60, .step = 4)



models_slide = model(macro_slide,
                     naive = SNAIVE(pop),
                     theta = THETA(pop),
                     auto = ARIMA(pop),
                     sarima = ARIMA(pop ~ 0 + pdq(0:1, 1, 0:3) +
                                             PDQ(0, 1, 0))
                     )


mod_aggr = mutate(models_slide,
                  average2 = (auto + sarima) / 2,
                  average4 = (auto + naive + theta + sarima) / 4,
                  auto_theta = (auto + theta) / 2,
                  naive_theta = (naive + theta) / 2)
                  


fcst_slide = forecast(mod_aggr, h = 4)

accuracy(fcst_slide, macro)
