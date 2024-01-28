library(tidyverse) # обработка данных
library(fpp3) # куча плюшек для рядов
library(rio) # импорт данных
library(imputeTS)
library(ggplot2)

n_sim = 1000

set.seed(777)

# AR(4)
data = tibble(y = arima.sim(n = n_sim, n.start=1000, model = list(ar = c(0.25, 0.02, 0.03, 0.04))))
data$date = yearmonth(ymd('2000-12-01') + months(1:n_sim))
data = as_tsibble(data, index = date)

autoplot(data)

# MA(q): ACF обрывается на лаге q, PACF экспоненциально убывает.

ACF(data, y, lag_max=90) %>% autoplot()

PACF(data, y, lag_max=90) %>% autoplot()

ARIMA()
# ARMA(4)
data = tibble(y = arima.sim(n = n_sim, n.start=1000, model = list(
    ar = c(0.25, 0.1, 0.2, 0.1), 
    ma = c(0.25, 0.1, 0.2, 0.1))
    )
)
data$date = yearmonth(ymd('2000-12-01') + months(1:n_sim))
data = as_tsibble(data, index = date)

autoplot(data)

# MA(q): ACF обрывается на лаге q, PACF экспоненциально убывает.

ACF(data, y, lag_max=90) %>% autoplot()

PACF(data, y, lag_max=90) %>% autoplot()


data = import("series.csv")


glimpse(data)
data$date = dmy(data$date)
data = as_tsibble(data, index = date)

data %>% autoplot(target)

# na_interpolation(data) %>% autoplot(target)

data = data %>% mutate(target=target) %>% select(target)%>% na.omit()

data %>% autoplot(target)

data = data %>% fill_gaps() 

data %>% autoplot(target)

data = na_interpolation(data)

data %>% filter(date > dmy("01/01/2015")) %>% autoplot(target)

ACF(data, target, lag_max=90) %>% autoplot()
PACF(data, target, lag_max=90) %>% autoplot()

data_diff = data %>% mutate(target=c(NA,diff(target))) %>% na.omit()

autoplot(data_diff, target)

ACF(data_diff, target, lag_max=90) %>% autoplot()
PACF(data_diff, target, lag_max=90) %>% autoplot()

# ARIMA(non_seasonal = (p, d, q), seasonal(P, D, Q))

our_model =data %>% model(
  auto = ARIMA(target ~ pdq(p=0:5, d = 1, q = 0:5))
) 

report(our_model$auto[[1]])

tail(data, 250)

data_train =  filter(data, date < dmy("20/03/2016"))

data_stretch <- data_train %>% 
  stretch_tsibble(.init = 150, .step = 20)

fc <- data_stretch |>
  model(
    auto = ARIMA(target ~ pdq(p=0, d = 1, q = 0)),
    auto2 = ARIMA(target ~ pdq(p=0, d = 2, q = 0))
    ) |>
  forecast(h = 5) |>
  group_by(.id, .model)|>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "target", distribution = target)

metrics = fc |>
  accuracy(data, by = c("h", ".model"))

metrics

metrics |>
  group_by(.model) |>
  ggplot(aes(x = h, y = MAPE))+
  facet_wrap(~.model, nrow = 1)  +
  geom_point() +
  theme_bw()


arima_grid <-  expand.grid(p = seq(1, 5), 
                        d = 1,
                        q = seq(1, 5)
                        )

arima_grid

arima_grid = arima_grid %>% mutate(param_id = row_number())

arima_grid

results = data.frame()

data_stretch

f <- function(x) {
  
  p = x[['p']]
  d = x[['d']]
  q = x[['q']]
  param_id = x[['param_id']]

  
  fc <- data_stretch |>
    model(
      auto <-  ARIMA(target ~ pdq(p=p, d = d, q = q)),
    ) |>
    forecast(h = 8) |>
    group_by(.id, .model) |>
    mutate(h = row_number()) |>
    ungroup() |>
    mutate(param_id = param_id) |>
    as_fable(response = "target", distribution = target)
  
  metrics = fc |>
    accuracy(data, by = c("h", ".model"))
  
  metrics = mutate(metrics, param_id = param_id)

  assign("results", rbind(results, metrics), envir = .GlobalEnv)
}

# Работает долго
a = apply(arima_grid,1,f)

results |>
  group_by(param_id) |>
  ggplot()+
  # facet_wrap(~param_id, nrow = 1)  +
  geom_line(aes(x = h, y = MAPE, group=param_id, colour=param_id)) +
  theme_bw()


