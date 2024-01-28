library(fpp3) # куча плюшек для рядов
library(tidyverse) # обработка данных
library(rio)

options(scipen=999)

macro = import("macrodata.csv")

nrow(macro)
ncol(macro)

length(macro)

head(macro, 10)
tail(macro, 10)

colnames(macro)[1] = "date"



macro$date=yearquarter(macro$date)

# macro$new_index = 0:nrow(macro)

macro = tsibble(macro, index=date)

# trends
autoplot(macro, infl)

gg_season(macro, infl)

gg_subseries(macro, infl)

gg_tsdisplay(macro, infl)

gg_tsdisplay(macro, infl, plot_type="partial")

gg_tsdisplay(macro, infl, plot_type="season")

gg_tsdisplay(macro, infl, plot_type="histogram")

gg_tsdisplay(macro, infl, plot_type="scatter")

stl_model = model(macro,
                  decomp = STL(infl ~ trend(window = 15) +
                                 season(period=4, window = 25)))
components(stl_model)

components(stl_model) %>% autoplot()

feat_stl(macro$infl, 4)

train_infl = macro %>% 
  filter_index("1959 Q1" ~ "1995 Q4") %>% select(infl)

h = (nrow(macro)-nrow(train_infl))

mean_model = train_infl %>% model(MEAN(infl))
mean_forecast = forecast(mean_model, h=h)

autoplot(mean_forecast)

naive_model = train_infl %>% model(NAIVE(infl))
naive_forecast = forecast(naive_model, h=h)

rw_model = train_infl %>% model(RW(infl))
rw_forecast = forecast(rw_model, h=h)

snaive_model = train_infl %>% model(SNAIVE(infl))
snaive_forecast = forecast(snaive_model, h=h)

ggplot() +
        geom_line(data=tail(macro, h + 50), mapping=aes(date, infl)) +
        geom_line(mean_forecast, mapping=aes(date, .mean), color="blue") +
        geom_line(naive_forecast, mapping=aes(date, .mean), color="red") +
        geom_line(rw_forecast, mapping=aes(date, .mean), color="green") +
        geom_line(snaive_forecast, mapping=aes(date, .mean), color="orange")
        


# cycles
autoplot(macro, unemp)

gg_season(macro, unemp)

gg_subseries(macro, unemp)

gg_tsdisplay(macro, unemp)

stl_model = model(macro,
                  decomp = STL(unemp ~ trend(window = 50) +
                                 season(window = 35)))
components(stl_model)

components(stl_model) %>% autoplot()

feat_stl(macro$unemp, 4)


# seasonality
autoplot(macro, pop)
gg_season(macro, pop)
gg_subseries(macro, pop)

autoplot(macro, c(NA, diff(pop)))
gg_season(macro, c(NA, diff(pop)))
gg_subseries(macro, c(NA, diff(pop)))

gg_tsdisplay(macro, c(NA, diff(pop)))

stl_model = model(macro,
                  decomp = STL(pop ~ trend(window = 100) +
                                 season(window = 25)))
components(stl_model)

components(stl_model) %>% autoplot()

feat_stl(macro$pop, 4)


spain = read_csv("spain.csv")

head(spain)


spain = tsibble(spain, index=time)

autoplot(spain, target)

nrow(spain)


gg_season(spain, target, 7)

gg_tsdisplay(spain, target, plot_type="partial", lag_max=400)

ggacf(spain$target)