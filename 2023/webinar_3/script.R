library(tidyverse)
library(fpp3)
library(rio)

library(lubridate)
library(zoo)

data = AirPassengers

data = as_tsibble(AirPassengers)
data = mutate(data, index = yearmonth(index))

autoplot(data)

train = filter(data, index < yearmonth('1957-09-01'))
test = filter(data, index >= yearmonth('1957-09-01'))

models = model(train,
      # snaive = SNAIVE(value),
      zzz = ETS(value),
      # aam = ETS(value~error('A') + trend('A') + season('M')),
      amm = ETS(value~error('A') + trend('M') + season('M'))
      
)

accuracy(models)

report(models$zzz[[1]])

forec = forecast(models, h=nrow(test))

accuracy(forec, test)

autoplot(forec, tail(data, 50))


models = model(train,
               # snaive = SNAIVE(value),
               zzz = ETS(value),
               theta = THETA(value),
               comp = decomposition_model(
                 STL(value ~ season(window=31)),
                 ETS(season_adjust ~ error('M') + trend('Ad') + season('N')),
                 SNAIVE(season_year)
               )
)

models

accuracy(models)

forec = forecast(models, h=nrow(test))

accuracy(forec, test)


online = import("hour_online.csv")

data = mutate(online, Time=as_datetime(Time, format="%m/%d/%y %H:%M"))

data = as_tsibble(data, index=Time) %>%  fill_gaps() %>%  mutate(Users = na.locf(Users, fromLast=TRUE))
# data = as_tsibble(data, index=Time) %>% fill_gaps(37613)

data = mutate(data, Users = Users + 10000)

# data %>% filter(Time >= as_datetime('2017-03-12 00:00:00')) %>% filter(Time <= as_datetime('2017-03-13 00:00:00'))
# nrow(data)
# data[is.na(data$Users), ]
# autoplot(data %>% filter(Time > as_datetime('2017-03-05')) %>% filter(Time < as_datetime('2017-03-17')))

autoplot(tail(data, 600))
gg_season(data, Users, period = 24)

gg_subseries(data, Users, period=24)


train = filter(data, Time<as_datetime('2017-04-01'))
test = filter(data, Time>=as_datetime('2017-04-01'))

# train[is.na(train$Users),]

train_stretch = stretch_tsibble(train,
                                .init=500,
                                .step=200
                                )

mods = model(train_stretch,
             snaive = SNAIVE(Users),
             ana = ETS(Users ~ error('A') + trend('N') + season('A')),
             # anm = ETS(Users ~ error('A') + trend('N') + season('M')),
             mna = ETS(Users ~ error('M') + trend('N') + season('A')),
             # mnm = ETS(Users ~ error('M') + trend('N') + season('M'))
             theta = THETA(Users)
             )

acc = accuracy(mods)
acc %>% group_by(.model) %>% 
  summarize(mean_MAPE=mean(MAPE))

mods = model(train,
        snaive = SNAIVE(Users),
        # ana = ETS(Users ~ error('A') + trend('N') + season('A')),
        # anm = ETS(Users ~ error('A') + trend('N') + season('M')),
        mna = ETS(Users ~ error('M') + trend('N') + season('A')),
        # mnm = ETS(Users ~ error('M') + trend('N') + season('M'))
        theta = THETA(Users)
  )


accuracy(mods)

forec = forecast(mods, h=nrow(test))

accuracy(forec, test)


autoplot(forec, tail(data, 500))




