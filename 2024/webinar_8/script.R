library(fpp3)
library(imputeTS)

air = as_tsibble(AirPassengers)
gg_tsdisplay(air)

lair = log(AirPassengers)
lair_na = lair
where_na = c(5:6, 30:32, 70, 90:91, 110, 124)
lair_na[where_na] = NA

ggplot_na_distribution(lair_na)
ggplot_na_distribution2(lair_na)

lair_int = na_interpolation(lair_na)
ggplot_na_imputations(lair_na, 
                      lair_int, lair)

mod = arima(lair_na, 
            order = c(1, 1, 0),
            seasonal = list(order = c(0, 1, 0)))$model

lair_ar1 = na_kalman(lair_na, 
                     model = mod)
ggplot_na_imputations(lair_na, 
                      lair_ar1, lair)

lair_seas_dec = na_seadec(lair_na)
ggplot_na_imputations(lair_na, 
                      lair_seas_dec, lair)

lair_seas_split = na_seasplit(lair_na)
ggplot_na_imputations(lair_na, 
                      lair_seas_split, lair)

lair_ar2 = na_kalman(lair_na, 
                     model = 'auto.arima')


ggplot_na_imputations(lair_na, 
                      lair_ar2, lair)


library(tidyverse) # манипуляции с данными
library(tibbletime) # таблички с рядами
library(anomalize) # выявление аномалий
library(lubridate)

AirPassengers

air = tibble(pass = AirPassengers)
air

n = nrow(air)
air2 = mutate(air, date = ymd('1949-01-01') +
                months(0:(n - 1)))
air2

qplot(data = air2, x = date, y = pass, geom = 'line')


air3 = mutate(air2, ln_pass = log(pass))
air3

qplot(data = air3, x = date, y = ln_pass, geom = 'line')

air4 = mutate(air3, ln_pass_an = ln_pass)
air4$ln_pass_an[30] = 6
air4$ln_pass_an[110] = 5.3

qplot(data = air4, x = date, y = ln_pass_an, geom = 'line')

decomp = time_decompose(air4, target = ln_pass_an)

decomp

decomp2 = anomalize(decomp, target = remainder)

glimpse(decomp2)

plot_anomalies(decomp2)

decomp3 = time_recompose(decomp2)
glimpse(decomp3)

plot_anomaly_decomposition(decomp2)

decomp4 = clean_anomalies(decomp3)

glimpse(decomp4)

qplot(data = decomp4, x = date, 
      y = observed_cleaned, geom = 'line')



library(tidyverse) # манипуляции с данными
library(tibbletime) # таблички с рядами
library(anomalize) # выявление аномалий
library(lubridate) # работа с датами
library(changepoint) # выявление структурного сдвига
library(rio)

url = 'https://github.com/akarlinsky/world_mortality/raw/main/world_mortality.csv'

world_mort = import(url)
world_mort

rm = dplyr::filter(world_mort, country_name == 'Russia')
rm
rm2 = mutate(rm, date = ymd(paste0(year, '-', time, '-01')))
rm2

rm3 = dplyr::select(rm2, deaths, date)
rm3

qplot(data = rm3, x = date, y = deaths,
      geom = 'line')

rm4 = as_tbl_time(rm3, index = date)
decomp = time_decompose(rm4, target = deaths)
decomp

rm4$deaths

one_break = cpt.mean(rm4$deaths, 
                     method = 'AMOC')
one_break

decomp[69, ]

plot(one_break)

all_breaks = cpt.mean(rm4$deaths,
                      method = 'BinSeg',
                      Q = 3)

cpt.
all_breaks

plot(all_breaks)



glimpse(decomp)

one_break_rem = cpt.mean(decomp$remainder, 
                         method = 'AMOC')
one_break_rem

decomp[69, ]

plot(one_break_rem)



library(fpp3) # работа с рядами
library(bsts) # байесовская структурная модель

plot(AirPassengers)
str(AirPassengers)

air = log(AirPassengers)
plot(air)

model = list()
model = AddLocalLinearTrend(model, y = air)
model = AddTrig(model, y = air,
                period = 12, frequencies = 1:2)

poster = bsts(air, 
              state.specification = model, niter = 2000)

poster

plot(poster, 'components')

fcst = predict(poster, horizon = 24, 
               quantiles = c(0.05, 0.95), burn = 1000)
fcst$mean
plot(fcst)


library(fpp3)
library(bsts)
library(CausalImpact)

library(rio)

url = 'https://github.com/akarlinsky/world_mortality/raw/main/world_mortality.csv'

world_mort = import(url)
world_mort

rm = dplyr::filter(world_mort, 
                   country_name == 'Russia')
glimpse(rm)
rm2 = mutate(rm, 
             date = ymd(paste0(year, '-', time, '-01')))
glimpse(rm2)

rm3 = dplyr::select(rm2, deaths, date)
rm3

qplot(data = rm3, x = date, y = deaths, 
      geom = 'line')

start = 61
impact = CausalImpact(
  data = rm3$deaths, 
  pre.period = c(1, start - 1),
  post.period = c(start, nrow(rm3)))


impact

plot(impact)
summary(impact, 'report')
