library(tidyverse) # обработка данных
library(fpp3) # куча плюшек для рядов
library(rio) # импорт данных

macro = import("macrodata.csv")

glimpse(macro)

nrow(macro)
ncol(macro)

length(macro)

head(macro, 10)
tail(macro, 10)

colnames(macro)[1] = "date"



macro$date=yearquarter(macro$date)

macro = tsibble(macro, index=date)

glimpse(macro)

cur_series = macro$pop

train = filter(macro, date < yearquarter('2000-03-31'))
test =  filter(macro, date >= yearquarter('2000-03-31'))

autoplot(macro, cpi)

autoplot(macro, c(NA, diff(pop)))

model_table = model(train,
                    snaive = SNAIVE(pop),
                    ets_aada = ETS(pop ~ error('A') + trend('Ad') + season('A')),
                    ets_aaa = ETS(pop ~ error('A') + trend('A') + season('A')),
                    ets_aam = ETS(pop ~ error('A') + trend('A') + season('M')),
                    ets_ana = ETS(pop ~ error('A') + trend('N') + season('A')))


model_table

report(select(model_table, ets_aaa))
report(select(model_table, ets_aam))
report(select(model_table, ets_ana))


fcst = forecast(model_table, h = nrow(test))
fcst


macro_vis = filter(macro, date >= yearquarter('2000-03-31'))

autoplot(fcst, macro_vis)

model_table = model(train,
                    snaive = SNAIVE(cpi),
                    ets_aaa = ETS(cpi ~ error('A') + trend('A') + season('A')),
                    ets_ana = ETS(cpi ~ error('A') + trend('N') + season('A')),
                    ets_aaa_ln = ETS(log(cpi) ~ error('A') + trend('A') + season('A')),
                    ets_ana_ln = ETS(log(cpi) ~ error('A') + trend('N') + season('A'))
)

model_table



fcst = forecast(model_table, h = nrow(test))
fcst

accuracy(fcst, test)

model_table2 = mutate(model_table,
                      top2 = (ets_aaa + ets_aaa_ln) / 2,
                      top3 = (ets_aaa + ets_aaa_ln + ets_ana) / 3)

fcst = forecast(model_table2, h = nrow(test))
fcst

accuracy(fcst, test)


macro_stretch = stretch_tsibble(macro,
                               .init = 170, .step = nrow(test))




macro_stretch


model_table = model(macro_stretch,
                    snaive = SNAIVE(cpi),
                    ets_aaa = ETS(cpi ~ error('A') + trend('A') + season('A')),
                    ets_ana = ETS(cpi ~ error('A') + trend('N') + season('A')),
                    ets_aaa_ln = ETS(log(cpi) ~ error('A') + trend('A') + season('A')),
                    ets_ana_ln = ETS(log(cpi) ~ error('A') + trend('N') + season('A'))
)

model_table

fcst = forecast(model_table, h = nrow(test))

accuracy(fcst, macro)

model_table2 = mutate(model_table,
                      top2 = (ets_aaa + ets_aaa_ln) / 2)

fcst = forecast(model_table2, h = nrow(test))

accuracy(fcst, macro)


spain = read_csv("spain.csv")

head(spain)

spain = tsibble(spain, index=time)


