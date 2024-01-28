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

autoplot(macro, pop)

autoplot(macro, c(NA, diff(pop)))
autoplot(macro, c(NA, NA, diff(diff(pop))))

model_table = model(train,
                    ets_aada = ETS(pop ~ error('A') + trend('Ad') + season('A')),
                    
                    ets_aaa = ETS(pop ~ error('A') + trend('A') + season('A')),
                    ets_ama = ETS(pop ~ error('A') + trend('M') + season('A')),
                    
                    
                    )


model_table


report(select(model_table, ets_aaa))
report(select(model_table, ets_ama))
report(select(model_table, ets_aada))


fcst = forecast(model_table, h = nrow(test))
fcst


macro_vis = filter(macro, date >= yearquarter('2000-03-31'))

autoplot(fcst, macro_vis)



model_table = model(train,
                    ets_aada = ETS(pop ~ error('A') + trend('Ad') + season('A')),
                    # ets_aada_ln = ETS(log(pop) ~ error('A') + trend('Ad') + season('A')),

                    
                    ets_aaa_ln = ETS(log(pop) ~ error('A') + trend('A') + season('A')),
                    # ets_ama = ETS(pop ~ error('A') + trend('M') + season('A')),
)

model_table



fcst = forecast(model_table, h = nrow(test))
fcst

accuracy(fcst, test)

autoplot(fcst, macro_vis)

model_table2 = mutate(model_table,
                      top2 = (ets_aada + ets_aaa_ln) / 2)

fcst = forecast(model_table2, h = nrow(test))
fcst

accuracy(fcst, test)

autoplot(fcst, macro_vis)



macro_stretch = stretch_tsibble(macro,
                               .init = 170, .step = nrow(test))


