#Call libraries

library(tidyverse)
library(forecast)
library(tseries)
library(readxl)

#Create database

data_d <- tibble(read_excel("Zadquiel/Folder/Datos/ER_Diary.xlsx"))

data_d$DATE <- as.Date(data_d$DATE) #Convert Date chr into date type

#Graphing time series

ggplot(data_d, aes(x = DATE, y = RATE)) + 
  geom_line(col = '#325F8C', size = 1) + theme_light() + 
  labs(title = 'Exchange Rate', subtitle = '2019-2022', 
       caption = 'Source: BCV', x = 'Year', y = '') + 
  theme(plot.title = element_text(size = 15, face = 'bold', hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))

#First log differences

data <- diff(log(data_d$RATE))

adf.test(data) #Dickey Fuller test

pacf(data) #Selfcorrelogram

#Model

model <- auto.arima(data)
summary(model)

#Predict and Graph

fore <- forecast(model, h = 16)
plot(fore, 
     main = 'Forecast from ARIMA(2,1,2) for Venezuelan Exchange Rate', 
     xaxt = 'n', xlim = c(600, 750)) 
axis(1, at = c(600, 650, 700, 800), labels = seq(2020, 2023))
title(sub = expression(Y[t] == 0.1506*Y[t-1]-0.0524*Y[t-2]-0.677*epsilon[t-1]-0.1286*epsilon[t-2]))

nfore <- as.tibble(fore) #Convert forecast data into tibble

#Now, we will calculate the exchange rate forecast at feb-2023

for (i in 1:nrow(nfore)) {
  if (i == 1) {
    one <- data_d$RATE[745] * (1 + nfore$'Point Forecast'[i])
  }
  else 
    one <- one * (1 + nfore$'Point Forecast'[i])
  print(one)
}

