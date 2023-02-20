#Call libraries

library(tidyverse)
library(lmtest)
library(car)
library(strucchange)
library(forecast)
library(ggfortify)
library(tseries)
library(dynlm)
library(readxl)
library(urca)
library(patchwork)

#Create Data Frame

data <- tibble(read_excel("Peru Statistics.xlsx", 
               sheet = "data"))
head(data)

#Graph to see relations

#Create Data Graph

gdata <- data %>%
  mutate(GCPI = CPI / lag(CPI) - 1) %>%
  select(year, GCPI, ER, MB) %>%
  filter(year >= 1985, year <= 1995) %>%
  na.omit()

glabel <- function(x) { paste(x*100, "%") }

#Create Graph

GCPI <- ggplot(gdata, aes(x = year, y = GCPI)) +
  geom_line(colour = "#B54923", lwd = 0.8) + theme_light() +
  labs(title = "Peruan Inflation over time", 
       subtitle = "1985-1995", caption = "Source: BCRP", x = "", y = "") + 
  scale_y_continuous(label = glabel) + 
  scale_x_continuous(n.breaks = 15) + 
  theme(plot.title = element_text(family = "sans", face = "bold", 
                                  vjust = 0.5, hjust = 0.5), 
        plot.subtitle = element_text(vjust = 0.5, hjust = 0.5))

GER <- ggplot(gdata, aes(x = year, y = ER)) +
  geom_line(colour = "#2A5783", lwd = 0.8) + theme_light() +
  labs(title = "Peru's Exchange Rate", 
       subtitle = "1985-1995", caption = "Source: BCRP", x = "", y = "") + 
  scale_x_continuous(n.breaks = 15) + 
  theme(plot.title = element_text(family = "sans", face = "bold", 
                                  vjust = 0.5, hjust = 0.5), 
        plot.subtitle = element_text(vjust = 0.5, hjust = 0.5), 
        axis.text.x = element_text(angle = 45)) 

GMB <- ggplot(gdata, aes(x = year, y = MB)) +
  geom_line(colour = "#24693D", lwd = 0.8) + theme_light() +
  labs(title = "Peru's Monetary Base", 
       subtitle = "1985-1995", caption = "Source: BCRP", x = "", y = "") + 
  scale_x_continuous(n.breaks = 15) + 
  theme(plot.title = element_text(family = "sans", face = "bold", 
                                  vjust = 0.5, hjust = 0.5), 
        plot.subtitle = element_text(vjust = 0.5, hjust = 0.5), 
        axis.text.x = element_text(angle = 45))

plot <- GCPI / (GMB + GER)
plot

#Create Second Graph 

gdata1 <- cbind(CPI = diff(log(ts(data$CPI, start = 1961, frequency = 1))), 
               MB = diff(log(ts(data$MB, start = 1961, frequency = 1))))
gdata2 <- cbind(CPI = diff(log(ts(data$CPI, start = 1961, frequency = 1))), 
                ER = diff(log(ts(data$ER, start = 1961, frequency = 1))))

GG1 <- autoplot(gdata1,
         size = 0.7) + 
  theme_light() + 
  labs(title = "Relación Inflación e Incremento de M2 en el Peru", 
       subtitle = "Medido en primera diferencia de los logaritmos", 
       caption = "1961-2000", x = "", y = "") + 
  scale_color_manual(values = c("#D15821", "#5E8FB9"), 
                     labels = c("Inflación", "Base Monetaria")) + 
  theme(plot.title = element_text(family = "sans", face = "bold", 
                                  vjust = 0.5, hjust = 0.5), 
        plot.subtitle = element_text(vjust = 0.5, hjust = 0.5))

GG2 <- autoplot(gdata2,
         size = 0.7) + 
  theme_light() + 
  labs(title = "Relación Tipo de Cambio e Inflación en el Peru", 
       subtitle = "Medido en primera diferencia de los logaritmos", 
       caption = "1961-2000", x = "", y = "") + 
  scale_color_manual(values = c("#D15821", "#1C6CAA"), 
                     labels = c("Inflación", "Tipo de Cambio")) + 
  theme(plot.title = element_text(family = "sans", face = "bold", 
                                  vjust = 0.5, hjust = 0.5), 
        plot.subtitle = element_text(vjust = 0.5, hjust = 0.5))

plot2 <- GG1 / GG2
plot2

#Create differences and measuring stationary

data_cpi <- diff(log(ts(data$CPI, start = 1960, frequency = 1)), lag = 1, 
                 differences = 2)
data_mb <- diff(log(ts(data$MB, start = 1960, frequency = 1)), lag = 1, 
                differences = 2)
data_er <- diff(log(ts(data$ER, start = 1960, frequency = 1)), lag = 1, 
                differences = 2)


adf.test(data_cpi, k = 0)
adf.test(data_mb, k = 0)
adf.test(data_er, k = 0)

vardata <- cbind(MB = data_mb, CPI = data_cpi, ER = data_er)

#Create model

vars::VARselect(vardata, lag.max = 6)
model <- vars::VAR(vardata, p = 6)
summary(model$varresult$CPI)
summary(model$varresult$ER)

#Evaluate model CPI

jarque.bera.test(residuals(model$varresult$CPI)) #Normality
bgtest(model$varresult$CPI) #Autocorrelation
sctest(model$varresult$CPI, type = "Chow") #Breakpoint
bptest(model$varresult$CPI) #Heteroskedasticity
grangertest(data_cpi ~ data_mb, order = 6) #Granger Causality test

#Evaluate model ER

jarque.bera.test(residuals(model$varresult$ER)) #Normality
bgtest(model$varresult$ER) #Autocorrelation
bptest(model$varresult$ER) #Heteroskedasticity
grangertest(data_er ~ data_cpi, order = 6) #Granger Causality test

#Evaluate Cointegration

co_cpi <- ca.jo(vardata, K = 6)
summary(co_cpi)

#IRF Function

irf <- vars::irf(model, response = "CPI")
irf
plot(irf)

#Graph CPI

GCP <- autoplot(cbind(data_cpi, 
               ts(fitted(model$varresult$CPI), start = 1968, frequency = 1)),
               size = 0.7) + 
           theme_light() + 
  labs(title = "Hiperinflación en el Perú", 
       subtitle = "Modelo VAR(6) con diferencias en diferencias", 
       caption = "1964-2000", x = "", y = "") + 
  scale_color_manual(values = c("Red", "Blue"), 
                     labels = c("Estimado", "Real")) + 
  theme(plot.title = element_text(family = "sans", face = "bold", 
                                  vjust = 0.5, hjust = 0.5), 
        plot.subtitle = element_text(vjust = 0.5, hjust = 0.5))

#Graph ER

GRE <- autoplot(cbind(data_er, 
               ts(fitted(model$varresult$ER), start = 1968, frequency = 1)),
         size = 0.7) + 
  theme_light() + 
  labs(title = "Volatilidad del Tipo de Cambio Peruano", 
       subtitle = "Modelo VAR(6) con diferencias en diferencias", 
       caption = "1964-2000", x = "", y = "") + 
  scale_color_manual(values = c("Red", "Blue"), 
                     labels = c("Estimado", "Real")) + 
  theme(plot.title = element_text(family = "sans", face = "bold", 
                                  vjust = 0.5, hjust = 0.5), 
        plot.subtitle = element_text(vjust = 0.5, hjust = 0.5))

plot3 <- GCP / GRE
print(plot3)

