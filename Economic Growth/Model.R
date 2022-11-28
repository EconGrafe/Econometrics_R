#Librerias

library(tidyverse)
library(lmtest)
library(car)
library(orcutt)
library(pacman)
library(tseries)
library(scales)
library(patchwork)

#Datos

data <- tibble(pwt100) %>%
  filter(countrycode == "VEN") %>%
  filter(year >= 1959)

data_GDP <- tibble(GDP) %>%
  select(Consolidado, Petroleo, Comercio, Manufactura, 
         Comunicaciones, Inmobiliarios, Gobierno, 
         Otros)

D98 <- data_GDP %>%
  filter(Consolidado == 1998) %>%
  gather("Petroleo", "Comercio", "Manufactura", 
         "Comunicaciones", "Inmobiliarios", "Gobierno", 
         "Otros", key = "Sector", value = "Aporte")
D05 <- data_GDP %>%
  filter(Consolidado == 2005) %>%
  gather("Petroleo", "Comercio", "Manufactura", 
         "Comunicaciones", "Inmobiliarios", "Gobierno", 
         "Otros", key = "Sector", value = "Aporte")
D12 <- data_GDP %>%
  filter(Consolidado == 2012) %>%
  gather("Petroleo", "Comercio", "Manufactura", 
         "Comunicaciones", "Inmobiliarios", "Gobierno", 
         "Otros", key = "Sector", value = "Aporte")
D18 <- data_GDP %>%
  filter(Consolidado == 2018) %>%
  gather("Petroleo", "Comercio", "Manufactura", 
         "Comunicaciones", "Inmobiliarios", "Gobierno", 
         "Otros", key = "Sector", value = "Aporte")

#Grafico pirueta

G98 <- ggplot(D98, aes(x = Sector, y = Aporte)) + 
  geom_segment(aes(x = Sector, xend = Sector, 
                   y = 0, yend = Aporte)) +
  geom_point(size = 4, pch = 23, 
             fill = "#2C5985", color = "black") +
theme_light() + coord_flip() + 
  scale_y_continuous(breaks = seq(0, 20000, 5000), 
                     labels = label_number(suffix = "MM", scale = 1e-3)) +
  labs(title = "Actividad Económica por Sector", 
       subtitle = "Año: 1998. En USD MM", 
       caption = "Fuente: BCV",
       x = "", y = "") + 
  theme(plot.title=element_text(face = 'bold', hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))

G05 <- ggplot(D05, aes(x = Sector, y = Aporte)) + 
  geom_segment(aes(x = Sector, xend = Sector, 
                   y = 0, yend = Aporte)) +
  geom_point(size = 4, pch = 23, 
             fill = "#2C5985", color = "black") +
  theme_light() + coord_flip() + 
  scale_y_continuous(breaks = seq(0, 20000, 5000), 
                     labels = label_number(suffix = "MM", scale = 1e-3)) +
  labs(title = "Actividad Económica por Sector", 
       subtitle = "Año: 2005. En USD MM", 
       caption = "Fuente: BCV",
       x = "", y = "") + 
  theme(plot.title=element_text(face = 'bold', hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))
G12 <- ggplot(D12, aes(x = Sector, y = Aporte)) + 
  geom_segment(aes(x = Sector, xend = Sector, 
                   y = 0, yend = Aporte)) +
  geom_point(size = 4, pch = 23, 
             fill = "#2C5985", color = "black") +
  theme_light() + coord_flip() + 
  scale_y_continuous(breaks = seq(0, 20000, 5000), 
                     labels = label_number(suffix = "MM", scale = 1e-3)) +
  labs(title = "Actividad Económica por Sector", 
       subtitle = "Año: 2012. En USD MM", 
       caption = "Fuente: BCV",
       x = "", y = "") + 
  theme(plot.title=element_text(face = 'bold', hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))
G18 <- ggplot(D12, aes(x = Sector, y = Aporte)) + 
  geom_segment(aes(x = Sector, xend = Sector, 
                   y = 0, yend = Aporte)) +
  geom_point(size = 4, pch = 23, 
             fill = "#2C5985", color = "black") +
  theme_light() + coord_flip() + 
  scale_y_continuous(breaks = seq(0, 20000, 5000), 
                     labels = label_number(suffix = "MM", scale = 1e-3)) +
  labs(title = "Actividad Económica por Sector", 
       subtitle = "Año: 2018. En USD MM", 
       caption = "Fuente: BCV",
       x = "", y = "") + 
  theme(plot.title=element_text(face = 'bold', hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))

(G98 + G05) / (G12 + G18)

#Graficos, valores

Y <- ggplot(data, aes(x = year, y = cgdpe)) + 
  geom_line(lwd = 0.9) + 
  geom_area(fill = "#2E628C", alpha = 0.3) +
  theme_light() + labs(title = "Producto Interno Bruto", 
                       subtitle = "PPP's (2017)", 
                       caption = "Fuente: Penn World Table", 
                       x = "", y = "", tag = "Fig. 1") + 
  scale_x_continuous(breaks = seq(1959, 2019, 10)) + 
  scale_y_continuous(breaks = seq(5000, 725000, 144000), 
                     labels = label_number(suffix = " M", scale = 1e-3)) +
  theme(axis.text.x = element_text(angle = 30), 
        plot.title=element_text(face = 'bold', hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))

K <- ggplot(data, aes(x = year, y = cn)) + 
  geom_line(lwd = 0.9) + 
  geom_area(fill = "#2E628C", alpha = 0.3) +
  theme_light() + labs(title = "Stock de Capital", 
                       subtitle = "PPP's (2017)", 
                       caption = "Fuente: Penn World Table", 
                       x = "", y = "", tag = "Fig. 2") + 
  scale_x_continuous(breaks = seq(1959, 2019, 10)) + 
  scale_y_continuous(breaks = seq(90000, 3000000, 582000), 
                     labels = label_number(suffix = " M", scale = 1e-3)) +
  theme(axis.text.x = element_text(angle = 30), 
        plot.title=element_text(face = 'bold', hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))

L <- ggplot(data, aes(x = year, y = emp)) + 
  geom_line(lwd = 0.9) + 
  geom_area(fill = "#2E628C", alpha = 0.3) +
  theme_light() + labs(title = "Personas en Ocupación (Empleo)", 
                       subtitle = "En Millones de Personas", 
                       caption = "Fuente: Penn World Table", 
                       x = "", y = "", tag = "Fig. 3") + 
  scale_x_continuous(breaks = seq(1959, 2019, 10)) + 
  scale_y_continuous(breaks = seq(1, 16, 3), 
                     labels = label_number(suffix = " M", scale = 1)) +
  theme(axis.text.x = element_text(angle = 30), 
        plot.title=element_text(face = 'bold', hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))

A <- ggplot(data, aes(x = year, y = ctfp)) + 
  geom_line(lwd = 0.9) + 
  geom_area(fill = "#2E628C", alpha = 0.3) +
  theme_light() + labs(title = "Productividad Total de los Factores", 
                       subtitle = "Indice, USA = 1", 
                       caption = "Fuente: Penn World Table", 
                       x = "", y = "", tag = "Fig. 4") + 
  scale_x_continuous(breaks = seq(1959, 2019, 10)) + 
  scale_y_continuous(breaks = seq(0, 2, 0.4)) +
  theme(axis.text.x = element_text(angle = 30), 
        plot.title=element_text(face = 'bold', hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))

(Y + K) / (L + A) #Grafico

#Modelo Per capita

data <- tibble(pwt100) %>%
  filter(countrycode == "VEN") %>%
  mutate(k = cn / emp, y = cgdpe / emp, A = ctfp, 
         g = (A/lag(A))-1) %>%
  mutate(EC1 = y/(lag(A)*(1+g)), EC2 = k/(1+g)) %>%
  filter(year > 1955)

modelo_k <- lm(log(EC1) ~ log(EC2) + 0, data, na.action = na.exclude)
summary(modelo_k)

M <- function(alpha, PTF, r, Kpc) {
  PIBpc = PTF * ((1+r)^(1-alpha)) * Kpc^alpha
}

FIT <- M(0.91926, data$A, data$g, data$k)

datagraphy <- data %>%
  mutate(yfitted = FIT) %>%
  select(year, yfitted, y) %>%
  gather(yfitted, y, key = "Type", value = "Value")

data$y - FIT

plot(data$year, FIT)

jarque.bera.test(data$y - FIT)

ygraph <- ggplot(datagraphy, aes(x = year, y = Value, color = Type)) +
  geom_line(lwd = 1.3) +
  scale_color_manual(values = c("#26456E", "#0D652C"), 
                     labels = c("Real", "Fitted")) +
  theme_light() + labs(title = "PIB Per Capita", 
                       subtitle = "Real vs Estimado", 
                       caption = "Fuente: Penn World Table", 
                       x = "", y = "") + 
  scale_x_continuous(breaks = seq(1954, 2019, 5)) + 
  scale_y_continuous(breaks = seq(0, 60000, 10000), 
                     labels = label_number(suffix = "K", scale = 1e-3)) +
  theme(axis.text.x = element_text(angle = 30), 
        plot.title=element_text(face = 'bold', hjust=0.5),
        plot.subtitle=element_text(hjust=0.5)) + 
  theme(legend.title = element_blank()) 
 
ygraph

#Modelo No Per Cápita 

data <- tibble(pwt100) %>%
  filter(countrycode == "VEN") %>%
  mutate(EC1 = cgdpe /(ctfp*emp*hc), 
         EC2 = cn / (emp*hc), sv = rgdpe - rconna) %>%
  filter(year > 1954)

names(Saving) <- c("year", "sv")

modelo_Y <- lm(log(EC1) ~ log(EC2) + 0, data, 
               na.action = na.exclude)
summary(modelo_Y)

#Graficos Extra

Saving <- tibble(WEOSaving) %>%
  mutate(sv = Venezuela/3) %>%
  select(-Venezuela)

C <- ggplot(data, aes(x = year, y = rconna)) + 
  geom_area(fill = "#9C0824", 
            color = "black", alpha = 0.3, lwd = 0.8) + 
  theme_light() + 
  labs(title = "Consumo Final de Hogares",
       subtitle = "Real a precios del 2017",
       caption = "Fuente: Penn World Table",
       x = "", y = "", tag = "Fig. 1") + 
  scale_x_continuous(breaks = seq(1959, 2019, 10)) + 
  scale_y_continuous(breaks = seq(0, 30000, 5000), 
                     labels = label_number(suffix = "MM", 
                                           scale = 1e-3)) +
  theme(axis.text.x = element_text(angle = 30), 
        plot.title=element_text(face = 'bold', hjust=0.5),
        plot.subtitle=element_text(hjust=0.5)) + 
  theme(legend.title = element_blank()) 
print(C)

H <- ggplot(data, aes(x = year, y = hc)) + 
  geom_area(fill = "#64AAD2", 
            color = "black", alpha = 0.3, lwd = 0.8) + 
  theme_light() + 
  labs(title = "Indice de Capital Humano",
       subtitle = "Basado en tiempo de Escolarizacion",
       caption = "Fuente: Penn World Table",
       x = "", y = "", tag = "Fig. 3") + 
  scale_x_continuous(breaks = seq(1959, 2019, 10)) + 
  scale_y_continuous(breaks = seq(0, 3, 3/5)) +
  theme(axis.text.x = element_text(angle = 30), 
        plot.title=element_text(face = 'bold', hjust=0.5),
        plot.subtitle=element_text(hjust=0.5)) + 
  theme(legend.title = element_blank()) 
print(H)

S <- ggplot(Saving, aes(x = year, y = sv)) + 
  geom_area(fill = "#F4711F", 
            color = "black", alpha = 0.3, lwd = 0.8) + 
  theme_light() + 
  labs(title = "Ahorro Privado",
       subtitle = "Real a precios de 2017",
       caption = "Fuente: IMF",
       x = "", y = "", tag = "Fig. 2") + 
  scale_x_continuous(breaks = seq(1980, 2019, 7)) + 
  scale_y_continuous(breaks = seq(0, 20000, 4000), 
                     labels = label_number(suffix = "MM", 
                                           scale = 1e-3)) +
  theme(axis.text.x = element_text(angle = 30), 
        plot.title=element_text(face = 'bold', hjust=0.5),
        plot.subtitle=element_text(hjust=0.5)) + 
  theme(legend.title = element_blank()) 
print(S)

(C + S) / H





