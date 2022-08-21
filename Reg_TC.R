#Librerias
library(tidyverse)
library(lmtest)
library(car)
library(strucchange)
library(tseries)
library(orcutt)

#Matriz de Correlacion
data.frame(Data_Semanal) %>%
  select(TC_BCV, RIN, D_PDVSA, Nro_INT, M2, BM) %>%
  cor()

data <- data.frame(Data_Semanal) #Fuente de Datos

#Verificacion de tendencia de las variables
data %>%
  select(TC_BCV, RIN, D_PDVSA, Nro_INT) %>%
  plot()

#Construccion del Modelo
reg <- lm(TC_BCV ~ log(RIN) + D_PDVSA + Nro_INT, data)
summary(reg)

#Verificacion de omision de variables
resettest(reg) #Test Reset de Ramsey 

#Verificacion de normalidad en los errores
jarque.bera.test(residuals(reg)) #Test de Jarque Bera 

#Descarte de multicolinealidad
vif(reg) #Inflacion de la varianza

#Descarte de Heterocedasticidad
bptest(reg, ~predict(reg) + I(predict(reg)^2)) #Test de White
bptest(reg) #Test de Breusch Pagan

#Descarte de autocorrelacion
dwtest(reg) #Estadistico de Durbin Watson
bgtest(reg) #Test de Breusch Godfrey

#Correccion de Cochrane Orcutt
orreg <- cochrane.orcutt(reg)
summary(orreg)

resettest(orreg)

jarque.bera.test(residuals(orreg))

bptest(orreg)

dwtest(orreg)
bgtest(orreg)

#Prediccion del modelo 
B <- as.vector(coefficients(orreg))
Xi <- as.matrix(c(1, log(5257*(1-0.0023)^2), 126549.61*(1+0.022)^11, 1))
Xm <- as.matrix(c(1, log(5257*(1-0.0023)^2), 126549.61*(1+0.032)^11, 1))
Yi <- B*Xi
Ym <- B*Xm

apply(Yi, 2, sum) #Valor proyectado del TC (Li)
apply(Ym, 2, sum) #Valor proyectado del TC (Ls)


