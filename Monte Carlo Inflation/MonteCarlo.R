# Call Libraries

pacman::p_load(tidyverse, janitor, forecast, tseries, lmtest, readxl)

# Call Data

inflation <- read_csv('inflation.csv') %>% clean_names() %>%
  mutate(cpi = cpi / cpi[1] * 100)

inf <- ts(inflation$var, start = c('2012', '01'), frequency = 12)

# Graph Data

autoplot(inf)

# Stablish Combinations matrix

Time <- length(inf) + 5
index <- 5000
pred <- matrix(nrow = Time, ncol = index)
pred[1:139, ] <- inf[1:139]

# Arima base model

coeftest(arima(inf, order = c(3, 1, 2)))

# Montecarlo Distribution

for (i in 1:index) {
  for (t in 140:Time) {
    nval <- rnorm(2, 0.35, 0.25)
    pred[t, i] <- 0.41 * nval[1] + 0.75 * nval[2] - 0.67 * pred[t-1, i] - 0.95 * pred[t-2, i] - 0.47 * pred[t-3, i]
  }
}

# Reduce Observations

real <- matrix(nrow = 13, ncol = index)
real[1:8, ] <- inf[132:139]
real[9:13, ] <- pred[140:144, ]

ts.plot(real)

# Calculate CPI

cpi <- matrix(nrow = 13, ncol = index)
cpi[1, ] <- inflation$cpi[132]

for (i in 1:index) {
  for (t in 2:13) {
    cpi[t, i] <- (1 + real[t, i]) * cpi[t-1, i]
    }
}

# Estimate Inflations

dist <- (cpi[13, ] / cpi[1, ] - 1) * 100

hist(dist, prob = T, ylim = c(0, max(dnorm(dist, mean(dist), sd(dist)))), 
     main = 'Histograma con Curva de Distribución', col = 'white')
lines(density(dist), col = 'blue', lwd = 2)

jarque.bera.test(dist)

tibble(
  Li = mean(dist) - sd(dist),
  Mean = mean(dist),
  Ls = mean(dist) + sd(dist)
)
