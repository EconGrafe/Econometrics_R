library(tidyverse)
library(sf)

munven <- st_read("C:/XX/XX/map/Municipios_Venezuela.shp") #Colocar dirección donde se descargó la carpeta "map"

#Luego de hacer lo anterior, invocar la BDD "Municipios" como un tibble o data frame

names(Municipios) <- c("CODE", "STATE", "MUNICIPIO", "URBAN", "RURAL", 
                       "POP")

data <- left_join(munven, Municipios)

data <- data %>%
  mutate(POP = POP / 1000000)

ggplot() + geom_sf(data = data, aes(fill = POP), color = NA) + theme_light() +
  labs(title = "Población por Municipio", caption = "Fuente: INE", 
       x = "", y = "") + 
  theme(legend.title = element_blank(), 
        axis.text.y = element_blank(), 
        axis.text.x = element_blank(),
        plot.title=element_text(face = 'bold', hjust=0.5)) + 
  scale_fill_continuous(type = "viridis")

