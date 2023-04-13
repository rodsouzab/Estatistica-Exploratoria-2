library(ggplot2)
library(OpenStreetMap)
library(tidyverse)
library(readxl)
library(sp)

excel.data = read_xlsx("C:\\Users\\rodri\\OneDrive\\Documentos\\Estatística Exploratória 1\\2 VA\\dados_de_caminhada_corrida.xlsx")

excel.data = excel.data %>% separate(Coordenadas,c("lat","long"),",") %>%
              separate(Velocidade,c("veloc","khm")," ") %>%
                mutate(long = as.numeric(long), lat = as.numeric(lat), veloc = as.numeric(veloc))

long = excel.data %>%
        select(long)
lat = excel.data %>%
  select(lat)

bb = matrix(c(-34.945, -34.955,
              -8.018, -8.0135), 2,2, byrow=T)
rownames(bb) = c('long', 'lat')
colnames(bb) = c('min', 'max')

crs = CRS("+proj=utm +zone=25 +south +datum=WGS84")


df = data.frame(veloc = excel.data %>% select(veloc),
                long = excel.data %>% select(long),
                lat = excel.data %>% select(lat))



#df$long = long
#df$lat = lat
lonr = bb[1,2]; latu = bb[2,2]
lonl = bb[1,1]; latd = bb[2,1]

sa_map = openmap(c(latu+0.001, lonl-0.001),
                 c(latd-0.001, lonr+0.001),
                 type = "osm", mergeTiles = TRUE, minNumTiles = 9L)

sa_map2 = openproj(sa_map)


sa_map2_plt = OpenStreetMap::autoplot.OpenStreetMap(sa_map2) +
  geom_point(data = df,
             aes(x = long, y = lat), # slightly shift the points
             colour = "red", size =  2.5) +
  xlab("Longitude") + ylab("Latitude")
sa_map2_plt

