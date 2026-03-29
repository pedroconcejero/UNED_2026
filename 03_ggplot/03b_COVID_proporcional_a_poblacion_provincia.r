# Master Big Data UNED 2026
# Preparación dataset incidencia COVID proporcional a población por provincia

library(tidyverse)
library(stringr)

covid <- read_csv(unzip("casos_hosp_uci_def_sexo_edad_provres.zip"))
str(covid)

prv <- read_csv("provinces_es.csv")
str(prv)

# Mezclamos la información de incidencia covid con la de provincias
cov_prv <- merge(covid,
                 prv,
                 by.x = "provincia_iso",
                 by.y = "code")

str(cov_prv)

# Realizamos la importación de los datos de población de cada provincia, 
# correspondientes a los años 2020, 2021 y 2022.
# Para luego poder cruzar los datos de incidencia (diarios) con los de 
# población (anuales), debemos extraer el año

covid$anio <- as.numeric(format(covid$fecha, '%Y'))
summary(covid)

# Leemos ahora los datos de población

pob_esp2020 <- read.table('PoblacionEspana-Provincia_2020.csv',
                          sep = ";",
                          header = T,
                          encoding = "latin1")

str(pob_esp2020)

pob_esp2021 <- read.table('PoblacionEspana-Provincia_2021.csv',
                          sep = ";",
                          header = T,
                          encoding = "latin1")

str(pob_esp2021)

pob_esp2022 <- read.table('PoblacionEspana-Provincia_2022.csv',
                          sep = ";",
                          header = T,
                          encoding = "latin1")

str(pob_esp2022)

pob_esp <- rbind(pob_esp2020,
                 pob_esp2021,
                 pob_esp2022)

head(pob_esp)

# Cambiemos algunos nombres de este objeto con población
# para facilitarnos la mezcla posterior con incidencia COVID

names(pob_esp) <- c("edad",
                    "provincia",
                    "sexo",
                    "periodo",
                    "total")

# Y ese (absurdo!) formato de la población hay que cambiarlo

pob_esp$total <- str_replace(pob_esp$total, ",", "|")
pob_esp$total <- str_replace(pob_esp$total, "\\.", "")
pob_esp$total <- str_replace(pob_esp$total, "\\|", ".")

head(pob_esp$total)

pob_esp$total <- as.numeric(pob_esp$total)
summary(pob_esp)

# Ahora extraigamos el código de provincia (acrónimo ISO, 
# p.ej. M = Madrid) de nuestro dataset de incidencia
# Esto hay que hacerlo por un doble paso
# Primero sacamos el código postal

pob_esp$postal_code <- substr(pob_esp$provincia, 
                              start = 1,
                              stop = 2)
table(pob_esp$postal_code)

# Algunos cambios más en códigos
table(pob_esp$sexo)
pob_esp$sexo[ pob_esp$sexo == 'Hombres'] <- 'H'
pob_esp$sexo[ pob_esp$sexo == 'Mujeres'] <- 'M'
table(pob_esp$sexo)

head(pob_esp)
table(pob_esp$edad)
pob_esp$edad <- str_replace(pob_esp$edad, " años", "")
pob_esp$edad <- str_replace(pob_esp$edad, " año", "")
pob_esp$edad <- str_replace(pob_esp$edad, "100 y más", "100")

pob_esp$edad2 <- as.numeric(pob_esp$edad)
summary(pob_esp$edad2)


pob_esp$grupo_edad[pob_esp$edad2 < 10] <- '0-9'
pob_esp$grupo_edad[pob_esp$edad2 >= 10 
                   & pob_esp$edad2 < 20] <- '10-19'
pob_esp$grupo_edad[pob_esp$edad2 >= 20 
                   & pob_esp$edad2 < 30] <- '20-29'
pob_esp$grupo_edad[pob_esp$edad2 >= 30 
                   & pob_esp$edad2 < 40] <- '30-39'
pob_esp$grupo_edad[pob_esp$edad2 >= 40 
                   & pob_esp$edad2 < 50] <- '40-49'
pob_esp$grupo_edad[pob_esp$edad2 >= 50 
                   & pob_esp$edad2 < 60] <- '50-59'
pob_esp$grupo_edad[pob_esp$edad2 >= 60 
                   & pob_esp$edad2 < 70] <- '60-69'
pob_esp$grupo_edad[pob_esp$edad2 >= 70 
                   & pob_esp$edad2 < 80] <- '70-79'
pob_esp$grupo_edad[pob_esp$edad2 >= 80] <- '80+'

table(pob_esp$grupo_edad)

# Agrupamos por periodo, grupo_edad, y postal_code

poblacion <- pob_esp %>%
  group_by(periodo,
           grupo_edad,
           postal_code) %>%
  summarize(pobl_total = sum(total))

head(poblacion)
summary(poblacion)

poblacion[poblacion$pobl_total == max(poblacion$pobl_total), ]

table(poblacion$periodo)

poblacion$anio <- str_sub(poblacion$periodo, start = -4)
table(poblacion$anio)

# Leamos info. adicional de provincias

provincias <- read_csv("provinces_es.csv")
head(provincias)

poblacion_def <- merge(poblacion,
                       provincias[, c(1, 2,3)],
                       by.x = "postal_code",
                       by.y = "postal_code")

head(poblacion_def)
poblacion_def <- poblacion_def[, c(3:dim(poblacion_def)[2])]
str(poblacion_def)

# Necesitamos el anio en incidencia covid

covid$anio <- as.numeric(format(covid$fecha, '%Y'))
summary(covid)
# Ahora sí podemos cruzar incidencia y poblacion
# Y calcular la ratio por 100.000 (el estándar)

covid_proporcional <- merge(covid,
                            poblacion_def,
                            by.x = c("provincia_iso",
                                     "grupo_edad",
                                     "anio"),
                            by.y = c("code",
                                     "grupo_edad",
                                     "anio"))

head(covid_proporcional)

covid_proporcional$casos_prop <- covid_proporcional$num_casos/covid_proporcional$pobl_total * 100000
covid_proporcional$hosp_prop <- covid_proporcional$num_hosp/covid_proporcional$pobl_total * 100000
covid_proporcional$uci_prop <- covid_proporcional$num_uci/covid_proporcional$pobl_total * 100000
covid_proporcional$def_prop <- covid_proporcional$num_def/covid_proporcional$pobl_total * 100000

summary(covid_proporcional)

# Hagamos un heatmap año por año (necesitamos agregar)

covid_anio <- covid_proporcional %>%
#  filter(anio == 2020) %>%
  group_by(name, 
           grupo_edad,
           anio) %>%
    summarise(
        total_casos = sum(num_casos),
        total_hosp = sum(num_hosp),
        total_uci = sum(num_uci),
        total_def = sum(num_def),
        pobl_total = first(pobl_total)
    )

summary(covid_anio)

covid_anio$casos_prop <- covid_anio$total_casos/covid_anio$pobl_total * 100000
covid_anio$hosp_prop <- covid_anio$total_hosp/covid_anio$pobl_total * 100000
covid_anio$uci_prop <- covid_anio$total_uci/covid_anio$pobl_total * 100000
covid_anio$def_prop <- covid_anio$total_def/covid_anio$pobl_total * 100000

# Puedes seguir un buenísimo tutorial aquí:
# https://statisticsglobe.com/heatmap-in-r

# Hagamos año por año
# 2020

covid_2020 <- covid_anio[covid_anio$anio == 2020, ]

# Para mostrar el formato tabla de texto (wide)

covid_2020_tabla <- pivot_wider(covid_2020[, c("name",
                                               "grupo_edad",
                                               "casos_prop")],
                                names_from = grupo_edad,
                                values_from = casos_prop)

View(covid_2020_tabla)
# Los datos ya están en long_format

library(ggplot2)

ggp <- ggplot(covid_2020,
              aes(grupo_edad,
                  name)) +
  geom_tile(aes(fill = casos_prop))

ggp + scale_fill_gradient(low = "yellow",
                          high = "red")



# Mostrar los valores en cada casilla ("tile")
# a partir de https://r-charts.com/correlation/heat-map-ggplot2/

# you can add the values over the tiles with geom_text, 
#passing the numerical variable to the label argument of the aes function.

ggp + scale_fill_gradient(low = "yellow",
                          high = "red") +
    geom_text(aes(label = casos_prop), 
              color = "black", size = 1) 


ggp + scale_fill_gradient(low = "yellow",
                          high = "red") +
    geom_text(aes(label = as.integer(casos_prop)), 
                color = "black", size = 2) 

def_casos_2020 <- ggp + scale_fill_gradient(low = "yellow",
                          high = "red") +
    geom_text(aes(label = round(casos_prop)), 
              color = "black", size = 2) 

ggsave("heatmap_casos_2020.png",
       plot = def_casos_2020)

ggsave("heatmap_casos_2020_max.png",
       plot = def_casos_2020,
       width = 12,
       height = 12,
       dpi = 600)
