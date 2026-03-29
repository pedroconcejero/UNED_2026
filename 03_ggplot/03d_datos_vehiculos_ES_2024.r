# Master Big Data UNED 2026
# Datos vehículos España 2024

# Datos obtenidos a partir de datos homologación Comisión Europea
# https://www.eea.europa.eu/en/datahub/datahubitem-view/fa8b1229-3db6-495d-b18e-9c9b3267c02b

# Explicado en detalle en la documentación del módulo

library(ggplot2)

veh_Esp_24 <- read.table("coches_ES_2024.csv",
                         sep = "|",
                         header = T,
                         comment.char = "")
                         
View(veh_Esp_24)

names(veh_Esp_24)

names(veh_Esp_24) <- c("Manufacturer", 
                       "Model", 
                       "Category", # una constante
                       "Type", # incluye combustible, eléctrico, híbrido
                       "Weight_kg", 
                       "power_kW",
                       "engine_capacity_cm3",
                       "Electric_consumption_Wh_km",
                       "Emissions_CO2_g_km_WLTP",
                       "Wheel_base_mm", # vacía
                       "Width_axis1_mm", # vacía
                       "Width_axis2_mm", # vacía
                       "Electric_power_kW_1", #repetido con 6
                       "Electric_consumption_Wh_km_2", #repetido con 8
                       "Fuel_consumption_l_100_km",
                       "Electric_range_km",
                       "Total_registros",
                       "last_date")

veh_Esp_24_clean <- veh_Esp_24[, c("Manufacturer", 
                                   "Model", 
                                   "Type", 
                                   "Weight_kg", 
                                   "power_kW",
                                   "engine_capacity_cm3", 
                                   "Electric_consumption_Wh_km",
                                   "Emissions_CO2_g_km_WLTP", 
                                   "Fuel_consumption_l_100_km",
                                   "Electric_range_km",
                                   "Total_registros",
                                   "last_date")]

View(veh_Esp_24)
save(veh_Esp_24_clean,
     file = "veh_Esp_24_clean.RData")

dim(veh_Esp_24_clean)

Motor_combustion <- veh_Esp_24_clean[veh_Esp_24_clean$Type != "electric", ]
dim(Motor_combustion)
View(Motor_combustion)
str(Motor_combustion)

ggplot(Motor_combustion) +
    aes(x = 'engine_capacity_cm3', 
        y = 'Fuel_consumption_l_100_km')

ggplot(Motor_combustion) + 
    aes(x = engine_capacity_cm3, 
        y = Fuel_consumption_l_100_km) + geom_point()

primer_grafico <- ggplot(Motor_combustion) + 
    aes(x = engine_capacity_cm3, 
        y = Fuel_consumption_l_100_km) + geom_point()

primer_grafico + geom_smooth(method = "lm", se = F) 

segundo_grafico <- ggplot(Motor_combustion) + 
    aes(x = engine_capacity_cm3, 
        y = Fuel_consumption_l_100_km,
        color = Type) + 
    geom_point()

segundo_grafico

segundo_grafico + geom_smooth(method = "lm")

tercer_grafico <- ggplot(Motor_combustion) + 
    aes(x = engine_capacity_cm3, 
        y = Fuel_consumption_l_100_km,
        color = Type) + 
    geom_point() + 
    facet_wrap(~Type)

tercer_grafico



tercer_grafico <- ggplot(Motor_combustion[Motor_combustion$Type %in% c("diesel",
                                                                       "diesel/electric",
                                                                       "lpg",
                                                                       "petrol",
                                                                       "petrol/electric"),]) + 
    aes(x = engine_capacity_cm3, 
        y = Fuel_consumption_l_100_km,
        color = Type) + 
    geom_point() + 
    facet_wrap(~Type)

tercer_grafico + geom_smooth(method = "loess", 
                             span = 0.1)

cuarto_grafico <- ggplot(Motor_combustion[Motor_combustion$Type %in% c("diesel",
                                                                       "diesel/electric",
#                                                                       "lpg",
                                                                       "petrol",
                                                                       "petrol/electric"),]) + 
    aes(x = engine_capacity_cm3, 
        y = Fuel_consumption_l_100_km,
        color = Type) + 
    geom_point() + 
    geom_smooth(method = "loess", 
                span = 1)
cuarto_grafico

quinto_grafico <- ggplot(Motor_combustion[Motor_combustion$Type %in% c("diesel",
                                                                       "diesel/electric",
#                                                                       "lpg",
                                                                       "petrol",
                                                                       "petrol/electric"),]) + 
    aes(x = engine_capacity_cm3, 
        y = Fuel_consumption_l_100_km,
        color = Type) + 
    geom_point() + 
    geom_smooth(method = "gam")
quinto_grafico

primer_plot_CO2 <- ggplot(Motor_combustion) + 
    aes(x = engine_capacity_cm3, 
        y = Emissions_CO2_g_km_WLTP,
        color = Type) + 
    geom_point()
primer_plot_CO2 + geom_smooth(method = "lm", se = F)

