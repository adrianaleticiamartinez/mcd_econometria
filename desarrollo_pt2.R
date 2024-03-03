# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)
library(forecast)
library(vars)

#Paso1
# Cargando datos
datos_INEGI <- read.csv("mcd_econometria/datasets/AGEEML_CDMX_2024213148973.csv")
datos_SHF <- read.csv("mcd_econometria/datasets/Indice_SHF _4_trim_2023.csv")
datos_SNIIV <- read.csv("mcd_econometria/datasets/financiamientos_full_CDMX.csv")
datos_CNBV <- read.csv("mcd_econometria/datasets/cnbv_full_CDMX.csv")

#Eliminamos algunas columnas inecesarias delos sets de datos de financiamientos y CNBV
#Para posteriormente poderlos unir
datos_SNIIV <- subset(datos_SNIIV, select = -c(tipo,modalidad,destino))
datos_CNBV <- subset(datos_CNBV, select = -c(zona,poblacion_indigena,modalidad,linea_credito,esquema))

#Cambiamos nombre de una variable para compatibilidad organismo , cambiar a intermediario_financiero
datos_SNIIV <- datos_SNIIV %>% rename(intermediario_financiero = organismo)

# Unir los datasets
datos_unidos <- bind_rows(datos_SNIIV, datos_CNBV)

#Creamos una columna de fecha considerando mes y año
datos_unidos$fecha <- as.Date(paste(datos_unidos$año, datos_unidos$mes, "1", sep = "-"), format = "%Y-%m-%d")


# Filtrando datos para CDMX y con un monto mayor a 1 y que cuentan con valores nulos en vivienda valor
cdmx_datos <- datos_unidos %>% filter(cve_ent == 9)
cdmx_datos <- cdmx_datos[!is.na(cdmx_datos$vivienda_valo),]
datos_financiamiento <- cdmx_datos %>% filter(monto > 1)

#Filtrando los valores de rango ingreso para considerar a aquellas familias con ingresos bajos
datos_vivienda_incluyente <- datos_financiamiento %>% filter(ingresos_rango < 4)
datos_vivienda_no_incluyente <- datos_financiamiento %>% filter(ingresos_rango > 4)


#Paso 2

# Convertir las fechas a formato de fecha y agregar una columna de año para agrupar
datos_vivienda_no_incluyente$fecha <- as.Date(as.character(datos_vivienda_no_incluyente$fecha), format="%Y")
datos_vivienda_incluyente$fecha <- as.Date(as.character(datos_vivienda_incluyente$fecha), format="%Y")


# Sumarizar los datos para obtener el monto total por fecha
datos_fecha_f <- datos_vivienda_no_incluyente %>%
  group_by(fecha) %>%
  summarise(monto_total = sum(monto, na.rm = TRUE))

datos_fecha_i <- datos_vivienda_incluyente %>%
  group_by(fecha) %>%
  summarise(monto_total = sum(monto, na.rm = TRUE))

# Sumarizar los datos para obtener el monto promedio por fecha para cada grupo
resumen_incluyente <- datos_vivienda_incluyente %>%
  group_by(fecha) %>%
  summarise(total_incluyente = mean(monto, na.rm = TRUE))

resumen_no_incluyente <- datos_vivienda_no_incluyente %>%
  group_by(fecha) %>%
  summarise(total_no_incluyente = mean(monto, na.rm = TRUE))
#Montos
#Visualizar tendencias
library(ggplot2)
ggplot(datos_fecha_f, aes(x = fecha, y = monto_total)) + geom_line() + labs(title = "Tendencias de Financiamiento en CDMX")
ggplot(datos_fecha_i, aes(x = fecha, y = monto_total)) + geom_line() + labs(title = "Tendencias de Financiamiento vivienda incluyente en CDMX")

# Unir los dos resúmenes para comparar
comparativa <- full_join(resumen_incluyente, resumen_no_incluyente, by = "fecha")
# Crear una visualización comparativa
ggplot(comparativa, aes(x = fecha)) + 
  geom_line(aes(y = total_incluyente, colour = "Incluyente")) + 
  geom_line(aes(y = total_no_incluyente, colour = "No Incluyente")) +
  labs(title = "Comparativa de Financiamiento: Vivienda Incluyente vs No Incluyente", 
       y = "Monto promedio de Financiamiento")

## Totales
# Sumarizar los datos para obtener el total de créditos por fecha
datos_credito_incluyente <- datos_vivienda_incluyente %>%
  group_by(fecha) %>%
  summarise(total_creditos_i = n())

datos_credito_no_incluyente <- datos_vivienda_no_incluyente %>%
  group_by(fecha) %>%
  summarise(total_creditos_n = n())

comparativa_creditos <- full_join(datos_credito_incluyente, datos_credito_no_incluyente, by = "fecha")

# Crear una visualización comparativa
ggplot(comparativa_creditos, aes(x = fecha)) + 
  geom_line(aes(y = total_creditos_i, colour = "Incluyente")) + 
  geom_line(aes(y = total_creditos_n, colour = "No Incluyente")) +
  labs(title = "Comparativa de Créditos otorgados: Vivienda Incluyente vs No Incluyente", 
       y = "Total de Créditos")

# Crear una serie de tiempo con los datos sumarizados
ts_datos <- ts(resumen_incluyente$total_incluyente, start = c(2013), frequency = 1)

# Aplicar un modelo ARIMA a la serie de tiempo
#Para calcular montos de financiamiento promedio
modelo_arima <- auto.arima(ts_datos)
forecast_arima <- forecast(modelo_arima, h = 5)
plot(forecast_arima)

# Crear un modelo VAR con ambos montos de financiamiento y valor de vivienda
datos_var <- cbind(datos_vivienda_incluyente$vivienda_valor, datos_vivienda_incluyente$monto)
modelo_var <- VAR(datos_var, p = 2)
plot(irf(modelo_var))


