
##cargar librerias 

library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)

##importar datos 

anac_2025 <- read_csv2("tarea05/202512-informe-ministerio-actualizado-dic-final.csv")
anac_2024 <- read_csv2("tarea05/202412-informe-ministerio-actualizado-dic-final.csv")
anac_2023 <- read_csv2("tarea05/202312-informe-ministerio-actualizado-dic.csv")
anac_2022 <- read_csv2("tarea05/202212-informe-ministerio-actualizado-dic-final.csv")
anac_2021 <- read_csv2("tarea05/202112-informe-ministerio-actualizado-dic-final.csv")
anac_2020 <- read_csv2("tarea05/202012-informe-ministerio-actualizado-dic-final.csv")
anac_2019 <- read_csv2("tarea05/201912-informe-ministerio-actualizado-dic-final.csv")

#aeropuertos

aeropuertos <- read_csv("tarea05/iata-icao.csv")

#clima

clima <- read_fwf("tarea05/registro_temperatura365d_smn.txt", 
                  col_positions = fwf_widths(
                    c(8, 1, 5, 1, 5, 200), 
                    c('fecha', 'x', 'tmax', 'y', 'tmin', 'nombre')
                  ),
                  skip = 3) |> 
  select(-x,-y)

##organización 
#normalizar columna Aeronave a texto para no tener conflictos al unir
lista_tablas <- list(anac_2019, anac_2020, anac_2021, anac_2022, anac_2023, anac_2024, anac_2025)
lista_tablas <- map(lista_tablas, ~ mutate(.x, Aeronave = as.character(Aeronave)))

#unir todas las tablas
vuelos_total <- bind_rows(lista_tablas)
clima <- clima |> mutate(fecha_prolija = dmy(fecha))
vuelos_total <- vuelos_total |> mutate(fecha_prolija = dmy(`Fecha UTC`))

#transformar variables clave a factor
vuelos_total <- vuelos_total |> 
  mutate(
    clasif_vuelo = factor(`Clasificación Vuelo`),
    tipo_movimiento = factor(`Tipo de Movimiento`),
    aerolinea = factor(`Aerolinea Nombre`)
  )

#creo un resumen mensual
vuelos_mensual <- vuelos_total |> 
  mutate(anio_mes = floor_date(fecha_prolija, "month")) |> 
  group_by(anio_mes, clasif_vuelo) |> 
  summarise(total_vuelos = n(), .groups = "drop")

##visualización

#a- ¿Qué se observa en la pandemia?
ggplot(vuelos_mensual, aes(x = anio_mes, y = total_vuelos, color = clasif_vuelo)) +
  geom_line(size = 1) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-20")), linetype = "dashed", color = "red") +
  labs(title = "Impacto del COVID-19 en Vuelos (2019-2025)",
       subtitle = "Caída drástica a partir de marzo 2020 (línea roja)",
       x = "Año", y = "Cantidad de Vuelos Mensuales") +
  theme_minimal()

#b- ¿Cuánto tiempo se tarda en recuperar flujos pre-pandemia?
promedio_2019 <- vuelos_mensual |> 
  filter(year(anio_mes) == 2019) |> 
  summarise(m = mean(total_vuelos)) |> pull(m)

ggplot(vuelos_mensual, aes(x = anio_mes, y = total_vuelos)) +
  geom_line() +
  geom_hline(yintercept = promedio_2019, color = "darkgreen", linetype = "dotted") +
  annotate("text", x = as.Date("2021-01-01"), y = promedio_2019 + 1000, 
           label = "Nivel Promedio 2019", color = "darkgreen") +
  labs(title = "Seguimiento de la Recuperación del Sector") +
  theme_light()

#c- ¿Diferencias en patrones de viaje (Destinos)? 

#preparo los datos
vuelos_patrones <- vuelos_total |> 
  rename(dest_codigo = `Origen / Destino`) |> 
  mutate(epoca = if_else(fecha_prolija < "2020-03-01", "Pre-Pandemia", "Post-Pandemia"))

#cruce con Aeropuertos
top_destinos <- vuelos_patrones |> 
  count(epoca, dest_codigo) |> 
  group_by(epoca) |> 
  slice_max(n, n = 5) |> 
  ungroup() |> 
  left_join(aeropuertos, by = c("dest_codigo" = "iata")) |> 
  select(epoca, dest_codigo, airport, n)

print(top_destinos)

#volúmenes del periodo Post-Pandemia se ven mucho más grandes en las barras porque se sumaron más años de datos (2021-2025) en comparación al periodo Pre (2019-2020).

ggplot(top_destinos, aes(x = reorder(dest_codigo, n), y = n, fill = epoca)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Top 5 Destinos: Pre vs Post Pandemia",
       x = "Código IATA", 
       y = "Cantidad de Vuelos", 
       fill = "Periodo") +
  theme_minimal()

##relación Clima y Vuelos

#resumo vuelos a nivel diario
vuelos_diarios_2025 <- vuelos_total |> 
  filter(year(fecha_prolija) == 2025) |> 
  group_by(fecha_prolija) |> 
  summarise(cantidad_vuelos = n(), .groups = "drop")

vuelos_clima_resumen <- vuelos_diarios_2025 |> 
  left_join(clima, by = "fecha_prolija")

#gráfico de dispersión (+ línea de tendencia)
ggplot(vuelos_clima_resumen, aes(x = tmax, y = cantidad_vuelos)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "darkorange") +
  labs(title = "Relación Clima y Actividad Aérea (2025)",
       subtitle = "Análisis diario: Temperatura Máxima vs. Cantidad de Vuelos",
       x = "Temperatura Máxima (°C)", 
       y = "Vuelos por día") +
  theme_minimal()

##Preguntas

1-¿Qué se observa en la pandemia?
Vemos que en el gráfico "Impacto del COVID-19", la caída es abrupta y casi total a partir de marzo 2020 (línea roja). Además, observamos que tanto los vuelos domésticos como internacionales llegaron a niveles cercanos a cero durante los primeros meses de cuarentena.
Por otro lado, si nos enfocamos en la linea verde (vuelos domésticos), vemos que iniciaron una recuperación mucho más agresiva y temprana en comparación con los internacionales (línea azul), que permanecieron estancados hasta casi finales de 2021 debido a las restricciones de las fronteras.

2-¿Cuánto tiempo se tarda en recuperar flujos pre-pandemia?
Observando el gráfico de "Seguimiento de la Recuperación" y comparando contra la línea verde punteada (Promedio 2019), encontramos que la actividad aérea tardó aproximadamente 3 años en volver a niveles normales. Recién para principios de 2023 la serie de tiempo comienza a oscilar de forma más consistente por encima del promedio de 2019.
Es notable que cuando recupera el nivel se mantienen los picos y valles pronunciados, mostrando que el mercado recuperó su comportamiento habitual de temporadas altas.

3-¿Se pueden apreciar diferencias en los patrones de viajes antes/después?
Según gráfico "Top 5 Destinos", el patrón más evidente es el crecimiento masivo de Aeroparque. En la post-pandemia se mantuvo como líder mientras que su volumen de operaciones se multiplicó, probablemente debido a la regionalización del aeropuerto que permitía más vuelos a países limítrofes desde ahí.
Por otro lado, Ezeiza aparece entre los primeros 5 de la post-pandemia, desplazando a destinos del interior que figuraba en ese puesto durante el Pre, indicando un cambio en la concentración de vuelos en los nodos principales de Buenos Aires. Esto sucede a la vez que se demuestra la estabilidad y centralidad de la aviación de instrucción para el sistema aéreo argentino al observar la permanencia de aeropuertos secundarios como el de Morón durante ambos periodos.
