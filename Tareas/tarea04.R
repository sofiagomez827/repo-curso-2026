library(nycflights13)
library(tidyverse)
library(dplyr)
flights
?flights
glimpse(flights)

#Devuelve la demora promedio de llegada a IAH, organizada por cada día.
flights |>
  filter(dest == "IAH") |> 
  group_by(year, month, day) |> 
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )

#Permite conservar filas según los valores de las columnas
flights |> 
  filter(dep_delay > 120)

#y
flights |> 
  filter(month == 1 & day == 1)
#o
flights |> 
  filter(month == 1 | month == 2)

# A shorter way to select flights that departed in January or February
flights |> 
  filter(month %in% c(1, 2))

#guardar el resultado
jan1 <- flights |> 
  filter(month == 1 & day == 1)

flights |> 
  filter(month = 1)

flights |> 
  filter(month == 1 | 2)

#arrange()Cambia el orden de las filas según el valor de las columnas
flights |> 
  arrange(year, month, day, dep_time)

#reordenar el marco de datos según esa columna en orden descendente
flights |> 
  arrange(desc(dep_delay))

# Remove duplicate rows, if any
flights |> 
  distinct()

# Find all unique origin and destination pairs
flights |> 
  distinct(origin, dest)

#distinct()encontrará la primera aparición de una fila única en el conjunto de datos y descartará el resto.
flights |> 
  distinct(origin, dest, .keep_all = TRUE)

#encontrar el número de ocurrencias
flights |>
  count(origin, dest, sort = TRUE)


#mutate()consiste en añadir nuevas columnas que se calculan a partir de las columnas existentes
#gaintiempo que un vuelo retrasado recuperó en el aire y la velocidad speeden millas por hora
flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / (air_time / 60)
  )

#.beforeargumento para agregar las variables al lado izquierdo
flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = 1
  )

#.beforees un argumento de la función, no el nombre de una tercera variable nueva que estamos creando. También puede usar .afterpara agregar después de una variable
flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .after = day
  )

#controlar qué variables se conservan con el .keepargumento
flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used"
  )

#Seleccione las columnas por nombre:

flights |> 
  select(year, month, day)

#Seleccione todas las columnas entre el año y el día (inclusive):

flights |> 
  select(year:day)

#Seleccione todas las columnas excepto las que van desde el año hasta el día de hoy (inclusive):

flights |> 
  select(!year:day)

#Seleccione todas las columnas que contengan caracteres:

flights |> 
  select(where(is.character))


#renombrar las variables
flights |> 
  select(tail_num = tailnum)

#conservar todas las variables existentes y solo desea cambiar el nombre de algunas, puede usar rename()en lugar de select()
flights |> 
  rename(tail_num = tailnum)

#relocate()para reorganizar variables
flights |> 
  relocate(time_hour, air_time)

flights |> 
  relocate(year:dep_time, .after = time_hour)
flights |> 
  relocate(starts_with("arr"), .before = dep_time)

#tuberia: encontrar los vuelos más rápidos al aeropuerto IAH de Houston
flights |> 
  filter(dest == "IAH") |> 
  mutate(speed = distance / air_time * 60) |> 
  select(year:day, dep_time, carrier, flight, speed) |> 
  arrange(desc(speed))

arrange(
  select(
    mutate(
      filter(
        flights, 
        dest == "IAH"
      ),
      speed = distance / air_time * 60
    ),
    year:day, dep_time, carrier, flight, speed
  ),
  desc(speed)
)

flights1 <- filter(flights, dest == "IAH")
flights2 <- mutate(flights1, speed = distance / air_time * 60)
flights3 <- select(flights2, year:day, dep_time, carrier, flight, speed)
arrange(flights3, desc(speed))

#grupos
#group_by()para dividir su conjunto de datos en grupos significativos para su análisis:
flights |> 
  group_by(month)

#summarize()
flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE)
  )

#n(), que devuelve el número de filas en cada grupo
flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    n = n()
  )

#df |> slice_head(n = 1)toma la primera fila de cada grupo.
#df |> slice_tail(n = 1)toma la última fila de cada grupo.
#df |> slice_min(x, n = 1)toma la fila con el valor más pequeño de la columna x.
#df |> slice_max(x, n = 1)toma la fila con el valor más grande de la columna x.
#df |> slice_sample(n = 1)toma una fila aleatoria.

#vuelos que sufren más retrasos al llegar a cada destino
flights |> 
  group_by(dest) |> 
  slice_max(arr_delay, n = 1) |>
  relocate(dest)
#Agrupación por múltiples variables,grupo para cada fecha
daily <- flights |>  
  group_by(year, month, day)
daily

daily_flights <- daily |> 
  summarize(n = n())

daily_flights <- daily |> 
  summarize(
    n = n(), 
    .groups = "drop_last"
  )
#Desagrupación, eliminar la agrupación de un marco de datos sin usar summarize()
daily |> 
  ungroup()

# se resume un marco de datos no agrupado
daily |> 
  ungroup() |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    flights = n()
    
    # .byargumento para agrupar dentro de una sola operación
    flights |> 
      summarize(
        delay = mean(dep_delay, na.rm = TRUE), 
        n = n(),
        .by = month
      )
    
    flights |> 
      summarize(
        delay = mean(dep_delay, na.rm = TRUE), 
        n = n(),
        .by = c(origin, dest)
      )

##Ejercicios 19.2 

# 1: We forgot to draw the relationship between weather and airports in Figure 19.1. 
# What is the relationship and how should it appear in the diagram?
glimpse(weather)
glimpse(airports)

unique(airports$faa)
unique(weather$origin)

# Se relacionan con faa y origin. 

# 2: weather only contains information for the three origin airports in NYC. 
# If it contained weather records for all airports in the USA, what additional connection would it make to flights?

# Se podria conectar con dest. 

# 3: The year, month, day, hour, and origin variables almost form a compound key for weather, but there’s one hour that has duplicate observations. 
# Can you figure out what’s special about that hour?

weather |> 
  count(year, month, day, hour, origin) |> 
  filter(n > 1)

# observaciones duplicadas por cambio de horario verano-invierno

# 4: We know that some days of the year are special and fewer people than usual fly on them (e.g., Christmas). 
# How might you represent that data as a data frame? 
# What would be the primary key? How would it connect to the existing data frames?

less_people_dates <- tribble(
  ~month, ~day, ~event, 
  1, 1, "New Year", 
  12, 24, "Christmas Eve", 
  12, 25, "Christmas Day",   
  12, 31, "New Years Eve", 
)

# PK: month & day

# 5: Draw a diagram illustrating the connections between the Batting, People, and Salaries data frames in the Lahman package. 
# Draw another diagram that shows the relationship between People, Managers, AwardsManagers. 
# How would you characterize the relationship between the Batting, Pitching, and Fielding data frames?

glimpse(Batting) # PK: playerID, yearID, stint
Batting |> 
  count(playerID, yearID, stint) |> 
  filter(n > 1)

glimpse(People) # PK: playerID
People |> 
  count(playerID) |> 
  filter(n > 1)

glimpse(Salaries) # PK: playerID, yearID, teamID
Salaries |> 
  count(playerID, yearID, teamID) |>
  filter(n > 1)
    
##Ejercicios 19.3.4 

#Find the 48 hours (over the course of the whole year) that have the worst delays. Cross-reference it with the weather data. Can you see any patterns?
flights |> 
  group_by(year, month, day, hour, origin) |> 
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE),
    .groups = "drop" 
  ) |> 
  arrange(desc(arr_delay)) |> 
  head(48) |> 
  left_join(weather, by = c("year", "month", "day", "hour", "origin"))
#muchos datos concentrados en meses como julio y agosto con humedad alta y mucho frio y nieve. El patron puede ser mucha niebla y tormentas de nieve que dificulta los vuelos.


#Ejercicio 2:
top_dest <- flights2 |>
  count(dest, sort = TRUE) |>
  head(10)

top_dest |>
  semi_join(flights2, join_by(dest == dest))

# Ejercicio 3
# Does every departing flight have corresponding weather data for that hour?
weather

flights2 |>
  left_join(weather)

# Ejercicio 4: What do the tail numbers that don’t have a matching record in planes have in common? 
flights2 
planes

# Ejercicio 5
#Crear un resumen de los aviones y sus aerolíneas
carrier_por_avion <- flights2 |> 
  filter(!is.na(tailnum)) |> 
  group_by(tailnum) |> 
  summarise(historial_carriers = paste(unique(carrier), collapse = ", ")) 

# Pegar esa columna a la tabla planes original
planes_actualizado <- planes |> 
  left_join(carrier_por_avion, join_by(tailnum))

planes_actualizado

# Ejercicio 6: Add the latitude and the longitude of the origin and destination airport to flights. 
# Is it easier to rename the columns before or after the join?

flights2
airports

flights2 |>
  left_join(airports, join_by(origin == faa)) |>  
  rename(origin_lat = lat, origin_lon = lon) |> 
  left_join(airports, join_by(dest == faa)) |> 
  rename(dest_lat = lat, dest_lon = lon) |> 
  select(origin, origin_lat, origin_lon, dest, dest_lat, dest_lon)
 # Es más fácil renombrar las columnas después del join. 

# Ejercicio 7: Compute the average delay by destination, then join on the airports data frame so you can show the spatial distribution of delays. Here’s an easy way to draw a map of the United States:
airports |>
  semi_join(flights, join_by(faa == dest)) |>
  ggplot(aes(x = lon, y = lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()


demoras_por_destino <- flights |> 
  group_by(dest) |> 
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE)) #demora promedio de la llegada al destino.


mapa_datos <- demoras_por_destino |> 
  inner_join(airports, join_by(dest == faa))


mapa_datos |> 
  ggplot(aes(x = lon, y = lat, color = avg_delay)) + 
  borders("state") +
  geom_point(size = 2, alpha = 0.8) + 
  coord_quickmap() # rta final.

# Ejercicio 8: What happened on June 13 2013? Draw a map of the delays, and then use Google to cross-reference with the weather.
flights |> 
  filter(month == 6 & day == 13) |> 
  group_by(dest) |> 
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE)) |> 
  inner_join(airports, join_by(dest == faa)) |> 
  ggplot(aes(x = lon, y = lat, color = avg_delay)) + 
  borders("state") +
  geom_point(size = 2, alpha = 0.8) + 
  coord_quickmap() # rta final.
# El 13 de junio de 2013 hubo una tormenta severa en el medio oeste de EEUU, lo que explica las demoras en esa zona.
