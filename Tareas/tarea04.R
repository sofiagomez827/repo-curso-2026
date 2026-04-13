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

?select

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
