library(tidyverse)
library(janitor)

data <- read_csv2("data/smoke_test/2023-11-10_15-34-52.csv") |> 
  clean_names()

sensor_levels <- c(
  "back_temperature_c",
  "front_temperature_o_c",
  "top_temperature_c",
  "ir_temperature_c",
  "thermocouple_temperature_c",
  "outside_temperature_c",
  "outside_humidity_percent",
  "inside_humidity_percent"
)

sensor_labels <- c(
  "Temperature Sensor (back)",
  "Temperature Sensor (front)",
  "Temperature Sensor (top)",
  "IR Temperature Sensor",
  "Thermocouple Temperature Sensor",
  "Temperature Sensor (outside)",
  "Humidity Sensor (outside)",
  "Humidity Sensor (inside)"
)

# Make the data long instead of wide (see "tidy data")
data <- 
  data |> 
  mutate(time = lubridate::dmy_hm(time)) |> 
  
  # Create a version down to the hour (if needed for hourly plots)
  mutate(time_hour = floor_date(time, unit ="hours")) |>
  
  # Transform sensor variables into a "sensor_name" / "value" setup (wide to long data)
  pivot_longer(cols = 
                 c(
                   back_temperature_c,
                   outside_temperature_c,
                   front_temperature_o_c,
                   top_temperature_c,
                   ir_temperature_c,
                   thermocouple_temperature_c,
                   outside_humidity_percent,
                   inside_humidity_percent
                   ),
               names_to = "sensor_name",
               values_to = "value"
               ) |> 
  
  mutate(sensor_name = factor(sensor_name, levels=sensor_levels, labels=sensor_labels))

# Take a peak at the data
data |> 
  glimpse()


data |> 
  # Filter on specific sensors for this plot
  filter(sensor_name %in% c(
    "Temperature Sensor (back)",
    "Temperature Sensor (top)",
    "Temperature Sensor (front)",
    "IR Temperature Sensor",
    "Temperature Sensor (outside)"
    )) |>
  drop_na() |> 

  # Create a smoothed line plot (regular line is to noisy)  
  ggplot() +
  aes(x=time, y = value, color=sensor_name) +
  geom_smooth() +
  labs(x = "Time", y = "Temperature in °C") +
  scale_color_viridis_d() +
  theme_bw()
