library(dplyr)
library(ggplot2)

airline.delays<- read.csv("/Users/devinrappe/Downloads/airline_delay.csv")
our.airline.data<- airline.delays %>% subset(select = -c(carrier_ct, weather_ct, 
nas_ct, security_ct, late_aircraft_ct, arr_del15))

clean.airline.data<- our.airline.data %>% rename(Year = year, Airline = carrier,
Airline.Name = carrier_name, Arrival.Delay = arr_delay, Month = month, Airport = airport, 
Airport.Name = airport_name, Arrival.Flights = arr_flights, 
Arrivals.Cancelled = arr_cancelled, Arrivals.Diverted = arr_diverted,
Carrier.Delays = carrier_delay, Weather.Delay = weather_delay, 
NAS.Delay = nas_delay, Late.Aircraft.Delay = late_aircraft_delay, 
Security.Delay = security_delay)

clean.airline.data <- clean.airline.data %>% mutate(Month.Name = as.character(Month))
clean.airline.data <- clean.airline.data %>% mutate(Month.Name = recode(Month.Name,
"5" =  "May", "1" = "January", "2" = "February", "3" = "March", "4" = "April","6" = "June", "7" = "July", "8" = "August", "9" = "September",  "10" = "October", "11" = "November", "12" = "December"))


