library(readr)
library(dbplyr)
library(lubridate)
library(ggplot2)
Border_Crossing_Entry_Data <- read_csv("Border_Crossing_Entry_Data.csv")
View(Border_Crossing_Entry_Data)
head(Border_Crossing_Entry_Data)

####Exploring border data

pedestrian_data <- Border_Crossing_Entry_Data %>%
  filter(Measure == "Pedestrians")
print(pedestrian_data)
View(pedestrian_data)

##### Only pedestrian crossing through texas

texas_pedestrian_data <- pedestrian_data %>%
  filter(State == "Texas")
print(texas_pedestrian_data)

#### Order from the past to nowadays

texas_pedestrian_data <- texas_pedestrian_data %>%
  mutate(Date = my(Date))
ordered_data <- texas_pedestrian_data %>%
  arrange(Date)
print(ordered_data)

###Isolate "El Paso"

el_paso_data <- ordered_data %>%
  filter(`Port Name` == "El Paso")
print(el_paso_data)

###Plot trends in El Paso

ggplot(el_paso_data, aes(x = Date, y = Value)) +
  geom_line(color = "blue") +  
  geom_point(color = "red") +   
  labs(title = "People crossing through El Paso",
       x = "Date",
       y = "Value") +
  theme_minimal()

#### ORDER obtaining average by month

el_paso_data <- el_paso_data %>%
  mutate(Date = as.Date(Date)) 

monthly_avg <- el_paso_data %>%
  group_by(Year = year(Date), Month = month(Date, label = TRUE)) %>%  
  summarize(Average_Value = mean(Value, na.rm = TRUE)) %>%  
  ungroup()  

print(monthly_avg)




##### Now plot average by month

el_paso_data <- el_paso_data %>%
  mutate(Date = as.Date(Date))

monthly_avg <- el_paso_data %>%
  group_by(Year = year(Date), Month = month(Date)) %>%  
  summarize(Average_Value = mean(Value, na.rm = TRUE)) %>%  
  ungroup()

monthly_avg <- monthly_avg %>%
  mutate(Month_Year = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d")) 

ggplot(monthly_avg, aes(x = Month_Year, y = Average_Value)) +
  geom_line(color = "blue") +  
  geom_point(color = "red") +   
  labs(title = "Average Values by month in El Paso",
       x = "Date",
       y = "Average value") +
  scale_x_date(date_labels = "%b %Y") +  
  theme_minimal() 