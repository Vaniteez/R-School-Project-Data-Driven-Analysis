d <- read.csv2("Airbus Project.csv")

#Data exploration and cleaning

summary(d)

d <- d[,-1]

# Upper case on "Customer.Type"

d$Customer.Type[d$Customer.Type=="disloyal Customer"]<-"Disloyal Customer"

colnames(d)
d$Arrival.Delay.in.Minutes <- as.integer(d$Arrival.Delay.in.Minutes)
hist(d$Arrival.Delay.in.Minutes)

# Calculate average ratings for each phase
d$Before.flight.avg <- rowMeans(d[, c('Ease.of.Online.booking', 'Departure.Arrival.time.convenient', 'Gate.location', 'Checkin.service', 'Online.boarding')], na.rm = TRUE)
d$After.flight.avg <- rowMeans(d[, c('Inflight.wifi.service', 'Food.and.drink', 'Seat.comfort', 'Inflight.entertainment', 'On.board.service', 'Leg.room.service', 'Inflight.service', 'Cleanliness', 'Baggage.handling')], na.rm = TRUE)
d$Total.flight.avg <- rowMeans(d[, c('Ease.of.Online.booking', 'Departure.Arrival.time.convenient', 'Gate.location', 'Checkin.service', 'Online.boarding', 'Inflight.wifi.service', 'Food.and.drink', 'Seat.comfort', 'Inflight.entertainment', 'On.board.service', 'Leg.room.service', 'Inflight.service', 'Cleanliness', 'Baggage.handling')], na.rm = TRUE)

hist(d$Total.flight.avg)
hist(d$satisfaction)

# View the updated dataframe
head(d)

#Create range of Age
# Create a new categorical variable for age ranges
d_clean$Range.Age <- cut(d$Age,
                   breaks = c(8, 18, 31, 61, 100),  
                   labels = c("Kids", "Young Adults", "Adults", "Senior"),
                   right = FALSE)  





# Define satisfaction levels
# Define satisfaction levels based on Total_Flight_avg
d$satisfaction.category <- cut(d$Total.flight.avg,
                                breaks = c(0, 2, 3, 4, 5),  # Specific thresholds
                                labels = c("Dissatisfied", "Neutral", "Satisfied", "Strongly Satisfied"),
                                right = FALSE)

# Verify the result
table(d$satisfaction.category)  # View count in each category
head(d[, c("Total.flight.avg", "satisfaction.category")])  # View the new column with averages

# Delete the original satisfaction column
d$satisfaction <- NULL

# Verify the deletion
head(d)

# We delete the missing values
d_clean <- na.omit(d)

#Data Analysis and finding what we want to search

d_numeric <- d[,c('Age','Flight.Distance','Inflight.wifi.service','Departure.Arrival.time.convenient','Ease.of.Online.booking','Gate.location','Online.boarding','Seat.comfort','On.board.service','Leg.room.service','Baggage.handling','Checkin.service','Inflight.service','Cleanliness','Departure.Delay.in.Minutes','Arrival.Delay.in.Minutes','Before.flight.avg','After.flight.avg','Total.flight.avg')]
cor_matrix <- cor(d_numeric)
subset_cor_matrix <- cor_matrix[c("Total.flight.avg"), c('Age','Flight.Distance','Inflight.wifi.service','Departure.Arrival.time.convenient','Ease.of.Online.booking','Gate.location','Online.boarding','Seat.comfort','On.board.service','Leg.room.service','Baggage.handling','Checkin.service','Inflight.service','Cleanliness','Departure.Delay.in.Minutes','Arrival.Delay.in.Minutes')]
print(subset_cor_matrix)

library(corrplot)
corrplot(subset_cor_matrix, method = "square")

## We want to know what are the main things that make our different types of customers more satisfied based on different criteria (Age, Gender, Class, Flight distance).
## Let's start with "Class"

table(d$Class)
prop.table(table(d$Class))
barplot(table(d$Class), main = "Class Distribution", xlab = "Class", ylab = "Count", col = "lightblue")

# chi square

library(dplyr)
chi_square <- table(d_clean$Class, d_clean$satisfaction.category)
print(chi_square)
chi_square_result <- chisq.test(chi_square)
print(chi_square_result)

## the pvalue is below 5% so its significant

# We start the model

d_clean$Class <- as.factor(d_clean$Class)
d_clean$Gender <- as.factor(d_clean$Gender)
d_clean$Age <- as.factor(d_clean$Age)

model_class <- multinom(satisfaction.category ~ Range.Age + Flight.Distance + Class + Gender+ Inflight.wifi.service + 
                    Departure.Arrival.time.convenient + Ease.of.Online.booking + 
                    Gate.location + Online.boarding + Seat.comfort + 
                    On.board.service + Leg.room.service + Baggage.handling + 
                    Checkin.service + Inflight.service + Cleanliness + 
                    Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes, data = d_clean)
summary(model_class)


# We run the model for the eco class

# Filter the dataset for Eco class
d_eco <- d_clean[d_clean$Class == "Eco", ]

model_class_eco <- multinom(satisfaction.category ~ Range.Age + Flight.Distance + Gender+ Inflight.wifi.service + 
                          Departure.Arrival.time.convenient + Ease.of.Online.booking + 
                          Gate.location + Online.boarding + Seat.comfort + 
                          On.board.service + Leg.room.service + Baggage.handling + 
                          Checkin.service + Inflight.service + Cleanliness + 
                          Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes, data = d_eco)
summary(model_class_eco)


## We shift to see which category of people are the most dissatisfied based on different factors, to target them and try to find insights to improve the ratings
# Age

library(ggplot2)

ggplot(d_clean, aes(x = Range.Age, fill = satisfaction.category)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Satisfaction Level by Age Range (Percentage)", x = "Age Range", y = "Percentage") +
  theme_minimal()

#Gender
ggplot(d_clean, aes(x = Gender, fill = satisfaction.category)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Satisfaction Level by Gender (Percentage)", x = "Gender", y = "Percentage") +
  theme_minimal()

ggplot(d_clean, aes(x = Gender, fill = satisfaction.category)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Satisfaction Level by Gender", x = "Gender", y = "Count") +
  theme_minimal()

#Class
ggplot(d_clean, aes(x = Class, fill = satisfaction.category)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Satisfaction Level by Class (Percentage)", x = "Class", y = "Percentage") +
  theme_minimal()

ggplot(d_clean, aes(x = Class, fill = satisfaction.category)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Satisfaction Level by Class", x = "Class", y = "Count") +
  theme_minimal()

#Customer Type
ggplot(d_clean, aes(x = Customer.Type, fill = satisfaction.category)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Satisfaction Level by Type of Customer (Percentage)", x = "Customer.Type", y = "Percentage") +
  theme_minimal()

ggplot(d_clean, aes(x = Customer.Type, fill = satisfaction.category)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Satisfaction Level by Type of Customer", x = "Customer.Type", y = "Count") +
  theme_minimal()

#Type of travel
ggplot(d_clean, aes(x = Type.of.Travel, fill = satisfaction.category)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Satisfaction Level by Type of Travel (Percentage)", x = "Type.of.Travel", y = "Percentage") +
  theme_minimal()

ggplot(d_clean, aes(x = Type.of.Travel, fill = satisfaction.category)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Satisfaction Level by Type of Travel", x = "Type.of.Travel", y = "Count") +
  theme_minimal()

# We also find that we can add the flight distance variable to our analysis and maybe changing this variable into a range of flight distance like we did for the Age variable
hist(d_clean$Flight.Distance,
     main = "Distribution of Flight Distance",
     xlab = "Kilometers",
     col = "lightblue",
     border = "black",
     breaks = 10)


d_clean$Range.of.Travel <- cut(d_clean$Flight.Distance,
                         breaks = c(0, 500, 1500, 3000, 10000),  
                         labels = c("Short", "Medium", "Long", "Extra-long"),
                         right = FALSE)  
summary(d_clean)
d_clean

ggplot(d_clean, aes(x = Range.of.Travel, fill = satisfaction.category)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Satisfaction Level by duration of travel (Percentage)", x = "Range of Travel", y = "Percentage") +
  theme_minimal()

ggplot(d_clean, aes(x = Range.of.Travel, fill = satisfaction.category)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Satisfaction Level by duration of travel", x = "Range of Travel", y = "Count") +
  theme_minimal()

### What insights we can extract from the last 2 plots is that the long and the extra-long travels (+ 1500km) performs well, the shorter flights are worse (short and medium so less than 1500km)
## We want to focus on the Class variable and then compute some of the variables that we found interesting, indeed with the previous plots we can see that the eco has more dissatisfied people in proportion and in overall values to the others. 
# We are also taking into consideration the neutral one because it corresponds to a rating between 2 and 3 out of 5 which is not really a good rating

library(ggplot2)
library(dplyr)

# Filter dataset for Economy class
d_eco <- d_clean %>% filter(Class == "Eco")

# Plot 1: Satisfaction by Age
plot_age <- ggplot(d_eco, aes(x = Range.Age, fill = satisfaction.category)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Satisfaction by Age in Eco Class", x = "Age Range", y = "Percentage") +
  theme_minimal()

# Plot 2: Satisfaction by Gender
plot_gender <- ggplot(d_eco, aes(x = Gender, fill = satisfaction.category)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Satisfaction by Gender in Eco Class", x = "Gender", y = "Percentage") +
  theme_minimal()

# Plot 3: Satisfaction by Flight Distance
plot_flight_distance <- ggplot(d_eco, aes(x = Range.of.Travel, fill = satisfaction.category)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Satisfaction by Flight Distance in Eco Class", x = "Flight Distance", y = "Percentage") +
  theme_minimal()

# Display the plots
plot_age
plot_gender
plot_flight_distance

## With the first 2 plots we didn't see so much insights, it was basically the same as our first plots but with the third plot we see something interesting, the longer flights are the one with more dissatisfied people and when we were looking in general it was the best performing travels
## We are going to focus on eco and long travel and get the proportion of every grades of every services and see which services impact the most for this category of travel

long_flights_eco <- d_clean %>% filter(Class == "Eco" & Range.of.Travel == "Long")


library(dplyr)
library(ggplot2)
library(tidyr) 

rating_columns <- c("Inflight.wifi.service", "Departure.Arrival.time.convenient",
                    "Ease.of.Online.booking", "Gate.location", "Food.and.drink",
                    "Online.boarding", "Seat.comfort", "Inflight.entertainment",
                    "On.board.service", "Leg.room.service", "Baggage.handling",
                    "Checkin.service", "Inflight.service", "Cleanliness")

long_flights_ratings <- long_flights_eco %>%
  select(all_of(rating_columns)) %>%
  pivot_longer(cols = everything(), names_to = "Service", values_to = "Rating") %>%
  group_by(Service, Rating) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Proportion = Count / sum(Count) * 14 * 100)

ggplot(long_flights_ratings, aes(x = Service, y = Proportion, fill = as.factor(Rating))) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(aes(label = paste0(round(Proportion, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Proportion of Ratings for Various Services (Long Flights, Eco Class)",
       x = "Service", y = "Proportion (%)", fill = "Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())


## Now our analysis is over and we can extract insights and recommendations from the last plot to see which services is the most important for the long flights of eco class



