#Q1a

# Load the required libraries
library(readr)
library(dplyr)

# Load the dataframe, which has been previously cleaned
Question_1 <- read_csv("Question 1.csv")

# View the dataframe to see how it looks
View(Question_1)

# Group the data into 6-month periods
Question_1 <- mutate(Question_1, Period = cut(REF_DATE, breaks = "6 months"))

Q1a <- Question_1 %>%
  filter(Rates %in% c("Bank rate", "Chartered bank administered interest rates - Prime rate"))

# Calculate the average bank rate and prime rate for each period
average_rates <- Q1a %>%
  group_by(Rates, Period) %>%
  summarise(Avg_Bank_Rate = mean(VALUE))

# Display the results
View(average_rates)


#Q1b

# Calculate the difference in days between consecutive rate changes for each type of rate
Question_1 <- Question_1 %>%
  group_by(Rates) %>%
  mutate(Days_Between_Changes = c(0, diff(REF_DATE))) %>%
  filter(Days_Between_Changes != 0)

# Calculate the average rate change period in days for each type of rate
average_change_period <- Question_1 %>%
  group_by(Rates) %>%
  summarise(Avg_Change_Period = mean(Days_Between_Changes))

# Display the results
print(average_change_period)


#Q1c

# Filter out non-bank rates
bank_rates <- Question_1 %>%
  filter(Rates == "Bank rate")

# Count the frequency of each bank rate
unique_bank_rate <- bank_rates %>%
  group_by(VALUE) %>%
  summarise(Frequency = n())

# Display the results
print(unique_bank_rate)


#Q1d

# Arrange the dataframe in descending order
unique_bank_rate <- unique_bank_rate %>%
  arrange(desc(Frequency))

# Display the results
print(unique_bank_rate)


#################################################################

#Q2a

# Load the required libraries
library(dplyr)
library(tidyr)

# Load the dataframe, which has been previously cleaned
Question_2 <- read_csv("Question 2.csv")

# View the dataframe to see how it looks
View(Question_2)

# Filter data for the relevant provinces
Q2a <- Question_2 %>%
  filter(GEO %in% c("Ontario", "Alberta", "British Columbia", "Canada"),
         Labour_force_characteristics %in% c("Population", "Full-time employment", "Part-time employment", "Unemployment"),
         REF_DATE %in% c("Jan-24", "Jan-23"))

# Calculate growth rates
growth_rates <- Q2a %>%
  group_by(GEO, Labour_force_characteristics) %>%
  summarise(Growth_Rate = ((VALUE[REF_DATE == "Jan-24"] - VALUE[REF_DATE == "Jan-23"]) / VALUE[REF_DATE == "Jan-23"]) * 100)

# Display the results
print(growth_rates)


#Q2b

# Filter the required data for analysis
Q2b <- Question_2 %>%
  filter(Labour_force_characteristics %in% c("Part-time employment", "Labour force"),
         REF_DATE == "Jan-24")

# Calculate Part-time employment rate as a percentage of the labor force
part_time_rate <- Q2b %>%
  group_by(GEO) %>%
  mutate(Part_time_Employment_Rate = (sum(VALUE[Labour_force_characteristics == "Part-time employment"]) / 
                                      sum(VALUE[Labour_force_characteristics == "Labour force"])) * 100) %>%
  select(GEO, Part_time_Employment_Rate) %>%
  arrange(desc(Part_time_Employment_Rate))

# Display the results
print(part_time_rate)


#Q2c

# Filter the required data for analysis
Q2c <- Question_2 %>%
  filter(Labour_force_characteristics %in% c("Employment rate", "Unemployment rate"),
         REF_DATE == "Jan-24")

# Make a dataframe with the requested information
employment_unemployment_rate <- Q2c %>%
 group_by(GEO) %>%
  select(GEO, Labour_force_characteristics, VALUE) %>%
  arrange(GEO)

# Display the results
print(employment_unemployment_rate)
