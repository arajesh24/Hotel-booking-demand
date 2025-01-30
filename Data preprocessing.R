#########################################
# Install Necessary Packages
#########################################
install.packages("dplyr")
install.packages("skimr")
install.packages("DataExplorer")
install.packages("corrplot")
install.packages("caret")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("scales")
install.packages("gridExtra")
install.packages("mvoutlier")
install.packages("countrycode")
install.packages("stringr")
install.packages("caret")

#########################################
# Load the Necessary Libraries
#########################################
library(dplyr)
library(skimr)
library(DataExplorer)
library(corrplot)
library(caret)
library(lubridate)
library(ggplot2)
library(scales)
library(gridExtra)
library(mvoutlier)
library(countrycode)
library(stringr)
library(caret)

#########################################
# Dataset Loading:
#########################################
# Reads the original hotel booking demand dataset downloaded from Kaggle. 
hotel_data <- read.csv("hotel_bookings.csv", stringsAsFactors = FALSE)

#########################################
# Data Inspection:
## Reference : Lab 1 Data Exploration
#########################################
str(hotel_data)
summary(hotel_data)
head(hotel_data)
tail(hotel_data)
sapply(hotel_data, class)
is.numeric(hotel_data$adr)
##############################################################
# Data Cleaning and Outlier Removal
# Reference:Lab 03 Part A - Data preparation and cleaning in R
##############################################################
# Checking for duplicates
sum(duplicated(hotel_data))  

# Removing duplicates
hotel_data <- hotel_data[!duplicated(hotel_data), ]

# Checking missing data per column
colSums(is.na(hotel_data))

# Handle Missing Children Values
hotel_data$children[is.na(hotel_data$children)] <- 0

# Check how many rows have zero total guests
sum((hotel_data$adults + hotel_data$children + hotel_data$babies) == 0)

# Remove Zero-Guest Bookings
hotel_data <- hotel_data %>%
  filter(adults + children + babies > 0)

# (Insert code to check rows with only babies. Meaning zero adults and any no. of children. remove those rows)
# Check how many rows have zero adults but some children or babies
sum(hotel_data$adults == 0 & (hotel_data$children > 0 | hotel_data$babies > 0))

# Remove such rows
hotel_data <- hotel_data %>%
  filter(!(adults == 0 & (children > 0 | babies > 0)))

# (Insert code for checking negative ads)
# Check how many negative ADR values exist
sum(hotel_data$adr < 0)

# Remove Negative ADR Values as it doesn't make business sense
hotel_data <- hotel_data %>%
  filter(adr >= 0)

# Check how many rows have zero stay but are marked "Check-Out"
sum((hotel_data$stays_in_weekend_nights + hotel_data$stays_in_week_nights == 0) & 
      hotel_data$reservation_status == "Check-Out")

# Remove Contradictory Zero-Stay Check-Outs
hotel_data <- hotel_data %>%
  filter(!((stays_in_weekend_nights + stays_in_week_nights == 0) & reservation_status == "Check-Out"))

# Convert reservation_status_date to Date
hotel_data <- hotel_data %>%
  mutate(reservation_status_date = ymd(reservation_status_date))
str(hotel_data$reservation_status_date)

# Clean Agent and Company Columns as it has maximum missing values
hotel_data$agent[hotel_data$agent == "NULL"] <- NA
hotel_data$company[hotel_data$company == "NULL"] <- NA
if(!is.numeric(hotel_data$agent) && !all(is.na(hotel_data$agent))){
  hotel_data$agent <- as.numeric(hotel_data$agent)
}
if(!is.numeric(hotel_data$company) && !all(is.na(hotel_data$company))){
  hotel_data$company <- as.numeric(hotel_data$company)
}

# Dimensionality reduction for arrival_date
if(!is.character(hotel_data$arrival_date_month)) {
  hotel_data$arrival_date_month <- as.character(hotel_data$arrival_date_month)
}

hotel_data <- hotel_data %>%
  mutate(
    arrival_date_month_num = match(arrival_date_month, month.name),
    arrival_date = make_date(
      year = arrival_date_year,
      month = arrival_date_month_num,
      day = arrival_date_day_of_month
    )
  ) %>%
  select(-c(arrival_date_year, arrival_date_month, arrival_date_week_number, arrival_date_day_of_month, arrival_date_month_num))

# Clean and Format Country Column
hotel_data$country[hotel_data$country == "NULL"] <- NA

# Based on our research, The maximum number of guests that can be accommodated 
# in hotels in Portugal is 10. And that is just one hotel, the usual capacity 
# is usually 8 or less for family suites.
# So lets Remove rows where adults + children + babies > 8
hotel_data <- hotel_data[(hotel_data$adults + hotel_data$children + hotel_data$babies) <= 8, ]

# Changing the meal entries of Undefined to SC. As per the original Hotel
# Booking Demand Dataset from sciencedirect.com they both mean the same thing
# which is no meal package
print(unique(hotel_data$meal))
hotel_data <- hotel_data %>%
  mutate(meal = case_when(
    meal == "Undefined" ~ "SC",
    TRUE ~ meal
  ))

# Identify rows with "Undefined" in Market Segment or Distribution Channel
undefined_rows <- which(hotel_data$market_segment == "Undefined" | hotel_data$distribution_channel == "Undefined")
hotel_data[undefined_rows, ]

# Remove rows where Market Segment or Distribution Channel is "Undefined"
hotel_data <- hotel_data[!(hotel_data$market_segment == "Undefined" | hotel_data$distribution_channel == "Undefined"), ]

head(hotel_data)
str(hotel_data)

#########################################
# Summary of Completed Cleaning Steps
#########################################
# - Duplicate rows removed.
# - Missing children values handled (NAs replaced by 0).
# - Zero-guest bookings removed.
# - Rows with negative ADR values removed.
# - Contradictory zero-stay check-outs removed.
# - Agent and Company columns cleaned ("NULL" replaced with NA, converted to numeric if applicable).
# - Dimensionality reduction and date formatting applied to arrival date columns.
# - Country column cleaned ("NULL" replaced with NA).
# - Rows exceeding maximum guest capacity (adults + children + babies > 8) removed.
# - "Undefined" meal entries replaced with "SC" (no meal package).
# - Rows with "Undefined" in market_segment or distribution_channel removed.


#############################################################################################################
# Basic Exploratory Data Analysis (Statistical & Visualization) mostly based on target variable is_canceled 
# No of Visuals : 11

# Reference:Data Visualisation with R : 100 Examples. Springer; 2017.
# .Rahlf T.
# Oxford University Press, Incorporated, 2018. 
#############################################################################################################

# Distribution of Booking Cancellations
ggplot(hotel_data, aes(x = factor(is_canceled))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Booking Cancellations",
    x = "Cancelled (1) vs Not Cancelled (0)",
    y = "Number of Bookings"
  ) +
  theme_minimal()

# Cancellation Rate by Lead Time
hotel_data %>%
  mutate(lead_time_group = cut(
    lead_time, 
    breaks = c(0, 7, 30, 90, 180, Inf),
    labels = c("0-7 days", "8-30 days", "31-90 days", "91-180 days", "180+ days"),
    include.lowest = TRUE
  )) %>%
  mutate(lead_time_group = droplevels(lead_time_group)) %>%
  group_by(lead_time_group) %>%
  summarise(
    total_bookings = n(),
    cancellation_rate = mean(is_canceled) * 100
  ) %>%
  ggplot(aes(x = lead_time_group, y = cancellation_rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Cancellation Rate by Lead Time",
    x = "Lead Time",
    y = "Cancellation Rate (%)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Density Plot for Lead Time
ggplot(hotel_data, aes(x = lead_time, fill = factor(is_canceled))) +
  geom_density(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Density Plot of Lead Time by Cancellation Status",
    x = "Lead Time",
    y = "Density",
    fill = "Cancellation Status"
  )


# Distribution of Room Prices (ADR) by Cancellation Status
ggplot(hotel_data, aes(x = factor(is_canceled), y = adr)) +
  geom_boxplot(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Distribution of Room Prices (ADR) by Cancellation Status",
    x = "Cancelled (1) vs Not Cancelled (0)",
    y = "Average Daily Rate (ADR)"
  ) +
  coord_cartesian(ylim = c(0, 300))

# Density Plot for ADR
ggplot(hotel_data, aes(x = adr, fill = factor(is_canceled))) +
  geom_density(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Density Plot of ADR by Cancellation Status",
    x = "Average Daily Rate (ADR)",
    y = "Density",
    fill = "Cancellation Status"
  )

# Market segment analysis
market_segment_analysis <- hotel_data %>%
  group_by(market_segment) %>%
  summarise(
    total_bookings = n(),
    cancellation_rate = mean(is_canceled) * 100
  )

# Cancellation Rates by Market Segment
ggplot(market_segment_analysis, 
       aes(x = reorder(market_segment, cancellation_rate), 
           y = cancellation_rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Cancellation Rates by Market Segment",
    x = "Market Segment",
    y = "Cancellation Rate (%)"
  )

# Seasonal analysis
seasonal_analysis <- hotel_data %>%
  mutate(booking_month = format(arrival_date, "%B")) %>%
  group_by(booking_month, market_segment) %>%
  summarise(
    total_bookings = n(),
    cancellation_rate = mean(is_canceled) * 100,
    .groups = 'drop'
  ) %>%
  mutate(booking_month = factor(booking_month, levels = month.name))

# Cancellation Rates by Month and Market Segment
ggplot(seasonal_analysis, 
       aes(x = booking_month, y = cancellation_rate, fill = market_segment)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Cancellation Rates by Month and Market Segment",
    x = "Month",
    y = "Cancellation Rate (%)",
    fill = "Market Segment"
  )

# Deposit analysis
deposit_analysis <- hotel_data %>%
  group_by(deposit_type, market_segment) %>%
  summarise(
    total_bookings = n(),
    cancellation_rate = mean(is_canceled) * 100,
    avg_adr = mean(adr),
    .groups = 'drop'
  )

# Cancellation Rates by Deposit Type and Market Segment
ggplot(deposit_analysis, 
       aes(x = deposit_type, y = cancellation_rate, fill = market_segment)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Cancellation Rates by Deposit Type and Market Segment",
    x = "Deposit Type",
    y = "Cancellation Rate (%)",
    fill = "Market Segment"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Stay analysis
stay_analysis <- hotel_data %>%
  mutate(
    total_stays = stays_in_weekend_nights + stays_in_week_nights,
    stay_length_group = cut(total_stays, 
                            breaks = c(-1, 1, 3, 7, Inf),
                            labels = c("1 night", "2-3 nights", "4-7 nights", "8+ nights"))
  ) %>%
  group_by(stay_length_group, deposit_type) %>%
  summarise(
    total_bookings = n(),
    cancellation_rate = mean(is_canceled) * 100,
    avg_adr = mean(adr),
    .groups = 'drop'
  )

# Cancellation Rates by Length of Stay and Deposit Type
ggplot(stay_analysis, 
       aes(x = stay_length_group, y = cancellation_rate, fill = deposit_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Cancellation Rates by Length of Stay and Deposit Type",
    x = "Length of Stay",
    y = "Cancellation Rate (%)",
    fill = "Deposit Type"
  )

# Hotel type analysis
hotel_type_analysis <- hotel_data %>%
  mutate(
    total_stays = stays_in_weekend_nights + stays_in_week_nights,
    stay_length_group = cut(total_stays, 
                            breaks = c(-1, 1, 3, 7, Inf),
                            labels = c("1 night", "2-3 nights", "4-7 nights", "8+ nights"))
  ) %>%
  group_by(hotel, stay_length_group) %>%
  summarise(
    total_bookings = n(),
    cancellation_rate = mean(is_canceled) * 100,
    avg_adr = mean(adr),
    .groups = 'drop'
  )

# Cancellation Rates by Hotel Type and Length of Stay
ggplot(hotel_type_analysis, 
       aes(x = stay_length_group, y = cancellation_rate, fill = hotel)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Cancellation Rates by Hotel Type and Length of Stay",
    x = "Length of Stay",
    y = "Cancellation Rate (%)",
    fill = "Hotel Type"
  )

# Monthly hotel analysis
monthly_hotel_analysis <- hotel_data %>%
  mutate(booking_month = format(arrival_date, "%B")) %>%
  group_by(hotel, booking_month) %>%
  summarise(
    total_bookings = n(),
    cancellation_rate = mean(is_canceled) * 100,
    avg_lead_time = mean(lead_time),
    .groups = 'drop'
  ) %>%
  mutate(booking_month = factor(booking_month, levels = month.name))

# Monthly Cancellation Rates by Hotel Type
ggplot(monthly_hotel_analysis, 
       aes(x = booking_month, y = cancellation_rate, fill = hotel)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Monthly Cancellation Rates by Hotel Type",
    x = "Month",
    y = "Cancellation Rate (%)",
    fill = "Hotel Type"
  )

# Lead time & price analysis
lead_price_analysis <- hotel_data %>%
  mutate(
    lead_time_group = cut(lead_time, 
                          breaks = c(-1, 7, 30, 90, 180, Inf),
                          labels = c("0-7 days", "8-30 days", "31-90 days", 
                                     "91-180 days", "180+ days")),
    adr_group = cut(adr, 
                    breaks = quantile(adr, probs = seq(0, 1, 0.25)),
                    labels = c("Low", "Medium-Low", "Medium-High", "High"),
                    include.lowest = TRUE)
  ) %>%
  group_by(lead_time_group, adr_group, hotel) %>%
  summarise(
    total_bookings = n(),
    cancellation_rate = mean(is_canceled) * 100,
    .groups = 'drop'
  )

# Cancellation Rates by Lead Time, Price Range, and Hotel Type
ggplot(lead_price_analysis, 
       aes(x = lead_time_group, y = cancellation_rate, fill = adr_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~hotel) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Cancellation Rates by Lead Time, Price Range, and Hotel Type",
    x = "Lead Time",
    y = "Cancellation Rate (%)",
    fill = "Price Range"
  )

# Customer analysis
customer_analysis <- hotel_data %>%
  mutate(
    previous_experience = case_when(
      previous_bookings_not_canceled > 0 ~ "Returning Customer",
      previous_cancellations > 0 ~ "Previous Canceller",
      TRUE ~ "New Customer"
    )
  ) %>%
  group_by(customer_type, previous_experience, hotel) %>%
  summarise(
    total_bookings = n(),
    cancellation_rate = mean(is_canceled) * 100,
    avg_lead_time = mean(lead_time),
    avg_adr = mean(adr),
    .groups = 'drop'
  )

# Cancellation Rates by Customer Type and Previous Booking History
ggplot(customer_analysis, 
       aes(x = customer_type, y = cancellation_rate, fill = previous_experience)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~hotel) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Cancellation Rates by Customer Type and Previous Booking History",
    x = "Customer Type",
    y = "Cancellation Rate (%)",
    fill = "Previous Experience"
  )

# Density Plot for Previous Bookings Not Canceled
ggplot(hotel_data, aes(x = previous_bookings_not_canceled, fill = factor(is_canceled))) +
  geom_density(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Density Plot of Previous Bookings Not Canceled by Cancellation Status",
    x = "Previous Bookings Not Canceled",
    y = "Density",
    fill = "Cancellation Status"
  )

# Density Plot for Previous Cancellations
ggplot(hotel_data, aes(x = previous_cancellations, fill = factor(is_canceled))) +
  geom_density(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Density Plot of Previous Cancellations by Cancellation Status",
    x = "Previous Cancellations",
    y = "Density",
    fill = "Cancellation Status"
  )
#  Basic Exploratory Data Analysis is done.   

#############################################
# Outlier Detection using IQR
#############################################

p1 <- ggplot(hotel_data, aes(x = adr)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of ADR (All Values)",
       x = "Average Daily Rate",
       y = "Count")

p2 <- ggplot(hotel_data, aes(x = hotel, y = adr)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "ADR Distribution by Hotel Type",
       x = "Hotel Type",
       y = "Average Daily Rate") +
  coord_cartesian(ylim = c(0, 500))

grid.arrange(p1, p2, ncol = 2)

adr_outlier_analysis <- hotel_data %>%
  group_by(hotel) %>%
  summarise(
    mean_adr = mean(adr),
    median_adr = median(adr),
    Q1 = quantile(adr, 0.25),
    Q3 = quantile(adr, 0.75),
    IQR = Q3 - Q1,
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR,
    n_outliers = sum(adr > upper_bound | adr < lower_bound),
    pct_outliers = (n_outliers/n()) * 100
  )
print(adr_outlier_analysis)

high_adr_details <- hotel_data %>%
  filter(adr > 500) %>%
  select(hotel, adr, reserved_room_type, market_segment, 
         stays_in_weekend_nights, stays_in_week_nights) %>%
  arrange(desc(adr))

print("\nDetails of bookings with ADR > 500:")
print(high_adr_details)

# Remove extreme ADR outliers
hotel_data <- hotel_data %>%
  filter(adr <= 500)

verification <- summarise(hotel_data,
                          total_records = n(),
                          max_adr = max(adr)
)
print("Verification after removing high ADR records:")
print(verification)

###################################################################################
#  Removing Outlier with zero-ADR values
###################################################################################
zero_adr_analysis <- hotel_data %>%
  filter(adr == 0) %>%
  select(
    hotel,
    market_segment,
    customer_type,
    deposit_type,
    reserved_room_type,
    assigned_room_type,
    stays_in_weekend_nights,
    stays_in_week_nights,
    adults,
    children,
    babies,
    is_canceled
  ) %>%
  mutate(total_nights = stays_in_weekend_nights + stays_in_week_nights,
         total_guests = adults + children + babies)

print("Analysis of zero-ADR bookings")
print(summary(zero_adr_analysis))

zero_adr_details <- hotel_data %>%
  filter(adr == 0) %>%
  group_by(market_segment, customer_type) %>%
  summarise(
    count = n(),
    avg_total_guests = mean(adults + children + babies),
    max_guests = max(adults + children + babies),
    avg_nights = mean(stays_in_weekend_nights + stays_in_week_nights),
    cancellation_rate = mean(is_canceled) * 100,
    .groups = 'drop'
  ) %>%
  arrange(desc(count))

print("Distribution of zero-ADR bookings by market segment and customer type:")
print(zero_adr_details)
##########################################################################################################################
#  Handling Outlier(Special case): To keep only complementary and corporate zero ADRs and remove all other zero ADRs
##########################################################################################################################
# Examine bookings with zero ADR that are either Complementary or Corporate in market segment
zero_adr_corporate_comp <- hotel_data %>%
  filter(adr == 0 & market_segment %in% c("Complementary", "Corporate")) %>%
  group_by(market_segment) %>%
  summarise(count = n())

# Examine all bookings with Corporate distribution channel, regardless of market segment
corporate_distribution <- hotel_data %>%
  filter(distribution_channel == "Corporate") %>%
  group_by(market_segment) %>%
  summarise(count = n())

combined_analysis <- hotel_data %>%
  filter(
    (adr == 0 & market_segment %in% c("Complementary", "Corporate")) |
      (distribution_channel == "Corporate")
  ) %>%
  group_by(market_segment, distribution_channel) %>%
  summarise(
    count = n(),
    avg_adr = mean(adr),
    zero_adr_count = sum(adr == 0),
    .groups = 'drop'
  )

print("Bookings with zero ADR (Corporate/Complementary market segments):")
print(zero_adr_corporate_comp)

print("\nBookings with Corporate distribution channel:")
print(corporate_distribution)

print("\nCombined analysis:")
print(combined_analysis)

kept_rows <- hotel_data %>%
  mutate(keep_row = case_when(
    adr > 0 ~ TRUE,
    adr == 0 & market_segment %in% c("Complementary", "Corporate") ~ TRUE,
    adr == 0 & distribution_channel == "Corporate" ~ TRUE,
    TRUE ~ FALSE
  ))

hotel_data <- kept_rows %>%
  filter(keep_row == TRUE) %>%
  select(-keep_row)

###################################################################################
#  Analyzing and finding potential Outliers for lead_time column
##################################################################################
# Lead time analysis
lead_time_analysis <- hotel_data %>%
  summarise(
    min_lead_time = min(lead_time),
    q1_lead_time = quantile(lead_time, 0.25),
    median_lead_time = median(lead_time),
    mean_lead_time = mean(lead_time),
    q3_lead_time = quantile(lead_time, 0.75),
    max_lead_time = max(lead_time),
    iqr = IQR(lead_time),
    potential_outliers = sum(lead_time > (quantile(lead_time, 0.75) + 1.5 * IQR(lead_time)))
  )

print("Analysis of lead_time distribution:")
print(lead_time_analysis)

ggplot(hotel_data, aes(x = lead_time)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distribution of Lead Time",
    x = "Lead Time (days)",
    y = "Count"
  )

lead_time_patterns <- hotel_data %>%
  group_by(hotel, is_canceled) %>%
  summarise(
    avg_lead_time = mean(lead_time),
    med_lead_time = median(lead_time),
    max_lead_time = max(lead_time),
    n_bookings = n(),
    n_long_lead = sum(lead_time > (125 + 1.5 * 113)), 
    pct_long_lead = (n_long_lead / n_bookings) * 100,
    .groups = 'drop'
  )

print("Lead time patterns by hotel type and cancellation status:")
print(lead_time_patterns)

lead_time_by_segment <- hotel_data %>%
  group_by(market_segment) %>%
  summarise(
    avg_lead_time = mean(lead_time),
    med_lead_time = median(lead_time),
    max_lead_time = max(lead_time),
    n_long_lead = sum(lead_time > (125 + 1.5 * 113)),
    pct_bookings = n() / nrow(hotel_data) * 100,
    cancellation_rate = mean(is_canceled) * 100,
    .groups = 'drop'
  ) %>%
  arrange(desc(avg_lead_time))

print("\nLead time patterns by market segment:")
print(lead_time_by_segment)

ggplot(hotel_data, aes(x = market_segment, y = lead_time)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Lead Time Distribution by Market Segment",
    x = "Market Segment",
    y = "Lead Time (days)"
  )

# Calculate IQR bounds for lead_time and cap values exceeding the upper bound
Q1_lt <- quantile(hotel_data$lead_time, 0.25)
Q3_lt <- quantile(hotel_data$lead_time, 0.75)
IQR_lt <- Q3_lt - Q1_lt
upper_bound_lt <- Q3_lt + 1.5 * IQR_lt

# Cap lead_time at upper_bound_lt
# (If you cap, any lead_time above that is set to upper_bound_lt)
hotel_data$lead_time <- ifelse(hotel_data$lead_time > upper_bound_lt, upper_bound_lt, hotel_data$lead_time)

# Log transformation for days in waiting list since standard iqr results in 0
#since most of the rows are 0 and some rows have really high values

hotel_data <- hotel_data %>%
  mutate(
    days_in_waiting_list_log = log(days_in_waiting_list + 1)
  )

# Inspect the transformation
summary(hotel_data$days_in_waiting_list_log)

# Visualize the transformed data
ggplot(hotel_data, aes(x = days_in_waiting_list_log)) +
  geom_histogram(binwidth = 0.2, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(
    title = "Log-Transformed Distribution of Days in Waiting List",
    x = "Log(Days in Waiting List + 1)",
    y = "Frequency"
  )

# Density Plot for Log-Transformed Days in Waiting List
ggplot(hotel_data, aes(x = days_in_waiting_list_log, fill = factor(is_canceled))) +
  geom_density(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Density Plot of Log(Days in Waiting List) by Cancellation Status",
    x = "Log(Days in Waiting List)",
    y = "Density",
    fill = "Cancellation Status"
  )

# Density Plot for Booking Changes
ggplot(hotel_data, aes(x = booking_changes, fill = factor(is_canceled))) +
  geom_density(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Density Plot of Booking Changes by Cancellation Status",
    x = "Booking Changes",
    y = "Density",
    fill = "Cancellation Status"
  )

# Density Plot for Total Special Requests
ggplot(hotel_data, aes(x = total_of_special_requests, fill = factor(is_canceled))) +
  geom_density(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Density Plot of Total Special Requests by Cancellation Status",
    x = "Total Special Requests",
    y = "Density",
    fill = "Cancellation Status"
  )

hotel_data <- hotel_data %>%
  select(-days_in_waiting_list)

#################################################################################################
# Feature Engineering
# Reference : Feature Engineering and Selection: A Practical Approach for Predictive Models
# Max Kuhn (Author), Kjell Johnson (Author)
#################################################################################################

# 1. Create total_stay feature
# Provide visualization of total_stay with is_canceled
hotel_data <- hotel_data %>%
  mutate(total_stay = stays_in_weekend_nights + stays_in_week_nights)
View(hotel_data)
ggplot(hotel_data, aes(x = factor(is_canceled), y = total_stay)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Total Stay vs. Cancellation Status",
    x = "Cancelled (1) vs Not Cancelled (0)",
    y = "Total Stay (nights)"
  )

# First, let's calculate the quartiles and IQR for total_stay
q1_stay <- quantile(hotel_data$total_stay, 0.25)
q3_stay <- quantile(hotel_data$total_stay, 0.75)
iqr_stay <- q3_stay - q1_stay

# Calculate the upper bound using the standard 1.5 * IQR rule
upper_bound_stay <- q3_stay + 1.5 * iqr_stay

# Let's print these values to understand our data better
print(paste("Q1 (25th percentile):", q1_stay))
print(paste("Q3 (75th percentile):", q3_stay))
print(paste("IQR:", iqr_stay))
print(paste("Upper bound:", upper_bound_stay))

# Now let's see how many values will be capped
original_outliers <- sum(hotel_data$total_stay > upper_bound_stay)
print(paste("Number of stays that will be capped:", original_outliers))
print(paste("Percentage of stays that will be capped:", 
            round(original_outliers/nrow(hotel_data) * 100, 2), "%"))

# Cap the values
hotel_data$total_stay <- pmin(hotel_data$total_stay, upper_bound_stay)

# Density Plot for Total Stay
ggplot(hotel_data, aes(x = total_stay, fill = factor(is_canceled))) +
  geom_density(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Density Plot of Total Stay by Cancellation Status",
    x = "Total Stay (nights)",
    y = "Density",
    fill = "Cancellation Status"
  )

# 2. Create price_per_person feature
# Provide visualization of price_per_person with is_cancelled
hotel_data <- hotel_data %>%
  mutate(price_per_person = ifelse(
    (adults + children + babies) > 0,
    adr / (adults + children + babies),
    adr  # fallback if total guests is 0, though we've filtered out zero-guest rows
  ))

ggplot(hotel_data, aes(x = factor(is_canceled), y = price_per_person)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Price per Person vs. Cancellation Status",
    x = "Cancelled (1) vs Not Cancelled (0)",
    y = "Price per Person (ADR / total guests)"
  )

# Density Plot for Price per Person
ggplot(hotel_data, aes(x = price_per_person, fill = factor(is_canceled))) +
  geom_density(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Density Plot of Price per Person by Cancellation Status",
    x = "Price per Person (ADR / Total Guests)",
    y = "Density",
    fill = "Cancellation Status"
  )

# 3. Price category relative to market average
mean_adr_all <- mean(hotel_data$adr, na.rm = TRUE)
hotel_data <- hotel_data %>%
  mutate(price_category = case_when(
    adr > 1.5 * mean_adr_all ~ "premium",
    adr < 0.5 * mean_adr_all ~ "budget",
    TRUE ~ "standard"
  ))

# Create visualization of price category relative to market average
ggplot(hotel_data, aes(x = price_category, fill = price_category)) +
  geom_bar() +
  theme_minimal() +
  labs(
    title = "Distribution of Price Categories",
    x = "Price Category",
    y = "Count"
  )
##########################################################
# Discretization of lead_time column
##########################################################
# 4. Create lead time categories (booking_window)
hotel_data <- hotel_data %>%
  mutate(booking_window = case_when(
    lead_time <= 7 ~ "last_minute",
    lead_time <= 30 ~ "near_term",
    lead_time <= 90 ~ "medium_term",
    TRUE ~ "long_term"
  ))

# Visualization of lead time categories
ggplot(hotel_data, aes(x = booking_window, fill = booking_window)) +
  geom_bar() +
  theme_minimal() +
  labs(
    title = "Distribution of Lead Time Categories",
    x = "Lead Time Category",
    y = "Count"
  )
##############################################################
#Discretization
# 5. Create a seasonality feature with approximate seasons
# For simplicity, define:
# peak_season = Jun, Jul, Aug
# shoulder_season = Apr, May, Sep, Oct
# off_season = all other months
##############################################################
hotel_data <- hotel_data %>%
  mutate(month_num = month(arrival_date)) %>%
  mutate(season = case_when(
    month_num %in% c(6,7,8) ~ "peak_season",
    month_num %in% c(4,5,9,10) ~ "shoulder_season",
    TRUE ~ "off_season"
  ))

# Visualization of seasonality
ggplot(hotel_data, aes(x = season, fill = season)) +
  geom_bar() +
  theme_minimal() +
  labs(
    title = "Distribution of Bookings by Season",
    x = "Season",
    y = "Count"
  )

hotel_data <- hotel_data %>% select(-month_num)


room_type_stats <- hotel_data %>%
  group_by(reserved_room_type) %>%
  summarise(
    avg_price_per_person = mean(price_per_person, na.rm = TRUE),
    median_price_per_person = median(price_per_person, na.rm = TRUE),
    .groups = 'drop'
  )

# View the statistics by room type
print(room_type_stats)

# Calculate overall quartiles for average price_per_person
quartiles <- quantile(room_type_stats$avg_price_per_person, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

Q1 <- quartiles[1]  # 25th percentile
Q2 <- quartiles[2]  # 50th (median)
Q3 <- quartiles[3]  # 75th percentile
######################################################################################
# Discretization of reserved_room_type column
#######################################################################################
# Create a function to categorize based on these quartiles
categorize_room_type <- function(x) {
  case_when(
    x <= Q1 ~ "Economy",
    x <= Q2 ~ "Standard",
    x <= Q3 ~ "Premium",
    TRUE    ~ "Ultra-Premium"
  )
}

# Add a category column to your summary table
room_type_stats <- room_type_stats %>%
  mutate(
    room_price_category = categorize_room_type(avg_price_per_person)
  )

print(room_type_stats)

# Merge room_type_stats onto hotel_data by reserved_room_type
hotel_data <- hotel_data %>%
  left_join(room_type_stats %>% select(reserved_room_type, room_price_category),
            by = "reserved_room_type")

# Drop the reserved_room_type column
hotel_data <- hotel_data %>%
  select(-reserved_room_type)  

##########################################################################################
# Derivation of new field continent from existing columns country
##########################################################################################
#Reducing dimensionality for country
country_counts <- hotel_data %>%
  count(country, sort = TRUE)

head(country_counts, 20)  # See the top 20 countries

hotel_data <- hotel_data %>%
  mutate(
    continent = countrycode(sourcevar = country,
                            origin = "iso3c",
                            destination = "continent",
                            warn = FALSE)
  )

sum(is.na(hotel_data$continent))

# For NA values or unknown codes, optionally set them to "Other"
hotel_data$continent[is.na(hotel_data$continent)] <- "Other"

#Drop the country coloumn
hotel_data <- hotel_data %>%
  select(-country)
#Drop stays in weekend and week nights since we have total stays to simplify 
#feature space
hotel_data <- hotel_data %>%
  select(-stays_in_weekend_nights, -stays_in_week_nights)

######################################################################################
# Discretization of guest_composition column
#######################################################################################
# Make guest composition feature
hotel_data <- hotel_data %>%
  mutate(
    guest_composition = case_when(
      adults == 1 & children == 0 & babies == 0 ~ "Solo Traveler",
      adults == 2 & children == 0 & babies == 0 ~ "Couple",
      adults == 2 & children >= 1 & children <= 2 & babies == 0 ~ "Small Family",
      adults == 2 & children >= 3 & children <= 4 & babies == 0 ~ "Large Family",
      adults == 2 & children >= 1 & babies >= 1 ~ "Family with Babies",
      TRUE ~ "Other"  # Catch-all for any other combinations
    )
  )

# Check the distribution of guest_composition
guest_comp_distribution <- table(hotel_data$guest_composition)
print("Distribution of Guest Composition:")
print(guest_comp_distribution)

hotel_data <- hotel_data %>%
  select(-adults, -babies, -children)



# Step 1: Create binary features from agent and company first
# We do this first because we'll need these columns before we drop them
hotel_analysis <- hotel_data %>%
  mutate(
    # Convert NA values to 0 and present values to 1 for both agent and company
    has_agent = ifelse(!is.na(agent), 1, 0),
    has_company = ifelse(!is.na(company), 1, 0)
  )

# Step 2: Drop columns that contain data leakage or have been transformed
# These columns either contain post-booking information or have been converted to other formats
hotel_analysis <- hotel_analysis %>%
  select(-c(
    "reservation_status",     
    "reservation_status_date", 
    "assigned_room_type",      
    "agent",                   
    "company"                 
    
  ))
######################################################################################
# Feature and Selection & Feature Encoding
######################################################################################

# Step 3: Convert categorical columns to factors
# This step is important because factors preserve the categorical nature of the data
# and ensure proper encoding in subsequent steps
# Define the vector of columns that need to become factors
cat_columns <- c(
  "hotel",
  "meal",
  "continent",
  "market_segment",
  "distribution_channel",
  "deposit_type",
  "customer_type",
  "price_category",
  "booking_window",
  "season",
  "room_price_category",
  "guest_composition"
)

hotel_analysis <- hotel_analysis %>%
  mutate(across(all_of(cat_columns), as.factor))

str(hotel_analysis)

#Cylical encoding for month and drop arrival date
hotel_analysis <- hotel_analysis %>%
  mutate(
    # Extract numeric month (1 through 12)
    arrival_month = month(arrival_date),
    
    # Cyclical (sine) encoding for the month
    month_sin = sin(2 * pi * (arrival_month - 1) / 12),
    
    # Cyclical (cosine) encoding for the month
    month_cos = cos(2 * pi * (arrival_month - 1) / 12)
  ) %>%
  # Drop arrival_date now that we've extracted what we need
  select(-arrival_date, 
         -arrival_month)

# First, identify the numerical columns we want to scale
numeric_features <- c(
  # Time-based
  "lead_time",
  
  # Count features
  "previous_cancellations",
  "previous_bookings_not_canceled", 
  "booking_changes",
  "total_of_special_requests",
  "total_stay",
  
  # Price features
  "adr",
  "price_per_person"
)

# Create the binary column 'has_car_parking'
hotel_analysis <- hotel_analysis %>%
  mutate(
    has_car_parking = if_else(
      required_car_parking_spaces > 0,
      1,
      0
    )
  )

hotel_analysis <- hotel_analysis %>%
  select(-required_car_parking_spaces)

# Remove rows where total_stay is 0
hotel_analysis <- hotel_analysis %>%
  filter(total_stay != 0)

# Create the MinMax scaling function
minmax_scale <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Apply scaling directly to the columns
hotel_analysis[numeric_features] <- lapply(hotel_analysis[numeric_features], minmax_scale)

# Let's verify our scaling worked by checking the ranges
# All values should now be between 0 and 1
summary(hotel_analysis[numeric_features])

# Identify categorical (factor) columns
categorical_cols <- hotel_analysis %>%
  select(where(is.factor)) %>%
  names()

# Print the categorical columns
print("Categorical Columns to One-Hot Encode:")
print(categorical_cols)

# Create a formula that includes all categorical columns
formula <- as.formula(paste("~", paste(categorical_cols, collapse = " + "), "-1"))

# Generate the one-hot encoded matrix
one_hot_encoded_matrix <- model.matrix(formula, data = hotel_analysis)

# Convert the matrix to a dataframe
one_hot_encoded_df <- as.data.frame(one_hot_encoded_matrix)


colnames(one_hot_encoded_df) <- str_replace_all(colnames(one_hot_encoded_df), " ", "_")

# Identify numeric columns
numeric_cols <- hotel_analysis %>%
  select(where(is.numeric)) %>%
  names()

# Print the numeric columns
print("Numeric Columns:")
print(numeric_cols)

# Extract numeric data
numeric_data <- hotel_analysis %>%
  select(all_of(numeric_cols))

# Combine numeric data with one-hot encoded categorical data
final_dataset <- cbind(numeric_data, one_hot_encoded_df)

# Distribution of Booking Cancellations
ggplot(final_dataset, aes(x = factor(is_canceled))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Booking Cancellations",
    x = "Cancelled (1) vs Not Cancelled (0)",
    y = "Number of Bookings"
  ) +
  theme_minimal()

str(final_dataset)

# Verify that all columns are numeric
sapply(final_dataset, is.numeric)

sum(is.na(final_dataset))

# 1. Compute the correlation matrix
cor_matrix <- cor(final_dataset, use = "complete.obs", method = "pearson")

# Define the output file with increased dimensions
png(filename = "enhanced_correlation_matrix.png", 
    width = 4000, height = 4000, res = 300) 
######################################################################################
# Feature Encoding
######################################################################################

corrplot(cor_matrix, 
         method = "color",        
         type = "full",           
         order = "original",      
         tl.col = "black",        
         tl.srt = 45,             
         tl.cex = 0.5,            
         addCoef.col = "black",   
         number.cex = 0.4,        
         diag = FALSE,            
         mar = c(2,2,2,2))        

# Close the PNG device to save the file
dev.off()

cat("Enhanced correlation matrix saved as 'enhanced_correlation_matrix.png'.\n")

# Check if 'is_canceled' exists in the dataset
if(!"is_canceled" %in% colnames(cor_matrix)){
  stop("'is_canceled' is not a column in the correlation matrix.")
}

# Extract correlations with 'is_canceled'
cor_with_target <- cor_matrix["is_canceled", ]

# Remove the correlation of 'is_canceled' with itself
cor_with_target <- cor_with_target[names(cor_with_target) != "is_canceled"]

# Order the variables based on their correlation with 'is_canceled'
ordered_features <- names(sort(abs(cor_with_target), decreasing = TRUE))

# Reorder the correlation matrix
cor_matrix_ordered <- cor_matrix[ordered_features, ordered_features]

# Define the output file with increased dimensions
png(filename = "ordered_correlation_matrix.png", 
    width = 6000, height = 6000, res = 300) 
# Create the correlation plot with adjusted parameters
corrplot(cor_matrix_ordered, 
         method = "color",        
         type = "full",           
         order = "original",      
         tl.col = "black",        
         tl.srt = 45,             
         tl.cex = 0.5,            
         addCoef.col = "black",   
         number.cex = 0.4,        
         diag = FALSE,            
         mar = c(2,2,2,2))       

# Close the PNG device to save the file
dev.off()

cat("Ordered correlation matrix saved as 'ordered_correlation_matrix.png'.\n")

write.csv(final_dataset, "Unbalanced_hotel_data.csv", row.names = FALSE)

# Check class distribution
table(final_dataset$is_canceled)

# Separate features and target
features <- final_dataset %>% select(-is_canceled)
target <- final_dataset$is_canceled

# Convert target to a factor
target <- as.factor(target)

# Verify the conversion
str(target)

######################################################################################
# perform balancing of dataset
######################################################################################
set.seed(123)  # For reproducibility
undersampled_data <- downSample(x = features, y = target, yname = "is_canceled")

# Check the new class distribution
table(undersampled_data$is_canceled)

# Calculate the number of missing values per column
missing_counts <- sapply(undersampled_data, function(x) sum(is.na(x)))

# Convert to a data frame
missing_values_df <- data.frame(
  Variable = names(missing_counts),
  Missing_Count = as.integer(missing_counts)
)

# Display columns with missing values
missing_values_df %>%
  filter(Missing_Count > 0)

write.csv(undersampled_data, "Balanced_hotel_data.csv", row.names = FALSE)