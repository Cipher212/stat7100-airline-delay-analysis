# setwd() removed - run from the repo root


# Preface

# I have tried to keep the code to mostly base R, but I did have to resort to
# ggplot2, dplyr in certain cases to keep the code simple and make the graphs 
# look better


# Rough Working Section (Please ignore this section)

# AIRLINE COLOURS
# 1. Colors
#airline_colors <- c(
 # "American Airlines"   = "#E41A1C", 
 # "Delta Air Lines"     = "#377EB8", 
 # "United Airlines"     = "#4DAF4A", 
 # "Southwest Airlines"  = "#984EA3", 
 # "Alaska Airlines"     = "#FF7F00", 
 # "JetBlue Airways"     = "#FFFF33", 
 # "Spirit Airlines"     = "#A65628", 
 # "Frontier Airlines"   = "#F781BF", 
 # "Hawaiian Airlines"   = "#999999", 
 # "Allegiant Air"       = "#000000"  )





airdata_raw <- read.csv("Airline_Delay_Cause.csv", stringsAsFactors = FALSE)
test <- sum(airdata_raw$arr_flights, na.rm = TRUE)

dim(airdata_raw) # 398233 obs. of  21 variables
str(airdata_raw)  
head(airdata_raw)
summary(airdata_raw)
airdata_raw <- na.omit(airdata_raw)
sd(airdata_raw$arr_del15)
sd(airdata_raw$arr_delay)
mean(airdata_raw$arr_delay)
mean(airdata_raw$arr_del15)





max_delay_row <- airdata_raw[which.max(airdata_raw$arr_delay), ]
max_delay_row

# Airline and Carrier Code Count

table_carrier <- table(airdata_raw$carrier)
table_carriername <- table(airdata_raw$carrier_name)
length(table_carrier)
length(table_carriername)




# Total Flights per carrier
flight_totals <- aggregate(arr_flights ~ carrier_name, data = airdata_raw, FUN = sum)
flight_totals_sorted <- flight_totals[order(flight_totals$arr_flights, decreasing = TRUE), ]
print(flight_totals_sorted)


#Airline Service Span
min_years <- aggregate(year ~ carrier_name, data = airdata_raw, FUN = min)

max_years <- aggregate(year ~ carrier_name, data = airdata_raw, FUN = max)



# End of Rough Working Section
#=========================================================================================================================



# Data Cleaning Steps. 

# Main Issues:
# Compiling 51 reporting airlines to their respective marketing carrier
# Minor discrepancy in airport name vs airport code in some rows
# Omit negative delays. They appear to be an error in collection



airdata_raw$marketing_carrier <- airdata_raw$carrier_name


# American Airlines Group
AA_group<-c("American Airlines Inc.", "American Airlines Network", "Envoy Air", "Piedmont Airlines",
            "PSA Airlines Inc.", "US Airways Inc.", "America West Airlines Inc.", "American Eagle Airlines Inc.")

# Add to marketing_carrier
airdata_raw$marketing_carrier[airdata_raw$marketing_carrier %in% AA_group] <- "American Airlines"


# Delta Airlines
DAL_group <- c("Delta Air Lines Inc.", "Delta Air Lines Network", "Endeavor Air Inc.", 
               "Northwest Airlines Inc.", "Comair Inc.", "Pinnacle Airlines Inc.", 
               "Atlantic Southeast Airlines") # Major former regional partner
airdata_raw$marketing_carrier[airdata_raw$marketing_carrier %in% DAL_group] <- "Delta Air Lines"

# United Airlines
UA_group <- c("United Air Lines Inc.", "United Air Lines Network", 
              "Continental Air Lines Inc.")
airdata_raw$marketing_carrier[airdata_raw$marketing_carrier %in% UA_group] <- "United Airlines"

# Alaska Airlines
AS_group <- c("Alaska Airlines Inc.", "Alaska Airlines Network", 
              "Horizon Air", "Virgin America")
airdata_raw$marketing_carrier[airdata_raw$marketing_carrier %in% AS_group] <- "Alaska Airlines"

# Southwest
SW_group <- c("Southwest Airlines Co.", "Southwest Airlines", 
              "AirTran Airways Corporation")
airdata_raw$marketing_carrier[airdata_raw$marketing_carrier %in% SW_group] <- "Southwest Airlines"


# Frontier
FA_group <- c("Frontier Airlines Inc.", "Frontier Airlines")
airdata_raw$marketing_carrier[airdata_raw$marketing_carrier %in% FA_group] <- "Frontier Airlines"

# Spirit
SA_group <- c("Spirit Air Lines", "Spirit Airlines")
airdata_raw$marketing_carrier[airdata_raw$marketing_carrier %in% SA_group] <- "Spirit Airlines"

# Hawaian 
HA_group <- c("Hawaiian Airlines Inc.", "Hawaiian Airlines Network")
airdata_raw$marketing_carrier[airdata_raw$marketing_carrier %in% HA_group] <- "Hawaiian Airlines"




# NOTE: JetbBlue and Allegient is already sorted as required. Skipped.


# Vector of all marketing carriers

marketing_carrier_list <- c(
  "American Airlines",
  "Delta Air Lines",
  "United Airlines",
  "Southwest Airlines",
  "Alaska Airlines",
  "JetBlue Airways",
  "Spirit Airlines",
  "Frontier Airlines",
  "Hawaiian Airlines",
  "Allegiant Air"
)

# Add to dataset:

airdata_clean <- airdata_raw[airdata_raw$marketing_carrier %in% marketing_carrier_list,]
airdata_clean$marketing_carrier <- factor(airdata_clean$marketing_carrier)

# delete carrier_name 
airdata_clean$carrier_name <- NULL



flight_totals <- aggregate(arr_flights ~ marketing_carrier, data = airdata_clean, FUN = sum)
flight_totals_sorted <- flight_totals[order(flight_totals$arr_flights, decreasing = TRUE), ]
print(flight_totals_sorted)

#marketing_carrier       arr_flights
#   American Airlines     31175293
#   Southwest Airlines    28248672
#   Delta Air Lines       26333133
#   United Airlines       13639313
#   Alaska Airlines       4965086
#   JetBlue Airways       4720711
#   Frontier Airlines     2161763
#   Spirit Airlines       1894520
#   Hawaiian Airlines     1437315
#   Allegiant Air         773860



# Matching airport names to airport code


unique_airport_pairs <- unique(airdata_clean[c("airport", "airport_name")])

length(unique_airport_pairs$airport) # 373


# Shifting to table 
airport_code_counts <- table(unique_airport_pairs$airport)
airport_code_counts

# If count is greater than 1. Means a single code is corresponding to more than one name


problem_codes <- names(airport_code_counts[airport_code_counts > 1])
problem_pairs <- unique_airport_pairs[unique_airport_pairs$airport %in% problem_codes, ]



canonical_names <- unique_airport_pairs[!duplicated(unique_airport_pairs$airport), ]


airdata_clean$airport_name <- canonical_names$airport_name[ match(airdata_clean$airport, canonical_names$airport) ]

# Check code worked
airport_code_test <- unique(airdata_clean[c("airport", "airport_name")])
any(duplicated(airport_code_test$airport))

# Negative Delays

negative_causes <- sum(
airdata_clean$arr_delay <0,
airdata_clean$arr_del15 <0,
airdata_clean$carrier_delay < 0,
airdata_clean$weather_delay < 0,
airdata_clean$nas_delay < 0, 
airdata_clean$security_delay < 0, 
airdata_clean$late_aircraft_delay < 0, 
na.rm = TRUE)

print(negative_causes)

# 2 Rows where delays are negative. Due to their insignificance, we will remove

airdata_clean <- subset(
airdata_clean, 
carrier_delay >= 0 & 
weather_delay >= 0 & 
nas_delay >= 0 & 
security_delay >= 0 & 
late_aircraft_delay >= 0 &
arr_delay >= 0 &
arr_del15 >= 0)




summary(airdata_clean$marketing_carrier)
length(airdata_clean$marketing_carrier)



# We are only working for the years 2018-2025 as this is the time period
# where all marketing carriers have flight data (for this dataset)


airdata <- subset(airdata_clean, year >= 2018)


#_______________________________________________________________________________________________________________




# Part 1: Historical Trend Analysis 

library(ggplot2) 
# Usage: Graphing

library(ggtext) 
# Usage: experimenting with adding airline logos as pictures to the graphs

# I planned to use dplyr but project was doable with Base R only


# Aggregate delay for Marketing Carriers
agg_delay <- aggregate(arr_delay ~ marketing_carrier + year, data = airdata, FUN = sum)

# Total Flights for each carrier
agg_flights <- aggregate(arr_flights ~ marketing_carrier + year, data = airdata, FUN = sum, na.rm = TRUE)

# Merge tables
annual_trends <- merge(agg_delay, agg_flights, by = c("marketing_carrier", "year"))

# Calculate the average delay per flight
annual_trends$avg_delay <- annual_trends$arr_delay / annual_trends$arr_flights




# Aircraft Logos (ggtext) and Colours

# 1. Colors for Graph
airline_colors <- c(
  "American Airlines"   = "#E41A1C", # Red
  "Delta Air Lines"     = "#377EB8", # Blue
  "United Airlines"     = "#4DAF4A", # Green
  "Southwest Airlines"  = "#984EA3", # Purple
  "Alaska Airlines"     = "#FF7F00", # Orange
  "JetBlue Airways"     = "#FFFF33", # Yellow
  "Spirit Airlines"     = "#A65628", # Brown
  "Frontier Airlines"   = "#F781BF", # Pink
  "Hawaiian Airlines"   = "#999999", # Grey
  "Allegiant Air"       = "#000000"  # Black
)



# Plotting The Graph

trend_plot <- ggplot(annual_trends, aes(x = year, y = avg_delay, color = marketing_carrier)) +
  
  # --- COVID-19 Highlight ---
  annotate("rect", xmin = 2019.5, xmax = 2020.5, ymin = -Inf, ymax = Inf, 
           alpha = 0.1, fill = "red") +
  annotate("text", x = 2020, y = max(annual_trends$avg_delay), 
           label = "COVID-19", vjust = 1.5, size = 3.5, fontface = "bold") +
  
  # Lines and Points
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  
  # Colors
  scale_color_manual(values = airline_colors) +
  
  # Labels
  labs(
    title = "Average Arrival Delay per Flight (2018-2025)",
    subtitle = "Historical Performance of 10 Major U.S. Marketing Carriers",
    x = "Year",
    y = "Average Delay per Flight (Minutes)"
  ) +
  
  # Theme and formatting
  scale_x_continuous(breaks = 2018:2025) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.grid.minor = element_blank()
  )

print(trend_plot)




#===================================================================================================================
# Bootstrapping Confidence Interval for Population Trend

set.seed(7100) # same result on diff. machines

bootstrap_function <- function(delays, n_boot = 1000) {
  # ^this creates a function
delays <- na.omit(delays)
n <- length(delays)
  

boot_means <- replicate(n_boot, mean(sample(delays, size = n, replace = TRUE)))
  
ci <- quantile(boot_means, probs = c(0.025, 0.975)) 
 
return(c(avg_delay = mean(delays), ci_lower = ci[1], ci_upper = ci[2]))
}

#ci[1] is lower
#ci[2] is upper


# New variable in dataset without 0s

airdata$delay_per_flight_row <- ifelse(
  airdata$arr_flights == 0, 0, 
  airdata$arr_delay / airdata$arr_flights
)

# dividing by zero will cause error which is why we need this

# Split data
grouped_data <- split(airdata, list(airdata$marketing_carrier, airdata$year), drop = TRUE)
# creates 80 different datasets
# we are splliting airdata by each marketing carrrier and each year
# for example, Delta.2019, United.2020 etc.


# Run bootstrap
bootstrap_list <- lapply(grouped_data, function(df_subset) {

# df_subset is stand in for the 80 subsetted datasets we created with split

stats <- bootstrap_function(df_subset$delay_per_flight_row)
  
data.frame(
  marketing_carrier = df_subset$marketing_carrier[1], 
  # checks 1st row for name of carrier
  year              = df_subset$year[1],
  # checks 1st row for the year
  avg_delay         = stats["avg_delay"], 
  ci_lower          = stats["ci_lower.2.5%"], 
  ci_upper          = stats["ci_upper.97.5%"], 
  row.names         = NULL
  )
})

# Result
bootstrap_results <- do.call(rbind, bootstrap_list)

# Run to check
bootstrap_results

#Adding the CIs to the line graph

library(ggplot2)

pd <- position_dodge(0.5) 

boot_plot <- ggplot(bootstrap_results, aes(x = year, y = avg_delay, color = marketing_carrier)) +
  
  # covid-19 highlight
  annotate("rect", xmin = 2019.5, xmax = 2020.5, ymin = -Inf, ymax = Inf, 
           alpha = 0.1, fill = "red") +
  annotate("text", x = 2020, y = max(bootstrap_results$ci_upper, na.rm=TRUE), 
           label = "COVID-19", vjust = 1.5, size = 3.5, fontface = "bold") +
  
  geom_line(position = pd, linewidth = 0.8, alpha = 0.6) +
  geom_point(position = pd, size = 2) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), position = pd, width = 0.4, linewidth = 0.8) +
  
  scale_color_manual(values = airline_colors) +
  scale_x_continuous(breaks = 2018:2025)
  labs(
    title = "Bootstrapped 95% CI for Delay per Flight (2018-2025)",
    subtitle = "Comparison of Sample Mean vs. Population Estimate Uncertainty",
    x = "Year",
    y = "Average Delay per Flight (Minutes)"
  ) +
  
  theme_minimal() +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.grid.minor = element_blank()
  )

print(boot_plot)








#=================================================================================================================================

# Airport Traffic Heatmap


# Find Top 20 Busiest Airports by Total Flights
airport_vols <- aggregate(arr_flights ~ airport, data = airdata, FUN = sum)
# Note: airport_vols is Airport Volumes
top_20_codes <- airport_vols[order(-airport_vols$arr_flights), "airport"][1:20]

#[1:20] cuts off for top 20 instead of 316 airports

# Note: Code refers to the airport codes here. 
# In aviation we had 3 letter identifiers for airports.
# For example ATL is Hartsfield Jackson airport in Atlanta

# Filter dataset to keep only rows with these airports
airdata_heatmap <- subset(airdata, airport %in% top_20_codes)


# Omit zeros to prevent division error

airdata_heatmap$delay_per_flight <- ifelse(
airdata_heatmap$arr_flights == 0, 0,
airdata_heatmap$arr_delay / airdata_heatmap$arr_flights
)

# Heatmap

# Aggregate mean delay for each Airline-Airport pair
heatmap_agg <- aggregate(
  delay_per_flight ~ marketing_carrier + airport, 
  data = airdata_heatmap, 
  FUN = mean
)

# Create Heatmap
heat_plot <- ggplot(heatmap_agg, aes(x = airport, y = marketing_carrier, fill = delay_per_flight)) +
  geom_tile(color = "white") +
  
# Colour Theme
scale_fill_gradientn(colors = c("white", "#FFC629", "black")) +
  
labs(
title = "Heatmap: Average Delay by Airline and Airport (Top 20 Airports)",
x = "Airport Code",
y = "Marketing Carrier",
fill = "Avg Delay (min)"
) +
  
theme_minimal() +
theme(
panel.grid = element_blank(),
axis.text.x = element_text(angle = 45, hjust = 1)
)

print(heat_plot)


# Conclusion: 
# Jetblue and Frontier are consisently poor performers across all airports
# however,
# EWR (Newark Liberty) is consistently causing high delay across all carriers
# Graph supports theory that airports are a bigger player in delay
# lets confirm with ANOVA test



# The ANOVA test is not being run on the heatmap subset.
# Entire dataset is being considered


# Formula: Delay is explained by Carrier + Airport
two_way_ANOVA <- aov(delay_per_flight_row ~ marketing_carrier + airport, data = airdata)

# Print the summary to see the F-statistics side-by-side
summary(two_way_ANOVA)
two_way_ANOVA


# My Conclusion:
# Marketing Carrier have a larger impact on delay cause than the marketing carrier
# Comparing airlines without taking into account the differnce of routes is unfair


#===========================================================================================================================

#  Airport adjusted on-time performance comparison 

# Setting up new dataset

airport_carrier_counts <- aggregate(
  marketing_carrier ~ airport, 
  data = airdata, 
  FUN = function(x) length(unique(x))
)

# x is marketing_carrier in the above code 
#length(unique(x)) will be equal to 10


airport_carrier_counts

shared_airports <- airport_carrier_counts$airport[airport_carrier_counts$marketing_carrier == 10]
# Filters out airport_carrier_counts where marketing_carrier<10

print(shared_airports)

# Subset airdata for shared airports only
airdata_common_airports <- subset(airdata, airport %in% shared_airports)

nrow(airdata_common_airports) # 7238 observations for common airports


# Check to see all marketing carriers have enough flights to do analysis on for these airports
marketing_carrier_counts <- table(airdata_common_airports$marketing_carrier)


# Average delay per flight for each row

#ANOVA on Common Airports




# Justificaiton for using Welch
# CLT applies to a large dataset
# That does not include equal variance
variance_test <- fligner.test(delay_per_flight_row ~ marketing_carrier, data = airdata)
print(variance_test) # p-value is 2.2e-16

# Conclusion: Using Welch ANOVA For one way tests
#  Create the Performance Metric (Row-Level)
#    We need to calculate the average delay per flight for each row 
#    in this specific subset before we can run the test.


airdata_common_airports$delay_per_flight <- ifelse(
  airdata_common_airports$arr_flights == 0, 0,
  airdata_common_airports$arr_delay/
 airdata_common_airports$arr_flights
)


anova_common_airports <- oneway.test(
  delay_per_flight ~ marketing_carrier, 
  data = airdata_common_airports, 
  var.equal = FALSE
)

print(anova_common_airports)


# p value is less than 0.05, proceed with ranking:
common_airport_ranking <- aggregate(
  delay_per_flight ~ marketing_carrier, 
  data = airdata_common_airports, 
  FUN = mean
)

# Sort by lowest delay (The Winner)
common_airport_ranking <- common_airport_ranking[order(common_airport_ranking$delay_per_flight), ]


print(common_airport_ranking)



# --- Step 1: Prepare the Statistics ---

# We already have the Mean in 'common_airport_ranking'

#  Calculate Standard Deviation 
common_airport_sd <- aggregate(
  delay_per_flight ~ marketing_carrier, 
  data = airdata_common_airports, 
  FUN = sd
)

# Calculate Count
common_airport_n <- aggregate(
  delay_per_flight ~ marketing_carrier, 
  data = airdata_common_airports, 
  FUN = length
)



# Merge  Mean and SD
# Since both have a column named "delay_per_flight", R will rename them:
# delay_per_flight.x and delay_per_flight.y by default

plot_data <- merge(common_airport_ranking, common_airport_sd, by = "marketing_carrier")

# Merge the result with N
# The new column from common_airport_n will stay as "delay_per_flight"
plot_data <- merge(plot_data, common_airport_n, by = "marketing_carrier")


# --- Step 3: Calculate Confidence Intervals ---

# We now reference the columns based on how R merged them:
# Mean = delay_per_flight.x
# SD   = delay_per_flight.y
# N    = delay_per_flight

# Standard Error (SE = sd / sqrt(n))
plot_data$se <- plot_data$delay_per_flight.y / sqrt(plot_data$delay_per_flight)

# Margin of Error (95% Confidence)
plot_data$margin <- qt(0.975, df = plot_data$delay_per_flight - 1) * plot_data$se

# Calculate Bounds using the Mean (.x)
plot_data$lower_ci <- plot_data$delay_per_flight.x - plot_data$margin
plot_data$upper_ci <- plot_data$delay_per_flight.x + plot_data$margin


# --- Step 4: Create the Plot ---
ksu_gold <- "#FFC629"
ksu_black <- "#000000"

# Note: We use delay_per_flight.x (the Mean) for the x-axis and reordering
ggplot(plot_data, aes(x = reorder(marketing_carrier, -delay_per_flight.x), y = delay_per_flight.x)) +
  
  # Error Bars
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = ksu_black, size = 0.8) +
  
  # The Points
  geom_point(size = 4, color = ksu_gold) +
  
  # Formatting
  coord_flip() + 
  labs(
    title = "Airline Delay Ranking (Common Airports)",
    subtitle = "Mean Delay per Flight with 95% Confidence Intervals",
    x = "Airline",
    y = "Average Delay (Minutes)"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = ksu_black),
    plot.subtitle = element_text(face = "italic", size = 11, color = "gray30"),
    axis.text = element_text(color = ksu_black, size = 10),
    axis.title = element_text(face = "bold", color = ksu_black),
    panel.grid.major.y = element_blank()
  )


# Part 4 Time Series Forecast (Holt-Winters)

?HoltWinters()
monthly_data <- aggregate(
cbind(arr_delay, arr_flights) ~ marketing_carrier + year + month, 
data = airdata, # Assumes 'airdata' is your filtered 2018-2025 dataset
FUN = sum
)

# avergae delay variable

monthly_data$avg_delay <- monthly_data$arr_delay / monthly_data$arr_flights

# Sort by date 
monthly_data <- monthly_data[order(monthly_data$marketing_carrier, monthly_data$year, monthly_data$month), ]


# Forecast Function

predict_airline_performance <- function(airline) {
  

data_sub <- subset(monthly_data, marketing_carrier == airline)
  

ts_data <- ts(data_sub$avg_delay, start = c(2018, 1), frequency = 12)

# Apply HoltWinters

model <- HoltWinters(ts_data)
pred  <- predict(model, n.ahead = 11, prediction.interval = TRUE, level = 0.95)
  

# Print results to table (data frame)

  return(data.frame(
    Airline = airline,
    Month = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
    Prediction = round(as.numeric(pred[, "fit"]), 2),
    Lower_CI   = round(as.numeric(pred[, "lwr"]), 2),
    Upper_CI   = round(as.numeric(pred[, "upr"]), 2)
  ))
}



# Run the function for all 10 airlines
all_airlines <- unique(monthly_data$marketing_carrier)
final_forecast <- do.call(rbind, lapply(all_airlines, predict_airline_performance))

# Print the table
print(head(final_forecast, 11))
ignore <- unique(final_forecast$Airline)

# Delta Airlines TSF
plot_data_delta <- subset(final_forecast, Airline == "Delta Air Lines")
# Add a numeric index for plotting order
plot_data_delta$Month_Index <- 1:11 

# Graph

ggplot(plot_data_delta, aes(x = Month_Index, y = Prediction)) +
geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, fill = "#FFC629") +
geom_line(color = "#FFC629", linewidth = 1) +
geom_point(color = "black", size = 3) +
scale_x_continuous(breaks = 1:11, labels = plot_data_delta$Month) +
labs(title = "Delta Air Lines: 2025 Forecast", y = "Predicted Delay (min)", x = "Month") +
theme_minimal()


# United Airlines TSF
plot_data_united <- subset(final_forecast, Airline == "United Airlines")
plot_data_united$Month_Index <- 1:11


# Linegraph
ggplot(plot_data_united, aes(x = Month_Index, y = Prediction)) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, fill = "#FFC629") +
  geom_line(color = "#FFC629", linewidth = 1) +
  geom_point(color = "black", size = 3) +
  scale_x_continuous(breaks = 1:11, labels = plot_data_united$Month) +
  labs(title = "United Airlines: 2025 Forecast", y = "Predicted Delay (min)", x = "Month") +
  theme_minimal()


# Alaska Airlines TSF
plot_data_alaska <- subset(final_forecast, Airline == "Alaska Airlines")
plot_data_alaska$Month_Index <- 1:11

# Graph
ggplot(plot_data_alaska,aes(x = Month_Index, y = Prediction)) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, fill = "#FFC629") +
  geom_line(color = "#FFC629", linewidth = 1) +
  geom_point(color = "black", size = 3) +
  scale_x_continuous(breaks = 1:11, labels = plot_data_alaska$Month) +
  labs(title = "Alaska Airlines: 2025 Forecast", y = "Predicted Delay (min)", x = "Month") +
  theme_minimal()

# Allegiant Air TSF
plot_data_allegiant <- subset(final_forecast, Airline == "Allegiant Air")
plot_data_allegiant$Month_Index <- 1:11

ggplot(plot_data_allegiant, aes(x = Month_Index, y = Prediction)) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, fill = "#FFC629") +
  geom_line(color = "#FFC629", linewidth = 1) +
  geom_point(color = "black", size = 3) +
  scale_x_continuous(breaks = 1:11, labels = plot_data_allegiant$Month) +
  labs(title = "Allegiant Air: 2025 Forecast", y = "Predicted Delay (min)", x = "Month") +
  theme_minimal()


# American Airlines TSF
plot_data_american <- subset(final_forecast, Airline == "American Airlines")
plot_data_american$Month_Index <- 1:11

ggplot(plot_data_american, aes(x = Month_Index, y = Prediction)) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, fill = "#FFC629") +
  geom_line(color = "#FFC629", linewidth = 1) +
  geom_point(color = "black", size = 3) +
  scale_x_continuous(breaks = 1:11, labels = plot_data_american$Month) +
  labs(title = "American Airlines: 2025 Forecast", y = "Predicted Delay (min)", x = "Month") +
  theme_minimal()


# Frontier Airlines TSF
plot_data_frontier <- subset(final_forecast, Airline == "Frontier Airlines")
plot_data_frontier$Month_Index <- 1:11

ggplot(plot_data_frontier, aes(x = Month_Index, y = Prediction)) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, fill = "#FFC629") +
  geom_line(color = "#FFC629", linewidth = 1) +
  geom_point(color = "black", size = 3) +
  scale_x_continuous(breaks = 1:11, labels = plot_data_frontier$Month) +
  labs(title = "Frontier Airlines: 2025 Forecast", y = "Predicted Delay (min)", x = "Month") +
  theme_minimal()


# Hawaiian Airlines TSF
plot_data_hawaiian <- subset(final_forecast, Airline == "Hawaiian Airlines")
plot_data_hawaiian$Month_Index <- 1:11

ggplot(plot_data_hawaiian, aes(x = Month_Index, y = Prediction)) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, fill = "#FFC629") +
  geom_line(color = "#FFC629", linewidth = 1) +
  geom_point(color = "black", size = 3) +
  scale_x_continuous(breaks = 1:11, labels = plot_data_hawaiian$Month) +
  labs(title = "Hawaiian Airlines: 2025 Forecast", y = "Predicted Delay (min)", x = "Month") +
  theme_minimal()


# JetBlue Airways TSF
plot_data_jetblue <- subset(final_forecast, Airline == "JetBlue Airways")
plot_data_jetblue$Month_Index <- 1:11

ggplot(plot_data_jetblue, aes(x = Month_Index, y = Prediction)) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, fill = "#FFC629") +
  geom_line(color = "#FFC629", linewidth = 1) +
  geom_point(color = "black", size = 3) +
  scale_x_continuous(breaks = 1:11, labels = plot_data_jetblue$Month) +
  labs(title = "JetBlue Airways: 2025 Forecast", y = "Predicted Delay (min)", x = "Month") +
  theme_minimal()


# Southwest Airlines TSF
plot_data_southwest <- subset(final_forecast, Airline == "Southwest Airlines")
plot_data_southwest$Month_Index <- 1:11

ggplot(plot_data_southwest, aes(x = Month_Index, y = Prediction)) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, fill = "#FFC629") +
  geom_line(color = "#FFC629", linewidth = 1) +
  geom_point(color = "black", size = 3) +
  scale_x_continuous(breaks = 1:11, labels = plot_data_southwest$Month) +
  labs(title = "Southwest Airlines: 2025 Forecast", y = "Predicted Delay (min)", x = "Month") +
  theme_minimal()


# Spirit Airlines TSF
plot_data_spirit <- subset(final_forecast, Airline == "Spirit Airlines")
plot_data_spirit$Month_Index <- 1:11

ggplot(plot_data_spirit, aes(x = Month_Index, y = Prediction)) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, fill = "#FFC629") +
  geom_line(color = "#FFC629", linewidth = 1) +
  geom_point(color = "black", size = 3) +
  scale_x_continuous(breaks = 1:11, labels = plot_data_spirit$Month) +
  labs(title = "Spirit Airlines: 2025 Forecast", y = "Predicted Delay (min)", x = "Month") +
  theme_minimal()




#====================================================================================================


final_forecast$Month_Index <- rep(1:11, times = 10) 

# Grid Plot
ggplot(final_forecast, aes(x = Month_Index, y = Prediction)) +
  

  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, fill = "#FFC629") +
  geom_line(color = "#FFC629", linewidth = 1) +
  geom_point(color = "black", size = 1.5) + # Slightly smaller dots for the small graphs
  
# Facet Wrap for 2 row of 5 each
  facet_wrap(~ Airline, nrow = 2, ncol = 5, scales = "free_y") +
  
  # Formatting
  labs(x = NULL, y = NULL) +
  
  theme_minimal() +
  theme(
    # Remove axis titles and text (ticks) to reduce clutter
    axis.title = element_blank(),
    axis.text = element_blank(),
    
    # Make the grid lines subtle
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    
    # Make the Airline Names (Strip Text) bold and readable
    strip.text = element_text(face = "bold", size = 10, color = "black")
  )




