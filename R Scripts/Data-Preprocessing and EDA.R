# Load the dplyr package
library(dplyr)
library(readr)  
library(ggplot2)
library(corrplot)
library(here)
library(stringr)


data <- read_csv("C:/Users/Ahmad Afzal/Desktop/WRFdata_May2023.csv", col_names = FALSE)

# Step 2: Assign the second row as the header
colnames(data) <- as.character(data[2, ])

# Step 3: Remove the second row from the data (to avoid duplication in the rows)
data <- data[-2, ]




# Initial EDA: Overview of the dataset
print(dim(data))  # Dimensions of the dataset
print(str(data))  # Structure of the dataset
print(summary(data))  # Summary statistics for each column

# Missing data analysis
missing_data_summary <- colSums(is.na(data))
print(missing_data_summary)

# Visualize missing data distribution
missing_data_plot <- data.frame(Variable = names(missing_data_summary), 
                                Missing_Count = missing_data_summary) %>%
  ggplot(aes(x = reorder(Variable, -Missing_Count), y = Missing_Count)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Missing Data Summary", x = "Variables", y = "Count of Missing Values") +
  theme_minimal()

print(missing_data_plot)






print(head(data))





# Define Bolton's coordinates (approximately)
bolton_lat <- 53.5789
bolton_long <- -2.4292


# Set the conditions for filtering
min_latitude <- 49     # Example lower bound for latitude
max_latitude <- 54     # Example upper bound for latitude
min_longitude <- -11   # Example lower bound for longitude
max_longitude <- -7    # Example upper bound for longitude

colnames(data) <- make.names(names(data), unique = TRUE)

# Filter out rows that match Bolton's coordinates and fall within your specified latitude and longitude range
filtered_df <- data %>%
  filter(!(XLAT == bolton_lat & XLONG == bolton_long)) %>%  # Exclude Bolton's coordinates
  filter(XLAT >= min_latitude & XLONG <= max_latitude) %>%   # Select appropriate latitude range
  filter(XLAT >= min_longitude & XLONG <= max_longitude)   # Select appropriate longitude range





# Ensure that at least 300 rows are selected
filtered_df <- filtered_df %>%
  sample_n(min(n(), 300))  # Randomly sample at least 300 rows or fewer if less than 300 rows are available

# Display the first few rows of the filtered dataset
print(head(filtered_df))









# Write the cleaned data to a new CSV file (optional)
write_csv(filtered_df, "C:/Users/Ahmad Afzal/Desktop/cleaned_file.csv")

# Display the first few rows of the cleaned data
print(head(data_cleaned))





df <- read_csv("C:/Users/Ahmad Afzal/Desktop/cleaned_file.csv")


# Assuming your dataset is named 'df'
# Define the start date and time
start_datetime <- as.POSIXct("2018-05-01 00:00:00", format="%Y-%m-%d %H:%M:%S")

# Initialize an empty dataframe to store the results
result <- data.frame()

# Define the base columns to keep
base_columns <- c("XLAT", "XLONG", "TSK", "PSFC", "X.U10.", "X.V10.", 
                  "X.Q2.", "RAINC", "RAINNC", "SNOW", "TSLB", "SMOIS")

# Loop over each set of columns
#This data is for the 10 days of data
for (i in 0:80) {
  # Create a copy of the base columns
  temp_df <- df[, base_columns]
  
  if (i > 0) {
    # Modify the column names to include the suffix
    suffix <- paste0(".", i)
    columns_with_suffix <- paste0(base_columns[-c(1:2)], suffix)
    temp_df[, -c(1:2)] <- df[, columns_with_suffix]
  }
  
  # Calculate the current datetime for this set
  current_datetime <- start_datetime + (i * 3 * 3600)
  
  # Format the datetime as required
  temp_df$date_time <- format(current_datetime, "%d.%m.%Y.%H.%M")
  
  # Bind the current dataframe to the result
  result <- bind_rows(result, temp_df)
}

# Display the result
head(result)



write_csv(result, "C:/Users/Ahmad Afzal/Desktop/processed_file.csv")





df <- read_csv("C:/Users/Ahmad Afzal/Desktop/processed_file.csv")



# Just keeping the coordinates of Scotland
scotland_lat_min <- 55.8
scotland_lat_max <- 58.6
scotland_long_min <- -6.5
scotland_long_max <- -2.0

# Filter for Coordinates within Scotland
scotland_data <- df[df$XLAT >= scotland_lat_min & 
                            df$XLAT <= scotland_lat_max &
                            df$XLONG >= scotland_long_min & 
                            df$XLONG <= scotland_long_max, ]

# Print the filtered coordinates (Optional)
print(scotland_data)

# Save the filtered coordinates to a new CSV file
write.csv(scotland_data, "C:/Users/Ahmad Afzal/Desktop/wrf_scotland_data.csv", row.names = FALSE)




data <- read_csv("C:/Users/Ahmad Afzal/Desktop/wrf_scotland_data.csv")



# Remove the XLAT and XLONG columns
data_without_coordinates <- data %>%
  select(-XLAT, -XLONG)

# Save the updated data to a new CSV file
write_csv(data_without_coordinates, "C:/Users/Ahmad Afzal/Desktop/Scotland_WRFdata_Without_Coordinates.csv")

# Print the first few rows of the updated data to the console
print(head(data_without_coordinates))


#----------------------------------------------------------------------------

new_data <- read_csv("C:/Users/Ahmad Afzal/Desktop/Scotland_WRFdata_Without_Coordinates.csv")



# Check if 'date_time' column exists and rename it
if ("date_time" %in% colnames(new_data)) {
  colnames(new_data)[colnames(new_data) == "date_time"] <- "DATETIME"
} else {
  stop("The 'date_time' column is missing in the dataset.")
}


# Print the first few rows of the updated new_data to check changes
print(head(new_data))


newdata_copy <- new_data[-1, ]
colnames(newdata_copy) <- c("TSK", "PSFC", "U10", "V10", "Q2", "RAINC", "RAINNC", "SNOW", "TSLB", "SMOIS", "DATETIME")

# Convert columns to numeric format, excluding 'DATETIME'
numeric_cols <- setdiff(colnames(newdata_copy), "DATETIME")
newdata_copy[numeric_cols] <- lapply(newdata_copy[numeric_cols], as.numeric)

# Replace NA values with the mean of the previous two values
fill_na_with_mean <- function(x) {
  na_index <- which(is.na(x))
  for (i in na_index) {
    if (i > 2) {
      x[i] <- mean(c(x[i - 1], x[i - 2]), na.rm = TRUE)
    }
  }
  return(x)
}
print(colnames(newdata_copy))






newdata_copy[numeric_cols] <- lapply(newdata_copy[numeric_cols], fill_na_with_mean)

# Convert 'DATETIME' to POSIXct format
newdata_copy$DATETIME <- as.POSIXct(newdata_copy$DATETIME, format = "%d.%m.%Y.%H.%M")

# Calculate Wind Speed
newdata_copy$WIND_SPEED <- round(sqrt(newdata_copy$U10^2 + newdata_copy$V10^2), 2)

# Exploratory Data Analysis
print(str(newdata_copy))
print(head(newdata_copy))
print(summary(newdata_copy))

# Descriptive statistics for numerical variables
print(summary(newdata_copy[, sapply(newdata_copy, is.numeric)]))

# Check for missing values
print(colMeans(is.na(newdata_copy)))

# Explore data distribution with histograms
numeric_vars <- names(newdata_copy)[sapply(newdata_copy, is.numeric)]
for (var in numeric_vars) {
  print(
    ggplot(newdata_copy, aes_string(x = var)) +
      geom_histogram(bins = 30, color = "black", fill = "lightblue") +
      labs(title = paste("Distribution of", var), x = var, y = "Frequency") +
      theme_bw()
  )
}



# Identify outliers with z-scores (more than 3 standard deviations from the mean)
outliers <- lapply(newdata_copy[numeric_vars], function(x) {
  abs_z_scores <- abs((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
  return(which(abs_z_scores > 3))
})

# Report columns containing outliers based on z-scores
outlier_columns <- names(outliers)[sapply(outliers, length) > 0]
if (length(unlist(outliers)) > 0) {
  cat("Potential outliers identified in columns:", outlier_columns, "\n")
} else {
  cat("No potential outliers identified based on z-scores.\n")
}

# Cap the outliers for numerical columns (except 'RAINC' and 'RAINNC')
data1_clean <- newdata_copy
for (var in outlier_columns) {
  if (!(var %in% c("RAINC", "RAINNC"))) {
    lower_bound <- quantile(data1_clean[[var]], probs = 0.25, na.rm = TRUE) - 1.5 * IQR(data1_clean[[var]], na.rm = TRUE)
    upper_bound <- quantile(data1_clean[[var]], probs = 0.75, na.rm = TRUE) + 1.5 * IQR(data1_clean[[var]], na.rm = TRUE)
    data1_clean[[var]] <- pmin(pmax(data1_clean[[var]], lower_bound), upper_bound)
  }
}



# Daytime vs. Nighttime Pressure (PSFC) Analysis
data_df <- data1_clean
data_df$Hour <- as.numeric(format(data_df$DATETIME, "%H"))
data_df$Day_Night <- ifelse(data_df$Hour >= 6 & data_df$Hour < 18, "Daytime", "Nighttime")

# Subset data and perform t-test
daytime_data <- filter(data_df, Day_Night == "Daytime")
nighttime_data <- filter(data_df, Day_Night == "Nighttime")
t_test_result <- t.test(daytime_data$PSFC, nighttime_data$PSFC)
print(t_test_result)

# Correlation Analysis between SMOIS and TSK
correlation <- cor(data_df$SMOIS, data_df$TSK, use = "complete.obs")
print(correlation)
corrplot(cor(data_df[, c("SMOIS", "TSK")], use = "complete.obs"), method = "circle")

# Mean Wind Speed Analysis
data_df$Date <- as.Date(data_df$DATETIME)
mean_wind_speed <- aggregate(WIND_SPEED ~ Date, data = data_df, FUN = mean)
print(mean_wind_speed)

# Plot the distribution of wind speed
ggplot(data_df, aes(x = WIND_SPEED)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Wind Speed", x = "WIND_SPEED (km/h)", y = "Frequency") +
  theme_minimal()


write_csv(data_df, "C:/Users/Ahmad Afzal/Desktop/Refined_Scotland_WRFdata.csv")


