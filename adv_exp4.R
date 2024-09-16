install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
library(dplyr)


# Check for missing values
summary(Crime_Data)


Crime_Data$Occurred.Date <- as.Date(Crime_Data$Occurred.Date, format = "%m/%d/%Y")
Crime_Data$Reported.Date <- as.Date(Crime_Data$Reported.Date, format = "%m/%d/%Y")
-------------------------------------------------------------------------------
# Bar Chart
  
# Summarize and sort the data to get the top 10 categories
top_10_crime <- Crime_Data %>%
  group_by(Crime.Subcategory) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  slice_head(n = 10)  # Select the top 10 categories

# Create a bar chart for the top 10 categories
ggplot(top_10_crime, aes(x = reorder(Crime.Subcategory, -Count), y = Count)) + 
  geom_bar(stat = "identity", fill = "skyblue") + 
  theme_minimal(base_size = 15) + 
  labs(title = "Top 10 Crime Subcategories", x = "Crime Subcategory", y = "Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

--------------------------------------------------------------------------------
#Pie Chart 
  
# Create a frequency table for the Precinct column
pie_data_precinct <- table(Crime_Data$Precinct)

# Create a pie chart for Precincts
pie(pie_data_precinct, 
    main = "Pie Chart of Precincts", 
    col = rainbow(length(pie_data_precinct)))

# Optional: Add percentages to the pie chart for better clarity
percent_labels <- round(100 * pie_data_precinct / sum(pie_data_precinct), 1)
labels <- paste(names(pie_data_precinct), "(", percent_labels, "%)", sep="")
pie(pie_data_precinct, labels = labels, main = "Pie Chart of Precincts", col = rainbow(length(pie_data_precinct)))

-------------------------------------------------------------------------------
#Histogram
  
ggplot(Crime_Data, aes(x = Reported.Time)) + 
  geom_histogram(binwidth = 100, fill = "orange", color = "black") + 
  theme_minimal() + 
  labs(title = "Distribution of Reported Times", x = "Reported Time", y = "Frequency")

ggplot(Crime_Data, aes(x = Occurred.Time)) + 
  geom_histogram(binwidth = 100, fill = "yellow", color = "black") + 
  theme_minimal() + 
  labs(title = "Distribution of Occurred Times", x = "Occurred Time", y = "Frequency")


-------------------------------------------------------------------------------
# Time-Line Chart

ggplot(Crime_Data, aes(x = Occurred.Date)) + 
geom_histogram(binwidth = 365, fill = "purple", color = "black") + 
theme_minimal(base_size = 15) + 
labs(title = "Timeline of Crime Occurrences", x = "Occurred Date", y = "Count") + 
scale_x_date(limits = as.Date(c("2006-01-01", "2020-01-01")), 
               date_breaks = "5 years", 
               date_labels = "%Y")  # Setting breaks and labels on the x-axis

--------------------------------------------------------------------------------
# Scatter PLot 

# Filter data between 2006 and 2020
filtered_data <- Crime_Data %>%
  filter(Occurred.Date >= as.Date("2006-01-01") & Occurred.Date <= as.Date("2020-12-31"))

# Plot the filtered data
ggplot(filtered_data, aes(x = Occurred.Date, y = Reported.Date)) + 
  geom_point(color = "darkgreen") + 
  theme_minimal(base_size = 15) + 
  labs(title = "Scatter Plot of Occurred Date vs Reported Date (2006-2020)", 
       x = "Occurred Date", y = "Reported Date")


  ggplot(Crime_Data, aes(x = Occurred.Date, y = Reported.Date)) + 
  geom_point(color = "darkgreen") + 
  theme_minimal(base_size = 15) + 
  labs(title = "Scatter Plot of Occurred Date vs Reported Date", x = "Occurred Date", y = "Reported Date")


ggplot(Crime_Data, aes(x = Occurred.Time, y = Reported.Time)) + 
  geom_point(color = "blue") + 
  theme_minimal() + 
  labs(title = "Scatter Plot of Occurred vs Reported Time", x = "Occurred Time", y = "Reported Time")

ggplot(Crime_Data, aes(x = Reported.Time, y = Neighborhood)) + 
  geom_point(color = "purple", alpha = 0.7) + 
  theme_minimal(base_size = 15) + 
  labs(title = "Scatter Plot of Reported Time vs Neighborhood", x = "Reported Time", y = "Neighborhood")


-------------------------------------------------------------------------------------
# Bubble Plot

# Create a frequency table to count the number of occurrences for each crime subcategory
crime_count <- Crime_Data %>%
  group_by(Crime.Subcategory) %>%
  summarise(count = n())

# Join the frequency count back to the original data
Crime_Data <- Crime_Data %>%
  left_join(crime_count, by = "Crime.Subcategory")

# Plot the bubble plot with count as the size
ggplot(Crime_Data, aes(x = Occurred.Date, y = Reported.Date, size = count, color = Precinct)) + 
  geom_point(alpha = 0.6) + 
  theme_minimal(base_size = 15) + 
  labs(title = "Bubble Plot of Occurred Date vs Reported Date", 
       x = "Occurred Date", y = "Reported Date", size = "Crime Count") + 
  scale_size_continuous(range = c(3, 10))  # Adjust the size range for bubbles


