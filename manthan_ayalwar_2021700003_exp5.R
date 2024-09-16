install.packages("ggplot2")
install.packages("dplyr")
install.packages("plotly")
install.packages("RColorBrewer")

library(ggplot2)
library(dplyr)
library(plotly)
library(RColorBrewer)

data <- read.csv("C:/Users/manth/Downloads/MELBOURNE_HOUSE_PRICES_LESS.csv")

summary(data)

head(data)

# Word Cloud

# Install and load wordcloud2 library
install.packages("wordcloud2")
library(wordcloud2)

# Prepare data for the word cloud (example using 'Suburb')
word_freq <- data %>% count(Suburb)

# Generate word cloud
wordcloud2(word_freq, size = 1, color = 'random-dark')

property_types <- c("h", "h", "h", "h", "u", "t", "h", "h")

# Create word cloud
freq_table <- data.frame(property_types = names(table(property_types)), 
                         freq = as.numeric(table(property_types)))

# Create word cloud
wordcloud2(data = freq_table, size = 1, color = "random-light", backgroundColor = "black")

# Box and Whisker 
# plot of Price vs Suburb
top_10_prices <- data %>%
  arrange(desc(Price)) %>%
  slice(1:10) # Select the top 10 rows by Price

# Create a boxplot for the top 10 suburbs based on Price
ggplot(top_10_prices, aes(x = reorder(Suburb, Price), y = Price)) +
  geom_boxplot(fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Box and Whisker Plot of Top 10 Prices by Suburb",
       x = "Suburb", y = "Price")

top_5_regions <- data %>%
  group_by(Regionname) %>%
  summarise(avg_price = mean(Price, na.rm = TRUE)) %>%
  arrange(desc(avg_price)) %>%
  slice(1:5) %>%
  pull(Regionname)

# Filter the data to only include the top 5 regions
top_5_data <- data %>%
  filter(Regionname %in% top_5_regions)

# Create a boxplot for the top 5 regions by Price
ggplot(top_5_data, aes(x = Regionname, y = Price)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of Price by Top 5 Regions", x = "Region", y = "Price")

# Violin plot of Price vs Regionname
ggplot(data, aes(x = factor(Rooms), y = Price, fill = factor(Rooms))) +
  geom_violin(trim = TRUE) +  # trim = TRUE to show only relevant distribution
  labs(title = "Violin Plot of Property Prices by Number of Rooms", x = "Number of Rooms", y = "Price") +
  theme_minimal() +  # Apply minimal theme for a clean look
  scale_fill_brewer(palette = "Set1")  # Apply a different color palette

ggplot(data, aes(x = Regionname, y = Price, fill = Regionname)) +
  geom_violin(trim = FALSE) +  # trim = FALSE to show the full distribution
  labs(title = "Violin Plot of Property Prices by Region", x = "Region", y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  scale_fill_brewer(palette = "Set3")  # Add color palette for fill

# Linear regression 
# plot between Rooms and Price
ggplot(data, aes(x = Rooms, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression: Rooms vs Price")

# Non-linear regression plot (using LOESS)
ggplot(data, aes(x = Rooms, y = Price)) +
  geom_point() +
  geom_smooth(method = "loess", col = "red") +
  labs(title = "Non-linear Regression (LOESS): Rooms vs Price")

# Linear regression of Price vs Distance
ggplot(data, aes(x = Distance, y = Price)) +
  geom_point(alpha = 0.6, color = "darkgreen") +  # Add transparency to the points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Fit linear model
  labs(title = "Linear Regression of Price vs Distance", x = "Distance", y = "Price") +
  theme_minimal()  # Use a minimal theme for better aesthetics



# Nonlinear regression plot: Price vs Distance
# Polynomial regression (degree 2) of Price vs Distance
ggplot(data, aes(x = Distance, y = Price)) +
  geom_point(alpha = 0.6, color = "purple") +  # Add scatter plot points
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "orange") +  # Fit quadratic regression
  labs(title = "Polynomial Regression (Degree 2) of Price vs Distance", x = "Distance", y = "Price") +
  theme_light()  # Apply a light theme for a clean look



# 3D scatter plot
plot_ly(data, x = ~Rooms, y = ~Price, z = ~Distance, color = ~Regionname, type = 'scatter3d', mode = 'markers') %>%
  layout(scene = list(
    xaxis = list(title = 'Rooms'),
    yaxis = list(title = 'Price'),
    zaxis = list(title = 'Distance')
  ),
  title = "3D Scatter Plot of Rooms, Price, and Distance")


# Jitter plot for Rooms vs Price
ggplot(data, aes(x = Rooms, y = Price)) +
  geom_jitter(width = 0.2, height = 0.2, color = "blue", alpha = 0.5) +
  labs(title = "Jitter Plot of Rooms vs Price")

top_10_suburbs <- data %>%
  group_by(Suburb) %>%
  summarize(MedianPrice = median(Price)) %>%
  top_n(10, MedianPrice) %>%
  pull(Suburb)

# Filter the original data for only these top 10 suburbs
filtered_data <- data %>%
  filter(Suburb %in% top_10_suburbs)

# Create the plot
ggplot(filtered_data, aes(x = reorder(Suburb, Price, FUN = median), y = Price)) +
  geom_jitter(aes(color = as.factor(Rooms)), width = 0.2, height = 0, size = 3) +
  labs(title = "Jitter Plot of Price vs Top 10 Suburbs", 
       x = "Suburb", 
       y = "Price", 
       color = "Rooms") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 




