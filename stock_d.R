#install the required packages
install.packages(c("ggplot2", "dplyr"))

# Load required libraries
library(dplyr)
library(ggplot2)

# Load historical stock price data
data <- read.csv("stock_data.csv")
print(data)
str(data)

# Descriptive statistics
IQR<- IQR(data$close)
var<- var(data$close)
sd<- sd(data$close)

print(paste("IQR for Close Last:", IQR))
print(paste("Variance for Close Last:", var))
print(paste("Standard Deviation for Close Last:", sd))

# Covariance and correlation
cov_matrix <- cov(select(data, high, low))
print(cov_matrix)

#summarise
avg_volume<- data %>% summarise(AvgVolume = mean(volume, na.rm = TRUE))
print(avg_volume)

#filter
high_values<-data%>%filter(high>20.9)
print(high_values)


# Create a linear regression model
model <- lm(close ~ open + high + low, data = data)

# Print a summary of the model
summary(model)

# Make predictions
new_data<-data.frame(open=19,high=25.6,low=27.89)
close<- predict(model, new_data)
print(paste("predicted for Close Last:", close))

# Plot the linear regression model
plot(data$high,data$low, main = "Linear Regression Model",
     xlab = "high", ylab = "low", col = "blue")
abline(lm(data$low ~ data$high), col = "red")


# Convert date to Date type
data$date <- as.Date(data$date, format="%m-%d-%Y")

# Line graph
ggplot(data, aes(x = date, y = close)) +
  geom_line() +
  labs(title = "Stock Price Over Time", x = "Date", y = "Closing Price")

# Assuming you want a pie chart based on the average closing price for a specific time period
avg_close <- mean(data$close)
avg_open <- mean(data$open)

pie_data <- data.frame(label = c("Average Closing Price", "Average Opening Price"),
                       value = c(avg_close, avg_open))

# Pie chart
ggplot(pie_data, aes(x = "", y = value, fill = label)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Average Closing and Opening Prices")

# Bar graph
ggplot(data, aes(x = date, y = volume)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Volume Over Time", x = "Date", y = "Volume")

# Histogram
ggplot(data, aes(x = close)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Closing Prices", x = "Closing Price", y = "Frequency")

# Scatter plot
ggplot(data, aes(x = open, y = close)) +
  geom_point(color = "red") +
  labs(title = "Scatter Plot of Opening vs. Closing Prices", x = "Opening Price", y = "Closing Price")