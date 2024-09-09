# Install necessary libraries if they are not already installed
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("corrplot", quietly = TRUE)) {
  install.packages("corrplot")
}

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(corrplot)

# Load the dataset
data <- read.csv("C:/Users/Admin/Downloads/sales_new_data.csv")

# View the first few rows of the dataset
head(data)

# Check the structure of the dataset
str(data)

# Summary statistics for numeric columns
summary(data)

# Check for missing values
cat("Missing Values:\n")
print(colSums(is.na(data)))

# Convert date columns to Date type
data$Order.Date <- as.Date(data$Order.Date, format = "%m/%d/%Y")
data$Ship.Date <- as.Date(data$Ship.Date, format = "%m/%d/%Y")

# Check the structure again to confirm changes
str(data)

# Convert categorical variables to factors
data$Region <- as.factor(data$Region)
data$Country <- as.factor(data$Country)
data$Item.Type <- as.factor(data$Item.Type)
data$Sales.Channel <- as.factor(data$Sales.Channel)
data$Order.Priority <- as.factor(data$Order.Priority)

# Total sales and profit
total_sales <- sum(data$Total.Revenue)
total_profit <- sum(data$Total.Profit)

cat("Total Sales: ", total_sales, "\n")
cat("Total Profit: ", total_profit, "\n")

# Total revenue by region
revenue_by_region <- data %>%
  group_by(Region) %>%
  summarise(Total.Revenue = sum(Total.Revenue),
            Total.Profit = sum(Total.Profit))

cat("Total Revenue by Region:\n")
print(revenue_by_region)

# Total revenue by item type
revenue_by_item <- data %>%
  group_by(Item.Type) %>%
  summarise(Total.Revenue = sum(Total.Revenue),
            Total.Profit = sum(Total.Profit))

cat("Total Revenue by Item Type:\n")
print(revenue_by_item)

# Bar Plot for Revenue by Region
ggplot(revenue_by_region, aes(x = Region, y = Total.Revenue, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Revenue by Region", x = "Region", y = "Total Revenue") +
  theme_minimal()

# Pie Chart for Item Type Distribution
item_type_distribution <- data %>%
  group_by(Item.Type) %>%
  summarise(Count = n())

ggplot(item_type_distribution, aes(x = "", y = Count, fill = Item.Type)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Item Types") +
  theme_void()

# RFM Analysis
# Assuming you have a Customer ID column, replace 'CustomerID' with the actual name of your customer ID column
if ("CustomerID" %in% colnames(data)) {
  rfm_data <- data %>%
    group_by(CustomerID) %>%
    summarise(Recency = as.numeric(Sys.Date() - max(Order.Date)),
              Frequency = n(),
              Monetary = sum(Total.Revenue))
  
  # View RFM summary
  cat("RFM Summary:\n")
  print(head(rfm_data))
} else {
  cat("CustomerID column not found for RFM analysis.\n")
}

# Correlation analysis
# Correlation matrix for numerical features
correlation_matrix <- cor(data %>% select(Total.Revenue, Units.Sold, Unit.Price, Unit.Cost, Total.Profit))
cat("Correlation Matrix:\n")
print(correlation_matrix)

# Visualizing the correlation matrix
corrplot(correlation_matrix, method = "circle")



