library(dplyr)  # For data manipulation
library(ggplot2)  # For data visualization


library(caret)



data <- read.csv("C:\\Users\\Jatin Yadav\\Desktop\\R Project\\store2.csv", header = TRUE, sep = ",")
df<-data.frame(data)
df


#Structure of data
str(df)

# View the first few rows of the dataset
head(df)

# Summary statistics for numeric columns
summary(df)


#to know data type of each row
data_types <- sapply(df, class)
print(data_types)

# Check for null values 
na2 <- sum(is.na(df))
na2
df<-na.omit(df)
na2 <- sum(is.na(df))
na2
#histogram for distribution of age 
ggplot(df, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")

#histogram for distribution of amount
ggplot(df, aes(x = Amount)) +
  geom_histogram(binwidth = 20, fill = "lightgreen", color = "black") +
  labs(title = "Amount Distribution", x = "Amount", y = "Frequency")

#Number of orders in each category
ggplot(df, aes(x = Category)) +
  geom_bar(fill = "cyan") +
  labs(title = "Count of Orders by Category", x = "Category", y = "Count")

#number of order according to gender
ggplot(df, aes(x = Gender)) +
  geom_bar(fill = c("skyblue","orange")) +
  labs(title = "Count of Orders by gender", x = "Gender", y = "Count")


#Amount vs category

ggplot(df, aes(x = Category, y = Amount)) +
  geom_bar(stat = "summary", fun = "sum", fill = "skyblue") +
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Total Amount by Category", x = "Category", y = "Total Amount")

#Total sales according to each channel
ggplot(df, aes(x = Channel, y = Amount)) +
  geom_bar(stat = "summary", fun = "sum", fill = "cyan") +
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Total Amount by channel", x = "Channel", y = "Total Amount")

#Relationship between sales and age group
color=c("pink","lightgreen","orange")

ggplot(df, aes(x = Age.Group, y = Amount, fill = Age.Group)) +
  geom_bar(stat = "summary", fun = "sum",fill=color) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total Sales in each age group", x = "Age Group", y = "Total Amount") 



# scatterplot for 'Age' vs. 'Amount' with a specific color
ggplot(df, aes(x = Age, y =Amount ,  color = "Line Color")) +
  geom_point() +
  labs(title = "Line Chart of Age vs. Amount", x = "Age", y = "Amount") +
  scale_color_manual(values = "green")


orders_by_age <- df %>%
  group_by(Age) %>%
  summarise(NumOrders = n())

# Line chart for 'Age' vs. the number of orders with red color
ggplot(orders_by_age, aes(x = Age, y = NumOrders, group = 1)) +
  geom_line(color = "red") +
  labs(title = "Number of Orders by Age", x = "Age", y = "Number of Orders")



df$Gender <- as.integer(factor(data$Gender, levels = c("Men", "Women")))
df$Age.Group <- as.integer(factor(data$Age.Group, levels = c("Teen", "Senior", "Adult")))
df$Month <- as.integer(factor(data$Month, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")))
df$Status <- as.integer(factor(data$Status, levels = c("Cancelled", "Refunded", "Returned", "Delivered")))
df$Channel <- as.integer(factor(data$Channel, levels = c("Myntra", "Ajio", "Amazon","Meesho", "Flipkart", "Nalli", "Others")))
df$Category <- as.integer(factor(data$Category, levels = c("Blouse", "Bottom", "Kurta", "Saree", "Set", "Top", "Western", "Ethnic Dress")))
df$Size <- as.integer(factor(data$Size, levels = c("Free", "S", "M", "XL", "XXL", "L", "XS")))
df


# Find the data type of each column
column_data_types <- sapply(df, class)

# Print the data types
print(column_data_types)



numeric_columns <- df[, sapply(df, is.numeric)]

# Calculate the correlation matrix for the numeric columns
correlation_matrix <- cor(numeric_columns)

# Display the correlation matrix
print(correlation_matrix)



# Load the required library for random forest
library(randomForest)

# Assuming df is your dataset
set.seed(123)  # for reproducibility

# Split the data into training and testing sets
# You can use the `createDataPartition` function from the `caret` package to create a stratified split
# Install and load caret package if you haven't already
# install.packages("caret")
# library(caret)

# Set the proportion for the test set (e.g., 70% training and 30% testing)
test_size <- 0.3

# Create an index for the test set
test_index <- createDataPartition(df$Amount, p = test_size, list = FALSE)

# Split the data into training and testing sets
train_data <- df[-test_index, ]  # Training data
test_data <- df[test_index, ]    # Testing data

# Fit a Random Forest regression model on the training data
rf_model <- randomForest(Amount ~ Age+Category+Gender+Channel, data = train_data, ntree = 101)

# Make predictions on the test data
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model on the test data
rf_rmse <- sqrt(mean((test_data$Amount - rf_predictions)^2))
rf_r_squared <- 1 - sum((test_data$Amount - rf_predictions)^2) / sum((test_data$Amount - mean(test_data$Amount))^2)

# Print RMSE and R-squared
cat("Random Forest RMSE: ", rf_rmse, "\n")
cat("Random Forest R-squared: ", rf_r_squared, "\n")

# Define a threshold for accuracy (e.g., within 10%)
accuracy_threshold <- 0.90

# Calculate the percentage of predictions within the threshold
accuracy_percentage <- mean(abs(test_data$Amount - rf_predictions) / test_data$Amount <= accuracy_threshold) * 100

# Print the accuracy percentage
cat("Accuracy within", accuracy_threshold * 100, "%:", accuracy_percentage, "%\n")



