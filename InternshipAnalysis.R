install.packages("ggplot2")

setwd("C:/IDS project")

#LOADING THE DATA
internship_data<-read.csv("internship.csv")

#EXPLORING THE DATA
head(internship_data)
tail(internship_data)
str(internship_data)
summary(internship_data)
dim(internship_data)

#DATA PREPROCESSING  
#DATA CLEANING

# Load necessary libraries
library(dplyr)

#DUPLICATES
print("Total number of rows before deleting duplicates:")
print(nrow(internship_data))

#REMOVE DUPLICATE ROWS BASED ON ALL COLUMNS
internship_data <- distinct(internship_data)

#PRINT TOTAL NUMBER OF ROWS AFTER DELETING DUPLICATES
print("Total number of rows after deleting duplicates")
print(nrow(internship_data))
#check for the missing values
# Assuming your data frame is named 'internship_data'
# Assuming your data is stored in a data frame called 'internship_data'



# Handle missing values in 'stipend' column
internship_data$stipend[is.na(internship_data$stipend)] <- 0

# Remove commas and convert 'stipend' to numeric
internship_data$stipend <- as.numeric(gsub(",", "", internship_data$stipend))

# Handle missing values in numeric columns
internship_data[is.na(internship_data)] <- 0

# Print the cleaned data
print(internship_data)

#DATA ANALYSIS

mean_stipend <- mean(internship_data$stipend, na.rm =TRUE)
print(mean_stipend)

median_stipend <- median(internship_data$stipend, na.rm =TRUE)
print(median_stipend)

#DATA VISUALIZATION

# Load necessary libraries
library(ggplot2)


# Create a bar plot
top10<- head(internship_data[order(internship_data$stipend, decreasing = TRUE), ], 10)
ggplot(top10, aes(x = reorder(internship_title, stipend), y = stipend)) +
  geom_bar(stat = "identity", fill = "light blue") +
  labs(title = "Internships with highest stipend",
       x = "Internship Title",
       y = "Stipend") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Histogram for location
ggplot(internship_data, aes(x = location)) + 
  geom_bar(fill = "purple", alpha = 0.7) +
  labs(title = "Distribution of Internship Locations", x = "Location", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#pie chart for duration
duration_counts <- table(internship_data$duration)

# Create a data frame for plotting
df <- data.frame(duration = names(duration_counts), count = as.numeric(duration_counts))

# Calculate percentages
df$percentage <- df$count / sum(df$count) * 100

# Create a pie chart with a legend and percentage labels
pie_chart <- ggplot(df, aes(x = "", y = count, fill = duration)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = paste0(sprintf("%.1f", percentage), "%")), position = position_stack(vjust = 0.5)) +
  coord_polar("y") +
  theme_void() +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "right") +
  labs(title = "Distribution of Duration", fill = "Difficulty")

print(pie_chart)


#Line plot
# Count the number of internships per company 
internship_counts <- table(internship_data$company_name)

# Filter companies with more than 3 internships
selected_companies <- names(internship_counts[internship_counts > 5])

# Filter the data for selected companies
selected_data <- internship_data[internship_data$company_name %in% selected_companies, ]

# Plotting the line plot
ggplot(selected_data, aes(x = company_name)) +
  geom_line(stat = "count", aes(group = 1), color = "blue") +
  labs(title = "Number of Internships per Company (Companies with > 5 Internships)", x = "Company Name", y = "Number of Internships") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#LINEAR REGRESSION
# Assuming you have a dataframe named 'data' with columns 'independent_variable' and 'dependent_variable'
# Replace 'independent_variable' and 'dependent_variable' with your actual column names

# Example data (replace this with your actual data)
data <- data.frame(
  independent_variable = c(1, 2, 3, 4, 5),
  dependent_variable = c(2, 4, 5, 4, 5)
)
# Perform linear regression
model <- lm(dependent_variable ~ independent_variable, data = data)
# Display the summary of the regression

summary(model)
