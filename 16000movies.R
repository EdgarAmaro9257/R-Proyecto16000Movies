library(readr)
library(stringr)

# 2. Data Cleaning and Understanding
# Import Dataset
X16k_Movies <- read_csv("data/16k_Movies.csv")

# View Dataset
View(X16k_Movies)

# Display the first few rows to understand the structure
head(X16k_Movies)

# Display the structure of the dataset
str(X16k_Movies)

# Summary statistics for a quick overview
summary(X16k_Movies)

# Convert 'Title' and 'Rating' columns into factors
X16k_Movies$Title <- as.factor(X16k_Movies$Title)
X16k_Movies$Rating <- as.factor(X16k_Movies$Rating)
# Verify changes
summary(X16k_Movies)

# Load Libraries
library(dplyr)
library(ggplot2)

# Check for missing values
sapply(X16k_Movies, function(x) sum(is.na(x)))
# Clean column names (optional, if needed)
colnames(X16k_Movies) <- make.names(colnames(X16k_Movies), unique = TRUE)

# Check column names to verify
colnames(X16k_Movies)

# 3. Descriptive Analysis using dplyr
# Summary statistics for numerical variables
X16k_Movies %>%
  select(Rating, Duration, `No.of.Persons.Voted`) %>%
  summary()
# Most common genres
X16k_Movies %>%
  count(Genres, sort = TRUE)
# Distribution of Ratings
X16k_Movies %>%
  count(Rating, sort = TRUE)

# 4. Visualizations using ggplot2
# 4.1. Distribution of Movie Ratings
# Bar plot of movie ratings
ggplot(X16k_Movies, aes(x = Rating)) +
  geom_bar(fill = "blue") +
  labs(title = "Distribution of Movie Ratings", x = "Rating", y = "Count") +
  theme_minimal()

#4.2. Distribution of Movie Duration
# Convert Duration from "9 h 32 m" to total minutes
X16k_Movies$Duration <- str_extract(X16k_Movies$Duration, "\\d+ h \\d+ m") %>%
  str_replace_all(" h ", ":") %>%
  str_replace_all(" m", "") %>%
  strptime(format = "%H:%M") %>%
  as.numeric(units = "mins")

# View the modified duration column
head(X16k_Movies$Duration)

# Create histogram of movie duration
ggplot(X16k_Movies, aes(x = Duration)) +
  geom_histogram(bins = 30, fill = "green", color = "black") +
  labs(title = "Distribution of Movie Duration", x = "Duration (minutes)", y = "Frequency") +
  theme_minimal()

#4.3 Relationship between Number of Votes and Rating
# Boxplot of number of votes by rating
ggplot(X16k_Movies, aes(x = Rating, y = No.of.Persons.Voted)) +
  geom_boxplot() +
  labs(title = "Number of Votes by Rating", x = "Rating", y = "Number of Votes") +
  theme_minimal() +
  scale_y_log10() # Log scale if necessary 

# 5 Statistical Analysis
# Correlation between Duration and Number of Votes
cor(X16k_Movies$Duration, X16k_Movies$`No.of.Persons.Voted`, use = "complete.obs")


# 6. Building a Linear Prediction Model
# Convert Rating to numeric for modeling
X16k_Movies$Rating_numeric <- as.numeric(as.character(X16k_Movies$Rating))
# Linear model to predict Number of Persons Voted
model <- lm(`No.of.Persons.Voted` ~ Rating_numeric + Duration, data = X16k_Movies)
# Summary of the model
summary(model)

# Predict number of votes (productivity)
X16k_Movies$predicted_votes <- predict(model, newdata = X16k_Movies)
# View predicted values
head(X16k_Movies[, c("Title", "predicted_votes")])

# Plot residuals
par(mfrow = c(2, 2))
plot(model)
