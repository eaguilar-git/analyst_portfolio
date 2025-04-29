#+ In this R file I am creating the visualizations that then I will include in the R-markdown file. 
# Slides Assignment
# Edgar Aguilar
# 04.8.2025

# 1. LOAD PACKAGES -------------------------------
library(tidyverse)
library(scales)
library(lfe)
library(modelsummary)
library(gt)
library(data.table)

# 2. LOAD THE DATASET ----------------
data_slides <- readxl::read_xlsx("slides_utilities_data.xlsx")
colnames(data_slides)

# 3. DESCRIPTIVE STATISTICS ----------
# Here I want to count number of utilities in the sample,count number of countries, and count number utilities by income group

# Counts
## Count number of utilities (using unique "Utility short name")
num_utilities <- length(unique(data_slides$`Utility short name`))
cat("Number of utilities:", num_utilities, "\n")

## Count number of countries (using unique "Country")
num_countries <- length(unique(data_slides$Country))
cat("Number of countries:", num_countries, "\n")

## Count number of utilities by income group (using table)
utilities_income_count <- table(data_slides$`Income Group`)
print("Utilities by Income Group:")
print(utilities_income_count)


# Now I want to create a bar chart that shows the number of utilities per income group
## Define the desired order for the income groups
income_levels <- c("Low income", "Lower middle income", "Upper middle income", "High income")

## Create a frequency table for the ordered Income Group
utilities_income_count <- table(factor(data_slides$`Income Group`, levels = income_levels))

## Create the bar chart and capture the x-positions of the bars
bar_positions <- barplot(utilities_income_count,
                         main = "Power Utilities by Income Group",
                         xlab = "Income Group",
                         ylab = "Count of Utilities",
                         col = "steelblue",
                         border = "white",
                         ylim = c(0, max(utilities_income_count) * 1.15))

## Annotate each bar with its count value
text(x = bar_positions,
     y = utilities_income_count,
     labels = utilities_income_count,
     pos = 3,      # Places the text above the bar
     cex = 0.8,
     col = "black")

# 4. ANALYSIS 1 ----------
# The first thing that I want to show is from the sample how many utilities are recovering operation and debt costs and then by region. 

# Pie chart: Number of utilities with average cost recovery above one and below 1
## Convert "Average Cost Recovery" to numeric if not already numeric
data_slides$`Average Cost Recovery` <- as.numeric(data_slides$`Average Cost Recovery`)

## Create a new grouping variable based on the threshold
data_slides$recovery_group <- ifelse(data_slides$`Average Cost Recovery` > 1, "Above 100%", "Below 100%")

## Count the number of utilities in each group
recovery_counts <- table(data_slides$recovery_group)

## Print the counts for verification
print("Utilities Achieving Full Cost Recovery")
print(recovery_counts)

## Create a pie chart of the counts
pie(recovery_counts,
    labels = paste0(names(recovery_counts), " (", recovery_counts, ")"),
    main = "Utilities by Average Cost Recovery",
    col = c("lightblue", "lightgreen"))


# Box plot: Shows cost recovery by income group

## Filter out values above 3 in "Average Cost Recovery"
data_filtered <- subset(data_slides, `Average Cost Recovery` <= 3)

## Convert Average Cost Recovery to percentage by multiplying by 100
data_filtered$`Average Cost Recovery (%)` <- data_filtered$`Average Cost Recovery` * 100

## Define the desired order for Income Group and set factor levels
desired_levels <- c("Low income", "Lower middle income", "Upper middle income", "High income")
data_filtered$`Income Group` <- factor(data_filtered$`Income Group`, levels = desired_levels)

## Create the box plot of Average Cost Recovery (as percentage) by Income Group
bp <- boxplot(`Average Cost Recovery (%)` ~ `Income Group`, 
              data = data_filtered,
              main = "Average Cost Recovery by Income Group",
              xlab = "Income Group",
              ylab = "Average Cost Recovery (%)",
              col = "lightblue",
              border = "darkblue")

## Compute group means and medians by Income Group (using the percentage variable)
group_means <- tapply(data_filtered$`Average Cost Recovery (%)`, data_filtered$`Income Group`, mean, na.rm = TRUE)
group_medians <- tapply(data_filtered$`Average Cost Recovery (%)`, data_filtered$`Income Group`, median, na.rm = TRUE)

## Overlay markers for mean (red circles) and median (blue triangles)
points(1:length(desired_levels), group_means, pch = 19, col = "red")    # Mean marker: red circle
points(1:length(desired_levels), group_medians, pch = 17, col = "blue")   # Median marker: blue triangle

## Add a legend to the plot in the top right corner
legend("topright", legend = c("Mean", "Median"), 
       pch = c(19, 17), col = c("red", "blue"))


# 5. ANALYSIS 2 --------
# I want to see if there is any relationship between cost recovery and asset growth. 
# Specificallly I want to see if utilities with higher average cost recovery in the period 2012-2022 have a higer growth rate for total assets.
# For this I am going to run a regression and build box plot again. 

# Regression. Explanatory variable: Average Cost Recovery. Dependent variable: Total Asset Growth Rate

## Ensure the variables are numeric (if not already)
data_slides$`Average Cost Recovery` <- as.numeric(data_slides$`Average Cost Recovery`)
data_slides$`Total Assets Growth Rate` <- as.numeric(data_slides$`Total Assets Growth Rate`)

## Run the regression
model <- lm(`Total Assets Growth Rate` ~ `Average Cost Recovery`, data = data_slides)

## View the regression summary
summary(model)


# Box plot: I am showing asset growth rate by cost recovery bucket. 

## Ensure the variable is numeric
data_slides$`Average Cost Recovery` <- as.numeric(data_slides$`Average Cost Recovery`)

## Create buckets for Average Cost Recovery
data_slides$ACR_bucket <- cut(data_slides$`Average Cost Recovery`,
                              breaks = c(-Inf, 0.5, 0.75, 1, Inf),
                              labels = c("Below 50%", "Between 50%-75%", "Between 75%-100%", "Above 100%"),
                              right = FALSE)

## Filter out observations where Total Assets Growth Rate exceeds 0.40 (i.e., 40%)
data_filtered <- subset(data_slides, `Total Assets Growth Rate` <= 0.40)

## Convert Total Assets Growth Rate to percentage by multiplying by 100
data_filtered$`Total Assets Growth Rate (%)` <- data_filtered$`Total Assets Growth Rate` * 100

## Create the box plot of Total Assets Growth Rate (%) by Average Cost Recovery bucket
bp <- boxplot(`Total Assets Growth Rate (%)` ~ ACR_bucket, 
              data = data_filtered,
              main = "Total Assets Growth Rate by Average Cost Recovery Bucket",
              xlab = "Average Cost Recovery Bucket",
              ylab = "Total Assets Growth Rate (%)",
              col = "lightblue",
              border = "darkblue")

## Compute mean and median of Total Assets Growth Rate (%) for each ACR bucket
means <- tapply(data_filtered$`Total Assets Growth Rate (%)`, data_filtered$ACR_bucket, mean, na.rm=TRUE)
medians <- tapply(data_filtered$`Total Assets Growth Rate (%)`, data_filtered$ACR_bucket, median, na.rm=TRUE)

## Get the bucket positions (each box is drawn at positions 1, 2, 3, ...)
bucket_positions <- 1:length(means)

## Add mean markers as red circles (pch = 19)
points(bucket_positions, means, col = "red", pch = 19)

## Add median markers as blue triangles (pch = 18)
points(bucket_positions, medians, col = "blue", pch = 18)

## Add a legend to the plot
legend("topright", legend = c("Mean", "Median"), 
       col = c("red", "blue"), pch = c(19, 18))