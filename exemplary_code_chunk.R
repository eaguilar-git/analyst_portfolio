## Exemplary Code Chunk: Data Envelopment Analysis (DEA) for Utility Benchmarking

#+ Introduction
#+ 
#+ This script demonstrates how I implemented Data Envelopment Analysis (DEA) to benchmark the relative efficiency 
#+ of electricity utilities across 37 countries. It includes the full workflow: preparing input/output indicators, 
#+ reshaping and cleaning the data, executing the DEA model using the Benchmarking package, and merging the efficiency 
#+ scores with market structure and access-to-electricity indicators.
#+ 
#+ This type of analysis is particularly valuable for evaluating the operational efficiency of public 
#+ utilities, and it supports comparative policy insights on regulation, unbundling, and competition.

# Library Set-up
# Core libraries for data wrangling and reshaping
library(tidyverse)
library(readxl)
library(tidyr)
library(dplyr)
library(writexl)
library(knitr)
library(kableExtra)

# DEA methodology
install.packages("Benchmarking")
library(Benchmarking)

# Step 1: Load and inspect data ----
df <- read_excel("complete_upbeat.xlsx")
unique_indicators <- df %>% pull(Indicators) %>% unique()
print(unique_indicators)

# Step 2: Filter, Aggregate, and Pivot to Wide Format ----
selected_indicators <- c(
  "Number of employees", "Total Assets_USDvalue(m)",
  "Net Profit Margin", "Customers",
  "Distribution Losses",
  "Operating and debt service cost recovery (billed revenue), excluding subsidies (%)"
)

agg_df <- df %>%
  filter(Indicators %in% selected_indicators) %>%
  group_by(...other_identifier_columns..., Indicators) %>%
  summarize(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Indicators, values_from = mean_value)

# Step 3: Finalize and Clean DEA Inputs ----
final_df <- agg_df %>%
  select(...columns...) %>%
  drop_na(...required DEA indicators...) %>%
  mutate(`Transformed Distribution Losses` = 1 - `Distribution Losses`)

write_xlsx(final_df, "data_for_DEA.xlsx")

# Step 4: Run DEA Model ----
df <- read_excel("data_for_DEA.xlsx")
X <- as.matrix(df %>% select(`Number of employees`, `Total Assets_USDvalue(m)`))
Y <- as.matrix(df %>% select(`Net Profit Margin`, `Customers`, `Transformed Distribution Losses`))

dea_result <- dea(X, Y, RTS = "crs", ORIENTATION = "in")
df$Efficiency <- dea_result$eff
write_xlsx(df, "DEA_Results.xlsx")

# Step 5: Harmonize and Merge Market Structure Data ----
market_data <- read_excel("market_structure_data.xlsx") %>% filter(Year == 2012)

# Harmonize country names for join
df <- df %>%
  mutate(Country = recode(Country,
                          "Côte d’Ivoire" = "Ivory Coast", ...
  ))

# Join on Year and Country
df_combined <- left_join(df, market_data, by = c("Country", "Year"))
write_xlsx(df_combined, "DEA_Results_Market_Structure.xlsx")


# Step 6: Merge with Electricity Data ----
access_data <- read_excel("access_data.xlsx") %>%
  mutate(`Country Name` = recode(`Country Name`, "Cote d'Ivoire" = "Ivory Coast", ...))

df_combined <- read_excel("DEA_Results_Market_Structure.xlsx")
final_df <- left_join(df_combined, access_data, by = c("Country" = "Country Name")) %>%
  rename(`Access 2022` = `2022`)

write_xlsx(final_df, "DEA_Results_Market_Structure_with_Access.xlsx")


# Step 7; Rename and Reformat Final Dataset ----
df <- read_excel("DEA_Results_Market_Structure_with_Access.xlsx") %>%
  rename(
    `Utility Long Name` = `Utility Name`,
    `Total Assets` = `Total Assets_USDvalue(m)`,
    ...
  ) %>%
  select(-`Lending category`) %>%
  relocate(Ownership, .after = `Utility Short Name`)

write_csv(df, "Efficiency_Markets_Access.csv")



