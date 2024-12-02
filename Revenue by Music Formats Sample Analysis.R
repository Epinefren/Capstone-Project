# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)

# Step 1: Data Cleaning and Preprocessing
# Inspect the first few rows and structure of the data
revenuedata <- read.csv("US Recorded Music Revenues by Format.csv")
head(revenuedata)
str(revenuedata)

# Convert Year column to integer format if necessary
revenuedata$Year <- as.integer(revenuedata$Year)

# Step 2: Revenue Trend Analysis for Licensed Rights Formats
# Define licensed rights formats
licensed_formats <- c("DVD Audio", "Kiosk", "Other Ad-supported streaming",
                      "Ringtones & Ringbacks", "Synchronization", "SoundExchange Distributions")

# Filter data for the licensed rights formats
licensed_revenue_data <- filter(revenuedata, Format %in% licensed_formats)

# Summarize revenue for each licensed format over time
licensed_revenue_trends <- group_by(licensed_revenue_data, Year, Format)
licensed_revenue_trends <- summarize(licensed_revenue_trends, TotalRevenue = sum(Revenue, na.rm = TRUE))

# Plot the revenue trend for each licensed format
ggplot(licensed_revenue_trends, aes(x = Year, y = TotalRevenue, color = Format)) +
  geom_line(size = 1) +
  labs(title = "Revenue Trends for Licensed Rights Formats Over Time",
       x = "Year", y = "Total Revenue (in Millions)") +
  theme_minimal()

# Step 3: Licensed Rights Revenue Contribution
# Calculate the total revenue for each year across all licensed formats
total_licensed_revenue_per_year <- group_by(licensed_revenue_trends, Year)
total_licensed_revenue_per_year <- summarize(total_licensed_revenue_per_year, YearlyTotal = sum(TotalRevenue))

# Join the total licensed revenue back to the revenue trends
licensed_revenue_trends <- left_join(licensed_revenue_trends, total_licensed_revenue_per_year, by = "Year")

# Calculate the percentage contribution of each licensed format
licensed_revenue_trends$PercentageContribution <- (licensed_revenue_trends$TotalRevenue / licensed_revenue_trends$YearlyTotal) * 100

# Plot percentage contribution over time by licensed format
ggplot(licensed_revenue_trends, aes(x = Year, y = PercentageContribution, fill = Format)) +
  geom_area(alpha = 0.6) +
  labs(title = "Percentage Contribution of Revenue by Licensed Format",
       x = "Year", y = "Percentage of Total Revenue") +
  theme_minimal()

# Step 4: Year-over-Year Growth Rates for Licensed Rights Formats
licensed_revenue_growth <- arrange(licensed_revenue_trends, Year)
licensed_revenue_growth <- group_by(licensed_revenue_growth, Format)
licensed_revenue_growth <- mutate(licensed_revenue_growth, YearlyGrowth = (TotalRevenue / lag(TotalRevenue) - 1) * 100)

# Plot the year-over-year growth rates for licensed formats
ggplot(licensed_revenue_growth, aes(x = Year, y = YearlyGrowth, color = Format)) +
  geom_line(size = 1) +
  labs(title = "Year-over-Year Growth Rates for Licensed Rights Formats",
       x = "Year", y = "Growth Rate (%)") +
  theme_minimal()

# Step 5: Visualization Example for a Specific Year (Pie Chart)
year_specific <- 2018  # Change this to the desired year

# Filter data for the specific year and licensed formats
revenue_by_format <- filter(licensed_revenue_trends, Year == year_specific)

# Check if the filtered data has any rows
if (nrow(revenue_by_format) > 0) {
  # Create a pie chart of revenue distribution by licensed format for the specified year
  ggplot(revenue_by_format, aes(x = "", y = TotalRevenue, fill = Format)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    labs(title = paste("Revenue Distribution by Licensed Format for", year_specific),
         y = "Revenue") +
    theme_minimal()
} else {
  # Print a message if there's no data for the specified year
  print(paste("No licensed rights revenue data available for the year", year_specific))
}

# Comparative Analysis of SoundExchange Distributions vs. Direct Record Sales
# Step 6: Define Direct Record Sales Formats
direct_record_sales_formats <- c("CD", "Downloads")  # Add other direct sales formats if present

# Step 7: Filter Data for Comparative Analysis
# Extract revenue for SoundExchange Distributions
soundexchange_revenue <- filter(revenuedata, Format == "SoundExchange Distributions")

# Extract revenue for Direct Record Sales formats
direct_sales_revenue <- filter(revenuedata, Format %in% direct_record_sales_formats)

# Step 8: Summarize Revenue for Each Category Over Time
# Summarize SoundExchange Revenue
soundexchange_trends <- group_by(soundexchange_revenue, Year)
soundexchange_trends <- summarize(soundexchange_trends, SoundExchangeRevenue = sum(Revenue, na.rm = TRUE))

# Summarize Direct Record Sales Revenue
direct_sales_trends <- group_by(direct_sales_revenue, Year)
direct_sales_trends <- summarize(direct_sales_trends, DirectSalesRevenue = sum(Revenue, na.rm = TRUE))

# Merge the two summaries into one dataframe
comparative_revenue_trends <- full_join(soundexchange_trends, direct_sales_trends, by = "Year")

# Step 9: Plot Revenue Trends Comparing SoundExchange vs. Direct Sales
ggplot(comparative_revenue_trends, aes(x = Year)) +
  geom_line(aes(y = SoundExchangeRevenue, color = "SoundExchange Distributions"), size = 1) +
  geom_line(aes(y = DirectSalesRevenue, color = "Direct Record Sales"), size = 1) +
  labs(title = "Revenue Trends: SoundExchange Distributions vs. Direct Record Sales",
       x = "Year", y = "Total Revenue (in Millions)",
       color = "Revenue Category") +
  theme_minimal()

# Step 10: Calculate Percentage Contribution of Each Category Per Year
# Calculate total revenue per year for both categories
comparative_revenue_trends <- comparative_revenue_trends %>%
  mutate(TotalRevenue = SoundExchangeRevenue + DirectSalesRevenue,
         SoundExchangePercentage = (SoundExchangeRevenue / TotalRevenue) * 100,
         DirectSalesPercentage = (DirectSalesRevenue / TotalRevenue) * 100)

# Step 11: Plot Percentage Contribution Over Time
# Reshape data for easier plotting
library(tidyr)
comparative_percentage <- comparative_revenue_trends %>%
  select(Year, SoundExchangePercentage, DirectSalesPercentage) %>%
  pivot_longer(cols = c("SoundExchangePercentage", "DirectSalesPercentage"),
               names_to = "Category", values_to = "Percentage") %>%
  mutate(Category = recode(Category,
                           "SoundExchangePercentage" = "SoundExchange Distributions",
                           "DirectSalesPercentage" = "Direct Record Sales"))

ggplot(comparative_percentage, aes(x = Year, y = Percentage, fill = Category)) +
  geom_area(alpha = 0.6) +
  labs(title = "Percentage Contribution: SoundExchange Distributions vs. Direct Record Sales",
       x = "Year", y = "Percentage of Total Revenue") +
  theme_minimal()

# Step 12: Calculate Year-over-Year Growth Rates for Each Category
comparative_revenue_growth <- comparative_revenue_trends %>%
  arrange(Year) %>%
  group_by() %>%
  mutate(SoundExchangeGrowth = (SoundExchangeRevenue / lag(SoundExchangeRevenue) - 1) * 100,
         DirectSalesGrowth = (DirectSalesRevenue / lag(DirectSalesRevenue) - 1) * 100)

# Step 13: Plot Year-over-Year Growth Rates
ggplot(comparative_revenue_growth, aes(x = Year)) +
  geom_line(aes(y = SoundExchangeGrowth, color = "SoundExchange Distributions"), size = 1) +
  geom_line(aes(y = DirectSalesGrowth, color = "Direct Record Sales"), size = 1) +
  labs(title = "Year-over-Year Growth Rates: SoundExchange vs. Direct Sales",
       x = "Year", y = "Growth Rate (%)",
       color = "Revenue Category") +
  theme_minimal()

# Step 14: Visualization Example for a Specific Year (Pie Chart)
year_specific_comparative <- 2018  # Change this to the desired year

# Filter data for the specific year
comparative_revenue_specific <- filter(comparative_revenue_trends, Year == year_specific_comparative)

# Check if there's data for the specific year
if (nrow(comparative_revenue_specific) > 0) {
  # Create a dataframe for pie chart
  pie_data <- data.frame(
    Category = c("SoundExchange Distributions", "Direct Record Sales"),
    Revenue = c(comparative_revenue_specific$SoundExchangeRevenue,
                comparative_revenue_specific$DirectSalesRevenue)
  )
  
  # Create a pie chart
  ggplot(pie_data, aes(x = "", y = Revenue, fill = Category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    labs(title = paste("Revenue Distribution: SoundExchange vs. Direct Sales for", year_specific_comparative),
         y = "Revenue") +
    theme_minimal()
} else {
  # Print a message if there's no data for the specified year
  print(paste("No comparative revenue data available for the year", year_specific_comparative))
}
# Export data for licensing revenue trends
write.csv(licensed_revenue_trends, "licensed_revenue_trends.csv", row.names = FALSE)

# Export percentage contribution data to CSV
write.csv(licensed_revenue_trends, "licensed_revenue_trends_with_percentage.csv", row.names = FALSE)

# Export data for year-over-year growth
write.csv(licensed_revenue_growth, "licensed_revenue_growth.csv", row.names = FALSE)

# Export data for comparative revenue trends
write.csv(comparative_revenue_trends, "comparative_revenue_trends.csv", row.names = FALSE)
