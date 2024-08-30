# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Step 1: Load the data from the Excel file
file_path <- "/Users/kabeer/Downloads/Cases registered and their disposal under Dowry Prohibition Act during 2001-2014.xls"
data <- read_excel(file_path)


# Step 2: Clean the dataset
data_clean <- data %>%
  # Convert STATE/UT to lowercase and remove whitespace
  mutate(`STATE/UT` = tolower(trimws(`STATE/UT`))) %>%
  # Filter out rows containing totals
  filter(!(`STATE/UT` %in% c("total (uts)", "total (all-india)", "total (states)"))) %>%
  # Standardize specific STATE/UT names
  mutate(`STATE/UT` = case_when(
    `STATE/UT` %in% c("d & n haveli", "d&n haveli", "daman & diu", "daman and diu", "d n haveli", "daman & diu", "d&n haveli", "daman & diu") ~ 
      "D & N HAVELI AND D & D",
    `STATE/UT` == "jharkhand" ~ "JHARKHAND",
    `STATE/UT` %in% c("delhi ut", "delhi") ~ "DELHI",
    TRUE ~ toupper(`STATE/UT`)  # Convert other STATE/UT names to uppercase
  ))

# Step 3: Summarize the data by year
data_summary <- data_clean %>%
  group_by(Year) %>%
  summarize(
    Sum_Cases_Reported_during_the_year = sum(`Cases reported during the year`, na.rm = TRUE),
    Sum_Cases_Pending_Investigation_from_Previous_Year = sum(`Cases Pending Investigation from Previous Year`, na.rm = TRUE),
    Sum_Total_Cases_For_Trial_during_the_Year = sum(`Total cases for trial during the year`, na.rm = TRUE),
    Sum_Cases_Convicted = sum(`Cases convicted`, na.rm = TRUE),
    Sum_Cases_Pending_Trial_at_the_End_of_the_Year = sum(`Cases pending trial at the end of the year`, na.rm = TRUE),
    Sum_Cases_Declared_False = sum(`Cases declared false on account of mistake of fact or of law`, na.rm = TRUE)
  )

# View the summarized data to ensure it has been computed correctly
head(data_summary)


# Step 4: Visualization - Cases Reported During the Year by STATE/UT
plot1 <- ggplot(data_clean, aes(x = `Year`, y = `Cases reported during the year`, fill = `STATE/UT`)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Cases Reported During the Year by STATE/UT",
       subtitle = "Data showing cases reported under the Dowry Prohibition Act",
       x = "Year",
       y = "Number of Cases Reported",
       fill = "STATE/UT") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

# Display the plot
print(plot1)

# Save the plot as a JPEG file
ggsave("cleaned_cases_reported_by_state_year.jpeg", plot = plot1, width = 12, height = 8, dpi = 300)

# Line chart for Summarized Data by Year
plot2 <- ggplot(data_summary, aes(x = Year)) +
  geom_line(aes(y = Sum_Cases_Reported_during_the_year, color = "Total cases reported during the year"), linetype = "twodash") +
  geom_line(aes(y = Sum_Cases_Pending_Investigation_from_Previous_Year, color = "Total cases pending investigation from the previous year"), , linetype = "dotted") +
  geom_line(aes(y = Sum_Total_Cases_For_Trial_during_the_Year, color = "Total cases for trial during the year"), linetype = "solid") +
  geom_line(aes(y = Sum_Cases_Convicted, color = "Total cases convicted during the year"), linewidth = 1) +
  geom_line(aes(y = Sum_Cases_Pending_Trial_at_the_End_of_the_Year, color = "Total cases pending trial at the end of the year"), linewidth = 1) +
  geom_line(aes(y = Sum_Cases_Declared_False, color = "Total cases declared false on account of mistake of fact or of law"), linetype = "dotdash") +
  theme_minimal() +
  labs(
    title = "Yearly Summary of Cases Under the Dowry Prohibition Act",
    subtitle = "Comparison of various case metrics from 2001 to 2014",
    x = "Year",
    y = "Number of Cases",
    color = "Case Metrics"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Display the plot
print(plot2)

# Save the plot as a JPEG file
ggsave("yearly_summary_of_cases.jpeg", plot = plot2, width = 12, height = 8, dpi = 300)
