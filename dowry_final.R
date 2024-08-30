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

# Step 4: Visualization - Cases Reported During the Year by STATE/UT
plot1 <- ggplot(data_clean, aes(x = `Year`, y = `Cases reported during the year`, fill = `STATE/UT`)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal(base_size = 15) +  # Increase base font size for better readability
  labs(title = "Cases Reported During the Year by STATE/UT",
       subtitle = "Data showing cases reported under the Dowry Prohibition Act",
       x = "Year",
       y = "Number of Cases Reported",
       fill = "STATE/UT") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),  # Increase title size
    plot.subtitle = element_text(hjust = 0.5, size = 18),  # Increase subtitle size
    legend.title = element_text(size = 12),  # Reduce legend title size
    legend.text = element_text(size = 10),   # Reduce legend text size
    legend.position = "right",
    legend.key.size = unit(0.6, "cm")  # Reduce legend key size
  )

# Display the plot
print(plot1)

# Save the plot as a JPEG file with improved size and DPI
ggsave("larger_cases_reported_by_state_year.jpeg", plot = plot1, width = 20, height = 12, dpi = 400)


# Line chart for Summarized Data by Year
plot2 <- ggplot(data_summary, aes(x = Year)) +
  geom_line(aes(y = Sum_Cases_Reported_during_the_year, color = "Total cases reported during the year"), linetype = "twodash") +
  geom_line(aes(y = Sum_Cases_Pending_Investigation_from_Previous_Year, color = "Total cases pending investigation (previous year)"), linetype = "dotted") +
  geom_line(aes(y = Sum_Total_Cases_For_Trial_during_the_Year, color = "Total cases for trial during the year"), linetype = "solid") +
  geom_line(aes(y = Sum_Cases_Convicted, color = "Total cases convicted during the year")) +
  geom_line(aes(y = Sum_Cases_Pending_Trial_at_the_End_of_the_Year, color = "Total cases pending trial (end of the year)")) +
  geom_line(aes(y = Sum_Cases_Declared_False, color = "Total cases declared false"), linetype = "dotdash") +
  theme_minimal(base_size = 18) +  # Increase base font size for better readability
  labs(
    title = "Yearly Summary of Cases Under the Dowry Prohibition Act",
    subtitle = "Comparison of various case metrics from 2001 to 2014",
    x = "Year",
    y = "Number of Cases",
    color = "Case Metrics"
  ) +
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold", margin = margin(b = 15)),  
    plot.subtitle = element_text(hjust = 0, size = 14, margin = margin(b = 15)),  
    axis.text.x = element_text(size = 12),  # Maintain x-axis text size (tick labels)
    axis.text.y = element_text(size = 12),  # Maintain y-axis text size (tick labels)
    axis.title.x = element_text(size = 14),  # Change the font size of the x-axis title ("Year")
    axis.title.y = element_text(size = 14),  # Change the font size of the y-axis title ("Number of Cases")
    legend.title = element_text(size = 12),  # Maintain legend title size
    legend.text = element_text(size = 10),   # Maintain legend text size
    plot.margin = unit(c(1, 1, 1, 1.5), "cm")  # Adjust plot margins to allow space for the title
  )

# Display the plot
print(plot2)

# Save the plot as a JPEG file with the same size and DPI
ggsave("yearly_summary_of_cases_left_aligned_smaller_title_subtitle.jpeg", plot = plot2, width = 20, height = 12, dpi = 400)



#Top 5 States
# Summarize total cases reported by state
top_states <- data_clean %>%
  group_by(`STATE/UT`) %>%
  summarize(Total_Cases_Reported = sum(`Cases reported during the year`, na.rm = TRUE)) %>%
  arrange(desc(Total_Cases_Reported)) %>%
  top_n(5, Total_Cases_Reported)

# View the top 5 states
print(top_states)

# Filter data to include only the top 5 states
top_states_data <- data_clean %>%
  filter(`STATE/UT` %in% top_states$`STATE/UT`)

# Facet Wrap for Top 5 States by Number of Cases
plot_top5_facet <- ggplot(top_states_data, aes(x = `Year`, y = `Cases reported during the year`, fill = `STATE/UT`)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ `STATE/UT`, scales = "free_y") +  # Create separate plots for each of the top 5 states
  theme_minimal(base_size = 15) +  # Increase base font size for better readability
  labs(title = "Cases Reported During the Year by Top 5 STATES/UTs",
       subtitle = "Top 5 states with the highest number of cases reported under the Dowry Prohibition Act",
       x = "Year",
       y = "Number of Cases Reported") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        legend.position = "none")  # Remove legend since facets make it redundant

# Display the plot
print(plot_top5_facet)

# Save the plot as a JPEG file with improved size and DPI
ggsave("top5_states_facet_cases_reported_by_year.jpeg", plot = plot_top5_facet, width = 16, height = 12, dpi = 400)

# Waffle Chart
install.packages("waffle")
install.packages("RColorBrewer")

# Load necessary libraries
library(waffle)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Filter out states/UTs with fewer than 1,000 cases reported in total
  state_summary_filtered <- state_summary %>%
  filter(Total_Cases_Reported >= 1000)

# Scale down the cases for the waffle chart (e.g., 1 square = 1000 cases)
state_summary_filtered$scaled_cases <- round(state_summary_filtered$Total_Cases_Reported / 1000)

# Create a named vector for the waffle chart
cases_vector_filtered <- setNames(state_summary_filtered$scaled_cases, state_summary_filtered$`STATE/UT`)

# Generate a color palette with enough colors for each state
num_colors <- length(cases_vector_filtered)
colors <- brewer.pal(n = min(num_colors, 12), name = "Set3")  # Use "Set3" palette, with a maximum of 12 colors
if (num_colors > 12) {
  colors <- colorRampPalette(colors)(num_colors)  # Expand palette if more than 12 states
}

# Create the waffle chart with the filtered data
waffle_chart_filtered <- waffle(cases_vector_filtered, rows = 10, size = 0.5, colors = colors)

# Convert the waffle chart to a ggplot object for annotation
waffle_plot_filtered <- waffle_chart_filtered +
  labs(
    title = "Distribution of Cases Reported Under the Dowry Prohibition Act",
    subtitle = "Each square represents approximately 1,000 cases reported from 2001 to 2014",
    caption = "Source: Dowry Prohibition Act Data, 2001-2014"
  ) +
  theme(
    plot.title = element_text(hjust = 0, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0, size = 14),
    plot.caption = element_text(hjust = 0, size = 10),  # Align the caption to the left
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )


# Display the waffle plot
print(waffle_plot_filtered)

# Save the annotated waffle chart as an image
ggsave("filtered_waffle_chart.png", plot = waffle_plot_filtered, width = 12, height = 8, dpi = 300)

XXXXXXXX

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Summarize the data to get the total cases reported during the year for each State/UT
data_summary <- data_clean %>%
  group_by(`STATE/UT`) %>%
  summarize(Total_Cases_Reported = sum(`Cases reported during the year`, na.rm = TRUE)) %>%
  mutate(Percentage = (Total_Cases_Reported / sum(Total_Cases_Reported)) * 100)

# Create the pie chart
pie_chart <- ggplot(data_summary, aes(x = "", y = Percentage, fill = `STATE/UT`)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "none") + # Remove legend to add custom labels
  labs(title = "Percentage of Cases Reported During the Year by State/UT") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))

# Add labels to the pie chart
pie_chart <- pie_chart +
  geom_text(aes(label = ifelse(Percentage > 3, 
                               paste0(`STATE/UT`, "\n", round(Percentage, 1), "%"), 
                               "")),
            position = position_stack(vjust = 0.5), size = 3) +
  geom_text(aes(label = ifelse(Percentage <= 3, 
                               paste0(`STATE/UT`, "\n", round(Percentage, 1), "%"), 
                               "")),
            position = position_stack(vjust = 0.5), size = 3, hjust = 1.2)  # Adjusted hjust for better positioning

# Display the pie chart
print(pie_chart)

# Save the pie chart as a JPEG file
ggsave("cases_reported_percentage_pie_chart.jpeg", plot = pie_chart, width = 10, height = 8, dpi = 300)


