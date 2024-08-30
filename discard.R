# Step 1: Download the PDF
pdf_url <- "https://figshare.com/ndownloader/files/37708152"
dest_file <- "Spurdog_Report.pdf"
download.file(pdf_url, destfile = dest_file, mode = "wb")

# Confirm the download
if (file.exists(dest_file)) {
  cat("Download successful. The file is saved as:", dest_file, "\n")
} else {
  cat("Download failed.\n")
}

# Step 2: Import the Excel data into R

# Install necessary packages if not already installed
install.packages("readxl")
install.packages("ggplot2")

# Load the packages
library(readxl)
library(ggplot2)

# Define the file path (adjust this if your file is saved elsewhere)
file_path <- "/Users/kabeer/Desktop/R data/spurdog_discards.xlsx"

# Import the Excel data into R
spurdog_discards <- read_excel(file_path)

# Step 3: Clean the Data
# Create a new column to indicate if the discard data was supplemented
spurdog_discards$Supplemented_Data <- spurdog_discards$Year %in% c(2013, 2014, 2015)

# Clean the Discards (Tonnes) column by removing "**" and converting to numeric
spurdog_discards$Discards_Tonnes <- as.numeric(gsub("\\*\\*", "", spurdog_discards$`Discards (Tonnes)`))

# Clean the Landings (Tonnes) column by ensuring it's numeric
spurdog_discards$Landings_Tonnes <- as.numeric(spurdog_discards$`Landings (Tonnes)`)

# Convert the Year column to a factor to prevent fractional values on the x-axis
spurdog_discards$Year <- as.factor(spurdog_discards$Year)

install.packages("ggimage")

library(ggplot2)
library(ggimage)

# Paths to your fish images
landings_fish <- "/Users/kabeer/Downloads/landings fish.png"  
discards_fish <- "/Users/kabeer/Downloads/discards fish.png"

# Create the plot using fish images
p <- ggplot(spurdog_discards, aes(x = Year)) +
  geom_image(aes(y = Landings_Tonnes, image = landings_fish), size = 0.1) +
  geom_image(aes(y = Discards_Tonnes, image = discards_fish), size = 0.1) +
  geom_text(aes(y = Discards_Tonnes, label = ifelse(Supplemented_Data, "**", "")), 
            vjust = -1, color = "black") +
  labs(title = "Landings and Discards of Spurdog (2013-2021)",
       subtitle = "** Discards data supplemented with averages from UK England & Wales (2010, 2012, 2016–2019)",
       x = "Year",
       y = "Tonnes",
       caption = "Data Source: ICES Advice 2022") +
  theme_minimal()

# Print the plot
print(p)



