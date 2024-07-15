#install.packages(dplyr)
# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}
# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA")
lapply(libraries, install_and_load)

# Reading the file into R
data <- read.csv("C:\\Users\\HP\\Downloads\\NSSO68 (2).csv")


# Filtering for ARP
df <- data %>%
  filter(state_1 == "ARP")

# Display dataset info
cat("Dataset Information:\n")
print(names(df))
print(head(df))
print(dim(df))

# Finding missing values
missing_info <- colSums(is.na(df))
cat("Missing Values Information:\n")
print(missing_info)

# Subsetting the data
arpnew <- df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
arpnew$Meals_At_Home <- impute_with_mean(arpnew$Meals_At_Home)

# Finding outliers and removing them
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}

outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  arpnew <- remove_outliers(arpnew, col)
}

# Summarize consumption
arpnew$total_consumption <- rowSums(arpnew[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

# Summarize and display top consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- arpnew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)

# Rename districts and sectors
district_mapping <- c ("4" = "papum pare","8" = "east siang","13" = "tirap","2" = "west kameng","1" = "tawang","15" = "kurungkumey","11" = "lohit","7" = "west siang","12" = "changlang","3" = "east kameng","9"="upper siang","6"="upper subansiri","5"="lower subansiri","16"="lower dibang","14"="anjaw","10"="dibang valley")


sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

arpnew$District <- as.character(arpnew$District)
arpnew$Sector <- as.character(arpnew$Sector)
arpnew$District <- ifelse(arpnew$District %in% names(district_mapping), district_mapping[arpnew$District], arpnew$District)
arpnew$Sector <- ifelse(arpnew$Sector %in% names(sector_mapping), sector_mapping[arpnew$Sector], arpnew$Sector)
View(arpnew)

hist(arpnew$total_consumption, breaks = 10, col = 'purple', border = 'black', 
     xlab = "Consumption", ylab = "Frequency", main = "Consumption Distribution in Arunachal Pradesh  State")

ARP_consumption <- aggregate(total_consumption ~ District, data = arpnew, sum) 
View(ARP_consumption)
??barplot
barplot(ARP_consumption$total_consumption, 
        names.arg = ARP_consumption$District, 
        las = 2, # Makes the district names vertical
        col = 'purple', 
        border = 'black', 
        xlab = "District", 
        ylab = "Total Consumption", 
        main = "Total Consumption per District",
        cex.names = 0.7) # Adjust the size of district names if needed

# b) Plot {'any variable of your choice'} on the Arunachal Pradesh state map using NSSO68.csv data
# Filtering for Arunachal Pradesh
df_arp <- data %>%
  filter(state_1 == "ARP")

# Sub-setting the data
arp_new <- df_arp %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Check for missing values in the subset
cat("Missing Values in Subset:\n")
print(colSums(is.na(arp_new)))

# Impute missing values with mean for specific columns
arp_new$Meals_At_Home <- impute_with_mean(arp_new$Meals_At_Home)

# Check for missing values after imputation
cat("Missing Values After Imputation:\n")
print(colSums(is.na(arp_new)))

# Finding outliers and removing them
outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  arp_new <- remove_outliers(arp_new, col)
}

# Summarize consumption
arp_new$total_consumption <- rowSums(arp_new[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

district_summary <- summarize_consumption("District")
cat("District Consumption Summary:\n")
print(district_summary)

# mapping districts so that meging of the tables will be easier
district_mapping <- c(
  "1"="tawang",
  "2"="west kameng",
  "3"="East Kameng",	
  "4"="Papum Pare *",	
  "5"="Lower Subansiri",	
  "6"="Upper Subansiri",	
  "7"="West Siang",	
  "8"="East Siang",	
  "9"="Upper Siang *",	
  "10"="Dibang Valley",	
  "11"="Lohit",	
  "12"="Changlang",	
  "13"="Tirap",	
  "14"="Anjaw",	
  "15"="Kurungkumey",	
  "16"="Lower Dibang Valley"	
)


arp_new$District <- as.character(arp_new$District)
arp_new$District <- district_mapping[arp_new$District]
#arp_new$District <- ifelse(arp_new$District %in% names(district_mapping), district_mapping[arp_new$District], arp_new$District)
View(arp_new)

# arp_consumption stores aggregate of total consumption district wise
arp_consumption <- aggregate(total_consumption ~ District, data = arp_new, sum) 
View(arp_consumption)

#Plotting total consumption on the Arunachal Pradesh state 

Sys.setenv("SHkaE_RESTORE_SHX" = "YES") 
# Load required libraries
library(sf)
library(ggplot2)

# Read  data map from GeoJSON
data_map <- st_read("C:\\Users\\HP\\OneDrive\\Desktop\\ARUNACHAL PRADESH_DISTRICTS.geojson")

# Plot the  data map
ggplot() +
  geom_sf(data = data_map) +
  labs(title = "Arunachal Pradesh Districts") +
  theme_minimal()



data_map <- st_read("C:\\Users\\HP\\OneDrive\\Desktop\\ARUNACHAL PRADESH_DISTRICTS.geojson") 
View(data_map)

# Rename the column
data_map <- data_map %>% 
  rename (district_$dtname)
  




# merging arp_consumption and data_map tables
data_map_data <- merge(arp_consumption,data_map,by = "District") 
View(data_map_data)

# Plot without labeling district names
ggplot(data_map_data) + 
  geom_sf(aes(fill =total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption by District") 

# Plot with labelled district names
ggplot(data_map_data) + 
  geom_sf(aes(fill = total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption by District") +
  geom_sf_text(aes(label = District, geometry = geometry), size = 3, color = "black")
