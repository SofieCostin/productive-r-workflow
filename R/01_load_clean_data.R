library(readxl)

data <- read_excel("input/data.xlsx")
summary(data)

# Remove rows 23 and 48
clean_data <- data %>%
  slice(-c(23, 48))

# Check result
summary(clean_data)

# save in RDS format
