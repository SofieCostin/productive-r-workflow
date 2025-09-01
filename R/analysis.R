

data <- read.csv("https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/data_2.csv")

summary(data)

print(round(mean(subset(na.omit(data), species == "Adelie" & island == "Torgersen")$bill_length_mm), 2))
print(round(mean(subset(na.omit(data), species == "Adelie" & island == "Biscoe")$bill_length_mm), 2))
print(round(mean(subset(na.omit(data), species == "Adelie" & island == "Dream")$bill_length_mm), 2))


# Plot
penguins_clean <- na.omit(data)
plot(penguins_clean$bill_length_mm, penguins_clean$bill_depth_mm, type = "n", xlab = "Bill Length (mm)", ylab = "Bill Depth (mm)", main = "Penguin Bill Dimensions")
points(
  penguins_clean$bill_length_mm[penguins_clean$species == "Adelie"], penguins_clean$bill_depth_mm[penguins_clean$species == "Adelie"],
  col = "red", pch = 16
)
points(penguins_clean$bill_length_mm[penguins_clean$species == "Chinstrap"], penguins_clean$bill_depth_mm[penguins_clean$species == "Chinstrap"], col = "green", pch = 17)
points(penguins_clean$bill_length_mm[penguins_clean$species == "Gentoo"],
  penguins_clean$bill_depth_mm[penguins_clean$species == "Gentoo"],
  col = "blue", pch = 18
)
legend("topright",
  legend = unique(penguins_clean$species),
  col = c(
    "red",
    "green",
    "blue"
  ), pch = c(16, 17, 18)
)



# Using tidyverse ---------------------------------------------------------

library(dplyr)
library(ggplot2)


# read data using readr
data <- readr::read_csv("https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/data_2.csv")

summary(data)

# Calculating mean bill length for different species and islands using dplyr
data %>%
  filter(species == "Adelie") %>%
  group_by(island) %>%
  summarize(mean_bill_length = round(mean(bill_length_mm, na.rm = TRUE), digits = 2))

# Plot using ggplot2
data %>%
  na.omit() %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species, shape = species)) +
  geom_point() +
  labs(x = "Bill Length (mm)", y = "Bill Depth (mm)", title = "Penguin Bill Dimensions") +
  scale_shape_manual(values = c("Adelie" = 16, "Chinstrap" = 17, "Gentoo" = 18))


# import using readxl -----------------------------------------------------


library(readxl)
data <- read_excel("input/data.xlsx", na = "-") %>%
  mutate(
    bill_length_mm = as.numeric(bill_length_mm),
    bill_depth_mm  = as.numeric(bill_depth_mm),
    flipper_length_mm = as.numeric(flipper_length_mm),
    body_mass_g = as.numeric(body_mass_g)
  )


# create a function that multiplies a number by 234

multiply_by_234 <- function(a) {
  return(a*234)
}

multiply_by_234(311)

# create a function that adds two numbers together. use it to compute 3256 + 8934

add_two_numbers <- function(x,y) {
  return(x + y)
}

add_two_numbers(3256, 8934)

# write a function that shows the relationship between bill length and bill depth with a scatterplot. 
#the function should expect two arguments to target a specific island and species

library(tidyverse)
# plot function
create_scatterplot <- function(data, selected_species, selected_island) {
  # Filter the data for the specified species and island
  filtered_data <- data %>%
    na.omit() %>%
    filter(species == selected_species, island == selected_island)
  
  # Create the scatterplot
  plot <- ggplot(
    filtered_data,
    aes(x = bill_length_mm, y = bill_depth_mm, color = species, shape = species)
  ) +
    geom_point() +
    scale_x_continuous(breaks = scales::breaks_width(1)) +  # whole numbers on x-axis
    scale_y_continuous(breaks = scales::breaks_width(1)) +  # whole numbers on y-axis
    labs(
      x = "Bill Length (mm)",
      y = "Bill Depth (mm)",
      title = paste("Penguin Bill Dimensions -", selected_species, "-", selected_island)
    )
  
  return(plot)
}

# Use the function
create_scatterplot(data, "Chinstrap", "Dream")


source("R/functions.R")

# load clean RDS file

data <- readRDS(file = "input/clean_data.rds")

