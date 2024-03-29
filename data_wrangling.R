# Analysis of GHG emissions by meal
library(dplyr)
library(plotly)
library(ggplot2)
library(shiny)
library(sf)
library(spData)

simple_df <- read.csv("./data/simple_ghg_food.csv")
global_df <- read.csv("./data/GHG_Foods_Global.csv", stringsAsFactors = FALSE)
by_food <- read.csv("./data/recipe_calculations.csv")

#----Works with larger df to simplify to ghg emissions by product-------------

global_df$Food.type <- trimws(global_df$Food.type)

global_df$Food.type[global_df$Food.type == "Ling Common"] <- "Ling"

global_df_ghg <- global_df %>%
  rename(Product = Food.type) %>%
  rename(CO2 = kg.CO2.eq.kg.produce..BFM.or.L.after.conversion) %>%
  mutate(CO2 = as.character(CO2)) %>%
  mutate(CO2 = as.double(CO2)) %>%
  group_by(Product) %>%
  summarise(
    GHG.Emissions = mean(CO2, na.rm = TRUE)
  ) %>%
  arrange(-GHG.Emissions) %>%
  filter(!is.na(GHG.Emissions))

write.csv(global_df_ghg, "global_ghg_df.csv", row.names = FALSE)

global_df <- tail(global_df, -1)

simple_df_ghg <- simple_df %>%  # simplifies df to just ghg emissions
  select(Product, GHG.Emissions) %>%
  arrange(-GHG.Emissions)

#---------Creates Water Data and Land Data bar graphs----------------------

water_data <- function(list) {
  single <- simple_df[simple_df$Product %in% list, ]
  p <- ggplot(data = single, aes(x = Product, y = Freshwater.Withdrawals, 
                             fill = Product)) +
    geom_bar(stat = "identity") + theme_minimal()
  p + coord_flip()
}

land_data <- function(list) {
  single <- simple_df[simple_df$Product %in% list, ]
  p <- ggplot(data = single, aes(x = Product, y = Land.Usage,
                             fill = Product)) +
    geom_bar(stat = "identity", width = 0.7) + theme_minimal()
  p + coord_flip()
}

land_data(c("Wine", "Tofu", "Rapeseed Oil", "Nuts"))

#----------Cleans Large Dataset-----------------------------------------
global_df_map <- global_df %>%
  rename(Product = Food.type) %>%
  rename(name_long = Region) %>%
  rename(CO2 = kg.CO2.eq.kg.produce..BFM.or.L.after.conversion) %>%
  mutate(CO2 = as.character(CO2)) %>%
  mutate(CO2 = as.double(CO2)) %>%
  filter(!is.na(CO2)) %>%
  filter(!(name_long %in%
             global_df$name_long[!global_df$name_long
                                 %in% world$name_long])) %>%
  left_join(world, by = "name_long")