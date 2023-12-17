#library
library(dplyr)
library(tidyverse)

#remove everything
rm(list=ls())

#storing data to get shorter names
sales <- Unravelling_the_stocking_puzzle_sales_details
product <- Unraveling_the_stocking_puzzle_product_details



#joining sales data with product data
(sales_joined <- left_join(sales, product,
                            by = c("Product_code" = "Product_code")))


#choosing appropriate columns
(sales_joined2 <- sales_joined %>% 
  dplyr::select(Month, Sales_sum, Sales_quantity, Product))

(covid <- covid2 %>% 
  group_by(Month) %>% 
  summarise(Sum_cases = sum(new_cases)) %>% 
    mutate(Month = factor(Month, levels = month.abb)) %>% 
    arrange(Month))

#joining sales and product data with temperature data
(sales_weather <- left_join(sales_joined2, temperature,
                        by = c("Month" = "Month")))

#joining sales and product data with covid data
(sales_weather1 <- left_join(sales_weather, covid, by = "Month"))

#joining sales and product data with rainfall data
(sales_weather2 <- left_join(sales_weather1, precipitation, by = "Month"))



#modelling the data as a whole
mod_weather <- lm(Sales_sum ~ temperature + Rainfall + Sum_cases, data = sales_weather2)
anova(mod_weather)
summary(mod_weather)

#filtering and modelling 4-Pack Regular Beer

(regular_beer <- sales_weather2 %>% 
  filter(Product == "4-Pack Regular Beer"))

mod_beer <- lm(Sales_sum ~ temperature + Rainfall + Sum_cases, data = regular_beer)
anova(mod_beer)
summary(mod_beer)


#filtering and modelling Belgian Beer 

belgian_beer <- sales_weather2 %>% 
  filter(Product == "Brussel Special 4-Pack")

mod_belgian <- lm(Sales_sum ~ temperature + Rainfall + Sum_cases, data = belgian_beer)
anova(mod_belgian)
summary(mod_belgian)


#filtering and modelling lighting fluid

lighting_fluid <- sales_weather2 %>% 
  filter(Product == "Barbecue Lighting Fluid")

mod_lighting <- lm(Sales_sum ~ temperature + Rainfall + Sum_cases, data = lighting_fluid)
anova(mod_lighting)
summary(mod_lighting)


#filtering and modelling cigarette blue 20S

blue_20S <- sales_weather2 %>% 
  filter(Product == "Blue Line 20S")

mod_blue20S <- lm(Sales_sum ~ temperature + Rainfall + Sum_cases, data = blue_20S)
anova(mod_blue20S)
summary(mod_blue20S)


#filtering and modelling caramel chocolate bar

caramel <- sales_weather2 %>% 
  filter(Product == "Caramel Chocolate Bar")

mod_caramel <- lm(Sales_sum ~ temperature + Rainfall + Sum_cases, data = caramel)
anova(mod_caramel)
summary(mod_caramel)


#filtering and modelling chicken and cheese sandwich

chicken_sandwich <- sales_weather2 %>% 
  filter(Product == "Chicken & Cheese")

mod_chicken <- lm(Sales_sum ~ temperature + Rainfall + Sum_cases, data = chicken_sandwich)
anova(mod_chicken)
summary(mod_chicken)


#filtering and modelling chocolate ice cream

chocolate_icecream <- sales_weather2 %>% 
  filter(Product == "Chocolate Ice Cream")

mod_chocolate <- lm(Sales_sum ~ temperature + Rainfall + Sum_cases, data = chocolate_icecream)
anova(mod_chocolate)
summary(mod_chocolate)


#filtering and modelling Coconut and passionfruit ice cream

coconut_icecream <- sales_weather2 %>% 
  filter(Product == "Coconut & Passion Fruit Ice Cream")

mod_coconut <- lm(Sales_sum ~ temperature + Rainfall + Sum_cases, data = coconut_icecream)
anova(mod_coconut)
summary(mod_coconut)


#filtering and modelling french fries

fries <- sales_weather2 %>% 
  filter(Product == "Frozen French Fries 1Kg")

mod_fries <- lm(Sales_sum ~ temperature + Rainfall + Sum_cases, data = fries)
anova(mod_fries)
summary(mod_fries)


#filtering and modelling fried fish

fried_fish <- sales_weather2 %>% 
  filter(Product == "Frozen fried fish")

mod_fish <- lm(Sales_sum ~ temperature + Rainfall + Sum_cases, data = fried_fish)
anova(mod_fish)
summary(mod_fish)


#filtering and modelling cigarette green stripe 20S

green_20S <- sales_weather2 %>% 
  filter(Product == "Green Stripes 20S")

mod_green20 <- lm(Sales_sum ~ temperature + Rainfall + Sum_cases, data = green_20S)
anova(mod_green20)
summary(mod_green20)


#filtering and modelling grilled cheese

grilled_cheese <- sales_weather2 %>% 
  filter(Product == "Grilled Cheese")

mod_grilled <- lm(Sales_sum ~ temperature + Rainfall + Sum_cases, data = grilled_cheese)
anova(mod_grilled)
summary(mod_grilled)


#filtering and modelling cigarette red 40S

red_40S <- sales_weather2 %>% 
  filter(Product == "Red Line 40S")

mod_red40 <- lm(Sales_sum ~ temperature + Rainfall + Sum_cases, data = red_40S)
anova(mod_red40)
summary(mod_red40)

#filtering and modelling small chips

chips <- sales_weather2 %>% 
  filter(Product == "Small Chips 60 g")

mod_chips <- lm(Sales_sum ~ temperature + Rainfall + Sum_cases, data = chips)
anova(mod_chips)
summary(mod_chips)

#filtering and modelling regular coffee

coffee <- sales_weather2 %>% 
  filter(Product == "Regular Coffee")

mod_coffee <- lm(Sales_sum ~ temperature + Rainfall + Sum_cases, data = coffee)
anova(mod_coffee)
summary(mod_coffee)

#filtering and modelling soda

soda <- sales_weather2 %>% 
  filter(Product == "Soda 50 CL")

mod_soda <- lm(Sales_sum ~ temperature + Rainfall + Sum_cases, data = soda)
anova(mod_soda)
summary(mod_soda)


#filtering and modelling sunglasses

sunglass <- sales_weather2 %>% 
  filter(Product == "Sunglasses")

mod_sunglass <- lm(Sales_sum ~ temperature + Rainfall + Sum_cases, data = sunglass)
anova(mod_sunglass)
summary(mod_sunglass)


#filtering and modelling tonic water

tonic_water <- sales_weather2 %>% 
  filter(Product == "Tonic Water 50CL")

mod_tonic <- lm(Sales_sum ~ temperature + Rainfall + Sum_cases, data = tonic_water)
anova(mod_tonic)
summary(mod_tonic)

#filtering and modelling winter cookie 

winter_cookie <- sales_weather2 %>% 
  filter(Product == "Winter Special Cookie")

mod_winter <- lm(Sales_sum ~ temperature + Rainfall + Sum_cases, data = winter_cookie)
anova(mod_winter)
summary(mod_winter)

#filtering and modelling latte

latte <- sales_weather2 %>% 
  filter(Product == "XL Latte")

mod_latte <- lm(Sales_sum ~ temperature + Rainfall + Sum_cases, data = latte)
anova(mod_latte)
summary(mod_latte)



