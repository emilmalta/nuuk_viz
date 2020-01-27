# POPULATION -------------------------------------------------------------------

# TODO: Animation of population pyramid

# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(janitor)
library(pxweb)

library(ggchicklet)
library(ggforce)
library(statgl)

# Import data ------------------------------------------------------------------

# Download data 
nuuk_pop_raw <- 
  pxweb_get_data(
    url = "http://bank.stat.gl/api/v1/en/Greenland/BE/BE01/BE0120/BEXSTNUK.PX",
    query = list("age" = "*",
                 "place of birth" = c("N", "S"),
                 "citydistrict"=c("1","2","3","4","9"),
                 "gender"=c("1","2"),
                 "time"="2019")
  ) %>% 
  clean_names()

# Households data
nuuk_houses_raw <- 
  pxweb_get_data(
    url = "http://bank.stat.gl/api/v1/en/Greenland/BE/BE60/BE6010/BEXHUSNUK.PX",
    query =list(
      "residence" = c("1","2","3","4","8","9"),
      "type of address" = "0",
      "size" = "*",
      "type" = "*",
      "time" = "*")
  ) %>% 
  clean_names()

# Constants + Helper functions -------------------------------------------------

theme_set(theme_statgl())

# Function to turn strings to a common format
pretty_col <- function(str) {
  str_remove_all(str_to_title(trimws(str)), "[:punct:]")
}


# Tidy/transform ---------------------------------------------------------------

# Population data
nuuk_pop <- nuuk_pop_raw %>% 
  mutate_at(c("age", "time"), as.numeric) %>% 
  uncount(population_in_nuuk) 

# Households
nuuk_houses <- nuuk_houses_raw %>% 
  mutate(time = strtoi(time),
         size = size %>% as.character() %>% fct_reorder(parse_number(.))) %>% 
  filter(residence != "tjek",
         !is.na(households_in_municipalities_districts)) %>% 
  separate(type, into = c("children", "adults"),  sep = "children|child") %>% 
  mutate_at(c("children", "adults"), pretty_col) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(children = case_when(children == "No" ~ "None", T ~ children)) %>% 
  uncount(households_in_municipalities_districts)

# Visualise --------------------------------------------------------------------

# Age distribution
nuuk_pop %>% 
  filter(time == max(time), citydistrict != "Unknown") %>% 
  mutate(place_of_birth = paste("Birthplace:", place_of_birth)) %>% 
  ggplot(aes(x = gender, y = age, color = gender)) +
  geom_sina(size = 0.25) +
  facet_grid(place_of_birth ~ citydistrict) +
  theme(legend.position = "none") +
  scale_color_statgl(palette = "dawn") +
  labs(x = "", y = "Age",
       title = "Age distribution of Nuuk residents",
       subtitle = "2019")

# Kids!
nuuk_houses %>% 
  filter(time == max(time)) %>% 
  mutate(
    residence = residence %>% fct_reorder(parse_number(size), sum) %>% 
      fct_rev,
    children  = factor(children) %>% fct_shift(-1),
    adults    = adults %>% fct_infreq %>% fct_rev) %>% 
  count(time, children, adults, residence) %>% 
  ggplot(aes(x = adults, y = n, fill = children)) +
  geom_chicklet(width = 0.5) +
  coord_flip() +
  facet_wrap(~residence, ncol = 1) +
  theme(legend.position = "bottom", legend.justification = 1) +
  labs(x = "", y = "Count", fill = "No. of Children:") +
  scale_fill_statgl(reverse = TRUE) 