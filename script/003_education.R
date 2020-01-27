# EDUCATION --------------------------------------------------------------------

# TODO: Add high school data
# TODO: Add uni data

# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(janitor)
library(pxweb)

library(patchwork)
library(ggforce)
library(statgl)
library(scales)

# Import data ------------------------------------------------------------------

# School test results
schooltest_raw <- 
  pxweb_get_data(
    url = "http://bank.stat.gl/api/v1/en/Greenland/UD/UD45/UD4530/UDXTDS.px",
    query = list(
      "district / school" = c("060601","060602","060603","060605","060606",
                              "060608","060650"),
      "subject" = c("01","02","03","04"),
      "grade"= c ("3","7"),
      "unit" = c("B","A"),
      "time" = "*")      
  ) %>% 
  clean_names()

# Constants + Helper functions -------------------------------------------------

theme_set(theme_statgl())

# Tidy/transform ---------------------------------------------------------------

# Primary school standardized tests
schooltest <- schooltest_raw %>% 
  separate(district_school, c("district", "school"), sep = " - ") %>% 
  spread(unit, grade_test_results) %>% 
  clean_names() %>% 
  filter(!is.na(number_of_pupils)) %>% 
  mutate(time = strtoi(time),
         grade = fct_rev(grade),
         school = school %>% 
           fct_reorder(number_of_pupils, sum) %>% fct_rev) %>% 
  select(-district)

# Visualise --------------------------------------------------------------------

# Standardized school tests
p1 <- schooltest %>% 
  arrange(grade, time) %>% 
  ggplot(aes(x = grade, y = problem_solving_proficiency_pct_correct,
             size = number_of_pupils,
             color = subject, alpha = time, group = grade)) +
  geom_sina(maxwidth = 0.75) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_color_statgl() +
  theme_statgl() +
  labs(x = "", y = "Problem solving proficiency\n(Median pct. correct)",
       color = "Subject", size = "No. of pupils", alpha = "Year")

p2 <- p1 + facet_wrap(~school, ncol = 1)

# Patch ------------------------------------------------------------------------

(p1 | p2) +
  plot_layout(guides = "collect")
