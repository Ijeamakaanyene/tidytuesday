here::here("repos", "uc_system_analysis", "data")

library(dplyr)
library(stringr)
library(ggplot2)

# Data obtained from: https://uccliometric.org/courses/

berkeley = readr::read_csv("data/Courses_Berkeley_2018-01-15.csv", 
                           col_types = c("dcccccciilllll"))
ucla = readr::read_csv("data/Courses_UCLA_2018-01-15.csv", 
                       col_types = c("dcccccciilllll"))

# Adding DS for data science specific terms, and overall for additional categories

berkeley$school = "berkeley"
ucla$school = "ucla"
schools_rbind = data.frame(rbind(berkeley, ucla))

schools_rbind = schools_rbind %>%
  mutate(ds_name = str_detect(Name, 
                              "(D|d)ata\\s*(S|s)cience|(D|d)ata\\s*(A|a)naly(sis|tics)|(M|m)achine\\s*(L|l)earning|(D|d)ata\\s*(M|m)ining"),
         ds_desc = str_detect(Description, 
                              "(D|d)ata\\s*(S|s)cience|(D|d)ata\\s*(A|a)naly(sis|tics)|(M|m)achine\\s*(L|l)earning|(D|d)ata\\s*(M|m)ining"),
         overall_name_t = str_detect(Name,
                                     "(S|s)tatistical\\s*(L|l)earning|(S|s)tatistical\\s*(C|c)omput(ing|ation)|(C|c)omputing|(D|d)ata"),
         overall_desc_t = str_detect(Description,
                                     "(S|s)tatistical\\s*(L|l)earning|(S|s)tatistical\\s*(C|c)omput(ing|ation)|(C|c)omputing")) %>%
  mutate(overall_name = if_else(ds_name == TRUE | overall_name_t == TRUE, TRUE, FALSE),
         overall_desc = if_else(ds_desc == TRUE | overall_desc_t == TRUE, TRUE, FALSE)) %>%
  mutate(data_science = if_else(ds_name == TRUE | ds_desc == TRUE, TRUE, FALSE),
         overall = if_else(overall_name == TRUE | overall_desc == TRUE, TRUE, FALSE))

# Creating data frame with one variables of interest
schools_reduced = schools_rbind %>%
  select(school, Year, Field, Name, Area, GenArea, data_science, overall)

# Counts by year, area, gen area 
counts_schools = schools_reduced %>%
  filter(data_science == TRUE) %>%
  group_by(school, Year, GenArea) %>%
  count(name = "num_classes") %>%
  ungroup()

counts_schools = counts_schools %>%
  tidyr::spread(key = GenArea, value = num_classes, fill = 0) %>%
  tidyr::gather(key = GenArea, value = num_classes, -Year, -school) %>%
  arrange(school, Year, GenArea)


# Plots!

extrafont::font_import()
extrafont::loadfonts()
extrafont::fonts()

counts_schools %>%
  filter(school == "berkeley") %>%
  group_by(Year) %>%
  summarise(sum_classes = sum(num_classes)) %>%
  ggplot(., aes(x = Year, y = sum_classes)) +
  geom_area(fill = "#b3cde0") + 
  labs(title = "Data Science Courses Offered at U.C. Berkeley, 1963 - 2011",
       subtitle = paste0("The number of data science courses offered has unsurprisingly experienced a major growth\n",
                          "over the past few decades, but what may surprise you are the departments driving this growth"),
       y = "Number of Classes",
       x = "School Year",
       caption = paste0("Source: UC ClioMetric History Project\n",
                     "Visualization: Ijeamaka Anyene")) +
  theme_minimal(base_size = 16) +
  theme()
  

counts_schools %>%
  filter(school == "berkeley") %>%
  group_by(Year, GenArea) %>%
  summarise(sum_classes = sum(num_classes)) %>%
  ggplot(., aes(x = Year, y = sum_classes)) +
  geom_area(aes(colour = GenArea, fill = GenArea)) +
  labs(y = "Number of Classes Taught",
       x = "School Year") +
  theme_minimal(base_size = 16)

# Questions: Do I make this an interactive HTML doc so you can switch between overall and gen areas? Probs yes. 





