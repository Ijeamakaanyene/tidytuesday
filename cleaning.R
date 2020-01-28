here::here("uc_system_analysis", "data")

library(dplyr)
library(stringr)
library(ggplot2)

# Data obtained from: https://uccliometric.org/courses/

berkeley = readr::read_csv("data/Courses_Berkeley_2018-01-15.csv")
ucla = readr::read_csv("data/Courses_UCLA_2018-01-15.csv")

# Adding DS for data science specific terms, and overall for additional categories
berkeley = berkeley %>%
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

ucla = ucla %>%
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
berkeley_reduced = berkeley %>%
  #filter(Taught == TRUE) %>%
  select(Year, Field, Name, Area, GenArea, data_science, overall)

ucla_reduced = ucla %>%
  #filter(Taught == TRUE) %>%
  select(Year, Field, Name, Area, GenArea, data_science, overall)

# Counts by year, area, gen area 
counts_berkeley = berkeley_reduced %>%
  filter(data_science == TRUE) %>%
  group_by(Year, GenArea) %>%
  count(name = "num_classes") %>%
  ungroup()

counts_berkeley = counts_berkeley %>%
  tidyr::spread(key = GenArea, value = num_classes, fill = 0) %>%
  tidyr::gather(key = GenArea, value = num_classes, -Year) %>%
  arrange(Year, GenArea)


plot1 = counts_berkeley %>%
  group_by(Year) %>%
  summarise(sum_classes = sum(num_classes)) %>%
  ggplot(., aes(x = Year, y = sum_classes)) +
  geom_area(fill = "#b3cde0", colour = "#011f4b") + 
  #geom_ribbon(aes(ymax = sum_classes, ymin = 0),
              #fill = "#b3cde0", alpha = 0.7) + 
  #geom_line(color = "#011f4b") + 
  labs(title = "Data Science Courses in U.C. Berkeley 1963 - 2011",
       y = "Number of Classes Offered",
       x = "School Year",
       caption = paste0("Source: UC ClioMetric History Project\n",
                     "Visualization: Ijeamaka Anyene")) +
  theme_minimal()

plot2 = counts_berkeley %>%
  group_by(Year, GenArea) %>%
  summarise(sum_classes = sum(num_classes)) %>%
  ggplot(., aes(x = Year, y = sum_classes)) +
  geom_area(aes(colour = GenArea, fill = GenArea)) +
  labs(title = "Data Science Courses in U.C. Berkeley 1963 - 2011",
       y = "Number of Classes Taught",
       x = "School Year",
       caption = paste0("Source: UC ClioMetric History Project\n",
                        "Visualization: Ijeamaka Anyene")) +
  theme_minimal()


theme_get()$axis.text




