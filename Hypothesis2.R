library(tidyverse)
library(extrafont)
loadfonts()

df_unfiltered <- read.csv("Final_Report_of_the_Asian_American_Quality_of_Life__AAQoL__20231113.csv")

# Wrangling, 2609 observations to 1620 observations
df <- df_unfiltered %>%
  filter(US.Born == 'No') %>% # Filter for only immigrants, 256 rows removed.
  filter(Student != 'Student') %>% # Filter out full-time students, 297 rows removed.
  filter(Income != '') %>% # Filter for only response with income information, 167 rows removed
  filter(English.Speaking != '') %>% # Filter for only responses with proficiency information, 6 rows removed
  select(1,Income,'Proficiency'=English.Speaking,21:24,26:30,Age) %>% # Select only relevant variables
  na.omit() # Omit all cells with N/A, 263 rows removed.

# Redefining Proficiency as a ordinal variable
df$Proficiency <- factor(df$Proficiency, ordered = TRUE,
                         levels = c("Not at all", "Not well", "Well", "Very well"))

# Primary Visualization
df %>% ggplot(aes(x = Income, fill = Proficiency)) +
              geom_bar(position = 'fill') +
              scale_fill_brewer(palette = 15) +
              theme(axis.text.x = element_text(angle = 15, hjust = 0.7, size = 8, family = 'serif'),
                    axis.text.y = element_text(angle = 90, hjust = 0.5, family = "serif"),
                    axis.title = element_text(family = "serif"),
                    plot.title = element_text(family = "serif", face = "bold"),
                    legend.title = element_text(family = "serif", face = "bold"),
                    legend.text = element_text(family = "serif")) +
              labs(title = "Levels of English Proficiency Within Different Income Brackets",
                    x = "Income Brackets",
                    y = "Percentage")

# Redefining Age and Income for the second visualization
df <- df %>%
  mutate(Income5 = recode(Income, "$10,000 - $19,999" = "$10,000 - $29,999",
                                  "$20,000 - $29,999" = "$10,000 - $29,999",
                                  "$30,000 - $39,999" = "$30,000 - $49,999",
                                  "$40,000 - $49,999" = "$30,000 - $49,999",
                                  "$50,000 - $59,999" = "$50,000 - $69,999",
                                  "$60,000 - $69,999" = "$50,000 - $69,999")) %>% # Recode Income into 5 categories
  mutate(AgeCat = case_when(Age >= 18 & Age <= 34 ~ "Age 18-34",
                            Age >= 35 & Age <= 50 ~ "Age 35-50",
                            Age >= 51 & Age <= 64 ~ "Age 51-64",
                            Age >= 65 ~ "Age 65+",
                            TRUE ~ NA_character_)) # Create a categorical variable for age
  
# Secondary Visualization - Faceted by Age
df %>% ggplot(aes(x = Income5, fill = Proficiency)) +
  geom_bar(position = 'fill') +
  scale_fill_brewer(palette = 15) +
  facet_wrap(~ AgeCat) +
  theme(axis.text.x = element_text(angle = 15, hjust = 0.7, size = 8, family = 'serif'),
        axis.text.y = element_text(angle = 90, hjust = 0.5, family = "serif"),
        axis.title = element_text(family = "serif"),
        plot.title = element_text(family = "serif", face = "bold"),
        legend.title = element_text(family = "serif", face = "bold"),
        legend.text = element_text(family = "serif")) +
  labs(title = "Proficiency Level v.s. Income Brackets, Faceted by Age Groups",
       x = "Income Brackets",
       y = "Percentage")

