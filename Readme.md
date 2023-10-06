Asian American Quality of Life Project
================

# Links

- [Dataset
  Source](https://data.austintexas.gov/dataset/Final-Report-of-the-Asian-American-Quality-of-Life/hc5t-p62z)

# Explanation of Project and Information about Data

This dataset that we are examining involves the fastest growing minority
group in the United States, Asian Americans. Within this dataset, the
broad term includes individuals having origins in the Far East,
Southeast Asia, or the Indian subcontinent. The Final Report of the
Asian American Quality of Life (AAQoL) comes from data provided by the
city of Austin, TX, and includes 2,609 rows of data in which each row is
an individual survey. There are 231 columns in the dataset, each column
providing a different piece of information about the respondents from a
question on the survey they were provided. Our project is evaluating the
quality of life of Asian Americans and factors that contribute to that.
Our data set will show why the quality of life may change based upon
different variables and why it is significant.

# Goals for Project

Some goals of our project include exploring quality of life in Asian
American populations based on different variables such as marriage. We
will also be using the other data provided from the surveys to determine
whether or not income level and level of English proficiency are
correlated. These can hopefully be answered by creating meaningful
visualizations and analyzing the data coming from those visualizations.

# Hypotheses

Hypothesis 1:

Marriage vs Quality of Life for Different Ethnicities Our first
hypothesis is that when people become married, their quality of life
will increase, and there will be a variance amongst the different types
of Asian ethnicities.

Hypothesis 2:

Income Bracket vs Level of English Proficiency for Asian Americans in
the United States Our second hypothesis is that the higher the income
level of Asian Americans in the United States, the higher their English
proficiency.

# Data Analysis and Visualizations Hypothesis 1

``` r
# Import the dataset into R environment.
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ lubridate 1.9.2     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.1     ✔ tidyr     1.3.0
    ## ✔ readr     2.1.4

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readr)
Final_Report_of_the_Asian_American_Quality_of_Life_AAQoL_ <- read_csv("Final_Report_of_the_Asian_American_Quality_of_Life__AAQoL_.csv")
```

    ## New names:
    ## Rows: 2609 Columns: 231
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (190): Gender, Ethnicity, Marital Status, No One, Spouse, Children, Gran... dbl
    ## (41): Survey ID, Age, Education Completed, Household Size, Grandparent,...
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `Other` -> `Other...17`
    ## • `Other` -> `Other...89`

``` r
# Load the dataset into a data frame named 'AAQoL'
AAQoL <- data.frame(Final_Report_of_the_Asian_American_Quality_of_Life_AAQoL_)

# Filter out rows with missing values in specific variables
# We retain only rows where 'Quality.of.Life', 'Marital.Status', and 'Ethnicity' are not missing (NA)
Filtered_AAQoL <- AAQoL %>%
  filter(!is.na(Quality.of.Life) & !is.na(Marital.Status) & !is.na(Ethnicity)) %>%
  filter(Marital.Status!= 'Other' & Ethnicity!= 'Other')

# Create a bar plot using 'ggplot2'
# - 'x' axis represents 'Marital.Status'
# - 'fill' the bars based on 'Marital.Status'
# - Use 'Quality.of.Life' for the 'y' values
# - Calculate the mean of 'Quality.of.Life' for each category of 'Marital.Status'
# - 'position = "dodge"' places bars side by side
# - 'na.rm = TRUE' removes NA values from the calculations
# - 'facet_wrap(~Ethnicity)' creates separate plots for each 'Ethnicity'
# - Customize the appearance: Hide x-axis text
Filtered_AAQoL %>% 
  ggplot(aes(x = Marital.Status, fill = Marital.Status)) +
  geom_bar(aes(y = Quality.of.Life), stat = "summary", fun = mean, position = "dodge", na.rm = TRUE) +
  facet_wrap(~Ethnicity) +
  theme(axis.text.x = element_blank()) +
  labs(x = 'Marital Status',
       y = 'Quality of Life',
       title = 'Relation between Marital Status and Quality for Different Asian American Ethnicities')
```

![](Readme_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

# Data Analysis and Visualizations Hypothesis 2

``` r
# Select the "Income" and "English.Speaking" columns, remove rows with missing values, and group by both columns
IncomeBasedOnEnglish <- AAQoL %>% 
  select(Income, English.Speaking) %>% 
  na.omit() %>% 
  group_by(Income, English.Speaking)

# Define the levels for the "English.Speaking" factor
english_levels <- c("Not at all", "Not well", "Well", "Very well")

# Create another dataset by swapping the order of columns and reordering "English.Speaking" as a factor
IncomeBasedOnEnglish01 <- AAQoL %>% 
  select(English.Speaking, Income) %>% 
  na.omit() %>% 
  group_by(English.Speaking, Income) %>% 
  mutate(English.Speaking = factor(English.Speaking, levels = english_levels)) %>% 
  summarize(n = n())  
```

    ## `summarise()` has grouped output by 'English.Speaking'. You can override using
    ## the `.groups` argument.

``` r
# Create a bar plot using ggplot
ggplot(IncomeBasedOnEnglish01, aes(x = English.Speaking, y = n)) +
  geom_bar(stat = "identity", 
           width = 0.7, 
           position = position_dodge(width = 0.8),
           aes(fill = factor(Income))) +
  
  # Customize the legend and axis labels
  scale_fill_discrete(name = "Income level") +
  scale_alpha_discrete(name = "Account", range = c(1, 0.5)) + 
  xlab("English level") +
  ylab("Number of people") + 
  
  # Customize the plot appearance
  theme_light() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1, margin = margin(0.2, 0, 0.3, 0, "cm")), 
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0.5, 0.5, 0, 0.5, "cm"),      
        panel.grid.major.x = element_blank(),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))
```

    ## Warning: Using alpha for a discrete variable is not advised.

![](Readme_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->