IncomeBasedOnReligion <- AAQoL %>% 
  select(Income, Religion) %>% 
  na.omit() %>% 
  group_by(Income, Religion) %>% 
  summarize(n = n())

totalresp <- AAQoL %>% select(Income, Religion) %>% 
  na.omit() %>%
  group_by(Religion) %>%
  summarize(total_responses = n())

IncomeBasedOnReligionFinal <- merge(IncomeBasedOnReligion, totalresp, by = "Religion") %>% mutate(percentage = n / total_responses * 100)

# Plot using ggplot
ggplot(IncomeBasedOnReligionFinal, aes(x = Religion, y = percentage, fill = Income)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of People in Each Religion Group by Income",
       x = "Income",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()
