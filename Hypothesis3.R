#V1

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

#V2

IncomeBasedOnReligionFinal1 <- IncomeBasedOnReligionFinal

Income_level <- c("$70,000 and over", "$60,000 - $69,999", "$50,000 - $59,999", "$40,000 - $49,999", "$30,000 - $39,999", "$20,000 - $29,999", "$10,000 - $19,999", "$0 - $9,999")

IncomeBasedOnReligionFinal1 <- IncomeBasedOnReligionFinal1 %>% mutate(Income = factor(Income, levels = Income_level))

ggplot(IncomeBasedOnReligionFinal1, aes(x = Religion, y = percentage, fill = Income)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of People in Each Religion Group by Income",
       x = "Income",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()
