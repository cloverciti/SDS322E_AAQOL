BelongingAndFullTimeEmployment <- AAQoL %>% 
  select(Belonging, Full.Time.Employment, Part.Time.Employment, Student, Homemaker) %>% 
  na.omit() %>% mutate(Employment = if_else(Full.Time.Employment != "0", Full.Time.Employment, Part.Time.Employment)) %>% 
  mutate(Employment = if_else(Employment != "0", Employment, Student))  %>%
  mutate(Employment = if_else(Employment != "0", Employment, Homemaker)) %>%
  select(-Full.Time.Employment, -Part.Time.Employment, -Student, -Homemaker) %>% group_by(Belonging, Employment) %>%
  filter(Employment != 0)%>% 
  summarize(n = n())

totalbelong <- BelongingAndFullTimeEmployment %>%
  group_by(Belonging) %>%
  summarize(total_belonging = sum(n))



BelongingAndEmployment <- merge(BelongingAndFullTimeEmployment, totalbelong, by = "Belonging") %>% mutate(percentage = n / total_belonging * 100)

# Plot using ggplot
ggplot(BelongingAndEmployment, aes(x = Belonging, y = percentage, fill = Employment)) +
  geom_bar(stat = "identity") +
  labs(title = "Level of belonging based on employment",
       x = "Belonging",
       y = "Percentage") + geom_text(aes(label = sprintf("%.1f%%", percentage)),
                                     position = position_stack(vjust = 0.5), # Adjust the position of labels
                                     size = 3, color = "white") + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()
