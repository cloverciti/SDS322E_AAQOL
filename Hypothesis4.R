BelongingAndFullTimeEmployment <- AAQoL %>% 
  select(Belonging, Full.Time.Employment, Part.Time.Employment, Student) %>% 
  na.omit() %>% mutate(Employment = if_else(Full.Time.Employment != "0", Full.Time.Employment, Part.Time.Employment)) %>% mutate(Employment = if_else(Employment != "0", Employment, Student))  %>%
  select(-Full.Time.Employment, -Part.Time.Employment, -Student) %>% group_by(Belonging, Employment) %>% 
  summarize(n = n())

totalbelong <- BelongingAndFullTimeEmployment %>%
  group_by(Belonging) %>%
  summarize(total_belonging = sum(n))

BelongingAndEmployment <- merge(BelongingAndFullTimeEmployment, totalbelong, by = "Belonging") %>% mutate(percentage = n / total_belonging * 100)

BelongingAndEmployment <- BelongingAndEmployment %>%
  mutate(Employment = ifelse(Employment == 0, "unemployed", as.character(Employment)))

# Plot using ggplot
ggplot(BelongingAndEmployment, aes(x = Belonging, y = percentage, fill = Employment)) +
  geom_bar(stat = "identity") +
  labs(title = "Level of belonging based on employment",
       x = "Belonging",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()











BelongingAndEmployment01 <- BelongingAndEmployment

employment_level <- c("unemployed", "Student", "Employed part time", "Employed full time")

BelongingAndEmployment01 <- BelongingAndEmployment01 %>% mutate(Employment = factor(Employment, levels = employment_level))


# Plot the ggplot with reversed order of the legend
ggplot(BelongingAndEmployment01, aes(x = Belonging, y = n)) +
  geom_bar(stat = "identity", 
           width = 0.7, 
           position = position_dodge(width = 0.8),
           aes(fill = factor(Employment))) +
  scale_fill_discrete(name = "Employment") +
  scale_alpha_discrete(name = "Account", range = c(1, 0.5)) + 
  xlab("Belonging") +
  ylab("Number of people") + 
  theme_light() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1, margin = margin(0.2, 0, 0.3, 0, "cm")), 
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0.5, 0.5, 0, 0.5, "cm"),      
        panel.grid.major.x = element_blank(),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))
