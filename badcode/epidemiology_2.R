adult23 <- read.csv("adult23.csv")
#subset the data for those with long-term COVID
adult23_long_covid <- subset(adult23, LONGCOVD1_A == 1)
print(nrow(adult23_long_covid))
#excluding all that entered in the education field the values "Refused" or "Don't Know"
adult23_long_covid <- subset(adult23_long_covid, 
                             LONGCOVD1_A == 1 & 
                               !(EDUCP_A %in% c(97, 99)))
print(nrow(adult23_long_covid))
#excluding all that entered in the severity of long term covid field "Refused" or "Don't Know"
adult23_long_covid <- subset(adult23_long_covid, 
                             !(LCVDACT_A %in% c(8, 9)))
print(nrow(adult23_long_covid))

# 1. Chi-squared test
# Create a contingency table of education vs severity
contingency_table <- table(adult23_long_covid$EDUCP_A, adult23_long_covid$LCVDACT_A)
print(contingency_table)

# Perform chi-squared test
chi_test <- chisq.test(contingency_table)
print(chi_test)

# 2. Create barplot with ggplot2
# Add severity labels for better readability
adult23_long_covid$LCVDACT_label <- factor(adult23_long_covid$LCVDACT_A, 
                                           levels = c(1, 2, 3), 
                                           labels = c("Not at all", "A little", "A lot"))

# Convert education level to factor with all possible values (0-10)
adult23_long_covid$EDUCP_A <- factor(adult23_long_covid$EDUCP_A, 
                                     levels = 0:10)

# Calculate percentages by education level
prop_table <- prop.table(table(adult23_long_covid$EDUCP_A, adult23_long_covid$LCVDACT_label), margin = 1) * 100

# Convert to data frame for plotting
plot_data <- as.data.frame(prop_table)
colnames(plot_data) <- c("Education", "Severity", "Percentage")

# Create barplot
library(ggplot2)
ggplot(plot_data, aes(x = Education, y = Percentage, fill = Severity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Long COVID Symptom Severity by Education Level",
       y = "Percentage (%)", 
       x = "Education Level (0=Kindergarten to 10=PhD)") +
  scale_x_discrete(breaks = 0:10, labels = 0:10) +  # Ensure all education levels from 0-10 are shown
  scale_fill_manual(values = c("Not at all" = "green", "A little" = "yellow", "A lot" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))  # Ensure horizontal alignment of x-axis labels