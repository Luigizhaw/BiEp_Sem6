######## Setup and load data

# Load required packages
install.packages(c("readxl", "ggplot2", "MASS", "ordinal", "car", "DescTools"))
library(readxl)
library(ggplot2)
library(MASS)       # for polr
library(ordinal)    # for clm
library(car)        # for VIF and Anova
library(DescTools)  # for CMH test

# Load the Excel data
data <- read.csv("C:/Users/Luigi/OneDrive/ZHAW/6. Semester/BiEp_Sem6/adult23.csv")

# Preview the data
str(data)


######## Chi-Squared Test

filtered <- subset(data, LONGCOVD1_A == 1 & !(EDUCP_A %in% c(97, 99)) & !(LCVDACT_A %in% c(8, 9)))

# Convert to factors
filtered$EDUCP_A <- factor(filtered$EDUCP_A)
filtered$LCVDACT_A <- factor(filtered$LCVDACT_A, levels = c(1, 2, 3), labels = c("Not at all", "A little", "A lot"))

# Contingency table
contingency_table <- table(filtered$EDUCP_A, filtered$LCVDACT_A)

# Chi-squared test
chisq.test(contingency_table)


######## Ordinal Logistic Regression

library(MASS)

ord_data <- subset(data, LONGCOVD1_A == 1 &
                     !(EDUCP_A %in% c(97, 99)) &
                     !(LCVDACT_A %in% c(8, 9)) &
                     !is.na(INCWRKO_A))

ord_data$severity <- factor(ord_data$LCVDACT_A, levels = c(1, 2, 3), ordered = TRUE)
ord_data$EDUCP_A <- factor(ord_data$EDUCP_A)
ord_data$INCWRKO_A <- factor(ord_data$INCWRKO_A)

model_ord <- polr(severity ~ EDUCP_A + INCWRKO_A, data = ord_data, Hess = TRUE)
summary(model_ord)

# Get p-values
(ctable <- coef(summary(model_ord)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = round(p, 4)))


######## Multivariate Logistic Regression

multi_data <- subset(data,
                     LONGCOVD1_A %in% c(1, 2) &               # keep only Yes/No
                       !(EDUCP_A %in% c(97, 99)) &              # valid education
                       !is.na(INCWRKO_A) &                      # valid income
                       !(HISTOPCOST_A %in% c(7, 9)) &           # valid healthcare access
                       !is.na(SEX_A) & !is.na(AGEP_A))          # valid sex and age

# Recode outcome variable to binary 0/1
multi_data$LONGCOVD1_A <- ifelse(multi_data$LONGCOVD1_A == 1, 1, 0)

# Convert predictors to factors
multi_data$EDUCP_A <- factor(multi_data$EDUCP_A)
multi_data$INCWRKO_A <- factor(multi_data$INCWRKO_A)
multi_data$HISTOPCOST_A <- factor(multi_data$HISTOPCOST_A,
                                  levels = c(1, 2),
                                  labels = c("Yes", "No"))
multi_data$SEX_A <- factor(multi_data$SEX_A)

# Fit logistic regression model
model_multi <- glm(LONGCOVD1_A ~ EDUCP_A + INCWRKO_A + HISTOPCOST_A + SEX_A + AGEP_A,
                   data = multi_data,
                   family = binomial)

# View summary and odds ratios
summary(model_multi)
exp(coef(model_multi))        # Odds Ratios
confint(model_multi)          # Confidence Intervals




######## Effect Size for ANOVA-lika Analysis

anova_data <- subset(data, !is.na(PHQ2SCREEN_A) & !is.na(EDUCP_A) & !is.na(INCWRKO_A))

anova_data$EDUCP_A <- factor(anova_data$EDUCP_A)
anova_data$INCWRKO_A <- factor(anova_data$INCWRKO_A)

anova_model <- aov(PHQ2SCREEN_A ~ EDUCP_A + INCWRKO_A, data = anova_data)
summary(anova_model)

# Effect size
library(lsr)
etaSquared(anova_model)


######## Cochran–Mantel–Haenszel (CMH) Test


filtered <- subset(data,
                   LONGCOVD1_A == 1 &
                     !(EDUCP_A %in% c(97, 99)) &
                     !(LCVDACT_A %in% c(8, 9)) &
                     SEX_A %in% c(1, 2))  # only valid strata

# Convert variables to factors
filtered$EDUCP_A <- factor(filtered$EDUCP_A)
filtered$LCVDACT_A <- factor(filtered$LCVDACT_A)
filtered$SEX_A <- factor(filtered$SEX_A)

# Build table and rerun CMH test
cmh_table <- xtabs(~ EDUCP_A + LCVDACT_A + SEX_A, data = filtered)
mantelhaen.test(cmh_table)




######## Visualization (Stacked Bar Plot)

library(ggplot2)

plot_data <- prop.table(table(filtered$EDUCP_A, filtered$LCVDACT_A), margin = 1) * 100
plot_df <- as.data.frame(plot_data)
colnames(plot_df) <- c("Education", "Severity", "Percentage")

ggplot(plot_df, aes(x = Education, y = Percentage, fill = Severity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Severity of Long COVID by Education Level",
       x = "Education Level", y = "Percentage (%)") +
  scale_fill_manual(values = c("Not at all" = "green", "A little" = "yellow", "A lot" = "red")) +
  theme_minimal()
















##################### Debugging

# List all column names containing 'income' or 'INC'
grep("income", names(data), ignore.case = TRUE, value = TRUE)
grep("INC", names(data), value = TRUE)


grep("cost", names(data), ignore.case = TRUE, value = TRUE)
grep("care", names(data), ignore.case = TRUE, value = TRUE)

