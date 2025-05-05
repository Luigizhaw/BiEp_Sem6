######## Setup and load data ########

# Load required packages
install.packages(("scico"))
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
head(data)
colnames(data)

# Load necessary libraries
library(dplyr)
library(tidyr)
library(purrr)

# Step 1: Basic dataset summary
cat("Total rows in dataset:", nrow(data), "\n")

# Step 2: Summary table for all variables
summary_table <- data.frame(
  Variable = names(data),
  Type = sapply(data, function(x) class(x)[1]),
  Unique_Values = sapply(data, function(x) length(unique(x))),
  Missing_Count = sapply(data, function(x) sum(is.na(x)))
)

print("=== VARIABLE OVERVIEW ===")
print(summary_table)

# Step 3: Frequency tables for categorical or integer-labeled variables
cat_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x) || is.integer(x))]
cat("=== FREQUENCY TABLES FOR CATEGORICAL / INTEGER VARIABLES ===\n")
for (var in cat_vars) {
  cat("\n---", var, "---\n")
  print(table(data[[var]], useNA = "ifany"))
}

# Step 4: Clean summary for numeric variables
library(tidyr)
library(dplyr)
library(stringr)

# Compute summary
num_summary_raw <- data %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), list(
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    missing = ~sum(is.na(.))
  ), .names = "{.fn}_{.col}"))

# Pivot safely using regular expressions
num_summary_clean <- num_summary_raw %>%
  pivot_longer(cols = everything(), names_to = "name", values_to = "value") %>%
  mutate(
    Metric = str_extract(name, "^(mean|sd|min|max|missing)"),
    Variable = str_remove(name, "^(mean|sd|min|max|missing)_")
  ) %>%
  select(Variable, Metric, value) %>%
  pivot_wider(names_from = Metric, values_from = value)

# Show output
cat("\n=== CLEANED SUMMARY FOR NUMERIC VARIABLES ===\n")
print(num_summary_clean, n = 25)  # Show 25 rows (increase if needed)




######## Chi-Squared Test ########
# Load required libraries
library(ggplot2)
library(scico)
library(ggpubr)

# ================================
# SECTION 1: Prevalence Analysis
# ================================

# Filter valid cases for prevalence test
filtered_prev <- subset(data,
                        LONGCOVD1_A %in% c(1, 2) &
                          !(EDUCP_A %in% c(97, 99)))

# Convert to factors
filtered_prev$LONGCOVD1_A <- factor(filtered_prev$LONGCOVD1_A,
                                    levels = c(1, 2),
                                    labels = c("Yes", "No"))
filtered_prev$EDUCP_A <- factor(filtered_prev$EDUCP_A)

# Education level labels
edu_labels <- c(
  "1" = "<9th grade", "2" = "9–11th grade", "3" = "High school", 
  "4" = "Some college", "5" = "Assoc. (vocational)", "6" = "Assoc. (academic)", 
  "7" = "Bachelor's", "8" = "Master's", "9" = "Professional", "10" = "Doctorate"
)

# Chi-squared test: Education vs Long COVID Prevalence
contingency_prev <- table(filtered_prev$EDUCP_A, filtered_prev$LONGCOVD1_A)
chisq.test(contingency_prev)

# Plot: Prevalence by education
ggplot(filtered_prev, aes(x = EDUCP_A, fill = LONGCOVD1_A)) +
  geom_bar(position = "fill") +
  scale_x_discrete(labels = edu_labels) +
  scale_fill_scico_d(palette = "vik", direction = -1) +
  labs(title = "Prevalence of Long COVID by Education Level",
       x = "Education Level", y = "Proportion",
       fill = "Long COVID") +
  theme_pubr(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ================================
# SECTION 2: Severity Analysis
# ================================

# Filter valid cases for severity test
filtered <- subset(data, 
                   LONGCOVD1_A == 1 & 
                     !(EDUCP_A %in% c(97, 99)) & 
                     !(LCVDACT_A %in% c(8, 9)))

# Convert to factors
filtered$EDUCP_A <- factor(filtered$EDUCP_A)
filtered$LCVDACT_A <- factor(filtered$LCVDACT_A, 
                             levels = c(1, 2, 3), 
                             labels = c("Not at all", "A little", "A lot"))

# Chi-squared test: Education vs Long COVID Severity
contingency_table <- table(filtered$EDUCP_A, filtered$LCVDACT_A)
chisq.test(contingency_table)

# Prepare data for plot
filtered$SeverityLabel <- filtered$LCVDACT_A
plot_data <- prop.table(table(filtered$EDUCP_A, filtered$SeverityLabel), margin = 1) * 100
df_plot <- as.data.frame(plot_data)
colnames(df_plot) <- c("Education", "Severity", "Percentage")

# Plot: Severity by education
ggplot(df_plot, aes(x = Education, y = Percentage, fill = Severity)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(labels = edu_labels) +
  scale_fill_manual(values = c("Not at all" = "#66c2a5", 
                               "A little" = "#fc8d62", 
                               "A lot" = "#8da0cb")) +
  labs(title = "Long COVID Severity by Education Level",
       y = "Percentage (%)", x = "Education Level") +
  theme_pubr(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



######## Ordinal Logistic Regression ########

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

# View contingency table stratified by sex
table(filtered$EDUCP_A, filtered$SeverityLabel, filtered$SEX_A)

# Results
mantelhaen.test(cmh_table)



######## Multivariate Logistic Regression ########

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

# Get coefficients and p-values
summary_table <- summary(model_multi)$coefficients

# Compute Odds Ratios and CI
ORs <- exp(coef(model_multi))
CIs <- exp(confint(model_multi))
result_table <- cbind(OR = ORs, CI_lower = CIs[,1], CI_upper = CIs[,2], summary_table[,4, drop = FALSE])
round(result_table, 3)

# View nicely in R
library(knitr)
kable(round(result_table, 3), caption = "Odds Ratios for Long COVID Presence")

#######################3
# Load required libraries
library(dplyr)
library(broom)
library(ggplot2)
library(forcats)

# Ensure EDUCP_A exists in your dataset
if (!"EDUCP_A" %in% colnames(filtered_data)) stop("EDUCP_A not found in filtered_data!")

# --- Recode SES Variables inside mutate ---
filtered_data <- filtered_data %>%
  mutate(
    collapsed_EDU = factor(case_when(
      EDUCP_A %in% 1:3 ~ "Low",
      EDUCP_A %in% 4:6 ~ "Medium",
      EDUCP_A %in% 7:10 ~ "High",
      TRUE ~ NA_character_
    ), levels = c("Low", "Medium", "High")),
    
    collapsed_INC = factor(case_when(
      INCWRKO_A %in% c(1, 2) ~ "Low",
      INCWRKO_A %in% c(3, 4, 5) ~ "Medium",
      INCWRKO_A %in% c(6, 7, 8) ~ "High",
      TRUE ~ NA_character_
    ), levels = c("Low", "Medium", "High")),
    
    collapsed_EMP = factor(case_when(
      EMPWRKFT1_A == 1 ~ "Employed Full-Time",
      EMPWRKFT1_A == 2 ~ "Employed Part-Time",
      EMPWRKFT1_A == 3 ~ "Not Working",
      TRUE ~ NA_character_
    ), levels = c("Employed Full-Time", "Employed Part-Time", "Not Working"))
  )

# --- Fit logistic regression model ---
model <- glm(LONGCOVD1_A ~ collapsed_EDU + collapsed_INC + collapsed_EMP +
               HISTOPCOST_A + AGEP_A + SEX_A + DISAB3_A +
               PHQ2SCREEN_A + GAD2SCREEN_A,
             data = filtered_data, family = binomial)

# --- Tidy model output ---
model_tidy <- tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  filter(!is.na(estimate), conf.low > 0, conf.high > 0) %>% # prevent log10 errors
  mutate(
    group = case_when(
      grepl("collapsed_EDU", term) ~ "Education",
      grepl("collapsed_INC", term) ~ "Income",
      grepl("collapsed_EMP", term) ~ "Employment",
      grepl("HISTOPCOST", term) ~ "Healthcare Cost Barrier",
      grepl("AGEP", term) ~ "Age",
      grepl("SEX", term) ~ "Sex",
      grepl("DISAB3", term) ~ "Disability",
      grepl("PHQ2SCREEN|GAD2SCREEN", term) ~ "Mental Health",
      TRUE ~ "Other"
    ),
    term = gsub("collapsed_", "", term),
    term = gsub("_A", "", term),
    term = fct_rev(fct_inorder(term))
  )

# --- Plot Odds Ratios ---
ggplot(model_tidy, aes(x = estimate, y = term, color = group)) +
  geom_point(size = 2.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray30") +
  scale_x_log10() +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Adjusted Odds Ratios for Long COVID Prevalence",
    subtitle = "95% Confidence Intervals (log scale)",
    x = "Odds Ratio (log scale)",
    y = "Predictor Variable",
    color = "Variable Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 10)
  )



################################## 
# Load required libraries
library(dplyr)
library(ggplot2)
library(broom)
library(forcats)

# Step 1: Filter dataset and recode SES and outcome
filtered_data <- data %>%
  filter(AGEP_A >= 18 & AGEP_A <= 65) %>%
  mutate(
    collapsed_EDU = factor(case_when(
      EDUCP_A %in% 1:3 ~ "Low",
      EDUCP_A %in% 4:6 ~ "Medium",
      EDUCP_A %in% 7:10 ~ "High",
      TRUE ~ NA_character_
    ), levels = c("Low", "Medium", "High")),
    
    collapsed_INC = factor(case_when(
      INCWRKO_A == 1 ~ "LowIncome",
      INCWRKO_A == 2 ~ "MidIncome",
      INCWRKO_A == 8 ~ "HighIncome",
      TRUE ~ NA_character_
    )),
    
    LONGCOVD_clean = case_when(
      LONGCOVD1_A == 1 ~ 1,
      LONGCOVD1_A == 2 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(LONGCOVD_clean))

# Step 2: Fit logistic regression model with interaction
model_interact <- glm(
  LONGCOVD_clean ~ collapsed_EDU * DISAB3_A + HISTOPCOST_A + SEX_A + AGEP_A,
  data = filtered_data,
  family = binomial
)

# Step 3: Tidy model output for visualization
model_tidy <- tidy(model_interact, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  filter(!is.na(conf.low) & !is.na(conf.high)) %>%
  mutate(
    group = case_when(
      grepl("collapsed_EDU", term) ~ "Education",
      grepl("DISAB3", term) ~ "Disability",
      grepl("HISTOPCOST", term) ~ "Healthcare Cost",
      grepl("SEX", term) ~ "Sex",
      grepl("AGEP", term) ~ "Age",
      TRUE ~ "Other"
    ),
    term = fct_reorder(term, estimate)
  )

# Step 4: Plot Odds Ratios with 95% CIs
ggplot(model_tidy, aes(x = estimate, y = fct_rev(term), color = group)) +
  geom_point(size = 2.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.25) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray30") +
  scale_x_log10() +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Adjusted Odds Ratios for Long COVID (Interaction Model)",
    subtitle = "Interaction: Education × Disability | Adults 18–65",
    x = "Odds Ratio (log scale)",
    y = "Predictor Variable",
    color = "Variable Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )




alias(model_interact)$Complete
table(data$EDUCP_A)


######## Effect Size for ANOVA-lika Analysis ########

anova_data <- subset(data, !is.na(PHQ2SCREEN_A) & !is.na(EDUCP_A) & !is.na(INCWRKO_A))

anova_data$EDUCP_A <- factor(anova_data$EDUCP_A)
anova_data$INCWRKO_A <- factor(anova_data$INCWRKO_A)

anova_model <- aov(PHQ2SCREEN_A ~ EDUCP_A + INCWRKO_A, data = anova_data)
summary(anova_model)

# Effect size
install.packages("effects")
library(lsr)
etaSquared(anova_model)

library(effects)
plot(allEffects(model_ord), main = "Effect of Education and Income on Long COVID Severity")
ggplot(multi_data, aes(x = AGEP_A, y = LONGCOVD1_A)) +
  geom_smooth(method = "loess") +
  labs(title = "Age vs Long COVID Risk",
       x = "Age", y = "Probability of Long COVID") +
  theme_minimal()




######## Cochran–Mantel–Haenszel (CMH) Test ########


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




######## Visualization (Stacked Bar Plot) ######## 

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



write.csv(result_table, "logistic_model_results.csv")
write.csv(df_plot, "severity_by_education_plotdata.csv")









######## Interaction Effects in Logistic Regression ########

model_interaction <- glm(LONGCOVD1_A ~ EDUCP_A * INCWRKO_A + HISTOPCOST_A + SEX_A + AGEP_A,
                         data = multi_data, family = binomial)

summary(model_interaction)










######## Stratified Logistic Regression ########

young <- subset(multi_data, AGEP_A < 40)
old <- subset(multi_data, AGEP_A >= 40)

glm(LONGCOVD1_A ~ EDUCP_A + INCWRKO_A + HISTOPCOST_A, data = young, family = binomial)
glm(LONGCOVD1_A ~ EDUCP_A + INCWRKO_A + HISTOPCOST_A, data = old, family = binomial)




######## Heatmap: Cross-tab of Severity × Education × Income ########

library(ggplot2)

filtered$Income <- factor(filtered$INCWRKO_A)
heat_data <- as.data.frame(table(filtered$EDUCP_A, filtered$Income, filtered$SeverityLabel))
colnames(heat_data) <- c("Education", "Income", "Severity", "Count")

ggplot(heat_data, aes(x = Education, y = Income, fill = Count)) +
  geom_tile() +
  facet_wrap(~ Severity) +
  scale_fill_viridis_c() +
  labs(title = "Long COVID Severity: Education × Income", x = "Education", y = "Income") +
  theme_minimal()


######## Occupational Risk Analysis ########

# Compare Long COVID prevalence by occupation group
table(data$LONGCOVD1_A, data$EMDOCCUPN1_A)

# Logistic model with occupation
model_occ <- glm(LONGCOVD1_A ~ factor(EMDOCCUPN1_A) + EDUCP_A + INCWRKO_A, 
                 data = multi_data, family = binomial)
summary(model_occ)



######## SES Disadvantage Index ########

multi_data$SES_Index <- 
  as.numeric(multi_data$EDUCP_A %in% 1:3) +     # low education
  as.numeric(multi_data$INCWRKO_A %in% 1:4) +   # low income
  as.numeric(multi_data$HISTOPCOST_A == "Yes")  # had to stop insurance due to cost

ggplot(multi_data, aes(x = SES_Index, fill = factor(LONGCOVD1_A))) +
  geom_bar(position = "fill") +
  labs(title = "Long COVID Prevalence by SES Disadvantage Score", y = "Proportion")



######## Follow up-regression ########

model_ses <- glm(LONGCOVD1_A ~ SES_Index + AGEP_A + SEX_A,
                 data = multi_data, family = binomial)
summary(model_ses)
exp(coef(model_ses))  # Odds ratios


######## Ordinal regression for severity ########

severity_data <- subset(multi_data, LONGCOVD1_A == 1 & !is.na(LCVDACT_A))
severity_data$LCVDACT_A <- as.numeric(as.character(severity_data$LCVDACT_A))

# Use ordinal logistic regression
library(MASS)
model_severity <- polr(factor(LCVDACT_A, ordered = TRUE) ~ SES_Index + AGEP_A + SEX_A,
                       data = severity_data, Hess = TRUE)
summary(model_severity)


######## Facet bar plots by sex or age group ########

ggplot(multi_data, aes(x = SES_Index, fill = factor(LONGCOVD1_A))) +
  geom_bar(position = "fill") +
  facet_wrap(~ SEX_A, labeller = label_both) +
  labs(title = "Long COVID Prevalence by SES and Sex",
       y = "Proportion", fill = "Long COVID")




######## (Healthcare Access Barrier) to Prevalence Model ########

# Make sure HISTOPCOST_A is a clean factor
multi_data <- subset(multi_data, HISTOPCOST_A %in% c("Yes", "No"))
multi_data$HISTOPCOST_A <- factor(multi_data$HISTOPCOST_A, levels = c("No", "Yes"))

# Update model
model_ses_access <- glm(LONGCOVD1_A ~ SES_Index + HISTOPCOST_A + AGEP_A + SEX_A,
                        data = multi_data, family = binomial)

summary(model_ses_access)
exp(cbind(OddsRatio = coef(model_ses_access), confint(model_ses_access)))



######## Test for SES × Sex Interaction (Moderation) ########

model_interaction <- glm(LONGCOVD1_A ~ SES_Index * SEX_A + AGEP_A,
                         data = multi_data, family = binomial)

summary(model_interaction)



######## Visualize SES × Sex Interaction ########

library(ggplot2)

ggplot(multi_data, aes(x = SES_Index, fill = factor(LONGCOVD1_A))) +
  geom_bar(position = "fill") +
  facet_wrap(~ SEX_A, labeller = label_both) +
  labs(title = "Long COVID Prevalence by SES and Sex",
       x = "SES Disadvantage Index", y = "Proportion",
       fill = "Long COVID (0 = No, 1 = Yes)") +
  theme_minimal()



######## Rebuild SES Index with components ########

multi_data$SES_Education <- as.numeric(multi_data$EDUCP_A %in% c(1, 2, 3))
multi_data$SES_Income    <- as.numeric(multi_data$INCWRKO_A %in% c(1, 2, 3, 4))
multi_data$SES_Access    <- as.numeric(multi_data$HISTOPCOST_A == "Yes")

# Visualize contribution of each SES component
logit_components <- glm(LONGCOVD1_A ~ SES_Education + SES_Income + SES_Access + SEX_A + AGEP_A,
                        data = multi_data, family = binomial)
summary(logit_components)







##################### Debugging

# List all column names containing 'income' or 'INC'
grep("income", names(data), ignore.case = TRUE, value = TRUE)
grep("INC", names(data), value = TRUE)


grep("cost", names(data), ignore.case = TRUE, value = TRUE)
grep("care", names(data), ignore.case = TRUE, value = TRUE)


nrow(filtered)
table(filtered$EDUCP_A, filtered$LCVDACT_A)

plot_data <- prop.table(table(filtered$EDUCP_A, filtered$SeverityLabel), margin = 1) * 100
as.data.frame(plot_data)



filtered$SeverityLabel <- factor(filtered$LCVDACT_A, 
                                 levels = c(1, 2, 3),
                                 labels = c("Not at all", "A little", "A lot"))



# Recreate SeverityLabel directly from LCVDACT_A as character levels
filtered$SeverityLabel <- factor(as.numeric(as.character(filtered$LCVDACT_A)),
                                 levels = c(1, 2, 3),
                                 labels = c("Not at all", "A little", "A lot"))


# Proportional table
plot_data <- prop.table(table(filtered$EDUCP_A, filtered$SeverityLabel), margin = 1) * 100
df_plot <- as.data.frame(plot_data)
colnames(df_plot) <- c("Education", "Severity", "Percentage")

# Plot
ggplot(df_plot, aes(x = Education, y = Percentage, fill = Severity)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Not at all" = "green", "A little" = "yellow", "A lot" = "red")) +
  labs(title = "Long COVID Severity by Education Level",
       x = "Education Level (EDUCP_A)", y = "Percentage (%)") +
  theme_minimal()

# Fresh filtered dataset
filtered <- subset(data,
                   LONGCOVD1_A == 1 &
                     !(EDUCP_A %in% c(97, 99)) &
                     !(LCVDACT_A %in% c(8, 9)) &
                     SEX_A %in% c(1, 2))  # keep valid sexes

# Convert to numeric if LCVDACT_A is stored as factor
filtered$LCVDACT_A <- as.numeric(as.character(filtered$LCVDACT_A))

# Create clean severity label factor
filtered$SeverityLabel <- factor(filtered$LCVDACT_A,
                                 levels = c(1, 2, 3),
                                 labels = c("Not at all", "A little", "A lot"))

# Check
table(filtered$EDUCP_A, filtered$SeverityLabel)



library(ggplot2)

# Proportional table
prop_tab <- prop.table(table(filtered$EDUCP_A, filtered$SeverityLabel), margin = 1) * 100
df_plot <- as.data.frame(prop_tab)
colnames(df_plot) <- c("Education", "Severity", "Percentage")

# Fix factor levels for EDUCP_A (to show in order)
df_plot$Education <- factor(df_plot$Education, levels = as.character(1:10))

# Plot
ggplot(df_plot, aes(x = Education, y = Percentage, fill = Severity)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Not at all" = "green", "A little" = "yellow", "A lot" = "red")) +
  labs(title = "Long COVID Severity by Education Level",
       x = "Education Level (EDUCP_A)", y = "Percentage (%)") +
  theme_minimal()

model_full <- glm(LONGCOVD1_A ~ EDUCP_A + INCWRKO_A + HISTOPCOST_A +
                    AGEP_A + SEX_A + RACEALLP_A + URBRRL +
                    DISAB3_A + PHQ2SCREEN_A + GAD2SCREEN_A +
                    EMPWRKFT1_A + EMDINDSTN1_A + EMDOCCUPN1_A,
                  data = multi_data, family = binomial)
summary(model_full)




library(MASS)

model_severity <- polr(factor(LCVDACT_A, ordered = TRUE) ~
                         EDUCP_A + INCWRKO_A + HISTOPCOST_A +
                         AGEP_A + SEX_A + DISAB3_A + PHQ2SCREEN_A,
                       data = your_data, Hess = TRUE)
summary(model_severity)















#################################
library(ggplot2)
library(broom)
library(dplyr)
library(forcats)

# Tidy the model with CIs and exponentiated odds ratios
model_tidy <- broom::tidy(model_full, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)")

# Manually define predictor groups for display
model_tidy <- model_tidy %>%
  mutate(group = case_when(
    grepl("EDUCP", term) ~ "Education Level",
    grepl("INCWRKO", term) ~ "Income Level",
    grepl("HISTOPCOST", term) ~ "Healthcare Access",
    grepl("AGEP", term) ~ "Age",
    grepl("SEX", term) ~ "Sex",
    grepl("RACE", term) ~ "Race/Ethnicity",
    grepl("URBRRL", term) ~ "Urban/Rural",
    grepl("DISAB", term) ~ "Disability",
    grepl("PHQ2", term) ~ "Depression",
    grepl("GAD2", term) ~ "Anxiety",
    grepl("EMPWRKFT", term) ~ "Employment",
    grepl("EMDINDSTN", term) ~ "Industry",
    grepl("EMDOCCUPN", term) ~ "Occupation",
    TRUE ~ "Other"
  ))

# Set order manually within groups
model_tidy <- model_tidy %>%
  mutate(term = fct_inorder(term))  # Keeps original order, no sorting by OR

# Plot grouped forest plot
ggplot(model_tidy, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, linewidth = 1) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  scale_x_log10() +
  facet_grid(group ~ ., scales = "free_y", space = "free", switch = "y") +
  labs(title = "Grouped Odds Ratios (95% CI) for Long COVID",
       x = "Odds Ratio (log scale)", y = NULL,
       caption = "Red line = No effect (OR = 1)") +
  theme_minimal(base_size = 14) +
  theme(strip.text.y = element_text(angle = 0, hjust = 0, face = "bold"),
        axis.text.y = element_text(size = 11),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold"))
###############################
library(ggplot2)
library(broom)
library(dplyr)
library(forcats)
library(scales)
library(grid)  # for unit()

# Clean and prepare
model_tidy <- broom::tidy(model_full, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  filter(!term %in% c("EDUCP_A9", "EDUCP_A10")) %>%
  mutate(
    group = case_when(
      grepl("EDUCP", term) ~ "Education Level",
      grepl("INCWRKO", term) ~ "Income Level",
      grepl("HISTOPCOST", term) ~ "Healthcare Access",
      grepl("AGEP", term) ~ "Age",
      grepl("SEX", term) ~ "Sex",
      grepl("RACE", term) ~ "Race/Ethnicity",
      grepl("URBRRL", term) ~ "Urbanicity",
      grepl("DISAB", term) ~ "Disability",
      grepl("PHQ2", term) ~ "Depression Screening",
      grepl("GAD2", term) ~ "Anxiety Screening",
      grepl("EMPWRKFT", term) ~ "Work Status",
      grepl("EMDINDSTN", term) ~ "Industry",
      grepl("EMDOCCUPN", term) ~ "Occupation",
      TRUE ~ "Other"
    ),
    label = case_when(
      term == "AGEP_A" ~ "Age",
      term == "SEX_A2" ~ "Female",
      term == "DISAB3_A" ~ "Disabled",
      term == "PHQ2SCREEN_A" ~ "PHQ-2 Positive",
      term == "GAD2SCREEN_A" ~ "GAD-2 Positive",
      term == "HISTOPCOST_AYes" ~ "Delayed Care (Cost)",
      term == "EMPWRKFT1_A" ~ "Working Full-Time",
      TRUE ~ term
    ),
    term = fct_inorder(label)
  ) %>%
  filter(conf.high < 50)

# Plot
ggplot(model_tidy, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.25, linewidth = 1) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  scale_x_log10(breaks = c(0.1, 0.5, 1, 2, 5, 10, 20), labels = label_log()) +
  facet_grid(group ~ ., scales = "free_y", space = "free", switch = "y") +
  labs(
    title = "Adjusted Odds Ratios for Long COVID by Predictors",
    subtitle = "With 95% Confidence Intervals (log scale)",
    x = "Odds Ratio (log10 scale)",
    y = NULL,
    caption = "Red line = No effect (OR = 1)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text.y = element_text(angle = 0, hjust = 0, face = "bold"),
    axis.text.y = element_text(size = 11, lineheight = 1.4),  # <- vertical spacing
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 10)),
    plot.caption = element_text(margin = margin(t = 10)),
    panel.spacing = unit(1, "lines")  # <- spacing between facet groups
  )

############################

