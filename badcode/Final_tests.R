######## Setup and load data ########

# Load required packages
#install.packages(("scico"))
library(readxl)
library(ggplot2)
library(MASS)       # for polr
library(ordinal)    # for clm
library(car)        # for VIF and Anova
library(DescTools)  # for CMH test

# Load the CSV data
data <- read.csv("C:/Users/Luigi/OneDrive/ZHAW/6. Semester/BiEp_Sem6/adult23.csv")

# Load dplyr
library(dplyr)

# Define relevant variables including gender
relevant_vars <- c(
  "AGEP_A", "LONGCOVD1_A", "LCVDACT_A",            # Long COVID and age
  "POVRATTC_A", "RATCAT_A",                        # Socioeconomic
  "EDUCP_A",                                       # Education
  "USUALPL_A", "MEDDL12M_A", "MEDNG12M_A", "TRANSPOR_A", # Healthcare access
  "SEX_A"                                          # Gender
)

# Select only the relevant columns
filtered_data <- data %>%
  select(all_of(relevant_vars))

# Clean and filter the dataset (also add gender as a factor)
filtered_clean <- filtered_data %>%
  filter(
    !LONGCOVD1_A %in% c(7, 8, 9),
    !LCVDACT_A %in% c(7, 8, 9),
    !EDUCP_A %in% c(97, 98, 99),
    !USUALPL_A %in% c(7, 8, 9),
    !MEDDL12M_A %in% c(7, 8, 9),
    !MEDNG12M_A %in% c(7, 8, 9),
    !TRANSPOR_A %in% c(7, 8, 9),
    LONGCOVD1_A %in% c(1, 2),          # Keep valid Long COVID prevalence
    AGEP_A >= 18 & AGEP_A <= 65,       # Age between 18–65
    SEX_A %in% c(1, 2)                 # Only Male and Female
  ) %>%
  mutate(
    # Recode long COVID status
    LONGCOV_YN = factor(LONGCOVD1_A, levels = c(2, 1), labels = c("No Long COVID", "Long COVID")),
    # Recode gender
    GENDER = factor(SEX_A, levels = c(1, 2), labels = c("Male", "Female"))
  )

# Preview the cleaned data
summary(filtered_clean)




#########
#########


# Recode LONGCOVD1_A: 1 = Yes (has Long COVID), 2 = No
# We'll create a new binary variable LONGCOV_YN for easier modeling
filtered_clean <- filtered_clean %>%
  mutate(LONGCOV_YN = ifelse(LONGCOVD1_A == 1, 1, 0))



# Summarize key SES and healthcare access variables by LONGCOV_YN
library(dplyr)

descriptive_stats <- filtered_clean %>%
  group_by(LONGCOV_YN) %>%
  summarise(
    count = n(),
    mean_povratio = mean(POVRATTC_A, na.rm = TRUE),
    mean_income_cat = mean(RATCAT_A, na.rm = TRUE),
    mean_education = mean(EDUCP_A, na.rm = TRUE),
    mean_usualpl = mean(USUALPL_A, na.rm = TRUE),
    mean_meddelay = mean(MEDDL12M_A, na.rm = TRUE),
    mean_medneeded = mean(MEDNG12M_A, na.rm = TRUE),
    mean_transport = mean(TRANSPOR_A, na.rm = TRUE),
    mean_age = mean(AGEP_A, na.rm = TRUE)
  )
print(descriptive_stats)


#########
#########


#📝 Descriptive Analysis Summary (Report Text)
#To explore the relationship between socioeconomic status (SES) and Long COVID, we computed group-wise descriptive statistics comparing individuals with and without self-reported Long COVID. The analysis included variables representing poverty ratio, income category, education level, access to healthcare, and age.

#Our results show that individuals with Long COVID tend to have a lower poverty ratio (3.92 vs. 4.73), lower income category (9.5 vs. 10.5), and slightly lower education levels (6.04 vs. 6.35) compared to those without Long COVID. These differences suggest that lower SES may be associated with higher Long COVID prevalence, supporting our initial hypothesis. Additionally, individuals with Long COVID reported slightly fewer delays or barriers to healthcare, which may reflect different healthcare-seeking behavior or access patterns. The average age in the Long COVID group was also slightly lower.

#This descriptive overview provides initial evidence that SES factors may influence Long COVID outcomes and sets the stage for further statistical testing.



#########
#########

# Load required libraries
library(ggplot2)
library(dplyr)

# Load the dataset
data <- read.csv("C:/Users/Luigi/OneDrive/ZHAW/6. Semester/BiEp_Sem6/adult23.csv")

# Select only relevant variables (including gender and age)
relevant_vars <- c(
  "AGEP_A", "LONGCOVD1_A", "LCVDACT_A",            # Long COVID and age
  "POVRATTC_A", "RATCAT_A",                        # Socioeconomic
  "EDUCP_A",                                       # Education
  "USUALPL_A", "MEDDL12M_A", "MEDNG12M_A", "TRANSPOR_A", # Healthcare access
  "SEX_A"                                          # Gender
)

# Keep relevant variables
filtered_data <- data %>%
  select(all_of(relevant_vars))

# Clean and label data
filtered_clean <- filtered_data %>%
  filter(
    AGEP_A >= 18 & AGEP_A <= 65,
    LONGCOVD1_A %in% c(1, 2),
    !LCVDACT_A %in% c(7, 8, 9),
    !EDUCP_A %in% c(97, 98, 99),
    !USUALPL_A %in% c(7, 8, 9),
    !MEDDL12M_A %in% c(7, 8, 9),
    !MEDNG12M_A %in% c(7, 8, 9),
    !TRANSPOR_A %in% c(7, 8, 9),
    SEX_A %in% c(1, 2)
  ) %>%
  mutate(
    LONGCOV_YN = factor(
      case_when(
        LONGCOVD1_A == 1 ~ 1,
        LONGCOVD1_A == 2 ~ 0
      ),
      levels = c(0, 1),
      labels = c("No Long COVID", "Long COVID")
    ),
    GENDER = factor(SEX_A, labels = c("Male", "Female")),
    USUALPL_LABEL = factor(USUALPL_A, levels = c(1, 2, 3), labels = c("Yes", "No", "More than one"))
  )

# ========================== PLOTTING ===========================

# Poverty Ratio
ggplot(filtered_clean, aes(x = LONGCOV_YN, y = POVRATTC_A, fill = LONGCOV_YN)) +
  geom_boxplot() +
  labs(
    title = "Poverty Ratio by Long COVID Status",
    x = "Long COVID Status",
    y = "Poverty Ratio (0 = Lowest Income, 11 = Highest)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Education Level
ggplot(filtered_clean, aes(x = LONGCOV_YN, y = EDUCP_A, fill = LONGCOV_YN)) +
  geom_boxplot() +
  labs(
    title = "Education Level by Long COVID Status",
    x = "Long COVID Status",
    y = "Education Code (1 = No Schooling, 10 = Doctorate)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Usual Place for Medical Care
ggplot(filtered_clean, aes(x = USUALPL_LABEL, fill = LONGCOV_YN)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
  labs(
    title = "Access to Usual Place for Medical Care by Long COVID Status",
    x = "Has a Usual Place for Care",
    y = "Number of Respondents",
    fill = "Long COVID Status"
  ) +
  theme_minimal()

# Age Distribution
ggplot(filtered_clean, aes(x = LONGCOV_YN, y = AGEP_A, fill = LONGCOV_YN)) +
  geom_boxplot() +
  labs(
    title = "Age Distribution by Long COVID Status",
    x = "Long COVID Status",
    y = "Age (years)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Gender Distribution by Long COVID Status
ggplot(filtered_clean, aes(x = GENDER, fill = LONGCOV_YN)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
  labs(
    title = "Gender Distribution by Long COVID Status",
    x = "Gender",
    y = "Number of Respondents",
    fill = "Long COVID Status"
  ) +
  theme_minimal()





########
########
# Convert relevant variables to factors for chi-squared tests
filtered_clean <- filtered_clean %>%
  mutate(
    RATCAT_A = as.factor(RATCAT_A),
    EDUCP_A = as.factor(EDUCP_A),
    USUALPL_A = as.factor(USUALPL_A),
    MEDDL12M_A = as.factor(MEDDL12M_A),
    MEDNG12M_A = as.factor(MEDNG12M_A),
    TRANSPOR_A = as.factor(TRANSPOR_A)
  )

# Run Chi-squared test for each categorical predictor vs LONGCOV_YN
chisq_test_results <- list(
  income_cat = chisq.test(table(filtered_clean$LONGCOV_YN, filtered_clean$RATCAT_A)),
  education = chisq.test(table(filtered_clean$LONGCOV_YN, filtered_clean$EDUCP_A)),
  usual_source = chisq.test(table(filtered_clean$LONGCOV_YN, filtered_clean$USUALPL_A)),
  med_delay = chisq.test(table(filtered_clean$LONGCOV_YN, filtered_clean$MEDDL12M_A)),
  med_needed = chisq.test(table(filtered_clean$LONGCOV_YN, filtered_clean$MEDNG12M_A)),
  transport = chisq.test(table(filtered_clean$LONGCOV_YN, filtered_clean$TRANSPOR_A))
)

# Print p-values from all tests
lapply(chisq_test_results, function(test) test$p.value)

###########
###########

# Load ggplot2 if not already
library(ggplot2)

# Helper function to plot proportion of Long COVID by categorical variable
plot_proportions <- function(var, var_label) {
  ggplot(filtered_clean, aes_string(x = var, fill = "LONGCOV_YN")) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("#F8766D", "#00BFC4"),
                      labels = c("No Long COVID", "Long COVID")) +
    labs(
      title = paste("Proportion of Long COVID by", var_label),
      x = var_label,
      y = "Proportion of Respondents",
      fill = "Long COVID Status"
    ) +
    theme_minimal()
}

# Generate and print plots
#plot_proportions("RATCAT_A", "Income Category")
#plot_proportions("EDUCP_A", "Education Level")
plot_proportions("MEDDL12M_A", "Delayed Medical Care")
plot_proportions("MEDNG12M_A", "Unmet Medical Need")
plot_proportions("TRANSPOR_A", "Transportation Barrier")


library(ggplot2)
library(dplyr)

# === Create labeled factors ===

# Income labels from codebook
income_labels <- c(
  "0.00–0.49", "0.50–0.74", "0.75–0.99", "1.00–1.24", "1.25–1.49",
  "1.50–1.74", "1.75–1.99", "2.00–2.49", "2.50–2.99", "3.00–3.49",
  "3.50–3.99", "4.00–4.49", "4.50–4.99", "5.00 or greater"
)

filtered_clean$RATCAT_LABEL <- factor(filtered_clean$RATCAT_A,
                                      levels = as.character(1:14),
                                      labels = income_labels
)

# Education labels from codebook
education_labels <- c(
  "Grade 1–11", "12th grade, no diploma", "GED or equivalent",
  "High School Graduate", "Some college, no degree",
  "Assoc. degree: occupational/technical", "Assoc. degree: academic",
  "Bachelor’s degree", "Master’s degree", "Doctorate/Prof. degree"
)

filtered_clean$EDUCP_LABEL <- factor(filtered_clean$EDUCP_A,
                                     levels = as.character(1:10),
                                     labels = education_labels
)

# === Bar Plot: Income Distribution by Long COVID ===
ggplot(filtered_clean, aes(x = RATCAT_LABEL, fill = LONGCOV_YN)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"),
                    labels = c("No Long COVID", "Long COVID")) +
  labs(
    title = "Income-to-Poverty Ratio by Long COVID Status",
    x = "Income Bracket (Ratio to Poverty Threshold)",
    y = "Number of Respondents",
    fill = "Long COVID Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# === Bar Plot: Education Distribution by Long COVID ===
ggplot(filtered_clean, aes(x = EDUCP_LABEL, fill = LONGCOV_YN)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"),
                    labels = c("No Long COVID", "Long COVID")) +
  labs(
    title = "Education Level by Long COVID Status",
    x = "Educational Attainment",
    y = "Number of Respondents",
    fill = "Long COVID Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#We visualized the distribution of income levels and educational attainment among individuals with and without Long COVID. The income data is shown as income-to-poverty threshold ratios, while education is categorized based on completed degrees. The plots reveal notable differences in distribution, which support the chi-squared test results indicating significant associations between socioeconomic status and Long COVID prevalence.
ggplot(filtered_clean, aes(x = POVRATTC_A)) + geom_histogram()
#we also looked at the distribution to make sure biases are ...



##############
#############



# Logistic regression to predict likelihood of Long COVID based on SES and access
model_prev <- glm(
  LONGCOV_YN ~ POVRATTC_A + EDUCP_A + USUALPL_A + MEDDL12M_A + MEDNG12M_A + TRANSPOR_A + AGEP_A,
  data = filtered_clean,
  family = binomial
)

# Show summary of logistic regression model
summary(model_prev)



#########
#########

#We used a logistic regression model to predict the likelihood of reporting Long COVID symptoms based on socioeconomic and healthcare access variables. The model revealed that:
  
#  Lower income (as measured by the poverty ratio) significantly increased the odds of Long COVID.

#Delayed or unmet medical needs due to cost, as well as lack of transportation, were strong predictors of higher Long COVID prevalence.

#Older adults had slightly elevated risk.

#Interestingly, individuals with mid-tier education levels (e.g., some college, associate, bachelor's) had higher odds, potentially reflecting greater workplace exposure or reporting awareness.

#Having a usual place for medical care showed a weak and non-significant association.

#########
#########

# --- Required Libraries ---
library(dplyr)
library(rcompanion)
library(pwr)

# --- Step 1: Prepare Derived Variables ---

# Income group: Low (≤ 5) vs High (> 5)
filtered_clean$income_group <- ifelse(as.numeric(as.character(filtered_clean$RATCAT_A)) <= 5, "Low", "High")

# Long COVID already binary; rename to avoid confusion
filtered_clean$longcovid <- filtered_clean$LONGCOV_YN

# Age group: <= Median (42) = Younger, > 42 = Older
filtered_clean$age_group <- ifelse(filtered_clean$AGEP_A <= 42, "Younger", "Older")

# Check structure
table(filtered_clean$income_group, filtered_clean$longcovid, filtered_clean$age_group)

# --- Step 2: Mantel–Haenszel Test ---
mantel_result <- mantelhaen.test(
  x = table(filtered_clean$income_group, filtered_clean$longcovid, filtered_clean$age_group)
)
print(mantel_result)

# --- Step 3: Fisher’s Exact Test ---
fisher_result <- fisher.test(
  table(filtered_clean$LONGCOV_YN, filtered_clean$USUALPL_A)
)
print(fisher_result)

# --- Step 4: Logistic Regression with Interaction Term (Gender × Poverty Ratio) ---
model_interaction <- glm(
  LONGCOV_YN ~ POVRATTC_A * GENDER + EDUCP_A + USUALPL_A + MEDDL12M_A + MEDNG12M_A + TRANSPOR_A + AGEP_A,
  data = filtered_clean,
  family = binomial
)
summary(model_interaction)

# --- Step 5: Power Calculation ---
# For chi-squared test: use previously computed Cramér's V effect size
effect_size <- cramerV(table(filtered_clean$LONGCOV_YN, filtered_clean$RATCAT_A))  # ≈ 0.1023
sample_size <- nrow(filtered_clean)
degrees_freedom <- length(unique(filtered_clean$RATCAT_A)) - 1  # 14 categories → df = 13

power_result <- pwr.chisq.test(
  w = effect_size,
  N = sample_size,
  df = degrees_freedom,
  sig.level = 0.05
)
print(power_result)







#########
#########

#############
#################Severity##########


# --- Severity Analysis with Interaction and Restructuring ---

# Load required libraries
library(dplyr)
library(ggplot2)
library(broom)
library(forcats)

# Step 1: Prepare and clean dataset with recoded SES and outcomes
filtered_data <- data %>%
  filter(AGEP_A >= 18 & AGEP_A <= 65) %>%
  mutate(
    # Collapse education levels
    collapsed_EDU = factor(case_when(
      EDUCP_A %in% 1:3 ~ "Low",
      EDUCP_A %in% 4:6 ~ "Medium",
      EDUCP_A %in% 7:10 ~ "High",
      TRUE ~ NA_character_
    ), levels = c("Low", "Medium", "High")),
    
    # Collapse income levels
    collapsed_INC = factor(case_when(
      INCWRKO_A == 1 ~ "LowIncome",
      INCWRKO_A == 2 ~ "MidIncome",
      INCWRKO_A == 8 ~ "HighIncome",
      TRUE ~ NA_character_
    )),
    
    # Clean Long COVID variable
    LONGCOVD_clean = case_when(
      LONGCOVD1_A == 1 ~ 1,
      LONGCOVD1_A == 2 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(LONGCOVD_clean))

# Step 2: Fit logistic regression model with interaction term (Education x Disability)
model_interact <- glm(
  LONGCOVD_clean ~ collapsed_EDU * DISAB3_A + HISTOPCOST_A + SEX_A + AGEP_A,
  data = filtered_data,
  family = binomial
)

# Step 3: Clean and tidy model output for plotting odds ratios
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

# Step 4: Plot Odds Ratios with 95% Confidence Intervals
plot <- ggplot(model_tidy, aes(x = estimate, y = fct_rev(term), color = group)) +
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

print(plot)

# Step 5: Check model terms if needed
alias(model_interact)$Complete

# Optional: Inspect frequency distribution for education levels
table(data$EDUCP_A)
