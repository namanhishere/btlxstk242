library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)

# --- Recreate simulated data (replace with your command to read actual data) ---
# Assume 'data' is your data frame loaded previously in the prediction script

# Check the data structure again
str(data)


print("--- General Descriptive Statistics ---")
summary(data)

# 2.1. Quantitative Variables (Age, Protein1, Protein2, Protein3, Protein4)
print("--- Detailed Descriptive Statistics for Quantitative Variables ---")
numeric_vars <- data %>% select(Age, Protein1, Protein2, Protein3, Protein4)


desc_stats_numeric <- numeric_vars %>%
  summarise(across(everything(),
                   list(mean   = ~mean(., na.rm = TRUE),       # na.rm bên trong
                        sd     = ~sd(., na.rm = TRUE),         # na.rm bên trong
                        median = ~median(., na.rm = TRUE),     # na.rm bên trong
                        min    = ~min(., na.rm = TRUE),        # na.rm bên trong
                        max    = ~max(., na.rm = TRUE),        # na.rm bên trong
                        Q1     = ~quantile(., 0.25, na.rm = TRUE), # na.rm bên trong quantile
                        Q3     = ~quantile(., 0.75, na.rm = TRUE)  # na.rm bên trong quantile
                   )
                   # Bỏ na.rm = TRUE ở đây
  )) %>%
  pivot_longer(everything(), names_to = c("variable", ".value"), names_sep = "_")

# Print the corrected results
print(desc_stats_numeric)

# 2.2. Qualitative Variables (Tumour_Stage, Surgery_type, Patient_Status)
print("--- Frequency Tables for Qualitative Variables ---")
print("Frequency of Patient_Status:")
print(table(data$Patient_Status))

# Pie chart for Patient_Status
patient_status_freq <- table(data$Patient_Status)
pie(patient_status_freq, 
    main = "Pie Chart of Patient Status", 
    col = rainbow(length(patient_status_freq)))

print("Frequency of Tumour_Stage:")
print(table(data$Tumour_Stage))

# Pie chart for Tumour_Stage
tumour_stage_freq <- table(data$Tumour_Stage)
pie(tumour_stage_freq, 
    main = "Pie Chart of Tumour Stage", 
    col = rainbow(length(tumour_stage_freq)))

print("Frequency of Surgery_type:")
print(table(data$Surgery_type))

# Pie chart for Surgery_type
surgery_type_freq <- table(data$Surgery_type)
pie(surgery_type_freq, 
    main = "Pie Chart of Surgery Type", 
    col = rainbow(length(surgery_type_freq)))








# 3.1. Quantitative Variables by Patient_Status
desc_stats_by_status <- data %>%
  group_by(Patient_Status) %>%
  summarise(across(c(Age, Protein1, Protein2, Protein3, Protein4),
                   list(mean = mean, sd = sd, median = median),
                   na.rm = TRUE)) 
print(desc_stats_by_status)

# 3.2. Qualitative Variables by Patient_Status (Cross Tabulation)
print("Cross Tabulation Tumour_Stage vs Patient_Status:")
print(table(data$Tumour_Stage, data$Patient_Status))

print("Cross Tabulation Surgery_type vs Patient_Status:")
print(table(data$Surgery_type, data$Patient_Status))


# --- Part 4: Data Visualization ---

# 4.1. Distribution of the Outcome Variable (Patient_Status)
plot_status_dist <- ggplot(data, aes(x = Patient_Status, fill = Patient_Status)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Patient Status Distribution", x = "Patient Status", y = "Count") +
  theme_minimal()
print(plot_status_dist)

# 4.2. Distribution of other Qualitative Variables
plot_tumour_dist <- ggplot(data, aes(x = Tumour_Stage, fill = Tumour_Stage)) +
  geom_bar() + labs(title = "Tumour Stage Distribution", x = "Tumour Stage", y = "Count") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot_tumour_dist)

plot_surgery_dist <- ggplot(data, aes(x = Surgery_type, fill = Surgery_type)) +
  geom_bar() + labs(title = "Surgery Type Distribution", x = "Surgery Type", y = "Count") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot_surgery_dist)


# 4.3. Relationship between Qualitative Variables and Patient_Status
# (Stacked Bar Chart - Proportions)
plot_tumour_status <- ggplot(data, aes(x = Tumour_Stage, fill = Patient_Status)) +
  geom_bar(position = "fill") + # "fill" for proportion, "dodge" for side-by-side bars
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Proportion of Patient Status by Tumour Stage", x = "Tumour Stage", y = "Proportion", fill = "Patient Status") +
  theme_minimal()
print(plot_tumour_status)

plot_surgery_status <- ggplot(data, aes(x = Surgery_type, fill = Patient_Status)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Proportion of Patient Status by Surgery Type", x = "Surgery Type", y = "Proportion", fill = "Patient Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot_surgery_status)


# 4.4. Relationship between Quantitative Variables and Patient_Status

# Boxplot
data_long_numeric <- data %>%
  select(Age, Protein1, Protein2, Protein3, Protein4, Patient_Status) %>%
  pivot_longer(cols = -Patient_Status, names_to = "variable", values_to = "value")

plot_boxplots <- ggplot(data_long_numeric, aes(x = Patient_Status, y = value, fill = Patient_Status)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") + 
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Distribution of Numeric Variables by Patient Status", x = "Patient Status", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot_boxplots)

# Histogram or Density Plots
plot_density <- ggplot(data_long_numeric, aes(x = value, fill = Patient_Status, color = Patient_Status)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ variable, scales = "free") +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Density Distribution of Numeric Variables by Patient Status", x = "Value", y = "Density") +
  theme_minimal()
print(plot_density)


# 4.5. Relationship between Quantitative Variables (Check for Multicollinearity)

# Correlation Matrix Plot
cor_matrix <- cor(numeric_vars, use = "complete.obs") 
print("Correlation matrix between quantitative variables:")
print(round(cor_matrix, 2))


corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         addCoef.col = "black",
         tl.col = "black", tl.srt = 45,
         diag = FALSE) 
