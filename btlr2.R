# --- Tải thư viện cần thiết ---
# install.packages("dplyr")     # Nếu chưa có
# install.packages("ggplot2")   # Nếu chưa có
# install.packages("tidyr")     # Nếu chưa có
# install.packages("corrplot")  # Nếu chưa có

library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)

# --- Tạo lại dữ liệu giả lập (thay bằng lệnh đọc dữ liệu thực tế của bạn) ---
# Kiểm tra cấu trúc dữ liệu lại lần nữa
str(data)

# --- Phần 1: Thống kê mô tả tổng quát ---

print("--- Thống kê mô tả tổng quát ---")
summary(data)

# --- Phần 2: Thống kê mô tả chi tiết ---

# --- Phần 2: Thống kê mô tả chi tiết ---

# 2.1. Biến định lượng (Age, Protein1, Protein2, Protein3, Protein4)
print("--- Thống kê mô tả chi tiết cho biến định lượng ---")
numeric_vars <- data %>% select(Age, Protein1, Protein2, Protein3, Protein4)

# Sửa lỗi ở đây: đặt na.rm = TRUE bên trong mỗi hàm cần nó
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

# In kết quả đã sửa
print(desc_stats_numeric)

# 2.2. Biến định tính (Tumour_Stage, Surgery_type, Patient_Status)
print("--- Bảng tần số cho biến định tính ---")
print("Tần số Patient_Status:")
print(table(data$Patient_Status))

print("Tần số Tumour_Stage:")
print(table(data$Tumour_Stage))

print("Tần số Surgery_type:")
print(table(data$Surgery_type))


# --- Phần 3: Thống kê mô tả theo biến kết quả (Patient_Status) ---

print("--- Thống kê mô tả theo Patient_Status ---")

# 3.1. Biến định lượng theo Patient_Status
print("--- Biến định lượng theo Patient_Status ---")
desc_stats_by_status <- data %>%
  group_by(Patient_Status) %>%
  summarise(across(c(Age, Protein1, Protein2, Protein3, Protein4),
                   list(mean = mean, sd = sd, median = median),
                   na.rm = TRUE))
print(desc_stats_by_status)

# 3.2. Biến định tính theo Patient_Status (Bảng chéo)
print("--- Bảng chéo biến định tính và Patient_Status ---")
print("Bảng chéo Tumour_Stage vs Patient_Status:")
print(table(data$Tumour_Stage, data$Patient_Status))

print("Bảng chéo Surgery_type vs Patient_Status:")
print(table(data$Surgery_type, data$Patient_Status))


# --- Phần 4: Trực quan hóa dữ liệu ---

print("--- Vẽ biểu đồ ---")

# 4.1. Phân phối của biến kết quả (Patient_Status)
plot_status_dist <- ggplot(data, aes(x = Patient_Status, fill = Patient_Status)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Patient Status Distribution", x = "Patient Status", y = "Count") +
  theme_minimal()
print(plot_status_dist)

# 4.2. Phân phối của các biến định tính khác
plot_tumour_dist <- ggplot(data, aes(x = Tumour_Stage, fill = Tumour_Stage)) +
  geom_bar() + labs(title = "Tumour Stage Distribution", x = "Period", y = "Count") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot_tumour_dist)

plot_surgery_dist <- ggplot(data, aes(x = Surgery_type, fill = Surgery_type)) +
  geom_bar() + labs(title = "Surgery Type Distribution", x = "Type of Surgery", y = "Count") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot_surgery_dist)


# 4.3. Mối quan hệ giữa Biến định tính và Patient_Status
# (Biểu đồ cột chồng tỉ lệ)
plot_tumour_status <- ggplot(data, aes(x = Tumour_Stage, fill = Patient_Status)) +
  geom_bar(position = "fill") + # "fill" cho tỉ lệ, "dodge" cho cột kề nhau
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Proportion of Patient Status by Tumour Stage", x = "Period", y = "Proportion", fill = "Status") +
  theme_minimal()
print(plot_tumour_status)

plot_surgery_status <- ggplot(data, aes(x = Surgery_type, fill = Patient_Status)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Proportion of Patient Status by Surgery Type", x = "Type of Surgery", y = "Proportion", fill = "Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot_surgery_status)


# 4.4. Mối quan hệ giữa Biến định lượng và Patient_Status

# Biểu đồ Boxplot (Rất quan trọng cho logistic regression)
# Sử dụng pivot_longer để vẽ nhiều boxplot cùng lúc
data_long_numeric <- data %>%
  select(Age, Protein1, Protein2, Protein3, Protein4, Patient_Status) %>%
  pivot_longer(cols = -Patient_Status, names_to = "variable", values_to = "value")

plot_boxplots <- ggplot(data_long_numeric, aes(x = Patient_Status, y = value, fill = Patient_Status)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") + # Chia thành nhiều biểu đồ nhỏ, trục y tự do
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Distribution of Numeric Variables by Patient Status", x = "Patient Status", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot_boxplots)

# Biểu đồ Histogram hoặc Density (Chồng lớp hoặc chia nhỏ)
plot_density <- ggplot(data_long_numeric, aes(x = value, fill = Patient_Status, color = Patient_Status)) +
  geom_density(alpha = 0.5) + # alpha để nhìn xuyên qua lớp chồng
  facet_wrap(~ variable, scales = "free") +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Density Distribution of Numeric Variables by Patient Status", x = "Value", y = "Density") +
  theme_minimal()
print(plot_density)


# 4.5. Mối quan hệ giữa các Biến định lượng (Kiểm tra đa cộng tuyến)

# Ma trận biểu đồ phân tán (Scatter Plot Matrix)
# pairs(numeric_vars, pch = 19, upper.panel = NULL) # Cách đơn giản

# Biểu đồ ma trận tương quan (Correlation Matrix Plot)
cor_matrix <- cor(numeric_vars, use = "complete.obs") # Tính ma trận tương quan
print("Ma trận tương quan giữa các biến định lượng:")
print(round(cor_matrix, 2))

plot_corr <- corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
                      addCoef.col = "black", # Thêm hệ số tương quan
                      tl.col = "black", tl.srt = 45, # Chỉnh màu và góc chữ
                      diag = FALSE) # Bỏ đường chéo
print("Đã vẽ biểu đồ ma trận tương quan.")