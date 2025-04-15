#load the data
dataRaw <- read.csv("C:/Users/namanhishere/Downloads/BRCA.csv")

rows_with_na <- which(apply(dataRaw, 1, function(x) any(is.na(x) | x == "")))
length(rows_with_na)/nrow(dataRaw)

colSums(is.na(dataRaw) | dataRaw == "")


#remove the row which empty Patient_Status
data <- dataRaw[dataRaw$Patient_Status != "", ] 

#choose data
data <- data[c("Age","Protein1","Protein2","Protein3","Protein4","Tumour_Stage","Surgery_type","Patient_Status")]



summary(data)

data$Patient_Status <- factor(data$Patient_Status, levels = c("Alive", "Dead"))







library(caTools)

set.seed(123)
split <- sample.split(7)
train_set <- subset(data, split == TRUE)
test_set <- subset(data, split == FALSE)


logistic_model <- glm(Patient_Status ~ Age + Protein1 + Protein2 + Protein3 + Protein4,
                       data = train_set, family = "binomial")
print("Tóm tắt mô hình hồi quy logistic:")
summary(logistic_model)


#set.seed(69)
#split <- initial_split(data, prop = 0.8, strata = y)

# --- Bước 5: Kiểm tra mô hình ---
print("Tóm tắt mô hình hồi quy logistic:")
summary(logistic_model)

# Giải thích tóm tắt:
# - Coefficients: Ước lượng (Estimate), Sai số chuẩn (Std. Error), z value, và p-value (Pr(>|z|)).
#   - Estimate: Cho biết log-odds thay đổi như thế nào khi biến độc lập tăng 1 đơn vị (giữ các biến khác không đổi).
#   - Pr(>|z|): p-value cho biết ý nghĩa thống kê của biến. Giá trị nhỏ (thường < 0.05) cho thấy biến có ảnh hưởng đáng kể đến việc dự đoán Patient_Status.
# - AIC (Akaike Information Criterion): Chỉ số để so sánh các mô hình khác nhau (mô hình có AIC thấp hơn thường tốt hơn).

# --- Bước 6: Dự đoán trên tập kiểm tra ---
# Dự đoán xác suất bệnh nhân ở trạng thái "Dead" (vì "Dead" là level thứ 2 của factor)
probabilities <- predict(logistic_model, newdata = test_set, type = "response")

# Chuyển đổi xác suất thành lớp dự đoán (Alive/Dead) dựa trên ngưỡng (ví dụ: 0.5)
predicted_status <- ifelse(probabilities > 0.5, "Dead", "Alive")
predicted_status <- factor(predicted_status, levels = c("Alive", "Dead")) # Đảm bảo là factor với đúng levels

# --- Bước 7: Đánh giá mô hình ---
# Tạo ma trận nhầm lẫn (Confusion Matrix)
confusion_matrix <- table(Actual = test_set$Patient_Status, Predicted = predicted_status)
print("Ma trận nhầm lẫn:")
print(confusion_matrix)

# Tính toán các chỉ số đánh giá cơ bản
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
# Sensitivity (True Positive Rate - Tỷ lệ dự đoán đúng "Dead" trên tổng số "Dead" thực tế)
# Lưu ý: Kiểm tra xem "Dead" là positive class trong ma trận của bạn (thường là hàng/cột thứ 2)
sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
# Specificity (True Negative Rate - Tỷ lệ dự đoán đúng "Alive" trên tổng số "Alive" thực tế)
specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])

print(paste("Accuracy:", round(accuracy, 4)))
print(paste("Sensitivity (Recall for 'Dead'):", round(sensitivity, 4)))
print(paste("Specificity (Recall for 'Alive'):", round(specificity, 4)))

# Vẽ đường cong ROC và tính AUC (Area Under Curve)
# install.packages("pROC") # Chạy nếu chưa cài
library(pROC)

# Cần chuyển đổi lớp thực tế thành số (0/1) cho hàm roc
actual_numeric <- ifelse(test_set$Patient_Status == "Dead", 1, 0)
roc_curve <- roc(response = actual_numeric, predictor = probabilities)

print(paste("AUC (Area Under Curve):", round(auc(roc_curve), 4)))

# Vẽ đồ thị ROC
plot(roc_curve, main = "Đường cong ROC", print.auc = TRUE, col = "blue")


# 5.1 tìm khoảng tin cậy

# vẽ qqplot
qqnorm(data$Age, main = "Normal Q-Q Plot of Age")
qqline(data$Age)

shapiro.test(data$Age)

# tìm khoảng tin cậy
n <- length(data$Age)
xtb = mean(data$Age)
s = sd(data$Age)
T_alpha = qt(p = .05/2, df = n - 1, lower.tail = FALSE)
Epsilon = T_alpha * s / sqrt(n)
Left_CI = xtb - Epsilon
Right_CI = xtb + Epsilon
print(data.frame(n, xtb, s, T_alpha, Left_CI, Right_CI))
