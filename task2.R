# 加载必要的库
library(dplyr)

# 读取数据
patients_trimmed <- read.csv("all_patients_trimmed.csv", stringsAsFactors = FALSE)
patient_metadata <- read.csv("patient_metadata.csv", stringsAsFactors = FALSE)

# 使用dplyr的mutate函数来创建新的性别数值列
patient_metadata <- patient_metadata %>%
  mutate(GenderNumeric = ifelse(gender == "M", 1, 0))

# 1. 测试性别之间异常ECG数量的显著性差异
# 创建列联表
table_normal_abnormal <- table(as.factor(patient_metadata$gender), as.factor(patient_metadata$normal))
# 计算相对比例
proportions <- prop.table(table_normal_abnormal, margin = 2)
# 使用Fisher精确检验
fisher.test(table_normal_abnormal)

# 2. 使用峰值进行ECG比较
# 计算峰值间隔
# 这里我们计算peak1到peak2，peak2到peak3，以及peak3到peak4的间隔
peak_differences <- data.frame(
  diff_p1_p2 = patient_metadata$peak2 - patient_metadata$peak1,
  diff_p2_p3 = patient_metadata$peak3 - patient_metadata$peak2,
  diff_p3_p4 = patient_metadata$peak4 - patient_metadata$peak3
)

# 根据normal列的值分为正常和异常两组
diff_normal <- peak_differences[patient_metadata$normal == 1, ]
diff_abnormal <- peak_differences[patient_metadata$normal == 0, ]

# 检查样本量是否足够进行t检验
if (nrow(diff_normal) >= 2 && nrow(diff_abnormal) >= 2) {
  # 比较两组的平均间隔差异
  t_test_diff_p1_p2 <- t.test(diff_normal$diff_p1_p2, diff_abnormal$diff_p1_p2, var.equal = TRUE)
  t_test_diff_p2_p3 <- t.test(diff_normal$diff_p2_p3, diff_abnormal$diff_p2_p3, var.equal = TRUE)
  t_test_diff_p3_p4 <- t.test(diff_normal$diff_p3_p4, diff_abnormal$diff_p3_p4, var.equal = TRUE)
  
  # 输出t检验结果
  print(paste("t-test for differences between normal and abnormal ECG intervals (p1-p2):"))
  print(t_test_diff_p1_p2)
  print(paste("t-test for differences between normal and abnormal ECG intervals (p2-p3):"))
  print(t_test_diff_p2_p3)
  print(paste("t-test for differences between normal and abnormal ECG intervals (p3-p4):"))
  print(t_test_diff_p3_p4)
} else {
  print("至少有一个分组的样本量太小，无法进行t检验。")
}

# 3. 关注峰值的实际值
# 提取峰值处的ECG信号值
# 假设patients_trimmed数据框的列代表不同患者的ECG数据，而行代表时间序列数据
# 我们将根据patient_metadata中的peak1, peak2, peak3, peak4列来提取对应的ECG信号值

# 创建一个空的数据框来存储所有患者的峰值ECG信号值
peak_values <- data.frame(
  PatientID = character(),
  Peak1 = numeric(),
  Peak2 = numeric(),
  Peak3 = numeric(),
  Peak4 = numeric()
)

# 遍历patient_metadata中的每个患者
for (i in 1:nrow(patient_metadata)) {
  # 提取每个患者的患者ID和峰值索引
  patient_id <- patient_metadata$patient[i]  
  peak1_index <- patient_metadata$peak1[i]
  peak2_index <- patient_metadata$peak2[i]  
  peak3_index <- patient_metadata$peak3[i]
  peak4_index <- patient_metadata$peak4[i]    
  # 确保索引是正数  
  if (all(peak1_index > 0, peak2_index > 0, peak3_index > 0, peak4_index > 0)) {    
    # 提取对应的ECG信号值    
    peak1_value <- patients_trimmed[patient_id, peak1_index]    
    peak2_value <- patients_trimmed[patient_id, peak2_index]    
    peak3_value <- patients_trimmed[patient_id, peak3_index]    
    peak4_value <- patients_trimmed[patient_id, peak4_index]        
    # 将提取的峰值添加到peak_values数据框中    
    peak_values <- rbind(peak_values, data.frame(      
      PatientID = patient_id,      
      Peak1 = peak1_value,      
      Peak2 = peak2_value,      
      Peak3 = peak3_value,      
      Peak4 = peak4_value    ))  }}

# 分类存储峰值
peaks_normal <- peak_values[patient_metadata$normal == 1, ]
peaks_abnormal <- peak_values[patient_metadata$normal == 0, ]

# 检查每个分组中的样本量
cat("正常ECG样本量:", nrow(peaks_normal), "\n")
cat("异常ECG样本量:", nrow(peaks_abnormal), "\n")

# 检查正常和异常ECG的第一个峰值是否有缺失值
cat("正常ECG第一个峰值的缺失值数量:", sum(is.na(peaks_normal$Peak1)), "\n")
cat("异常ECG第一个峰值的缺失值数量:", sum(is.na(peaks_abnormal$Peak1)), "\n")

# 如果存在缺失值，删除含有缺失值的行
peaks_normal_complete <- peaks_normal[!is.na(peaks_normal$Peak1), ]
peaks_abnormal_complete <- peaks_abnormal[!is.na(peaks_abnormal$Peak1), ]

# 再次检查样本量
cat("清洗后正常ECG样本量:", nrow(peaks_normal_complete), "\n")
cat("清洗后异常ECG样本量:", nrow(peaks_abnormal_complete), "\n")

# 如果每个分组中的样本量都足够，则进行t检验
if (nrow(peaks_normal_complete) >= 2 && nrow(peaks_abnormal_complete) >= 2) {
  # 比较R峰值的平均值，假设第一个峰值为R峰值
  t.test_result <- t.test(peaks_normal_complete$Peak1, peaks_abnormal_complete$Peak1, var.equal = TRUE)
  print(t.test_result)
} else {
  cat("至少有一个分组的样本量太小，无法进行t检验。请进一步检查数据。\n")
}