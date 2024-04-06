########### Task 1 - Analysis of a single patient ###########

#1

single_patient <- read.csv("single_patient.csv")[,1]

#2

plot(1:length(single_patient), single_patient, type = "l", ylab = "Voltage (mV)", xlab = "Time (ms)", main = "ECG Signal Plot", xlim = c(300,3795))

#3

single_patient_trimmed <- single_patient[which(single_patient != 0)]

#4

npeaks <- 9

#5,6,7,8

find_peaks <- function(single_patient_trimmed, npeaks) {
  interval_length <- length(single_patient_trimmed) / npeaks
  single_patient_matrix <- matrix(single_patient_trimmed, nrow = npeaks, byrow = TRUE)
  for (i in 1:npeaks) {
    peaks[i] <- which.max(single_patient_matrix[i,]) + (i-1)*interval_length
  }
  return(peaks)
}

#9

peaks <- find_peaks(single_patient_trimmed, npeaks)

#10

plot(1:length(single_patient_trimmed), single_patient_trimmed, type = "l", ylab = "Voltage (mV)", xlab = "Time (ms)", main = "ECG Signal Plot with Peaks")
points(peaks, single_patient_trimmed[peaks], col = "blue", pch = 19)

#11

for (i in 1:length(peaks)) {
  text(peaks[i], single_patient_trimmed[peaks[i]], labels = round(single_patient_trimmed[peaks[i]],2), pos = 2, cex = 0.8, col = "blue")
}

