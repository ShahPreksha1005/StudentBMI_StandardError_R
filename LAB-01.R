# Get and print current working directory.
print(getwd())

# Set current working directory.
setwd("D:/")

# Get and print current working directory.
print(getwd())
                         
#Reading a CSV File
df <- read.csv("Dataset.csv")
print(df)

#Analyzing the CSV File
print(is.data.frame(df))
print(ncol(df))
print(nrow(df))
summary(df)
names(df)
dframe = data.frame(df)
dframe
height = dframe$height
height
meanvalue = mean(height)
meanvalue 
stddev_val= sd(height)
stddev_val     
# Check the structure of your data frame
str(df)

# Point Estimation and Interval Estimation for Height and Weight:
mean_height <- mean(df$`height`)
mean_weight <- mean(df$`weight`)
ci_height <- t.test(df$`height`)$conf.int
ci_weight <- t.test(df$`weight`)$conf.int

cat("Point Estimation for Height:", mean_height, "\n")
cat("Point Estimation for Weight:", mean_weight, "\n\n")
cat("Confidence Interval for Height:", ci_height, "\n")
cat("Confidence Interval for Weight:", ci_weight, "\n\n")
# Probability of Height > 160:
prob_height_gt_160 <- 1 - pnorm(160, mean(df$height), sd(df$height))
cat("Probability of Height > 160:", prob_height_gt_160, "\n")

# Probability of Height > 160 and Less Than 190:
prob_height_gt_160_lt_190 <- pnorm(190, mean(df$height), sd(df$height)) - pnorm(160, mean(df$height), sd(df$height))
cat("Probability of Height > 160 and Less Than 190:", prob_height_gt_160_lt_190, "\n")

# Probability of Weight Between 50 to 90:
prob_weight_between_50_90 <- pnorm(90, mean(df$weight), sd(df$weight)) - pnorm(50, mean(df$weight), sd(df$weight))
cat("Probability of Weight Between 50 to 90:", prob_weight_between_50_90, "\n")

# Probability of Weight > 60:
prob_weight_gt_60 <- 1 - pnorm(60, mean(df$weight), sd(df$weight))
cat("Probability of Weight > 60:", prob_weight_gt_60, "\n")

# Probability of Weight > 60 and Less Than 90:
prob_weight_gt_60_lt_90 <- pnorm(90, mean(df$weight), sd(df$weight)) - pnorm(60, mean(df$weight), sd(df$weight))
cat("Probability of Weight > 60 and Less Than 90:", prob_weight_gt_60_lt_90, "\n")

# Probability of Weight Between 90 to 200:
prob_weight_between_90_200 <- pnorm(200, mean(df$weight), sd(df$weight)) - pnorm(90, mean(df$weight), sd(df$weight))
cat("Probability of Weight Between 90 to 200:", prob_weight_between_90_200, "\n")

# Draw Normal Distribution Diagram:
par(mfrow = c(1, 2))
hist(df$height, col = "purple", main = "Height Distribution", xlab = "Height")
hist(df$weight, col = "lightblue", main = "Weight Distribution", xlab = "Weight")

# Calculate Probability from Z table:
z_table_value_height_gt_160 <- (160 - mean(df$height)) / sd(df$height)
z_table_value_weight_gt_60 <- (60 - mean(df$weight)) / sd(df$weight)

cat("\nZ Table Value for Height > 160:", z_table_value_height_gt_160, "\n")
cat("Z Table Value for Weight > 60:", z_table_value_weight_gt_60, "\n")

# Test Observed and Calculated Results:
cat("\nObserved and Calculated Results Testing:\n")
cat("Height > 160 (Observed):", sum(df$height > 160) / length(df$height), "\n")
cat("Height > 160 (Calculated):", pnorm(160, mean(df$height), sd(df$height)), "\n")

cat("Weight > 60 (Observed):", sum(df$weight > 60) / length(df$weight), "\n")
cat("Weight > 60 (Calculated):", pnorm(60, mean(df$weight), sd(df$weight)), "\n")

# Draw Normal Distribution Graph for Height:
height_mean <- mean(df$height)
height_sd <- sd(df$height)
height_range <- seq(min(df$height), max(df$height), length.out = 1000)
height_density <- dnorm(height_range, mean = height_mean, sd = height_sd)

plot(height_range, height_density, type = "l", col = "blue", lwd = 2,
     main = "Normal Distribution - Height",
     xlab = "Height", ylab = "Density")

# Add a line for height = 160
abline(v = 160, col = "red", lty = 2)

# Add text with probability for height > 160
text(165, 0.03, paste("P(Height > 160) =", round(prob_height_gt_160, 4)), col = "red")

# Draw Normal Distribution Graph for Weight:
weight_mean <- mean(df$weight)
weight_sd <- sd(df$weight)
weight_range <- seq(min(df$weight), max(df$weight), length.out = 1000)
weight_density <- dnorm(weight_range, mean = weight_mean, sd = weight_sd)

plot(weight_range, weight_density, type = "l", col = "green", lwd = 2,
     main = "Normal Distribution - Weight",
     
     xlab = "Weight", ylab = "Density")

# Add a line for weight = 60
abline(v = 60, col = "orange", lty = 2)

# Add text with probability for weight > 60
text(70, 0.03, paste("P(Weight > 60) =", round(prob_weight_gt_60, 4)), col = "orange")

