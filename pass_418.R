#Hypothesis:
  
# Null Hypothesis (H0): The effect of the new device in raising blood pressure is similar to the effect of the old device.
# Alternative Hypothesis (HA): The effect of the new device in raising blood pressure is different from the effect of the old device.

group1 <- c(126, 120, 150, 118, 139, 98, 122, 140)
group2 <- c(114, 146, 123, 126, 116, 133)

# Observed test statistic (difference in means)
obs_diff <- mean(group1) - mean(group2)

# Permutation Test
combined <- c(group1, group2)
perm_diff <- vector(length = 10000)

for (i in 1:10000) {
  perm_combined <- sample(combined)
  perm_group1 <- perm_combined[1:length(group1)]
  perm_group2 <- perm_combined[(length(group1)+1):length(perm_combined)]
  perm_diff[i] <- mean(perm_group1) - mean(perm_group2)
}

# Calculate p-value
p_value_perm <- sum(abs(perm_diff) >= abs(obs_diff)) / length(perm_diff)

# Bootstrap
bootstrap_diff <- vector(length = 10000)

for (i in 1:10000) {
  bootstrap_group1 <- sample(group1, replace = TRUE)
  bootstrap_group2 <- sample(group2, replace = TRUE)
  bootstrap_diff[i] <- mean(bootstrap_group1) - mean(bootstrap_group2)
}

# Calculate bootstrap confidence interval
ci_bootstrap <- quantile(bootstrap_diff, c(0.025, 0.975))

# Print results
cat("Permutation Test:\n")
cat("Observed Difference in Means:", obs_diff, "\n")
cat("P-value:", p_value_perm, "\n\n")

cat("Bootstrap:\n")
cat("Observed Difference in Means:", obs_diff, "\n")
cat("95% Confidence Interval:", ci_bootstrap, "\n")

# Plot bootstrap distribution
hist(bootstrap_diff, breaks = 30, col = "lightblue", main = "Bootstrap Distribution",
     xlab = "Difference in Means", xlim = c(-20, 20))
abline(v = obs_diff, col = "red", lwd = 2)


#In the results, the p-value from the permutation test indicates the likelihood of observing a difference 
#in means as extreme as the observed difference if the null hypothesis were true. The confidence interval 
#from the bootstrap method provides a range of plausible values for the difference in means. If the p-value 
#is below the chosen significance level (e.g., 0.05) or if the confidence interval does not contain zero, 
#we can reject the null hypothesis and conclude that the effects of the old and new devices are different.



#========================================
