library(corrplot)
source("https://github.com/KaroRonty/ShillerGoyalDataRetriever/raw/master/ShillerGoyalDataRetriever.r")

current_cape <- 31.89

# Return calculation for the next i years
returns <- as.data.frame(matrix(nrow = 1774))
for(i in 1:20){
  temp <- (lead(full_data$index, 12 * i) / full_data$index) ^ (1 / i)
  returns <- cbind(returns, temp)
  temp <- NA
  colnames(returns)[i + 1] <- paste0("ret_", i)
}
returns <- returns %>% select(-V1)  

# CAPE calculation for the next i years
capes <- as.data.frame(matrix(nrow = 1774))
temp2 <- NA
for(i in 1:20){
  for(j in 1:I(nrow(full_data) - 12)){
    temp2[j + i * 12] <- full_data$Price[j + i * 12] / mean(full_data$Earnings[j:I(j + i * 12 - 1)])
  }
  temp2 <- temp2[1:1774]
  capes <- cbind(capes, temp2)
  temp2 <- NA
  colnames(capes)[i + 1] <- paste0("cape_", i)
}
capes <- capes %>% select(-V1)  

# Calculate correlations and save into csv for formatting
correlations_manual <- as.data.frame(matrix(nrow = 20, ncol = 20))
for(i in 1:20){
  for(j in 1:20){
    correlations_manual[i, j] <- cor(capes[, i], returns[, j], use = "complete.obs")
  }
}
write.csv(correlations_manual, "cape_correlations.csv")

# Combine the most accurate data for modeling and model
model_data <- as.data.frame(cbind(capes[, 10], returns[, 12]))
colnames(model_data) <- c("cape_10", "returns_12")
lm <- lm(returns_12 ~ cape_10, model_data)

current_cape <- as.data.frame(current_cape)
colnames(current_cape) <- "cape_10"

# Print the confidence and prediction intervals
predict(lm, newdata = current_cape, interval = "confidence") - 1
predict(lm, newdata = current_cape, interval = "predict") - 1