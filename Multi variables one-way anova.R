#Start and file choose
getwd()
setwd ("/Users/~")

data1<-read.csv(file.choose())

data1$MZs<-as.factor(data1$MZs)

#Make a empty data frame
results <- data.frame(F_stat = numeric(),
                      DF1 = numeric(),
                      DF2 = numeric(),
                      p_value = numeric(),
                      stringsAsFactors = FALSE)

#"for loop" for the multivariable one-way anova
for (i in 2:20) {
    model <- aov(data1[,i] ~ data1$MZs, data=data1)
    anova_result <- anova(model)
    df1 <- anova_result$Df[1]
    df2 <- anova_result$Df[2]
    f_stat <- anova_result$`F value`[1]
    p_value <- anova_result$`Pr(>F)`[1]
    results <- rbind(results, data.frame(F_Stat = f_stat,
                                         DF1 = df1,
                                         DF2 = df2,
                                         p_value = p_value))
    
}

rownames(results) <- colnames(data1[2:20])
  
print(results)

write.csv(results, file = "anova_results.csv")

#Tukey test
tukey_results <- list()

for (i in 2:20) {
  model2 <- aov(data1[,i] ~ data1$MZs, data=data1)
  tukey_result <- TukeyHSD(model2)
  tukey_results[[i]] <- tukey_result$`data1$MZs`
}

print(tukey_results)

tukey_output <- data.frame()

for (i in 2:20) {
  tukey_df <- as.data.frame(tukey_results[[i]])
  tukey_df$Variable <- rownames(tukey_df)
  tukey_output <- rbind(tukey_output, tukey_df)
}

write.csv(tukey_output, file = "tukey_results.csv", row.names = FALSE)
