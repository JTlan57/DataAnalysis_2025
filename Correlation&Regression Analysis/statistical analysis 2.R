#--------------------start-------------------------------
# Get current working directory
getwd()
#----------------read dataset--------------------------
data <- read.csv("data_for_analysis.csv")
install.packages("drc")
install.packages("aomisc")
install.packages("devtools")
devtools::install_github("onofriAndreaPG/aomisc")
install.packages("wPerm")
library(drc)
library(aomisc)
library(wPerm)

summary(data)
# testing for normality of distribution
shapiro.test(data$lipids1)
shapiro.test(data$lipids2)

hist(data$lipids1)  
qqnorm(data$lipids1)

# Spearman's correlation test

spearman_result<-cor.test(data$lipids1, data$lipids2, method="spearman")

print(spearman_result)

# data.frame for result
results <- data.frame(
  variable = character(),
  spearman_corr = numeric(),
  s_p_value = numeric(),
  stringsAsFactors = FALSE
)

# variables for analysis
target_vars <- c("antioxidant2", "antioxidant3", "antioxidant4", 
                 "antioxidant5")

# main 
for (var in target_vars) {
  # spearman
  perm_spearman <- perm.relation(
    x = data$antioxidant1, 
    y = data[[var]],
    method = "spearman",
    R = 10000
  )
  
  # add result
  results <- rbind(results, data.frame(
    variable = var,
    spearman_corr = perm_spearman$Observed,
    s_p_value =  perm_spearman$p.value
    ))
}


# output result
print(results)

#------visualization of significant results of correlation analysis---------

data<-data[order(data$antioxidant1),]

plot(data$antioxidant1, data$antioxidant2)

lines(data$antioxidant1, data$antioxidant2, col = "blue")

abline(lm(data$antioxidant1 ~ data$antioxidant2), col="red")




#_____________regression analysis________________ 

df=data
df<-df[order(df$antioxidant1),]


#linear regression

model_linear <- lm(antioxidant1 ~ antioxidant2, data=df)
summary(model_linear)


#second degree polynomal

model_2 <- lm(antioxidant1 ~ poly(antioxidant2, 2), data=df)
summary(model_2)

#third degree polynomal

model_3 <- lm(antioxidant1 ~ poly(antioxidant2, 3), data=df)

summary(model_3)
#exponential dependence

model_exp <- lm(log(antioxidant1) ~ antioxidant2, data=df)
summary(model_exp)
# log dependence

model_log <- lm(exp(antioxidant1) ~ antioxidant2, data=df)
summary(model_log)
#comparison of models
#table of result

rezult<-data.frame(model=c("model_linear", "model_2", "model_3", "model_exp", "model_log"), BIC_value=c(BIC(model_linear), BIC(model_2), BIC(model_3), BIC(model_exp), BIC(model_log)))

rezult<-rezult[order(rezult$BIC_value),]

rezult


# __________building graphs______________
#         linear regression graphs

plot(df$antioxidant1, df$antioxidant2)




