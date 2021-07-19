######### 
# Naloufi Manel 
# version :  V3.5.1
#########

# Visualization of the data distribution of our dataset 
smv <- read.csv("~/Bureau/BD/donnee+pluvio/smv-complet/donnee_brute_sans_na/smv.csv")
par(mfrow=c(2,5))
for (i in 5: length(smv)){
  boxplot(smv[,i], col="grey", ylab=colnames(smv)[i])
}
boxplot(log(smv$Ecoli), col="grey", ylab="Log (E.coli)")


# Plot visualization of the performance results (RMSE, MAE and RPD) of the machine learning models
par(mfrow=c(1,3))
plot_performance6 <- read_excel("plot-performance6.xlsx", sheet = "rmse", 
                                col_types = c("numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric"))
boxplot(plot_performance6[,2:7], main="RMSE", col="red")

plot_performance6 <- read_excel("plot-performance6.xlsx", sheet = "mae", 
                                col_types = c("numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric"))
boxplot(plot_performance6[,2:7], main="MAE", col="green")

plot_performance6 <- read_excel("plot-performance6.xlsx", sheet = "rpd", 
                                col_types = c("numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric"))
boxplot(plot_performance6[,2:7], main="RPD", col="blue")
