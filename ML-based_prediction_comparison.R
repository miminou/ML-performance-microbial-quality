######### 
# Naloufi Manel 
# version :  V3.5.1
#########

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
