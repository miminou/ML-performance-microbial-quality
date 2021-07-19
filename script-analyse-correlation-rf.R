######### 
# Naloufi Manel 
# version :  V3.5.1
######### 

######################################## Load the data #########################################
# define the path
setwd("/home/manel/Bureau/BD/donnee+pluvio/smv-complet/ML/replicat")

# creating the list containing the csv files
ldf_1 <- list.files(pattern="csv") 
myList <- list()

# loop to read the files 
for (k in 1:length(ldf_1)){
  myList[[k]]<-read.csv(ldf_1[k])
} 

names(myList)<-ldf_1 # renaming files  
################################################################################################


##################################### Analyse the data #########################################
######################## identification of the number of reasonnable and inaccurate prediction 
x<-sample(1, (length(ldf_1)), replace = TRUE)
tab<-cbind(x,x,x,x,x)

## The table contain : { the replicate number (replicat), the number of reasonable estimates (nombre-fit), 
# the number of inaccurate estimates (nombre-non-fit), the percentage of reasonable estimate (pourc-fit),
#the percentage of inaccurate estimates (pourc-non-fit)}
colnames(tab)<-c("replicat","nombre-fit","nombre-non-fit","pourc-fit","pourc-non-fit")
for (k in 1:length(ldf_1)){
  tab[k,1]<-names(myList[k])
  tab[k,2]<-length(which(myList[[k]]$Fit.50..=="fit"))
  tab[k,3]<-length(which(myList[[k]]$Fit.50..=="non"))
  tab[k,4]<-round((as.integer(tab[k,2])*100)/170, digits = 2)
  tab[k,5]<-round((as.integer(tab[k,3])*100)/170, digits = 2)
  }
tab

# export the table 
write.csv(tab, file = "/home/manel/Bureau/BD/donnee+pluvio/smv-complet/ML/replicat/calcule-nb-bien-predit.csv")
################################################################################################


################# Verification of the normality of the E.coli concentration ####################
# Creation of the table that will contain: 
# 1(the data have a normal distribution) and 0: the data do not have a normal distribution 
x<-sample(1, (length(myList)), replace = TRUE)
tab_pval<-cbind(x,x,x)
tabf_pval<-cbind(x,x,x)
tabn_pval<-cbind(x,x,x)

# For each analysis : 
# column 2 shows the analysis for the measured E.coli concentration and column 3 for the E.coli concentration predicted by the RF- based model 

# Analysis for the dataset 
colnames(tab_pval)<-c("replicat","ec-reel","ec-rf")
for (k in 1:length(ldf_1)){
  ### Analysis of the p-value of the shapiro test 
  tab_pval[k,1]<-names(myList[k])
  if( shapiro.test(myList[[k]][,13])$p.value > 0.05) {tab_pval[k,2]<-1}
  else { tab_pval[k,2]<-0}
  if( shapiro.test(myList[[k]][,14])$p.value > 0.05) {tab_pval[k,3]<-1}
  else { tab_pval[k,3]<-0}
  
}
tab_pval

# Analysis for the reasonable prediction
colnames(tabf_pval)<-c("replicat","ec-reel","ec-rf")
for (k in 1:length(ldf_1)){
  ### Analysis of the p-value of the shapiro test 
  rf_fit<-myList[[k]][which(myList[[k]][,16]=="fit"),1:15]
  tabf_pval[k,1]<-names(myList[k])
  if( shapiro.test(rf_fit[,13])$p.value > 0.05) {tabf_pval[k,2]<-1}
  else { tabf_pval[k,2]<-0}
  if( shapiro.test(rf_fit[,14])$p.value > 0.05) {tabf_pval[k,3]<-1}
  else { tabf_pval[k,3]<-0}
  
}
tabf_pval

# Analysis for the innacute prediction
colnames(tabn_pval)<-c("replicat","ec-reel","ec-rf")
for (k in 1:length(ldf_1)){
  ### Analysis of the p-value of the shapiro test 
  rf_non<-myList[[k]][which(myList[[k]][,16]=="non"),1:15]
  tabn_pval[k,1]<-names(myList[k])
  if( shapiro.test(rf_non[,13])$p.value > 0.05) {tabn_pval[k,2]<-1}
  else { tabn_pval[k,2]<-0}
  if( shapiro.test(rf_non[,14])$p.value > 0.05) {tabn_pval[k,3]<-1}
  else { tabn_pval[k,3]<-0}
  
}
tabn_pval

### The data do not have a normal distribution 
# Thus the correlation analyses will be performed by the spearman method. 
################################################################################################

################################### Correlation analysis #######################################
# Creation of the table that will contain the correlations  
x<-sample(1, (2*length(myList)), replace = TRUE)
tab_cor<-cbind(x,x,x,x,x,x,x,x,x,x,x)

# For each replicat (seed), a correlation analysis is performed between the predicted E.coli concentration and the water parameters for the reasonable and innacute prediction
seed<-c("seed0","seed1","seed2","seed3","seed4","seed5","seed6","seed7","seed8","seed9")
colnames(tab_cor)<-c("donnee","Temperature","Conductivity","Turbidity","MES","NH4","NTK","Number of dry days","Rainfall of the day","Rainfall of the day before","Flow")
i=0
for (k in 1:length(ldf_1)){
  # Analyse for the reasonable estimation
  tab_cor[k+i,1]<-paste(seed[i+1],"-reasonable")
  rf_fit<-myList[[k]][which(myList[[k]][,16]=="fit"),1:15]
  tab_cor[k+i,2]<-cor(rf_fit[,14],rf_fit[,3],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,3]<-cor(rf_fit[,14],rf_fit[,4],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,4]<-cor(rf_fit[,14],rf_fit[,5],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,5]<-cor(rf_fit[,14],rf_fit[,6],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,6]<-cor(rf_fit[,14],rf_fit[,7],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,7]<-cor(rf_fit[,14],rf_fit[,8],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,8]<-cor(rf_fit[,14],rf_fit[,9],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,9]<-cor(rf_fit[,14],rf_fit[,10],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,10]<-cor(rf_fit[,14],rf_fit[,11],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,11]<-cor(rf_fit[,14],rf_fit[,12],  method = "spearman", use = "na.or.complete")
  i=i+1
  
  # Analyse for the innacurate estimation
  tab_cor[k+i,1]<-paste(seed[i],"-inaccurate")
  rf_non<-myList[[k]][which(myList[[k]][,16]=="non"),1:15]
  tab_cor[k+i,2]<-cor(rf_non[,14],rf_non[,3],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,3]<-cor(rf_non[,14],rf_non[,4],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,4]<-cor(rf_non[,14],rf_non[,5],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,5]<-cor(rf_non[,14],rf_non[,6],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,6]<-cor(rf_non[,14],rf_non[,7],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,7]<-cor(rf_non[,14],rf_non[,8],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,8]<-cor(rf_non[,14],rf_non[,9],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,9]<-cor(rf_non[,14],rf_non[,10],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,10]<-cor(rf_non[,14],rf_non[,11],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,11]<-cor(rf_non[,14],rf_non[,12],  method = "spearman", use = "na.or.complete")
  
}
################################################################################################

################################ Visualization of correlations  ################################
# transforming values into numeric 
newtab<-tab_cor
t <- as.data.frame(newtab[,-1])
for (i in 1: ncol(t)){
  t[,i]<-as.character(t[,i])
  t[,i]<-as.numeric(t[,i])
}

# Creation of the matrix
mat<-as.matrix(t)
rownames(mat)<-newtab[,1]

# Visualization by a corrplot 
library(corrplot)
corrplot(mat)
corrplot(mat, method = "number")

## # Division into 2 sub-tables, one with the correlation for reasonable estimates and one for inaccurate estimates 
j=1
k=1
new1<-newtab[1:10,]
new2<-newtab[1:10,]
for(i in 1:nrow(mat)){
  if (i%%2==0){
    new1[j,]<-newtab[i,]
    j<-j+1
  }
  else {
    new2[k,]<-newtab[i,]
    k<-k+1
  }
}

# Transforming values into numbers for the 2 tables and export data
t1 <- as.data.frame(new1[,-1])
for (i in 1: ncol(t1)){
  t1[,i]<-as.character(t1[,i])
  t1[,i]<-as.numeric(t1[,i])
}
write.csv(t1, file = "/home/manel/Bureau/BD/donnee+pluvio/smv-complet/ML/replicat/tableau_correlationt-non.csv")

t2 <- as.data.frame(new2[,-1])
for (i in 1: ncol(t1)){
  t2[,i]<-as.character(t2[,i])
  t2[,i]<-as.numeric(t2[,i])
}
write.csv(t2, file = "/home/manel/Bureau/BD/donnee+pluvio/smv-complet/ML/replicat/tableau_correlationt-fit.csv")

# Creation of the matrix 
mat1<-as.matrix(t1)
mat2<-as.matrix(t2)
rownames(mat1)<-new1[,1]
rownames(mat2)<-new2[,1]

# Visualization by a corrplot 
corrplot(mat1, method="pie")
corrplot(mat2, method="pie")
################################################################################################

########################################### Statistical analysis ###############################
# Test de significativite entre les correlation 

# Analysis of normalization
x<-sample(0, length(t1), replace = TRUE)
tab_pval<-cbind(x,x,x)
for (k in 1:length(t1)){
  
  if( shapiro.test(t1[,k])$p.value > 0.05) {tab_pval[k,2]<-1}
  else { tab_pval[k,2]<-0}
  if( shapiro.test(t1[,k])$p.value > 0.05) {tab_pval[k,3]<-1}
  else { tab_pval[k,3]<-0}
  
}
tab_pval
### The data have a normal distribution 

# Correlation difference test 
x<-sample(0, length(t1), replace = TRUE)
tab_pval<-cbind(x,x)
for (k in 1:length(t1)){
  tab_pval[k,1]<-colnames(t1)[k]
  tt<-t.test(t1[,k],t2[,k])$p.value
  tab_pval[k,2]<-tt
}
tab_pval

