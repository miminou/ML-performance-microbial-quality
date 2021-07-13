# definir le chemin 
setwd("/home/manel/Bureau/BD/donnee+pluvio/smv-complet/ML/replicat") # chemin donnee brute

# creation de la liste 
ldf_1 <- list.files(pattern="csv") 
myList <- list()

# boucle pour lire les tableau
for (k in 1:length(ldf_1)){
  myList[[k]]<-read.csv(ldf_1[k])
} 
names(myList)<-ldf_1

######################## identification du nombre de prediction 
x<-sample(1, (length(ldf_1)), replace = TRUE)
tab<-cbind(x,x,x,x,x)
colnames(tab)<-c("replicat","nombre-fit","nombre-non-fit","pourc-fit","pourc-non-fit")
for (k in 1:length(ldf_1)){
  tab[k,1]<-names(myList[k])
  tab[k,2]<-length(which(myList[[k]]$Fit.50..=="fit"))
  tab[k,3]<-length(which(myList[[k]]$Fit.50..=="non"))
  tab[k,4]<-round((as.integer(tab[k,2])*100)/170, digits = 2)
  tab[k,5]<-round((as.integer(tab[k,3])*100)/170, digits = 2)
  }
tab

write.csv(tab, file = "/home/manel/Bureau/BD/donnee+pluvio/smv-complet/ML/replicat/calcule-nb-bien-predit.csv")



######################## Verification de la normaliter des donnée e coli et EI 
# creation du tableau qui va contenir : 
## 1 nrml, 0 non norml 
x<-sample(1, (length(myList)), replace = TRUE)
tab_pval<-cbind(x,x,x)
colnames(tab_pval)<-c("replicat","ec-reel","ec-rf")
for (k in 1:length(ldf_1)){
  # creation des tableau
  myList[[k]]
  
  ### tableau des p value
  tab_pval[k,1]<-names(myList[k])
  if( shapiro.test(myList[[k]][,13])$p.value > 0.05) {tab_pval[k,2]<-1}
  else { tab_pval[k,2]<-0}
  if( shapiro.test(myList[[k]][,14])$p.value > 0.05) {tab_pval[k,3]<-1}
  else { tab_pval[k,3]<-0}
  
}
tab_pval
### Les donnÃ©es n'ont pas une distribution nrml
################################

################################### correlation de spearman pour chaque annee 
# creation du tableau qui va contenir les correlation 
x<-sample(1, (2*length(myList)), replace = TRUE)
tab_cor<-cbind(x,x,x,x,x,x,x,x,x,x,x)
seed<-c("seed0","seed1","seed2","seed3","seed4","seed5","seed6","seed7","seed8","seed9")
colnames(tab_cor)<-c("donnee","Temperature","Conductivity","Turbidity","MES","NH4","NTK","Number of dry days","Rainfall of the day","Rainfall of the day before","Flow")
i=0
for (k in 1:length(ldf_1)){

  tab_cor[k+i,1]<-paste(seed[i+1],"-reasonable")
  rf_fit<-myList[[k]][which(myList[[k]][,16]=="fit"),1:15]
  #tab_cor[k+i,2]<-cor(rf_fit[,14],rf_fit[,1],  method = "spearman", use = "na.or.complete")
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
  
  tab_cor[k+i,1]<-paste(seed[i],"-inaccurate")
  rf_non<-myList[[k]][which(myList[[k]][,16]=="non"),1:15]
  #tab_cor[k+i,2]<-cor(rf_non[,14],rf_non[,1],  method = "spearman", use = "na.or.complete")
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


newtab<-tab_cor
# transformer les variables en numerique 
t <- as.data.frame(newtab[,-1])
for (i in 1: ncol(t)){
  t[,i]<-as.character(t[,i])
  t[,i]<-as.numeric(t[,i])
}

# creation de la matrice 
mat<-as.matrix(t)
rownames(mat)<-newtab[,1]
heatmap(mat)

library(heatmap3)
heatmap3(mat)

library(corrplot)
corrplot(mat)
corrplot(mat, method = "number")

new1<-newtab[1:10,]
new2<-newtab[1:10,]

j=1
k=1
for(i in 1:nrow(mat)){
  if (i%%2==0){
    new1[j,]<-newtab[i,]
    #rownames(mat1[j,])<-rownames(mat[i,])
    j<-j+1
  }
  else {
    new2[k,]<-newtab[i,]
    #rownames(mat2[k,])<-rownames(mat[i,])
    k<-k+1
  }
}

# creation de la matrice 
# transformer les variables en numerique 
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

mat1<-as.matrix(t1)
mat2<-as.matrix(t2)
rownames(mat1)<-new1[,1]
rownames(mat2)<-new2[,1]
heatmap(mat1)
heatmap(mat2)
corrplot(mat1, method="pie")
corrplot(mat2, method="pie")

#####################################" test de significativite entre les correlation 
x<-sample(0, length(t1), replace = TRUE)
tab_pval<-cbind(x,x,x)
#colnames(tab_pval)<-c("replicat","ec-reel","ec-rf")
for (k in 1:length(t1)){
  
  if( shapiro.test(t1[,k])$p.value > 0.05) {tab_pval[k,2]<-1}
  else { tab_pval[k,2]<-0}
  if( shapiro.test(t1[,k])$p.value > 0.05) {tab_pval[k,3]<-1}
  else { tab_pval[k,3]<-0}
  
}
tab_pval
#donne nrml faire un test t 
x<-sample(0, length(t1), replace = TRUE)
tab_pval<-cbind(x,x)
#colnames(tab_pval)<-c("replicat","ec-reel","ec-rf")
for (k in 1:length(t1)){
  
  tab_pval[k,1]<-colnames(t1)[k]
  tt<-t.test(t1[,k],t2[,k])$p.value
  tab_pval[k,2]<-tt
  
}
tab_pval

## anova : 
#create data
tab_pval[k,1]<-colnames(t1)[k]
df <- data.frame(cor=rbind(cbind(t1$Temperature),cbind(t2$Temperature)),
                 replicat=c(rep(c('1', '2', '3','4','5','6','7','8','9','10','1', '2', '3','4','5','6','7','8','9','10'))),
                 clas=c(rep(c('inacurate','reasonable'), each=10)))

#view first six rows of data
head(df)

nest <- aov(df$cor ~ df$replicat + factor(df$clas)+ (df$replicat * factor(df$clas)))
tab_pval[k,2]<-summary(nest)

