######### 
# Naloufi Manel 
# version :  V3.5.1
#########


######################################## Load the data #########################################
# define the path
setwd("/home/manel/Bureau/BD/donnee+pluvio/smv-complet/ML/replicat") # chemin donnee brute

# creating the list containing the csv files
ldf_1 <- list.files(pattern="csv") 
myList <- list()

# loop to read the files
for (k in 1:length(ldf_1)){
  myList[[k]]<-read.csv(ldf_1[k])
} 
names(myList)<-ldf_1 # renaming files  
################################################################################################

################################### Analyse of predicted data ##################################
#Group all 10 training sets together  
rf_rep<-myList[[1]][,]
for (k in 2:length(ldf_1)){
  r<-myList[[k]][,]
  rf_rep<-rbind(rf_rep,r)
}
rf_rep 

# Plot visualization of prediction results by the RF-based model 
x=rep(1:12,1)
plot(log(rf_rep$RF_Ecoli_pred),log(rf_rep$Ecolireal), xlab="Log(E.coli)",ylab="Log(predicted E.coli)", add=TRUE, xlim=c(3,13), ylim=c(3,13))
lines(x,x, col="red")
plot(rf_rep$RF_Ecoli_pred,rf_rep$Ecolireal, xlab="Log(E.coli)",ylab="Log(predicted E.coli)", add=TRUE)
lines(x,x, col="red")
rf_par<-list()
################################################################################################

################################### Analyse of predicted data ##################################
# Division into 2 sub-lists, the reasonnable estimates and the inaccurate estimates 
rf_fit<-myList[[1]][which(myList[[k]][,16]=="fit"),1:15]
rf_non<-myList[[1]][which(myList[[k]][,16]=="non"),1:15]

for (k in 2:length(ldf_1)){
  r1<-myList[[k]][which(myList[[k]][,16]=="fit"),1:15]
  r2<-myList[[k]][which(myList[[k]][,16]=="non"),1:15]
  rf_fit<-rbind(rf_fit,r1)
  rf_non<-rbind(rf_non,r2)
}

### Define the function not in 
# Identify elements present in one list but not in the other 
`%notin%` <- #create a new binary pipe operator
  function (x, table)
    is.na(match(x, table, nomatch = NA_integer_))
################################################################################################

################################### identification of values ###################################
# identification of the values of the parameters of the observations that present a reasonable estimate that does not give an inaccurate estimate 
rf<-list()

for (i in 1: 12){
  list_rf_fit<-unique(rf_fit[,i])
  l2<-list_rf_fit[][list_rf_fit%notin%rf_non[,i]]
  rf[[i]]<-list()
  rf[[i]]<-sort(l2)
}
rf

# export the list and the tables
capture.output(rf, file = "my_list-parametre.txt") 
write.csv(rf_fit, file = "/home/manel/Bureau/BD/donnee+pluvio/smv-complet/ML/replicat/test_fit.csv")
write.csv(rf_non, file = "/home/manel/Bureau/BD/donnee+pluvio/smv-complet/ML/replicat/test_non_fit.csv")
################################################################################################

################################### Visualisation of values ####################################
# Value range identifications
smv <- read.csv("~/Bureau/BD/donnee+pluvio/smv-complet/donnee_brute_sans_na/smv.csv")
summary(smv)

# Visualization by histograms of the values of each parameter to be optimized which gives a reasonable estimate 
par(mfrow=c(2,2))
hist(rf[[3]], nclass = 1000, xlim = c(17.6, 26.6),main="",xlab = "Temperature", xaxt="n")
axis(1, 17.6:26.6)
hist(rf[[4]], nclass = 1000, xlim = c(430, 657), main="",xlab = "Conductivity", xaxt="n")
axis(1, 430:657)
hist(rf[[11]], nclass = 1000, xlim = c(0, 35.4), main="",xlab = "rainfall of the day before", xaxt="n")
axis(1, 0:35.4)
hist(rf[[12]], nclass = 1000, xlim = c(4, 101), main="",xlab = "Flow", xaxt="n")
axis(1, 4:101)
################################################################################################

################################### additional analysis  #######################################
# Identification of all values in our dataset 
tt_par<-list()
for (i in 1: 12){
  list_tt<-unique(smv[,i])
  tt_par[[i]]<-list()
  tt_par[[i]]<-sort(list_tt)
}
tt_par

# Identification of all values in our testset 
for (i in 1: 12){
  list_rf_test<-unique(rf_rep[,i])
  rf_par[[i]]<-list()
  rf_par[[i]]<-sort(list_rf_test)
}
rf_par
capture.output(rf_par, file = "my_list-parametre-test.txt") 

# Identification of all values which does not give a reasonable estimate
nn_estimer<-list()
list_valeurpossible<-list()
list_valeurpossible[[3]]<-list()
list_valeurpossible[[3]]<-seq(17.6, 26.5, by=0.01)
list_valeurpossible[[4]]<-list()
list_valeurpossible[[4]]<-seq(430, 657, by=1)
list_valeurpossible[[5]]<-list()
list_valeurpossible[[5]]<-seq(0.12, 132, by=0.01)
list_valeurpossible[[6]]<-list()
list_valeurpossible[[6]]<-seq(0.9, 190, by=0.01)
list_valeurpossible[[7]]<-list()
list_valeurpossible[[7]]<-seq(0.03, 1.11, by=0.01)
list_valeurpossible[[8]]<-list()
list_valeurpossible[[8]]<-seq(0.15, 33.70, by=0.01)
list_valeurpossible[[9]]<-list()
list_valeurpossible[[9]]<-seq(0, 27, by=1)
list_valeurpossible[[10]]<-list()
list_valeurpossible[[10]]<-seq(0, 26, by=0.1)
list_valeurpossible[[11]]<-list()
list_valeurpossible[[11]]<-seq(0, 35.4, by=0.1)
list_valeurpossible[[12]]<-list()
list_valeurpossible[[12]]<-seq(4, 101, by=0.01)
for (i in 3: 12){
  list_rf_fit<-unique(rf_fit[,i])
  l1<-list_rf_fit[list_rf_fit%notin%rf_non[,i]]
  l3<-list_valeurpossible[i]
  l2<-l3[][[1]][l3[][[1]]%notin%l1]
  nn_estimer[[i]]<-list()
  nn_estimer[[i]]<-l2
}
nn_estimer
capture.output(nn_estimer, file = "my_list-parametre-non-fit.txt") 

