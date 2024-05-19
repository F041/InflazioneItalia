library(ppcor)
library(googlesheets4)
library(zoo)
library(gam)
library(PerformanceAnalytics)
library(lmtest)
library(mctest)
library(car)
library(sandwich)
library(caret)
library(vetiver)
library(pins)

Sys.setenv(PINS_CACHE_DIR = base::tempdir())
board <- board_folder("~/Downloads/")
board_cache_path(board)

link<-"https://docs.google.com/spreadsheets/d/1iUGFBhbx8Zrj-bvrn0kRrwnCCWzpF7Ya_OcHnR6jkXQ/edit#gid=440387417"
data= read_sheet(link,skip =0, sheet=1) #attenzione a numero colonne su righe
hist(data$InflazioneNIC)
hist(log(data$InflazioneNIC+1))


chart.Correlation(data[,2:10], method = "spearman") 
pcor(data[1:75,c(2:10)], method = "spearman") 

acf(data[1:75,c(2:10)]) #lag negativi, nosense
pacf(data[1:75,c(2:10)])

#data[1:15,c(2,7:10)]<-scale(data[1:15,c(2,7:10)])

#creating indices
data<-data[1:75,]
trainIndex <- createDataPartition(data$InflazioneNIC,p=0.75,list=FALSE)
train <- data[trainIndex,] #training data (75% of data)
test <- data[-trainIndex,] #testing data (25% of data)

### Modelli ----

lm1<-lm(InflazioneNIC~Carburanti+Salari, data=train)
summary(lm1) #rimuovere intercetta?
plot(lm1)

lm1<-lm(InflazioneNIC~Carburanti+Salari+0, data=train)
summary(lm1)
plot(lm1)


boxcoxreg1<-boxcox(lm1) 
title("Lambda")
lambda=boxcoxreg1$x[which.max(boxcoxreg1$y)]
lambda # suggerisce trasformata y -2
train$InflBC<-train$InflazioneNIC^lambda
hist(train$InflBC)
lmc<-lm(InflBC~Carburanti+Salari, data=train) 
summary(lmc) 

par(mfrow=c(2,2)) 
plot(lmc)
par(mfrow=c(1,1)) 

### GAM ----

gam1<-gam(InflazioneNIC~s(Carburanti)+s(Salari), data=train)
summary(gam1)
par(mfrow=c(2,2)) 
plot(gam1)
par(mfrow=c(1,1)) 
# interpretazione non immediata
#carburanti parabola, salari exp o log

lmg<-lm(InflBC~I(Carburanti)^3+log(Salari), data=train)
summary(lmg)
plot(lmg)
bptest(lmg)
coeftest(lmg, vcov=vcovHC(lmg))


###  Valori inf e modello ----
influencePlot(lmc,  main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
cooksd <- cooks.distance(lmc)
cooksda=data.frame(cooksd)
n_used=length(lmc$residuals)
n_used  
cutoff <- 4/(n_used-length(lmc$coefficients)-2)
cutoff
data<-train
Noinflu=data.frame(train[cooksd < cutoff, ])  # influential row numbers

hist(Noinflu$InflazioneNIC) 
plot(ecdf(Noinflu$InflazioneNIC)) 

lminf = lm(InflBC ~I(Carburanti)^2+log(Salari), data=Noinflu) 

summary(lminf) 
par(mfrow=c(2,2)) 
plot(lminf)
par(mfrow=c(1,1)) 
bptest(lminf) #ancora problemi 
coeftest(lminf, vcov=vcovHC(lminf))

### Test

test$lm = predict(lminf , test)
postResample(pred = test$lm, obs = test$InflazioneNIC)

### Vetiver ----

v <- vetiver_model(lminf, "inflazione_lm")
model_board <- board_folder(path = "pins-r", versioned = TRUE)
model_board |> vetiver_pin_write(v)
model_board |>
  vetiver::vetiver_write_plumber("inflazione_lm")
write_board_manifest(model_board)

vetiver_write_docker(v,plumber_file = "plumber.R",lockfile = "vetiver_renv.lock")
