library(AppliedPredictiveModeling)
require(corrplot)
require(caret)
require(randomForest)
require(doParallel)

data = read.csv("./UCI_Credit_Card.csv")
# class(Q) CHECK THE TYPE OF Q
# ILI TYPEOF()

podaci = subset(data, select = -c(0:1))
# DROPUJEMo ID kolonu, jer nam nije potrebna

funkcija_za_ispitivanje_ispravnosti <- function (podaci){
  kolone = c("SEX", 
             "EDUCATION", 
             "MARRIAGE", 
             "PAY_0",
             "PAY_2",
             "PAY_3",
             "PAY_4",
             "PAY_5",
             "PAY_6")
  for (kolona in kolone) {
    if (kolona == "SEX") {
      vrednosti = c(1, 2)
    } else if (kolona == "EDUCATION") {
      vrednosti = c(1, 2, 3, 4)
    }
    else if (kolona == "MARRIAGE") {
      vrednosti = c(1, 2, 3)
    } else{
      vrednosti = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    }
    writeLines(paste("Za kolonu ",kolona," dozvoljene vrednosti su "))
    print(vrednosti)
    writeLines(paste("A kolona ima sledece vrednosti "))
    print(unique(podaci[, kolona]))
    if(!FALSE %in% (podaci[, kolona] %in% vrednosti)){
      writeLines("Kolona je ispravna!\n")
    }else{
      writeLines("Kolona je neispravna!\n")
    }
  }
}

#ispitaj koja polja nemaju zadovoljen predefinisan opseg vrednosti
funkcija_za_ispitivanje_ispravnosti(podaci = podaci)

#vidimo da imamo neke vrednosti koje ne treba da se jave za odredjene kolone
#njih cemo izbaciti

#takodje vidimo da kolone PAY_x imaju pogresan raspored vrednosti u odnosu na ono 
#sto stoji u specifikaciji
for (idx in 1:nrow(podaci)) {
  podaci[idx, "PAY_0"] = podaci[idx, "PAY_0"]+1
  podaci[idx, "PAY_2"] = podaci[idx, "PAY_2"]+1
  podaci[idx, "PAY_3"] = podaci[idx, "PAY_3"]+1
  podaci[idx, "PAY_4"] = podaci[idx, "PAY_4"]+1
  podaci[idx, "PAY_5"] = podaci[idx, "PAY_5"]+1
  podaci[idx, "PAY_6"] = podaci[idx, "PAY_6"]+1
}

podaci = subset(podaci, 
                (
                  EDUCATION != 5 &
                    EDUCATION != 6 &
                    EDUCATION != 0 &
                    MARRIAGE != 0 &
                    PAY_0 != 0 &
                    PAY_2 != 0 &
                    PAY_3 != 0 &
                    PAY_4 != 0 &
                    PAY_5 != 0 &
                    PAY_6 != 0 
                 ))

print("Nakon ispravke podataka imamo sledece")
funkcija_za_ispitivanje_ispravnosti(podaci = podaci)

#sada su nam svi podaci sinhronizovani sa opisom podataka

#ispitajmo da li ima NA vrednosti
colSums(is.na(podaci)) 
#nemamo NA podataka, samim tim preskacemo brisanje missing vrednosti

# Ovde radimo pairplot
#transparentTheme(trans = .4)

corrplot(cor(podaci), type='lower', method='shade')
podaci$default.payment.next.month = factor(podaci$default.payment.next.month)
featurePlot( x=podaci[c('LIMIT_BAL','SEX','EDUCATION','MARRIAGE','AGE','PAY_6','BILL_AMT6','PAY_AMT6')], 
             y=podaci$default.payment.next.month, 
             plot="pairs",
             auto.key=list(columns=2)
             )

#uglavnom vidimo korelacije izmedju BILL_AMTx i BILL_AMTx-n, isto vazi i za PAY_AMTx i PAY_AMTx-n

what_to_keep_for_training = createDataPartition(podaci$default.payment.next.month, p = .75, list = FALSE)
training = podaci[what_to_keep_for_training,]
test = podaci[-what_to_keep_for_training,]

# oslobodimo memoriju
rm(data)
rm(podaci)
rm(what_to_keep_for_training)
gc() 

model_training =  function(modelMethod, preProcOptions=list(thresh = 0.9, k = 5, cutoff = 0.8), controlMethod="cv", number=10, repeats=5, tuneLength=1, classProbs=FALSE, savePredictions=FALSE) {
  set.seed(42) # setujemo seed radi reproducibilnosti splitovanja
  
  cl <- makePSOCKcluster(6) # koliko paralelnih vorkera zelimo
  
  registerDoParallel(cl)# paralelizuj workflow
  
  
  model <- tryCatch({
    if(controlMethod == "cv"){
      control = trainControl(method=controlMethod, number=number, preProcOptions=preProcOptions, allowParallel=TRUE, classProbs = classProbs, savePredictions = savePredictions, returnResamp = "all")
    }else{
      control = trainControl(method=controlMethod, number=number, repeats=repeats, preProcOptions=preProcOptions, allowParallel=TRUE, classProbs = classProbs, savePredictions = savePredictions)
    }
    
    return(train(
      y=as.factor(make.names(training$default.payment.next.month)), 
      x=training[, !(colnames(training) == "default.payment.next.month")], 
      data=training, method=modelMethod, 
      trControl=control, 
      preProcess=c("pca"), 
      tuneLenght=tuneLength
    ))
  
  }, 
  error=function(cond){
    message("Doslo je do greske\n")
    message(cond)
    return(NULL)
  }, 
  warning=function(cond){
    message(cond)
    return(NULL)
  }, 
  finally={
    stopCluster(cl) # ugasi paralelizaciju
  })
  
  return(model)
  
}

model_rf_cv = model_training(modelMethod = "rf")
#nakon treninga mozemo koristiti update da setujemo finalne vrednosti parametara bez da pokrecemo ceo proces treniranja opet
update(model_rf_cv, param = list(mtry=2))
#vrsimo predikciju azuriranog modela nad testnim skupom
model_rf_cv_test_predictions=predict(model_rf_cv, newdata = test[, -ncol(test)])
#prikazujemo konfuzionu matricu
confusionMatrix(as.factor(make.names(test$default.payment.next.month)), model_rf_cv_test_predictions)

trellis.par.set(caretTheme())
plot(model_rf_cv)  

make.names(training$default.payment.next.month)
model_svmPoly_cv = model_training(modelMethod = "svmPoly", savePredictions = TRUE ,tuneLength = 3)
update(model_svmPoly_cv, param = list(degree=2,scale=0.01,C=1))

model_svmPoly_cv_predictions = predict(model_svmPoly_cv, newdata=test[, -ncol(test)])
confusionMatrix(as.factor(make.names(test$default.payment.next.month)), model_svmPoly_cv_predictions)

trellis.par.set(caretTheme())
plot(model_svmPoly_cv) 