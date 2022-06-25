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

pairs( ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + PAY_6 + BILL_AMT6 + PAY_AMT6, data=podaci, main="Scatterplot matrica")

korelacija = cor(podaci, method=c("pearson", "kendall", "spearman"))
require(corrplot)
corrplot(korelacija, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45) 

#uglavnom vidimo korelacije izmedju BILL_AMTx i BILL_AMTx-n, isto vazi i za PAY_AMTx i PAY_AMTx-n

#za podelu na trening i test koristicemo caTools alat
require(caret)
#sample = sample.split(podaci$default.payment.next.month, SplitRatio=.75) # 75% naseg dataseta bice za treniranje, ostatak za test
#train = subset(podaci, sample==TRUE)
#test = subset(podaci, sample==FALSE)
#folds = createFolds(y=podaci$default.payment.next.month, k=10, list=FALSE)
#train = podaci[folds,]
#test = podaci[-folds,]


#train_scaled = scale(train)
require(randomForest)
#train$default.payment.next.month = as.factor(train$default.payment.next.month) # neophodno je da ova kolona bude faktor da bi randomForest radio klasifikaciju, a ne regresiju
#random_forest_classifier = randomForest( default.payment.next.month ~ ., data=train, proximity=TRUE, importance=TRUE)

#predictions = predict(random_forest_classifier, newdata = test[-24])

#confusionMatrix(predictions, as.factor(test$default.payment.next.month))

model_training = function(modelMethod, preProcOptions=list(thresh = 0.95, ICAcomp = 3, k = 5, freqCut = 95/5, uniqueCut =
                                                             10, cutoff = 0.9), controlMethod="cv", number=10, repeats=3, grid="none") {
  set.seed(42) # setujemo seed radi reproducibilnosti splitovanja
  control = trainControl(method=controlMethod, number=number, repeats=repeats)
  if(grid == "none"){
    return(train(as.factor(default.payment.next.month)~., data=podaci, method=modelMethod, trControl=control, preProcOptions=preProcOptions, allowParallel=TRUE))
  }else{
    return(train(as.factor(default.payment.next.month)~., data=podaci, method=modelMethod, trControl=control, tuneGrid=grid, preProcOptions=preProcOptions))
  }
}

model = model_training(modelMethod = "rf", controlMethod = "cv") # rf randomForest najobicniji
model