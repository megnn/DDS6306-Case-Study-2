#df = data.frame(sens = "", spec = "", nrows = 1000)
sens = list()
spec = list()
for(j in seq(1:1000)){
  percentage = .8
  indices = sample(seq(1:dim(attritionFullTrain)[1]), round(dim(attritionFullTrain)[1]*percentage))
  attritionSplitTrain = attritionFullTrain[indices,]
  attritionSplitTest = attritionFullTrain[-indices,]
  
  stepCols = c("Age", "DistanceFromHome", "EnvironmentSatisfaction", "JobInvolvement", "JobSatisfaction",
               "NumCompaniesWorked", "RelationshipSatisfaction", "TotalWorkingYears", "TrainingTimesLastYear",
               "WorkLifeBalance","YearsInCurrentRole", "YearsSinceLastPromotion", "BusinessTravel", 
               "EducationField", "JobRole", "MaritalStatus","OverTime") 
  
  
  stepCols1 = c("Age", "EnvironmentSatisfaction", "JobInvolvement", "JobSatisfaction",
                "NumCompaniesWorked", "RelationshipSatisfaction", "TotalWorkingYears", 
                "WorkLifeBalance","YearsInCurrentRole", "YearsSinceLastPromotion", "BusinessTravel", 
                "JobRole", "MaritalStatus","OverTime", "logMonthlyIncome") 
  
  
  fitAttrition = naiveBayes(attritionSplitTrain[,stepCols1], as.factor(attritionSplitTrain$Attrition), laplace = 1, data = attritionSplitTrain) 
  attritionPredict = predict(fitAttrition, attritionSplitTest[,stepCols1])
  #confusionMatrix(table(attritionPredict, attritionSplitTest$Attrition ))
  
  #Balanced KNN Model
  
  trueYes = attritionFullTrain %>% filter(Attrition == "Yes")
  trueNo = attritionFullTrain %>% filter(Attrition == "No") 
  indicesNo = sample(seq(1:dim(trueNo)[1]), round(dim(trueYes)[1]))
  trueNoSample = trueNo[indicesNo,]
  balancedTotal = rbind(trueYes, trueNoSample) 
  
  
  percentage = .8
  indices = sample(seq(1:dim(balancedTotal)[1]), round(dim(balancedTotal)[1]*percentage))
  balancedSplitTrain = balancedTotal[indices,]
  balancedSplitTest = balancedTotal[-indices,]
  
  
  #step based cols w/out char
  knnColsStep = c("Age", "EnvironmentSatisfaction", "JobInvolvement", "JobSatisfaction",
                  "NumCompaniesWorked", "RelationshipSatisfaction", "TotalWorkingYears", 
                  "WorkLifeBalance","YearsInCurrentRole", "YearsSinceLastPromotion","logMonthlyIncome") 
  
  #Balanced KNN predict based on limited data set. 
  Balanceknnpredict = knn(balancedSplitTrain[,knnColsStep], attritionSplitTest[,knnColsStep], balancedSplitTrain$Attrition, k = 5)
  
  #confusionMatrix(table(Balanceknnpredict,attritionSplitTest$Attrition))
  
  
  
  ensembleDF = data.frame(NB = attritionPredict, KNN = Balanceknnpredict)
  for(i in seq(1:dim(ensembleDF)[1])){
    score = 0
    if(ensembleDF[i,1] == "Yes" | ensembleDF[i,2] == "Yes"){
      ensembleDF$ensemblePredictions[i] = "Yes"
    }
    
    else{ensembleDF$ensemblePredictions[i] = "No"}
    
  }
  
  table(ensembleDF$ensemblePredictions,attritionSplitTest$Attrition)
  cm = confusionMatrix(table(ensembleDF$ensemblePredictions,attritionSplitTest$Attrition))
  sens[j] = unname(cm$byClass[1], force = TRUE)
  #print(cm$byClass[1])
  spec[j] = cm$byClass[2]


}
hist(unlist(sens))
hist(unlist(spec))
