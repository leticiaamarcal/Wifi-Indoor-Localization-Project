#------------------------------------------------------------------------------------ 
#Goal: Modelos
#Description: treinar modelos
#Developer: Letícia Marçal
#-------------------------------------------------------------------------------------

source("Scripts/Preprocess.R")

#libraries
library(utiml)
library(randomForest)

#Fazer 4 modelos- cada um para prever, respectivamente: Latitude(regression), 
#Longitude(reg), Floor(classification), Building(classif)

#Começamos pelo Building

#transformar building em factor
wifi_data$BUILDINGID <-  as.factor(wifi_data$BUILDINGID)

#set seed
set.seed(123)

#separar o dado em train e test
inTrain <- createDataPartition(y = wifi_data$BUILDINGID, p = .75, list = FALSE) 

#criar o train e o set
training <- wifi_data[ inTrain,]
testing <- wifi_data[-inTrain,]

#cross validation
crossV <- trainControl(method = 'repeatedcv', repeats = 2)

#set seed
set.seed(123)

#treinar modelo
DT_building <- train(BUILDINGID ~ .,
                     data = training %>% 
                     select(starts_with("WAP"), BUILDINGID),   
                     method = 'C5.0', 
                     preProc = c('center','scale'), 
                     tuneLength = 1, 
                     trControl = crossV)

# métricas
# model  winnow  trials  Accuracy   Kappa    
# rules  FALSE    1      0.9958118  0.9934150
# rules  FALSE   10      0.9981441  0.9970808
# rules   TRUE    1      0.9951849  0.9924261
# rules   TRUE   10      0.9961379  0.9939296
# tree   FALSE    1      0.9949842  0.9921140
# tree   FALSE   10      0.9978431  0.9966099
# tree    TRUE    1      0.9945829  0.9914800
# tree    TRUE   10      0.9960626  0.9938122 

#fazer prediction
DT_building_predic <- predict(DT_building, testing)

#postResample
postResample(testing$BUILDINGID, DT_building_predic)

# métricas
# Accuracy     Kappa 
# 0.9977929 0.9965276 


######################

#modelo para prever floor para o building 1

#transformar floor em factor
building0$FLOOR <- as.factor(building0$FLOOR)

#set seed
set.seed(123)

#separar o dado em train e test
inTrain_floor0 <- createDataPartition(y = building0$FLOOR, p = .75, list = FALSE) 

#criar o train e o set
training_floor0 <- building0[ inTrain_floor0,]
testing_floor0 <- building0[-inTrain_floor0,]

#set seed
set.seed(123)

#treinar modelo
DT_floor0 <- train(FLOOR ~ .,
                     data = training_floor0 %>% 
                     select(starts_with("WAP"), FLOOR),  
                     method = 'C5.0', 
                     preProc = c('center','scale'), 
                     tuneLength = 2, 
                     trControl = crossV)

# métricas
# model  winnow  trials  Accuracy   Kappa    
# rules  FALSE    1      0.9499897  0.9330288
# rules  FALSE   10      0.9865683  0.9820123
# rules   TRUE    1      0.9537964  0.9381309
# rules   TRUE   10      0.9858998  0.9811180
# tree   FALSE    1      0.9449385  0.9262636
# tree   FALSE   10      0.9839971  0.9785691
# tree    TRUE    1      0.9481759  0.9306155
# tree    TRUE   10      0.9811381  0.9747378


#fazer prediction
DT_floor0_predic <- predict(DT_floor0, testing_floor0)

#postResample
postResample(testing_floor0$FLOOR, DT_floor0_predic)

# metricas
# Accuracy    Kappa 
# 1        1 


##########
#modelo para longitude/ vou usar o building 1 para ter uma sample menor

#set seed
set.seed(123)

#treinar modelo
LR_longitude_b0 <- train(LONGITUDE ~ .,
                   data = training_floor0 %>%
                   select(starts_with("WAP"), LONGITUDE),  
                   method = 'lm', 
                   preProc = c('center','scale'), 
                   tuneLength = 2, 
                   trControl = crossV)

# metricas
# RMSE      Rsquared   MAE     
# 9.087237  0.8677395  7.020436

#fazer prediction
LR_longitude_b0_predic <- predict(LR_longitude_b0, testing_floor0)

#postResample
postResample(testing_floor0$LONGITUDE, LR_longitude_b0_predic)

# metricas
# RMSE  Rsquared       MAE 
# 9.0453358 0.8694893 7.0584956 

#######
#treinar Linear Regression para latitude prédio 0

#set seed
set.seed(123)

#treinar modelo
LR_latitude_b0 <- train(LATITUDE ~ .,
                        data = training_floor0 %>% 
                          select(starts_with("WAP"), LATITUDE),
                        method = 'lm',
                        preProc = c('center', 'scale'), 
                        tuneLength = 2, 
                        trControl = crossV)
                           
# metricas
# RMSE     Rsquared   MAE     
# 9.86693  0.9090417  7.670982


#prediction
LR_latitude_b0_predic <- predict(LR_latitude_b0, testing_floor0)

#postResample
postResample(testing_floor0$LATITUDE, LR_latitude_b0_predic)

# metricas
# RMSE  Rsquared       MAE 
# 9.6926251 0.9130794 7.4732094 

###################
# agora que tirei a variância perto de zero, vou train o modelo again
# para ver as métricas

#separar o dado em train e test
inTrain2 <- createDataPartition(y = building0_$LATITUDE, p = .75, list = FALSE) 

#criar o train e o set
training2 <- building0_[ inTrain2,]
testing2 <- building0_[-inTrain2,]

#cross validation
crossV <- trainControl(method = 'repeatedcv', repeats = 2)

#set seed
set.seed(123)

#treinar modelo
LR_latitude_b0_2 <- train(LATITUDE ~ .,
                        data = training2 %>% 
                         select(starts_with("WAP"), LATITUDE),
                        method = 'lm',
                        preProc = c('center', 'scale'), 
                        tuneLength = 2, 
                        trControl = crossV)

# metricas
# RMSE      Rsquared   MAE    
# 9.868478  0.9089665  7.67059

#prediction
LR_latitude_b0_predic2 <- predict(LR_latitude_b0_2, testing2)

#postResample
postResample(testing2$LATITUDE, LR_latitude_b0_predic2)

# metricas
# RMSE  Rsquared       MAE 
# 9.4617590 0.9156654 7.4001068

####
#transformei dBm em mw e 100 em 0. ver como ficam as métricas

#separar o dado em train e test
inTrain3 <- createDataPartition(y = building0_3$LATITUDE, p = .75, list = FALSE) 

#criar o train e o set
training3 <- building0_3[ inTrain3,]
testing3 <- building0_3[-inTrain3,]

#cross validation
crossV <- trainControl(method = 'repeatedcv', repeats = 2)

#set seed
set.seed(123)

#treinar modelo
LR_latitude_b0_3 <- train(LATITUDE ~ .,
                          data = training3 %>% 
                            select(starts_with("WAP"), LATITUDE),
                          method = 'lm',
                          preProc = c('center', 'scale'), 
                          tuneLength = 2, 
                          trControl = crossV)

# metricas
# RMSE      Rsquared   MAE     
# 27.63412  0.3278275  22.74936


#prediction
LR_latitude_b0_predic3 <- predict(LR_latitude_b0_3, testing3)

#postResample
postResample(testing3$LATITUDE, LR_latitude_b0_predic3)

# metricas
# RMSE   Rsquared        MAE 
# 30.9917372  0.2409022 23.3055285 

####
#transformei 100 em -105

#separar o dado em train e test
inTrain4 <- createDataPartition(y = building0_4$LATITUDE, p = .75, list = FALSE) 

#criar o train e o set
training4 <- building0_4[ inTrain4,]
testing4 <- building0_4[-inTrain4,]

#cross validation
crossV <- trainControl(method = 'repeatedcv', repeats = 2)

#set seed
set.seed(123)

#treinar modelo
LR_latitude_b0_4 <- train(LATITUDE ~ .,
                          data = training4 %>% 
                            select(starts_with("WAP"), LATITUDE),
                          method = 'lm',
                          preProc = c('center', 'scale'), 
                          tuneLength = 2, 
                          trControl = crossV)

# metricas
# RMSE      Rsquared   MAE     
# 7.913977  0.9413998  6.094137


#prediction
LR_latitude_b0_predic4 <- predict(LR_latitude_b0_4, testing4)

#postResample
postResample(testing4$LATITUDE, LR_latitude_b0_predic4)

# metricas
# RMSE  Rsquared       MAE 
# 7.8845178 0.9420612 6.1664396 

###
#agora vamos treinar latitude depois de eliminar os "outliers"
 
#separar o dado em train e test
inTrain5 <- createDataPartition(y = building0_5$LATITUDE, p = .75, list = FALSE) 

#criar o train e o set
training5 <- building0_5[ inTrain5,]
testing5 <- building0_5[-inTrain5,]

#cross validation
#crossV <- trainControl(method = 'repeatedcv', repeats = 2)

#set seed
set.seed(123)

#treinar modelo
LR_latitude_b0_5 <- train(LATITUDE ~ .,
                          data = training5 %>% 
                            select(starts_with("WAP"), LATITUDE),
                          method = 'lm',
                          preProc = c('center', 'scale'), 
                          tuneLength = 1, 
                          trControl = crossV)

# metricas
# RMSE      Rsquared   MAE     
# 7.976568  0.9403099  6.140259

#prediction
LR_latitude_b0_predic5 <- predict(LR_latitude_b0_5, testing5)

#postResample
postResample(testing5$LATITUDE, LR_latitude_b0_predic5)

# metricas
# RMSE  Rsquared       MAE 
# 7.7988836 0.9438796 5.9794374  

###
#vou treinar longitude depois de tirar os outliers

#Longitude 

#separar o dado em train e test
inTrain5_ <- createDataPartition(y = building0_5$LONGITUDE, p = .75, list = FALSE) 

#criar o train e o set
training5_ <- building0_5[ inTrain5_,]
testing5_ <- building0_5[-inTrain5_,]

#set seed
set.seed(123)

#treinar modelo
LR_longitude_b0_5 <- train(LONGITUDE ~ .,
                          data = training5_ %>% 
                            select(starts_with("WAP"), LONGITUDE),
                          method = 'lm',
                          preProc = c('center', 'scale'), 
                          tuneLength = 1, 
                          trControl = crossV)

# metricas
# RMSE      Rsquared   MAE     
# 7.984722  0.8975821  5.983043


#prediction
LR_longitude_b0_predic5 <- predict(LR_longitude_b0_5, testing5_)

#postResample
postResample(testing5_$LONGITUDE, LR_longitude_b0_predic5)

# metricas
# RMSE  Rsquared       MAE 
# 7.8348447 0.9021475 5.9372703 

###
#Sem duplicated ----
#Treinar longitude sem duplicated

#separar o dado em train e test
inTrain6_ <- createDataPartition(y = building0_6$LONGITUDE, p = .75, list = FALSE) 

#criar o train e o set
training6_ <- building0_6[ inTrain6_,]
testing6_ <- building0_6[-inTrain6_,]

#set seed
set.seed(123)

#treinar modelo
LR_longitude_b0_6 <- train(LONGITUDE ~ .,
                           data = training6_ %>% 
                           select(starts_with("WAP"), LONGITUDE),
                           method = 'lm',
                           preProc = c('center', 'scale'), 
                           tuneLength = 1, 
                           trControl = crossV)

# metricas
# RMSE      Rsquared   MAE     
# 8.749365  0.8784791  6.502109

#prediction
LR_longitude_b0_predic6 <- predict(LR_longitude_b0_6, testing6_)

#postResample
postResample(testing6_$LONGITUDE, LR_longitude_b0_predic6)

# metricas
# RMSE  Rsquared       MAE 
# 8.0285004 0.8930831 5.9587228 

###
#Latitude sem duplicados----

#separar o dado em train e test
inTrain6 <- createDataPartition(y = building0_6$LATITUDE, p = .75, list = FALSE) 

#criar o train e o set
training6 <- building0_6[ inTrain6,]
testing6 <- building0_6[-inTrain6,]

#cross validation
#crossV <- trainControl(method = 'repeatedcv', repeats = 2)

#set seed
set.seed(123)

#treinar modelo
LR_latitude_b0_6 <- train(LATITUDE ~ .,
                          data = training6 %>% 
                            select(starts_with("WAP"), LATITUDE),
                          method = 'lm',
                          preProc = c('center', 'scale'), 
                          tuneLength = 1, 
                          trControl = crossV)

# metricas
# RMSE      Rsquared   MAE     
# 8.173022  0.9376257  6.396017

#prediction
LR_latitude_b0_predic6 <- predict(LR_latitude_b0_6, testing6)

#postResample
postResample(testing6$LATITUDE, LR_latitude_b0_predic6)

# metricas
# RMSE  Rsquared       MAE 
# 8.1275320 0.9397796 6.3224246 

###
#Novo dataset com os waps que tem no validation. Vou treinar o dataset todo

#separar o dado em train e test
inTrain7 <- createDataPartition(y = wifi_data11$LATITUDE, p = .75, list = FALSE) 

#criar o train e o set
training7 <- wifi_data11[ inTrain7,]
testing7 <- wifi_data11[-inTrain7,]

#cross validation
crossV <- trainControl(method = 'repeatedcv', repeats = 2)

#set seed
set.seed(123)

#treinar modelo
LR_latitude_b0_7 <- train(LATITUDE ~ .,
                          data = training7 %>% 
                            select(starts_with("WAP"), LATITUDE),
                          method = 'lm',
                          preProc = c('center', 'scale'), 
                          tuneLength = 1, 
                          trControl = crossV)

# metricas
# RMSE      Rsquared   MAE     
# 18.13625  0.9230477  13.49017

#prediction
LR_latitude_b0_predic7 <- predict(LR_latitude_b0_7, testing7)

#postResample
postResample(testing7$LATITUDE, LR_latitude_b0_predic7)

# metricas
# RMSE   Rsquared        MAE 
# 17.8792059  0.9269793 13.2333815 

###
#aplicar meu modelo na validation e comparar com o resultado que já tem
predic_val1 <- predict(LR_latitude_b0_7, wifi_validation4) 

#comparando 
postResample(wifi_validation4$LATITUDE, predic_val1)

# metricas
# RMSE   Rsquared        MAE 
# 37.6587196  0.7651451 25.5448915 

###Modelo para building

#transformar building em factor
wifi_data11$BUILDINGID <-  as.factor(wifi_data11$BUILDINGID)

#set seed
set.seed(123)

#separar o dado em train e test
inTrain8 <- createDataPartition(y = wifi_data11$BUILDINGID, p = .75, list = FALSE) 

#criar o train e o set
training8 <- wifi_data11[ inTrain8,]
testing8 <- wifi_data11[-inTrain8,]

#cross validation
crossV <- trainControl(method = 'repeatedcv', repeats = 2)

#set seed
set.seed(123)

#treinar modelo
DT_building2 <- train(BUILDINGID ~ .,
                     data = training8 %>% 
                       select(starts_with("WAP"), BUILDINGID),   
                     method = 'C5.0', 
                     preProc = c('center','scale'), 
                     tuneLength = 1, 
                     trControl = crossV)

# métricas
# model  winnow  Accuracy   Kappa    
# rules  FALSE   0.9697387  0.9587079
# rules   TRUE   0.9937361  0.9899641
# tree   FALSE   0.9691516  0.9575598
# tree    TRUE   0.9929040  0.9886310

#fazer prediction
DT_building_predic2 <- predict(DT_building2, testing8)

#postResample
postResample(testing8$BUILDINGID, DT_building_predic2)

# métricas
# Accuracy     Kappa 
# 0.9938326 0.9901233 

###
#aplicar meu modelo na validation e comparar com o resultado que já tem
predic_val2 <- predict(DT_building2, wifi_validation4) 

#comparando 
postResample(wifi_validation4$BUILDINGID, predic_val2)

# metricas
# Accuracy     Kappa 
# 0.9864986 0.9786786 

##
###Modelo para building com knn

#set seed
set.seed(123)

#treinar modelo
knn_building <- train(BUILDINGID ~ .,
                      data = training8 %>% 
                        select(starts_with("WAP"), BUILDINGID),   
                      method = 'kknn', 
                      preProc = c('center','scale'), 
                      tuneLength = 1, 
                      trControl = crossV)

# metricas
# Accuracy   Kappa    
# 0.9954489  0.9927325

#fazer prediction
knn_building_predic <- predict(knn_building, testing8)

#postResample
postResample(testing8$BUILDINGID, knn_building_predic)

# métricas
# Accuracy     Kappa 
# 0.9947137 0.9915590 

###
#aplicar meu modelo na validation e comparar com o resultado que já tem
predic_val_knn_b <- predict(knn_building, wifi_validation4) 

#comparando 
postResample(wifi_validation4$BUILDINGID, predic_val_knn_b)

# metricas
# Accuracy     Kappa 
# 0.9873987 0.9801126 

###
#modelo para prever floor com knn

#transformar floor em factor
wifi_data11$FLOOR <- as.factor(wifi_data11$FLOOR)

#set seed
set.seed(123)

#separar o dado em train e test
inTrain9 <- createDataPartition(y = wifi_data11$FLOOR, p = .75, list = FALSE) 

#criar o train e o set
training9 <- wifi_data11[ inTrain9,]
testing9 <- wifi_data11[-inTrain9,]

#set seed
set.seed(123)

#treinar modelo
knn_floor <- train(FLOOR ~ .,
                   data = training9 %>% 
                     select(starts_with("WAP"), FLOOR),  
                   method = 'kknn', 
                   preProc = c('center','scale'), 
                   tuneLength = 2, 
                   trControl = crossV)

# metricas
# kmax  Accuracy   Kappa    
# 5     0.9852712  0.9805769
# 7     0.9842447  0.9792234

#fazer prediction
knn_floor_predic <- predict(knn_floor, testing9)

#postResample
postResample(testing9$FLOOR, knn_floor_predic)

# metricas
# Accuracy     Kappa 
# 0.9838425 0.9786885 

#aplicar meu modelo na validation e comparar com o resultado que já tem
knn_floor_predic_val <- predict(knn_floor, wifi_validation4) 

#comparando 
postResample(wifi_validation4$FLOOR, knn_floor_predic_val)

# metricas
# Accuracy     Kappa 
# 0.7920792 0.7117524  

###Treinar dataset todo, agora com knn para latitude. (já temos o train and test)

#set seed
set.seed(123)

#treinar modelo
knn_latitude <- train(LATITUDE ~ .,
                          data = training7 %>% 
                            select(starts_with("WAP"), LATITUDE),
                          method = 'kknn',
                          preProc = c('center', 'scale'), 
                          tuneLength = 1, 
                          trControl = crossV)

# metricas
# RMSE      Rsquared   MAE     
# 7.521851  0.9865994  2.313428

#prediction
knn_latitude_predic <- predict(knn_latitude, testing7)

#postResample
postResample(testing7$LATITUDE, knn_latitude_predic)

# metricas
# RMSE  Rsquared       MAE 
# 7.8171647 0.9859157 2.3738918 

#aplicar meu modelo na validation e comparar com o resultado que já tem
predic_val_lat <- predict(knn_latitude, wifi_validation4) 

#comparando 
postResample(wifi_validation4$LATITUDE, predic_val_lat)

# metricas
# RMSE   Rsquared        MAE 
# 14.9961076  0.9546553  7.3706100  

###Treinar longitude, todo dataset, com knn. depois do processo preprocess

#separar o dado em train e test
inTrain10 <- createDataPartition(y = wifi_data11$LONGITUDE, p = .75, list = FALSE) 

#criar o train e o set
training10 <- wifi_data11[ inTrain10,]
testing10 <- wifi_data11[-inTrain10,]

#set seed
set.seed(123)

#treinar modelo
knn_longitude <- train(LONGITUDE ~ .,
                           data = training10 %>% 
                             select(starts_with("WAP"), LONGITUDE),
                           method = 'kknn',
                           preProc = c('center', 'scale'), 
                           tuneLength = 1, 
                           trControl = crossV)

# metricas
# RMSE      Rsquared   MAE     
# 11.83269  0.9896071  2.837686

#prediction
knn_longitude_predic <- predict(knn_longitude, testing10)

#postResample
postResample(testing10$LONGITUDE, knn_longitude_predic)

# metricas
# RMSE   Rsquared        MAE 
# 18.7745672  0.9757283  3.1039861  

#aplicar meu modelo na validation e comparar com o resultado que já tem
predic_val_long <- predict(knn_longitude, wifi_validation4) 

#comparando 
postResample(wifi_validation4$LONGITUDE, predic_val_long)

# metricas
# RMSE   Rsquared        MAE 
# 20.3076430  0.9718677  8.0448605  

###Treinar building, todo dataset, knn. depois de rescale rows

#separar o dado em train e test
inTrain11 <- createDataPartition(y = wifi_data12$BUILDINGID, p = .75, list = FALSE) 

#criar o train e o set
training11 <- wifi_data12[ inTrain11,]
testing11 <- wifi_data12[-inTrain11,]

#set seed
set.seed(123)

#treinar modelo
knn_building2 <- train(BUILDINGID ~ .,
                      data = training11 %>% 
                        select(starts_with("WAP"), BUILDINGID),   
                      method = 'kknn', 
                      preProc = c('center','scale'), 
                      tuneLength = 1, 
                      trControl = crossV)

# metricas
# Accuracy   Kappa    
# 0.9952044  0.9923413

#fazer prediction
knn_building_predic2 <- predict(knn_building2, testing11)

#postResample
postResample(testing11$BUILDINGID, knn_building_predic2)

# métricas
# Accuracy     Kappa 
# 0.9967695 0.9948397 

###
#aplicar meu modelo na validation e comparar com o resultado que já tem
predic_val_knn_b2 <- predict(knn_building2, wifi_validation5) 

#comparando 
postResample(wifi_validation5$BUILDINGID, predic_val_knn_b2)

# metricas
# Accuracy     Kappa 
# 0.9990999 0.9985774 

# confusionMatrix(table(wifi_validation5$BUILDINGID, predic_val_knn_b2))
# 
# predic_val_knn_b2
# 0   1   2
# 0 536   0   0
# 1   0 306   1
# 2   0   0 268

###Treinar longitude, todo dataset, knn. depois de rescale rows

#separar o dado em train e test
inTrain12 <- createDataPartition(y = wifi_data12$LONGITUDE, p = .75, list = FALSE) 

#criar o train e o set
training12 <- wifi_data12[ inTrain12,]
testing12 <- wifi_data12[-inTrain12,]

#set seed
set.seed(123)

#treinar modelo
knn_longitude2 <- train(LONGITUDE ~ .,
                       data = training12 %>% 
                         select(starts_with("WAP"), LONGITUDE),
                       method = 'kknn',
                       preProc = c('center', 'scale'), 
                       tuneLength = 1, 
                       trControl = crossV)

# metricas
# RMSE      Rsquared   MAE     
# 10.31141  0.9919874  2.845192

#prediction
knn_longitude_predic2 <- predict(knn_longitude2, testing12)

#postResample
postResample(testing12$LONGITUDE, knn_longitude_predic2)

# metricas
# RMSE  Rsquared       MAE 
# 7.6760086 0.9958857 2.6402640 

#aplicar meu modelo na validation e comparar com o resultado que já tem
predic_val_long2 <- predict(knn_longitude2, wifi_validation5) 

#comparando 
postResample(wifi_validation5$LONGITUDE, predic_val_long2)

# metricas
# RMSE   Rsquared        MAE 
# 11.0043551  0.9916505  5.9733042   

###Treinar latitude, todo dataset, knn. depois de rescale rows

#separar o dado em train e test
inTrain13 <- createDataPartition(y = wifi_data12$LATITUDE, p = .75, list = FALSE) 

#criar o train e o set
training13 <- wifi_data12[ inTrain13,]
testing13 <- wifi_data12[-inTrain13,]

#set seed
set.seed(123)

#treinar modelo
knn_latitude2 <- train(LATITUDE ~ .,
                      data = training13 %>% 
                        select(starts_with("WAP"), LATITUDE),
                      method = 'kknn',
                      preProc = c('center', 'scale'), 
                      tuneLength = 1, 
                      trControl = crossV)

# metricas
# RMSE      Rsquared   MAE     
# 7.428611  0.9866964  2.355131

#prediction
knn_latitude_predic2 <- predict(knn_latitude2, testing13)

#postResample
postResample(testing13$LATITUDE, knn_latitude_predic2)

# metricas
# RMSE  Rsquared       MAE 
# 8.7726717 0.9822592 2.4137719 

#aplicar meu modelo na validation e comparar com o resultado que já tem
predic_val_lat2 <- predict(knn_latitude2, wifi_validation5) 

#comparando 
postResample(wifi_validation5$LATITUDE, predic_val_lat2)

# metricas
# RMSE  Rsquared       MAE 
# 9.1920822 0.9829311 5.4075202  

#modelo para prever floor com knn, depois de rescale rows

#transformar floor em factor
wifi_data12$FLOOR <- as.factor(wifi_data12$FLOOR)

#set seed
set.seed(123)

#separar o dado em train e test
inTrain14 <- createDataPartition(y = wifi_data12$FLOOR, p = .75, list = FALSE) 

#criar o train e o set
training14 <- wifi_data12[ inTrain14,]
testing14 <- wifi_data12[-inTrain14,]

#set seed
set.seed(123)

#treinar modelo
knn_floor2 <- train(FLOOR ~ .,
                   data = training14 %>% 
                     select(starts_with("WAP"), FLOOR),  
                   method = 'kknn', 
                   preProc = c('center','scale'), 
                   tuneLength = 2, 
                   trControl = crossV)

# metricas
# kmax  Accuracy   Kappa    
# 5     0.9900672  0.9869015
# 7     0.9894806  0.9861283

#fazer prediction
knn_floor_predic2 <- predict(knn_floor2, testing14)

#postResample
postResample(testing14$FLOOR, knn_floor_predic2)

# metricas
# Accuracy    Kappa 
# 0.989718 0.986439

#aplicar meu modelo na validation e comparar com o resultado que já tem
knn_floor_predic_val2 <- predict(knn_floor2, wifi_validation5) 

#comparando 
postResample(wifi_validation5$FLOOR, knn_floor_predic_val2)

# metricas
# Accuracy     Kappa 
# 0.8694869 0.8176293   

###
###Treinar building, todo dataset, knn. depois trocar os menores de -95 e rescale
#nao chegou a 100%

#separar o dado em train e test
inTrain15 <- createDataPartition(y = wifi_building$BUILDINGID, p = .75, list = FALSE) 

#criar o train e o set
training15 <- wifi_building[ inTrain15,]
testing15 <- wifi_building[-inTrain15,]

#set seed
set.seed(123)

#treinar modelo
knn_building3 <- train(factor(BUILDINGID) ~ .,
                       data = training15 %>% 
                         select(starts_with("WAP"), BUILDINGID),   
                       method = 'kknn', 
                       preProc = c('center','scale'), 
                       tuneLength = 1, 
                       trControl = crossV)

# metricas
# Accuracy   Kappa    
# 0.9975535  0.9960845

#fazer prediction
knn_building_predic3 <- predict(knn_building3, testing15)

#postResample
postResample(testing15$BUILDINGID, knn_building_predic3)

# metricas
# Accuracy   Kappa    
# 0.9975535  0.9960845

###
#aplicar meu modelo na validation e comparar com o resultado que já tem
predic_val_knn_b3 <- predict(knn_building3, wifi_validation5) 

#comparando 
postResample(wifi_validation5$BUILDINGID, predic_val_knn_b3)

# metricas
# Accuracy     Kappa 
# 0.9990999 0.9985774 

#confusionMatrix(table(wifi_validation5$BUILDINGID, predic_val_knn_b3))

# predic_val_knn_b2
# 0   1   2
# 0 536   0   0
# 1   0 306   1
# 2   0   0 268

###
#treinar building de novo, agora com SVM Radial

#set seed
set.seed(123)

#treinar modelo
knn_building4 <- train(factor(BUILDINGID) ~ .,
                       data = training15 %>% 
                         select(starts_with("WAP"), BUILDINGID),   
                       method = 'svmRadial', 
                       preProc = c('center','scale'), 
                       tuneLength = 1, 
                       trControl = crossV)

# metricas
# Accuracy   Kappa    
# 0.9959377  0.9935046

#fazer prediction
knn_building_predic4 <- predict(knn_building4, testing15)

#postResample
postResample(testing15$BUILDINGID, knn_building_predic4)

# metricas
# Accuracy     Kappa 
# 0.9979442 0.9967131 

###
#aplicar meu modelo na validation e comparar com o resultado que já tem
predic_val_knn_b4 <- predict(knn_building4, wifi_validation5) 

#comparando 
postResample(wifi_validation5$BUILDINGID, predic_val_knn_b4)

# metricas
# Accuracy     Kappa 
# 0.9981998 0.9971541 

confusionMatrix(table(wifi_validation5$BUILDINGID, predic_val_knn_b4))

# predic_val_knn_b2
# 0   1   2
# 0 536   0   0
# 1   1 305   1
# 2   0   0 268

###Treinar building, todo dataset, knn. depois trocar os menores de -95 e rescale
#nao chegou a 100%

#separar o dado em train e test
inTrain16 <- createDataPartition(y = wifi_data13$BUILDINGID, p = .75, list = FALSE) 

#criar o train e o set
training16 <- wifi_data13[ inTrain16,]
testing16 <- wifi_data13[-inTrain16,]

#set seed
set.seed(123)

#treinar modelo
knn_building5 <- train(factor(BUILDINGID) ~ .,
                       data = training16 %>% 
                         select(starts_with("WAP"), BUILDINGID),   
                       method = 'kknn', 
                       preProc = c('center','scale'), 
                       tuneLength = 1, 
                       trControl = crossV)

# metricas
# Accuracy   Kappa   
# 0.9995084  0.999214

#fazer prediction
knn_building_predic5 <- predict(knn_building5, testing16)

#postResample
postResample(testing16$BUILDINGID, knn_building_predic5)

# metricas
# Accuracy    Kappa 
# 1            1 

###
#aplicar meu modelo na validation e comparar com o resultado que já tem
predic_val_knn_b5 <- predict(knn_building5, wifi_validation6) 

#comparando 
postResample(wifi_validation6$BUILDINGID, predic_val_knn_b5)

# metricas
# Accuracy     Kappa 
# 0.9882988 0.9815475 

# confusionMatrix(table(wifi_validation5$BUILDINGID, predic_val_knn_b5))
# 
# predic_val_knn_b5
# 0   1   2
# 0 527   6   3
# 1   1 304   2
# 2   1   0 267


###Treinar building, todo dataset, knn. depois trocar os menores de -95 e rescale
#com o val tambem

#separar o dado em train e test
inTrain18 <- createDataPartition(y = wifi_building$BUILDINGID, p = .75, list = FALSE) 

#criar o train e o set
training18 <- wifi_building[ inTrain18,]
testing18 <- wifi_building[-inTrain18,]

#set seed
set.seed(123)

#treinar modelo
knn_building6 <- train(factor(BUILDINGID) ~ .,
                       data = training18 %>% 
                         select(starts_with("WAP"), BUILDINGID),   
                       method = 'kknn', 
                       preProc = c('center','scale'), 
                       tuneLength = 1, 
                       trControl = crossV)

# metricas
# Accuracy   Kappa    
# 0.9950082  0.9920272

#fazer prediction
knn_building_predic6 <- predict(knn_building6, testing18)

#postResample
postResample(testing18$BUILDINGID, knn_building_predic6)

# metricas
# Accuracy     Kappa 
# 0.9953010 0.9924972 

###
#aplicar meu modelo na validation e comparar com o resultado que já tem
predic_val_knn_b6 <- predict(knn_building6, wifi_validation_building) 

#comparando 
postResample(wifi_validation_building$BUILDINGID, predic_val_knn_b6)

# metricas
# Accuracy     Kappa 
# 0.9990999 0.9985774 

# confusionMatrix(table(wifi_validation_building$BUILDINGID, predic_val_knn_b6))
 
# predic_val_knn_b6
# 0   1   2
# 0 536   0   0
# 1   0 306   1
# 2   0   0 268

##
#O mesmo, mas com random forest

###Treinar building, todo dataset, knn. depois trocar os menores de -95 e rescale
#com o val tambem

#set seed
set.seed(123)

#treinar modelo
RForest_building <- train(factor(BUILDINGID) ~ .,
                       data = training18 %>% 
                         select(starts_with("WAP"), BUILDINGID),   
                       method = '	rf', 
                       preProc = c('center','scale'), 
                       tuneLength = 1, 
                       trControl = crossV)

metricas
Accuracy   Kappa    
0.9950082  0.9920272

#fazer prediction
RForest_building_predic <- predict(RForest_building, testing18)

#postResample
postResample(testing18$BUILDINGID, RForest_building_predic)

metricas
Accuracy     Kappa 
0.9953010 0.9924972 

###
#aplicar meu modelo na validation e comparar com o resultado que já tem
predic_val_RForest <- predict(RForest_building, wifi_validation_building) 

#comparando 
postResample(wifi_validation_building$BUILDINGID, predic_val_RForest)

metricas
Accuracy     Kappa 
0.9990999 0.9985774 

confusionMatrix(table(wifi_validation_building$BUILDINGID, predic_val_RForest))

predic_val_knn_b6
0   1   2
0 536   0   0
1   0 306   1
2   0   0 268



#####CORRER RANDOM FOREST ACIMA. NAO TERMINEI

