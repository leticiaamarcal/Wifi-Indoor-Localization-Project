#------------------------------------------------------------------------------------ 
#Goal: Modelos
#Description: treinar modelos
#Developer: Letícia Marçal
#-------------------------------------------------------------------------------------

source("Scripts/Preprocess.R")


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
inTrain7 <- createDataPartition(y = wifi_data9$LATITUDE, p = .75, list = FALSE) 

#criar o train e o set
training7 <- wifi_data9[ inTrain7,]
testing7 <- wifi_data9[-inTrain7,]

#cross validation
#crossV <- trainControl(method = 'repeatedcv', repeats = 2)

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
# 18.33757  0.9216367  13.53414


#prediction
LR_latitude_b0_predic7 <- predict(LR_latitude_b0_7, testing7)

#postResample
postResample(testing7$LATITUDE, LR_latitude_b0_predic7)

# metricas
# RMSE   Rsquared        MAE 
# 18.1540399  0.9234456 13.5656230  

###
#depois do zerovariance again

#separar o dado em train e test
inTrain8 <- createDataPartition(y = wifi_data12$LATITUDE, p = .75, list = FALSE) 

#criar o train e o set
training8 <- wifi_data12[ inTrain8,]
testing8 <- wifi_data12[-inTrain8,]

#cross validation
#crossV <- trainControl(method = 'repeatedcv', repeats = 2)

#set seed
set.seed(123)

#treinar modelo
LR_latitude_b0_8 <- train(LATITUDE ~ .,
                          data = training8 %>% 
                            select(starts_with("WAP"), LATITUDE),
                          method = 'lm',
                          preProc = c('center', 'scale'), 
                          tuneLength = 1, 
                          trControl = crossV)

# metricas
# RMSE      Rsquared   MAE     
# 18.23757  0.9225073  13.51944


#prediction
LR_latitude_b0_predic8 <- predict(LR_latitude_b0_8, testing8)

#postResample
postResample(testing8$LATITUDE, LR_latitude_b0_predic8)

# metricas
# RMSE   Rsquared        MAE 
# 18.7435812  0.9185473 13.8590876  

###
#aplicar meu modelo na validation e comparar com o resultado que já tem
predic_val1 <- predict(LR_latitude_b0_8, wifi_validation3) 
