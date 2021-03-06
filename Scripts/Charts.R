###
#Goal: Graficos
#Description: visualizacao 
#Developer: Letícia Marçal
###


source("Scripts/Preprocess.R")

#Latitude x Longitude ----
wifi_data %>% ggplot(aes(x = LONGITUDE, y = LATITUDE)) +
     geom_jitter()+
  facet_wrap(wifi_data$FLOOR)

#Lat x Long e UserID ----
#adicionar o usuário como cor. mas primeiro transformar em factor
wifi_data$USERID <-  as.factor(wifi_data$USERID)

wifi_data %>% ggplot(aes(x = LONGITUDE, y = LATITUDE, color = USERID)) +
  geom_jitter()+
  facet_wrap(wifi_data$FLOOR) 

#Lat x Long e PhoneID ----
wifi_data$PHONEID <-  as.factor(wifi_data$PHONEID)

wifi_data %>% ggplot(aes(x = LONGITUDE, y = LATITUDE, color = PHONEID)) +
  geom_jitter()+
  facet_wrap(wifi_data$FLOOR) 


#Lat x Long e SpaceID ----
wifi_data$SPACEID <-  as.factor(wifi_data$SPACEID)

wifi_data %>% ggplot(aes(x = LONGITUDE, y = LATITUDE, color = SPACEID)) +
  geom_jitter()+
  facet_wrap(wifi_data$FLOOR)

####
#Erros Modelo 1 ---- 
#plotar erros dos modelos de latitude e longitude (lm, o primeiro modelo)
testing_floor0$lati_pred <- LR_latitude_b0_predic 
testing_floor0$long_pred <- LR_longitude_b0_predic

testing_floor0 %>% ggplot() +
  geom_jitter(aes(x = LONGITUDE, y = LATITUDE), color = "green") +
  geom_jitter(aes(x = long_pred, y = lati_pred), color = "red") 

####

#Onde estão os WAPs ---- 
wifi_data3 %>% ggplot(aes(x= LONGITUDE, y= LATITUDE, color = StrongestWap)) +
                       geom_jitter() 

#checar de novo, mas depois que eu retireios erros
wifi_data6 %>% ggplot(aes(x= LONGITUDE, y= LATITUDE, color = StrongestWap)) +
  geom_jitter() +
  theme(legend.position="right")

###
#Erro Building----

#criar coluna no validation com a subritacao do resultado pela prediction
wifi_validation5$errorBU <- abs(as.numeric(as.factor(wifi_validation5$BUILDINGID)) - as.numeric(predic_val_knn_b2))


ggplot() +
  geom_jitter(data = wifi_validation5, aes(x = LONGITUDE, y = LATITUDE,
                                  color = as.factor(errorBU)))


#prediction 3
wifi_validation5$errorBU <- abs(as.numeric(as.factor(wifi_validation5$BUILDINGID)) - as.numeric(predic_val_knn_b3))


ggplot() +
  geom_jitter(data = wifi_validation5, aes(x = LONGITUDE , y = LATITUDE,
                                           color = as.factor(errorBU)))

#prediction 4
wifi_validation5$errorBU <- abs(as.numeric(as.factor(wifi_validation5$BUILDINGID)) - as.numeric(predic_val_knn_b4))


ggplot() +
  geom_jitter(data = wifi_validation5, aes(x = LONGITUDE , y = LATITUDE,
                                           color = as.factor(errorBU)))

#checar os erros
wifi_validation6$predic_long_knn3 <- predic_val_long3 

wifi_validation6 %>%
      ggplot() +
        geom_jitter(aes(x = LONGITUDE, y = LATITUDE), color = "green") +
            geom_jitter(aes(x = predic_long_knn3, y = LATITUDE), color = "red") 


