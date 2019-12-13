##--
#Goal: Preprocess
#Description: Clean the data
#Developer: Letícia Marçal
##--

#Data ---- 
wifi_data <- read.csv("Data/trainingData.csv", sep = ",")
wifi_validation <- read.csv("Data/validationData.csv", sep = ",")

#Libraries ---- 
library(caret)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(FactoMineR)
library(corrplot)
library(scales)

#Zero Variance ----
wifi_var0 <- nearZeroVar(wifi_data, saveMetrics = TRUE)

#vou dar um valor para x
x <- 0

#selecionar as variáveis que têm variância maior que zero
cols_ok <- c(rownames(wifi_var0[wifi_var0$freqRatio > x,]))

#manter ela no dataset principal/eliminar as que tem variancia zero
wifi_data2 <- as.data.frame(wifi_data [,cols_ok])

#Trocar 100 por -105 ---- 
#separar/ filtrar os waps e em outro dataframe as left variables
wifi_data2 %>% select(starts_with("WAP")) -> wifi_wap

wifi_data2 %>%  select(
  "LONGITUDE", "LATITUDE", "FLOOR", "BUILDINGID", "SPACEID",
   "RELATIVEPOSITION", "USERID", "PHONEID", "TIMESTAMP") -> wifi_other

#agora vou substitutir os 100 por 0 no dataframe dos waps
wifi_wap[wifi_wap == 100] <- -105

#agora vou juntar os waps modificados com as outras 9 variaveis
wifi_data3 <- cbind(wifi_wap, wifi_other)

#Outliers (-30 to 0) ---- 

#dar um gather e filtrar os outliers
outlier <- wifi_data3 %>% 
  rownames_to_column(var = "id") %>% 
  pivot_longer(
    cols = starts_with("WAP"), 
    names_to = "WAPs", 
    values_to = "values"
  ) %>% 
  filter(between(values, -30, 0))

#filtrar e criar novo dataframe
outlier_data <- wifi_data3 %>% 
  rownames_to_column(var = "id") %>% 
  filter(id %in% outlier$id)

#DF Sinais + fortes ---- 

#criar coluna com o wap mais forte de cada linha
wifi_data3$StrongestWap <- colnames(wifi_data3 %>% select(starts_with("WAP")))[apply(wifi_data3 %>% select(starts_with("WAP")), 1,which.max)]

#criar coluna com o valor do wap mais forte
wifi_data3$sw_sign <- apply(wifi_data3 %>% select(starts_with("WAP")), 1, max)

#criar no outlier_data também
outlier_data$StrongestWap <- colnames(outlier_data %>% select(starts_with("WAP")))[apply(outlier_data %>% select(starts_with("WAP")), 1,which.max)]

outlier_data$sw_sign <- apply(outlier_data %>% select(starts_with("WAP")), 1, max)

##

#criar coluna de id no wifi_data3 para poder eliminar os outliers
wifi_data4 <- wifi_data3 %>% 
  rownames_to_column(var = "id")

#Eliminar os outliers----
wifi_data5 <- anti_join(wifi_data4, outlier_data)


#Eliminar User 6 ----
#eliminar TODOS os User 6 e PhoneID 19 (celular nao deve estra funcionando bem)
#430 já tinham sido eliminados acima. Agora mais 550
wifi_data6 <- wifi_data5 %>% filter(!PHONEID == 19)

#criar nova sample pra treinar modelo
#building0_5 <- wifi_data6 %>% filter(BUILDINGID == 0)

###
#Eliminar duplicated----
wifi_data6 %>% select(-starts_with("WAP"),
                      -StrongestWap,
                      -sw_sign,
                      -id) %>% 
               duplicated() -> wifi_duplicated

wifi_data7 <- wifi_data6[!wifi_duplicated,]

#Preprocess validation----

#Zero Variance V----
wifi_0var_val <- nearZeroVar(wifi_validation, saveMetrics = TRUE)

x <- 0

cols_ok_v <- c(rownames(wifi_0var_val[wifi_0var_val$freqRatio > x,]))

wifi_validation2 <- as.data.frame(wifi_validation[,cols_ok_v])

#100 por -105 V----

wifi_validation2 %>% select(starts_with("WAP")) -> wifi_wap_v

wifi_validation2 %>%  select(
  "LONGITUDE", "LATITUDE", "FLOOR", "BUILDINGID", "PHONEID", "TIMESTAMP") -> wifi_other_v

wifi_wap_v[wifi_wap_v == 100] <- -105

wifi_validation3 <- cbind(wifi_wap_v, wifi_other_v)

#DF Sinais + fortes V ---- 

wifi_validation3$StrongestWap <- colnames(wifi_validation3 %>% select(starts_with("WAP")))[apply(wifi_validation3 %>% select(starts_with("WAP")), 1,which.max)]

wifi_validation3$sw_sign <- apply(wifi_validation3 %>% select(starts_with("WAP")), 1, max)

#achar os waps que têm em ambos (training e validation) depois do zeroVariance.

#Eliminar WAPs diferentes----

#primeiro vou deixar só waps nos dois datasets
wifi_data7 %>% select(starts_with("WAP")) -> wifi_data_waps
wifi_data7 %>% select(-starts_with("WAP")) -> wifi_data_others
#criar ID
wifi_data_waps <- tibble::rowid_to_column(wifi_data_waps, "ID")

#so waps
wifi_validation3 %>% select(starts_with("WAP")) -> wifi_validation_waps
wifi_validation3 %>% select(-starts_with("WAP")) -> wifi_validation_others
#criar ID
wifi_validation_waps <- tibble::rowid_to_column(wifi_validation_waps, "ID")

wifi_data8 <- wifi_data_waps[,names(wifi_data_waps) %in% names(wifi_validation_waps)]

#vou aplicar o zeroVariance again, porque alguns waps passaram 
#a ter zero var depois de alguma linhas eliminadas
wifi_var0_3 <- nearZeroVar(wifi_data8, saveMetrics = TRUE)

x <- 0

cols_ok_3 <- c(rownames(wifi_var0_3[wifi_var0_3$freqRatio > x,]))

wifi_data9 <- as.data.frame(wifi_data8 [,cols_ok_3])

#agora vou voltar as outras variaveis pro lugar
wifi_data10 <- cbind(wifi_data9, wifi_data_others)
wifi_validation4 <- cbind(wifi_validation_waps, wifi_validation_others)

#tirar coluna repetida
wifi_data11 <- wifi_data10[, -312]

#Escale rows----

#primeiro vou deixar so os waps
wifi_data11 %>% select(starts_with("WAP")) -> waps_temp
wifi_data11 %>% select(-starts_with("WAP")) -> other_temp

#aplicar o scale
waps_rescale <- as.data.frame(t(apply(waps_temp, 1, rescale)))

#juntar as variaveis
wifi_data12 <- cbind(waps_rescale, other_temp)

#Escale rows V----

#primeiro vou deixar so os waps
wifi_validation4 %>% select(starts_with("WAP")) -> waps_temp_va
wifi_validation4 %>% select(-starts_with("WAP")) -> other_temp_va

#aplicar o scale
waps_rescale_va <- as.data.frame(t(apply(waps_temp_va, 1, rescale)))

#juntar as variaveis
wifi_validation5 <- cbind(waps_rescale_va, other_temp_va)

#Building transformation ----

#eu quero checar a accuracy de 100% no building. Por isso vou fazer um dataset 
#especifico para ele. transformar todos os valores menores de -95 em -105, pois
#estao fazendo ruido. vou fazer a transformacao antes do rescale e depois rescale 
#again só nesse dataset pto building. o que é barulho em um, pode nao ser em outro

waps_temp2 <- waps_temp

waps_temp2[waps_temp2 < -94] <- -105

#agora vou rescale
waps_rescaleBU <- as.data.frame(t(apply(waps_temp2, 1, rescale)))

#Juntar as variaveis
wifi_building <- cbind(waps_rescaleBU, other_temp)

#o mesmo pra validation
waps_temp_va2 <- waps_temp_va
other_temp_va2 <- other_temp_va

waps_temp_va2[owaps_temp_va2 < -94] <- -105

waps_rescaleBU_va <- as.data.frame(t(apply(waps_temp_va2, 1, rescale)))

wifi_validation_building <- cbind(waps_rescaleBU_va, other_temp_va2)

#Sinal mais forte B ----
#vou recriar coluna de sinal mais forte

#criar coluna com o wap mais forte de cada linha
wifi_building$StrongestWap2 <- colnames(wifi_building %>% select(starts_with("WAP")))[apply(wifi_building %>% select(starts_with("WAP")), 1,which.max)]

#criar coluna com o valor do wap mais forte
wifi_building$sw_sign2 <- apply(wifi_building %>% select(starts_with("WAP")), 1, max)

##
#Zero variance rows----

# Separar os waps
waps_temp3 <- waps_temp
other_temp3 <- other_temp

#fazer lista: variancia é zero? T or F
wifi_0var_rows <- apply(waps_temp3, 1, var) != 0

#eliminar as rows com variancia zero
waps_temp4 <- waps_temp3[wifi_0var_rows,]
other_temp4 <- other_temp3[wifi_0var_rows,]

#juntar as variaveis
wifi_data13 <- cbind(waps_temp4, other_temp4)

#Zero variance rows V ----
waps_temp_va3 <- waps_temp_va
other_temp_va3 <- other_temp_va

wifi_0var_rows_va <- apply(waps_temp_va3, 1, var) != 0

waps_temp_va4 <- waps_temp_va3[wifi_0var_rows_va,]
other_temp_va4 <- other_temp_va3[wifi_0var_rows_va,]

wifi_validation6 <- cbind(waps_temp_va4, other_temp_va4)




