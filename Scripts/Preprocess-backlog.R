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

#filtrar por prédio
#building0 <- wifi_data %>% filter(BUILDINGID == 0) 

#Zero Variance ----
wifi_var0 <- nearZeroVar(wifi_data, saveMetrics = TRUE)

#vou dar um valor para x
x <- 0

#selecionar as variáveis que têm variância maior que zero
cols_ok <- c(rownames(wifi_var0[wifi_var0$freqRatio > x,]))

#manter ela no dataset principal/eliminar as que tem variancia zero
wifi_data2 <- as.data.frame(wifi_data [,cols_ok])

#filtrar o prédio zero para comprar a métrica com outro modelo
#building0_ <- wifi_data2 %>% filter(BUILDINGID == 0)


#Rescale N ---- 

#substituir o +100 dos waps por 0
#primeiro vou filtrar só os waps para poder substituir todos
#depois do um cbind e voltar com o dataset do mesmo tamanho

# #separar/ filtrar os waps e em outro dataframe as left variables
# wifi_data2 %>% select(starts_with("WAP")) -> wifi_wap
# 
# wifi_data2 %>%  select(
#   "LONGITUDE", "LATITUDE", "FLOOR", "BUILDINGID", "SPACEID",
#    "RELATIVEPOSITION", "USERID", "PHONEID", "TIMESTAMP") -> wifi_other
# 
# #agora vou substitutir os 100 por 0 no dataframe dos waps
# wifi_wap[wifi_wap == 100] <- -110
# 
# #agora quero aplicar a fórmula nas colunas de waps
# wifi_wap %>% mutate_all(as.integer) -> wifi_wap
# 
# #create function on R
# fun_wap_transform <- function(vector) {
#   vector <- 10^(vector/10) * 100000000000
#   return(vector)
# }
# 
# #apply the function
# #fun_wap_transform(vector = wifi_wap$WAP001)
# 
# #apply function to all data
# wifi_wap2 <- wifi_wap %>%
#   mutate_at(vars(starts_with("WAP")), fun_wap_transform)
# 
# #interger
# wifi_wap2 %>% mutate_all(as.integer) -> wifi_wap3
# 
# #eliminar NA
# wifi_wap3[wifi_wap3 == 1] <- 0
# 
# #agora vou juntar os waps modificados com as outras 9 variaveis
# wifi_data3 <- cbind(wifi_wap3, wifi_other)
# 
# #filtrar por prédio
# building0_2 <- wifi_data3 %>% filter(BUILDINGID == 0)
# 
# building0_2 %>% mutate_all(as.numeric) -> building0_3
# 
# #metrica ruim/ nao funcionou

# #tentar funçao log
# wifi_wap2 <- log10(wifi_wap)
# #também não funcionou

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

#filtrar por prédio
#building0_4 <- wifi_data3 %>% filter(BUILDINGID == 0)

###
#PCA ---- 
# wifi_pca <- prcomp(wifi_data3[,c(1:465)], center = TRUE,scale. = TRUE)
# summary(wifi_pca)
# screeplot(wifi_pca,npcs = 80)
# variance <- wifi_pca$sdev^2/sum(wifi_pca$sdev^2)*100
# plot(variance, type = "line", col = "red")

# #tentativa 2
# wifi_pca <- PCA(wifi_data3[,c(1:465)], graph = FALSE)
# 
# #para ver os componentes
# print(wifi_pca)
# 
# #ver esse componente
# eig.val <- get_eigenvalue(wifi_pca)
# eig.val
# 
# #plot
# fviz_eig(wifi_pca, addlabels = TRUE, ylim = c(0, 50))
# 
# #mais componente
# var <- get_pca_var(wifi_pca)
# var
# 
# # Coordinates
# head(var$coord)
# 
# # Cos2: quality on the factore map
# head(var$cos2)
# 
# # Contributions to the principal components
# head(var$contrib)
# 
# # Coordinates of variables
# head(var$coord, 4)
# 
# #plot
# fviz_pca_var(wifi_pca, col.var = "black")
# 
# #correlacao
# head(var$cos2, 4)
# 
# #visualizar
# corrplot(var$cos2, is.corr=FALSE)
# 
# #em bars
# fviz_cos2(wifi_pca, choice = "var", axes = 1:2)
# 
# # Color by cos2 values: quality on the factor map
# fviz_pca_var(wifi_pca, col.var = "cos2",
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
#              repel = TRUE # Avoid text overlapping
# )
# 
# #no final, olhando a contribuicao acumulada chegar a 80%, temos
# #142 atributos. Então vc faz o pca de novo com 142 dimensoes
# wifi_pca2 <- PCA(wifi_data3[,c(1:465)], graph = FALSE, ncp = 142)
# wifi_pca2$ind$contrib
# #essa seria a tabela com os novos atributos

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
#temp <- wifi_data4[!(wifi_data4$id %in% outlier_data$id),]
#checar se o anti_join funcinou, comparando com o resultado da 
#outra formula
# sum(temp != wifi_data5)
# [1] 0

#Eliminar User 6 ----
#eliminar TODOS os User 6 e PhoneID 19 (celular nao deve estra funcionando bem)
#430 já tinham sido eliminados acima. Agora mais 550
wifi_data6 <- wifi_data5 %>% filter(!PHONEID == 19)

#criar nova sample pra treinar modelo
#building0_5 <- wifi_data6 %>% filter(BUILDINGID == 0)

###
#Eliminar duplicated----

#se deixar todas as variaveis, nao vai encontrar os duplicados. o que nao pode 
#estar duplicado? Tempo e user/phone.

#chequei os duplciados com todo o dataset e deu zero. eliminando waps, strongestwap,
#sw_sign e id, conseguimos achar os duplicados

# wifi_data6 %>% select(-starts_with("WAP"), -StrongestWap, -sw_sign, -id ) %>% duplicated() %>% sum()
# [1] 5257
#agora vamos eliminar. o dataset tinha 18,879 e vamos eliminar 5,257. Ficamos
#com 13,622. 

wifi_data6 %>% select(-starts_with("WAP"),
                      -StrongestWap,
                      -sw_sign,
                      -id) %>% 
  duplicated() -> wifi_duplicated

wifi_data7 <- wifi_data6[!wifi_duplicated,]
#criou uma linha de false e true e disse que nao queria os trues.

###
#filtrar o prédio zero para comprar a métrica com outro modelo
#building0_6 <- wifi_data7 %>% filter(BUILDINGID == 0)

###
#Preprocess validation----

#aplicar todo o processo de preprocess no validation (zero variance,
#trocar 100 por -105, eliminar outliers,  eliminar duplicated)

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

#só deixei os waps que estão no validation também. Quando eu fizer a prediction
#no validation, ele automaticamente vai usar os waps que estao no modelo,
#que sao os que match com o training

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