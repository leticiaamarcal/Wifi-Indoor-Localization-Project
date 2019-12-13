###
#Goal: Informacoes
#Description: conclusoes/informacoes obtidas durante o preprocess
#Developer: Letícia Marçal
###

source("Scripts/Preprocess.R")

#Libraries ----
library(arsenal)

#Predio com 5 pisos ----
#entender qual building tem 5 andares
only_4floors <- wifi_data %>% filter(FLOOR == 4)
#B2 tem 5 andares. B0 e B1 tem 4 andares

#Outlier: User e Phone (duplicado) ----

#outlier dataframe 804 outliers)

#user 6 e PhoneID 19 têm 700 "erros"/ outilier
#user 14 com Phone 7 tem 59 outliers
#User 8 com PhoneID 1 tem só um outlier
#User 10 e Phone 8 tem cinco outliers
#User 15 e P11 tem dois outliers
#User 16 com Phone 14 tem sete outliers
#User 9 com Phone 14 tem cinco outliers
#User 1 com Phone 14 tem 18 outliers
#User 3 com Phone 16 tem sete outliers

#Questão: problema é no phone ou no wap?

#os dados aqui estao duplicados. Olhar os numeros no outilier_data

#Outlier: User e Phone (unicos) ----

#total: 508 outliers 

#User 6 e PhoneID 19 têm 430 outiliers
#User 14 com Phone 7 tem 52 outliers
#User 8 com PhoneID 1 tem só um outlier
#User 10 e Phone 8 tem dois outliers
#User 15 e Phone 11 tem dois outliers
#User 16 com Phone 14 tem cinco outliers
#User 9 com Phone 14 tem dois outliers
#User 1 com Phone 14 tem nove outliers
#User 3 com Phone 16 tem cinco outliers

#Quando filtramos o dado geral, temos 508 linhas de outiliers
#487 estão no building 2, cinco estão no building 1, e quatro 
#no building 0

outlier_data %>% filter(BUILDINGID == 2) -> out_build2

# #informacao do builind 2, por piso
# dim(out_build2 %>% filter(FLOOR == 4))
# [1] 219 
# > dim(out_build2 %>% filter(FLOOR == 3))
# [1] 261 
# > dim(out_build2 %>% filter(FLOOR == 2))
# [1]  0 
# > dim(out_build2 %>% filter(FLOOR == 1))
# [1]  2 
# > dim(out_build2 %>% filter(FLOOR == 0))
# [1]  5 

#quero checar os users e phones no dado grande
wifi_data3 %>% filter(USERID == 6) -> data_user6

#O user 6 com Phone 19 só tem registro no building 2 (980 registros no total)
#510 estão no andar 3 e 470 estão no andar 4
#430 sao outliers, pois estao entre -30 e 0. Os outros 550 estão entre -30 e -104

data_user6 %>% filter(FLOOR == 3) %>% 
  filter(between(sw_sign, -30, 0)) -> data_user6_out
#no piso 3 (b2), 216 registros do user 6 sao outliers/ 294 sao normais

#o terceiro andar do prédio 2 tem 2709 registros 
#o quarto andar do predio 2 tem 1102 registros

#checar phoneID 14
wifi_data3 %>% filter(PHONEID == 14) -> data_phone14
#tá dentro dos conformes. tem mais de um user

#agora vamos checar o outlier_data para ver se tem algum wap que tem uma 
#performance duvidosa
outlier_data %>% filter(sw_sign == 0) -> outlier_data0
#todos os waps que dão 0 são com o phone 19. O problema, então, neste caso,
# e no phone e nao no wap

outlier_data %>% filter(between(sw_sign, -10, 0)) -> outlier_data2
#além do phone 19, aparece o phone 7 (user 14)

#checar de -20 a -10
outlier_data %>% filter(between(sw_sign, -20, -10)) -> outlier_data3
#mesma coisa. Phone 19 e phone 7. Proavelmente o problema é no phone

#checar de -30 a -20
outlier_data %>% filter(between(sw_sign, -30, -20)) -> outlier_data4
#os phones estão mais misturados aqui. Olhando a distrbuicao dos waps, os que
#mais aparecem foram checados pelo phone 7. o que reforca a teoria de que o 
#problema é o no phone e nao no wap

# summary(as.factor(outlier_data4$StrongestWap))
# WAP043 WAP062 WAP065 WAP080 WAP084 WAP085 WAP087 WAP121 WAP122 WAP142 WAP180 WAP262 WAP315 
# 2      7      4      4      4      1      1      1      1      3      3      1      1 
# WAP386 WAP394 WAP495 WAP501 
# 1      2      3      2 

#Checar user 14/ phone 7
wifi_data3 %>% filter(PHONEID == 7) -> data_phone7

#comparacao entre wifi_data7 e wifi_validation3
#comparedf(wifi_data7, wifi_validation3)
#Shared: 320 non-by variables and 1111 observations


# mlknn (checar como usar)
wifi_data13 <- mlknn(wifi_data12, k = 10, s = 1, distance = "euclidean")
