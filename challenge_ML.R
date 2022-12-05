#Test media.monks
install.packages('fastDummies')
install.packages('plotrix')
install.packages('MLmetrics')
install.packages('GGally')
library(GGally)
library(tidyr)
library(dplyr)
library(ggplot2)
library(fastDummies)
library(plotrix)
library(MLmetrics)

data <- read.csv(choose.files())
data <- cbind(data, n_date=as.Date(as.character(data$date),"%Y%m%d"))
data_3 <- data  #copio base de datos
data_3[is.na(data_3)]<-0  #cambio na's por 0 (funciona para los cálculos que quiero hacer en este caso en particular)

#Gráficos para analizar los datos de la base de datos.

#1) Bounce rate por mes
bounce_rate <- data_3 %>% group_by(month = lubridate::floor_date(n_date, 'month'), channelGrouping)  %>% summarise(bounce_rate=mean(bounces))
ggplot(bounce_rate,aes(x=month, y=bounce_rate, color=channelGrouping))+geom_line()+theme(legend.position = 'bottom')+
  labs(x='Month', y='Bounce Rate', color='Group') 

#2) timeOnSite per device
time_per_device <- data_3 %>% group_by(data_3$deviceCategory)%>% summarise(meanTime=mean(timeOnSite))
ggplot(time_per_device, aes(x="",y=meanTime, fill=`data_3$deviceCategory`)) +
  geom_bar(stat="identity",width = 1,color="white")+
  coord_polar("y",start=0)+
  theme_void()+labs(fill='Device Category')+ggtitle("Time per device")

#3) stacked bar chart mes a mes, de los clientes según cantidad de transacciones
table_6 <- data_3 %>% filter(transactions>0) %>% group_by(month = lubridate::floor_date(n_date, 'month'), fullVisitorID)%>% summarise(tot_tr = sum(transactions))
table_6 <- cbind(table_6, condition = ifelse(table_6$tot_tr==1, "1 transaction", ifelse(table_6$tot_tr>2, "> 2 transactions", "2 transactions")))
table_6 <- table_6 %>% group_by(month, condition) %>% tally(., wt=NULL, name='Number of Clients')
ggplot(table_6, aes(fill=condition, y=`Number of Clients`, x=month))+geom_bar(position="fill", stat="identity")+
  labs(fill='Tipe of Client', x='Month', y='% of Clients')+ggtitle("Clients per month")

#4) Scatter diagram de revenue considerando sesiones que no fueron bounce y que duraron mas de 5min
no_bounce_data <- data_3 %>% filter(bounces==0 & timeOnSite>300 & hits > 3) %>% group_by(fullVisitorID) %>% summarise(max_visit_num = max(visitNumber), mean_hits = mean(hits), tot_revenue = sum(transactionRevenue))
no_bounce_data <- cbind(no_bounce_data, cond=ifelse(no_bounce_data$tot_revenue==0,"No Revenue", ifelse(no_bounce_data$tot_revenue>100, "High Revenue (> 100)", "Mid Revenue")))
ggplot(no_bounce_data, aes(x=log(max_visit_num), y=log(mean_hits), color=cond))+geom_point()+labs(x='Log(Total Visits)', y = 'Log(Mean Hits)', color = 'Revenue Category')+ggtitle("Revenue per interactive user")

#5) Mapa 
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
tr_country <- data_3 %>% group_by(country) %>% summarise(mean_tr = mean(transactions))
colnames(tr_country) <- c('name','mean_tr')
world <- merge(world, tr_country, by='name', all.x=TRUE)
world[is.na(world)] <-0
ggplot(data = world) +
  geom_sf(aes(fill = mean_tr)) +
  scale_fill_viridis_c(option = "plasma", trans='log')+
  labs(fill="log mean transactions")+ggtitle("Mean Transactions per country")

#En esta parte utilizo un ML para poder predecir si un usuario dado hará una transacción. 

#Armo la tabla necesaria para aplicar un modelo de regresión logística

data_4 <- dummy_cols(data_3, select_columns = 'deviceCategory')
data_visitNumber <- data_4 %>% group_by(fullVisitorID) %>% summarise(Max_visits=max(visitNumber))
data_rest <- data_4 %>% group_by(fullVisitorID) %>% summarise_at(.vars=vars(hits, bounces, deviceCategory_desktop, deviceCategory_mobile, deviceCategory_tablet, transactions),.funs = c(mean = 'mean'))
data_glm <- merge(data_visitNumber, data_rest, by = 'fullVisitorID')
data_glm <- cbind(data_glm, dummy_transact=data_glm$transactions_mean>0)
data_glm <- cbind(data_glm, n_max_visits1=(data_glm$Max_visits==1), n_max_visits2=(data_glm$Max_visits==2), n_max_visits3=(data_glm$Max_visits==3), n_max_visits4=(data_glm$Max_visits>3))
data_glm <- cbind(data_glm, hits_mean1=(data_glm$hits_mean<10), hits_mean2=(data_glm$hits_mean>=10 & data_glm$hits_mean<20), hits_mean3=(data_glm$hits_mean>=20 & data_glm$hits_mean<50), hits_mean4 = (data_glm$hits_mean>=50)) 

set.seed(1)
sample <- sample(c(TRUE,FALSE),nrow(data_glm),replace=TRUE,prob=c(0.85,0.15))
train <- data_glm[sample, ]
test <- data_glm[!sample, ]

scores <- c()
#pruebo con diferentes pesos (weights)
for(w in c(1,2,3,4,5,6,7,8,9)){
  #se usan weights porque los datos están desbalanceados.
  weights <- train$dummy_transact*w + (1-train$dummy_transact)
  #log por la escala
  classifier <- glm(dummy_transact~log(Max_visits)+n_max_visits1+n_max_visits2+n_max_visits3+log(hits_mean+1)+hits_mean1+hits_mean2+hits_mean3+bounces_mean+deviceCategory_desktop_mean+deviceCategory_mobile_mean,family='binomial', weights = weights, data=train)
  test_prob <- classifier %>% predict(test, type="response")
  F1 <- F1_Score(y_pred=(test_prob>0.5)*1,y_true = test$dummy_transact*1,positive="1")
  #F1 es la métrica que utilizo para comparar
  scores <- c(scores,F1)
}# el sexto weight resultó con mejor score
weights <- data_glm$dummy_transact*6 + (1-data_glm$dummy_transact)
final_classifier <- glm(dummy_transact~log(Max_visits)+n_max_visits1+n_max_visits2+n_max_visits3+log(1+hits_mean)+hits_mean1+hits_mean2+hits_mean3+bounces_mean+deviceCategory_desktop_mean+deviceCategory_mobile_mean,family='binomial', weights = weights, data=data_glm)

#La predicción se puede realizar a partir del ID de un usuario. Si el usuario ya visitó la web, se deben buscar las características del mismo necesarias para el modelo. En el caso de un nuevo usuario - que nunca interaccionó con la página - se podrían considerar valores promedios de usuarios que solo realizaron una visita.