
#### Tarea 1 Predicción del precio de casas ####

##### LIBRERÍAS ######
library(ggplot2)
library(inspectdf)
library(dplyr)
library(corrplot)
library(GGally)
library(ggrepel)
library(PerformanceAnalytics)
library(car)
library(leaps)
library(lubridate)
library(SparkR)
library(caret)
library(xgboost)
library(gridExtra)
library(sjPlot)
#install.packages("SparkR")
#####################

casas = read.csv("housePrices.csv", header = TRUE)
str(casas)
names(casas)
summary(casas$price)
sd(casas$price)

### Revisón NA
# entrega info de datos faltantes en las variables
casas %>% inspect_na

# grafica el numero de datos faltantes x variable
casas %>% inspect_na %>% show_plot


### Manipulación/Creación de variables #####
### Se considera importante tener la edad de la casa más que el año de de contrucción, también para,
## la renovación se crea variable Dummy con 0 para sin renovación y 1 con renovación
#transformación formato fecha
casas$date = as.Date(casas$date, "%Y%m%dT000000")
casas$years = ifelse((lubridate::year(casas$date) - casas$yr_built) < 0, 0,lubridate::year(casas$date) - casas$yr_built)
casas$renovated = ifelse(casas$yr_renovated!= 0, 1, 0)
#variable new para identificar casas nuevas
casas$new = ifelse(casas$years == 0, 1, 0)
#variable basement para identificar si tiene o no basement
casas$basement = ifelse(casas$sqft_basement!= 0, 1, 0)
str(casas)
summary(casas)


### Existen 3 columnas con 0 varianza. floors, lat y long. Para este caso se eliminarán lat y long
### y la variable floors se transformará en factor

casas$grade = ifelse(casas$grade<= 3,1,ifelse(casas$grade>=11,3,2))
str(casas)

#summary(casas)

### Creo Dataframe, uno solo con las variables a utilizar para comenzar con los modelos y
# otro solo con las variables numéricas numéricas


casas_predict = dplyr::select(casas, -id,-date,-lat,-long,-zipcode)  


# enrtega informacion del tipo de variable
casas_predict %>% inspect_types()
# calcula correlaciones seleccionando solo las variables numericas
cor(casas %>% select_if(is.numeric))

# entrega las correlaciones y su significancia
casas_predict %>% inspect_cor()



# Histograma price

p1 = ggplot(data = casas_predict , aes(x=price))+
  geom_histogram(bins = 15, fill="darkgreen", col = "black")+
  labs(x = "Precio", y = "Frecuencia", title = "Distribución Price")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 8000000, by = 500000))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 


# Boxplot price
p2 = ggplot(data = casas_predict, aes(x="", y=price))+
  geom_boxplot(fill="darkgreen", col = "black")+
  coord_flip()+ theme_bw()+
  scale_y_continuous(breaks = seq(0, 8000000, by = 500000))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "Precio")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

grid.arrange(p1, p2, ncol = 1, nrow = 2)
summary(casas$price)


# Histograma Sqft_living
p3 = ggplot(data = casas_predict , aes(x=sqft_living))+
  geom_histogram(bins = 15, fill="darkgreen", col = "black")+
  labs(title = "Distribución Sqft_living", x = "sqft_living", y = "Frecuencia")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 14000, by = 1000))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Boxplot sqft_living
p4 = ggplot(data = casas_predict, aes(x="", y=sqft_living))+
  geom_boxplot(fill="darkgreen", col = "black")+
  labs(y = "Sqft_living")+
  coord_flip()+ theme_bw()+
  scale_y_continuous(breaks = seq(0, 14000, by = 1000))+
  theme(axis.text.x = element_text(angle = 90)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

grid.arrange(p3, p4, ncol = 1, nrow = 2)
summary(casas$sqft_living)


# Histograma Sqft_lot
p5 = ggplot(data = casas_predict , aes(x=sqft_lot))+
  geom_histogram(bins = 15, fill="darkgreen", col = "black")+
  labs(title = "Distribución Sqft_lot", x = "sqft_lot", y = "Frecuencia")+
  theme_bw()+
  #scale_x_continuous(breaks = seq(0, 14000, by = 1000))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Boxplot sqft_lot
p6 = ggplot(data = casas_predict, aes(x="", y=sqft_lot))+
  geom_boxplot(fill="darkgreen", col = "black")+
  coord_flip()+ theme_bw()+
  #scale_y_continuous(breaks = seq(0, 14000, by = 1000))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "sqft_lot")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

grid.arrange(p5, p6, ncol = 1, nrow = 2)
summary(casas$sqft_lot)


# Histograma floors
p7 = ggplot(data = casas_predict , aes(x=floors))+
  geom_histogram(bins = 15, fill="darkgreen", col = "black")+
  labs(title = "Distribución floors", x = "floors", y = "Frecuencia")+
  theme_bw()+
  scale_x_continuous(breaks = seq(1, 4, by = 0.5))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Boxplot floors
p8 = ggplot(data = casas_predict, aes(x="", y=floors))+
  geom_boxplot(fill="darkgreen", col = "black")+
  coord_flip()+ theme_bw()+
  scale_y_continuous(breaks = seq(1, 4, by = 0.5))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "floors")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

grid.arrange(p7, p8, ncol = 1, nrow = 2)
summary(casas$floors)


# Histograma bathrooms
p13 = ggplot(data = casas_predict , aes(x=bathrooms))+
  geom_histogram(bins = 15, fill="darkgreen", col = "black")+
  labs(title = "Distribución bathrooms", x = "bathrooms", y = "Frecuencia")+
  theme_bw()+
  scale_x_continuous(breaks = seq(1, 8, by = 0.5))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Boxplot bathrooms
p14 = ggplot(data = casas_predict, aes(x="", y=bathrooms))+
  geom_boxplot(fill="darkgreen", col = "black")+
  coord_flip()+ theme_bw()+
  scale_y_continuous(breaks = seq(1, 8, by = 0.5))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "bathrooms")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

grid.arrange(p13, p14, ncol = 1, nrow = 2)
summary(casas$bathrooms)


# Histograma sqft_basement
p9 = ggplot(data = casas_predict , aes(x=sqft_basement))+
  geom_histogram(bins = 20, fill="darkgreen", col = "black")+
  labs(title = "Distribución Sqft_basement", x = "sqft_basement", y = "Frecuencia")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 5000, by = 500))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Boxplot sqft_basement
p10 = ggplot(data = casas_predict, aes(x="", y=sqft_basement))+
  geom_boxplot(fill="darkgreen", col = "black")+
  coord_flip()+ theme_bw()+
  scale_y_continuous(breaks = seq(0, 5000, by = 500))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "sqft_basement")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

grid.arrange(p9, p10, ncol = 1, nrow = 2)
summary(casas$sqft_basement)


# Histograma years
p11 = ggplot(data = casas_predict , aes(x=years))+
  geom_histogram(bins = 20, fill="darkgreen", col = "black")+
  labs(title = "Distribución years", x = "years", y = "Frecuencia")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 120, by = 10))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Boxplot years
p12 = ggplot(data = casas_predict, aes(x="", y=years))+
  geom_boxplot(fill="darkgreen", col = "black")+
  coord_flip()+ theme_bw()+
  scale_y_continuous(breaks = seq(0, 120, by = 10))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "years")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

grid.arrange(p11, p12, ncol = 1, nrow = 2)
summary(casas$years)

## Grade barras
ggplot(data = casas_predict, aes(factor(grade))) +
  geom_bar(fill = "darkgreen") +theme(legend.position="none") +
  labs(title = 'Grade', x = 'Grade') +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_y_continuous(breaks = seq(0, 22000, by = 5000))
table(casas_predict$grade)


## Grade condition
ggplot(data = casas_predict, aes(factor(condition))) +
  geom_bar(fill = "darkgreen") +theme(legend.position="none") +
  labs(title = 'Condition', x = 'Condition') +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_y_continuous(breaks = seq(0, 15000, by = 1000))
table(casas_predict$condition)


str(casas_predict)
 
############ 2.- Análisis de Correlaciones


#### Plot correlaciones todas las variables
corrplot(cor(casas_predict),type="upper",diag = FALSE,method = "circle")

chart.Correlation(casas_predict)

##Si es cercano a cero es pq hay variables que están altamente correlacionadas
det(cor(casas_predict))
#Determinante muy cercano a cero, indica que tenemos alta multicolinealidad entre las variables


###### Visualización de la relación entre "price" y las 2 variables,
### con las que está más altamente correlacionada

###### sqft_living

ggplot(data=casas_predict, aes(x=sqft_living, y=price))+
  geom_point(col='darkgreen') + 
  geom_smooth(method = "lm",se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0,8000000 , by=500000)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 


###### sqft_living15

ggplot(data=casas_predict, aes(x=sqft_living15, y=price))+
  geom_point(col='darkgreen') + 
  geom_smooth(method ="lm" ,se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0,8000000, by=500000)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
  



### Visualización con algunas variables categóricas
##### Grade
ggplot(data=casas_predict, aes(x=factor(grade), y=price))+
  geom_boxplot(col='darkgreen') + labs(x='grade')+
  scale_y_continuous(breaks= seq(0,8000000, by=500000)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank())


##### Waterfront
ggplot(data=casas_predict, aes(x=factor(waterfront), y=price))+
  geom_boxplot(col='darkgreen') + labs(x='waterfront')+
  scale_y_continuous(breaks= seq(0,8000000, by=500000)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) 

##### Condition
ggplot(data=casas_predict, aes(x=factor(condition), y=price))+
  geom_boxplot(col='darkgreen') + labs(x='condition')+
  scale_y_continuous(breaks= seq(0,8000000, by=500000)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank() ) 

summary(casas_predict)


########### Transformación variables Price a logaritmo dado que tiene asimetría hacia la 
# derecha, y a los modelos lineales les gusta la data normalmente distribuida
# Busco además variables con Skew mayor a 0,75 para transformar también a logaritmo
casas_predict$price = log1p(casas_predict$price)

#aux = casas_predict
skewed_columns = sapply(casas_predict, function(x) PerformanceAnalytics::skewness(x)  > 0.75)
casas_predict[skewed_columns] = sapply(casas_predict[skewed_columns], function(x) log1p(x))

### Reviso nuevamente los gráficos

# Histograma price
ggplot(data = casas_predict , aes(x=price))+
  geom_histogram(bins = 15, fill="darkgreen", col = "black")+
  labs( x = "Precio", y = "Frecuencia")+
  theme_bw()+
  scale_x_continuous(breaks = seq(10, 16, by = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank() ) 

summary(casas_predict$price)


# Boxplot price
ggplot(data = casas_predict, aes(x="", y=price))+
  geom_boxplot(fill="darkgreen", col = "black")+
  coord_flip()+ theme_bw()+
  scale_y_continuous(breaks = seq(10, 16, by = 1))+
  labs(y = "Price")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank() ) 



# Histograma Sqft_living
ggplot(data = casas_predict , aes(x=sqft_living))+
  geom_histogram(bins = 15, fill="darkgreen", col = "black")+
  labs( x = "Price", y = "Frecuencia")+
  theme_bw()+
  scale_x_continuous(breaks = seq(4, 10, by = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank() ) 



###### Separación de la data en entrenamiento y test
set.seed(1)
#indices_entrenamiento <- sample(x = 1:nrow(casas_predict),
                                #size = round(nrow(casas_predict) * (2/3)))
# 2/3 de las observaciones
#indices_test <- (1:nrow(casas_predict))[-indices_entrenamiento]
#casas_training <- casas_predict[indices_entrenamiento,]
#casas_test <- casas_predict[indices_test,]

train = createDataPartition(casas_predict$price,p=2/3,list=F)
casas_test = casas_predict[-train,]
casas_train = casas_predict[train,]


# Se ajusta el modelo predictivo lineal con todas las variables
# price = bedrooms + bathrooms + sqft_living + .....
Modelo_MCO_completo = lm(price~.-sqft_basement,data=casas_train)
summary(Modelo_MCO_completo)  
test_MSE_MCO= mean((predict(Modelo_MCO_completo, casas_test) - casas_test$price)^2)
coef(Modelo_MCO_completo)

# ECM estimado via LOOCV y K-Fold CV (proceso muy lento, muchas observaciones)
# la funcion glm es equivalente a la funcion lm ## validación cruzada, dejando uno
# fuera y CV K-fold
#MCO_completo_glm = glm(price~.,data=casas_predict)
#library(boot)
# loocv (no se especifica K)
#cverr1 = cv.glm(casas_predict,MCO_completo_glm)
#cverr1$delta

# K-Fold CV
#cverr1 = cv.glm(prostate,prostate.glm,K=5)
#cverr1$delta


Modelo_Bestsubset = regsubsets(price~.-sqft_basement,data=casas_train,nvmax=20)
reg.summary=summary(Modelo_Bestsubset)
reg.summary
names(reg.summary)
reg.summary$rsq

## Gráficas indicadores de Bondad de Ajuste ##
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Cantidad de Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Cantidad de Variables",ylab="R cuadrado Ajustado",type="l")
which.max(reg.summary$adjr2)
points(18,reg.summary$adjr2[18], col="red",cex=2,pch=13)
plot(reg.summary$cp,xlab="Cantidad de Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(18,reg.summary$cp[18],col="red",cex=2,pch=13)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Cantidad de Variables",ylab="BIC",type='l')
points(17,reg.summary$bic[17],col="red",cex=2,pch=13)

dev.off()
## Variables seleccionadas para el mejor modelo con un número dado de predictores
plot(Modelo_Bestsubset,scale="r2")
plot(Modelo_Bestsubset,scale="adjr2")
plot(Modelo_Bestsubset,scale="Cp")
plot(Modelo_Bestsubset,scale="bic")

coef(Modelo_Bestsubset,18)
coef(Modelo_Bestsubset,17)



# Usando el criterio paso a paso:
#función step(), por defecto ocupa criterio AIC
# cuando especificamos R ~ 1 estamos ajustando un modelo solo con el termino constante
nulo = lm(price~1,data=casas_predict)
nulo
# Usando el criterio forward:
# ver resultados en consola
Modelo_forward= step(nulo, scope=list(lower=nulo, upper=Modelo_MCO_completo), direction="forward")
summary(Modelo_forward)
test_MSE_forward= mean((predict(Modelo_forward, casas_test) - casas_test$price)^2)
coef(Modelo_forward)
# Usando el criterio backward:
# ver resultados en consola
Modelo_backward = step(Modelo_MCO_completo, data=casas_predict, direction="backward")
summary(Modelo_backward)
test_MSE_backward= mean((predict(Modelo_backward, casas_test) - casas_test$price)^2)
coef(Modelo_backward)

# Usando el criterio stepwise:
# ver resultados en consola
Modelo_stepwise = step(nulo, scope = list(upper=Modelo_MCO_completo), data=casas_predict, direction="both")
summary(Modelo_stepwise)
test_MSE_stepwise = mean((predict(Modelo_stepwise, casas_test[,-1]) - casas_test$price)^2)
coef(Modelo_stepwise)


tab_model(Modelo_forward)


##### Regularización ###

set.seed(123)
my_control <-trainControl(method="cv", number=5)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))
Modelo_lasso <- train(x=casas_train[,-1], y=casas_train$price, 
                      method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 
Modelo_lasso$bestTune
summary(Modelo_lasso)
test_MSE_lasso = mean((predict(Modelo_lasso, casas_test[,-1]) - casas_test$price)^2)



## Ridge
set.seed(123)
my_control <-trainControl(method="cv", number=5)
ridgeGrid <- expand.grid(alpha = 0, lambda = seq(0.001,0.1,by = 0.0005))
Modelo_ridge <- train(x=casas_train[,-1], y=casas_train$price, 
                      method='glmnet', trControl= my_control, tuneGrid=ridgeGrid) 
Modelo_ridge$bestTune
summary(Modelo_ridge)
test_MSE_ridge = mean((predict(Modelo_ridge, casas_test[,-1]) - casas_test$price)^2)

## ElasticNet
set.seed(123)
my_control <-trainControl(method="cv", number=5)
enetGrid <- expand.grid(alpha = 0.5, lambda = seq(0.001,0.1,by = 0.0005))
Modelo_enet <- train(x=casas_train[,-1], y=casas_train$price, 
                      method='glmnet', trControl= my_control, tuneGrid=enetGrid) 
Modelo_enet$bestTune
test_MSE_enet = mean((predict(Modelo_enet, casas_test[,-1]) - casas_test$price)^2)



### XGBoost

#### Se buscan los mejores parámetros para el modelo
xgb_grid = expand.grid(
  nrounds = 2000,
  eta = c(0.1, 0.05, 0.01),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(1, 2, 3, 4 ,5),
  subsample=1
)

my_control <-trainControl(method="cv", number=5)
xgb_caret <- train(x=casas_train[,-1], y=casas_train$price, method='xgbTree', trControl= my_control, tuneGrid=xgb_grid) 
xgb_caret$bestTune
############

#### Acá reemplazo los bestTune
default_param<-list(
  objective = "reg:linear",
  booster = "gbtree",
  eta= 0.01, #0.05, 
  gamma=0,
  max_depth= 6, #4,#3, 
  min_child_weight=2, #5, #4, 
  subsample=1,
  colsample_bytree=1)

#transformación de la data en formato Matriz, ya que así lo trabaja XGBoost
dtrain <- xgb.DMatrix(data = as.matrix(casas_train[,-1]), label= casas_train$price)
dtest <- xgb.DMatrix(data = as.matrix(casas_test[,-1]))

###calculo Nro rounds para entrenamiento del modelo
xgbcv <- xgb.cv( params = default_param, data = dtrain, nrounds = 2000, nfold = 5, 
                 showsd = T, stratified = T, print_every_n = 40, 
                 early_stopping_rounds = 10, maximize = F)

#Entrenammiento del modelo
Modelo_XGBoost <- xgb.train(data = dtrain,params=default_param, nrounds = 1291)
test_MSE_XGBoost = mean((predict(Modelo_XGBoost, dtest) - casas_test$price)^2)

#Se identifican las 10 variables más importantes para el Modelo
model.names <- dimnames(dtrain)[[2]]
importance_matrix <- xgb.importance(model.names, model = Modelo_XGBoost)
xgb.plot.importance(importance_matrix[1:10])


## Las siguiente líneas de código son para hacer predicciones con nueva data,
## ya que el resultado será en logaritmo dado la transformación de price por la asimetría
## En necesario transformar las predicciones de vuelta a su forma original de la
## siguiente forma f(x)=ex−1f(x)=ex−1

#y_pred <- as.double(predict(Modelo_XGBoost, dtest))
#y_pred <- as.double(exp(y_pred) - 1)


