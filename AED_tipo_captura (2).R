# ANALISIS EXPLORATORIO DE DATOS

# Renombramos Database
data<-DI_Anonimizada_2020_2021

# Anulamos las variables que no interes
data$C?digo<-NULL
data$`C?digo DANE dpto.`<-NULL
data$`C?odigo DANE municipio`<-NULL
data$Anotaciones<-NULL


# Recodificacion de la varible "Mes de desembarco"
table(data$`Mes de desembarco`)
data$`mes_desembarco`<-factor(data$`Mes de desembarco`,
                              labels = c("Enero", "Febrero", "Marzo", "Abril",
                                         "Mayo","Junio","Julio","Agosto",
                                         "Septiembre","Octubre","Noviembre", 
                                         "Diciembre"))
#Elimino variable numerica para mes de desembarco 
data$`Mes de desembarco`<-NULL

# Ordenando y renombrando la Database
names(data)
data=data[ , c(1,20,2,3,4,7,8,5,6,12,13,14,9,11,10,16,15,17,18,19)]
names(data)
names(data) = c("a?o","mes_desembarco","litoral","departamento",
                "municipio", "area_operacion","zona_pesca","tipo_pesqueria","metodo_pesca",
                "especie","nombre_comun","tipo_captura","cant_tripulantes","cant_lances_diarios",
                "dias_fuera_puerto","presentacion","cat_comercial","peso_kg","precio_kg","lugar_destino")
names(data)

# Variables 

# 13 variables cualitativas nominales
# 1 variable cualitativa categorica
# 3 variables cuantitativas discretas
# 1 variable cuantitativa continua


# Estructura base de datos

str(data)
summary(data)
names(data)
# --------------------------------------------------------------------------------------------------------

# LITORAL
a<-as.data.frame(table(data$litoral,data$a?o));a
colnames(a)<-c("Coast","Year","Frequency");a

library(ggplot2)
ggplot(data = a,mapping = aes(x=Coast,y=Frequency,fill=Year)) +
  geom_bar(stat = "identity",color="black",position = position_dodge2()) +
  scale_fill_manual(values = c("blue1","brown1")) +
  labs(tag = "BarPlot No. 1",title = "BarPlot for Coast and Year") +
  ggeasy::easy_center_title() + labs(x="Coast",y="Frequency")


b<-as.data.frame(table(data$litoral,data$mes_desembarco));b
colnames(b)<-c("Coast","Month","Frequency");b
  
ggplot(data = b,mapping = aes(x=Coast,y=Frequency,fill=Month)) +
  geom_bar(stat = "identity",color="black",position = position_dodge2()) +
  labs(title = "BarPlot for Coast and Month",tag = "BarPlot No. 2") + ggeasy::easy_center_title()
# -------------------------------------------------------------------------------------------------------

# DEPARTAMENTO Y MUNICIPIO
c<-as.data.frame(table(data$departamento,data$municipio));c
colnames(c)<-c("Department","Municipality", "Frequency");c

ggplot(c,aes(x=Department,y=Frequency,fill=Municipality)) +
  geom_col() +
  labs(title = "BarPlot for Department and Municipality",tag = "BarPlot No. 3") +
  ggeasy::easy_center_title()


# -------------------------------------------------------------------------------------------------------

# ZONA DE PESCA
d<-as.data.frame(table(data$zona_pesca));d
colnames(d)<-c("FishingZone","Frequency");d


ggplot(d,aes(x=FishingZone,y=Frequency,fill=FishingZone)) +
  geom_bar(stat = "identity",color="black") +
  theme_void() +
  labs(title = "BarPlot for Fishing Zone",tag = "BarPlot No. 4") +                              # REVISARR FULLL
  ggeasy::easy_center_title() +
  geom_text(aes(x=FishingZone, y=Frequency,
                label=Frequency),color="black",
            position = position_stack(0.8))

# --------------------------------------------------------------------------------------------------------

# AREA DE OPERACION
e<-as.data.frame(table(data$area_operacion,data$a?o));e
colnames(e)<-c("AreaOperation","Year","Frequency");e

ggplot(e,aes(x=AreaOperation,y=Frequency,fill=Year)) +
  geom_bar(stat = "identity",color="black") +
  labs(x="Area of Operation",y="Frequency") +
  labs(title = "BarPlot for Area Of Operation",tag = "BarPlot No. 5") + 
  ggeasy::easy_center_title() +
  geom_text(aes(x=AreaOperation, y=Frequency,
                label=Frequency),color="black",
            position = position_stack(0.8))

# --------------------------------------------------------------------------------------------------------

# TIPO DE PESQUERIA
f<-as.data.frame(table(data$tipo_pesqueria,data$metodo_pesca));f
colnames(f)<-c("TypeFishery","FishingMethod","Frequency");f

ggplot(f,aes(x=TypeFishery,y=Frequency,fill=FishingMethod)) +
  geom_bar(stat = "identity",position = position_dodge2(),color="black") +
  labs(x="Type of Fishery",y="Frequency") +
  labs(title = "BarPlot for Type of Fishery and Fishing Method",tag = "BarPlot No. 6") +
  ggeasy::easy_center_title()
  
# --------------------------------------------------------------------------------------------------------

# TIPO DE CAPTURA Y A?O
g<-as.data.frame(table(data$tipo_captura,data$a?o));g
colnames(g)<-c("CaptureType","Year","Frequency");g

ggplot(g,aes(x=CaptureType,y=Frequency,fill=Year)) +
  geom_bar(stat = "identity",position = position_dodge2(),color="black") +
  scale_fill_manual(values = c("dodgerblue2","darkorange2")) +
  labs(title = "BarPlot for Capture Type and Year",tag = "BarPlot No. 7") +
  ggeasy::easy_center_title() + labs(x="Capture Type",y="Frequency")

h<-as.data.frame(table(data$tipo_captura,data$mes_desembarco));h
colnames(h)<-c("x","Month","z");h

ggplot(h,aes(x=x,y=z,fill=Month)) +
  geom_bar(stat = "identity",position = position_dodge2(),color="black")+
  labs(title = "BarPlot for Capture Type and Month",tag = "BarPlot No. 8")+
  ggeasy::easy_center_title()+labs(x="Capture Type",y="Frequency")


# --------------------------------------------------------------------------------------------------------

# NOMBRE COMUN Y ESPECIE
i<-as.data.frame(table(data$especie));i
colnames(i)<-c("Common Name","Frequency");i

s<-as.data.frame(table(data$nombre_comun));s
s<-s[order(s$Freq),];s

barplot(table(data$nombre_comun),
        col = 'green2',
        border = 'black',
        xlab = "Common Name",
        ylab = "Frequency",
        
)


# ---------------------------------------------------------------------------------------------------------

# PRESENTACION

j<-as.data.frame(table(data$presentacion));j
colnames(j)<-c("Presentation", "Frequency");j

ggplot(j,aes(x=Presentation,y=Frequency,fill=Presentation)) +
  geom_bar(stat = "identity",position = position_dodge2(),color="black") +
  labs(title = "Barplot for Presentation to the market") +
  ggeasy::easy_center_title()
  
l<-as.data.frame(table(data$cat_comercial));l
ggplot(l,aes(x=Var1,y=Freq))+
  geom_bar(stat = "identity",position = position_dodge2(),color="black")
# ---------------------------------------------------------------------------------------------------------

# CANTIDAD DE TRIPULANTES

# Medidas de localizacion
summary(data$cant_tripulantes)
quantile(data$cant_tripulantes)

# Moda
mfv(data$cant_tripulantes)

# Medidas de dispersion

# Desviacion estandar
sd(data$cant_tripulantes)

# Coeficiente de variacion
cv_number_crew<-(4.295815/7.738407);cv_number_crew

# Coeficiente de Curtosis y asimetria
install.packages("moments")
library(moments)

kurtosis(data$cant_tripulantes) # Lectocurtica
skewness(data$cant_tripulantes) # Tiene una asimetria positiva

# Histograma de frecuencias
a<-ggplot(data = data, aes(cant_tripulantes)) +
  geom_histogram(fill="gold",colour="black",bins = 7) +
  labs(title = "Histogram for Number of Crew") +
  ggeasy::easy_center_title() +
  labs(x = "Number of Crew", y ="Count")
a

# BoxPlot - Diagrama de caja y bigotes
b<-ggplot(data,aes(x=cant_tripulantes)) +
  geom_boxplot(outlier.color = "black", fill="dodgerblue",) +
  labs(title = "BoxPlot for Number of Crew") +
  ggeasy::easy_center_title() +
  labs(x="Number of Crew") 
b

# Union
library(cowplot)
plot_grid(a,b,ncol = 2,nrow = 1)

# ---------------------------------------------------------------------------------------------------------

# DIAS FUERA DE PUERTO 
days_out_port<-as.data.frame(table(data$dias_fuera_puerto));days_out_port
days_out_port<-days_out_port[order(days_out_port$Freq),];days_out_port

# Medidas de localizacion
summary(data$dias_fuera_puerto)
quantile(data$dias_fuera_puerto)

# Moda
mfv(data$dias_fuera_puerto)

# Medidas de dispersion

# Desviacion estandar
sd(data$dias_fuera_puerto)

# Coeficiente de variacion
cv_days_out_port<-(16.07323/14.7222);cv_days_out_port

# Coeficiente de Curtosis y asimetria

kurtosis(data$dias_fuera_puerto) # Lectocurtica
skewness(data$dias_fuera_puerto) # Tiene una asimetria positiva

# Histograma de frecuencias
c<-ggplot(data = data, aes(dias_fuera_puerto)) +
  geom_histogram(fill="aquamarine1",colour="black",bins = 7) +
  labs(title = "Histogram for Days Out of Port") +
  ggeasy::easy_center_title() +
  labs(x = "Days Out of Port", y ="Count")
c

# BoxPlot - Diagrama de caja y bigotes
d<-ggplot(data,aes(x=dias_fuera_puerto)) +
  geom_boxplot(outlier.color = "black", fill="coral2",) +
  labs(title = "BoxPlot for Days out of Port") +
  ggeasy::easy_center_title() +
  labs(x="Days out of Port") 
d

# Union
plot_grid(c,d,ncol = 2,nrow = 1)
  
# ---------------------------------------------------------------------------------------------------------

# CANTIDAD DE LANCES DIARIOS
number_daily_casts<-as.data.frame(table(data$cant_lances_diarios));number_daily_casts
number_daily_casts<-number_daily_casts[order(number_daily_casts$Freq),];number_daily_casts

# Medidas de localizacion
summary(data$cant_lances_diarios)
quantile(data$cant_lances_diarios)

# Moda
mfv(data$cant_lances_diarios)

# Medidas de dispersion

# Desviacion estandar
sd(data$cant_lances_diarios)

# Coeficiente de variacion
cv_number_daily_casts<-(1.094684/2.97564);cv_number_daily_casts

# Coeficiente de Curtosis y asimetria

kurtosis(data$cant_lances_diarios) # Lectocurtica
skewness(data$cant_tripulantes) # Tiene una asimetria positiva

# Histograma de frecuencias
e<-ggplot(data = data, aes(cant_lances_diarios)) +
  geom_histogram(fill="darkslategray3",colour="black",bins = 9) +
  labs(title = "Histogram for Number of Daily Sets") +
  ggeasy::easy_center_title() +
  labs(x = "Number of Daily Casts", y ="Count")
e


# BoxPlot - Diagrama de caja y bigotes
f<-ggplot(data,aes(x=data$cant_lances_diarios)) +
  geom_boxplot(outlier.color = "black", fill="goldenrod4",) +
  labs(title = "BoxPlot for Number of Daily Casts") +
  ggeasy::easy_center_title() +
  labs(x="Number of Daily Casts") 
f

# Union
plot_grid(e,f,ncol = 2,nrow = 1)



# ---------------------------------------------------------------------------------------------------------

# PESO KG

# Medidas de localizacion
summary(data$peso_kg)

# Moda
mfv(data$peso_kg)

# Medidas de dispersion

# Desviacion estandar
sd(data$peso_kg)

# Coeficiente de Curtosis y asimetria

kurtosis(data$peso_kg) # Lectocurtica
skewness(data$peso_kg) # Tiene una asimetria positiva

g<-ggplot(data = data, aes(peso_kg)) +
  geom_histogram(fill="gold",colour="black",bins = 6.5) +
  labs(title = "Histogram for Weight (Kg)") +
  ggeasy::easy_center_title() +
  labs(x = "Weight", y ="Count")
g

# BoxPlot - Diagrama de caja y bigotes
h<-ggplot(data,aes(x=data$peso_kg)) +
  geom_boxplot(outlier.color = "black", fill="goldenrod4",) +
  labs(title = "BoxPlot for Weigth (Kg)") +
  ggeasy::easy_center_title() +
  labs(x="Weigth (Kg)") 
h

# Union
plot_grid(g,h,ncol = 2,nrow = 1)


# prboando

