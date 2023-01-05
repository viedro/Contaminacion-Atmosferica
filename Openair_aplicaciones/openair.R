library(readr)
data <- read_csv2("C:/Users/USUARIO/Desktop/2021 i/datos_conta/Estacion_PteRemedios.csv")

library(openair)
date<- seq.POSIXt(as.POSIXct("2019-01-01 01:00:00"), as.POSIXct("2019-12-31 24:00:00"), by ="hour")
View(data)
data <- data[,-c(1,2)]

data <- data.frame(date,data)

colnames(data)<- c("date","CO","pm10","pm2.5","no2","nox","so2","wd","ws","P","RG","UV","T","Hr")

data1 <- data[,-c(6,7,8,9)]

#######Grafica de resumen estadistico
summaryPlot(data1,clip=T,col.trend="skyblue", period="months")

summaryPlot(data, percentile=0.98, na.len=12, col.trend="blue", xlab=c("Gráficas de 
evolución anual", "Histogramas"), main="Gráfica resumen de parámetros",ylab=c("Contaminantes","Porcentaje total"))

####Agrupacion de promedios
timeVariation(data, pollutant=c("pm10","pm2.5","CO","no2"), cols=c("darkorange", "red", "blue"), alpha=0.2, main="EVOLUCION DE LOS CONTAMINANTES  ESTACIÓN CONGONHAS",  ylab="Concentración  en  ug/m3",  xlab=c("Evolución  de  las  concentraciones  horarias  durante  la  semana","Concentraciones horarias", "Concentraciones mensuales", "Evolución por días de la semana")) 



#########################Graficas polares
####Grafica polar por mes
polarPlot(data, pollutant="pm10",type="month", cols=c("green", "yellow", "orange","red","brown"))

#Grafica polar anual
polarPlot(data, pollutant="pm10", cols=c("green", "yellow", "orange","red","brown"))

#Grafica polar 
polarPlot(data, pollutant="pm10",type="weekday", cols=c("green", "yellow", "orange","red","brown"))


###########Graficas de calendarios
calendarPlot ( data, pollutant="CO", year=2019 ,layout=c(4,3), annotate="date", main="EVOLUCIÓN de las partículas PM10 en 2019" , col.lim = c("black",  "white"),lim=50,cols="Greens")

#######Rosa de polucion
pollutionRose(data, pollutant="no2")

pollutionRose(data, pollutant="pm10", statistic="prop.mean",type="month")

#######Grafica de percentiles
percentileRose (data, pollutant="CO", percentile=c(50,75,90,95,98,99), cols=c("green", "yellow", "red"), angle.scale=20, main="Evolución de los percentiles de so2", key.header="Percentil de so2",key.footer="", key.position="right") 

######Grafica anular
polarAnnulus (data, pollutant="no2", period="hour", type="season", width="fat", exclude.missing=FALSE, cols="increment", layout=c(4,1),key.header="Concentración de SO2", key.footer="", key.possition="bottom", main="Evolución horaria de las concentraciones de SO2 por estaciones")
#######################
polarAnnulus (data, pollutant="no2", period="hour", width="fat", exclude.missing=FALSE, cols="increment",key.header="Concentración de SO2", key.footer="", key.possition="bottom", main="Evolución horaria de las concentraciones de SO2 por estaciones")

#########Graficas de calendarios
calendarPlot ( data, pollutant="pm10", year=2019 , annotate="date", cols=c("white", "yellow", "orange", "red","black"), limits=c(0,90), lim=50, col.lim= c("black", "white"), digits=0,main="EVOLUCIÓN de las partículas PM10 en 2011",layout=c(4,3)) 

calendarPlot ( data, pollutant="pm10", year=2019 , annotate="date", cols=c("white", "yellow", "orange", "red","black"), limits=c(0,90), lim=0, col.lim= c("black", "white"), digits=0,main="EVOLUCIÓN de las partículas PM10 en 2011",layout=c(4,3),cex.lim= c(0.9,1.2)) 


#####Correlacion
corPlot(data,layout=c(2,2), cluster=FALSE, main="MATRIZ DE CORELACIÓN DE PARÁMETROS", xlab="Parámetros", ylab="Parámetros", auto.text=FALSE, text.col=c("darkblue", "brown"))

corPlot(data1, cluster=FALSE, main="Matriz de Correlación de parámetros Est. Pte. Remedios", xlab="Parámetros", ylab="Parámetros", auto.text=FALSE, text.col=c("darkblue", "black"))

corPlot(data1,type="month", layout=c(6,2),cluster=FALSE, main="Matriz de Correlación de parámetros Est. Pinheiros", xlab="Parámetros", ylab="Parámetros", auto.text=FALSE, text.col=c("darkblue", "black"))
