library(readr)
##Se importa los datos usar
setwd("C:/Users/USUARIO/Desktop/2021 i/datos_conta")

#Ingreso de datos

#Estaciones a trabajar
Stations <- c("Estacion_Congonhas", "Estacion_Parque D.Pedro II", "Estacion_PteRemedios","Estacion_Pinheiros")
data_hora<- list()
data_dia <- list()
data_sem <- list()
data_mes <- list()
data_year <- list()
data_ssn <- list()


#Bucle lector de archivos
for (k in 1:4) {
  path <- paste0("C:/Users/USUARIO/Desktop/2021 i/datos_conta/",Stations[k],".csv")
  data_2019_Pinheiros <- read_csv2(path)
  
  
  #Se importa los datos a usar
  #data_2019_Pinheiros <- read_csv2("Estacion_PteRemedios.csv")
  #Estableciendo nombre de columnas
  colnames(data_2019_Pinheiros) <-c("Fecha","Hora","CO_ppm","MP10_ug_m3","MP2.5_ug_m3"  ,"NO2_ug_m3","NOx_ppb","SO2_ug_m3")
  
  colnames(data_2019_Pinheiros)[3] 
  data <- data.frame(data_2019_Pinheiros)
  
  library(lubridate)
  
  library(xts)
  library(zoo)
  #####PROMEDIOS HORARIOS####################################
  horas <- rep(1:24,365)
  Horas <- 1:24
  data <- data.frame(horas, data[,-(1:2)])
  #data <- data[-8760,]

  dataset <- data.frame(CO_ppm = numeric(),
                        MP10_ug_m3 = numeric(),
                        MP2.5_ug_m3= numeric(),
                        NO2_ug_m3= numeric(),
                        NOx_ppb= numeric(),
                        SO2_ug_m3= numeric())

  for (j in 2:7){
    for (i in 1:24){
      datos <-data[,j][data$horas== i]
      dataset[i,j-1] <- mean(na.omit(datos))
    }
  }
  
  data_hora[[k]]<- data.frame(Horas,dataset)
  dataset <- data.frame(Horas,dataset)
  #write.table(dataset,paste0(Stations[k],"_horario.csv"),sep = ";",row.names = F)
  
  
  ####################
  data <- data[-8760,]
  fecha <- seq.POSIXt(as.POSIXct("2019-01-01 01:00:00"), as.POSIXct("2019-12-31 23:00:00"), by ="hour")
  
  data_hora_xts <- xts(data[,-1],order.by = fecha)
  
  ##Pasamos la primera columna a mensual, Fun= esta es la funcion a aplicar
  

  ###Horario a diario
  data_dia[[k]]<- apply.daily(data_hora_xts,FUN=mean,na.rm=T)
  
  ###Horario a semanal 
  data_sem[[k]]<- apply.weekly(data_hora_xts,FUN=mean,na.rm=T)
  
  ###Horario a mensual
  data_mes[[k]]<- apply.monthly(data_hora_xts,FUN=mean,na.rm=T)
  
  ##Diarioo a anual
  data_year[[k]] <- apply.yearly(data_hora_xts, FUN=mean,na.rm=T)
  
  ##Precipitacion estacional, suma de cada 3 meses
  data_ssn[[k]] <- apply.quarterly(data_hora_xts, FUN=mean,na.rm=T)
  
  
  library(xlsx)
  
  #write.xlsx(data_dia[[k]],paste0(Stations[k],"data_dia.xlsx"),row.names=T)
  #write.xlsx(data_sem[[k]],paste0(Stations[k],"data_sem.xlsx"),row.names=T)
  #write.xlsx(data_mes[[k]],paste0(Stations[k],"data_mes.xlsx"),row.names=T)
  #write.xlsx(data_year[[k]],paste0(Stations[k],"data_year.xlsx"),row.names=T)
  #write.xlsx(data_ssn[[k]],paste0(Stations[k],"data_ssn.xlsx"),row.names=T)
}  
#######Mensual co
library(ggplot2)
Fecha <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

data_mes_CO <- data.frame(Fecha,coredata(data_mes[[1]]),coredata(data_mes[[2]]),coredata(data_mes[[3]]),coredata(data_mes[[4]]))
data_mes_CO$Fecha <- factor(data_mes_CO$Fecha, levels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))



############CO mes
ggplot(data=data_mes_CO, aes(x=Fecha,group=1))+geom_line(aes(y=data_mes_CO[,2],colour="Est.Congonhas"))+geom_line(aes(y=data_mes_CO[,8],colour="Est.Parque D.Pedro II"))+geom_line(aes(y=data_mes_CO[,14],colour="Est.PteRemedios"))+geom_line(aes(y=data_mes_CO[,20],colour="Est.Pinheiros"))+ylab(colnames(data_2019_Pinheiros)[3])+xlab("Meses")+
  theme_light()+ggtitle("Serie de tiempo a nivel mensual: Contaminante CO \n2019 Estaciones Sao Paulo") +
  theme(plot.title = element_text(hjust = 0.5))+labs(caption="Fuente: Elaboración propia con datos de Cetesb",color="Estaciones:")+
    theme (plot.title = element_text(family="Comic Sans MS",
                                     size=rel(1.5), 
                                     vjust=0.1, 
                                     face="bold.italic", 
                                     color="gray48", 
                                     lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48", size=rel(1.3))) +
    theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))   


############PM10
ggplot(data=data_mes_CO, aes(x=Fecha,group=1))+geom_line(aes(y=data_mes_CO[,3],colour="Est.Congonhas"))+geom_line(aes(y=data_mes_CO[,9],colour="Est.Parque D.Pedro II"))+geom_line(aes(y=data_mes_CO[,15],colour="Est.PteRemedios"))+geom_line(aes(y=data_mes_CO[,21],colour="Est.Pinheiros"))+ylab(colnames(data_2019_Pinheiros)[4])+xlab("Meses")+
  theme_light()+ggtitle("Serie de tiempo a nivel mensual: Contaminante PM10 \n2019 Estaciones Sao Paulo") +
  theme(plot.title = element_text(hjust = 0.5))+labs(caption="Fuente: Elaboración propia con datos de Cetesb",color="Estaciones:")+
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=0.1, 
                                   face="bold.italic", 
                                   color="gray48", 
                                   lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48", size=rel(1.3))) +
  theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))   



#####
############PM2.5
ggplot(data=data_mes_CO, aes(x=Fecha,group=1))+geom_line(aes(y=data_mes_CO[,4],colour="Est.Congonhas"))+geom_line(aes(y=data_mes_CO[,10],colour="Est.Parque D.Pedro II"))+geom_line(aes(y=data_mes_CO[,16],colour="Est.PteRemedios"))+geom_line(aes(y=data_mes_CO[,22],colour="Est.Pinheiros"))+ylab(colnames(data_2019_Pinheiros)[5])+xlab("Meses")+
  theme_light()+ggtitle("Serie de tiempo a nivel mensual: Contaminante PM2.5 \n2019 Estaciones Sao Paulo") +
  theme(plot.title = element_text(hjust = 0.5))+labs(caption="Fuente: Elaboración propia con datos de Cetesb",color="Estaciones:")+
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=0.1, 
                                   face="bold.italic", 
                                   color="gray48", 
                                   lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48", size=rel(1.3))) +
  theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+scale_y_log10(labels = scales::comma)     

#####
############NO2
ggplot(data=data_mes_CO, aes(x=Fecha,group=1))+geom_line(aes(y=data_mes_CO[,5],colour="Est.Congonhas"))+geom_line(aes(y=data_mes_CO[,11],colour="Est.Parque D.Pedro II"))+geom_line(aes(y=data_mes_CO[,17],colour="Est.PteRemedios"))+geom_line(aes(y=data_mes_CO[,23],colour="Est.Pinheiros"))+ylab(colnames(data_2019_Pinheiros)[6])+xlab("Meses")+
  theme_light()+ggtitle("Serie de tiempo a nivel mensual: Contaminante NO2 \n2019 Estaciones Sao Paulo") +
  theme(plot.title = element_text(hjust = 0.5))+labs(caption="Fuente: Elaboración propia con datos de Cetesb",color="Estaciones:")+
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=0.1, 
                                   face="bold.italic", 
                                   color="gray48", 
                                   lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48", size=rel(1.3))) +
  theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))   


###########################################33
#########################3HOra


#######Mensual co
library(ggplot2)

data_hora_CO <- data.frame(data_hora[[1]],data_hora[[2]][,-1],data_hora[[3]][,-1],data_hora[[4]][,-1])

data_hora_CO$Horas <- factor(data_hora_CO$Horas, levels = c(1:24))

ggplot(data=data_hora_CO, aes(x=Horas,group=1))+geom_line(aes(y=data_hora_CO[,2],colour="Est.Congonhas"))+geom_line(aes(y=data_hora_CO[,8],colour="Est.Parque D.Pedro II"))+geom_line(aes(y=data_hora_CO[,14],colour="Est.PteRemedios"))+geom_line(aes(y=data_hora_CO[,20],colour="Est.Pinheiros"))+ylab(colnames(data_2019_Pinheiros)[3])+xlab("Horas")+
  theme_light()+ggtitle("Contaminación por horas del día: Contaminante CO \n2019 Estaciones Sao Paulo") +
  theme(plot.title = element_text(hjust = 0.5))+labs(caption="Fuente: Elaboración propia con datos de Cetesb",color="Estaciones:")+
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=0.1, 
                                   face="bold.italic", 
                                   color="gray48", 
                                   lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48", size=rel(1.3))) +
  theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+geom_hline(yintercept = 9,colour="red",size=1)+scale_y_log10(labels = scales::comma)+ geom_text(aes(x = 12, y = 10.5,label = "ECA = 9 ppm"),size = 3.5, color = "red")

#####PM10

ggplot(data=data_hora_CO, aes(x=Horas,group=1))+geom_line(aes(y=data_hora_CO[,3],colour="Est.Congonhas"))+geom_line(aes(y=data_hora_CO[,9],colour="Est.Parque D.Pedro II"))+geom_line(aes(y=data_hora_CO[,15],colour="Est.PteRemedios"))+geom_line(aes(y=data_hora_CO[,21],colour="Est.Pinheiros"))+ylab(colnames(data_2019_Pinheiros)[4])+xlab("Horas")+
  theme_light()+ggtitle("Contaminación por horas del día: Contaminante PM10 \n2019 Estaciones Sao Paulo") +
  theme(plot.title = element_text(hjust = 0.5))+labs(caption="Fuente: Elaboración propia con datos de Cetesb",color="Estaciones:")+
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=0.1, 
                                   face="bold.italic", 
                                   color="gray48", 
                                   lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48", size=rel(1.3))) +
  theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+geom_hline(yintercept = 50,colour="red",size=1)+scale_y_log10(labels = scales::comma)+ geom_text(aes(x = 12, y = 51.5,label = "ECA = 50 (??g/m³)"),size = 3.5, color = "red")

#####################
#####PM2.5

ggplot(data=data_hora_CO, aes(x=Horas,group=1))+geom_line(aes(y=data_hora_CO[,4],colour="Est.Congonhas"))+geom_line(aes(y=data_hora_CO[,10],colour="Est.Parque D.Pedro II"))+geom_line(aes(y=data_hora_CO[,16],colour="Est.PteRemedios"))+geom_line(aes(y=data_hora_CO[,22],colour="Est.Pinheiros"))+ylab(colnames(data_2019_Pinheiros)[5])+xlab("Horas")+
  theme_light()+ggtitle("Contaminación por horas del día: Contaminante PM2.5 \n2019 Estaciones Sao Paulo") +
  theme(plot.title = element_text(hjust = 0.5))+labs(caption="Fuente: Elaboración propia con datos de Cetesb",color="Estaciones:")+
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=0.1, 
                                   face="bold.italic", 
                                   color="gray48", 
                                   lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48", size=rel(1.3))) +
  theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+geom_hline(yintercept = 25,colour="red",size=1)+scale_y_log10(labels = scales::comma)+ geom_text(aes(x = 12, y = 25.8,label = "ECA = 25 (??g/m³)"),size = 3.5, color = "red")


#########NO2
ggplot(data=data_hora_CO, aes(x=Horas,group=1))+geom_line(aes(y=data_hora_CO[,5],colour="Est.Congonhas"))+geom_line(aes(y=data_hora_CO[,11],colour="Est.Parque D.Pedro II"))+geom_line(aes(y=data_hora_CO[,17],colour="Est.PteRemedios"))+geom_line(aes(y=data_hora_CO[,23],colour="Est.Pinheiros"))+ylab(colnames(data_2019_Pinheiros)[6])+xlab("Horas")+
  theme_light()+ggtitle("Contaminación por horas del día: Contaminante NO2 \n2019 Estaciones Sao Paulo") +
  theme(plot.title = element_text(hjust = 0.5))+labs(caption="Fuente: Elaboración propia con datos de Cetesb",color="Estaciones:")+
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=0.1, 
                                   face="bold.italic", 
                                   color="gray48", 
                                   lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48", size=rel(1.3))) +
  theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+geom_hline(yintercept = 200,colour="red",size=1)+scale_y_log10(labels = scales::comma)+ geom_text(aes(x = 12, y = 215,label = "ECA = 200 (??g/m³)"),size = 3.5, color = "red")

################
#######################Estacional

library(ggplot2)
Temporada <- c("Verano","Otoño","Invierno","Primavera")

data_ssn_CO <- data.frame(Temporada,coredata(data_ssn[[1]]),coredata(data_ssn[[2]]),coredata(data_ssn[[3]]),coredata(data_ssn[[4]]))
data_ssn_CO$temporada <- factor(data_ssn_CO$Temporada, levels = c("Verano","Otoño","Invierno","Primavera"))



############CO temporada
ggplot(data=data_ssn_CO, aes(x=Temporada,group=1))+geom_line(aes(y=data_ssn_CO[,2],colour="Est.Congonhas"))+geom_line(aes(y=data_ssn_CO[,8],colour="Est.Parque D.Pedro II"))+geom_line(aes(y=data_ssn_CO[,14],colour="Est.PteRemedios"))+geom_line(aes(y=data_ssn_CO[,20],colour="Est.Pinheiros"))+ylab(colnames(data_2019_Pinheiros)[3])+xlab("Temporada")+
  theme_light()+ggtitle("Contaminación por temporadas: Contaminante CO \n2019 Estaciones Sao Paulo") +
  theme(plot.title = element_text(hjust = 0.5))+labs(caption="Fuente: Elaboración propia con datos de Cetesb",color="Estaciones:")+
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=0.1, 
                                   face="bold.italic", 
                                   color="gray48", 
                                   lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48", size=rel(1.3))) +
  theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))   

###############PM10
ggplot(data=data_ssn_CO, aes(x=Temporada,group=1))+geom_line(aes(y=data_ssn_CO[,3],colour="Est.Congonhas"))+geom_line(aes(y=data_ssn_CO[,9],colour="Est.Parque D.Pedro II"))+geom_line(aes(y=data_ssn_CO[,15],colour="Est.PteRemedios"))+geom_line(aes(y=data_ssn_CO[,21],colour="Est.Pinheiros"))+ylab(colnames(data_2019_Pinheiros)[4])+xlab("Temporada")+
  theme_light()+ggtitle("Contaminación por temporadas: Contaminante PM10 \n2019 Estaciones Sao Paulo") +
  theme(plot.title = element_text(hjust = 0.5))+labs(caption="Fuente: Elaboración propia con datos de Cetesb",color="Estaciones:")+
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=0.1, 
                                   face="bold.italic", 
                                   color="gray48", 
                                   lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48", size=rel(1.3))) +
  theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))   


###############PM2.5
ggplot(data=data_ssn_CO, aes(x=Temporada,group=1))+geom_line(aes(y=data_ssn_CO[,4],colour="Est.Congonhas"))+geom_line(aes(y=data_ssn_CO[,10],colour="Est.Parque D.Pedro II"))+geom_line(aes(y=data_ssn_CO[,16],colour="Est.PteRemedios"))+geom_line(aes(y=data_ssn_CO[,22],colour="Est.Pinheiros"))+ylab(colnames(data_2019_Pinheiros)[5])+xlab("Temporada")+
  theme_light()+ggtitle("Contaminación por temporadas: Contaminante PM2.5 \n2019 Estaciones Sao Paulo") +
  theme(plot.title = element_text(hjust = 0.5))+labs(caption="Fuente: Elaboración propia con datos de Cetesb",color="Estaciones:")+
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=0.1, 
                                   face="bold.italic", 
                                   color="gray48", 
                                   lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48", size=rel(1.3))) +
  theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))   


###############NO2
ggplot(data=data_ssn_CO, aes(x=Temporada,group=1))+geom_line(aes(y=data_ssn_CO[,5],colour="Est.Congonhas"))+geom_line(aes(y=data_ssn_CO[,11],colour="Est.Parque D.Pedro II"))+geom_line(aes(y=data_ssn_CO[,17],colour="Est.PteRemedios"))+geom_line(aes(y=data_ssn_CO[,23],colour="Est.Pinheiros"))+ylab(colnames(data_2019_Pinheiros)[6])+xlab("Temporada")+
  theme_light()+ggtitle("Contaminación por temporadas: Contaminante NO2 \n2019 Estaciones Sao Paulo") +
  theme(plot.title = element_text(hjust = 0.5))+labs(caption="Fuente: Elaboración propia con datos de Cetesb",color="Estaciones:")+
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=0.1, 
                                   face="bold.italic", 
                                   color="gray48", 
                                   lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48", size=rel(1.3))) +
  theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))   


############3Anual

library(ggplot2)
Año <- 2019
Estaciones <- c("Est.Congonhas","Est.Parque D.PedroII","Est.Pinheiros","Est.PteRemedios")

data_year_CO <- data.frame(Estaciones,CO = c(coredata(data_year[[1]][,1]),coredata(data_year[[2]][,1]),coredata(data_year[[3]][,1]),coredata(data_year[[4]][,1])))

data_year_PM10 <- data.frame(Estaciones,PM10 = c(coredata(data_year[[1]][,2]),coredata(data_year[[2]][,2]),coredata(data_year[[3]][,2]),coredata(data_year[[4]][,2])))

data_year_PM2.5 <- data.frame(Estaciones,PM2.5 = c(coredata(data_year[[1]][,3]),coredata(data_year[[2]][,3]),coredata(data_year[[3]][,3]),coredata(data_year[[4]][,3])))

data_year_NO2 <- data.frame(Estaciones,NO2 = c(coredata(data_year[[1]][,4]),coredata(data_year[[2]][,4]),coredata(data_year[[3]][,4]),coredata(data_year[[4]][,4])))

data_year_CO$Estaciones <- factor(data_year_CO$Estaciones, levels = c("Est.Congonhas","Est.Parque D.PedroII","Est.Pinheiros","Est.PteRemedios"))

data_year_PM10$Estaciones <- factor(data_year_PM10$Estaciones, levels = c("Est.Congonhas","Est.Parque D.PedroII","Est.Pinheiros","Est.PteRemedios"))

data_year_PM2.5$Estaciones <- factor(data_year_PM2.5$Estaciones, levels = c("Est.Congonhas","Est.Parque D.PedroII","Est.Pinheiros","Est.PteRemedios"))

data_year_NO2$Estaciones<- factor(data_year_NO2$Estaciones, levels = c("Est.Congonhas","Est.Parque D.PedroII","Est.Pinheiros","Est.PteRemedios"))

ggplot(data=data_year_CO, aes(x=Estaciones,y=CO,group=1,fill=Estaciones))+geom_bar(stat="identity", position="stack")+ylab(colnames(data_2019_Pinheiros)[3])+
  theme_light()+ggtitle("Contaminación anual: Contaminante CO \n2019 Estaciones Sao Paulo") +
  theme(plot.title = element_text(hjust = 0.5))+labs(caption="Fuente: Elaboración propia con datos de Cetesb",color="Estaciones:")+
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=0.1, 
                                   face="bold.italic", 
                                   color="gray48", 
                                   lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48", size=rel(1.3))) +
  theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+guides(fill = "none")


ggplot(data=data_year_CO, aes(x=Estaciones,y=CO,group=1,fill=Estaciones))+geom_bar(stat="identity", position="stack")+ylab(colnames(data_2019_Pinheiros)[3])+
  theme_light()+ggtitle("Contaminación anual: Contaminante CO \n2019 Estaciones Sao Paulo") +
  theme(plot.title = element_text(hjust = 0.5))+labs(caption="Fuente: Elaboración propia con datos de CESTESB",color="Estaciones:")+
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=0.1, 
                                   face="bold.italic", 
                                   color="gray48", 
                                   lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48", size=rel(1.3))) +
  theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+guides(fill = "none")+scale_fill_manual(values=brewer.pal(n = 4, name = "Paired"))

library(RColorBrewer)


ggplot(data=data_year_PM10, aes(x=Estaciones,y=PM10,group=1,fill=Estaciones))+geom_bar(stat="identity", position="stack")+ylab(colnames(data_2019_Pinheiros)[4])+
  theme_light()+ggtitle("Contaminación anual: Contaminante PM10 \n2019 Estaciones Sao Paulo") +
  theme(plot.title = element_text(hjust = 0.5))+labs(caption="Fuente: Elaboración propia con datos de CESTESB",color="Estaciones:")+
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=0.1, 
                                   face="bold.italic", 
                                   color="gray48", 
                                   lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48", size=rel(1.3))) +
  theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+guides(fill = "none")+scale_fill_manual(values=brewer.pal(n = 4, name = "Paired"))


ggplot(data=data_year_PM2.5, aes(x=Estaciones,y=PM2.5,group=1,fill=Estaciones))+geom_bar(stat="identity", position="stack")+ylab(colnames(data_2019_Pinheiros)[5])+
  theme_light()+ggtitle("Contaminación anual: Contaminante PM2.5 \n2019 Estaciones Sao Paulo") +
  theme(plot.title = element_text(hjust = 0.5))+labs(caption="Fuente: Elaboración propia con datos de CESTESB",color="Estaciones:")+
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=0.1, 
                                   face="bold.italic", 
                                   color="gray48", 
                                   lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48", size=rel(1.3))) +
  theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+guides(fill = "none")+scale_fill_manual(values=brewer.pal(n = 4, name = "Paired"))


ggplot(data=data_year_NO2, aes(x=Estaciones,y=NO2,group=1,fill=Estaciones))+geom_bar(stat="identity", position="stack")+ylab(colnames(data_2019_Pinheiros)[6])+
  theme_light()+ggtitle("Contaminación anual: Contaminante NO2 \n2019 Estaciones Sao Paulo") +
  theme(plot.title = element_text(hjust = 0.5))+labs(caption="Fuente: Elaboración propia con datos de CESTESB",color="Estaciones:")+
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=0.1, 
                                   face="bold.italic", 
                                   color="gray48", 
                                   lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48", size=rel(1.3))) +
  theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+guides(fill = "none")+scale_fill_manual(values=brewer.pal(n = 4, name = "Paired"))
