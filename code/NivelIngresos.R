rm(list=ls())
graphics.off()
setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion")
library(readxl)

IPC<- read_excel("Inflacion.xlsx", "Nivel de ingresos")

class(IPC$Fecha)
IPC$Fecha<- as.character(IPC$Fecha)
IPC$Fecha<- as.Date(IPC$Fecha)
       
IPC_anual<-subset(IPC,  Periodicidad== "Anual")
IPC_añocor<-subset(IPC,  Periodicidad== "Año corrido")
IPC_mensual<-subset(IPC,  Periodicidad== "Mensual")


library(lubridate)
Sys.setlocale("LC_TIME", "Spanish")




# Anual -------------------------------------------------------------------

#Año Calendario 

library(ggplot2)
ggplot(IPC_anual, aes(Fecha, Variacion, group= Nivel_ingresos, color= Nivel_ingresos))+
  geom_line()+ scale_x_date(date_labels="%b %Y", breaks = unique(IPC_anual$Fecha))+
  theme(axis.text.x = element_text(angle = 90, size =10))+ 
  annotate(geom="text",x=as.Date("2021-12-01"),y=12,label="La inflación para las personas pobres y\nvulnerables en Marzo 2022 fue de 10,46% y 10,35%")+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(data=subset(IPC_anual, Nivel_ingresos=="Pobres"),aes(label=Variacion),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")


# Mensual  ----------------------------------------------------------------

#Año Calendario 

ggplot(IPC_mensual, aes(Fecha, Variacion, group= Nivel_ingresos, color= Nivel_ingresos))+
  geom_line()+ scale_x_date(date_labels="%b %Y", breaks = unique(IPC_mensual$Fecha))+
  theme(axis.text.x = element_text(angle = 90, size =10))+ 
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(data=subset(IPC_mensual, Nivel_ingresos=="Pobres"),aes(label=Variacion),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")

#Mismo año 

ggplot(IPC_mensual, aes(Mes, Variacion, group=interaction(Nivel_ingresos, Año), color=interaction(Nivel_ingresos, Año)))+
  geom_line()+
  scale_x_continuous(breaks=seq(1,12,by=1),
                     labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  theme(axis.text.x = element_text(angle = 90, size =10))+
  geom_text(data=subset(IPC_mensual, Nivel_ingresos=="Pobres"),aes(label=Variacion),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")+
  ggtitle("") + xlab("Mes") + ylab("Variación %")




# Año corrido-acumulada ---------------------------------------------------

#Año Calendario 

ggplot(IPC_añocor, aes(Fecha, Variacion, group= Nivel_ingresos, color= Nivel_ingresos))+
  geom_line()+ scale_x_date(date_labels="%b %Y", breaks = unique(IPC_añocor$Fecha))+
  theme(axis.text.x = element_text(angle = 90, size =10))+ 
  annotate(geom="text",x=as.Date("2021-12-01"),y=12,label="La inflación para las personas pobres y\nvulnerables en Marzo 2022 fue de 10,46% y 10,35%")+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(data=subset(IPC_añocor, Nivel_ingresos=="Pobres"),aes(label=Variacion),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")

#Mismo año 

ggplot(IPC_añocor, aes(Mes, Variacion, group=interaction(Nivel_ingresos, Año), color=interaction(Nivel_ingresos, Año)))+
  geom_line()+
  scale_x_continuous(breaks=seq(1,12,by=1),
                     labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  theme(axis.text.x = element_text(angle = 90, size =10))+
  geom_text(data=subset(IPC_añocor, Nivel_ingresos=="Pobres"),aes(label=Variacion),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")+
  ggtitle("") + xlab("Mes") + ylab("Variación %")
  



