rm(list=ls())
graphics.off()
setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion")
library(readxl)

VAC<- read_excel("Inflacion.xlsx", "Variación añocor")

VAC$Año<-as.character(VAC$Año)
Nacional<-subset(VAC,  Ciudades== "Total IPC")


# IPC Nacional ------------------------------------------------------------

ggplot(Nacional, aes(Mes1, Var13, group=Año, color= Año)) +
  geom_line()+
  scale_x_continuous(breaks=seq(1,12,by=1),
                     labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(aes(label=Var13),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")

# Alimentos ---------------------------------------------------------------

ggplot(Nacional, aes(Mes1, Var1, group=Año, color= Año)) +
  geom_line()+
  scale_x_continuous(breaks=seq(1,12,by=1),
                     labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(aes(label=Var1),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")



