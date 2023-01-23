rm(list=ls())
graphics.off()
library(plyr)
library(readr)
library(stringr)
library(readxl)
library(dplyr)
library(tidyr)
library(plyr)
library("writexl")
#install.packages("purrr")
library(purrr)
library(ggplot2)


#####################################################################################################################
############################################ 2007 #####################################################################
########################################################################################################################

# Variación MENSUAL  ----------------------------------------------------------------


setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2007")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c("Abril", "Agosto", "Diciembre", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo", "Noviembre", "Septiembre")
VM2007<- ldply(myfiles, read_excel, sheet = "Anexo4", range = "A5:J19")
VM2007$Año<-2007
rm(myfiles )

VM2007$Fecha1<-"."
VM2007$Fecha1[VM2007$.id == "Enero"] <- "2007-01-30"
VM2007$Fecha1[VM2007$.id == "Febrero"] <- "2007-02-28"
VM2007$Fecha1[VM2007$.id == "Marzo"] <- "2007-03-30"
VM2007$Fecha1[VM2007$.id == "Abril"] <- "2007-04-30"
VM2007$Fecha1[VM2007$.id == "Mayo"] <- "2007-05-30"
VM2007$Fecha1[VM2007$.id == "Junio"] <- "2007-06-30"
VM2007$Fecha1[VM2007$.id == "Julio"] <- "2007-07-30"
VM2007$Fecha1[VM2007$.id == "Agosto"] <- "2007-08-30"
VM2007$Fecha1[VM2007$.id == "Septiembre"] <- "2007-09-30"
VM2007$Fecha1[VM2007$.id == "Octubre"] <- "2007-10-30"
VM2007$Fecha1[VM2007$.id == "Noviembre"] <- "2007-11-30"
VM2007$Fecha1[VM2007$.id == "Diciembre"] <- "2007-12-30"


#  Variación AÑO CORRIDO  ----------------------------------------------------------------

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2007")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c("Abril", "Agosto", "Diciembre", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo", "Noviembre", "Septiembre")
VAC2007<- ldply(myfiles, read_excel, sheet = "Anexo5", range = "A5:J19")
VAC2007$Año<-2007
rm(myfiles )

VAC2007$Fecha1<-"."
VAC2007$Fecha1[VAC2007$.id == "Enero"] <- "2007-01-30"
VAC2007$Fecha1[VAC2007$.id == "Febrero"] <- "2007-02-28"
VAC2007$Fecha1[VAC2007$.id == "Marzo"] <- "2007-03-30"
VAC2007$Fecha1[VAC2007$.id == "Abril"] <- "2007-04-30"
VAC2007$Fecha1[VAC2007$.id == "Mayo"] <- "2007-05-30"
VAC2007$Fecha1[VAC2007$.id == "Junio"] <- "2007-06-30"
VAC2007$Fecha1[VAC2007$.id == "Julio"] <- "2007-07-30"
VAC2007$Fecha1[VAC2007$.id == "Agosto"] <- "2007-08-30"
VAC2007$Fecha1[VAC2007$.id == "Septiembre"] <- "2007-09-30"
VAC2007$Fecha1[VAC2007$.id == "Octubre"] <- "2007-10-30"
VAC2007$Fecha1[VAC2007$.id == "Noviembre"] <- "2007-11-30"
VAC2007$Fecha1[VAC2007$.id == "Diciembre"] <- "2007-12-30"



# Variación DOCE MESES --------------------------------------------------------------

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2007")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c("Abril", "Agosto", "Diciembre", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo", "Noviembre", "Septiembre")
VAN2007<- ldply(myfiles, read_excel, sheet = "Anexo6", range = "A5:J19")
VAN2007$Año<-2007
rm(myfiles )

VAN2007$Fecha1<-"."
VAN2007$Fecha1[VAN2007$.id == "Enero"] <- "2007-01-30"
VAN2007$Fecha1[VAN2007$.id == "Febrero"] <- "2007-02-28"
VAN2007$Fecha1[VAN2007$.id == "Marzo"] <- "2007-03-30"
VAN2007$Fecha1[VAN2007$.id == "Abril"] <- "2007-04-30"
VAN2007$Fecha1[VAN2007$.id == "Mayo"] <- "2007-05-30"
VAN2007$Fecha1[VAN2007$.id == "Junio"] <- "2007-06-30"
VAN2007$Fecha1[VAN2007$.id == "Julio"] <- "2007-07-30"
VAN2007$Fecha1[VAN2007$.id == "Agosto"] <- "2007-08-30"
VAN2007$Fecha1[VAN2007$.id == "Septiembre"] <- "2007-09-30"
VAN2007$Fecha1[VAN2007$.id == "Octubre"] <- "2007-10-30"
VAN2007$Fecha1[VAN2007$.id == "Noviembre"] <- "2007-11-30"
VAN2007$Fecha1[VAN2007$.id == "Diciembre"] <- "2007-12-30"





#  Variación NIVEL INGRESOS ----------------------------------------------------------------


setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2007")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c("Abril", "Agosto", "Diciembre", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo", "Noviembre", "Septiembre")
NIngresos2007<- ldply(myfiles, read_excel, sheet = "Anexo3", range = "A6:Q20")
rm(myfiles )

colnames(NIngresos2007)<-c(".id", "orden" ,"NA1", "Tot_men", "Tot_año", "Tot_anu", "NA2", "Ing_men", "Ing_año","Ing_anu", "NA3", "Ime_men", "Ime_año", "Ime_anu", "NA4", "Ial_men", "Ial_año", "Ial_anu")


NIngresos2007<-NIngresos2007[-c(3,7,11, 15)]


library("reshape2")

NIngresos_final<-melt(NIngresos2007, id.vars= c(".id", "orden"),
                      variable.name = "Ningresos",
                      value.name= "Variacion")

unique(NIngresos_final$Ningresos)

NIngresos_final$Nivel<-substr(NIngresos_final$Ningresos, 1, 3)
NIngresos_final$Periodo<-substr(NIngresos_final$Ningresos, 5, 7)

unique(NIngresos_final$Nivel)
unique(NIngresos_final$Periodo)

NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Ing"] <- "Pobres"
NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Ime"] <- "Clase Media"
NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Ial"] <- "Ingresos altos"
NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Tot"] <- "Total"


NIngresos_final['Periodo'][NIngresos_final['Periodo'] == "anu"] <- "Anual"
NIngresos_final['Periodo'][NIngresos_final['Periodo'] == "año"] <- "Año corrido"
NIngresos_final['Periodo'][NIngresos_final['Periodo'] == "men"] <- "Mensual"

NIngresos_final<-rename(NIngresos_final, "Mes"=".id")
NIngresos2007 = subset(NIngresos_final, select = -c(Ningresos, orden) )


NIngresos2007$Fecha1<-"."
NIngresos2007$Fecha1[NIngresos2007$Mes == "Enero"] <- "2007-01-30"
NIngresos2007$Fecha1[NIngresos2007$Mes == "Febrero"] <- "2007-02-28"
NIngresos2007$Fecha1[NIngresos2007$Mes == "Marzo"] <- "2007-03-30"
NIngresos2007$Fecha1[NIngresos2007$Mes == "Abril"] <- "2007-04-30"
NIngresos2007$Fecha1[NIngresos2007$Mes == "Mayo"] <- "2007-05-30"
NIngresos2007$Fecha1[NIngresos2007$Mes == "Junio"] <- "2007-06-30"
NIngresos2007$Fecha1[NIngresos2007$Mes == "Julio"] <- "2007-07-30"
NIngresos2007$Fecha1[NIngresos2007$Mes == "Agosto"] <-"2007-08-30"
NIngresos2007$Fecha1[NIngresos2007$Mes == "Septiembre"] <- "2007-09-30"
NIngresos2007$Fecha1[NIngresos2007$Mes == "Octubre"] <- "2007-10-30"
NIngresos2007$Fecha1[NIngresos2007$Mes == "Noviembre"] <- "2007-11-30"
NIngresos2007$Fecha1[NIngresos2007$Mes == "Diciembre"] <- "2007-12-30"
NIngresos2007$Año<-2007
rm(NIngresos_final)