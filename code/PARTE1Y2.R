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
library("ggpubr")
library(lubridate)



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

#####################################################################################################################
############################################ 2017 #####################################################################
########################################################################################################################

# Variación MENSUAL  ----------------------------------------------------------------

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2017")
Ene<- read_excel("anexo_ipc_ene17.xls", "Anexo4", range = "A4:K29")
Ene<-mutate(Ene, 
            .id="Enero")
Abr<- read_excel("anexo_ipc_abr17.xls", "Anexo4", range = "A4:K29")
Abr<-mutate(Abr, 
            .id="Abril")
Mar<- read_excel("anexo_ipc_mar17.xls", "Anexo4", range = "A4:K29")
Mar<-mutate(Mar, 
            .id="Marzo")
Feb<- read_excel("anexo_ipc_feb17.xls", "Anexo4", range = "A4:K29")
Feb<-mutate(Feb, 
            .id="Febrero")
May<- read_excel("anexo_ipc_may17.xls", "Anexo4", range = "A4:K29")
May<-mutate(May, 
            .id="Mayo")



Ago<- read_excel("anexo_ipc_ago17.xls", "Anexo4", range = "A6:K31")
Ago<-mutate(Ago, 
            .id="Agosto")
Jul<- read_excel("anexo_ipc_jul17.xls", "Anexo4", range = "A6:K31")
Jul<-mutate(Jul, 
            .id="Julio")
Jun<- read_excel("anexo_ipc_jun17.xls", "Anexo4", range = "A6:K31")
Jun<-mutate(Jun, 
            .id="Junio")
Nov<- read_excel("anexo_ipc_nov17.xls", "Anexo4", range = "A6:K31")
Nov<-mutate(Nov, 
            .id="Noviembre")
Oct<- read_excel("anexo_ipc_oct17.xls", "Anexo4", range = "A6:K31")
Oct<-mutate(Oct, 
            .id="Octubre")
Sep<- read_excel("anexo_ipc_sep17.xls", "Anexo4", range = "A6:K31")
Sep<-mutate(Sep, 
            .id="Septiembre")


Dic<- read_excel("anexo_ipc_dic17.xls", "Anexo4", range = "A8:K33")
Dic<-mutate(Dic, 
            .id="Diciembre")
VM2017<-rbind(Ene, Feb, Mar, Abr, May, Jun, Jul, Ago, Sep, Oct, Nov, Dic)


VM2017$Fecha1<-"."
VM2017$Fecha1[VM2017$.id == "Enero"] <- "2017-01-30"
VM2017$Fecha1[VM2017$.id == "Febrero"] <- "2017-02-28"
VM2017$Fecha1[VM2017$.id == "Marzo"] <- "2017-03-30"
VM2017$Fecha1[VM2017$.id == "Abril"] <- "2017-04-30"
VM2017$Fecha1[VM2017$.id == "Mayo"] <- "2017-05-30"
VM2017$Fecha1[VM2017$.id == "Junio"] <- "2017-06-30"
VM2017$Fecha1[VM2017$.id == "Julio"] <- "2017-07-30"
VM2017$Fecha1[VM2017$.id == "Agosto"] <- "2017-08-30"
VM2017$Fecha1[VM2017$.id == "Septiembre"] <- "2017-09-30"
VM2017$Fecha1[VM2017$.id == "Octubre"] <- "2017-10-30"
VM2017$Fecha1[VM2017$.id == "Noviembre"] <- "2017-11-30"
VM2017$Fecha1[VM2017$.id == "Diciembre"] <- "2017-12-30"

VM2017$Año<-2017
rm(Ene, Feb, Mar, Abr, May, Jun, Jul, Ago, Sep, Oct, Nov, Dic)




#  Variación AÑO CORRIDO  ----------------------------------------------------------------

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2017")
Ene<- read_excel("anexo_ipc_ene17.xls", "Anexo5", range = "A5:K30")
Ene<-mutate(Ene, 
            .id="Enero")
Abr<- read_excel("anexo_ipc_abr17.xls", "Anexo5", range = "A4:K29")
Abr<-mutate(Abr, 
            .id="Abril")
Mar<- read_excel("anexo_ipc_mar17.xls", "Anexo5", range = "A4:K29")
Mar<-mutate(Mar, 
            .id="Marzo")
Feb<- read_excel("anexo_ipc_feb17.xls", "Anexo5", range = "A4:K29")
Feb<-mutate(Feb, 
            .id="Febrero")
May<- read_excel("anexo_ipc_may17.xls", "Anexo5", range = "A4:K29")
May<-mutate(May, 
            .id="Mayo")

Ago<- read_excel("anexo_ipc_ago17.xls", "Anexo5", range = "A6:K31")
Ago<-mutate(Ago, 
            .id="Agosto")
Jul<- read_excel("anexo_ipc_jul17.xls", "Anexo5", range = "A6:K31")
Jul<-mutate(Jul, 
            .id="Julio")
Jun<- read_excel("anexo_ipc_jun17.xls", "Anexo5", range = "A6:K31")
Jun<-mutate(Jun, 
            .id="Junio")
Nov<- read_excel("anexo_ipc_nov17.xls", "Anexo5", range = "A6:K31")
Nov<-mutate(Nov, 
            .id="Noviembre")
Oct<- read_excel("anexo_ipc_oct17.xls", "Anexo5", range = "A6:K31")
Oct<-mutate(Oct, 
            .id="Octubre")
Sep<- read_excel("anexo_ipc_sep17.xls", "Anexo5", range = "A6:K31")
Sep<-mutate(Sep, 
            .id="Septiembre")

Dic<- read_excel("anexo_ipc_dic17.xls", "Anexo5", range = "A8:K33")
Dic<-mutate(Dic, 
            .id="Diciembre")
VAC2017<-rbind(Ene, Feb, Mar, Abr, May, Jun, Jul, Ago, Sep, Oct, Nov, Dic)


VAC2017$Fecha1<-"."
VAC2017$Fecha1[VAC2017$.id == "Enero"] <- "2017-01-30"
VAC2017$Fecha1[VAC2017$.id == "Febrero"] <- "2017-02-28"
VAC2017$Fecha1[VAC2017$.id == "Marzo"] <- "2017-03-30"
VAC2017$Fecha1[VAC2017$.id == "Abril"] <- "2017-04-30"
VAC2017$Fecha1[VAC2017$.id == "Mayo"] <- "2017-05-30"
VAC2017$Fecha1[VAC2017$.id == "Junio"] <- "2017-06-30"
VAC2017$Fecha1[VAC2017$.id == "Julio"] <- "2017-07-30"
VAC2017$Fecha1[VAC2017$.id == "Agosto"] <- "2017-08-30"
VAC2017$Fecha1[VAC2017$.id == "Septiembre"] <- "2017-09-30"
VAC2017$Fecha1[VAC2017$.id == "Octubre"] <- "2017-10-30"
VAC2017$Fecha1[VAC2017$.id == "Noviembre"] <- "2017-11-30"
VAC2017$Fecha1[VAC2017$.id == "Diciembre"] <- "2017-12-30"

VAC2017$Año<-2017
rm( Ene, Feb, Mar, Abr, May, Jun, Jul, Ago, Sep, Oct, Nov, Dic)

# Variación DOCE MESES --------------------------------------------------------------


setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2017")
Ene<- read_excel("anexo_ipc_ene17.xls", "Anexo5", range = "A5:K30")
Ene<-mutate(Ene, 
            .id="Enero")
Abr<- read_excel("anexo_ipc_abr17.xls", "Anexo6", range = "A5:K30")
Abr<-mutate(Abr, 
            .id="Abril")
Mar<- read_excel("anexo_ipc_mar17.xls", "Anexo6", range = "A5:K30")
Mar<-mutate(Mar, 
            .id="Marzo")
Feb<- read_excel("anexo_ipc_feb17.xls", "Anexo6", range = "A5:K30")
Feb<-mutate(Feb, 
            .id="Febrero")
May<- read_excel("anexo_ipc_may17.xls", "Anexo6", range =  "A5:K30")
May<-mutate(May, 
            .id="Mayo")

Ago<- read_excel("anexo_ipc_ago17.xls", "Anexo6", range = "A6:K31")
Ago<-mutate(Ago, 
            .id="Agosto")
Jul<- read_excel("anexo_ipc_jul17.xls", "Anexo6", range = "A6:K31")
Jul<-mutate(Jul, 
            .id="Julio")
Jun<- read_excel("anexo_ipc_jun17.xls", "Anexo6", range = "A6:K31")
Jun<-mutate(Jun, 
            .id="Junio")
Nov<- read_excel("anexo_ipc_nov17.xls", "Anexo6", range = "A6:K31")
Nov<-mutate(Nov, 
            .id="Noviembre")
Oct<- read_excel("anexo_ipc_oct17.xls", "Anexo6", range = "A6:K31")
Oct<-mutate(Oct, 
            .id="Octubre")
Sep<- read_excel("anexo_ipc_sep17.xls", "Anexo6", range = "A6:K31")
Sep<-mutate(Sep, 
            .id="Septiembre")

Dic<- read_excel("anexo_ipc_dic17.xls", "Anexo5", range = "A8:K33")
Dic<-mutate(Dic, 
            .id="Diciembre")

VAN2017<-rbind( Ene, Feb, Mar, Abr, May, Jun, Jul, Ago, Sep, Oct, Nov, Dic)

VAN2017$Fecha1<-"."
VAN2017$Fecha1[VAN2017$.id == "Enero"] <- "2017-01-30"
VAN2017$Fecha1[VAN2017$.id == "Febrero"] <- "2017-02-28"
VAN2017$Fecha1[VAN2017$.id == "Marzo"] <- "2017-03-30"
VAN2017$Fecha1[VAN2017$.id == "Abril"] <- "2017-04-30"
VAN2017$Fecha1[VAN2017$.id == "Mayo"] <- "2017-05-30"
VAN2017$Fecha1[VAN2017$.id == "Junio"] <- "2017-06-30"
VAN2017$Fecha1[VAN2017$.id == "Julio"] <- "2017-07-30"
VAN2017$Fecha1[VAN2017$.id == "Agosto"] <- "2017-08-30"
VAN2017$Fecha1[VAN2017$.id == "Septiembre"] <- "2017-09-30"
VAN2017$Fecha1[VAN2017$.id == "Octubre"] <- "2017-10-30"
VAN2017$Fecha1[VAN2017$.id == "Noviembre"] <- "2017-11-30"
VAN2017$Fecha1[VAN2017$.id == "Diciembre"] <- "2017-12-30"
VAN2017$Año<-2017
rm( Ene, Feb, Mar, Abr, May, Jun, Jul, Ago, Sep, Oct, Nov, Dic)

#  Variación NIVEL INGRESOS ----------------------------------------------------------------



setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2017")

Abr<- read_excel("anexo_ipc_abr17.xls", "Anexo3", range = "A5:P30")
Abr<-mutate(Abr, 
            .id="Abril")
colnames(Abr)<-c( "orden" , "Tot_men", "Tot_año", "Tot_anu", "NA1", "Ing_men", "Ing_año","Ing_anu", "NA2", "Ime_men", "Ime_año", "Ime_anu", "NA3", "Ial_men", "Ial_año", "Ial_anu", ".id")

Mar<- read_excel("anexo_ipc_mar17.xls", "Anexo3", range = "A5:P30")
Mar<-mutate(Mar, 
            .id="Marzo")
colnames(Mar)<-c( "orden" , "Tot_men", "Tot_año", "Tot_anu", "NA1", "Ing_men", "Ing_año","Ing_anu", "NA2", "Ime_men", "Ime_año", "Ime_anu", "NA3", "Ial_men", "Ial_año", "Ial_anu", ".id")

Ene<- read_excel("anexo_ipc_ene17.xls", "Anexo3", range = "A5:P30")
Ene<-mutate(Ene, 
            .id="Enero")
colnames(Ene)<-c( "orden" , "Tot_men", "Tot_año", "Tot_anu", "NA1", "Ing_men", "Ing_año","Ing_anu", "NA2", "Ime_men", "Ime_año", "Ime_anu", "NA3", "Ial_men", "Ial_año", "Ial_anu", ".id")

Feb<- read_excel("anexo_ipc_feb17.xls", "Anexo3", range = "A5:P30")
Feb<-mutate(Feb, 
            .id="Febrero")
colnames(Feb)<-c( "orden" , "Tot_men", "Tot_año", "Tot_anu", "NA1", "Ing_men", "Ing_año","Ing_anu", "NA2", "Ime_men", "Ime_año", "Ime_anu", "NA3", "Ial_men", "Ial_año", "Ial_anu", ".id")

May<- read_excel("anexo_ipc_may17.xls", "Anexo3", range =  "A5:P30")
May<-mutate(May, 
            .id="Mayo")
colnames(May)<-c( "orden" , "Tot_men", "Tot_año", "Tot_anu", "NA1", "Ing_men", "Ing_año","Ing_anu", "NA2", "Ime_men", "Ime_año", "Ime_anu", "NA3", "Ial_men", "Ial_año", "Ial_anu", ".id")


Ago<- read_excel("anexo_ipc_ago17.xls", "Anexo3", range = "A7:P32")
Ago<-mutate(Ago, 
            .id="Agosto")
colnames(Ago)<-c( "orden" , "Tot_men", "Tot_año", "Tot_anu", "NA1", "Ing_men", "Ing_año","Ing_anu", "NA2", "Ime_men", "Ime_año", "Ime_anu", "NA3", "Ial_men", "Ial_año", "Ial_anu", ".id")

Jul<- read_excel("anexo_ipc_jul17.xls", "Anexo3", range = "A7:P32")
Jul<-mutate(Jul, 
            .id="Julio")
colnames(Jul)<-c( "orden" , "Tot_men", "Tot_año", "Tot_anu", "NA1", "Ing_men", "Ing_año","Ing_anu", "NA2", "Ime_men", "Ime_año", "Ime_anu", "NA3", "Ial_men", "Ial_año", "Ial_anu", ".id")

Jun<- read_excel("anexo_ipc_jun17.xls", "Anexo3", range = "A7:P32")
Jun<-mutate(Jun, 
            .id="Junio")
colnames(Jun)<-c( "orden" , "Tot_men", "Tot_año", "Tot_anu", "NA1", "Ing_men", "Ing_año","Ing_anu", "NA2", "Ime_men", "Ime_año", "Ime_anu", "NA3", "Ial_men", "Ial_año", "Ial_anu", ".id")

Nov<- read_excel("anexo_ipc_nov17.xls", "Anexo3", range = "A7:P32")
Nov<-mutate(Nov, 
            .id="Noviembre")
colnames(Nov)<-c( "orden" , "Tot_men", "Tot_año", "Tot_anu", "NA1", "Ing_men", "Ing_año","Ing_anu", "NA2", "Ime_men", "Ime_año", "Ime_anu", "NA3", "Ial_men", "Ial_año", "Ial_anu", ".id")

Oct<- read_excel("anexo_ipc_oct17.xls", "Anexo3", range = "A7:P32")
Oct<-mutate(Oct, 
            .id="Octubre")
colnames(Oct)<-c( "orden" , "Tot_men", "Tot_año", "Tot_anu", "NA1", "Ing_men", "Ing_año","Ing_anu", "NA2", "Ime_men", "Ime_año", "Ime_anu", "NA3", "Ial_men", "Ial_año", "Ial_anu", ".id")

Sep<- read_excel("anexo_ipc_sep17.xls", "Anexo3", range = "A7:P32")
Sep<-mutate(Sep, 
            .id="Septiembre")
colnames(Sep)<-c( "orden" , "Tot_men", "Tot_año", "Tot_anu", "NA1", "Ing_men", "Ing_año","Ing_anu", "NA2", "Ime_men", "Ime_año", "Ime_anu", "NA3", "Ial_men", "Ial_año", "Ial_anu", ".id")


Dic<- read_excel("anexo_ipc_dic17.xls", "Anexo3", range = "A9:P34")
Dic<-mutate(Dic, 
            .id="Diciembre")
colnames(Dic)<-c( "orden" , "Tot_men", "Tot_año", "Tot_anu", "NA1", "Ing_men", "Ing_año","Ing_anu", "NA2", "Ime_men", "Ime_año", "Ime_anu", "NA3", "Ial_men", "Ial_año", "Ial_anu", ".id")




NIngresos2017<-rbind( Ene, Feb, Mar, Abr, May, Jun, Jul, Ago, Sep, Oct, Nov, Dic)
rm( Ene, Feb, Mar, Abr, May, Jun, Jul, Ago, Sep, Oct, Nov, Dic)
NIngresos2017<-NIngresos2017[-c(5,9, 13)]


library("reshape2")

NIngresos_final<-melt(NIngresos2017, id.vars= c(".id", "orden"),
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
NIngresos2017 = subset(NIngresos_final, select = -c(Ningresos, orden) )


NIngresos2017$Fecha1<-"."
NIngresos2017$Fecha1[NIngresos2017$Mes == "Enero"] <- "2017-01-30"
NIngresos2017$Fecha1[NIngresos2017$Mes == "Febrero"] <- "2017-02-28"
NIngresos2017$Fecha1[NIngresos2017$Mes == "Marzo"] <- "2017-03-30"
NIngresos2017$Fecha1[NIngresos2017$Mes == "Abril"] <- "2017-04-30"
NIngresos2017$Fecha1[NIngresos2017$Mes == "Mayo"] <- "2017-05-30"
NIngresos2017$Fecha1[NIngresos2017$Mes == "Junio"] <- "2017-06-30"
NIngresos2017$Fecha1[NIngresos2017$Mes == "Julio"] <- "2017-07-30"
NIngresos2017$Fecha1[NIngresos2017$Mes == "Agosto"] <-"2017-08-30"
NIngresos2017$Fecha1[NIngresos2017$Mes == "Septiembre"] <- "2017-09-30"
NIngresos2017$Fecha1[NIngresos2017$Mes == "Octubre"] <- "2017-10-30"
NIngresos2017$Fecha1[NIngresos2017$Mes == "Noviembre"] <- "2017-11-30"
NIngresos2017$Fecha1[NIngresos2017$Mes == "Diciembre"] <- "2017-12-30"
NIngresos2017$Año<-2017
rm(NIngresos_final)

#####################################################################################################################
############################################ 2018 #####################################################################
########################################################################################################################

# Variación MENSUAL  ----------------------------------------------------------------

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2018")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c("Abril", "Agosto", "Diciembre", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo", "Noviembre", "Octubre", "Septiembre")
VM2018<- ldply(myfiles, read_excel, sheet = "Anexo4", range = "A8:K33")
VM2018$Año<-2018

VM2018$Fecha1<-"."
VM2018$Fecha1[VM2018$.id == "Enero"] <- "2018-01-30"
VM2018$Fecha1[VM2018$.id == "Febrero"] <- "2018-02-28"
VM2018$Fecha1[VM2018$.id == "Marzo"] <- "2018-03-30"
VM2018$Fecha1[VM2018$.id == "Abril"] <- "2018-04-30"
VM2018$Fecha1[VM2018$.id == "Mayo"] <- "2018-05-30"
VM2018$Fecha1[VM2018$.id == "Junio"] <- "2018-06-30"
VM2018$Fecha1[VM2018$.id == "Julio"] <- "2018-07-30"
VM2018$Fecha1[VM2018$.id == "Agosto"] <- "2018-08-30"
VM2018$Fecha1[VM2018$.id == "Septiembre"] <- "2018-09-30"
VM2018$Fecha1[VM2018$.id == "Octubre"] <- "2018-10-30"
VM2018$Fecha1[VM2018$.id == "Noviembre"] <- "2018-11-30"
VM2018$Fecha1[VM2018$.id == "Diciembre"] <- "2018-12-30"
rm(myfiles )


#  Variación AÑO CORRIDO  ----------------------------------------------------------------

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2018")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c("Abril", "Agosto", "Diciembre", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo", "Noviembre", "Octubre", "Septiembre")
VAC2018<- ldply(myfiles, read_excel, sheet = "Anexo5", range = "A8:K33")
VAC2018$Año<-2018

VAC2018$Fecha1<-"."
VAC2018$Fecha1[VAC2018$.id == "Enero"] <- "2018-01-30"
VAC2018$Fecha1[VAC2018$.id == "Febrero"] <- "2018-02-28"
VAC2018$Fecha1[VAC2018$.id == "Marzo"] <- "2018-03-30"
VAC2018$Fecha1[VAC2018$.id == "Abril"] <- "2018-04-30"
VAC2018$Fecha1[VAC2018$.id == "Mayo"] <- "2018-05-30"
VAC2018$Fecha1[VAC2018$.id == "Junio"] <- "2018-06-30"
VAC2018$Fecha1[VAC2018$.id == "Julio"] <- "2018-07-30"
VAC2018$Fecha1[VAC2018$.id == "Agosto"] <- "2018-08-30"
VAC2018$Fecha1[VAC2018$.id == "Septiembre"] <- "2018-09-30"
VAC2018$Fecha1[VAC2018$.id == "Octubre"] <- "2018-10-30"
VAC2018$Fecha1[VAC2018$.id == "Noviembre"] <- "2018-11-30"
VAC2018$Fecha1[VAC2018$.id == "Diciembre"] <- "2018-12-30"
rm(myfiles )


# Variación ANUAL-DOCE MESES  --------------------------------------------------------

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2018")

Ene<- read_excel("anexo_ipc_ene18.xls", "Anexo5", range = "A8:K33")
Ene<-mutate(Ene, 
            .id="Enero")
Dic<- read_excel("anexo_ipc_dic18.xls", "Anexo5", range = "A8:K33")
Dic<-mutate(Dic, 
            .id="Diciembre")


Abr<- read_excel("anexo_ipc_abr18.xls", "Anexo6", range = "A8:K33")
Abr<-mutate(Abr, 
            .id="Abril")
Mar<- read_excel("anexo_ipc_mar18.xls", "Anexo6", range = "A8:K33")
Mar<-mutate(Mar, 
            .id="Marzo")
Feb<- read_excel("anexo_ipc_feb18.xls", "Anexo6", range = "A8:K33")
Feb<-mutate(Feb, 
            .id="Febrero")
May<- read_excel("anexo_ipc_may18.xls", "Anexo6",range = "A8:K33")
May<-mutate(May, 
            .id="Mayo")

Ago<- read_excel("anexo_ipc_ago18.xls", "Anexo6", range = "A8:K33")
Ago<-mutate(Ago, 
            .id="Agosto")
Jul<- read_excel("anexo_ipc_jul18.xls", "Anexo6",range = "A8:K33")
Jul<-mutate(Jul, 
            .id="Julio")
Jun<- read_excel("anexo_ipc_jun18.xls", "Anexo6", range = "A8:K33")
Jun<-mutate(Jun, 
            .id="Junio")
Nov<- read_excel("anexo_ipc_nov18.xls", "Anexo6", range = "A8:K33")
Nov<-mutate(Nov, 
            .id="Noviembre")
Oct<- read_excel("anexo_ipc_oct18.xls", "Anexo6", range = "A8:K33")
Oct<-mutate(Oct, 
            .id="Octubre")
Sep<- read_excel("anexo_ipc_sep18.xls", "Anexo6",range = "A8:K33")
Sep<-mutate(Sep, 
            .id="Septiembre")
VAN2018<-rbind(Ene, Feb, Mar, Abr, May, Jun, Jul, Ago, Sep, Oct, Nov, Dic)


VAN2018$Fecha1<-"."
VAN2018$Fecha1[VAN2018$.id == "Enero"] <- "2018-01-30"
VAN2018$Fecha1[VAN2018$.id == "Febrero"] <- "2018-02-28"
VAN2018$Fecha1[VAN2018$.id == "Marzo"] <- "2018-03-30"
VAN2018$Fecha1[VAN2018$.id == "Abril"] <- "2018-04-30"
VAN2018$Fecha1[VAN2018$.id == "Mayo"] <- "2018-05-30"
VAN2018$Fecha1[VAN2018$.id == "Junio"] <- "2018-06-30"
VAN2018$Fecha1[VAN2018$.id == "Julio"] <- "2018-07-30"
VAN2018$Fecha1[VAN2018$.id == "Agosto"] <- "2018-08-30"
VAN2018$Fecha1[VAN2018$.id == "Septiembre"] <- "2018-09-30"
VAN2018$Fecha1[VAN2018$.id == "Octubre"] <- "2018-10-30"
VAN2018$Fecha1[VAN2018$.id == "Noviembre"] <- "2018-11-30"
VAN2018$Fecha1[VAN2018$.id == "Diciembre"] <- "2018-12-30"
VAN2018$Año<-2018
rm( Ene, Feb, Mar, Abr, May, Jun, Jul, Ago, Sep, Oct, Nov, Dic)




#  Variación NIVEL INGRESOS   ----------------------------------------------------------------

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2018")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c("Abril", "Agosto", "Diciembre", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo", "Noviembre", "Octubre", "Septiembre")
NIngresos<- ldply(myfiles, read_excel, sheet = "Anexo3", range = "A9:P34")

#ING es para ingresos BAJOS
#IME es para ingresos MEDIOS 
#IAL es para ingresos ALTOS


NIngresos<-NIngresos[-c(6,10, 14,18,20,22,24)]
colnames(NIngresos)<-c(".id", "orden" , "Tot_men", "Tot_año", "Tot_anu", "Ing_men", "Ing_año","Ing_anu", "Ime_men", "Ime_año", "Ime_anu", "Ial_men", "Ial_año", "Ial_anu", "Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8", "Var9", "Var10", "Var11", "Var12")

#Diciembre: Ano corrido y anual son iguales 
#En la base completa no tiene datos para ninguno de los dos 
#Var1 corresponde a sus datos 
NIngresos$Tot_año<-ifelse(is.na(NIngresos$Tot_año), NIngresos$Var1, NIngresos$Tot_año)
NIngresos$Tot_anu<-ifelse(is.na(NIngresos$Tot_anu), NIngresos$Var1, NIngresos$Tot_anu)

NIngresos$Ing_año<-ifelse(is.na(NIngresos$Ing_año), NIngresos$Var2, NIngresos$Ing_año)
NIngresos$Ing_anu<-ifelse(is.na(NIngresos$Ing_anu), NIngresos$Var2, NIngresos$Ing_anu)

NIngresos$Ime_año<-ifelse(is.na(NIngresos$Ime_año), NIngresos$Var3, NIngresos$Ime_año)
NIngresos$Ime_anu<-ifelse(is.na(NIngresos$Ime_anu), NIngresos$Var3, NIngresos$Ime_anu)

NIngresos$Ial_año<-ifelse(is.na(NIngresos$Ial_año), NIngresos$Var4, NIngresos$Ial_año)
NIngresos$Ial_anu<-ifelse(is.na(NIngresos$Ial_anu), NIngresos$Var4, NIngresos$Ial_anu)


#Junio: Falta la información año corrido, que aparece como SEMESTRAL en la base faltante

NIngresos$Tot_año<-ifelse(is.na(NIngresos$Tot_año), NIngresos$Var5, NIngresos$Tot_año)
NIngresos$Ing_año<-ifelse(is.na(NIngresos$Ing_año), NIngresos$Var6, NIngresos$Ing_año)
NIngresos$Ime_año<-ifelse(is.na(NIngresos$Ime_año), NIngresos$Var7, NIngresos$Ime_año)
NIngresos$Ial_año<-ifelse(is.na(NIngresos$Ial_año), NIngresos$Var8, NIngresos$Ial_año)


#Marzo: Falta la información año corrido, que aparece como PRIMER TRIMESTRE en la base faltante

NIngresos$Tot_año<-ifelse(is.na(NIngresos$Tot_año), NIngresos$Var9, NIngresos$Tot_año)
NIngresos$Ing_año<-ifelse(is.na(NIngresos$Ing_año), NIngresos$Var10, NIngresos$Ing_año)
NIngresos$Ime_año<-ifelse(is.na(NIngresos$Ime_año), NIngresos$Var11, NIngresos$Ime_año)
NIngresos$Ial_año<-ifelse(is.na(NIngresos$Ial_año), NIngresos$Var12, NIngresos$Ial_año)


NIngresos2018<-NIngresos[c(1:14)]

library("reshape2")

NIngresos_final<-melt(NIngresos2018, id.vars= c(".id", "orden"),
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
NIngresos2018 = subset(NIngresos_final, select = -c(Ningresos, orden) )


NIngresos2018$Fecha1<-"."
NIngresos2018$Fecha1[NIngresos2018$Mes == "Enero"] <- "2018-01-30"
NIngresos2018$Fecha1[NIngresos2018$Mes == "Febrero"] <- "2018-02-28"
NIngresos2018$Fecha1[NIngresos2018$Mes == "Marzo"] <- "2018-03-30"
NIngresos2018$Fecha1[NIngresos2018$Mes == "Abril"] <- "2018-04-30"
NIngresos2018$Fecha1[NIngresos2018$Mes == "Mayo"] <- "2018-05-30"
NIngresos2018$Fecha1[NIngresos2018$Mes == "Junio"] <- "2018-06-30"
NIngresos2018$Fecha1[NIngresos2018$Mes == "Julio"] <- "2018-07-30"
NIngresos2018$Fecha1[NIngresos2018$Mes == "Agosto"] <-"2018-08-30"
NIngresos2018$Fecha1[NIngresos2018$Mes == "Septiembre"] <- "2018-09-30"
NIngresos2018$Fecha1[NIngresos2018$Mes == "Octubre"] <- "2018-10-30"
NIngresos2018$Fecha1[NIngresos2018$Mes == "Noviembre"] <- "2018-11-30"
NIngresos2018$Fecha1[NIngresos2018$Mes == "Diciembre"] <- "2018-12-30"

NIngresos2018$Año<-2018
rm(NIngresos, myfiles, NIngresos_final)

################################################################################################################
################################################ PEGUE 1  ##########################################################
##############################################################################################################

VAC2007<-rename(VAC2007, "Otros gastos"="Gastos varios")
VAN2007<-rename(VAN2007, "Otros gastos"="Gastos varios")
VM2007<-rename(VM2007, "Otros gastos"="Gastos varios")

VAC2007<-rename(VAC2007, "Diversión"="Cultura")
VAN2007<-rename(VAN2007, "Diversión"="Cultura")
VM2007<-rename(VM2007, "Diversión"="Cultura")

VAC2007$Comunicaciones<-"." 
VAN2007$Comunicaciones<-"." 
VM2007$Comunicaciones<-"." 


VM_1<-rbind( VM2017, VM2018, VM2007)
VAC_1<-rbind( VAC2017, VAC2018, VAC2007)
VAN_1<-rbind(VAN2017, VAN2018, VAN2007)
NIngresos_1<-rbind( NIngresos2017,NIngresos2018, NIngresos2007)
rm(VM2017, VM2018, VAC2017, VAC2018, NIngresos2017, NIngresos2018, VAN2017, VAN2018, VAC2007, VAN2007, VM2007, NIngresos2007)
VM_1$Año<-as.character(VM_1$Año)
VAC_1$Año<-as.character(VAC_1$Año)
NIngresos_1$Año<-as.character(NIngresos_1$Año)


################################################################################################################
################################################2019 ##########################################################
##############################################################################################################
# Año 2019 Variación MENSUAL  ----------------------------------------------------------------

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2019")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c("Abril", "Agosto", "Diciembre", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo", "Noviembre", "Octubre", "Septiembre")
VM2019<- ldply(myfiles, read_excel, sheet = "4", range = "A7:N31")
VM2019$Año<-2019

VM2019$Fecha1<-"."
VM2019$Fecha1[VM2019$.id == "Enero"] <- "2019-01-30"
VM2019$Fecha1[VM2019$.id == "Febrero"] <- "2019-02-28"
VM2019$Fecha1[VM2019$.id == "Marzo"] <- "2019-03-30"
VM2019$Fecha1[VM2019$.id == "Abril"] <- "2019-04-30"
VM2019$Fecha1[VM2019$.id == "Mayo"] <- "2019-05-30"
VM2019$Fecha1[VM2019$.id == "Junio"] <- "2019-06-30"
VM2019$Fecha1[VM2019$.id == "Julio"] <- "2019-07-30"
VM2019$Fecha1[VM2019$.id == "Agosto"] <- "2019-08-30"
VM2019$Fecha1[VM2019$.id == "Septiembre"] <- "2019-09-30"
VM2019$Fecha1[VM2019$.id == "Octubre"] <- "2019-10-30"
VM2019$Fecha1[VM2019$.id == "Noviembre"] <- "2019-11-30"
VM2019$Fecha1[VM2019$.id == "Diciembre"] <- "2019-12-30"
rm(myfiles )


# Año 2019 Variación AÑO CORRIDO  ----------------------------------------------------------------

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2019")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c("Abril", "Agosto", "Diciembre", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo", "Noviembre", "Octubre", "Septiembre")
VAC2019<- ldply(myfiles, read_excel, sheet = "5", range = "A7:N31")
VAC2019$Año<-2019

VAC2019$Fecha1<-"."
VAC2019$Fecha1[VAC2019$.id == "Enero"] <- "2019-01-30"
VAC2019$Fecha1[VAC2019$.id == "Febrero"] <- "2019-02-28"
VAC2019$Fecha1[VAC2019$.id == "Marzo"] <- "2019-03-30"
VAC2019$Fecha1[VAC2019$.id == "Abril"] <- "2019-04-30"
VAC2019$Fecha1[VAC2019$.id == "Mayo"] <- "2019-05-30"
VAC2019$Fecha1[VAC2019$.id == "Junio"] <- "2019-06-30"
VAC2019$Fecha1[VAC2019$.id == "Julio"] <- "2019-07-30"
VAC2019$Fecha1[VAC2019$.id == "Agosto"] <- "2019-08-30"
VAC2019$Fecha1[VAC2019$.id == "Septiembre"] <- "2019-09-30"
VAC2019$Fecha1[VAC2019$.id == "Octubre"] <- "2019-10-30"
VAC2019$Fecha1[VAC2019$.id == "Noviembre"] <- "2019-11-30"
VAC2019$Fecha1[VAC2019$.id == "Diciembre"] <- "2019-12-30"
rm(myfiles)



# Año 2019 Variación NIVEL INGRESOS  ----------------------------------------------------------------


setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2019")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c("Abril", "Agosto", "Diciembre", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo", "Noviembre", "Octubre", "Septiembre")
NIngresos<- ldply(myfiles, read_excel, sheet = "3", range = "A7:P9")

NIngresos<-rename(NIngresos, Pob_men=Pobres)
NIngresos<-rename(NIngresos, Pob_año="...3")
NIngresos<-rename(NIngresos, Pob_anu="...4")
NIngresos<-rename(NIngresos, Vul_men=Vulnerables)
NIngresos<-rename(NIngresos, Vul_año="...6")
NIngresos<-rename(NIngresos, Vul_anu="...7")
NIngresos<-rename(NIngresos, Cla_men="Clase media")
NIngresos<-rename(NIngresos, Cla_año="...9")
NIngresos<-rename(NIngresos, Cla_anu="...10")
NIngresos<-rename(NIngresos, Ing_men="Ingresos altos")
NIngresos<-rename(NIngresos, Ing_año="...12")
NIngresos<-rename(NIngresos, Ing_anu="...13")
NIngresos<-rename(NIngresos, Tot_men=Total)
NIngresos<-rename(NIngresos, Tot_año="...15")
NIngresos<-rename(NIngresos, Tot_anu="...16")
NIngresos<-rename(NIngresos, orden="...1")


NIngresos<-drop_na(NIngresos, orden)
NIngresos = subset(NIngresos, select = -c(orden) )

library("reshape2")

NIngresos_final<-melt(NIngresos, id.vars= c(".id"),
                      variable.name = "Ningresos",
                      value.name= "Variacion")

NIngresos_final$Nivel<-substr(NIngresos_final$Ningresos, 1, 3)
NIngresos_final$Periodo<-substr(NIngresos_final$Ningresos, 5, 7)


NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Pob"] <- "Pobre"
NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Vul"] <- "Vulnerable"
NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Cla"] <- "Clase Media"
NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Ing"] <- "Ingresos altos"
NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Tot"] <- "Total"


NIngresos_final['Periodo'][NIngresos_final['Periodo'] == "anu"] <- "Anual"
NIngresos_final['Periodo'][NIngresos_final['Periodo'] == "año"] <- "Año corrido"
NIngresos_final['Periodo'][NIngresos_final['Periodo'] == "men"] <- "Mensual"

NIngresos_final = subset(NIngresos_final, select = -c(Ningresos) )
NIngresos_final<-rename(NIngresos_final, "Mes"=".id")

Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")

NIngresos_final$Año<-2019

NIngresos_final$Fecha1<-"."
NIngresos_final$Fecha1[NIngresos_final$Mes == "Enero"] <- "2019-01-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Febrero"] <- "2019-02-28"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Marzo"] <- "2019-03-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Abril"] <- "2019-04-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Mayo"] <- "2019-05-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Junio"] <- "2019-06-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Julio"] <- "2019-07-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Agosto"] <-"2019-08-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Septiembre"] <- "2019-09-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Octubre"] <- "2019-10-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Noviembre"] <- "2019-11-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Diciembre"] <- "2019-12-30"

NIngresos2019<-NIngresos_final
rm(myfiles, NIngresos_final, NIngresos )

################################################################################################################
################################################2020 ##########################################################
##############################################################################################################

# Año 2019 Variación MENSUAL  ----------------------------------------------------------------

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2020")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c("Abril", "Agosto", "Diciembre", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo", "Noviembre", "Octubre", "Septiembre")
VM2020<- ldply(myfiles, read_excel, sheet = "4", range = "A7:N31")
VM2020$Año<-2020

VM2020$Fecha1<-"."
VM2020$Fecha1[VM2020$.id == "Enero"] <- "2020-01-30"
VM2020$Fecha1[VM2020$.id == "Febrero"] <- "2020-02-28"
VM2020$Fecha1[VM2020$.id == "Marzo"] <- "2020-03-30"
VM2020$Fecha1[VM2020$.id == "Abril"] <- "2020-04-30"
VM2020$Fecha1[VM2020$.id == "Mayo"] <- "2020-05-30"
VM2020$Fecha1[VM2020$.id == "Junio"] <- "2020-06-30"
VM2020$Fecha1[VM2020$.id == "Julio"] <- "2020-07-30"
VM2020$Fecha1[VM2020$.id == "Agosto"] <- "2020-08-30"
VM2020$Fecha1[VM2020$.id == "Septiembre"] <- "2020-09-30"
VM2020$Fecha1[VM2020$.id == "Octubre"] <- "2020-10-30"
VM2020$Fecha1[VM2020$.id == "Noviembre"] <- "2020-11-30"
VM2020$Fecha1[VM2020$.id == "Diciembre"] <- "2020-12-30"
rm(myfiles )



# Año 2020 Variación AÑO CORRIDO  ----------------------------------------------------------------

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2020")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c("Abril", "Agosto", "Diciembre", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo", "Noviembre", "Octubre", "Septiembre")
VAC2020<- ldply(myfiles, read_excel, sheet = "5", range = "A7:N31")
VAC2020$Año<-2020

VAC2020$Fecha1<-"."
VAC2020$Fecha1[VAC2020$.id == "Enero"] <- "2020-01-30"
VAC2020$Fecha1[VAC2020$.id == "Febrero"] <- "2020-02-28"
VAC2020$Fecha1[VAC2020$.id == "Marzo"] <- "2020-03-30"
VAC2020$Fecha1[VAC2020$.id == "Abril"] <- "2020-04-30"
VAC2020$Fecha1[VAC2020$.id == "Mayo"] <- "2020-05-30"
VAC2020$Fecha1[VAC2020$.id == "Junio"] <- "2020-06-30"
VAC2020$Fecha1[VAC2020$.id == "Julio"] <- "2020-07-30"
VAC2020$Fecha1[VAC2020$.id == "Agosto"] <- "2020-08-30"
VAC2020$Fecha1[VAC2020$.id == "Septiembre"] <- "2020-09-30"
VAC2020$Fecha1[VAC2020$.id == "Octubre"] <- "2020-10-30"
VAC2020$Fecha1[VAC2020$.id == "Noviembre"] <- "2020-11-30"
VAC2020$Fecha1[VAC2020$.id == "Diciembre"] <- "2020-12-30"

rm(myfiles)

# Año 2020 Variación NIVEL INGRESOS  ----------------------------------------------------------------


setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2020")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c("Abril", "Agosto", "Diciembre", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo", "Noviembre", "Octubre", "Septiembre")
NIngresos<- ldply(myfiles, read_excel, sheet = "3", range = "A7:P9")

NIngresos<-rename(NIngresos, Pob_men=Pobres)
NIngresos<-rename(NIngresos, Pob_año="...3")
NIngresos<-rename(NIngresos, Pob_anu="...4")
NIngresos<-rename(NIngresos, Vul_men=Vulnerables)
NIngresos<-rename(NIngresos, Vul_año="...6")
NIngresos<-rename(NIngresos, Vul_anu="...7")
NIngresos<-rename(NIngresos, Cla_men="Clase media")
NIngresos<-rename(NIngresos, Cla_año="...9")
NIngresos<-rename(NIngresos, Cla_anu="...10")
NIngresos<-rename(NIngresos, Ing_men="Ingresos altos")
NIngresos<-rename(NIngresos, Ing_año="...12")
NIngresos<-rename(NIngresos, Ing_anu="...13")
NIngresos<-rename(NIngresos, Tot_men=Total)
NIngresos<-rename(NIngresos, Tot_año="...15")
NIngresos<-rename(NIngresos, Tot_anu="...16")
NIngresos<-rename(NIngresos, orden="...1")


NIngresos<-drop_na(NIngresos, orden)
NIngresos = subset(NIngresos, select = -c(orden) )

library("reshape2")

NIngresos_final<-melt(NIngresos, id.vars= c(".id"),
                      variable.name = "Ningresos",
                      value.name= "Variacion")

NIngresos_final$Nivel<-substr(NIngresos_final$Ningresos, 1, 3)
NIngresos_final$Periodo<-substr(NIngresos_final$Ningresos, 5, 7)

NIngresos_final<-replace(Nivel, "Pobres"="Pob")

NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Pob"] <- "Pobre"
NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Vul"] <- "Vulnerable"
NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Cla"] <- "Clase Media"
NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Ing"] <- "Ingresos altos"
NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Tot"] <- "Total"


NIngresos_final['Periodo'][NIngresos_final['Periodo'] == "anu"] <- "Anual"
NIngresos_final['Periodo'][NIngresos_final['Periodo'] == "año"] <- "Año corrido"
NIngresos_final['Periodo'][NIngresos_final['Periodo'] == "men"] <- "Mensual"

NIngresos_final = subset(NIngresos_final, select = -c(Ningresos) )
NIngresos_final<-rename(NIngresos_final, "Mes"=".id")

Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
NIngresos_final$Año<-2020

NIngresos_final$Fecha1<-"."
NIngresos_final$Fecha1[NIngresos_final$Mes == "Enero"] <- "2020-01-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Febrero"] <- "2020-02-28"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Marzo"] <- "2020-03-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Abril"] <- "2020-04-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Mayo"] <- "2020-05-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Junio"] <- "2020-06-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Julio"] <- "2020-07-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Agosto"] <-"2020-08-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Septiembre"] <- "2020-09-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Octubre"] <- "2020-10-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Noviembre"] <- "2020-11-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Diciembre"] <- "2020-12-30"

NIngresos2020<-NIngresos_final
rm(myfiles, NIngresos_final, NIngresos )


################################################################################################################
################################################2021 ##########################################################
##############################################################################################################



# Año 2021 Variación MENSUAL  ----------------------------------------------------------------

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2021")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c("Abril", "Agosto", "Diciembre", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo", "Octubre", "Septiembre")
VM2021<- ldply(myfiles, read_excel, sheet = "4", range = "A7:N31")
VM2021$Año<-2021


VM2021$Fecha1<-"."
VM2021$Fecha1[VM2021$.id == "Enero"] <- "2021-01-30"
VM2021$Fecha1[VM2021$.id == "Febrero"] <- "2021-02-28"
VM2021$Fecha1[VM2021$.id == "Marzo"] <- "2021-03-30"
VM2021$Fecha1[VM2021$.id == "Abril"] <- "2021-04-30"
VM2021$Fecha1[VM2021$.id == "Mayo"] <- "2021-05-30"
VM2021$Fecha1[VM2021$.id == "Junio"] <- "2021-06-30"
VM2021$Fecha1[VM2021$.id == "Julio"] <- "2021-07-30"
VM2021$Fecha1[VM2021$.id == "Agosto"] <- "2021-08-30"
VM2021$Fecha1[VM2021$.id == "Septiembre"] <- "2021-09-30"
VM2021$Fecha1[VM2021$.id == "Octubre"] <- "2021-10-30"
VM2021$Fecha1[VM2021$.id == "Diciembre"] <- "2021-12-30"
rm(myfiles )



# Año 2021 Variación AÑO CORRIDO  ----------------------------------------------------------------

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2021")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c("Abril", "Agosto", "Diciembre", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo", "Octubre", "Septiembre")
VAC2021<- ldply(myfiles, read_excel, sheet = "5", range = "A7:N31")
VAC2021$Año<-2021

VAC2021$Fecha1<-"."
VAC2021$Fecha1[VAC2021$.id == "Enero"] <- "2021-01-30"
VAC2021$Fecha1[VAC2021$.id == "Febrero"] <- "2021-02-28"
VAC2021$Fecha1[VAC2021$.id == "Marzo"] <- "2021-03-30"
VAC2021$Fecha1[VAC2021$.id == "Abril"] <- "2021-04-30"
VAC2021$Fecha1[VAC2021$.id == "Mayo"] <- "2021-05-30"
VAC2021$Fecha1[VAC2021$.id == "Junio"] <- "2021-06-30"
VAC2021$Fecha1[VAC2021$.id == "Julio"] <- "2021-07-30"
VAC2021$Fecha1[VAC2021$.id == "Agosto"] <- "2021-08-30"
VAC2021$Fecha1[VAC2021$.id == "Septiembre"] <- "2021-09-30"
VAC2021$Fecha1[VAC2021$.id == "Octubre"] <- "2021-10-30"
VAC2021$Fecha1[VAC2021$.id == "Diciembre"] <- "2021-12-30"

rm(myfiles)

# Año 2021 Variación NIVEL INGRESOS  ----------------------------------------------------------------


setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2021")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c("Abril", "Agosto", "Diciembre", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo","Octubre", "Septiembre")
NIngresos<- ldply(myfiles, read_excel, sheet = "3", range = "A7:P9")

NIngresos<-rename(NIngresos, Pob_men=Pobres)
NIngresos<-rename(NIngresos, Pob_año="...3")
NIngresos<-rename(NIngresos, Pob_anu="...4")
NIngresos<-rename(NIngresos, Vul_men=Vulnerables)
NIngresos<-rename(NIngresos, Vul_año="...6")
NIngresos<-rename(NIngresos, Vul_anu="...7")
NIngresos<-rename(NIngresos, Cla_men="Clase media")
NIngresos<-rename(NIngresos, Cla_año="...9")
NIngresos<-rename(NIngresos, Cla_anu="...10")
NIngresos<-rename(NIngresos, Ing_men="Ingresos altos")
NIngresos<-rename(NIngresos, Ing_año="...12")
NIngresos<-rename(NIngresos, Ing_anu="...13")
NIngresos<-rename(NIngresos, Tot_men=Total)
NIngresos<-rename(NIngresos, Tot_año="...15")
NIngresos<-rename(NIngresos, Tot_anu="...16")
NIngresos<-rename(NIngresos, orden="...1")


NIngresos<-drop_na(NIngresos, orden)
NIngresos = subset(NIngresos, select = -c(orden) )

library("reshape2")

NIngresos_final<-melt(NIngresos, id.vars= c(".id"),
                      variable.name = "Ningresos",
                      value.name= "Variacion")

NIngresos_final$Nivel<-substr(NIngresos_final$Ningresos, 1, 3)
NIngresos_final$Periodo<-substr(NIngresos_final$Ningresos, 5, 7)

NIngresos_final<-replace(Nivel, "Pobres"="Pob")

NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Pob"] <- "Pobre"
NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Vul"] <- "Vulnerable"
NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Cla"] <- "Clase Media"
NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Ing"] <- "Ingresos altos"
NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Tot"] <- "Total"


NIngresos_final['Periodo'][NIngresos_final['Periodo'] == "anu"] <- "Anual"
NIngresos_final['Periodo'][NIngresos_final['Periodo'] == "año"] <- "Año corrido"
NIngresos_final['Periodo'][NIngresos_final['Periodo'] == "men"] <- "Mensual"

NIngresos_final = subset(NIngresos_final, select = -c(Ningresos) )
NIngresos_final<-rename(NIngresos_final, "Mes"=".id")

Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
NIngresos_final$Año<-2021

NIngresos_final$Fecha1<-"."
NIngresos_final$Fecha1[NIngresos_final$Mes == "Enero"] <- "2021-01-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Febrero"] <- "2021-02-28"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Marzo"] <- "2021-03-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Abril"] <- "2021-04-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Mayo"] <- "2021-05-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Junio"] <- "2021-06-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Julio"] <- "2021-07-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Agosto"] <-"2021-08-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Septiembre"] <- "2021-09-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Octubre"] <- "2021-10-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Noviembre"] <- "2021-11-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Diciembre"] <- "2021-12-30"

NIngresos2021<-NIngresos_final
rm(myfiles, NIngresos_final, NIngresos )

################################################################################################################
#############################################   2022   ##########################################################
##############################################################################################################


# Año 2022 Variación MENSUAL  ----------------------------------------------------------------

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2022")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c( "Abril","Agosto", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo", "Septiembre")
VM2022<- ldply(myfiles, read_excel, sheet = "4", range = "A7:N31")
VM2022$Año<-2022


VM2022$Fecha1<-"."
VM2022$Fecha1[VM2022$.id == "Enero"] <- "2022-01-30"
VM2022$Fecha1[VM2022$.id == "Febrero"] <- "2022-02-28"
VM2022$Fecha1[VM2022$.id == "Marzo"] <- "2022-03-30"
VM2022$Fecha1[VM2022$.id == "Abril"] <- "2022-04-30"
VM2022$Fecha1[VM2022$.id == "Mayo"] <- "2022-05-30"
VM2022$Fecha1[VM2022$.id == "Junio"] <- "2022-06-30"
VM2022$Fecha1[VM2022$.id == "Julio"] <- "2022-07-30"
VM2022$Fecha1[VM2022$.id == "Agosto"] <- "2022-08-30"
VM2022$Fecha1[VM2022$.id == "Septiembre"] <- "2022-09-30"






rm(myfiles )



# Año 2022 Variación AÑO CORRIDO  ----------------------------------------------------------------

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2022")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c(  "Abril", "Agosto", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo", "Septiembre")
VAC2022<- ldply(myfiles, read_excel, sheet = "5", range = "A7:N31")
VAC2022$Año<-2022

VAC2022$Fecha1<-"."
VAC2022$Fecha1[VAC2022$.id == "Enero"] <- "2022-01-30"
VAC2022$Fecha1[VAC2022$.id == "Febrero"] <- "2022-02-28"
VAC2022$Fecha1[VAC2022$.id == "Marzo"] <- "2022-03-30"
VAC2022$Fecha1[VAC2022$.id == "Abril"] <- "2022-04-30"
VAC2022$Fecha1[VAC2022$.id == "Mayo"] <- "2022-05-30"
VAC2022$Fecha1[VAC2022$.id == "Junio"] <- "2022-06-30"
VAC2022$Fecha1[VAC2022$.id == "Julio"] <- "2022-07-30"
VAC2022$Fecha1[VAC2022$.id == "Agosto"] <- "2022-08-30"
VAC2022$Fecha1[VAC2022$.id == "Septiembre"] <- "2022-09-30"



rm(myfiles)

# Año 2022 Variación NIVEL INGRESOS  ----------------------------------------------------------------


setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2022")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c( "Abril", "Agosto", "Enero", "Febrero", "Julio", "Junio",  "Marzo", "Mayo", "Septiembre")
NIngresos<- ldply(myfiles, read_excel, sheet = "3", range = "A7:P9")


NIngresos<-rename(NIngresos, Pob_men=Pobres)
NIngresos<-rename(NIngresos, Pob_año="...3")
NIngresos<-rename(NIngresos, Pob_anu="...4")
NIngresos<-rename(NIngresos, Vul_men=Vulnerables)
NIngresos<-rename(NIngresos, Vul_año="...6")
NIngresos<-rename(NIngresos, Vul_anu="...7")
NIngresos<-rename(NIngresos, Cla_men="Clase media")
NIngresos<-rename(NIngresos, Cla_año="...9")
NIngresos<-rename(NIngresos, Cla_anu="...10")
NIngresos<-rename(NIngresos, Ing_men="Ingresos altos")
NIngresos<-rename(NIngresos, Ing_año="...12")
NIngresos<-rename(NIngresos, Ing_anu="...13")
NIngresos<-rename(NIngresos, Tot_men=Total)
NIngresos<-rename(NIngresos, Tot_año="...15")
NIngresos<-rename(NIngresos, Tot_anu="...16")
NIngresos<-rename(NIngresos, orden="...1")

NIngresos<-drop_na(NIngresos, orden)
NIngresos = subset(NIngresos, select = -c(orden) )

library("reshape2")

NIngresos_final<-melt(NIngresos, id.vars= c(".id"),
                      variable.name = "Ningresos",
                      value.name= "Variacion")

NIngresos_final$Nivel<-substr(NIngresos_final$Ningresos, 1, 3)
NIngresos_final$Periodo<-substr(NIngresos_final$Ningresos, 5, 7)

NIngresos_final<-replace(Nivel, "Pobres"="Pob")

NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Pob"] <- "Pobre"
NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Vul"] <- "Vulnerable"
NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Cla"] <- "Clase Media"
NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Ing"] <- "Ingresos altos"
NIngresos_final['Nivel'][NIngresos_final['Nivel'] == "Tot"] <- "Total"


NIngresos_final['Periodo'][NIngresos_final['Periodo'] == "anu"] <- "Anual"
NIngresos_final['Periodo'][NIngresos_final['Periodo'] == "año"] <- "Año corrido"
NIngresos_final['Periodo'][NIngresos_final['Periodo'] == "men"] <- "Mensual"

NIngresos_final = subset(NIngresos_final, select = -c(Ningresos) )
NIngresos_final<-rename(NIngresos_final, "Mes"=".id")

Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
NIngresos_final$Año<-2022

NIngresos_final$Fecha1<-"."
NIngresos_final$Fecha1[NIngresos_final$Mes == "Enero"] <- "2022-01-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Febrero"] <- "2022-02-28"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Marzo"] <- "2022-03-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Abril"] <- "2022-04-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Mayo"] <- "2022-05-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Junio"] <- "2022-06-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Julio"] <- "2022-07-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Agosto"] <- "2022-08-30"
NIngresos_final$Fecha1[NIngresos_final$Mes == "Septiembre"] <- "2022-09-30"



NIngresos2022<-NIngresos_final
rm(myfiles, NIngresos_final, NIngresos )


################################################################################################################
################################################ PEGUE 2  ##########################################################
##############################################################################################################
VM_2<-rbind( VM2019, VM2020, VM2021, VM2022)
VAC_2<-rbind( VAC2019, VAC2020, VAC2021, VAC2022)
NIngresos_2<-rbind( NIngresos2019, NIngresos2020, NIngresos2021, NIngresos2022) 
rm(NIngresos2019, NIngresos2020,NIngresos2021, NIngresos2022, VM2019, VM2020, VM2021, VM2022, VAC2019, VAC2020, VAC2021, VAC2022)

################################################################################################################
################################################ PEGUE 3  ##########################################################
##############################################################################################################


colnames(VM_2)<-c( ".id" , "Ciudades", "Var1", "Var2", "Var3", "Var4", "Var5","Var6", "Var7", "Var8", "Var9", "Var10", "Var11", "Var12", "Var13", "Año", "Fecha1")
colnames(VAC_2)<-c( ".id" , "Ciudades", "Var1", "Var2", "Var3", "Var4", "Var5","Var6", "Var7", "Var8", "Var9", "Var10", "Var11", "Var12", "Var13", "Año", "Fecha1")

colnames(VM_1)<-c( "Ciudades", "Var1", "Var4", "Var3", "Var6", "Var10","Var9", "Var7", "Var8", "Var14", "Var13", ".id", "Fecha1", "Año")
colnames(VAC_1)<-c( "Ciudades", "Var1", "Var4", "Var3", "Var6", "Var10","Var9", "Var7", "Var8", "Var14", "Var13", ".id", "Fecha1", "Año")

VM_1$Var2<-"." 
VM_1$Var5<-"." 
VM_1$Var11<-"." 
VM_1$Var12<-"." 
VM_2$Var14<-"." 

VAC_1$Var2<-"." 
VAC_1$Var5<-"." 
VAC_1$Var11<-"." 
VAC_1$Var12<-"." 
VAC_2$Var14<-"." 


VM<-rbind( VM_1, VM_2)
VAC<-rbind(VAC_1, VAC_2)
NIngresos<-rbind(NIngresos_1, NIngresos_2)
rm(NIngresos_1, NIngresos_2, VAN_1, VAC_1, VAC_2, VM_1, VM_2)


################################################################################################################
####################################### AJUSTES ################################################
##############################################################################################################


VM$NumMes<-0 
VM$NumMes[VM$.id == "Enero"] <- 1
VM$NumMes[VM$.id == "Febrero"] <- 2
VM$NumMes[VM$.id == "Marzo"] <- 3
VM$NumMes[VM$.id == "Abril"] <- 4
VM$NumMes[VM$.id == "Mayo"] <- 5
VM$NumMes[VM$.id == "Junio"] <- 6
VM$NumMes[VM$.id == "Julio"] <- 7
VM$NumMes[VM$.id == "Agosto"] <- 8
VM$NumMes[VM$.id == "Septiembre"] <- 9
VM$NumMes[VM$.id == "Octubre"] <- 10
VM$NumMes[VM$.id == "Noviembre"] <- 11
VM$NumMes[VM$.id == "Diciembre"] <- 12

VAC$NumMes<-0 
VAC$NumMes[VAC$.id == "Enero"] <- 1
VAC$NumMes[VAC$.id == "Febrero"] <- 2
VAC$NumMes[VAC$.id == "Marzo"] <- 3
VAC$NumMes[VAC$.id == "Abril"] <- 4
VAC$NumMes[VAC$.id == "Mayo"] <- 5
VAC$NumMes[VAC$.id == "Junio"] <- 6
VAC$NumMes[VAC$.id == "Julio"] <- 7
VAC$NumMes[VAC$.id == "Agosto"] <- 8
VAC$NumMes[VAC$.id == "Septiembre"] <- 9
VAC$NumMes[VAC$.id == "Octubre"] <- 10
VAC$NumMes[VAC$.id == "Noviembre"] <- 11
VAC$NumMes[VAC$.id == "Diciembre"] <- 12

NIngresos$NumMes<-0 
NIngresos$NumMes[NIngresos$Mes == "Enero"] <- 1
NIngresos$NumMes[NIngresos$Mes == "Febrero"] <- 2
NIngresos$NumMes[NIngresos$Mes == "Marzo"] <- 3
NIngresos$NumMes[NIngresos$Mes == "Abril"] <- 4
NIngresos$NumMes[NIngresos$Mes == "Mayo"] <- 5
NIngresos$NumMes[NIngresos$Mes == "Junio"] <- 6
NIngresos$NumMes[NIngresos$Mes == "Julio"] <- 7
NIngresos$NumMes[NIngresos$Mes == "Agosto"] <- 8
NIngresos$NumMes[NIngresos$MEs == "Septiembre"] <- 9
NIngresos$NumMes[NIngresos$Mes == "Octubre"] <- 10
NIngresos$NumMes[NIngresos$Mes == "Noviembre"] <- 11
NIngresos$NumMes[NIngresos$Mes == "Diciembre"] <- 12


class(VM$Fecha1)
VM$Fecha1<- as.Date(VM$Fecha1)
class(NIngresos$Fecha1)
NIngresos$Fecha1<- as.Date(NIngresos$Fecha1)
class(VAC$Fecha1)
VAC$Fecha1<- as.Date(VAC$Fecha1)

VM_Nal<-subset(VM,  Ciudades== "Total IPC" | Ciudades== "Nacional" | Ciudades== "Total IPC" | Ciudades== "Total Nacional")
VM_Nal$Año<-as.character(VM_Nal$Año)
VAC_Nal<-subset(VAC,  Ciudades== "Total IPC" | Ciudades== "Nacional" | Ciudades== "Total IPC" | Ciudades== "Total Nacional")
VAC_Nal$Año<-as.character(VAC_Nal$Año)

class(NIngresos$Variacion)
NIngresos$Variacion<- as.numeric(NIngresos$Variacion)

NIngresos_ANU<-subset(NIngresos,  Periodo== "Anual")
NIngresos_AC<-subset(NIngresos,  Periodo== "Año corrido")
NIngresos_MEN<-subset(NIngresos,  Periodo== "Mensual")


################################################################################################################
################################################ GRÁFICAS ##########################################################
##############################################################################################################


####################################GRAFICAS POR AÑO ############################################
#######################################GRÁFICA 1 
####VARIACIÓN MENSUAL - total

VM_NalNUEVA<-VM_Nal[!(VM_Nal$Año==2017 | VM_Nal$Año==2018),]

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion")
svg("Gráfica1.svg", height=5, width=10)
ggplot(VM_NalNUEVA, aes(NumMes, Var13, group=Año, color= Año)) +
  geom_line()+geom_point()+scale_x_continuous(breaks=seq(1,12,by=1),
                                              labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(size= 3, aes(label=Var13),position=position_dodge(width=0.5), vjust=-0.25, color = "black")+
  annotate("text",x=10  , y=1.25,label="La inflación mensual en\nseptiembre fue de 0.93%")+
  scale_color_manual(values=c("deeppink4",  "darkblue", 'dodgerblue','cyan2', "deeppink"))
dev.off()

####VARIACIÓN MENSUAL - alimentos



#setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion")
#svg("Var_mensual_año_alimentos.svg", height=5, width=10)
#ggplot(VM_Nal, aes(NumMes, Var1, group=Año, color= Año)) +
#  geom_line()+geom_point()+scale_x_continuous(breaks=seq(1,12,by=1),
                                              #labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
#  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
#  geom_text(aes(label=Var1),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")+
#  annotate("text",x=3  , y=4.5,label="La inflación abril de alimentos\n en marzo 2022 fue de 2,75%")
#dev.off()
###VARIACIÓN AÑO CORRIDO - total 

#svg("Var_ac_año.svg", height=5, width=10)
#ggplot(VAC_Nal, aes(NumMes, Var13, group=Año, color= Año)) +
  #geom_line()+geom_point()+scale_x_continuous(breaks=seq(1,12,by=1),
                                              #labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  #ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  #geom_text(aes(label=Var13),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")+
  #annotate("text",x=2  , y=5.5,label="La inflación acumulada en \nabril 2022 fue de 5,66%")
#dev.off()

###VARIACIÓN AÑO CORRIDO - alimentos 

#svg("Var_ac_año_alimentos.svg", height=5, width=10)
#ggplot(VAC_Nal, aes(NumMes, Var1, group=Año, color= Año)) +
  #geom_line()+geom_point()+scale_x_continuous(breaks=seq(1,12,by=1),
                                              #labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  #ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  #geom_text(aes(label=Var1),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")+
  #annotate("text",x=4  , y=15,label="La inflación acumulada en\nabril 2022 fue de 13,25%")
#dev.off()

##VARIACIÓN POR NIVEL DE INGRESOS

# Anual  ----------------------------------------------------------------


#ggplot(NIngresos_ANU, aes(NumMes,Variacion , group=interaction(Año, Nivel) , color= Nivel)) +
  #geom_line()+geom_point()+scale_x_continuous(breaks=seq(1,12,by=1),
                                              #labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  #ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  #geom_text(aes(label=Variacion),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")



# Año Corrido -------------------------------------------------------------

#ggplot(NIngresos_AC, aes(NumMes, Variacion, group=interaction(Año, Nivel), color= Nivel)) +
  #geom_line()+geom_point()+scale_x_continuous(breaks=seq(1,12,by=1),
                                              #labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  #ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  #geom_text(aes(label=Variacion),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")

# Mensual -----------------------------------------------------------------



#ggplot(NIngresos_MEN, aes(NumMes, Variacion, group=interaction(Año, Nivel), color= Nivel)) +
  #geom_line()+geom_point()+scale_x_continuous(breaks=seq(1,12,by=1),
                                              #labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  #ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  #geom_text(aes(label=Variacion),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")



########################## GRAFICAS COMO SERIE ##########################


#VARIACIÓN MENSUAL - total 
#svg("Var_mensual_serie_total.svg", height=5, width=10)
#ggplot(VM_Nal, aes(Fecha1, Var13, group= Año, color= Año))+
  #geom_line()+geom_point()+ scale_x_date(date_labels="%b %Y", breaks = unique(VM_Nal$Fecha1))+
  #theme(axis.text.x = element_text(angle = 90, size =10))+
  #ggtitle("") + xlab("Fecha") + ylab("Variación %") +
  #annotate(geom="text",x=as.Date("2021-10-30") ,y=1.25,label="La inflación mensual en\nabril 2022 fue de 1.25%")+
  #geom_text(aes(label=Var13),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")
#dev.off()

#VARIACIÓN MENSUAL - alimentos
#svg("Var_mensual_serie_alimentos.svg", height=5, width=10)
#ggplot(VM_Nal, aes(Fecha1, Var1, group= Año, color= Año))+
  #geom_line()+geom_point()+ scale_x_date(date_labels="%b %Y", breaks = unique(VM_Nal$Fecha1))+
  #theme(axis.text.x = element_text(angle = 90, size =10))+
  #ggtitle("") + xlab("Fecha") + ylab("Variación %") +
  #annotate(geom="text",x=as.Date("2022-1-30") ,y=4.4,label="La inflación mensual de alimentos en\nabril 2022 fue de 2,75%")+
  #geom_text(aes(label=Var1),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")
#dev.off()


#VARIACIÓN AÑO CORRIDO -total 
#svg("Var_ac_serie.svg", height=5, width=10)
#ggplot(VAC_Nal, aes(Fecha1, Var13, group= Año, color= Año))+
  #geom_line()+ geom_point()+scale_x_date(date_labels="%b %Y", breaks = unique(VM_Nal$Fecha1))+
  #theme(axis.text.x = element_text(angle = 90, size =10))+
  #ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  #annotate(geom="text",x=as.Date("2022-01-30") ,y=1,label="La inflación acumulada\nen abril 2022\nfue de 5,66%")+
  #geom_text(aes(label=Var13),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")
#dev.off()

#VARIACIÓN AÑO CORRIDO -alimentos
#svg("Var_ac_serie_alimentos.svg", height=5, width=10)
#ggplot(VAC_Nal, aes(Fecha1, Var1, group= Año, color= Año))+
  #geom_line()+ geom_point()+scale_x_date(date_labels="%b %Y", breaks = unique(VM_Nal$Fecha1))+
  #theme(axis.text.x = element_text(angle = 90, size =10))+
  #ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  #annotate(geom="text",x=as.Date("2022-03-30") ,y=16,label="La inflación\nacumulada\nen abril 2022\nfue de 13,25%")+
  #geom_text(aes(label=Var1),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")
#dev.off()

##VARIACIÓN POR NIVEL DE INGRESOS

# Anual -------------------------------------------------------------------
#ggplot(NIngresos_ANU, aes(Fecha1, Variacion, group= Nivel, color= Nivel))+
  #geom_line()+ geom_point()+scale_x_date(date_labels="%b %Y", breaks = unique(NIngresos_ANU$Fecha1))+
  #theme(axis.text.x = element_text(angle = 90, size =10))+
  #ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  #geom_text(data=subset(NIngresos_ANU, Nivel=="Pobre"),aes(label=Variacion),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")



# Año corrido -------------------------------------------------------------

#svg("NING_ac_serie.svg", height=5, width=10)
#ggplot(NIngresos_AC, aes(Fecha1, Variacion, group= Nivel, color= Nivel))+
  #geom_line()+ geom_point()+scale_x_date(date_labels="%b %Y", breaks = unique(NIngresos_ANU$Fecha1))+
  #theme(axis.text.x = element_text(angle = 90, size =10))+
  #ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  #geom_text(data=subset(NIngresos_AC, Nivel=="Pobre"),aes(label=Variacion),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+
  #annotate(geom="text",x=as.Date("2022-02-28") ,y=0.8 ,label="La inflación\nacumulada\npara los pobres\nfue de 6,6%")
#dev.off()



# Mensual  ----------------------------------------------------------------

#svg("NING_men_serie.svg", height=5, width=10)
#ggplot(NIngresos_MEN, aes(Fecha1, Variacion, group= Nivel, color= Nivel))+
  #geom_line()+ scale_x_date(date_labels="%b %Y", breaks = unique(NIngresos_ANU$Fecha1))+
  #theme(axis.text.x = element_text(angle = 90, size =10))+
  #ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  #geom_text(data=subset(NIngresos_MEN, Nivel=="Pobre"),aes(label=Variacion),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+ 
  #annotate(geom="text",x=as.Date("2021-09-30") ,y=1.5 ,label="La inflación\nacumulada\npara los pobres\nfue de 1,47%")
#dev.off()


#########################################
#######GRÁFICA 2  

##Voy a quitar el 2007
VM_Nal<-subset(VM_Nal, Año!=2007)

library(lubridate)
Sys.setlocale("LC_TIME", "Spanish")

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion")
svg("Gráfica2.svg", height=5, width=10)
ggplot(VM_Nal, aes(Fecha1)) +
  geom_line(aes(y=Var13, col = "Total"))+ geom_point(aes(y=Var13, col = "Total" ))+
  geom_line(aes(y=Var1, col = "Alimentos" ))+ geom_point(aes(y=Var1, col = "Alimentos" ))+
  scale_x_date(date_labels="%b %Y", breaks = unique(VM_Nal$Fecha1))+
  theme(axis.text.x = element_text(angle = 90, size =10))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %") + labs(col = 'Categoría')+
  geom_text(aes(y=Var13, label=Var13), position=position_dodge(width=0.1),vjust=2, cex=3) + 
  geom_text(aes(y=Var1, label=Var1), position=position_dodge(width=0.1),vjust=-1, cex=3)+
  geom_vline(xintercept=as.Date("2018-01-30"), linetype="dashed", color = "deeppink")+
  geom_vline(xintercept=as.Date("2019-01-30"), linetype="dashed", color = "deeppink")+
  geom_vline(xintercept=as.Date("2020-01-30"), linetype="dashed", color = "deeppink")+
  geom_vline(xintercept=as.Date("2021-01-30"), linetype="dashed", color = "deeppink")+
  geom_vline(xintercept=as.Date("2022-01-01"), linetype="dashed", color = "deeppink")+
  scale_color_manual(values=c('dodgerblue','cyan2'))
dev.off()


######GRÁFICA 3  
##VARIACIÓN AÑO CORRIDO - total y alimentos

VAC_NalNUEVA<-VAC_Nal[!(VAC_Nal$Año==2017 | VAC_Nal$Año==2018 | VAC_Nal$Año==2007) ,]


gra1<-ggplot(VAC_NalNUEVA, aes(NumMes, Var13, group=Año, color= Año)) +
  geom_line()+geom_point()+scale_x_continuous(breaks=seq(1,12,by=1),
                                              labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(size=3, aes(label=Var13),position=position_dodge(width=0.9), vjust=-0.25, color = "black", cex=3)+
  annotate("text",x=9 , y=15,label="La inflación acumulada en \nseptiembre fue de 10,08%")+
  ylim(1, 20)+scale_color_manual(values=c( "darkblue", 'dodgerblue','cyan2', "deeppink"))+
  theme(legend.position="none")

gra1

gra22<-ggplot(VAC_NalNUEVA, aes(NumMes, Var1, group=Año, color= Año)) +
  geom_line()+geom_point()+scale_x_continuous(breaks=seq(1,12,by=1),
                                              labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  ggtitle("") + xlab("Fecha") + ylab("")+
  geom_text(size=3, aes(label=Var1),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+
  annotate("text",x=3  , y=19,label="La inflación acumulada en\nseptiembre fue de 21,19%")+
  ylim(1, 22)+scale_color_manual(values=c("darkblue", 'dodgerblue','cyan2', "deeppink"))

gra22

figure <- ggarrange(gra1, gra22,
                    labels = c("Total", "Alimentos"),
                    ncol = 2, nrow = 1)
figure

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion")
svg("Gráfica3.svg", height=5, width=10)
figure
dev.off()

######GRAFICA 4 ADICIONAL QUE PIDIÓ LAURA 
NIngresos_AC2022<-subset(NIngresos_AC, Año==2022)


#library(lubridate)
Sys.setlocale("LC_TIME", "Spanish")

svg("Gráfica4.svg", height=5, width=10)
ggplot(NIngresos_AC2022, aes(Fecha1, Variacion, group= Nivel, color= Nivel))+
  geom_line()+ geom_point()+scale_x_date(date_labels="%b %Y", breaks = unique(NIngresos_ANU$Fecha1))+
  theme(axis.text.x = element_text(angle = 90, size =10))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(data=subset(NIngresos_AC2022, Nivel=="Pobre"),aes(label=Variacion),position=position_dodge(width=0.9), vjust=1, color = "black")+
  geom_text(data=subset(NIngresos_AC2022, Nivel=="Total"),aes(label=Variacion),position=position_dodge(width=0.9), vjust=1, color = "black")+
  geom_text(data=subset(NIngresos_AC2022, Nivel=="Ingresos altos"),aes(label=Variacion),position=position_dodge(width=0.9), vjust=1, color = "black")+
  annotate(geom="text",x=as.Date("2022-08-30") ,y=4.5 ,label="La inflación acumulada para los pobres\ny vulnerables fue de 11,39% ")+
  scale_color_manual(values=c("darkblue", 'dodgerblue','cyan2', "azure4", "deeppink"))
dev.off()


