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

Oct<- read_excel("anexo_ipc_nov17.xls", "Anexo3", range = "A7:P32")
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

NIngresos2017 = subset(NIngresos_final, select = -c(Ningresos) )

NIngresos2017$Fecha1<-"."
NIngresos2017$Fecha1[NIngresos2017$.id == "Enero"] <- "2017-01-30"
NIngresos2017$Fecha1[NIngresos2017$.id == "Febrero"] <- "2017-02-28"
NIngresos2017$Fecha1[NIngresos2017$.id == "Marzo"] <- "2017-03-30"
NIngresos2017$Fecha1[NIngresos2017$.id == "Abril"] <- "2017-04-30"
NIngresos2017$Fecha1[NIngresos2017$.id == "Mayo"] <- "2017-05-30"
NIngresos2017$Fecha1[NIngresos2017$.id == "Junio"] <- "2017-06-30"
NIngresos2017$Fecha1[NIngresos2017$.id == "Julio"] <- "2017-07-30"
NIngresos2017$Fecha1[NIngresos2017$.id == "Agosto"] <-"2017-08-30"
NIngresos2017$Fecha1[NIngresos2017$.id == "Septiembre"] <- "2017-09-30"
NIngresos2017$Fecha1[NIngresos2017$.id == "Octubre"] <- "2017-10-30"
NIngresos2017$Fecha1[NIngresos2017$.id == "Noviembre"] <- "2017-11-30"
NIngresos2017$Fecha1[NIngresos2017$.id == "Diciembre"] <- "2017-12-30"
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

NIngresos2018 = subset(NIngresos_final, select = -c(Ningresos) )


NIngresos2018$Fecha1<-"."
NIngresos2018$Fecha1[NIngresos2018$.id == "Enero"] <- "2018-01-30"
NIngresos2018$Fecha1[NIngresos2018$.id == "Febrero"] <- "2018-02-28"
NIngresos2018$Fecha1[NIngresos2018$.id == "Marzo"] <- "2018-03-30"
NIngresos2018$Fecha1[NIngresos2018$.id == "Abril"] <- "2018-04-30"
NIngresos2018$Fecha1[NIngresos2018$.id == "Mayo"] <- "2018-05-30"
NIngresos2018$Fecha1[NIngresos2018$.id == "Junio"] <- "2018-06-30"
NIngresos2018$Fecha1[NIngresos2018$.id == "Julio"] <- "2018-07-30"
NIngresos2018$Fecha1[NIngresos2018$.id == "Agosto"] <-"2018-08-30"
NIngresos2018$Fecha1[NIngresos2018$.id == "Septiembre"] <- "2018-09-30"
NIngresos2018$Fecha1[NIngresos2018$.id == "Octubre"] <- "2018-10-30"
NIngresos2018$Fecha1[NIngresos2018$.id == "Noviembre"] <- "2018-11-30"
NIngresos2018$Fecha1[NIngresos2018$.id == "Diciembre"] <- "2018-12-30"

NIngresos2018$Año<-2018
rm(NIngresos, myfiles, NIngresos_final)
################################################################################################################
################################################ PEGUE ##########################################################
##############################################################################################################


VM<-rbind( VM2017, VM2018)
VAC<-rbind( VAC2017, VAC2018)
VAN<-rbind(VAN2017, VAN2018)
NIngresos<-rbind( NIngresos2017,NIngresos2018)
rm(VM2017, VM2018, VAC2017, VAC2018, NIngresos2017, NIngresos2018, VAN2017, VAN2018)
VM$Año<-as.character(VM$Año)
VAC$Año<-as.character(VAC$Año)
NIngresos$Año<-as.character(NIngresos$Año)


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
NIngresos$NumMes[NIngresos$.id == "Enero"] <- 1
NIngresos$NumMes[NIngresos$.id == "Febrero"] <- 2
NIngresos$NumMes[NIngresos$.id == "Marzo"] <- 3
NIngresos$NumMes[NIngresos$.id == "Abril"] <- 4
NIngresos$NumMes[NIngresos$.id == "Mayo"] <- 5
NIngresos$NumMes[NIngresos$.id == "Junio"] <- 6
NIngresos$NumMes[NIngresos$.id == "Julio"] <- 7
NIngresos$NumMes[NIngresos$.id == "Agosto"] <- 8
NIngresos$NumMes[NIngresos$.id == "Septiembre"] <- 9
NIngresos$NumMes[NIngresos$.id == "Octubre"] <- 10
NIngresos$NumMes[NIngresos$.id == "Noviembre"] <- 11
NIngresos$NumMes[NIngresos$.id == "Diciembre"] <- 12


VM_Nal<-subset(VM,  Ciudades== "Total IPC" | Ciudades== "Nacional" | Ciudades== "Total IPC" | Ciudades== "Total Nacional")
VAC_Nal<-subset(VAC,  Ciudades== "Total IPC" | Ciudades== "Nacional" | Ciudades== "Total IPC" | Ciudades== "Total Nacional")
NIngresos_Nal<-subset(NIngresos,  orden== "Total IPC" |  orden== "Nacional" |  orden== "Total IPC" |  orden== "Total Nacional")


################################################################################################################
################################################ GRÁFICAS ##########################################################
##############################################################################################################

####VARIACIÓN MENSUAL 


ggplot(VM_Nal, aes(NumMes, Total, group=Año, color= Año)) +
  geom_line()+scale_x_continuous(breaks=seq(1,12,by=1),
                                 labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(aes(label=Total),position=position_dodge(width=0.9), vjust=-0.25, color = "black")


###VARIACIÓN AÑO CORRIDO 


ggplot(VAC_Nal, aes(NumMes, Total, group=Año, color= Año)) +
  geom_line()+scale_x_continuous(breaks=seq(1,12,by=1),
                                 labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(aes(label=Total),position=position_dodge(width=0.9), vjust=-0.25, color = "black")


##VARIACIÓN POR NIVEL DE INGRESOS

#Mensual
ggplot(NIngresos_Nal, aes(NumMes, Ing_men, group=Año, color= Año)) +
  geom_line()+scale_x_continuous(breaks=seq(1,12,by=1),
                                 labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(aes(label=Ing_men),position=position_dodge(width=0.9), vjust=-0.25, color = "black")

#Anual 
ggplot(NIngresos_Nal, aes(NumMes, Ing_anu, group=Año, color= Año)) +
  geom_line()+scale_x_continuous(breaks=seq(1,12,by=1),
                                 labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(aes(label=Ing_anu),position=position_dodge(width=0.9), vjust=-0.25, color = "black")

#Año corrido 
ggplot(NIngresos_Nal, aes(NumMes, Ing_año, group=Año, color= Año)) +
  geom_line()+scale_x_continuous(breaks=seq(1,12,by=1),
                                 labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(aes(label=Ing_año),position=position_dodge(width=0.9), vjust=-0.25, color = "black")


