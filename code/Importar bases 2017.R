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

VAC2017<-rbind(Feb, Mar, Abr, May, Jun, Jul, Ago, Sep, Oct, Nov, Dic)
VAC2017$Año<-2017
rm(Feb, Mar, Abr, May, Jun, Jul, Ago, Sep, Oct, Nov, Dic)

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
NIngresos2017$Año<-2017
NIngresos2017<-NIngresos2017[-c(5,9, 13)]
rm( Ene, Feb, Mar, Abr, May, Jun, Jul, Ago, Sep, Oct, Nov, Dic)



