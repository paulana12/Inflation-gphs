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

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2018")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c("Abril", "Agosto", "Diciembre", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo", "Noviembre", "Octubre", "Septiembre")
VM2018<- ldply(myfiles, read_excel, sheet = "Anexo4", range = "A8:K33")
VM2018$Año<-2018
rm(myfiles )


#  Variación AÑO CORRIDO  ----------------------------------------------------------------

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2018")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c("Abril", "Agosto", "Diciembre", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo", "Noviembre", "Octubre", "Septiembre")
VAC2018<- ldply(myfiles, read_excel, sheet = "Anexo5", range = "A8:K33")
VAC2018$Año<-2018
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
NIngresos2018$Año<-2018
rm(NIngresos, myfiles)

