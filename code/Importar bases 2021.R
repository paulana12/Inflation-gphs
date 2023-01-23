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

rm(list=ls())
graphics.off()


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


