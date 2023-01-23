library(plyr)
library(readr)
library(stringr)
library(readxl)
library(dplyr)

library("writexl")
#install.packages("purrr")
library(purrr)

rm(list=ls())
graphics.off()


# Año 2019 Variación MENSUAL  ----------------------------------------------------------------

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2019")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c("Abril", "Agosto", "Diciembre", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo", "Noviembre", "Octubre", "Septiemmbre")
A2019VMensual<- ldply(myfiles, read_excel, sheet = "4", range = "A7:N31")

# Año 2019 Variación AÑO CORRIDO  ----------------------------------------------------------------

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2019")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c("Abril", "Agosto", "Diciembre", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo", "Noviembre", "Octubre", "Septiemmbre")
A2019VAC<- ldply(myfiles, read_excel, sheet = "5", range = "A7:N31")

# Año 2019 Variación NIVEL INGRESOS  ----------------------------------------------------------------

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2019")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c("Abril", "Agosto", "Diciembre", "Enero", "Febrero", "Julio", "Junio", "Marzo", "Mayo", "Noviembre", "Octubre", "Septiemmbre")
NIngresos<- ldply(myfiles, read_excel, sheet = "3", range = "A7:P9")
NIngresos<-rename(NIngresos, Pobres_mensual=Pobres)
NIngresos<-rename(NIngresos, Pobres_añocor="...3")
NIngresos<-rename(NIngresos, Pobres_anual="...4")
NIngresos<-rename(NIngresos, Vulnerables_mensual=Vulnerables)
NIngresos<-rename(NIngresos, Vulnerables_añocor="...6")
NIngresos<-rename(NIngresos, Vulnerables_anual="...7")
NIngresos<-rename(NIngresos, ClaseMedia_mensual="Clase media")
NIngresos<-rename(NIngresos, ClaseMedia_añocor="...9")
NIngresos<-rename(NIngresos, ClaseMedia_anual="...10")
NIngresos<-rename(NIngresos, Ingresos_altos_mensual="Ingresos altos")
NIngresos<-rename(NIngresos, Ingresos_altos_añocor="...12")
NIngresos<-rename(NIngresos, Ingresos_altos_anual="...13")
NIngresos<-rename(NIngresos, Total_mensual=Total)











        
