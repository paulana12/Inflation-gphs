library(plyr)
library(readr)
library(stringr)
library(readxl)
library(dplyr)
library(tidyr)
library(plyr)
library("writexl")
library(purrr)
library(lubridate)
library(ggplot2)
Sys.setlocale("LC_TIME", "Spanish")
library(ggpubr)
#install.packages("ggthemes") 
library(ggthemes)

rm(list=ls())
graphics.off()

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
################################################2022##########################################################
##############################################################################################################


# Año 2020 Variación MENSUAL  ----------------------------------------------------------------

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2022")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c( "Abril", "Enero", "Febrero", "Marzo")
VM2022<- ldply(myfiles, read_excel, sheet = "4", range = "A7:N31")
VM2022$Año<-2022


VM2022$Fecha1<-"."
VM2022$Fecha1[VM2022$.id == "Enero"] <- "2022-01-30"
VM2022$Fecha1[VM2022$.id == "Febrero"] <- "2022-02-28"
VM2022$Fecha1[VM2022$.id == "Marzo"] <- "2022-03-30"
VM2022$Fecha1[VM2022$.id == "Abril"] <- "2022-04-30"

rm(myfiles )



# Año 2022 Variación AÑO CORRIDO  ----------------------------------------------------------------

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2022")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c(  "Abril", "Enero", "Febrero", "Marzo")
VAC2022<- ldply(myfiles, read_excel, sheet = "5", range = "A7:N31")
VAC2022$Año<-2022

VAC2022$Fecha1<-"."
VAC2022$Fecha1[VAC2022$.id == "Enero"] <- "2022-01-30"
VAC2022$Fecha1[VAC2022$.id == "Febrero"] <- "2022-02-28"
VAC2022$Fecha1[VAC2022$.id == "Marzo"] <- "2022-03-30"
VAC2022$Fecha1[VAC2022$.id == "Abril"] <- "2022-04-30"


rm(myfiles)

# Año 2022 Variación NIVEL INGRESOS  ----------------------------------------------------------------


setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion/2022")
myfiles<- list.files(pattern=NULL, full.names=TRUE)
myfiles <-as.list(myfiles)
names(myfiles) <- c( "Abril", "Enero", "Febrero", "Marzo")
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



NIngresos2022<-NIngresos_final
rm(myfiles, NIngresos_final, NIngresos )


################################################################################################################
################################################ PEGUE ##########################################################
##############################################################################################################
VM<-rbind( VM2019, VM2020, VM2021, VM2022)
VAC<-rbind( VAC2019, VAC2020, VAC2021, VAC2022)
NIngresos<-rbind( NIngresos2019, NIngresos2020, NIngresos2021, NIngresos2022) 
rm(NIngresos2019, NIngresos2020,NIngresos2021, NIngresos2022, VM2019, VM2020, VM2021, VM2022, VAC2019, VAC2020, VAC2021, VAC2022)

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

colnames(VM_Nal)<-c( ".id" , "Ciudades", "Var1", "Var2", "Var3", "Var4", "Var5","Var6", "Var7", "Var8", "Var9", "Var10", "Var11", "Var12", "Var13", "Año", "Fecha1", "NumMes")
colnames(VAC_Nal)<-c( ".id" , "Ciudades", "Var1", "Var2", "Var3", "Var4", "Var5","Var6", "Var7", "Var8", "Var9", "Var10", "Var11", "Var12", "Var13", "Año", "Fecha1", "NumMes")


################################################################################################################
################################################ GRÁFICAS ##########################################################
##############################################################################################################


####################################GRAFICAS POR AÑO ############################################
####VARIACIÓN MENSUAL - total

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion")
svg("Var_mensual_año_total.svg", height=5, width=10)
ggplot(VM_Nal, aes(NumMes, Var13, group=Año, color= Año)) +
  geom_line()+geom_point()+scale_x_continuous(breaks=seq(1,12,by=1),
                     labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(aes(label=Var13),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")+
  annotate("text",x=5  , y=1.5,label="La inflación mensual en\nabril 2022 fue de 1.25%")+
  scale_color_manual(values=c("darkblue", 'dodgerblue','cyan2', "deeppink"))
dev.off()

####VARIACIÓN MENSUAL - alimentos

names(VM_Nal)

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion")
svg("Var_mensual_año_alimentos.svg", height=5, width=10)
ggplot(VM_Nal, aes(NumMes, Var1, group=Año, color= Año)) +
  geom_line()+geom_point()+scale_x_continuous(breaks=seq(1,12,by=1),
                                              labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(aes(label=Var1),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")+
  annotate("text",x=3  , y=4.5,label="La inflación abril de alimentos\n en marzo 2022 fue de 2,75%")
dev.off()
###VARIACIÓN AÑO CORRIDO - total 

svg("Var_ac_año.svg", height=5, width=10)
ggplot(VAC_Nal, aes(NumMes, Var13, group=Año, color= Año)) +
  geom_line()+geom_point()+scale_x_continuous(breaks=seq(1,12,by=1),
                                 labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(aes(label=Var13),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")+
  annotate("text",x=2  , y=5.5,label="La inflación acumulada en \nabril 2022 fue de 5,66%")
dev.off()

###VARIACIÓN AÑO CORRIDO - alimentos 

svg("Var_ac_año_alimentos.svg", height=5, width=10)
ggplot(VAC_Nal, aes(NumMes, Var1, group=Año, color= Año)) +
  geom_line()+geom_point()+scale_x_continuous(breaks=seq(1,12,by=1),
                                              labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(aes(label=Var1),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")+
  annotate("text",x=4  , y=15,label="La inflación acumulada en\nabril 2022 fue de 13,25%")
dev.off()

##VARIACIÓN POR NIVEL DE INGRESOS

# Anual  ----------------------------------------------------------------


ggplot(NIngresos_ANU, aes(NumMes,Variacion , group=interaction(Año, Nivel) , color= Nivel)) +
  geom_line()+geom_point()+scale_x_continuous(breaks=seq(1,12,by=1),
                                              labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(aes(label=Variacion),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")



# Año Corrido -------------------------------------------------------------

ggplot(NIngresos_AC, aes(NumMes, Variacion, group=interaction(Año, Nivel), color= Nivel)) +
  geom_line()+geom_point()+scale_x_continuous(breaks=seq(1,12,by=1),
                                              labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(aes(label=Variacion),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")

# Mensual -----------------------------------------------------------------



ggplot(NIngresos_MEN, aes(NumMes, Variacion, group=interaction(Año, Nivel), color= Nivel)) +
  geom_line()+geom_point()+scale_x_continuous(breaks=seq(1,12,by=1),
                                              labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(aes(label=Variacion),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")



########################## GRAFICAS COMO SERIE ##########################


#VARIACIÓN MENSUAL - total 
svg("Var_mensual_serie_total.svg", height=5, width=10)
ggplot(VM_Nal, aes(Fecha1, Var13, group= Año, color= Año))+
  geom_line()+geom_point()+ scale_x_date(date_labels="%b %Y", breaks = unique(VM_Nal$Fecha1))+
  theme(axis.text.x = element_text(angle = 90, size =10))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %") +
  annotate(geom="text",x=as.Date("2021-10-30") ,y=1.25,label="La inflación mensual en\nabril 2022 fue de 1.25%")+
  geom_text(aes(label=Var13),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")
dev.off()

#VARIACIÓN MENSUAL - alimentos
svg("Var_mensual_serie_alimentos.svg", height=5, width=10)
ggplot(VM_Nal, aes(Fecha1, Var1, group= Año, color= Año))+
  geom_line()+geom_point()+ scale_x_date(date_labels="%b %Y", breaks = unique(VM_Nal$Fecha1))+
  theme(axis.text.x = element_text(angle = 90, size =10))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %") +
  annotate(geom="text",x=as.Date("2022-1-30") ,y=4.4,label="La inflación mensual de alimentos en\nabril 2022 fue de 2,75%")+
  geom_text(aes(label=Var1),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")
dev.off()


#VARIACIÓN AÑO CORRIDO -total 
svg("Var_ac_serie.svg", height=5, width=10)
ggplot(VAC_Nal, aes(Fecha1, Var13, group= Año, color= Año))+
  geom_line()+ geom_point()+scale_x_date(date_labels="%b %Y", breaks = unique(VM_Nal$Fecha1))+
  theme(axis.text.x = element_text(angle = 90, size =10))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  annotate(geom="text",x=as.Date("2022-01-30") ,y=1,label="La inflación acumulada\nen abril 2022\nfue de 5,66%")+
  geom_text(aes(label=Var13),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")
dev.off()

#VARIACIÓN AÑO CORRIDO -alimentos
svg("Var_ac_serie_alimentos.svg", height=5, width=10)
ggplot(VAC_Nal, aes(Fecha1, Var1, group= Año, color= Año))+
  geom_line()+ geom_point()+scale_x_date(date_labels="%b %Y", breaks = unique(VM_Nal$Fecha1))+
  theme(axis.text.x = element_text(angle = 90, size =10))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  annotate(geom="text",x=as.Date("2022-03-30") ,y=16,label="La inflación\nacumulada\nen abril 2022\nfue de 13,25%")+
  geom_text(aes(label=Var1),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")
dev.off()

##VARIACIÓN POR NIVEL DE INGRESOS

# Anual -------------------------------------------------------------------
library(ggplot2)
ggplot(NIngresos_ANU, aes(Fecha1, Variacion, group= Nivel, color= Nivel))+
  geom_line()+ geom_point()+scale_x_date(date_labels="%b %Y", breaks = unique(NIngresos_ANU$Fecha1))+
  theme(axis.text.x = element_text(angle = 90, size =10))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(data=subset(NIngresos_ANU, Nivel=="Pobre"),aes(label=Variacion),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+labs(fill = "Dose (mg)")
  


# Año corrido -------------------------------------------------------------

svg("NING_ac_serie.svg", height=5, width=10)
ggplot(NIngresos_AC, aes(Fecha1, Variacion, group= Nivel, color= Nivel))+
  geom_line()+ geom_point()+scale_x_date(date_labels="%b %Y", breaks = unique(NIngresos_ANU$Fecha1))+
  theme(axis.text.x = element_text(angle = 90, size =10))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(data=subset(NIngresos_AC, Nivel=="Pobre"),aes(label=Variacion),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+
  annotate(geom="text",x=as.Date("2022-02-28") ,y=0.8 ,label="La inflación\nacumulada\npara los pobres\nfue de 6,6%")
dev.off()



# Mensual  ----------------------------------------------------------------

svg("NING_men_serie.svg", height=5, width=10)
ggplot(NIngresos_MEN, aes(Fecha1, Variacion, group= Nivel, color= Nivel))+
  geom_line()+ scale_x_date(date_labels="%b %Y", breaks = unique(NIngresos_ANU$Fecha1))+
  theme(axis.text.x = element_text(angle = 90, size =10))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(data=subset(NIngresos_MEN, Nivel=="Pobre"),aes(label=Variacion),position=position_dodge(width=0.9), vjust=-0.25, color = "black")+ 
  annotate(geom="text",x=as.Date("2021-09-30") ,y=1.5 ,label="La inflación\nacumulada\npara los pobres\nfue de 1,47%")
dev.off()


#########################################
##GRÁFICA 1  ADICIONAL QUE PIDIÓ LAURA 

library(lubridate)
Sys.setlocale("LC_TIME", "Spanish")

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion")
svg("Nueva.svg", height=5, width=10)
ggplot(VM_Nal, aes(Fecha1)) +
         geom_line(aes(y=Var13, col = "Total"))+ geom_point(aes(y=Var13, col = "Total" ))+
         geom_line(aes(y=Var1, col = "Alimentos" ))+ geom_point(aes(y=Var1, col = "Alimentos" ))+
           scale_x_date(date_labels="%b %Y", breaks = unique(VM_Nal$Fecha1))+
  theme(axis.text.x = element_text(angle = 90, size =10))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %") + labs(col = 'Categoría')+
  geom_text(aes(y=Var13, label=Var13), position=position_dodge(width=0.1),vjust=2, cex=3) + 
  geom_text(aes(y=Var1, label=Var1), position=position_dodge(width=0.1),vjust=-1, cex=3)+
  geom_vline(xintercept=as.Date("2020-01-30"), linetype="dashed", color = "deeppink")+
  geom_vline(xintercept=as.Date("2021-01-30"), linetype="dashed", color = "deeppink")+
  geom_vline(xintercept=as.Date("2022-01-01"), linetype="dashed", color = "deeppink")+
  scale_color_manual(values=c('dodgerblue','cyan2'))
dev.off()

##GRÁFICA 2  ADICIONAL QUE PIDIÓ LAURA 
##VARIACIÓN AÑO CORRIDO - total y alimentos

gra1<-ggplot(VAC_Nal, aes(NumMes, Var13, group=Año, color= Año)) +
  geom_line()+geom_point()+scale_x_continuous(breaks=seq(1,12,by=1),
                                              labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(aes(label=Var13),position=position_dodge(width=0.9), vjust=-0.25, color = "black", cex=3)+
  annotate("text",x=4  , y=8,label="La inflación acumulada en \nabril 2022 fue de 5,66%")+
  ylim(1, 20)+scale_color_manual(values=c("darkblue", 'dodgerblue','cyan2', "deeppink"))+
  theme(legend.position="none")

gra2<-ggplot(VAC_Nal, aes(NumMes, Var1, group=Año, color= Año)) +
  geom_line()+geom_point()+scale_x_continuous(breaks=seq(1,12,by=1),
                                              labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
  ggtitle("") + xlab("Fecha") + ylab("")+
  geom_text(aes(label=Var1),position=position_dodge(width=0.9), vjust=-0.25, color = "black" , cex=3)+
  annotate("text",x=4  , y=17,label="La inflación acumulada en\nabril 2022 fue de 13,25%")+
  ylim(1, 20)+scale_color_manual(values=c("darkblue", 'dodgerblue','cyan2', "deeppink"))

figure <- ggarrange(gra1, gra2,
                    labels = c("Total", "Alimentos"),
                    ncol = 2, nrow = 1)

setwd("C:/Users/Paula Neira/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Inflacion")
svg("Nueva2.svg", height=5, width=10)
figure
dev.off()

#GRAFICA 3 ADICIONAL QUE PIDIÓ LAURA 
NIngresos_AC2022<-subset(NIngresos_AC, Año==2022)


library(lubridate)
Sys.setlocale("LC_TIME", "Spanish")

svg("Nueva3.svg", height=5, width=10)
ggplot(NIngresos_AC2022, aes(Fecha1, Variacion, group= Nivel, color= Nivel))+
  geom_line()+ geom_point()+scale_x_date(date_labels="%b %Y", breaks = unique(NIngresos_ANU$Fecha1))+
  theme(axis.text.x = element_text(angle = 90, size =10))+
  ggtitle("") + xlab("Fecha") + ylab("Variación %")+
  geom_text(data=subset(NIngresos_AC2022, Nivel=="Pobre"),aes(label=Variacion),position=position_dodge(width=0.9), vjust=1, color = "black")+
  geom_text(data=subset(NIngresos_AC2022, Nivel=="Total"),aes(label=Variacion),position=position_dodge(width=0.9), vjust=1, color = "black")+
  annotate(geom="text",x=as.Date("2022-03-15") ,y=6 ,label="La inflaciónacumulada para los pobres\nfue de 6,6% y el total fue 5.66%")+
  scale_color_manual(values=c("darkblue", 'dodgerblue','cyan2', "azure4", "deeppink"))
dev.off()