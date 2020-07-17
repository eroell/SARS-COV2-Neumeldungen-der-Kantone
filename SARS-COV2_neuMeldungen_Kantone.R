##
## Covid-19 Neumeldungen der Kantone
## Autor: Eljas R�llin
## Data managament mainly based on the HealthCovid19cases.R script from
## https://github.com/statistikZH/covid19monitoring_health_covid19cases on 14.06.2020


##############################################################################
###### Data Management
##############################################################################
## From the Above Source
rm(list =ls())
# Import libraries
library(dplyr) # Version: ‘0.8.5’
library(tidyr) # ‘1.0.2’
library(lubridate) # ‘1.7.4’
library(reshape2)
library(xts)
library(lattice)
#library(hochr)
# My Import
library(ggplot2)


# Number formatting

#Einlesen Fälle
url_cases  <- "https://raw.githubusercontent.com/openZH/covid_19/master/COVID19_Fallzahlen_CH_total.csv"
cases <- read.csv(url(url_cases), header=T, sep=",", stringsAsFactors=FALSE, encoding="UTF-8")
# Schlüsselfile Grossregionen
#setwd('C:/Users/Eljas/Documents/Datenanalysen/covid19_ownScripts')
#grossregion <- read.csv("KantonGrossregion.csv", header=T, sep=",", stringsAsFactors=FALSE, encoding="UTF-8")

#confirmed cases by canton
conf<-with(cases, tapply(ncumul_conf, list(date, abbreviation_canton_and_fl), sum))

#first row of data: 0 except for ticino
conf[1,]<-ifelse(is.na(conf[1,]), 0, conf[1,])

#replace NA with last number before
conf<-na.locf(as.xts(conf))
# decumulate
conf<-apply(conf,2, diff)


###########################################################################
## By me:
## for total cases in Switzerland

# Addiere F�lle der einzelenen Kantone auf
totalCH <- data.frame(totalCasesCH = rowSums(conf))

# "long format", vor allem erhalte ich so das Datum als Spalte
totalCH <- melt(as.matrix(totalCH))

# L�sche mittlere Spalte
totalCH <- totalCH[ , c(1,3)]

# Umbenennung der Spalten
names(totalCH)[names(totalCH) == "Var1"] <- "Datum"

# Behalte nur Daten der letzten Tage, ohne heute
names(totalCH)[names(totalCH) == "value"] <- "GemeldeteNeuinfektionen"
totalCH$Datum <- lubridate::ymd(totalCH$Datum)
totalCH <- filter(totalCH, Datum + 60 > max(Datum) & Datum + 1 <= max(Datum))

###########################################################################


#long format
conf<-melt(as.matrix(conf))
# variables
conf$variable_short<-"faelle_sars_cov2"
conf$variable_long<-"neu gemeldete Anzahl SARS-CoV-2 Fälle"

###########################################################################
## By me:
# Rename coloumn
names(conf)[names(conf) == "Var1"] <- "Datum"
names(conf)[names(conf) == "value"] <- "GemeldeteNeuinfektionen"
# Ignore FL
conf <- filter(conf, Var2 != "FL")


# Only use the last 10 days: (Might alter do ignore also the last 2 days, as potentially incomplete data)
conf <- arrange(conf,Datum)
conf$Datum <- lubridate::ymd(conf$Datum) # A more handy date format
conf <- filter(conf, conf$Datum+14 > max(conf$Datum))

# Ignore today and yesterday, as to prevent incomplete data displaying
conf <- filter(conf, conf$Datum+1 <= max(conf$Datum))

a <- as.Date(max(conf$Datum))
##############################################################################
###### Plotting
##############################################################################
##setwd("C:/Users/Eljas/Documents/Datenanalysen/covid19_ownScripts")
pdf("Neugemeldete Faelle der Kantone.pdf",width=7,height=5)
ggplot(data = conf, aes(x = Datum, y = GemeldeteNeuinfektionen) ) +
  geom_point() +
  facet_wrap(~Var2, ncol = 5) +
  theme(axis.text.x = element_text(angle = 90 , hjust = 0, vjust = 0.5) ) +
  xlab("Datum") +
  ylab("Neugemeldete SARS-COV2 Infektionen")# + 
 # scale_y_discrete( breaks = 0:max(conf$GemeldeteNeuinfektionen))+
 # coord_cartesian(ylim = c(-1, max(conf$GemeldeteNeuinfektionen +1)))

# print(
#   paste(
#     "Verwendet wurden die gesammelten Daten vom statistischen Amts des Kantons Z�rich (Stand ",
#     as.Date( max(conf$Datum) ), 
#     ", unter https://raw.githubusercontent.com/openZH/covid_19/master). Der letzte Tag ist rausgenommen, um keinen unvollst�ndig erfassten Tag drin zu haben", by = ""
#   )
 # )
dev.off()

ggplot(data = totalCH, aes (x = Datum, y = GemeldeteNeuinfektionen)) +
  geom_line() +
  geom_point()
#######################################################################
# ### Ich auskommentiert
# #dead cases by canton
# dead<-with(cases, tapply(ncumul_deceased, list(date, abbreviation_canton_and_fl), sum))
# #replace NA with last number before
# #first row of data (25.2.2020): no dead
# dead[1,]<-ifelse(is.na(dead[1,]), 0,dead[1,])
# #replace NA with last value above
# dead<-na.locf(as.xts(dead))
# #decumulate
# dead<-apply(dead,2, diff)
# #long format
# dead<-melt(as.matrix(dead))
# dead$variable_short<-"verstorbene_sars_cov2"
# dead$variable_long<-"neu gemeldete Anzahl SARS-CoV-2 Verstorbene"
# 
# all<-rbind(conf, dead)
# 
# #Aggregation auf Gesamtschweiz
# chtotal<-with(all, aggregate(value, list(date=Var1, variable_short=variable_short, variable_long=variable_long), sum))
# chtotal$location<-"CH"
# 
# #Aggregation auf die Grossregionen
# all<-merge(all, grossregion, by.x="Var2", by.y="Kanton", all.x=T)
# allreg<-with(all, aggregate(value, list(date=Var1, 
#                                         location=location, 
#                                         variable_short=variable_short, 
#                                         variable_long=variable_long), sum))
# #Beides zusammen
# allreg<-rbind(allreg, chtotal)
# 
# covid19<-data.frame(date=as.POSIXct(paste(allreg$date, "00:00:00", sep=" ")),
#                     value=allreg$x,
#                     topic="Gesundheit",
#                     variable_short=allreg$variable_short,
#                     variable_long=allreg$variable_long,
#                     location=allreg$location,
#                     unit="Anzahl",
#                     source="Kantone, STAT",
#                     update="täglich",
#                     public="ja",
#                     description="https://github.com/statistikZH/covid19monitoring_health_covid19cases")
# 
# #letzten =tage rausnehmen zu sicherheit, um keinen unvollständig erfassten Tag drin zu haben
# covid19<-subset(covid19, date<Sys.Date()-1)
# # nur schweiz
# 
# covid19<-subset(covid19, location%in%c("CH", "Espace Mittelland", "Genferseeregion", "Nordwestschweiz", "Ostschweiz", "TI", "Zentralschweiz", "ZH"))
# 
# 
# write.table(covid19, "Health_covid19cases.csv", sep=",", fileEncoding="UTF-8", row.names = F)
# 
# range(covid19$date)