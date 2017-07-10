library(XLConnect)
library(lubridate)
library(ggplot2)
library(plyr)
library(rstudioapi)
library(reshape)
library(RMark)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


##Inlezen gegevens vanuit excel file
Ringgegevens <- readWorksheetFromFile('Input/Databank_brandganzen.xlsx', sheet = "Tbl1 Ringgegevens")
Koppels <- readWorksheetFromFile('Input/Databank_brandganzen.xlsx', sheet = "Tbl2 Koppels")
Waarnemingen <- readWorksheetFromFile('Input/Databank_brandganzen.xlsx', sheet = "Tbl3 Waarnemingen")
Nesten <- readWorksheetFromFile('Input/Databank_brandganzen.xlsx', sheet = "Tbl4 Nesten")
Nestcontroles <- readWorksheetFromFile('Input/Databank_brandganzen.xlsx', sheet = "Tbl5 Nestcontroles")
Uitkipsucces <- readWorksheetFromFile('Input/Databank_brandganzen.xlsx', sheet = "Tbl6 Uitkipsucces")
Kuikenoverleving <- readWorksheetFromFile('Input/Databank_brandganzen.xlsx', sheet = "Tbl7 Kuikenoverleving")

Kuikens <- merge(Kuikenoverleving, Uitkipsucces, by="Nestnummer", all.x=TRUE)
Kuikens$Overleving <- Kuikens$Kuikens / Kuikens$Succes
Kuikens$Controleweek <- week(Kuikens$Controledatum)
Kuikens$Uitkipweek <- week(Kuikens$Uitkipdatum)
Kuikens$Week <- Kuikens$Controleweek - Kuikens$Uitkipweek
Nesten$Duidelijk[is.na(Nesten$Duidelijk)] <- "ja"

###Hieronder worden stap voor stap de verschillende parameters berekend nodig voor het populatiemodel

###1. Berekenen Clutch Size##
#############################

##Enkel brandganzen, duidelijke en bebroede nesten
ClutchSize <- subset(Nesten, Soort=="Brandgans")
ClutchSize <- subset(ClutchSize, Duidelijk!="nee" & Bebroed=="ja")
##Koppelen met verschillende controles
ClutchSize <- merge(ClutchSize, Nestcontroles, by="Nestnummer")
ClutchSize$Jaar <- year(ClutchSize$Controledatum)
##Voorlopig wordt gewoon maximaal aantal eieren genomen als clutch size
##Gemiddelde wordt berekend per jaar met standaarddeviatie
ClutchSize <- ddply(ClutchSize, c("Nestnummer","Jaar"), summarize,
                    Nestgrootte=max(Eieren))
ClutchSize_jaar <- ddply(ClutchSize, c("Jaar"), summarize,
                         clutch.size = mean(Nestgrootte, na.rm=TRUE),
                         sd = sd(Nestgrootte, na.rm=TRUE))
##Toevoegen totaal over alle jaren
ClutchSize_jaar <- rbind(ClutchSize_jaar, c("Totaal", 
                                            mean(ClutchSize$Nestgrootte, na.rm=TRUE), 
                                            sd(ClutchSize$Nestgrootte, na.rm=TRUE)))

###2. Berekenen Hatch Succes##
##############################

##Verder bouwen op CluchtSize, koppelen met uikipsucces, zonder mislukte nesten
HatchSucces <- merge(ClutchSize, Uitkipsucces, by="Nestnummer")
HatchSucces$Mislukt <- as.numeric(HatchSucces$Mislukt)
HatchSucces$Uitkipsucces <- (HatchSucces$Nestgrootte-HatchSucces$Mislukt)/HatchSucces$Nestgrootte
##Gemiddelde wordt berekend per jaar met standaarddeviatie
HatchSucces_jaar <- ddply(HatchSucces, c("Jaar"), summarize,
                         hatch.succes = mean(Uitkipsucces, na.rm=TRUE),
                         sd = sd(Uitkipsucces, na.rm=TRUE))
##Toevoegen totaal over alle jaren
HatchSucces_jaar <- rbind(HatchSucces_jaar, c("Totaal", 
                                              mean(HatchSucces$Uitkipsucces, na.rm=TRUE),
                                              sd(HatchSucces$Uitkipsucces, na.rm=TRUE)))

###3. Kuikenoverleving##
########################

##Omdat er soms tussentijds geteld is wordt telkens maar 1 telling per week weerhouden,
##als er meerdere zijn voor een bepaalde telweek wordt degene met de laatste datum genomen.
##Op die manier wordt voor elk nestnummer voor elke beschikbare telweek een aantal bekomen.

Kuikens2 <- subset(Kuikens, !is.na(Nestnummer))
Kuikens2$Nestnummer <- as.factor(Kuikens2$Nestnummer)
KuikensPerweek <- data.frame(Nestnummer=factor(levels=unique(Kuikens2$Nestnummer)), 
                             Week=factor(levels=unique(Kuikens$Week)), 
                             Kuikenaantal=numeric(0))
for (i in levels(Kuikens2$Nestnummer)){
  data <- subset(Kuikens2, Nestnummer==i)
  data$Week2 <- factor(data$Week)
  for (j in levels(data$Week2)){
    data2 <- subset(data, Week2==j)
    Controledatum <- as.Date("2000-01-01")
    for (k in 1:nrow(data2)){
      if((data2[k,]$Controledatum > Controledatum)){Kuikenaantal <- data2[k,]$Kuikens}
      Controledatum <- data2[k,]$Controledatum
    }
    tmp <- setNames(data.frame(as.factor(i),as.factor(j),Kuikenaantal), names(KuikensPerweek))
    KuikensPerweek <- rbind(KuikensPerweek,tmp)
  }
}

##In een eerste simpele benadering nemen we gewoon het laatste getelde aantal per nest, dat minimum week 3 overschrijdt
KuikensPerweek$Week <- as.numeric(KuikensPerweek$Week)
ChickSurvival <- ddply(KuikensPerweek, "Nestnummer", transform, maxweek=max(Week))
ChickSurvival <- subset(ChickSurvival, Week==maxweek)
ChickSurvival$Overleving <- as.numeric(NA)
for (i in 1:nrow(ChickSurvival)){
      if (ChickSurvival[i,"maxweek"]>3){
  ChickSurvival[i,"Overleving"] <- ChickSurvival[i,"Kuikenaantal"]
  } else if (ChickSurvival[i,"Kuikenaantal"]==0){
    ChickSurvival[i,"Overleving"] <- ChickSurvival[i,"Kuikenaantal"]
  }
}
ChickSurvival <- subset(ChickSurvival, !is.na(Overleving))
ChickSurvival <- merge(ChickSurvival,ClutchSize, by="Nestnummer")
ChickSurvival$ChickSurvival <- ChickSurvival$Overleving/ChickSurvival$Nestgrootte
ChickSurvival$ChickSurvival[ChickSurvival$ChickSurvival>1] <- 1

ChickSurvival_jaar <- ddply(ChickSurvival, c("Jaar"), summarize,
                          surv.chick = mean(ChickSurvival, na.rm=TRUE),
                          sd=sd(ChickSurvival, na.rm=TRUE))
ChickSurvival_jaar <- rbind(ChickSurvival_jaar, c("Totaal", 
                                                  mean(ChickSurvival$ChickSurvival, na.rm=TRUE),
                                                  sd(ChickSurvival$ChickSurvival, na.rm=TRUE)))
ChickSurvival_jaar <- rbind(c("2014", NA, NA), ChickSurvival_jaar)

##Tweede methode is Burnham model gebruiken voor kuikenoverleving
##Problemen, dieren zijn niet individueel gemarkeerd, maar behoren tot een nest, we laten gewoon telkens het laatste kuiken
##in de rij sterven
tmp <- HatchSucces[,c(1,2,5)]
tmp[,2] <- 0
colnames(tmp) <- colnames(KuikensPerweek)
tmp <- rbind (KuikensPerweek, tmp)
Markdata <- cast(tmp, Nestnummer~Week)
colnames(Markdata) <- c("Nest","w0","w1","w2","w3","w4","w5","w6","w7","w8","w9")
Markdata <- subset(Markdata, !is.na(w0))
Markdata <- subset(Markdata, !is.na(apply(Markdata[,c(3:11)], 1, mean, na.rm=TRUE)))
Markdata <- subset(Markdata, w0>0)

x <- 7
##lege template voor file waar Mark mee werkt
chdata <- NULL

##scriptje om data per nest om te zetten in LDLDLD data per kuiken in die nesten
for (i in sort(unique(Markdata$Nest))){
  temp <- subset(Markdata, Nest==i)
  temp2 <- data.frame(matrix(NA, nrow=temp$w0, ncol=2*x))
  temp2[,1] <- 1
  z <- temp$w0
  for (j in (seq(from=2, to=2*x, by=2))){
    y <- temp[1,j/2+2]
    if(is.na(y)){
      temp2[,j]<- 0
      temp2[,j+1]<-0
    } else {
      if(y>z){
        y<-z
      } else {}
      for (k in 0:y){
        temp2[k,j] <- 0
        temp2[k,j+1] <-1
      }
      if(y==z){
      } else {
        for (l in (y+1):z){
          temp2[l,j] <- 1
          temp2[l,j+1] <-0
        }
      }
        z<-y
    }
  }
  temp2[,2*x+1] <-NULL
  temp2[is.na(temp2)] <- 0
  chdata <- rbind(chdata, temp2)
}

##Omzetten van verschillen de kolommen naar enkele LDLDLD data
chdata$ch <- with(chdata, paste(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14, sep="")) 
chdata <- chdata[c("ch")]

##Toepassen Mark met Burnham model
data.prc <- process.data(data=chdata, model = "Burnham")

data.dll <- make.design.data(data.prc, parameters = list(S = list(formula = ~1),
                                                         p = list(formula = ~1),
                                                         r = list(formula = ~1),
                                                         F = list(formula = ~1)))
data2 <- make.mark.model(data.prc,data.dll)

run.mark.model(data2)
modBH <- mark(data.prc,
              model.parameters = list(S = list(formula = ~1),
                                      p = list(formula = ~1),
                                      r = list(formula = ~1),
                                      F = list(formula = ~1)))

summary(modBH, showall = FALSE, se = TRUE, brief = FALSE)
##NOGAL EEN VERSCHIL, MAAR IN DIT MODEL ZITTEN TEVEEL PARAMETERS, VERDER BEKIJKEN...

###4. Juvenile survival##
#########################

###Gebruik van Mark voor berekenen overleving jongen geringd in 2015 en 2016
###Er zijn ook dieren geringd in de jaren daarvoor maar we werken enkel met kleurringen zodat kans op terugmelding gelijk is
JuvSurv <- subset(Ringgegevens, Leeftijdscategorie=="Pullus" & Soort=="Brandgans" & !is.na(Kleurring))
JuvSurv$Ringjaar <- year(JuvSurv$Datum.vangst)
JuvSurv <- JuvSurv[,c(2,4,10)]
JuvSurvtmp <- data.frame(Kleurring=rep(levels(factor(JuvSurv$Kleurring)),3), 
                         Jaar=c(rep(2015, length(levels(factor(JuvSurv$Kleurring)))),
                                    rep(2016, length(levels(factor(JuvSurv$Kleurring)))),
                                        rep(2017, length(levels(factor(JuvSurv$Kleurring))))))
Terugmeld <- Waarnemingen
Terugmeld$Jaar <- year(Terugmeld$Datum)
Terugmeld2 <- ddply(Terugmeld, c("Kleurring", "Jaar"), summarize, 
                   Aantal = length(Datum))
JuvSurvtmp <- merge(JuvSurvtmp, Terugmeld2, by=c("Kleurring","Jaar"), all.x=TRUE)
JuvSurvtmp$Resight[JuvSurvtmp$Aantal>0] <- 1
JuvSurvtmp$Resight[is.na(JuvSurvtmp$Aantal)] <- 0

JuvSurvtmp2 <- cast(JuvSurvtmp, Kleurring~Jaar, max)

###Per Jaar
JuvSurv$Kleurring <- factor(JuvSurv$Kleurring)
JuvenileSurvival_jaar <- data.frame(Jaar = factor(), surv.juv=numeric(0))
for (i in 2015:2016){
  JuvSurv2 <- subset(JuvSurv, Ringjaar==i)
  JuvSurv2 <- merge(JuvSurv2, JuvSurvtmp2, by="Kleurring", all.x=TRUE)
  JuvSurv2 <- JuvSurv2[,c(1,4:6)]
  chdata <- JuvSurv2
  colnames(chdata) <- c("Ring","X2015","X2016","X2017")
  var <- paste("X",i, sep="") 
  chdata[var] <- 1
  chdata$ch <- with(chdata, paste(X2015,X2016,X2017, sep="")) 
  chdata <- chdata[c("ch")]

  data.prc <- process.data(data=chdata, model = "CJS")

  data.dll <- make.design.data(data.prc, parameters = list(Phi = list(formula = ~time),
                                                         p = list(formula = ~1)))

  data2 <- make.mark.model(data.prc,data.dll)

  mod <- mark(data.prc,
              model.parameters = list(Phi= list(formula = ~time),
                                      p = list(formula = ~1)))

  JuvenileSurvival_jaar <- rbind(JuvenileSurvival_jaar, c(i,mod$results$real$estimate[1]))
}
colnames(JuvenileSurvival_jaar) <- c("Jaar", "surv.juv")
JuvenileSurvival_jaar$surv.juv <- as.numeric(JuvenileSurvival_jaar$surv.juv)
JuvenileSurvival_jaar <- rbind(JuvenileSurvival_jaar, c("Totaal", mean(JuvenileSurvival_jaar$surv.juv, na.rm=TRUE)))
JuvenileSurvival_jaar <- rbind(c("2014", NA),JuvenileSurvival_jaar)

##MOET BETER KUNNEN... COHORTEMODEL ZODAT OOK OVERLEVING VAN 2e naar 3e levensjaar eruit kan...
#JuvSurv$Kleurring <- factor(JuvSurv$Kleurring)
#JuvenileSurvival_jaar <- data.frame(Jaar = factor(), surv.juv=numeric(0))

#JuvSurv2 <- JuvSurv
#JuvSurv2 <- merge(JuvSurv2, JuvSurvtmp2, by="Kleurring", all.x=TRUE)
#JuvSurv2 <- JuvSurv2[,c(1,3:6)]
#chdata <- JuvSurv2
#colnames(chdata) <- c("Ring","Ringjaar","X2015","X2016","X2017")
#chdata$X2015[chdata$Ringjaar==2015] <- 1
#chdata$X2016[chdata$Ringjaar==2016] <- 1
#chdata$ch <- with(chdata, paste(X2015,X2016,X2017, sep="")) 
#chdata$Ringjaar <- factor(chdata$Ringjaar)  
#data.prc <- process.data(data=chdata, model = "CJS", groups="Ringjaar")
#data.dll <- make.design.data(data.prc, parameters = list(Phi = list(formula = ~time+Ringjaar),
                                                           p = list(formula = ~1)))
#data2 <- make.mark.model(data.prc,data.dll)
  
#mod <- mark(data.prc,
#              model.parameters = list(Phi= list(formula = ~time+Ringjaar),
#                                      p = list(formula = ~1)))
  
###5. Adult survival##
######################

###Gebruik van Mark voor berekenen overleving adulten geringd in 2015
###Er zijn ook dieren geringd in de jaren daarvoor maar we werken enkel met kleurringen zodat kans op terugmelding gelijk is
AdSurv <- subset(Ringgegevens, Leeftijdscategorie=="Adult" & Soort=="Brandgans" & !is.na(Kleurring))
AdSurv$Ringjaar <- year(AdSurv$Datum.vangst)
AdSurv <- AdSurv[,c(2,10)]
AdSurvtmp <- data.frame(Kleurring=rep(levels(factor(AdSurv$Kleurring)),3), 
                         Jaar=c(rep(2015, length(levels(factor(AdSurv$Kleurring)))),
                                rep(2016, length(levels(factor(AdSurv$Kleurring)))),
                                rep(2017, length(levels(factor(AdSurv$Kleurring))))))
Terugmeld <- Waarnemingen
Terugmeld$Jaar <- year(Terugmeld$Datum)
Terugmeld2 <- ddply(Terugmeld, c("Kleurring", "Jaar"), summarize, 
                    Aantal = length(Datum))
AdSurvtmp <- merge(AdSurvtmp, Terugmeld2, by=c("Kleurring","Jaar"), all.x=TRUE)
AdSurvtmp$Resight[AdSurvtmp$Aantal>0] <- 1
AdSurvtmp$Resight[is.na(AdSurvtmp$Aantal)] <- 0

AdSurvtmp2 <- cast(AdSurvtmp, Kleurring~Jaar, max)
AdSurvtmp2 <- merge(AdSurvtmp2,AdSurv, by="Kleurring")
colnames(AdSurvtmp2) <- c("Ring","X2015","X2016","X2017","Ringjaar")
AdSurvtmp2$X2015[AdSurvtmp2$Ringjaar==2015]<-1
AdSurvtmp2 <- subset(AdSurvtmp2,Ringjaar==2015)

###Per Jaar
chdata <- AdSurvtmp2
chdata$ch <- with(chdata, paste(X2015,X2016,X2017, sep="")) 
chdata <- chdata[c("ch")]
  
data.prc <- process.data(data=chdata, model = "CJS")
  
data.dll <- make.design.data(data.prc, parameters = list(Phi = list(formula = ~1),
                                                           p = list(formula = ~1)))
  
data2 <- make.mark.model(data.prc,data.dll)
  
mod <- mark(data.prc,
            model.parameters = list(Phi= list(formula = ~1),
                                      p = list(formula = ~1)))

AdultSurvival_jaar <- data.frame(Jaar = factor(), surv.ad=numeric(0))
AdultSurvival_jaar <- rbind(AdultSurvival_jaar, c(2015,mod$results$real$estimate[1]))
AdultSurvival_jaar <- rbind(AdultSurvival_jaar, c(2016,mod$results$real$estimate[1]))
AdultSurvival_jaar <- rbind(AdultSurvival_jaar, c("Totaal",mod$results$real$estimate[1]))
colnames(AdultSurvival_jaar) <- c("Jaar", "surv.ad")
AdultSurvival_jaar$surv.ad <- as.numeric(AdultSurvival_jaar$surv.ad)
AdultSurvival_jaar <- rbind(c("2014", NA),AdultSurvival_jaar)


PopModel <- join_all(list(ClutchSize_jaar, HatchSucces_jaar, ChickSurvival_jaar, JuvenileSurvival_jaar,AdultSurvival_jaar), by="Jaar")



