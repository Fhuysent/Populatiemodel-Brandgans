library(INBOtheme)
library(tidyverse)

##Basismodel
Population <- data.frame(age=character(), year=numeric(), number=numeric(), run=numeric())

for (j in 1:50){
  Newdata <- data.frame(age=rep(factor(c("adult","subadult")),31),
                        year = rep(2020:2050, by=1,each=2),
                        number = rep(0,62),
                        run = j)
  
  Startpopulation <- c(600,200)
  FractionReproducing <- 0.70
  Sexratio <- 0.55
  
  Newdata$number[Newdata$year==2020 & Newdata$age=="adult"] <- Startpopulation[1]
  Newdata$number[Newdata$year==2020 & Newdata$age=="subadult"] <- Startpopulation[2]
  Newdata$number[Newdata$year==2020 & Newdata$age=="juvenile"] <- Startpopulation[3]
  
  for (k in 2021:2050){
    Number_nests <- round(Newdata$number[Newdata$year==k-1 & Newdata$age=="adult"]*FractionReproducing*Sexratio)
    Number_nests <- ifelse(Number_nests>2500,2500, ifelse(Number_nests<1,1,Number_nests))
    NestData <- data.frame(Nestsize = round(rnorm(Number_nests,4.06,1))) %>%
      mutate(Goslings = as.numeric(0)) %>%
      mutate(Fledglings = as.numeric(0))
    NestData$Nestsize[NestData$Nestsize<0] <-0
    for (i in 1:nrow(NestData)){
      Nestsize <- NestData$Nestsize[i]
      Succesprob <- exp(0.37+(0.63*Nestsize))/(exp(0.37+(0.63*Nestsize))+1)
      Failure <- rbinom(1,1,Succesprob)
      hs <- rnorm(1,0.85,0.21)
      hs <- ifelse(hs>1, 1, (ifelse(hs<0, 0, hs)))
      goslings <- rbinom(1, Nestsize, hs)
      goslings <- ifelse(Failure==1, ifelse(goslings==0,1,goslings), 0)
      NestData$Goslings[i] <- goslings
      year <- sample(size=1,c("good","normal"), prob=c(0.25,0.75), replace=T)
      cs <- ifelse(year=="good", rnorm(1,0.70,0.04), rnorm(1,0.50,0.10))
      cs <- ifelse(cs>1, 1, (ifelse(cs<0, 0, cs)))
      Chicksucces <- exp(-3.232+(1.085*goslings))/(exp(-3.232+(1.085*goslings))+1)
      Chickfailure <- rbinom(1,1,Chicksucces)
      fledglings <- rbinom(1, goslings, cs)*Chickfailure
      NestData$Fledglings[i] <- fledglings
      remove(Nestsize, Succesprob, i, hs, Failure, goslings, fledglings)
    }
    JuvSurv <- rnorm(1,0.78,0.04)
    SubadultSurv <- rnorm(1,0.88,0.05)
    AdultSurv <- rnorm(1,0.93,0.02)
    JuvSurv <- ifelse(JuvSurv>1, 1, (ifelse(JuvSurv<0, 0, JuvSurv)))
    SubadultSurv <- ifelse(SubadultSurv>1, 1, (ifelse(SubadultSurv<0, 0, SubadultSurv)))
    AdultSurv <- ifelse(AdultSurv>1, 1, (ifelse(AdultSurv<0, 0, AdultSurv)))
    SubadultNew <- round(sum(NestData$Fledglings) * JuvSurv)
    AdultFirst <- round(Newdata$number[Newdata$year==k-1 & Newdata$age=="subadult"] * SubadultSurv)
    AdultOld <- round(Newdata$number[Newdata$year==k-1 & Newdata$age=="adult"] * AdultSurv)
    AdultNew <- AdultFirst+AdultOld
    Newdata$number[Newdata$year==k & Newdata$age=="subadult"] <- SubadultNew
    Newdata$number[Newdata$year==k & Newdata$age=="adult"] <- AdultNew
  }
  Population <- rbind(Population,Newdata)
}

fraction <- Population %>%
  group_by(year,run) %>% 
  summarise(populatie=sum(number))

fraction$lambda <- as.numeric(1)
for (i in 2021:2050){
  lambda = fraction$populatie[fraction$year==i]/fraction$populatie[fraction$year==i-1]
  fraction$lambda[fraction$year==i] <- lambda
} 

Overalllambda_mean <- mean(fraction$lambda[fraction$year>2020],na.rm=T)
Overalllambda_sd <- sd(fraction$lambda[fraction$year>2020],na.rm=T)

lambda <- fraction %>% 
  group_by(run) %>% 
  summarise(mean=mean(lambda),
            sd=sd(lambda))

popdev <- fraction %>%
  group_by(year) %>% 
  summarise(size=mean(populatie),
            max=max(populatie),
            min=min(populatie))

ggplot(popdev, aes(x=year, y=size)) + 
  geom_ribbon(aes(ymin=min, ymax=max), fill = "grey70") +
  geom_line(size=1) +
  theme_inbo() +
  theme(legend.position="bottom") +
  ylab("estimated population size")

ggsave("../Output/PBModel_all.jpg", width = 24, height = 12, units = "cm")

fraction <- Population %>%
  group_by(year,run,age) %>% 
  summarise(populatie=sum(number))

popdev <- fraction %>%
  group_by(year,age) %>% 
  summarise(size=mean(populatie),
            max=max(populatie),
            min=min(populatie))

ggplot(popdev, aes(x=year, y=size, colour=age)) + 
  geom_ribbon(aes(ymin=min, ymax=max), fill = "grey70") +
  geom_line(size=1) +
  theme_inbo() +
  theme(legend.position="bottom") +
  ylab("estimated population size")

ggsave("../Output/PBModel_ages.jpg", width = 24, height = 12, units = "cm")

##Variabelen testen
Population <- data.frame(age=character(), year=numeric(), number=numeric(), run=numeric(), variable=character(),value=numeric())

variables <- data.frame(variable_name=c("Null_var","Nestsize_var","Hs_var","Cs_var","Juvs_var","Subs_var","Ads_var","Frar_var"))

for (m in 1:8){
  Null_var <- 1
  Nestsize_var <- 1
  Hs_var <- 1
  Cs_var <- 1
  Juvs_var <- 1
  Subs_var <- 1
  Ads_var <- 1
  Frar_var <- 1
  variable_name <- as.character(variables[m,1])
  for (n in seq(0.5,1,0.1)){
    assign(variable_name, n)
    for (j in 1:10){
      Newdata <- data.frame(age=rep(factor(c("adult","subadult")),31),
                            year = rep(2020:2050, by=1),
                            number = rep(0,62),
                            run = j,
                            variable=variable_name,
                            value=n)
      Startpopulation <- c(600,150,250)
      FractionReproducing <- 0.70 * Frar_var
      SubadultReproducing <- 0.10
      Sexratio <- 0.55
      
      Newdata$number[Newdata$year==2020 & Newdata$age=="adult"] <- Startpopulation[1]
      Newdata$number[Newdata$year==2020 & Newdata$age=="subadult"] <- Startpopulation[2]
      
      for (k in 2021:2050){
        Number_nests <- round(Newdata$number[Newdata$year==k-1 & Newdata$age=="adult"]*FractionReproducing*Sexratio)
        Number_nests <- ifelse(Number_nests>2500,2500, ifelse(Number_nests<1,1,Number_nests))
        NestData <- data.frame(Nestsize = round(rnorm(Number_nests,4.06,1)*Nestsize_var)) %>%
          mutate(Goslings = as.numeric(0)) %>%
          mutate(Fledglings = as.numeric(0))
        NestData$Nestsize[NestData$Nestsize<0] <- 0
        for (i in 1:nrow(NestData)){
          Nestsize <- NestData$Nestsize[i]
          Succesprob <- exp(0.37+(0.63*Nestsize))/(exp(0.37+(0.63*Nestsize))+1)
          Failure <- rbinom(1,1,Succesprob)
          hs <- rnorm(1,0.85,0.21)
          hs <- ifelse(hs>1, 1, (ifelse(hs<0, 0, hs))) * Hs_var
          goslings <- rbinom(1, Nestsize, hs)
          goslings <- ifelse(Failure==1, ifelse(goslings==0,1,goslings), 0)
          NestData$Goslings[i] <- goslings
          year <- sample(size=1,c("good","normal"), prob=c(0.25,0.75), replace=T)
          cs <- ifelse(year=="good", rnorm(1,0.70,0.04), rnorm(1,0.50,0.10))
          cs <- ifelse(cs>1, 1, (ifelse(cs<0, 0, cs))) * Cs_var
          Chicksucces <- exp(-3.232+(1.085*goslings))/(exp(-3.232+(1.085*goslings))+1)
          Chickfailure <- rbinom(1,1,Chicksucces)
          fledglings <- rbinom(1, goslings, cs)*Chickfailure
          NestData$Fledglings[i] <- fledglings
          remove(Nestsize, Succesprob, i, hs, Failure, goslings, fledglings)
        }
        JuvSurv <- rnorm(1,0.78,0.04)
        SubadultSurv <- rnorm(1,0.88,0.04)
        AdultSurv <- rnorm(1,0.92,0.04)
        JuvSurv <- ifelse(JuvSurv>1, 1, (ifelse(JuvSurv<0, 0, JuvSurv))) * Juvs_var
        SubadultSurv <- ifelse(SubadultSurv>1, 1, (ifelse(SubadultSurv<0, 0, SubadultSurv))) * Subs_var
        AdultSurv <- ifelse(AdultSurv>1, 1, (ifelse(AdultSurv<0, 0, AdultSurv))) * Ads_var
        SubadultNew <- round(sum(NestData$Fledglings) * JuvSurv)
        AdultFirst <- round(Newdata$number[Newdata$year==k-1 & Newdata$age=="subadult"] * SubadultSurv)
        AdultOld <- round(Newdata$number[Newdata$year==k-1 & Newdata$age=="adult"] * AdultSurv)
        AdultNew <- AdultFirst+AdultOld
        Newdata$number[Newdata$year==k & Newdata$age=="subadult"] <- SubadultNew
        Newdata$number[Newdata$year==k & Newdata$age=="adult"] <- AdultNew
      }
      Population <- rbind(Population,Newdata)
    }
  }
}


fraction <- Population %>%
  filter(run==2) %>% 
  group_by(year,value,variable) %>% 
  summarise(populatie=sum(number),na.rm=T)

ggplot(fraction, aes(x=year, y=populatie, colour=factor(value))) + 
  geom_line() +
  facet_wrap(~variable)

ggsave("../Output/PBElasticity_size.jpg", width = 24, height = 12, units = "cm")

fraction$lambda <- as.numeric(1)
for (i in 2021:2050){
  lambda = fraction$populatie[fraction$year==i]/fraction$populatie[fraction$year==i-1]
  fraction$lambda[fraction$year==i] <- lambda
} 

fraction <- fraction %>% 
  mutate(variable=recode(variable, 
                         "Null_var" = "None", 
                         "Nestsize_var" = "Clutch size",
                         "Hs_var" = "Hatching success",
                         "Cs_var" = "Chick survival",
                         "Juvs_var" = "Juvenile survival",
                         "Subs_var" = "Subadult survival",
                         "Ads_var" = "Adult survival",
                         "Frar_var" = "Fraction reproducing"))

elasticity <- fraction %>% 
  filter(year>2020) %>% 
  group_by(variable,value) %>%
  summarise(lambda_mean=mean(lambda),lambda_sd=sd(lambda))


ggplot(elasticity, aes(x=value, y=lambda_mean, colour=variable)) +
  geom_point(position=position_dodge(0.1), size=2)+
  geom_errorbar(aes(ymin=lambda_mean-lambda_sd, ymax=lambda_mean+lambda_sd), position=position_dodge(0.1), width=0, size=1) +
  ylab("mean lambda +/- SD") +
  xlab("fraction of variable") +
  theme_bw() +
  theme(legend.position="bottom", 
        legend.title = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  geom_hline(aes(yintercept=1.12), linetype="dotted") +
  geom_hline(aes(yintercept=1), linetype="dashed")


ggsave("../Output/PBElasticity_lambda.jpg", width = 24, height = 18, units = "cm")

##Nest destruction
Population <- data.frame(age=character(), year=numeric(), number=numeric(), run=numeric(),fraction=numeric())

for (l in seq(0.0,0.9,0.1)){
  for (j in 1:50){
    Newdata <- data.frame(age=rep(factor(c("adult","subadult")),31),
                          year = rep(2020:2050, by=1,each=2),
                          number = rep(0,62),
                          run = j,
                          fraction = l)
    
    Startpopulation <- c(600,200)
    FractionReproducing <- 0.70
    Sexratio <- 0.55
    
    Newdata$number[Newdata$year==2020 & Newdata$age=="adult"] <- Startpopulation[1]
    Newdata$number[Newdata$year==2020 & Newdata$age=="subadult"] <- Startpopulation[2]
    
    for (k in 2021:2050){
      Number_nests <- round(Newdata$number[Newdata$year==k-1 & Newdata$age=="adult"]*FractionReproducing*Sexratio)
      Number_nests <- ifelse(Number_nests>2500,2500, ifelse(Number_nests<1,1,Number_nests))
      fraction <- 1-rnorm(1,l,0.05)
      Number_nests <- round(ifelse(
        fraction>1,1,ifelse(fraction<0,0,fraction))*Number_nests)
      NestData <- data.frame(Nestsize = round(rnorm(Number_nests,4.06,1))) %>%
        mutate(Goslings = as.numeric(0)) %>%
        mutate(Fledglings = as.numeric(0))
      NestData$Nestsize[NestData$Nestsize<0] <-0
      for (i in 1:nrow(NestData)){
        Nestsize <- NestData$Nestsize[i]
        Succesprob <- exp(0.37+(0.63*Nestsize))/(exp(0.37+(0.63*Nestsize))+1)
        Failure <- rbinom(1,1,Succesprob)
        hs <- rnorm(1,0.85,0.21)
        hs <- ifelse(hs>1, 1, (ifelse(hs<0, 0, hs)))
        goslings <- rbinom(1, Nestsize, hs)
        goslings <- ifelse(Failure==1, ifelse(goslings==0,1,goslings), 0)
        NestData$Goslings[i] <- goslings
        year <- sample(size=1,c("good","normal"), prob=c(0.25,0.75), replace=T)
        cs <- ifelse(year=="good", rnorm(1,0.70,0.04), rnorm(1,0.50,0.10))
        cs <- ifelse(cs>1, 1, (ifelse(cs<0, 0, cs)))
        Chicksucces <- exp(-3.232+(1.085*goslings))/(exp(-3.232+(1.085*goslings))+1)
        Chickfailure <- rbinom(1,1,Chicksucces)
        fledglings <- rbinom(1, goslings, cs)*Chickfailure
        NestData$Fledglings[i] <- fledglings
        remove(Nestsize, Succesprob, i, hs, Failure, goslings, fledglings)
      }
      JuvSurv <- rnorm(1,0.78,0.04)
      SubadultSurv <- rnorm(1,0.88,0.05)
      AdultSurv <- rnorm(1,0.93,0.02)
      JuvSurv <- ifelse(JuvSurv>1, 1, (ifelse(JuvSurv<0, 0, JuvSurv)))
      SubadultSurv <- ifelse(SubadultSurv>1, 1, (ifelse(SubadultSurv<0, 0, SubadultSurv)))
      AdultSurv <- ifelse(AdultSurv>1, 1, (ifelse(AdultSurv<0, 0, AdultSurv)))
      SubadultNew <- round(sum(NestData$Fledglings) * JuvSurv)
      AdultFirst <- round(Newdata$number[Newdata$year==k-1 & Newdata$age=="subadult"] * SubadultSurv)
      AdultOld <- round(Newdata$number[Newdata$year==k-1 & Newdata$age=="adult"] * AdultSurv)
      AdultNew <- AdultFirst+AdultOld
      Newdata$number[Newdata$year==k & Newdata$age=="subadult"] <- SubadultNew
      Newdata$number[Newdata$year==k & Newdata$age=="adult"] <- AdultNew
    }
    Population <- rbind(Population,Newdata)
  }
}

fraction <- Population %>%
  group_by(year,run,fraction) %>% 
  summarise(populatie=sum(number))

fraction$lambda <- as.numeric(1)
for (i in 2021:2050){
  lambda = fraction$populatie[fraction$year==i]/fraction$populatie[fraction$year==i-1]
  fraction$lambda[fraction$year==i] <- lambda
} 

lambda <- fraction %>%
  filter(populatie>0) %>%
  group_by(fraction) %>% 
  summarise(mean=mean(lambda),
            sd=sd(lambda))

ggplot(lambda, aes(x=fraction, y=mean)) +
  geom_point(position=position_dodge(0.1), size=2)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position=position_dodge(0.1), width=0, size=1) +
  ylab("mean lambda +/- SD") +
  xlab("% of nests destroyed") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  geom_hline(aes(yintercept=1.12), linetype="dotted") +
  geom_hline(aes(yintercept=1), linetype="dashed")

ggsave("../Output/PBNestreduction.jpg", width = 24, height = 18, units = "cm")

fraction <- Population %>%
  group_by(year,run,fraction) %>% 
  summarise(populatie=sum(number))

popdev <- fraction %>%
  group_by(year,fraction) %>% 
  summarise(size=mean(populatie),
            max=max(populatie),
            min=min(populatie))

ggplot(popdev, aes(x=year, y=size, group=fraction, colour=as.factor(fraction))) + 
  geom_ribbon(aes(ymin=min, ymax=max, group=fraction),fill="grey70") +
  geom_line(size=1) +
  theme_inbo() +
  theme(legend.position="bottom") +
  ylab("estimated population size")

ggsave("../Output/Nestdestruction2.jpg", width = 24, height = 12, units = "cm")

##Moult captures
Population <- data.frame(age=character(), year=numeric(), number=numeric(), run=numeric(),fraction=numeric())

for (l in seq(0.0,0.9,0.1)){
  for (j in 1:50){
    Newdata <- data.frame(age=rep(factor(c("adult","subadult")),31),
                          year = rep(2020:2050, by=1,each=2),
                          number = rep(0,62),
                          run = j,
                          fraction = l)
    
    Startpopulation <- c(600,200)
    FractionReproducing <- 0.70
    Sexratio <- 0.55
    
    Newdata$number[Newdata$year==2020 & Newdata$age=="adult"] <- Startpopulation[1]
    Newdata$number[Newdata$year==2020 & Newdata$age=="subadult"] <- Startpopulation[2]
    
    for (k in 2021:2050){
      Number_nests <- round(Newdata$number[Newdata$year==k-1 & Newdata$age=="adult"]*FractionReproducing*Sexratio)
      Number_nests <- ifelse(Number_nests>2500,2500, ifelse(Number_nests<1,1,Number_nests))
      NestData <- data.frame(Nestsize = round(rnorm(Number_nests,4.06,1))) %>%
        mutate(Goslings = as.numeric(0)) %>%
        mutate(Fledglings = as.numeric(0))
      NestData$Nestsize[NestData$Nestsize<0] <-0
      for (i in 1:nrow(NestData)){
        Nestsize <- NestData$Nestsize[i]
        Succesprob <- exp(0.37+(0.63*Nestsize))/(exp(0.37+(0.63*Nestsize))+1)
        Failure <- rbinom(1,1,Succesprob)
        hs <- rnorm(1,0.85,0.21)
        hs <- ifelse(hs>1, 1, (ifelse(hs<0, 0, hs)))
        goslings <- rbinom(1, Nestsize, hs)
        goslings <- ifelse(Failure==1, ifelse(goslings==0,1,goslings), 0)
        NestData$Goslings[i] <- goslings
        year <- sample(size=1,c("good","normal"), prob=c(0.25,0.75), replace=T)
        cs <- ifelse(year=="good", rnorm(1,0.70,0.04), rnorm(1,0.50,0.10))
        cs <- ifelse(cs>1, 1, (ifelse(cs<0, 0, cs)))
        Chicksucces <- exp(-3.232+(1.085*goslings))/(exp(-3.232+(1.085*goslings))+1)
        Chickfailure <- rbinom(1,1,Chicksucces)
        fledglings <- rbinom(1, goslings, cs)*Chickfailure
        NestData$Fledglings[i] <- fledglings
        remove(Nestsize, Succesprob, i, hs, Failure, goslings, fledglings)
      }
      JuvSurv <- rnorm(1,0.78,0.04)
      SubadultSurv <- rnorm(1,0.88,0.05)
      AdultSurv <- rnorm(1,0.93,0.02)
      JuvSurv <- ifelse(JuvSurv>1, 1, (ifelse(JuvSurv<0, 0, JuvSurv)))
      SubadultSurv <- ifelse(SubadultSurv>1, 1, (ifelse(SubadultSurv<0, 0, SubadultSurv)))
      AdultSurv <- ifelse(AdultSurv>1, 1, (ifelse(AdultSurv<0, 0, AdultSurv)))
      fraction <- ifelse(1-rnorm(1,l,0.05)>1,1,ifelse(1-rnorm(1,l,0.05)<0,0,1-rnorm(1,l,0.05)))
      SubadultNew <- round(sum(NestData$Fledglings) * JuvSurv * fraction)
      AdultFirst <- round(Newdata$number[Newdata$year==k-1 & Newdata$age=="subadult"] * SubadultSurv)
      AdultOld <- round(Newdata$number[Newdata$year==k-1 & Newdata$age=="adult"] * AdultSurv)
      AdultNew <- (AdultFirst+AdultOld) * fraction
      Newdata$number[Newdata$year==k & Newdata$age=="subadult"] <- SubadultNew
      Newdata$number[Newdata$year==k & Newdata$age=="adult"] <- AdultNew
    }
    Population <- rbind(Population,Newdata)
  }
}

fraction <- Population %>%
  group_by(year,run,fraction) %>% 
  summarise(populatie=sum(number))

fraction$lambda <- as.numeric(1)
for (i in 2021:2050){
  lambda = ifelse(fraction$populatie[fraction$year==i-1]==0,0,
                  fraction$populatie[fraction$year==i]/fraction$populatie[fraction$year==i-1])
  fraction$lambda[fraction$year==i] <- lambda
} 

lambda <- fraction %>% 
  group_by(fraction) %>% 
  summarise(mean=mean(lambda),
            sd=sd(lambda))

ggplot(lambda, aes(x=fraction, y=mean)) +
  geom_point(position=position_dodge(0.1), size=2)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position=position_dodge(0.1), width=0, size=1) +
  ylab("mean lambda +/- SD") +
  xlab("% moulting birds captured") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  geom_hline(aes(yintercept=1.12), linetype="dotted") +
  geom_hline(aes(yintercept=1), linetype="dashed")

ggsave("../Output/PBMoultcaptures.jpg", width = 24, height = 18, units = "cm")

##Moult captures absoluut
Population <- data.frame(age=character(), year=numeric(), number=numeric(), run=numeric(),gevangen=numeric())

for (l in seq(100,800,100)){
  CaptJuv <- round(l*0.25)
  CaptSub <- round(l*0.15)
  CaptAd <- round(l*0.6)
  for (j in 1:50){
    Newdata <- data.frame(age=rep(factor(c("adult","subadult")),11),
                          year = rep(2020:2030, by=1,each=2),
                          number = rep(0,22),
                          run = j,
                          gevangen = l)
    
    Startpopulation <- c(600,200)
    FractionReproducing <- 0.70
    Sexratio <- 0.55
    
    Newdata$number[Newdata$year==2020 & Newdata$age=="adult"] <- Startpopulation[1]
    Newdata$number[Newdata$year==2020 & Newdata$age=="subadult"] <- Startpopulation[2]
    
    for (k in 2021:2030){
      Number_nests <- round(Newdata$number[Newdata$year==k-1 & Newdata$age=="adult"]*FractionReproducing*Sexratio)
      Number_nests <- ifelse(Number_nests>2500,2500, ifelse(Number_nests<1,1,Number_nests))
      NestData <- data.frame(Nestsize = round(rnorm(Number_nests,4.06,1))) %>%
        mutate(Goslings = as.numeric(0)) %>%
        mutate(Fledglings = as.numeric(0))
      NestData$Nestsize[NestData$Nestsize<0] <-0
      for (i in 1:nrow(NestData)){
        Nestsize <- NestData$Nestsize[i]
        Succesprob <- exp(0.37+(0.63*Nestsize))/(exp(0.37+(0.63*Nestsize))+1)
        Failure <- rbinom(1,1,Succesprob)
        hs <- rnorm(1,0.85,0.21)
        hs <- ifelse(hs>1, 1, (ifelse(hs<0, 0, hs)))
        goslings <- rbinom(1, Nestsize, hs)
        goslings <- ifelse(Failure==1, ifelse(goslings==0,1,goslings), 0)
        NestData$Goslings[i] <- goslings
        year <- sample(size=1,c("good","normal"), prob=c(0.25,0.75), replace=T)
        cs <- ifelse(year=="good", rnorm(1,0.70,0.04), rnorm(1,0.50,0.10))
        cs <- ifelse(cs>1, 1, (ifelse(cs<0, 0, cs)))
        Chicksucces <- exp(-3.232+(1.085*goslings))/(exp(-3.232+(1.085*goslings))+1)
        Chickfailure <- rbinom(1,1,Chicksucces)
        fledglings <- round(rbinom(1, goslings, cs)*Chickfailure)
        fledglings <- ifelse(fledglings<0,0,fledglings)
        NestData$Fledglings[i] <- fledglings
        remove(Nestsize, Succesprob, i, hs, Failure, goslings, fledglings)
      }
      JuvSurv <- rnorm(1,0.78,0.04)
      SubadultSurv <- rnorm(1,0.88,0.05)
      AdultSurv <- rnorm(1,0.93,0.02)
      JuvSurv <- ifelse(JuvSurv>1, 1, (ifelse(JuvSurv<0, 0, JuvSurv)))
      SubadultSurv <- ifelse(SubadultSurv>1, 1, (ifelse(SubadultSurv<0, 0, SubadultSurv)))
      AdultSurv <- ifelse(AdultSurv>1, 1, (ifelse(AdultSurv<0, 0, AdultSurv)))
      
      SubadultNew <- round(sum(NestData$Fledglings) * JuvSurv) - CaptJuv
      SubadultNew <- ifelse(SubadultNew<0,0,SubadultNew)
      AdultFirst <- round(Newdata$number[Newdata$year==k-1 & Newdata$age=="subadult"] * SubadultSurv) - CaptSub
      AdultFirst <- ifelse(AdultFirst<0,0,AdultFirst)
      AdultOld <- round(Newdata$number[Newdata$year==k-1 & Newdata$age=="adult"] * AdultSurv) - CaptAd
      AdultOld <- ifelse(AdultOld<0,0,AdultOld)
      AdultNew <- (AdultFirst+AdultOld) 
      Newdata$number[Newdata$year==k & Newdata$age=="subadult"] <- SubadultNew
      Newdata$number[Newdata$year==k & Newdata$age=="adult"] <- AdultNew
    }
    Population <- rbind(Population,Newdata)
  }
}

fraction <- Population %>%
  group_by(year,run,gevangen) %>% 
  summarise(populatie=sum(number))

fraction$lambda <- as.numeric(1)
for (i in 2021:2030){
  lambda = ifelse(fraction$populatie[fraction$year==i-1]==0,0,
                  fraction$populatie[fraction$year==i]/fraction$populatie[fraction$year==i-1])
  fraction$lambda[fraction$year==i] <- lambda
} 

lambda <- fraction %>% 
  group_by(gevangen) %>% 
  summarise(mean=mean(lambda),
            sd=sd(lambda))

ggplot(lambda, aes(x=gevangen, y=mean)) +
  geom_point(position=position_dodge(0.1), size=2)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position=position_dodge(0.1), width=0, size=1) +
  ylab("mean lambda +/- SD") +
  xlab("number of moulting birds captured") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  geom_hline(aes(yintercept=1.12), linetype="dotted") +
  geom_hline(aes(yintercept=1), linetype="dashed")

ggsave("../Output/PBMoultcaptures_abs2030.jpg", width = 24, height = 18, units = "cm")

fraction <- Population %>%
  group_by(year,run,gevangen) %>% 
  summarise(populatie=sum(number))

popdev <- fraction %>%
  group_by(year,gevangen) %>% 
  summarise(size=mean(populatie),
            max=max(populatie),
            min=min(populatie))

ggplot(popdev, aes(x=year, y=size, group=gevangen, colour=as.factor(gevangen))) + 
  geom_ribbon(aes(ymin=min, ymax=max, group=gevangen),fill="grey70") +
  geom_line(size=1) +
  theme_inbo() +
  theme(legend.position="bottom") +
  ylab("estimated population size")

ggsave("./Output/Moultcaptures_abs20302.jpg", width = 24, height = 12, units = "cm")

##Combined absoluut
Population <- data.frame(age=character(), year=numeric(), number=numeric(), 
                         run=numeric(), gevangen=numeric(), vernietigd=numeric())

for (m in seq(0,400,100)){
  for (l in seq(0,300,100)){
    for (j in 1:50){
      CaptJuv <- round(l*0.25)
      CaptSub <- round(l*0.15)
      CaptAd <- round(l*0.6)
      DestroyedNests <- m
      Newdata <- data.frame(age=rep(factor(c("adult","subadult")),11),
                            year = rep(2020:2030, by=1,each=2),
                            number = rep(0,22),
                            run = j,
                            gevangen = l,
                            vernietigd = m)
      Startpopulation <- c(600,22)
      FractionReproducing <- 0.70
      Sexratio <- 0.55
      
      Newdata$number[Newdata$year==2020 & Newdata$age=="adult"] <- Startpopulation[1]
      Newdata$number[Newdata$year==2020 & Newdata$age=="subadult"] <- Startpopulation[2]
      
      for (k in 2021:2030){
        Number_nests <- round(Newdata$number[Newdata$year==k-1 & Newdata$age=="adult"]*FractionReproducing*Sexratio)
        Number_nests <- ifelse(Number_nests>2500,2500, ifelse(Number_nests<1,1,Number_nests)) - DestroyedNests
        Number_nests <- ifelse(Number_nests<1,1,Number_nests)
        NestData <- data.frame(Nestsize = round(rnorm(Number_nests,4.06,1))) %>%
          mutate(Goslings = as.numeric(0)) %>%
          mutate(Fledglings = as.numeric(0))
        NestData$Nestsize[NestData$Nestsize<0] <-0
        for (i in 1:nrow(NestData)){
          Nestsize <- NestData$Nestsize[i]
          Succesprob <- exp(0.37+(0.63*Nestsize))/(exp(0.37+(0.63*Nestsize))+1)
          Failure <- rbinom(1,1,Succesprob)
          hs <- rnorm(1,0.85,0.21)
          hs <- ifelse(hs>1, 1, (ifelse(hs<0, 0, hs)))
          goslings <- rbinom(1, Nestsize, hs)
          goslings <- ifelse(Failure==1, ifelse(goslings==0,1,goslings), 0)
          NestData$Goslings[i] <- goslings
          year <- sample(size=1,c("good","normal"), prob=c(0.25,0.75), replace=T)
          cs <- ifelse(year=="good", rnorm(1,0.70,0.04), rnorm(1,0.50,0.10))
          cs <- ifelse(cs>1, 1, (ifelse(cs<0, 0, cs)))
          Chicksucces <- exp(-3.232+(1.085*goslings))/(exp(-3.232+(1.085*goslings))+1)
          Chickfailure <- rbinom(1,1,Chicksucces)
          fledglings <- round(rbinom(1, goslings, cs)*Chickfailure)
          fledglings <- ifelse(fledglings<0,0,fledglings)
          NestData$Fledglings[i] <- fledglings
          remove(Nestsize, Succesprob, i, hs, Failure, goslings, fledglings)
        }
        JuvSurv <- rnorm(1,0.78,0.04)
        SubadultSurv <- rnorm(1,0.88,0.05)
        AdultSurv <- rnorm(1,0.93,0.02)
        JuvSurv <- ifelse(JuvSurv>1, 1, (ifelse(JuvSurv<0, 0, JuvSurv)))
        SubadultSurv <- ifelse(SubadultSurv>1, 1, (ifelse(SubadultSurv<0, 0, SubadultSurv)))
        AdultSurv <- ifelse(AdultSurv>1, 1, (ifelse(AdultSurv<0, 0, AdultSurv)))
        
        SubadultNew <- round(sum(NestData$Fledglings) * JuvSurv) - CaptJuv
        SubadultNew <- ifelse(SubadultNew<0,0,SubadultNew)
        AdultFirst <- round(Newdata$number[Newdata$year==k-1 & Newdata$age=="subadult"] * SubadultSurv) - CaptSub
        AdultFirst <- ifelse(AdultFirst<0,0,AdultFirst)
        AdultOld <- round(Newdata$number[Newdata$year==k-1 & Newdata$age=="adult"] * AdultSurv) - CaptAd
        AdultOld <- ifelse(AdultOld<0,0,AdultOld)
        AdultNew <- (AdultFirst+AdultOld) 
        Newdata$number[Newdata$year==k & Newdata$age=="subadult"] <- SubadultNew
        Newdata$number[Newdata$year==k & Newdata$age=="adult"] <- AdultNew
      }
      Population <- rbind(Population,Newdata)
    }}}

fraction <- Population %>%
  group_by(year,run,gevangen,vernietigd) %>% 
  summarise(populatie=sum(number))

fraction$lambda <- as.numeric(1)
for (i in 2021:2030){
  lambda = ifelse(fraction$populatie[fraction$year==i-1]==0,0,
                  fraction$populatie[fraction$year==i]/fraction$populatie[fraction$year==i-1])
  fraction$lambda[fraction$year==i] <- lambda
} 

lambda <- fraction %>% 
  group_by(vernietigd, gevangen) %>% 
  summarise(mean=mean(lambda),
            sd=sd(lambda))

ggplot(lambda, aes(x=vernietigd, y=mean)) +
  geom_point(position=position_dodge(0.1), size=2)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position=position_dodge(0.1), width=0, size=1) +
  ylab("mean lambda +/- SD") +
  xlab("number of nests destroyed") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  geom_hline(aes(yintercept=1.12), linetype="dotted") +
  geom_hline(aes(yintercept=1), linetype="dashed") +
  facet_wrap(~gevangen)

ggsave("../Output/PBCombinedScenario.jpg", width = 24, height = 18, units = "cm")


###Real life scenario sinds 2015
##Moult captures absoluut
Population <- data.frame(age=character(), year=numeric(), number=numeric(), run=numeric(),gevangen=numeric())
##klopt nog niet, aantal gevangen moet variëren per jaar, niet per run...
for (l in c(187,232,836,614)){
  CaptJuv <- round(l*0.25)
  CaptSub <- round(l*0.15)
  CaptAd <- round(l*0.6)
  for (j in 1:10){
    Newdata <- data.frame(age=rep(factor(c("adult","subadult","juvenile")),4),
                          year = rep(2015:2018, by=1,each=3),
                          number = rep(0,12),
                          run = j,
                          gevangen = l)
    
    Startpopulation <- c(1200,300,500)
    FractionReproducing <- 0.70
    Sexratio <- 0.55
    
    Newdata$number[Newdata$year==2015 & Newdata$age=="adult"] <- Startpopulation[1]
    Newdata$number[Newdata$year==2015 & Newdata$age=="subadult"] <- Startpopulation[2]
    Newdata$number[Newdata$year==2015 & Newdata$age=="juvenile"] <- Startpopulation[3]
    
    for (k in 2016:2018){
      Number_nests <- round(Newdata$number[Newdata$year==k-1 & Newdata$age=="adult"]*FractionReproducing*Sexratio)
      Number_nests <- ifelse(Number_nests>2500,2500, ifelse(Number_nests<1,1,Number_nests))
      NestData <- data.frame(Nestsize = round(rnorm(Number_nests,4.06,1))) %>%
        mutate(Goslings = as.numeric(0)) %>%
        mutate(Fledglings = as.numeric(0))
      NestData$Nestsize[NestData$Nestsize<0] <-0
      for (i in 1:nrow(NestData)){
        Nestsize <- NestData$Nestsize[i]
        Succesprob <- exp(0.37+(0.63*Nestsize))/(exp(0.37+(0.63*Nestsize))+1)
        Failure <- rbinom(1,1,Succesprob)
        hs <- rnorm(1,0.85,0.21)
        hs <- ifelse(hs>1, 1, (ifelse(hs<0, 0, hs)))
        goslings <- rbinom(1, Nestsize, hs)
        goslings <- ifelse(Failure==1, ifelse(goslings==0,1,goslings), 0)
        NestData$Goslings[i] <- goslings
        year <- sample(size=1,c("good","normal"), prob=c(0.25,0.75), replace=T)
        cs <- ifelse(year=="good", rnorm(1,0.70,0.04), rnorm(1,0.50,0.10))
        cs <- ifelse(cs>1, 1, (ifelse(cs<0, 0, cs)))
        Chicksucces <- exp(-3.232+(1.085*goslings))/(exp(-3.232+(1.085*goslings))+1)
        Chickfailure <- rbinom(1,1,Chicksucces)
        fledglings <- round(rbinom(1, goslings, cs)*Chickfailure)-CaptJuv
        fledglings <- ifelse(fledglings<0,0,fledglings)
        NestData$Fledglings[i] <- fledglings
        remove(Nestsize, Succesprob, i, hs, Failure, goslings, fledglings)
      }
      JuvSurv <- rnorm(1,0.78,0.04)
      SubadultSurv <- rnorm(1,0.88,0.05)
      AdultSurv <- rnorm(1,0.93,0.02)
      JuvSurv <- ifelse(JuvSurv>1, 1, (ifelse(JuvSurv<0, 0, JuvSurv)))
      SubadultSurv <- ifelse(SubadultSurv>1, 1, (ifelse(SubadultSurv<0, 0, SubadultSurv)))
      AdultSurv <- ifelse(AdultSurv>1, 1, (ifelse(AdultSurv<0, 0, AdultSurv)))
      
      SubadultNew <- round(Newdata$number[Newdata$year==k-1 & Newdata$age=="juvenile"] * JuvSurv) - CaptSub
      SubadultNew <- ifelse(SubadultNew<0,0,SubadultNew)
      AdultFirst <- round(Newdata$number[Newdata$year==k-1 & Newdata$age=="subadult"] * SubadultSurv)
      AdultOld <- round(Newdata$number[Newdata$year==k-1 & Newdata$age=="adult"] * AdultSurv)
      AdultNew <- round(AdultFirst+AdultOld)-CaptAd
      AdultNew <- ifelse(AdultNew<0,0,AdultNew)
      Newdata$number[Newdata$year==k & Newdata$age=="juvenile"] <- round(sum(NestData$Fledglings))
      Newdata$number[Newdata$year==k & Newdata$age=="subadult"] <- SubadultNew
      Newdata$number[Newdata$year==k & Newdata$age=="adult"] <- AdultNew
    }
    Population <- rbind(Population,Newdata)
  }
}

fraction <- Population %>%
  group_by(year,run,gevangen) %>% 
  summarise(populatie=sum(number))

fraction$lambda <- as.numeric(1)
for (i in 2021:2030){
  lambda = ifelse(fraction$populatie[fraction$year==i-1]==0,0,
                  fraction$populatie[fraction$year==i]/fraction$populatie[fraction$year==i-1])
  fraction$lambda[fraction$year==i] <- lambda
} 

lambda <- fraction %>% 
  group_by(gevangen) %>% 
  summarise(mean=mean(lambda),
            sd=sd(lambda))

ggplot(lambda, aes(x=gevangen, y=mean)) +
  geom_point(position=position_dodge(0.1), size=2)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position=position_dodge(0.1), width=0, size=1) +
  ylab("mean lambda +/- SD") +
  xlab("number of moulting birds captured") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  geom_hline(aes(yintercept=1.12), linetype="dotted") +
  geom_hline(aes(yintercept=1), linetype="dashed")

ggsave("./Output/Moultcaptures_abs2030.jpg", width = 24, height = 18, units = "cm")

