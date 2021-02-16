#load original data
mfauna <- read.csv("./Stewart_et_al_SupplementaryData_Megafauna.csv",as.is=T)

#isolate necessary columns
mfauna <- data.frame(Taxon=mfauna[,1],
                    State=mfauna[,2],
                    Site=mfauna[,3],
                    SampleID=mfauna[,4],
                    RCYBP=mfauna[,5],
                    RCYBPerror=mfauna[,6],
                    Material=mfauna[,10],
                    Method=mfauna[,11],
                    Score=mfauna[,12])

#remove dates for which the posterior will overlap the limits of the
#calibration curve or appear outside it (only two)

mfauna <- mfauna[-c(434,436),]

#add in regions
mfauna[grep("Ontario|New York|Pennsylvania|Ohio|Michigan|Indiana|Illinois|Wisconsin|Minnesota",mfauna$State),"Region"] <- "GreatLakes"
mfauna[grep("California|Nevada|Utah|Arizona|New Mexico",mfauna$State),"Region"] <- "Southwest"

#divide into species groups and get calibrated dates
##Mammoth
mammoth <- subset(mfauna,Taxon=="Mammuthus")
Ndates <- dim(mammoth)[1]
c14post_mammoth <- lapply(1:Ndates,function(x){
                     try(calibrate(mammoth[x,"RCYBP"],mammoth[x,"RCYBPerror"],graph=F)$calib)
                     })

##Mammoth regional
mammoth_SW <- subset(mfauna,Taxon=="Mammuthus" & Region=="Southwest")
Ndates <- dim(mammoth)[1]
c14post_mammoth_SW <- lapply(1:Ndates,function(x){
                     try(calibrate(mammoth[x,"RCYBP"],mammoth[x,"RCYBPerror"],graph=F)$calib)
                     })

mammoth_GL <- subset(mfauna,Taxon=="Mammuthus" & Region=="GreatLakes")
Ndates <- dim(mammoth)[1]
c14post_mammoth_GL <- lapply(1:Ndates,function(x){
                     try(calibrate(mammoth[x,"RCYBP"],mammoth[x,"RCYBPerror"],graph=F)$calib)
                     })

##Equus
equus <- subset(mfauna,Taxon=="Equus")
Ndates <- dim(equus)[1]
c14post_equus <- lapply(1:Ndates,function(x){
                     try(calibrate(equus[x,"RCYBP"],equus[x,"RCYBPerror"],graph=F)$calib)
                     })

##Mastodon
mastodon <- subset(mfauna,Taxon=="Mammut")
Ndates <- dim(mastodon)[1]
c14post_mastodon <- lapply(1:Ndates,function(x){
                     try(calibrate(mastodon[x,"RCYBP"],mastodon[x,"RCYBPerror"],graph=F)$calib)
                     })

##Mastodon Regional
mastodon_SW <- subset(mfauna,Taxon=="Mammut" & Region=="Southwest")
Ndates <- dim(mastodon)[1]
c14post_mastodon_SW <- lapply(1:Ndates,function(x){
                     try(calibrate(mastodon[x,"RCYBP"],mastodon[x,"RCYBPerror"],graph=F)$calib)
                     })

mastodon_GL <- subset(mfauna,Taxon=="Mammut" & Region=="GreatLakes")
Ndates <- dim(mastodon)[1]
c14post_mastodon_GL <- lapply(1:Ndates,function(x){
                     try(calibrate(mastodon[x,"RCYBP"],mastodon[x,"RCYBPerror"],graph=F)$calib)
                     })

##Sabertooth
sabertooth <- subset(mfauna,Taxon=="Smilodon")
Ndates <- dim(sabertooth)[1]
c14post_sabertooth <- lapply(1:Ndates,function(x){
                     try(calibrate(sabertooth[x,"RCYBP"],sabertooth[x,"RCYBPerror"],graph=F)$calib)
                     })

##Sloth
sloth <- subset(mfauna,Taxon=="Nothrotheriops")
Ndates <- dim(sloth)[1]
c14post_sloth <- lapply(1:Ndates,function(x){
                     try(calibrate(sloth[x,"RCYBP"],sloth[x,"RCYBPerror"],graph=F)$calib)
                     })

##Sloth regional
sloth_SW <- subset(mfauna,Taxon=="Nothrotheriops" & Region=="Southwest")
Ndates <- dim(sloth)[1]
c14post_sloth_SW <- lapply(1:Ndates,function(x){
                     try(calibrate(sloth[x,"RCYBP"],sloth[x,"RCYBPerror"],graph=F)$calib)
                     })

sloth_GL <- subset(mfauna,Taxon=="Nothrotheriops" & Region=="GreatLakes")
Ndates <- dim(sloth)[1]
c14post_sloth_GL <- lapply(1:Ndates,function(x){
                     try(calibrate(sloth[x,"RCYBP"],sloth[x,"RCYBPerror"],graph=F)$calib)
                     })

##Mega Fauna All
Ndates <- dim(mfauna)[1]
c14post_mfauna <- lapply(1:Ndates,function(x){
                     try(calibrate(mfauna[x,"RCYBP"],mfauna[x,"RCYBPerror"],graph=F)$calib)
                     })
