##Mammoth
mammoth <- read.csv("../Data/MFauna/Mammoth 14C.csv",as.is=T)
Ndates <- dim(mammoth)[1]
c14post_mammoth <- lapply(1:Ndates,function(x){
                     try(calibrate(mammoth[x,"RCYBP"],mammoth[x,"Error"],graph=F)$calib)
                     })

##Equus
equus <- read.csv("../Data/MFauna/Equus 14C.csv",as.is=T,nrow=26)
Ndates <- dim(equus)[1]
c14post_equus <- lapply(1:Ndates,function(x){
                     try(calibrate(equus[x,"RCYBP"],equus[x,"Error"],graph=F)$calib)
                     })

##Mastodon
mastodon <- read.csv("../Data/MFauna/Mastodon 14C.csv",as.is=T,nrow=99)
Ndates <- dim(mastodon)[1]
c14post_mastodon <- lapply(1:Ndates,function(x){
                     try(calibrate(mastodon[x,"RCYBP"],mastodon[x,"Error"],graph=F)$calib)
                     })

##Sabertooth
sabertooth <- read.csv("../Data/MFauna/Sabertooth 14C.csv",as.is=T,nrow=23)
Ndates <- dim(sabertooth)[1]
c14post_sabertooth <- lapply(1:Ndates,function(x){
                     try(calibrate(sabertooth[x,"RCYBP"],sabertooth[x,"Error"],graph=F)$calib)
                     })

##Sloth
sloth <- read.csv("../Data/MFauna/Sloth 14C.csv",as.is=T,nrow=40)
Ndates <- dim(sloth)[1]
c14post_sloth <- lapply(1:Ndates,function(x){
                     try(calibrate(sloth[x,"RCYBP"],sloth[x,"Error"],graph=F)$calib)
                     })

##Mega Fauna All
mfauna <- read.csv(file="../Data/BroughtonWeiztal2018/BroughtonWeiztal2018SPDdata.csv",as.is=T)
Ndates <- dim(mfauna)[1]
c14post_mfauna <- lapply(1:Ndates,function(x){
                     try(calibrate(mfauna[x,"RCYBP"],mfauna[x,"Error"],graph=F)$calib)
                     })
c14post_mfauna <- c14post_mfauna[-c(139,345)]
