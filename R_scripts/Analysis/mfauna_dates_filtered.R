library(clam)

mfauna_filt <- read.csv("../Data/Filtered/mfauna_filt.csv",as.is=T)

##Mammoth
mammoth <- mfauna_filt[grep("Mammuthus\\b",mfauna_filt$Taxon),]
Ndates <- dim(mammoth)[1]
c14post_mammoth_filt <- lapply(1:Ndates,function(x){
                     try(calibrate(mammoth[x,"RCYBP"],mammoth[x,"Error"],graph=F)$calib)
                     })

##Equus
equus <- mfauna_filt[grep("Equus\\b",mfauna_filt$Taxon),]
Ndates <- dim(equus)[1]
c14post_equus_filt <- lapply(1:Ndates,function(x){
                     try(calibrate(equus[x,"RCYBP"],equus[x,"Error"],graph=F)$calib)
                     })

##Mastodon
mastodon <- mfauna_filt[grep("Mammut\\b",mfauna_filt$Taxon),]
Ndates <- dim(mastodon)[1]
c14post_mastodon_filt <- lapply(1:Ndates,function(x){
                     try(calibrate(mastodon[x,"RCYBP"],mastodon[x,"Error"],graph=F)$calib)
                     })
c14post_mastodon_filt <- c14post_mastodon_filt[-34]


##Sabertooth
sabertooth <- mfauna_filt[grep("Smilodon\\b",mfauna_filt$Taxon),]
Ndates <- dim(sabertooth)[1]
c14post_sabertooth_filt <- lapply(1:Ndates,function(x){
                     try(calibrate(sabertooth[x,"RCYBP"],sabertooth[x,"Error"],graph=F)$calib)
                     })

##Sloth
sloth <- mfauna_filt[grep("Nothrotheriops\\b",mfauna_filt$Taxon),]
Ndates <- dim(sloth)[1]
c14post_sloth_filt <- lapply(1:Ndates,function(x){
                     try(calibrate(sloth[x,"RCYBP"],sloth[x,"Error"],graph=F)$calib)
                     })

##Mega Fauna All
Ndates <- dim(mfauna_filt)[1]
c14post_mfauna_filt <- lapply(1:Ndates,function(x){
                     try(calibrate(mfauna_filt[x,"RCYBP"],mfauna_filt[x,"Error"],graph=F)$calib)
                     })
c14post_mfauna_filt <- c14post_mfauna_filt[-68]
