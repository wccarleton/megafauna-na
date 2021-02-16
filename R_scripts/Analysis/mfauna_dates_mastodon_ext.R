##Mastodon
mastodon <- read.csv("../Data/MFauna/Mastodon 14C_extended.csv",as.is=T,nrow=106)
Ndates <- dim(mastodon)[1]
c14post_mastodon_ext <- lapply(1:Ndates,function(x){
                     try(calibrate(mastodon[x,"RCYBP"],mastodon[x,"Error"],graph=F)$calib)
                     })
