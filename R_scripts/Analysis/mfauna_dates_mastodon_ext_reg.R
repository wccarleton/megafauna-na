##Mastodon
mastodon <- read.csv("../Data/MFauna/Mastodon 14C_extended.csv",as.is=T,nrow=106)
mastodon <- mastodon[grep("Ontario|New York|Pennsylvania|Ohio|Michigan|Indiana|Illinois|Wisconsin|Minnesota",mastodon$State),]
Ndates <- dim(mastodon)[1]
c14post_mastodon_ext_reg <- lapply(1:Ndates,function(x){
                     try(calibrate(mastodon[x,"RCYBP"],mastodon[x,"Error"],graph=F)$calib)
                     })
