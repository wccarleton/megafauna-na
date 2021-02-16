##Volcanic Taphonomic Dates
tapho <- read.csv("../Data/tapho.csv", as.is=T)
tapho <- tapho[-which(is.na(tapho[,7]) | is.na(tapho[,8])),]
Ndates <- dim(tapho)[1]
c14post_tapho <- lapply(1:Ndates,function(x){
                     try(calibrate(tapho[x,"RADIOCARBON.AGE.B.P."],tapho[x,"STD.DEV"],graph=F)$calib)
                     })
