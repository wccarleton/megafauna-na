#CARD_arch <- read.csv("../Data/CARD_anthro_US_Clovis_cleaned.csv",as.is=T)
CARD_arch <- read.csv("../Data/BroughtonWeiztal2018/InputData/CARD_studyarea_cleaned_simplified.csv",as.is=T)
CARD_arch <- CARD_arch[-which(apply(CARD_arch,1,function(x)any(is.na(x)))),]
#CARD_arch_Clovis <- subset(CARD_arch, Normalized.age > 6000 & Normalized.age < 12500)
Ndates <- dim(CARD_arch)[1]
c14post_arch <- lapply(1:Ndates,function(x){
                     try(calibrate(CARD_arch[x,"Normalized.age"],CARD_arch[x,"NA.Sigma"],graph=F)$calib)
                     })
#
