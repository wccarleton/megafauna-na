NGRIP_Cooper_redux <- read.csv("../Data/Climate/Cooper_NGRIP_age_v2.csv",head=T,as.is=T)
NGRIP_age <- data.frame(lab_ID = NGRIP_Cooper_redux$ID,
                        C14_age = NA,
                        cal_age = NGRIP_Cooper_redux$YBP,
                        error = NGRIP_Cooper_redux$Error1SD,
                        reservoir = NA,
                        depth = NGRIP_Cooper_redux$Depth)
write.csv(NGRIP_age,file="./clam_runs/coop_chrono/cooper_chrono.csv",row.names=F)

NGRIP_Cooper_redux_clam_model <- read.table("./clam_runs/cooper_chrono/cooper_chrono_smooth_spline_ages.txt",head=T)

NGRIP_Cooper <- read.csv("../Data/Climate/Cooper_NGRIP.csv")

NGRIP_Cooper_redux_fun <- approxfun(y=NGRIP_Cooper_redux$d18O,x=NGRIP_Cooper_redux$YBP)

NGRIP_Cooper_resampled <- NGRIP_Cooper_redux_fun(NGRIP_Cooper$BP)

cbind(y=NGRIP_Cooper$D18O x=NGRIP_Cooper_resampled)

temp <- data.frame(y=NGRIP_Cooper$D18O,x=NGRIP_Cooper_resampled,BP=NGRIP_Cooper$BP)

temp <- temp[-which(apply(temp,1,function(x)any(is.na(x)))),]

NGRIP_20year <- read.table(file="../Data/Climate/gicc05-20yr.txt",head=F,skip=136)
NGRIP_ICC05_age <- data.frame(lab_ID = 1:dim(NGRIP_20year)[1],
                        C14_age = NA,
                        cal_age = NGRIP_20year[,1],
                        error = NGRIP_20year[,6]/2,
                        reservoir = NA,
                        depth = NGRIP_20year[,2])

write.csv(NGRIP_age,file="./clam_runs/coop_chrono/cooper_chrono.csv",row.names=F)
