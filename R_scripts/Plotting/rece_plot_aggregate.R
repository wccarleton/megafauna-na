#
library(clam)
library(ggplot2)
library(ggpubr)
library(tidyr)
#

#
even <- function(x){return(x%%2==0)}
#

mfauna_sp <- c("mfauna",
                "equus",
                "mammoth",
                "mastodon",
                "sloth",
                "sabertooth",
                "arch")

namen <- c("All Species",
           "Equus",
           "Mammoth",
           "Mastodon",
           "Sloth",
           "Sabertooth",
           "Humans")

plist <- list()

#Climate
load("../Data/Climate/NGRIP_d18O_X.RData")
NGRIP_hires <- read.csv(file="../Data/Climate/NGRIP_d18O_and_dust_5cm.csv",head=T)
names(NGRIP_hires) <- c("Depth","d18O","Dust","GICC05","MCE")
NGRIP_hires <- subset(NGRIP_hires,GICC05 <= 21000)

#globals
range_x <- range(c(range(NGRIP_hires$d18O)-(2*0.01),range(NGRIP_hires$d18O)+(2*0.01)))
x_grid <- seq(range_x[1],range_x[2],0.001)

dates <- cbind(NGRIP_hires$GICC05 - NGRIP_hires$MCE,NGRIP_hires$GICC05 + NGRIP_hires$MCE)
t_grid <- seq(10001,20000,10)
ntimes <- length(t_grid)

#get ensemble sample of d18O functions
NGRIP_d18O_ensemble <- as.data.frame(do.call(cbind,lapply(1:100,function(j)apply(X,1,function(x)sample(x_grid,size=1,prob=x)))))
NGRIP_d18O_ensemble$B2K <- t_grid
NGRIP_d18O_ensemble$BP <- NGRIP_d18O_ensemble$B2K - 50

NGRIP_d18O_ensemble_long <- gather(as.data.frame(NGRIP_d18O_ensemble),key=Draw,value=d18O,1:100)

#sample the original d18O for overplotting
NGRIP_hires_sample <- as.data.frame(approx(y=NGRIP_hires$d18O,x=NGRIP_hires$GICC05,xout=t_grid))
NGRIP_hires$BP <- NGRIP_hires$GICC05 - 50
NGRIP_Cooper <- read.csv("../Data/Climate/Cooper_NGRIP.csv")
NGRIP_Cooper$B2K <- NGRIP_Cooper$BP + 50

#background info polygons
poly_clovis <- data.frame(x=c(13150,13150,12850,12850),
                            y=c(-32,-55,-55,-32))
poly_bollingallerod <- data.frame(x=c(14690,14690,12890,12890),
                            y=c(-32,-55,-55,-32))
poly_youngerdryas <- data.frame(x=c(12900,12900,11700,11700),
                            y=c(-32,-55,-55,-32))

plist[[1]] <- ggplot(NGRIP_d18O_ensemble_long) +
        geom_polygon(data=poly_bollingallerod,
                    mapping=aes(y=y,x=x),
                    fill="red",
                    colour=NA,
                    alpha=0.5) +
        geom_polygon(data=poly_youngerdryas,
                    mapping=aes(y=y,x=x),
                    fill="blue",
                    colour=NA,
                    alpha=0.5) +
        geom_polygon(data=poly_clovis,
                    mapping=aes(y=y,x=x),
                    colour="black",
                    fill=NA) +
        geom_path(mapping=aes(y=d18O,x=BP,group=Draw),
                colour="steelblue",
                alpha=0.5) +
        geom_path(data=NGRIP_hires,
                mapping=aes(x=BP,y=d18O),
                colour="lightblue",
                alpha=0.5,
                size=0.5) +
        geom_path(data=NGRIP_Cooper,
                mapping=aes(x=BP,y=D18O),
                colour="white",
                alpha=0.8,
                size=0.5) +
        coord_cartesian(ylim=c(-51,-32)) +
        labs(y=expression(delta^{18}~"O")) +
        scale_x_reverse(position = "top",limits=c(20000,10000)) +
        theme_minimal() +
        theme(text = element_text(family="Times", size=12),
                plot.margin=unit(c(0,0,0,0),"cm"),
                axis.title.y=element_text(family="Times", size=12),
                axis.title.x=element_blank())

#Megafauna
#

poly_clovis <- data.frame(x=c(13150,13150,12850,12850),
                            y=c(-5,7,7,-5))
poly_bollingallerod <- data.frame(x=c(14690,14690,12890,12890),
                            y=c(-5,7,7,-5))
poly_youngerdryas <- data.frame(x=c(12900,12900,11700,11700),
                            y=c(-5,7,7,-5))

for(j in 1:length(mfauna_sp)){
    load(paste("../Data/rects_sample_",mfauna_sp[j],".RData",sep=""))
    rece <- get(paste("rects_sample_",mfauna_sp[j],sep=""))
    recelong <- gather(rece,
                        key="Sample",
                        value="Count",
                        paste(1:1000))
    if(j < length(mfauna_sp)){
        plist[[j + 1]] <- ggplot(data=recelong) +
            geom_polygon(data=poly_bollingallerod,
                        mapping=aes(y=y,x=x),
                        fill="red",
                        colour=NA,
                        alpha=0.5) +
            geom_polygon(data=poly_youngerdryas,
                        mapping=aes(y=y,x=x),
                        fill="blue",
                        colour=NA,
                        alpha=0.5) +
            geom_polygon(data=poly_clovis,
                        mapping=aes(y=y,x=x),
                        colour="black",
                        fill=NA) +
            geom_col(mapping=aes(y=Count,x=Dates),
                    position="identity",
                    alpha=0.1,
                    colour=NA) +
            #scale_x_reverse(position = "top",limits=c(20000,10000)) +
            xlim(c(20000,10000)) +
            coord_cartesian(ylim = c(0,5)) +
            labs(y="Count") +
            theme_minimal() +
            theme(text = element_text(family="Times", size=12),
                    plot.margin=unit(c(0,0,-0.1,0),"cm"),
                    axis.title.y=element_text(family="Times", size=12),
                    axis.title.x=element_blank(),
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank())
    }else{
        poly_clovis <- data.frame(x=c(13150,13150,12850,12850),
                                    y=c(0,7,7,0))
        poly_bollingallerod <- data.frame(x=c(14690,14690,12890,12890),
                                    y=c(0,7,7,0))
        poly_youngerdryas <- data.frame(x=c(12900,12900,11700,11700),
                                    y=c(0,7,7,0))
        plist[[j + 1]] <- ggplot(data=recelong) +
            geom_polygon(data=poly_bollingallerod,
                        mapping=aes(y=y,x=x),
                        fill="red",
                        colour=NA,
                        alpha=0.5) +
            geom_polygon(data=poly_youngerdryas,
                        mapping=aes(y=y,x=x),
                        fill="blue",
                        colour=NA,
                        alpha=0.5) +
            geom_polygon(data=poly_clovis,
                        mapping=aes(y=y,x=x),
                        colour="black",
                        fill=NA) +
            geom_col(mapping=aes(y=Count,x=Dates),
                    position="identity",
                    alpha=0.1,
                    colour=NA) +
            xlim(c(20000,10000)) +
            #coord_cartesian(ylim = c(0,5)) +
            labs(y="Count",x="Year BP") +
            theme_minimal() +
            theme(text = element_text(family="Times", size=12),
                plot.margin=unit(c(0,0,0,0),"cm"),
                axis.title.y=element_text(family="Times", size=12))
    }
}

###
fig <- ggarrange(plotlist=plist,
        ncol=1,
        nrow=length(mfauna_sp) + 1,
        align="v",
        heights=c(2,1,1,1,1,1,1,1.2))

fig

ggsave(filename="../Images/primary_reces_alltaxa.png",
      device = "png",
      height = 20,
      width = 20,
      units = "cm",
      scale = 1.15,
      dpi = 300)
