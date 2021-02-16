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

NGRIP_d18O_ensemble_long <- gather(as.data.frame(NGRIP_d18O_ensemble),key=Draw,value=d18O,1:100)

#sample the original d18O for overplotting
NGRIP_hires_sample <- as.data.frame(approx(y=NGRIP_hires$d18O,x=NGRIP_hires$GICC05,xout=t_grid))
NGRIP_Cooper <- read.csv("../Data/Climate/Cooper_NGRIP.csv")
NGRIP_Cooper$B2K <- NGRIP_Cooper$BP + 50

p1 <- ggplot(NGRIP_d18O_ensemble_long) +
        geom_path(mapping=aes(y=d18O,x=B2K,group=Draw),colour="steelblue",alpha=0.5) +
        geom_path(data=NGRIP_hires,mapping=aes(x=GICC05,y=d18O),colour="lightblue",alpha=0.5,size=0.5) +
        geom_path(data=NGRIP_Cooper,mapping=aes(x=B2K,y=D18O),colour="white",alpha=0.8,size=0.5) +
        xlim(c(20000,10000)) +
        labs(x="Year B2K") +
        theme_minimal() +
        theme(text = element_text(family="Times", size=12))
#
ggsave(filename="../Images/NGRIP_ensemble.png",
      device = "png",
      height = 10,
      width = 20,
      units = "cm",
      scale = 1.5,
      dpi = 2000)
