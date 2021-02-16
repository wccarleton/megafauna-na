NGRIP_hires <- read.csv(file="../Data/Climate/NGRIP_d18O_and_dust_5cm.csv",head=T)
names(NGRIP_hires) <- c("Depth","d18O","Dust","GICC05","MCE")
NGRIP_hires <- subset(NGRIP_hires,GICC05 <= 21000)

#globals
range_x <- range(c(range(NGRIP_hires$d18O)-(2*0.01),range(NGRIP_hires$d18O)+(2*0.01)))
x_grid <- seq(range_x[1],range_x[2],0.001)

dates <- cbind(NGRIP_hires$GICC05 - NGRIP_hires$MCE,NGRIP_hires$GICC05 + NGRIP_hires$MCE)
t_grid <- seq(10001,20000,10)
ntimes <- length(t_grid)

Z <- dim(x_matrix)[1]

t_matrix <- t(apply(dates,1,function(x)dunif(x=t_grid,min=x[1],max=x[2])))

xs <- lapply(NGRIP_hires$d18O,function(x)dnorm(x=x_grid,mean=x,sd=0.01))
x_matrix <- do.call(rbind,xs)

d <- 0.05

X <- matrix(nrow=dim(t_matrix)[2],ncol=dim(x_matrix)[2])

for(j in 1:ntimes){
    x_t <- lapply(1:Z,function(z,j)0.05*x_matrix[z,]*t_matrix[z,j],j=j)
    x_t <- do.call(rbind,x_t)
    X[j,] <- colSums(x_t)/(sum(0.05 * t_matrix[,j]))
}

rownames(X) <- t_grid
colnames(X) <- x_grid
temp <- as.data.frame(as.table(X))
names(temp) <- c("Date","d18O","Density")

#sample the original d18O for overplotting
NGRIP_hires_sample <- as.data.frame(approx(y=NGRIP_hires$d18O,x=NGRIP_hires$GICC05,xout=t_grid))
NGRIP_Cooper <- read.csv("../Data/Climate/Cooper_NGRIP.csv")
NGRIP_Cooper$B2K <- NGRIP_Cooper$BP + 50

rownames(X) <- t_grid
colnames(X) <- x_grid
temp <- as.data.frame(as.table(X_scaled))
names(temp) <- c("Date","d18O","Density")
temp$Date <- as.numeric(as.vector(temp$Date))
temp$d18O <- as.numeric(as.vector(temp$d18O))

mybreaks <- as.vector(quantile(temp$Density,probs=c(0,seq(0.5,1,length=10))))
mycols <- grey(seq(1,0,length=4))

p1 <- ggplot() +
        geom_raster(data=temp,aes(x=Date,y=d18O,fill=Density),interpolate=TRUE) +
        scale_fill_gradient(low="white",high="black",na.value="transparent") +
        geom_path(data=NGRIP_hires,mapping=aes(x=GICC05,y=d18O),colour="blue",alpha=0.25,size=0.5) +
        geom_path(data=NGRIP_Cooper,mapping=aes(x=B2K,y=D18O),colour="white",alpha=0.9,size=0.5) +
        xlim(c(20000,10000)) +
        labs(x="Year BP") +
        theme_minimal() +
        theme(text = element_text(family="Times", size=12))
p1
#
ggsave(filename="../Images/NGRIP_proxies.png",
      device = "png",
      height = 10,
      width = 20,
      units = "cm",
      scale = 1.5,
      dpi = 2000)
