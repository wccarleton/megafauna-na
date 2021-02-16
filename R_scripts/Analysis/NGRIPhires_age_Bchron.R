NGRIP_hires <- read.csv(file="../Data/Climate/NGRIP_d18O_and_dust_5cm.csv",head=T)
names(NGRIP_hires) <- c("Depth","d18O","Dust","GICC05","MCE")
NGRIP_hires <- subset(NGRIP_hires,B2K <= 21000)
chron_anchors <- seq(1,dim(NGRIP_hires)[1],100)
NGRIP_Bchron <- Bchronology(ages = NGRIP_hires$GICC05[chron_anchors],
                        ageSds = round(NGRIP_hires$MCE[chron_anchors]/2,0),
                        positions = NGRIP_hires$Depth[chron_anchors],
                        positionThicknesses = c(0.5,diff(NGRIP_hires$Depth[chron_anchors])),
                        predictPositions = NGRIP_hires$Depth,
                        calCurves=rep("normal",length(NGRIP_hires$GICC05[chron_anchors])))


#quick comparison
NGRIP_d18O_Bchron <- approx(x=summary(NGRIP_Bchron)[,4],y=NGRIP_hires[,"d18O"],xout=NGRIP_hires$GICC05)
plot(y=NGRIP_d18O_Bchron$y,x=NGRIP_hires[,"d18O"])

#get internally valid models
NGRIP_Bchron_ensemble <- t(predict(NGRIP_Bchron,newPositions=NGRIP_20year$NGRIP_Depth))

NGRIP_Bchron_ensemble <- NGRIP_Bchron_ensemble[,which(apply(NGRIP_Bchron_ensemble,2,function(x)all(diff(x)>0)))]

#sample event-count universe onto the temporal grids in the ensemble

dates <- as.vector(NGRIP_Bchron_ensemble[,1])

calSampleApprox <- function(x,dates){
    n <- length(x)
    funs <- lapply(x,approxfun)
    y_list <- lapply(1:n,function(j)funs[[j]](dates))
    y_mat <- do.call(cbind,y_list)
    y_mat[which(is.na(y_mat))] <- 0
    return(y_mat)
}

sp <- "mfauna"
c14post <- get(paste("c14post_",sp,sep=""))

resolution <- 1

ndates <- length(c14post)
nsamps <- 1

sample_date_range <- range(unlist(lapply(c14post,function(x)range(x[,1]))))

c14_matrix <- calSampleApprox(c14post[1:ndates],dates)

c14_matrix <- c14_matrix[,-which(apply(c14_matrix,2,function(x)all(x == 0)))]

Dates <- as.vector(NGRIP_Bchron_ensemble[,1])#seq(sample_date_range[1],sample_date_range[2],resolution)

rects_sample <- data.frame(Date=Dates)
for(a in 1:nsamps){
    count_sample <- apply(c14_matrix,2,function(x)sample(Dates,size=1,prob=x))
    count_df <- as.data.frame(table(count_sample))
    names(count_df) <- c("Date","Count")
    rects_sample <- merge(rects_sample,count_df,by="Date",all=T)
}
