NGRIP_20year <- read.table(file="../Data/Climate/gicc05-20yr.txt",head=F,skip=136)
names(NGRIP_20year) <- c("B2K","NGRIP_Depth","NGRIP_d18O","GRIP_Depth","GRIP_d18O","MCE")
NGRIP_20year <- subset(NGRIP_20year,B2K >= 10000 & B2K < 20000)
chron_anchors <- seq(1,dim(NGRIP_20year)[1],10)
NGRIP_Bchron <- Bchronology(ages = NGRIP_20year$B2K[chron_anchors],
                        ageSds = round(NGRIP_20year$MCE[chron_anchors]/2,0),
                        positions = NGRIP_20year$NGRIP_Depth[chron_anchors],
                        positionThicknesses = c(0.5,diff(NGRIP_20year$NGRIP_Depth[chron_anchors])),
                        predictPositions = NGRIP_20year$NGRIP_Depth,
                        calCurves=rep("normal",length(NGRIP_20year$B2K[chron_anchors])))


#quick comparison
NGRIP_d18O_Bchron <- approx(x=summary(NGRIP_Bchron)[,4],y=NGRIP_20year[,"NGRIP_d18O"],xout=NGRIP_20year$B2K)
plot(y=NGRIP_d18O_Bchron$y,x=NGRIP_20year[,"NGRIP_d18O"])

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
