binaryRECE <- function(x,n){
    xlen <- length(x)
    newMatrix <- matrix(0,ncol=n,nrow=xlen)
    for(j in 1:xlen){
        if(x[j] > 0){
            newMatrix[j,x[j]] <- 1
        }
    }
    return(newMatrix)
}

rece_mat_list <- lapply(2:dim(rece)[2],function(k){
    binaryRECE(rece[,k],n=10)
})

rece_heat_array <- abind(rece_mat_list,along=3)

rece_heat_mat <- apply(rece_heat_array,c(1,2),sum)
rece_heat_df <- as.data.frame(cbind(rece[,1],rece_heat_mat))
names(rece_heat_df) <- c("YBP",as.character(1:10))

rece_heat_long <- gather(rece_heat_df,key="EventCount",value="NMembers",as.character(1:10))

rece_heat_long$EventCount <- as.numeric(rece_heat_long$EventCount)

rece_heat_tibble <- as_tibble(rece_heat_long)

rece_heat_tibble_sub <- subset(rece_heat_tibble,YBP <= 12500 & YBP >= 11000)

p1 <- ggplot(
        rece_heat_tibble_sub,
        aes(x=YBP, y=EventCount, fill=NMembers)
        ) +
        geom_raster(vjust=0) +
        scale_fill_viridis_c(
                    option="B",
                    na.value=rgb(0,0,0,0),
                    begin=0.15,
                    alpha=0.9,
                    trans="log"
                    ) +
        scale_y_continuous(breaks=c(1:10)) +
        coord_cartesian(ylim=c(0,5),xlim=c(11725,11700)) +
        labs(y="Count",x="Year BP") +
        theme_minimal() +
        theme(text = element_text(family="Times", size=12),
            plot.margin=unit(c(0,0,0,0),"cm"),
            axis.title=element_text(family="Times", size=12))
p1
ggsave(filename="../Images/reading_a_rece_plot.png",
      device = "png",
      height = 20,
      width = 20,
      units = "cm",
      scale = 0.5,
      dpi = 300)
