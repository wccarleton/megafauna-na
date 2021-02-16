png(filename="../Images/mfauna_trends.png",
      height = 20,
      width = 20,
      units = "cm",
#      scale = 1.5,
#      dpi = 2000,
      res = 300)

start <- 20000#c(13150,20000)
end <- 11700#13150#sample_date_range[1]#5000

plist <- list()

mfauna_sp <- c("mfauna",
               "equus",
               "mammoth",
               "mastodon",
               "sloth",
               "sabertooth",
               "arch")

calcurve <- copyCalibrationCurve()
names(calcurve) <- c("CalendarBP","C14","Uncertainty")
plist[[1]] <- ggplot(data=calcurve) +
   geom_rect( aes(xmin=11700,xmax=15000,ymin=0,ymax=4),fill="lightgrey", alpha=0.25) +
    geom_path(mapping=aes(y=C14,x=CalendarBP),size=0.5) +
    geom_ribbon(mapping=aes(x=CalendarBP,ymin=C14 - (2.96 * Uncertainty),ymax=C14 + (2.96 * Uncertainty)),fill="skyblue",alpha=0.75) +
    ylim(c(11000,17000)) +
    #scale_x_reverse() +
    xlim(c(start,end)) +
    theme_minimal() +
    theme(text = element_text(family="Times", size=12),
            plot.margin=unit(c(0,1,0,1),"cm"),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())

load(paste("../Data/rects_sample_tapho.RData",sep=""))
rece_tapho <- rects_sample_tapho
trange <- c(end,start)
rece_tapho <- merge(data.frame(Date=trange[1]:trange[2]),rece_tapho,all.x=T,by=1)
Nsub <- seq(1,dim(rece_tapho)[1],10)
rects_df_long <- gather(rece_tapho,key="Sample",value="Count",paste(1:100))

plist[[2]] <- ggplot(data=rects_df_long) +
                  geom_rect( aes(xmin=11700,xmax=15000,ymin=0,ymax=4),fill="lightgrey", alpha=0.25) +
                 geom_col(mapping=aes(y=Count,x=Date),
                        position="identity",
                        alpha=0.5,
                        colour=NA) +
                 #scale_x_reverse() +
                 xlim(c(start,end)) +
                 labs(y="Global Tephra") +
                 theme_minimal() +
                 theme(text = element_text(family="Times", size=12),
                     plot.margin=unit(c(0,1,0,1),"cm"),
                     axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank())

for( n in 1:length(mfauna_sp)){
   load(paste("../Data/rects_sample_",mfauna_sp[n],".RData",sep=""))
   rece_mfauna <- get(paste("rects_sample_",mfauna_sp[n],sep=""))
   trange <- c(end,start)
   rece_mfauna <- merge(data.frame(Date=trange[1]:trange[2]),rece_mfauna,all.x=T,by=1)
   Nsub <- seq(1,dim(rece_mfauna)[1],10)
   rects_df_long <- gather(rece_mfauna,key="Sample",value="Count",paste(1:100))

   plist[[n + 2]] <- ggplot(data=rects_df_long) +
                     geom_rect( aes(xmin=11700,xmax=15000,ymin=0,ymax=4),fill="lightgrey", alpha=0.25) +
                    geom_col(mapping=aes(y=Count,x=Date),
                           position="identity",
                           alpha=0.5,
                           colour=NA) +
                    #scale_x_reverse() +
                    xlim(c(start,end)) +
                    labs(y=mfauna_sp[n]) +
                    theme_minimal() +
                    theme(text = element_text(family="Times", size=12),
                        plot.margin=unit(c(0,1,0,1),"cm"),
                        axis.title.x=element_blank(),
                        axis.text.x=element_blank(),
                        axis.ticks.x=element_blank())
}

plist[[length(plist)+1]] <- ggplot(data=calcurve) +
          #scale_x_reverse() +
          geom_rect( aes(xmin=11700,xmax=15000,ymin=0,ymax=4),fill="lightgrey") +
          xlim(c(start,end)) +
          theme_minimal() +
          labs(x="Year BP") +
          theme(text = element_text(family="Times", size=12),
                  plot.margin=unit(c(0,1,0,1),"cm"))


###
fig <- ggarrange(plotlist=plist,
         ncol=1,
         nrow=length(plist),
         align="v")
fig

dev.off()

#ggsave(filename="../Images/mfauna_trends.png",
#      device = "png",
#      height = 10,
#      width = 20,
#      units = "cm",
#      scale = 1.5,
#      dpi = 2000)
