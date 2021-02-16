start <- 20000#c(13150,20000)
end <- 13150#sample_date_range[1]#5000

plist <- list()

mfauna_sp <- c("mfauna",
               "equus",
               "mammoth",
               "mastodon",
               "sloth",
               "sabertooth")

calcurve <- copyCalibrationCurve()
names(calcurve) <- c("CalendarBP","C14","Uncertainty")
plist[[1]] <- ggplot(data=calcurve) +
    geom_path(mapping=aes(y=C14,x=CalendarBP)) +
    geom_ribbon(mapping=aes(x=CalendarBP,ymin=C14 - (2.96 * Uncertainty),ymax=C14 + (2.96 * Uncertainty)),fill="skyblue",alpha=0.75) +
    #coord_flip() +
    ylim(c(3500,5500)) +
    scale_x_reverse() +
    xlim(c(start,end)) +
    theme_minimal() +
    theme(text = element_text(family="Times", size=12),
            plot.margin=unit(c(0,1,0,1),"cm"),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())



rects_df_long <- gather(rece_mfauna,key="Sample",value="Count",paste(1:100))

p2 <- ggplot(data=rects_df_long) +
        geom_col(mapping=aes(y=Count,x=Dates),position="identity",alpha=0.05,colour=NA) +
        scale_x_reverse() +
        xlim(c(start,end)) +
        labs(x="Date (Calendar BP)",
            y="REC Ensemble Count") +
        theme_minimal() +
        theme(text = element_text(family="Times", size=12),
            plot.margin=unit(c(0,1,0,1),"cm"),)

###
fig <- ggarrange(p1,p2,
         ncol=1,
         nrow=2,
         align="v")
fig

#ggsave(filename="../Images/auspop_Denniston.png",
#      device = "png",
#      height = 10,
#      width = 20,
#      units = "cm",
#      scale = 1.5,
#      dpi = 2000)
