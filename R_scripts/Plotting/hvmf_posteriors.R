#png(filename="../Images/mfauna_hvmf_posteriors.png",
#      height = 10,
#      width = 20,
#      units = "cm",
#      res = 300)

plist <- list()

mfauna_sp <- c("mfauna",
               "equus",
               "mammoth",
               "mastodon",
               "sloth",
               "sabertooth")

mfauna_namen <- c("All",
                    "Equus",
                    "Mammoth",
                    "Mastodon",
                    "Sloth",
                    "Sabertooth")

for( n in 1:length(mfauna_sp)){
   load(paste("../Results/MCMC/FvH/mcmc_samples_",mfauna_sp[n],"_ta_sub.RData",sep=""))
   niter <- dim(samples$samples)[1]
   mcmc_samps <- as.data.frame(samples$samples[20000:niter,])

   plist[[n]] <- ggplot(data=mcmc_samps,
                           mapping=aes(B)) +
                    geom_density(fill="steelblue",alpha=0.75,colour=NA) +
                    labs(x=mfauna_namen[n]) +
                    theme_minimal() +
                    theme(text = element_text(family="Times", size=12),
                    axis.title.y=element_blank(),
                    axis.text.y=element_blank(),
                    axis.ticks.y=element_blank())
}

###
fig <- ggarrange(plotlist=plist,
         ncol=3,
         nrow=2,
         align="v")
fig

#dev.off()

#ggsave(filename="../Images/mfauna_trends.png",
#      device = "png",
#      height = 10,
#      width = 20,
#      units = "cm",
#      scale = 1.5,
#      dpi = 2000)
