plist <- list()

mfauna_sp <- c("mfauna",
               "equus",
               "mammoth",
               "mastodon",
               #"sloth",
               "sabertooth")

for( n in 1:length(mfauna_sp)){
   load(paste("../Results/MCMC/NewGround/Climate/mcmc_samples_",mfauna_sp[n],"_ta_NGRIP_ensemble.RData",sep=""))
   niter <- dim(samples$samples)[1]
   mcmc_samps <- as.data.frame(samples$samples[20000:niter,])

   plist[[n]] <- ggplot(data=mcmc_samps,
                           mapping=aes(B1)) +
                    geom_density(fill="steelblue",alpha=0.75,colour=NA) +
                    labs(x=mfauna_sp[n]) +
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

ggsave(filename="../Images/c_enemsble_posteriors.png",
      device = "png",
      height = 10,
      width = 20,
      units = "cm",
      scale = 1.5,
      dpi = 2000)
