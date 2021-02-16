mfauna_sp <- c("mammoth",
               "mastodon")

plist <- list()
plist_position <- 1

for( n in 1:length(mfauna_sp)){
   load(paste("../Results/MCMC/Regional/mcmc_samples_",mfauna_sp[n],"_GL_ta_h_GL.RData",sep=""))
   niter <- dim(samples$samples)[1]
   mcmc_samps <- as.data.frame(samples$samples[20000:niter,])

   plist[[plist_position]] <- ggplot(data=mcmc_samps) +
                    geom_vline(xintercept=0,size=0.75,colour="darkgrey",alpha=0.75) +
                    geom_density(mapping=aes(x=B0,y=..scaled..),fill="steelblue",colour=NA,alpha=0.75) +
                    #labs(y=namen[n]) +
                    xlim(-0.4,0.4) +
                    theme_minimal() +
                    theme(text = element_text(family="Times", size=12),
                        axis.title.x=element_blank(),
                        axis.title.y=element_blank(),
                        axis.text.y=element_blank(),
                        axis.ticks.x=element_blank())
    #
    plist[[plist_position + 1]] <- ggplot(data=mcmc_samps) +
                    geom_vline(xintercept=0,size=0.75,colour="darkgrey",alpha=0.75) +
                     geom_density(mapping=aes(x=B,y=..scaled..),fill="steelblue",colour=NA,alpha=0.75) +
                     #labs(y=mfauna_sp[n]) +
                     xlim(-0.4,0.4) +
                     theme_minimal() +
                     theme(text = element_text(family="Times", size=12),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank(),
                         axis.text.y=element_blank(),
                         axis.ticks.x=element_blank())
    #
    plist[[plist_position + 2]] <- ggplot(data=mcmc_samps) +
                    geom_vline(xintercept=0,size=0.75,colour="darkgrey",alpha=0.75) +
                     geom_density(mapping=aes(x=B_ta,y=..scaled..),fill="steelblue",colour=NA,alpha=0.75) +
                     #labs(y=mfauna_sp[n]) +
                     xlim(-0.4,0.4) +
                     theme_minimal() +
                     theme(text = element_text(family="Times", size=12),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank(),
                         axis.text.y=element_blank(),
                         axis.ticks.x=element_blank())
    #
    plist_position <- plist_position + 3
}

mfauna_sp <- c("mammoth",
               "sloth")

for( n in 1:length(mfauna_sp)){
   load(paste("../Results/MCMC/Regional/mcmc_samples_",mfauna_sp[n],"_SW_ta_h_SW.RData",sep=""))
   niter <- dim(samples$samples)[1]
   mcmc_samps <- as.data.frame(samples$samples[20000:niter,])

   plist[[plist_position]] <- ggplot(data=mcmc_samps) +
                    geom_vline(xintercept=0,size=0.75,colour="darkgrey",alpha=0.75) +
                    geom_density(mapping=aes(x=B0,y=..scaled..),fill="steelblue",colour=NA,alpha=0.75) +
                    #labs(y=namen[n]) +
                    xlim(-0.4,0.4) +
                    theme_minimal() +
                    theme(text = element_text(family="Times", size=12),
                        axis.title.x=element_blank(),
                        axis.title.y=element_blank(),
                        axis.text.y=element_blank(),
                        axis.ticks.x=element_blank())
    #
    plist[[plist_position + 1]] <- ggplot(data=mcmc_samps) +
                    geom_vline(xintercept=0,size=0.75,colour="darkgrey",alpha=0.75) +
                     geom_density(mapping=aes(x=B,y=..scaled..),fill="steelblue",colour=NA,alpha=0.75) +
                     #labs(y=mfauna_sp[n]) +
                     xlim(-0.4,0.4) +
                     theme_minimal() +
                     theme(text = element_text(family="Times", size=12),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank(),
                         axis.text.y=element_blank(),
                         axis.ticks.x=element_blank())
    #
    plist[[plist_position + 2]] <- ggplot(data=mcmc_samps) +
                    geom_vline(xintercept=0,size=0.75,colour="darkgrey",alpha=0.75) +
                     geom_density(mapping=aes(x=B_ta,y=..scaled..),fill="steelblue",colour=NA,alpha=0.75) +
                     #labs(y=mfauna_sp[n]) +
                     xlim(-0.4,0.4) +
                     theme_minimal() +
                     theme(text = element_text(family="Times", size=12),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank(),
                         axis.text.y=element_blank(),
                         axis.ticks.x=element_blank())
    #
    plist_position <- plist_position + 3
}

###
fig <- ggarrange(plotlist=plist,
         ncol=3,
         nrow=4,
         align="v")

annotate_figure(fig,
                bottom=text_grob("Value",face="bold",family="Times"),
                left=text_grob("Density",face="bold",family="Times",rot=90),
                right=text_grob(" ",family="Times"),
                top=text_grob("Posterior Estimates (Humans only)",face="bold",family="Times"))

ggsave(filename="../Images/posteriors_Broughton_h_regional.png",
      device = "png",
      height = 20,
      width = 20,
      units = "cm",
      scale = 1,
      dpi = 600)
