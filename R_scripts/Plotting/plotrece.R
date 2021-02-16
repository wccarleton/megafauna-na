namen <- c("All Species",
           "Equus",
           "Mammoth",
           "Mastodon",
           "Sloth",
           "Sabertooth")

mfauna_sp <- "mfauna"

load(paste("../Data/rects_sample_",mfauna_sp,".RData",sep=""))
rece <- get(paste("rects_sample_",mfauna_sp,sep=""))
recelong <- gather(rece,
                    key="Sample",
                    value="Count",
                    paste(1:1000))

p <- ggplot(data=recelong) +
    geom_col(mapping=aes(y=Count,x=Dates),
                position="identity",
                alpha=0.25,
                colour=NA) +
    xlim(c(20000,10000)) +
    labs(y=namen[1]) +
    theme_minimal() +
    theme(text = element_text(family="Times", size=12),
        plot.margin=unit(c(0,1,0,1),"cm"),
        axis.title.y=element_text(family="Times", size=12),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p
