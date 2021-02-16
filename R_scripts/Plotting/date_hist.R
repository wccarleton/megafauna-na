BW_CARD <- read.csv("../Data/BroughtonWeiztal2018/InputData/CARD_studyarea_cleaned.csv",as.is=T)
BW_CARD_sub <- subset(BW_CARD,Normalized.age >= 8872 & Normalized.age <= 16566)
CARD_site_sample_count <- as.data.frame(table(BW_CARD_sub$Site.identifier))

ggplot(data=CARD_site_sample_count) +
    geom_histogram(mapping=aes(Freq)) +
    labs(y="N. Sites", x="Radiocarbon Sample Count") +
    theme_minimal() +
    theme(text = element_text(family="Times", size=12))

ggsave(filename="../Images/CARD_radiocarbon_sample_sizes.png",
      device = "png",
      height = 10,
      width = 10,
      units = "cm",
      scale = 1,
      dpi = 150)
