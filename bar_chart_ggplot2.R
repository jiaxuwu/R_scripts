# bar chart script

library(ggplot2)
library(dplyr)
library(tidygraph)

data <- read.csv(file.choose())

data2 = data.frame(data$Protein.Family, data$Count)

data_r = data2 %>%
  mutate (Protein.Family1 = fct_reorder (data2$data.Protein.Family, data2$data.Count))

  ggplot(data_r) +
  guides(fill=FALSE) +
  coord_flip() +
  aes(x = data_r$Protein.Family1, y = data_r$data.Count, fill = data_r$Protein.Family1) +
  scale_fill_hue(direction = 1) +
  labs(x = "Protein Family", fill = "Protein Family", y = "Count") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 14L, 
                                    face = "bold"), axis.title.x = element_text(size = 14L, face = "bold")) +
    geom_col()

