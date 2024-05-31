library(ggplot2)
library(dplyr)
library(see)
library(ggsignif)
df <- read.csv(file.choose())
df$parts <- as.factor(df$parts)
df$parts <- factor(df$parts, levels = c("Gall", "Development"))
df$lines <- factor(df$lines, levels = c("#9.4", "#15.3", "#16.3", "Col-0"))

df1 <- df %>% 
  na.omit() %>% 
  group_by(lines, parts) %>% 
  summarise(mean_value = mean(disease_index),
            sd_value = sd(disease_index),
            n = n(),  
            se_value = sd(disease_index) / sqrt(n()))

df1$parts <- factor(df1$parts, levels = c("Gall", "Development"))
df1$lines <- factor(df1$lines, levels = c("#9.4", "#15.3", "#16.3", "Col-0"))

df2 <- df1 %>% 
  group_by(lines) %>% 
  mutate(cumulative_mean = cumsum(mean_value))

df2$parts <- factor(df2$parts, levels = c("Development", "Gall"))
df2$lines <- factor(df2$lines, levels = c("#9.4", "#15.3", "#16.3", "Col-0"))

custom_colors <- c("Development" = "skyblue", "Gall" = "tomato")

p1 <- ggplot(data = df2, aes(x = lines, y = mean_value, fill = parts)) +
  geom_bar(position = "stack", stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = cumulative_mean - se_value,
                    ymax = cumulative_mean + se_value), width = 0.1) +  
  scale_y_continuous(expand = c(0, .5), limits = c(0, 7.5)) +
  scale_fill_manual(values = custom_colors) + 
  theme_bw() +theme(legend.title=element_blank())+theme(legend.position = c(.07, .94))+
  labs(x = "Lines", y = "Disease index")
p1
p2 <- p1+
  geom_signif(data=df2,
              aes(xmin=2, xmax=4, annotations="*", y_position=7.4),
              textsize = 5, vjust = -0.0000025, tip_length = c(0.4, 0.05),
              manual=TRUE)+
  geom_segment(aes(x = 1, y = 4.7, xend = 3, yend = 4.7))
p2
ggsave(p2,file="figure1.png",dpi = 600)
