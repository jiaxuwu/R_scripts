install.packages("gggenes")
library(gggenes)
library(ggplot2)
head(example_genes)
example_genes
domain <- read.csv(file.choose())

#alignment
dummies <- make_alignment_dummies(
  domain,
  aes(xmin = start, xmax = end, y = molecule, id = domain),
  on = "NB-ARC"
)

p <- ggplot(domain,
       # axis infromation
       aes(
         xmin = start,
         xmax = end,
         y = molecule,
         fill = domain,
         label = domain
       )) +
  # arrow
  geom_gene_arrow(arrowhead_height = unit(4, "mm"),
                  arrowhead_width = unit(2, "mm")) +
  # alignment
  geom_blank(data = dummies) +
  # gene label
  geom_gene_label() +
  # make them individually
  facet_wrap( ~ molecule, scales = "free", ncol = 1) +
  # color and theme
  scale_fill_brewer(palette = "Set3") +
  theme_genes()
p
# save the figure
ggsave("x.png", p , width = 5.2, height = 4.5, dpi = 600)


