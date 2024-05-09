library(readr)
library(readxl)
library(ggpubr)
library(extrafont)
library(showtext)
library(patchwork)

# Load Loe dataset "Experimental Gengivitis in man", JoP, 1965, doi.org/10.1902/jop.1965.36.3.177
Loe <- read.csv("~/Downloads/Loe 1965.csv", header = TRUE, sep=";")

# Define my theme
MyTheme <-   theme_bw(13)+
  theme(axis.text = element_text(colour = "black"),
        aspect.ratio = 1,
        panel.grid = element_blank(),
        legend.key = element_blank(),
        strip.text = element_text(size = 12),
        legend.background=element_blank(),
        strip.background = element_rect(fill = NA, colour = NA),
        text = element_text(family = "Avenir"))

# my comparisons
MyComparisons <- list(c("Final", "End of no-cleansing period"), c("Start", "End of no-cleansing period"), c("Start", "Final"))

# Plaque index barplot
p <- ggbarplot(Loe, x="Timing", y="Mean_plaque_index_table1", 
          fill = "#E7B800",
          add = c("mean_se", "jitter"), error.plot = "upper_errorbar")+
  labs(x="", y="Plaque index")+
  stat_compare_means(aes(group = Timing), comparisons = MyComparisons)+
  MyTheme

# Gingival index barplot
p1 <- ggbarplot(Loe, x="Timing", y="Mean_gingival_index_table4", 
          fill = "#FFC0CB",
          add = c("mean_se", "jitter"), error.plot = "upper_errorbar")+
  labs(x="", y="Gingival index")+
  stat_compare_means(aes(group = Timing), comparisons = MyComparisons)+
  MyTheme


# Correlation Plot
p2 <- ggscatter(Loe, x = "Mean_plaque_index_table1", y = "Mean_gingival_index_table4",
          add = "reg.line",                                 # Add regression line
          conf.int = TRUE,                                  # Add confidence interval
          add.params = list(color = "blue", fill = "lightgray"))+
  stat_cor(method = "pearson", label.x = 0, label.y = 1.5) +
  labs(x="Plaque index", y="Gingiva index")+
  MyTheme

p+p1+p2+  plot_annotation(title = 'Löe et al. - "Experimental Gingivitis in Man", JoP 1965')
