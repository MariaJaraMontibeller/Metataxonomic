library(readr)
require(reshape2)
require(ggplot2)
library(ggprism)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)

# choose the directory 

setwd("~/R/Alpha_diversity_R/alpha_jara_results/alpha-boxplot-jara/t4/")

## add file environment > import dataset> from text (base)

# create data frame
df <- read.table("~/R/Alpha_diversity_R/alpha_jara_results/alpha-boxplot-jara/t4/t4.txt", header=TRUE)
#df <- read_csv("C_all_times-chao1.csv")

################################ GRAPHIC  1 #########################################
#choose statistic test
stat.test1 <- df %>%
  pairwise_wilcox_test(chao1 ~ otu,p.adj = "none") # or fdr 
stat.test1

# save statistic  results 
capture.output(stat.test1, file = "stat.test1.txt")

# crate y position for graphic 
stat.test1 <- stat.test1 %>%
  add_y_position()

stat.test1

# see the groups for create 
##cat(df$otu)

# choose the colour  for your graphic 
#display.brewer.all()


## make the graphic 

ggboxplot(df, x = "otu", y = "chao1", fill = "otu", palette = "Set3") +
  stat_pvalue_manual(stat.test1, label = "p.adj.signif", tip.length = 0.05, ref.group = ".all.",  hide.ns = TRUE) +
  theme(legend.position="none")

# create a p lists for joined different results 
# a lot of different design option 

p1 <- ggboxplot(df, x = "otu", y = "chao1", fill = "otu", palette = "Set3") +
  stat_pvalue_manual(stat.test1, label = "p.adj.signif", tip.length = 0.05, ref.group = ".all.",  hide.ns = TRUE)  +
  theme(legend.position="none")
p1
## p <- p + scale_fill_manual(values = c("blue", "black", "yellow", "red", "green")) # if you want choose a specific colours 
p1
p1 <- p1 + xlab("chao1") + ylab("Relative Measure")
p1
p1 <- p1 + theme(axis.text = element_text(size = 13, family = "Times", face = "italic",colour="black"))
p1
p1 <- p1 + theme(panel.background = element_rect(fill = "white", colour = "black"))

p1  <- plot_grid(p1, labels = c('D)'))

p1
################################ GRAPHIC  2 #########################################
#choose statistic test
stat.test2 <- df %>%
  pairwise_wilcox_test(observed_otus ~ otu,p.adj = "none") # or fdr 
stat.test2

# save statistic  results 
capture.output(stat.test2, file = "stat.test2.txt")

# crate y position for graphic 
stat.test2 <- stat.test2 %>%
  add_y_position()

stat.test2

# see the groups for create 
##cat(df$otu)

# choose the colour  for your graphic 
#display.brewer.all()

## df$otu <- factor(df$otu,levels=c("C-T1", "C-T2", "C-T3", "C-T4"))

## make the graphic 

ggboxplot(df, x = "otu", y = "observed_otus", fill = "otu", palette = "Set3") +
  stat_pvalue_manual(stat.test2, label = "p.adj.signif", tip.length = 0.05, ref.group = ".all.",  hide.ns = TRUE) +
  theme(legend.position="none")

# create a p lists for joined different results 
# a lot of different design option 

p2 <- ggboxplot(df, x = "otu", y = "observed_otus", fill = "otu", palette = "Set3") +
  stat_pvalue_manual(stat.test2, label = "p.adj.signif", tip.length = 0.05, ref.group = ".all.",  hide.ns = TRUE) +
  theme(legend.position="none")
p2
## p <- p + scale_fill_manual(values = c("blue", "black", "yellow", "red", "green")) # if you want choose a specific colors 
p2
p2 <- p2 + xlab("observed_otus") + ylab("")
p2
p2 <- p2 + theme(axis.text = element_text(size = 13, family = "Times", face = "italic",colour="black"))
p2
p2 <- p2 + theme(panel.background = element_rect(fill = "white", colour = "black"))
p2


################################ GRAPHIC  3 #########################################
#choose statistic test
stat.test3 <- df %>%
  pairwise_wilcox_test(shannon ~ otu,p.adj = "none") # or fdr 
stat.test3

# save statistic  results 
capture.output(stat.test3, file = "stat.test3.txt")

# crate y position for graphic 
stat.test3 <- stat.test3 %>%
  add_y_position()

stat.test3

# see the groups for create 
##cat(df$otu)

# choose the colour  for your graphic 
#display.brewer.all()


## make the graphic 

ggboxplot(df, x = "otu", y = "shannon", fill = "otu", palette = "Set3") +
  
  stat_pvalue_manual(stat.test3, label = "p.adj.signif", tip.length = 0.05, ref.group = ".all.",  hide.ns = TRUE) +
  theme(legend.position="none")

# create a p lists for joined different results 
# a lot of different design option 

p3 <- ggboxplot(df, x = "otu", y = "shannon", fill = "otu", palette = "Set3") +
  
  stat_pvalue_manual(stat.test3, label = "p.adj.signif", tip.length = 0.05, ref.group = ".all.",  hide.ns = TRUE) +
    theme(legend.position="none")

p3

## p <- p + scale_fill_manual(values = c("blue", "black", "yellow", "red", "green")) # if you want choose a specific colors 

p3 <- p3 + xlab("shannon") + ylab("")
p3
p3 <- p3 + theme(axis.text = element_text(size = 13, family = "Times", face = "italic",colour="black"))
p3
p3 <- p3 + theme(panel.background = element_rect(fill = "white", colour = "black"))
p3
#p3 <- p3 + guides(fill=guide_legend(title="Groups"))
p3
####### GRAPHS TOGHETER ###########

# https://anotherecoblog.wordpress.com/2018/12/13/juntando-graficos-em-r/
# http://2engenheiros.com/2017/12/05/criar-combinar-graficos-r/
# https://br.pinterest.com/pin/801148221207782036/

## remotes::install_github("wilkelab/cowplot")

library(cowplot)

# plot_grid(p1, p2, p3 labels = c('A', 'B'), label_size = 12)

a <-  plot_grid(p1, p2, p3, nrow=1, ncol=3)
a
# if : Warning message: In drawGTree(x) : tempo limite atingido use: 
#dev.off()



