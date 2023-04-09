library(ggsignif)
library(ggpubr)


getwd()

ov <- read.csv('WU_all_times.csv')

fdr_wt<- apply(ov[-1],2,function(x) pairwise.wilcox.test(x,ov$otu ,p.adj = "fdr"))

fdr_wt

# save results 
capture.output(fdr_wt, file = "fdr_wt.txt")


none_wt<- apply(ov[-1],2,function(x) pairwise.wilcox.test(x,ov$otu ,p.adj = "none"))
none_wt

# save results 
capture.output(none_wt, file = "none_wt.txt")


kw<- apply(ov[-1],2,function(x) kruskal.test(x,ov$otu))

kw

# save results 
capture.output(kw, file = "kw.txt")

