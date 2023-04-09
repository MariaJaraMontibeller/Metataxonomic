library(rstatix)
library(dplyr)
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

#library(SparkR)

############## ADD FILES #############
# choose the directory 

setwd("~/R/phylogenia_comparada/TABLE")

## add file environment > import dataset> from text (base)

# create data frame
df <- read.csv("~/R/phylogenia_comparada/TABLE/genus-mix-relative-abundace.csv", header=TRUE)


################################ CHOSSE GROUPS #########################################

Data1 <- df%>%
  filter(mix =="C-T1") 

Data2 <- df%>%
  filter(mix =="C-T2")

Data3 <- df%>%
  filter(mix =="C-T3")

Data4 <- df%>%
  filter(mix =="C-T4")

df2 <- bind_rows(Data1, Data2, Data3, Data4)

##########################  STATISCTIC #########################

a <- colnames(df)
a
capture.output(a, file = "a.txt")
# open with csv , use  opcoes separaces - outros - "
# seee a final file

stat.test1	<-  df2 %>% pairwise_wilcox_test(	Anaerotruncus	~mix,p.adj = "none")
stat.test2	<-  df2 %>% pairwise_wilcox_test(	Butyricicoccus	~mix,p.adj = "none")
stat.test3	<-  df2 %>% pairwise_wilcox_test(	Colidextribacter	~mix,p.adj = "none")
stat.test4	<-  df2 %>% pairwise_wilcox_test(	Enterorhabdus	~mix,p.adj = "none")
stat.test5	<-  df2 %>% pairwise_wilcox_test(	Faecalibaculum	~mix,p.adj = "none")
stat.test6	<-  df2 %>% pairwise_wilcox_test(	Lachnospiraceae	~mix,p.adj = "none")
stat.test7	<-  df2 %>% pairwise_wilcox_test(	Oscillibacter	~mix,p.adj = "none")
stat.test8	<-  df2 %>% pairwise_wilcox_test(	Parasutterella	~mix,p.adj = "none")
stat.test9	<-  df2 %>% pairwise_wilcox_test(	R.Eubacterium	~mix,p.adj = "none")
stat.test10	<-  df2 %>% pairwise_wilcox_test(	Roseburia	~mix,p.adj = "none")
stat.test11	<-  df2 %>% pairwise_wilcox_test(	A.Eubacterium          	~mix,p.adj = "none")
stat.test12	<-  df2 %>% pairwise_wilcox_test(	Bacteroides            	~mix,p.adj = "none")
stat.test13	<-  df2 %>% pairwise_wilcox_test(	Candidatus             	~mix,p.adj = "none")
stat.test14	<-  df2 %>% pairwise_wilcox_test(	Coriobacteriaceae      	~mix,p.adj = "none")
stat.test15	<-  df2 %>% pairwise_wilcox_test(	Erysipelatoclostridium 	~mix,p.adj = "none")
stat.test16	<-  df2 %>% pairwise_wilcox_test(	Gastranaerophilales    	~mix,p.adj = "none")
stat.test17	<-  df2 %>% pairwise_wilcox_test(	Lactobacillus          	~mix,p.adj = "none")
stat.test18	<-  df2 %>% pairwise_wilcox_test(	Oscillospiraceae       	~mix,p.adj = "none")
stat.test19	<-  df2 %>% pairwise_wilcox_test(	Peptococcaceae         	~mix,p.adj = "none")
stat.test20	<-  df2 %>% pairwise_wilcox_test(	R.Ruminococcus         	~mix,p.adj = "none")
stat.test21	<-  df2 %>% pairwise_wilcox_test(	Ruminococcaceae        	~mix,p.adj = "none")
stat.test22	<-  df2 %>% pairwise_wilcox_test(	Alistipes	~mix,p.adj = "none")
stat.test23	<-  df2 %>% pairwise_wilcox_test(	Blautia	~mix,p.adj = "none")
stat.test24	<-  df2 %>% pairwise_wilcox_test(	Christensenellaceae	~mix,p.adj = "none")
stat.test25	<-  df2 %>% pairwise_wilcox_test(	Desulfovibrio	~mix,p.adj = "none")
stat.test26	<-  df2 %>% pairwise_wilcox_test(	Erysipelotrichaceae	~mix,p.adj = "none")
stat.test27	<-  df2 %>% pairwise_wilcox_test(	Helicobacter	~mix,p.adj = "none")
stat.test28	<-  df2 %>% pairwise_wilcox_test(	Muribaculaceae	~mix,p.adj = "none")
stat.test29	<-  df2 %>% pairwise_wilcox_test(	Oscillospirales	~mix,p.adj = "none")
stat.test30	<-  df2 %>% pairwise_wilcox_test(	Prevotellaceae	~mix,p.adj = "none")
stat.test31	<-  df2 %>% pairwise_wilcox_test(	RF39	~mix,p.adj = "none")
stat.test32	<-  df2 %>% pairwise_wilcox_test(	Turicibacter	~mix,p.adj = "none")
stat.test33	<-  df2 %>% pairwise_wilcox_test(	Anaeroplasma          	~mix,p.adj = "none")
stat.test34	<-  df2 %>% pairwise_wilcox_test(	Butyricicoccaceae     	~mix,p.adj = "none")
stat.test35	<-  df2 %>% pairwise_wilcox_test(	Clostridia            	~mix,p.adj = "none")
stat.test36	<-  df2 %>% pairwise_wilcox_test(	E.Eubacterium         	~mix,p.adj = "none")
stat.test37	<-  df2 %>% pairwise_wilcox_test(	Escherichia           	~mix,p.adj = "none")
stat.test38	<-  df2 %>% pairwise_wilcox_test(	Lachnoclostridium     	~mix,p.adj = "none")
stat.test39	<-  df2 %>% pairwise_wilcox_test(	Muribaculum           	~mix,p.adj = "none")
stat.test40	<-  df2 %>% pairwise_wilcox_test(	Parabacteroides       	~mix,p.adj = "none")
stat.test41	<-  df2 %>% pairwise_wilcox_test(	Proteus               	~mix,p.adj = "none")
stat.test42	<- df2 %>% pairwise_wilcox_test(	Rhodospirillales      	~mix,p.adj = "none")

#### graphifc ###

# crate y position for graphic 
stat.test1	<- 	stat.test1	%>% add_y_position()
stat.test2	<- 	stat.test2	%>% add_y_position()
stat.test3	<- 	stat.test3	%>% add_y_position()
stat.test4	<- 	stat.test4	%>% add_y_position()
stat.test5	<- 	stat.test5	%>% add_y_position()
stat.test6	<- 	stat.test6	%>% add_y_position()
stat.test7	<- 	stat.test7	%>% add_y_position()
stat.test8	<- 	stat.test8	%>% add_y_position()
stat.test9	<- 	stat.test9	%>% add_y_position()
stat.test10	<- 	stat.test10	%>% add_y_position()
stat.test11	<- 	stat.test11	%>% add_y_position()
stat.test12	<- 	stat.test12	%>% add_y_position()
stat.test13	<- 	stat.test13	%>% add_y_position()
stat.test14	<- 	stat.test14	%>% add_y_position()
stat.test15	<- 	stat.test15	%>% add_y_position()
stat.test16	<- 	stat.test16	%>% add_y_position()
stat.test17	<- 	stat.test17	%>% add_y_position()
stat.test18	<- 	stat.test18	%>% add_y_position()
stat.test19	<- 	stat.test19	%>% add_y_position()
stat.test20	<- 	stat.test20	%>% add_y_position()
stat.test21	<- 	stat.test21	%>% add_y_position()
stat.test22	<- 	stat.test22	%>% add_y_position()
stat.test23	<- 	stat.test23	%>% add_y_position()
stat.test24	<- 	stat.test24	%>% add_y_position()
stat.test25	<- 	stat.test25	%>% add_y_position()
stat.test26	<- 	stat.test26	%>% add_y_position()
stat.test27	<- 	stat.test27	%>% add_y_position()
stat.test28	<- 	stat.test28	%>% add_y_position()
stat.test29	<- 	stat.test29	%>% add_y_position()
stat.test30	<- 	stat.test30	%>% add_y_position()
stat.test31	<- 	stat.test31	%>% add_y_position()
stat.test32	<- 	stat.test32	%>% add_y_position()
stat.test33	<- 	stat.test33	%>% add_y_position()
stat.test34	<- 	stat.test34	%>% add_y_position()
stat.test35	<- 	stat.test35	%>% add_y_position()
stat.test36	<- 	stat.test36	%>% add_y_position()
stat.test37	<- 	stat.test37	%>% add_y_position()
stat.test38	<- 	stat.test38	%>% add_y_position()
stat.test39	<- 	stat.test39	%>% add_y_position()
stat.test40	<- 	stat.test40	%>% add_y_position()
stat.test41	<- 	stat.test41	%>% add_y_position()
stat.test42	<- 	stat.test42	%>% add_y_position()


capture.output(	stat.test1	, file = "	stat.test1	.txt")
capture.output(	stat.test2	, file = "	stat.test2	.txt")
capture.output(	stat.test3	, file = "	stat.test3	.txt")
capture.output(	stat.test4	, file = "	stat.test4	.txt")
capture.output(	stat.test5	, file = "	stat.test5	.txt")
capture.output(	stat.test6	, file = "	stat.test6	.txt")
capture.output(	stat.test7	, file = "	stat.test7	.txt")
capture.output(	stat.test8	, file = "	stat.test8	.txt")
capture.output(	stat.test9	, file = "	stat.test9	.txt")
capture.output(	stat.test10	, file = "	stat.test10	.txt")
capture.output(	stat.test11	, file = "	stat.test11	.txt")
capture.output(	stat.test12	, file = "	stat.test12	.txt")
capture.output(	stat.test13	, file = "	stat.test13	.txt")
capture.output(	stat.test14	, file = "	stat.test14	.txt")
capture.output(	stat.test15	, file = "	stat.test15	.txt")
capture.output(	stat.test16	, file = "	stat.test16	.txt")
capture.output(	stat.test17	, file = "	stat.test17	.txt")
capture.output(	stat.test18	, file = "	stat.test18	.txt")
capture.output(	stat.test19	, file = "	stat.test19	.txt")
capture.output(	stat.test20	, file = "	stat.test20	.txt")
capture.output(	stat.test21	, file = "	stat.test21	.txt")
capture.output(	stat.test22	, file = "	stat.test22	.txt")
capture.output(	stat.test23	, file = "	stat.test23	.txt")
capture.output(	stat.test24	, file = "	stat.test24	.txt")
capture.output(	stat.test25	, file = "	stat.test25	.txt")
capture.output(	stat.test26	, file = "	stat.test26	.txt")
capture.output(	stat.test27	, file = "	stat.test27	.txt")
capture.output(	stat.test28	, file = "	stat.test28	.txt")
capture.output(	stat.test29	, file = "	stat.test29	.txt")
capture.output(	stat.test30	, file = "	stat.test30	.txt")
capture.output(	stat.test31	, file = "	stat.test31	.txt")
capture.output(	stat.test32	, file = "	stat.test32	.txt")
capture.output(	stat.test33	, file = "	stat.test33	.txt")
capture.output(	stat.test34	, file = "	stat.test34	.txt")
capture.output(	stat.test35	, file = "	stat.test35	.txt")
capture.output(	stat.test36	, file = "	stat.test36	.txt")
capture.output(	stat.test37	, file = "	stat.test37	.txt")
capture.output(	stat.test38	, file = "	stat.test38	.txt")
capture.output(	stat.test39	, file = "	stat.test39	.txt")
capture.output(	stat.test40	, file = "	stat.test40	.txt")
capture.output(	stat.test41	, file = "	stat.test41	.txt")
capture.output(	stat.test42	, file = "	stat.test42	.txt")


stat.test1
stat.test2
stat.test3
stat.test4
stat.test5
stat.test6
stat.test7
stat.test8
stat.test9
stat.test10
stat.test11
stat.test12
stat.test13
stat.test14
stat.test15
stat.test16
stat.test17
stat.test18
stat.test19
stat.test20
stat.test21
stat.test22
stat.test23
stat.test24
stat.test25
stat.test26
stat.test27
stat.test28
stat.test29
stat.test30
stat.test31
stat.test32
stat.test33
stat.test34
stat.test35
stat.test36
stat.test37
stat.test38
stat.test39
stat.test40
stat.test41
stat.test42

########## make STAT the graphic1 ################### 

colnames(df2)

#### graph1 statistic  ###

stat.test16	<- 	stat.test16	%>% add_y_position("median_iqr")
#arg' deve ser um dentre “full”, “common”, “robust”, “five_number”, “mean_sd”, “mean_se”, “mean_ci”, “median_iqr”, “median_mad”, “quantile”, “mean”, “median”, “min”, “max”
stat.test16	


df3 <- df2%>%
  select(mix, Gastranaerophilales)

df3
reviewstatdata <- head(df3 %>% group_by(mix) %>% get_summary_stats(type = "median_iqr"))  #mean_sd

capture.output(	reviewstatdata, file = "reviewstatdata.txt")

stat.test16

############# graph1 #################

colnames(df2)

ggboxplot(df2, x = "mix", y = "Gastranaerophilales", fill = "mix", palette = "Reds") +
  stat_pvalue_manual(stat.test16, label = "p.adj.signif", tip.length = 0.05,ref.group = ".all.", hide.ns = TRUE) +
  theme(legend.position="none")

# create a p lists for joined different results 
# a lot of different design option 

p1 <- ggboxplot(df2, x = "mix", y = "Gastranaerophilales", fill = "mix", palette = "Reds") +
  stat_pvalue_manual(stat.test16, label = "p.adj.signif", tip.length = 0.05,ref.group = ".all.", hide.ns = TRUE) +
  theme(legend.position="none")
p1

## p <- p + scale_fill_manual(values = c("blue", "black", "yellow", "red", "green")) # if you want choose a specific colors 

p1 <- p1 + xlab("Gastranaerophilales") + ylab("Relative Abundance (%)")
p1
p1 <- p1 + theme(axis.text = element_text(size = 13, family = "Times", face = "italic",colour="black"))
p1
p1 <- p1 + theme(panel.background = element_rect(fill = "white", colour = "black"))
p1
#p3 <- p3 + guides(fill=guide_legend(title="Groups"))




## make stat graphic 2###################

colnames(df2)

#### graph1 statistic  ###

stat.test22	<- 	stat.test22	%>% add_y_position()
#arg' deve ser um dentre “full”, “common”, “robust”, “five_number”, “mean_sd”, “mean_se”, “mean_ci”, “median_iqr”, “median_mad”, “quantile”, “mean”, “median”, “min”, “max”
stat.test22


df3 <- df2%>%
  select(mix, Gastranaerophilales)

df3
reviewstatdata <- head(df3 %>% group_by(mix) %>% get_summary_stats(type = "median_iqr"))  #mean_sd

capture.output(	reviewstatdata, file = "reviewstatdata.txt")

stat.test16

################ graph 2 ################

ggboxplot(df2, x = "mix", y = "Alistipes", fill = "mix", palette = "Reds") +
  stat_pvalue_manual(stat.test22, label = "p.adj.signif", tip.length = 0.05,ref.group = ".all.", hide.ns = TRUE) +
  theme(legend.position="none")

# create a p lists for joined different results 
# a lot of different design option 

p2 <- ggboxplot(df2, x = "mix", y = "Alistipes", fill = "mix", palette = "Reds") +
  stat_pvalue_manual(stat.test22, label = "p.adj.signif", tip.length = 0.05,ref.group = ".all.", hide.ns = TRUE) +
  theme(legend.position="none")
p2

## p <- p + scale_fill_manual(values = c("blue", "black", "yellow", "red", "green")) # if you want choose a specific colors 

p2 <- p2 + xlab("Alistipes") + ylab("Relative Abundance (%)")
p2
p2 <- p2 + theme(axis.text = element_text(size = 13, family = "Times", face = "italic",colour="black"))
p2
p2 <- p2 + theme(panel.background = element_rect(fill = "white", colour = "black"))
p2
#p3 <- p3 + guides(fill=guide_legend(title="Groups"))



########## make stat the graphic3 ################### 

colnames(df2)

stat.test28	<- 	stat.test28	%>% add_y_position() # ("median_iqr")
#arg' deve ser um dentre “full”, “common”, “robust”, “five_number”, “mean_sd”, “mean_se”, “mean_ci”, “median_iqr”, “median_mad”, “quantile”, “mean”, “median”, “min”, “max”
stat.test28


# df3 <- df2%>%
  select(mix, Gastranaerophilales)

#df3
#reviewstatdata <- head(df3 %>% group_by(mix) %>% get_summary_stats(type = "median_iqr"))  #mean_sd

#capture.output(	reviewstatdata, file = "reviewstatdata.txt")

#stat.test16

############# graph3 #################

colnames(df2)

ggboxplot(df2, x = "mix", y = "Muribaculaceae", fill = "mix", palette = "Reds") +
  stat_pvalue_manual(stat.test28, label = "p.adj.signif", tip.length = 0.05,ref.group = ".all.", hide.ns = TRUE) +
  theme(legend.position="none")

# create a p lists for joined different results 
# a lot of different design option 

p3 <- ggboxplot(df2, x = "mix", y = "Muribaculaceae", fill = "mix", palette = "Reds") +
  stat_pvalue_manual(stat.test28, label = "p.adj.signif", tip.length = 0.05,ref.group = ".all.", hide.ns = TRUE) +
  theme(legend.position="none")
p3

## p <- p + scale_fill_manual(values = c("blue", "black", "yellow", "red", "green")) # if you want choose a specific colors 

p3 <- p3 + xlab("Muribaculaceae") + ylab("Relative Abundance (%)")
p3
p3 <- p3 + theme(axis.text = element_text(size = 13, family = "Times", face = "italic",colour="black"))
p3
p3 <- p3 + theme(panel.background = element_rect(fill = "white", colour = "black"))
p3
#p3 <- p3 + guides(fill=guide_legend(title="Groups"))


########## make stat the graphic 4 ################### 

colnames(df2)

stat.test30	<- 	stat.test30	%>% add_y_position() # ("median_iqr")
#arg' deve ser um dentre “full”, “common”, “robust”, “five_number”, “mean_sd”, “mean_se”, “mean_ci”, “median_iqr”, “median_mad”, “quantile”, “mean”, “median”, “min”, “max”
stat.test30


# df3 <- df2%>%
#select(mix, Gastranaerophilales)

#df3
#reviewstatdata <- head(df3 %>% group_by(mix) %>% get_summary_stats(type = "median_iqr"))  #mean_sd

#capture.output(	reviewstatdata, file = "reviewstatdata.txt")

#stat.test16

############# graph4 #################

colnames(df2)

ggboxplot(df2, x = "mix", y = "Prevotellaceae", fill = "mix", palette = "Reds") +
  stat_pvalue_manual(stat.test30, label = "p.adj.signif", tip.length = 0.05,ref.group = ".all.", hide.ns = TRUE) +
  theme(legend.position="none")

# create a p lists for joined different results 
# a lot of different design option 

p4 <- ggboxplot(df2, x = "mix", y = "Prevotellaceae", fill = "mix", palette = "Reds") +
  stat_pvalue_manual(stat.test30, label = "p.adj.signif", tip.length = 0.05,ref.group = ".all.", hide.ns = TRUE) +
  theme(legend.position="none")
p4

## p <- p + scale_fill_manual(values = c("blue", "black", "yellow", "red", "green")) # if you want choose a specific colors 

p4 <- p4 + xlab("Prevotellaceae") + ylab("Relative Abundance (%)")
p4
p4 <- p4 + theme(axis.text = element_text(size = 13, family = "Times", face = "italic",colour="black"))
p4
p4 <- p4 + theme(panel.background = element_rect(fill = "white", colour = "black"))
p4
#p3 <- p3 + guides(fill=guide_legend(title="Groups"))


########## make stat the graphic 5 ################### 

colnames(df2)

stat.test39	<- 	stat.test39	%>% add_y_position() # ("median_iqr")
#arg' deve ser um dentre “full”, “common”, “robust”, “five_number”, “mean_sd”, “mean_se”, “mean_ci”, “median_iqr”, “median_mad”, “quantile”, “mean”, “median”, “min”, “max”
stat.test39


df5 <- df2%>%
select(mix, Muribaculum)

df5
reviewstatdataMuribaculum <- head(df5 %>% group_by(mix) %>% get_summary_stats(type = "median_iqr"))  #mean_sd

capture.output(	reviewstatdataMuribaculum, file = "reviewstatdataMuribaculum.txt")

#stat.test16

############# graph5 #################

colnames(df2)

ggboxplot(df2, x = "mix", y = "Muribaculum", fill = "mix", palette = "Reds") +
  stat_pvalue_manual(stat.test39, label = "p.adj.signif", tip.length = 0.05,ref.group = ".all.", hide.ns = TRUE) +
  theme(legend.position="none")

# create a p lists for joined different results 
# a lot of different design option 

p5 <- ggboxplot(df2, x = "mix", y = "Muribaculum", fill = "mix", palette = "Reds") +
  stat_pvalue_manual(stat.test39, label = "p.adj.signif", tip.length = 0.05,ref.group = ".all.", hide.ns = TRUE) +
  theme(legend.position="none")
p5

## p <- p + scale_fill_manual(values = c("blue", "black", "yellow", "red", "green")) # if you want choose a specific colors 

p5 <- p5 + xlab("Muribaculum") + ylab("Relative Abundance (%)")
p5
p5 <- p5 + theme(axis.text = element_text(size = 13, family = "Times", face = "italic",colour="black"))
p5
p5 <- p5 + theme(panel.background = element_rect(fill = "white", colour = "black"))
p5
#p3 <- p3 + guides(fill=guide_legend(title="Groups"))


########## make stat the graphic6 ################### 

colnames(df2)

stat.test40	<- 	stat.test40	%>% add_y_position() # ("median_iqr")
#arg' deve ser um dentre “full”, “common”, “robust”, “five_number”, “mean_sd”, “mean_se”, “mean_ci”, “median_iqr”, “median_mad”, “quantile”, “mean”, “median”, “min”, “max”
stat.test40


# df3 <- df2%>%
#select(mix, Gastranaerophilales)

#df3
#reviewstatdata <- head(df3 %>% group_by(mix) %>% get_summary_stats(type = "median_iqr"))  #mean_sd

#capture.output(	reviewstatdata, file = "reviewstatdata.txt")

#stat.test16

############# graph3 #################

#colnames(df2)

ggboxplot(df2, x = "mix", y = "Parabacteroides", fill = "mix", palette = "Reds") +
  stat_pvalue_manual(stat.test40, label = "p.adj.signif", tip.length = 0.05,ref.group = ".all.", hide.ns = TRUE) +
  theme(legend.position="none")

# create a p lists for joined different results 
# a lot of different design option 

p6 <- ggboxplot(df2, x = "mix", y = "Parabacteroides", fill = "mix", palette = "Reds") +
  stat_pvalue_manual(stat.test40, label = "p.adj.signif", tip.length = 0.05,ref.group = ".all.", hide.ns = TRUE) +
  theme(legend.position="none")
p6

## p <- p + scale_fill_manual(values = c("blue", "black", "yellow", "red", "green")) # if you want choose a specific colors 

p6 <- p6 + xlab("Parabacteroides") + ylab("Relative Abundance (%)")
p6
p6 <- p6 + theme(axis.text = element_text(size = 13, family = "Times", face = "italic",colour="black"))
p6
p6 <- p6 + theme(panel.background = element_rect(fill = "white", colour = "black"))
p6
#p3 <- p3 + guides(fill=guide_legend(title="Groups"))


####### GRAPHS TOGHETER ###########

# https://anotherecoblog.wordpress.com/2018/12/13/juntando-graficos-em-r/
# http://2engenheiros.com/2017/12/05/criar-combinar-graficos-r/
# https://br.pinterest.com/pin/801148221207782036/

## remotes::install_github("wilkelab/cowplot")

library(cowplot)

# plot_grid(p1, p2, p3 labels = c('A', 'B'), label_size = 12)

colit_genus <-  plot_grid(p1, p2, p3,p4, p5, p6,  nrow=3, ncol=2,label_size = 12)
colit_genus
# if : Warning message: In drawGTree(x) : tempo limite atingido use: 
#dev.off()




