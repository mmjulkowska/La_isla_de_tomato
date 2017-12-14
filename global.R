library(shiny)
library(shinythemes)
library(ggplot2)
library(car)
library(multcomp)
library(RColorBrewer)
library(doBy)
library(plotrix)
library(car)
library(reshape)
library(gplots)
library(ggrepel)
library(colorRamps)
library(plotly)
library(agricolae)
library(corrplot)
library(Hmisc)
library(made4)

yve <- read.csv("melted_tomato.csv")
head(yve)
yve <- yve[,c("Trait", "AccessionName", "Species","Condition", "variable", "value")]

# yve <- na.omit(yve)

yveNOVA <- read.csv("Pailles et al Tomato seedlings screen.csv")

YveSum <- summaryBy(value ~ AccessionName + Trait + Condition + Species, data = yve, FUN = function(x) { c(m = mean(x), se = std.error(x)) })
head(YveSum)

YveAVG <- YveSum[,c(1:5)]
Yve_Cast <- cast(YveAVG, AccessionName + Species ~ Condition + Trait, fill=NA)

head(Yve_Cast)
list_casted <- colnames(Yve_Cast)
list_casted <- setdiff(list_casted, c("AccessionName", "Species"))
list_casted

species <- unique(Yve_Cast$Species)
species

