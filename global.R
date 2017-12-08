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

yve <- read.csv("melted_tomato.csv")
yve <- yve[,c("Trait", "AccessionName", "Condition", "variable", "value")]

# yve <- na.omit(yve)

yveNOVA <- read.csv("Pailles et al Tomato seedlings screen.csv")

YveSum <- summaryBy(value ~ AccessionName + Trait + Condition, data = yve, FUN = function(x) { c(m = mean(x), se = std.error(x)) })
head(YveSum)

YveAVG <- YveSum[,c(1:4)]
Yve_Cast <- cast(YveAVG, AccessionName ~ Condition + Trait, fill=NA)

head(Yve_Cast)
