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

head(yve)
unique(yve$Condition)


# yve <- na.omit(yve)


YveSum <- summaryBy(value ~ AccessionName + Trait + Condition + Species, data = yve, FUN = function(x) { c(m = mean(x), se = std.error(x)) })
head(YveSum)

YveAVG <- YveSum[,c(1:5)]
head(YveAVG)
possible <- c("control", "salt", "T0")

YveAVG <- subset(YveAVG, YveAVG$Condition %in% possible)

Yve_Cast <- cast(YveAVG, AccessionName + Species ~ Condition + Trait, fill=NA)

head(Yve_Cast)

colnames(Yve_Cast)
relative <- read.csv("RelativePhenotypesByAverage.csv")
colnames(relative)
colnames(relative)[1] <- "Species"
colnames(relative)[2] <- "AccessionName"
head(relative)
dim(relative)
relative2 <- relative[,c(2,1,4:32)]
head(relative2)
relative2$id <- paste(relative2$Species, relative2$AccessionName, sep="_")

Yve_Cast$id <- paste(Yve_Cast$Species, Yve_Cast$AccessionName, sep="_")

unique(relative2$Species)

relative2 <- subset(relative2, relative2$AccessionName != "VF-36")
unique(relative2$AccessionName)
unique(Yve_Cast$AccessionName)

head(Yve_Cast)

Yve_Cast <- merge(Yve_Cast, relative2, id = "id")
colnames(Yve_Cast)
Yve_Cast <- Yve_Cast[,c(1,2,4:96)]

list_casted <- colnames(Yve_Cast)
list_casted <- setdiff(list_casted, c("AccessionName", "Species"))
list_casted

species <- unique(Yve_Cast$Species)
species

