function(input,output){
	
output$dataAll <- downloadHandler(
filename = "Pailles et al Tomato seedlings screen.csv",
content <- function(file){
file.copy("Pailles et al Tomato seedlings screen.csv", file)
	},
contentType = "txt/csv")	


output$plot2a <- renderPlot({
  to_test <- yveNOVA[,c("AccessionName",input$tr_compare)]
  to_test <- subset(to_test, to_test$AccessionName == input$gen_compare)
  names(to_test)[2] <- "phenotype"
  to_test[,1] <- as.factor(to_test[,1])
  # then paste here ANOVA script with cluster as testing variable
  amod <- aov(phenotype ~ AccessionName, data = to_test)
  tuk <- glht(amod, linfct = mcp(AccessionName = "Tukey"))
  tuk.cld <- cld(tuk)   
  old.par <- par( mai=c(1,1,1.25,1))
  plot(tuk.cld, las=1, col="olivedrab3", ylab=input$tr_compare)
})

output$plot3 <- renderPlotly({

pheno_cor <- Yve_Cast[,c("AccessionName",input$Trait_cor1, input$Trait_cor2)]
pheno_cor <- na.omit(pheno_cor)

p <- plot_ly(pheno_cor, x= ~pheno_cor[,2], y=~pheno_cor[,3], type = 'scatter', mode = 'markers', text = ~pheno_cor[,1]) %>% 
layout(xaxis = list(title = paste(input$Trait_cor1)), yaxis = list(title = paste(input$Trait_cor2)))
})

output$corr <- renderText({
pheno_cor <- Yve_Cast[,c("AccessionName",input$Trait_cor1, input$Trait_cor2)]
pheno_cor <- na.omit(pheno_cor)
correl <- cor(pheno_cor[,2], pheno_cor[,3])
correl
})

output$corpval <- renderText({
pheno_cor <- Yve_Cast[,c("AccessionName",input$Trait_cor1, input$Trait_cor2)]
pheno_cor <- na.omit(pheno_cor)
pval <- cor.test(pheno_cor[,2], pheno_cor[,3])$p.val
pval[1]
})

# Cluster tree of ALL accessions based on three selected traits
output$ClusterTree <- renderPlot({
	# make a temporary subset based on Clust1, Clust2 and Clust3 (les traits)
  clust_lista <-input$Clust_traits
  clust_temp <- Yve_Cast[,c("AccessionName", clust_lista)]
	clust_temp <- na.omit(clust_temp)
	YVE_matrix <- clust_temp[,2:ncol(clust_temp)]
	YVE_matrix = as.matrix(YVE_matrix)
	row.names(YVE_matrix) <- clust_temp$AccessionName
  colnames(YVE_matrix) <- colnames(clust_temp)[2:4]
	YVE_t_matrix = t(YVE_matrix)
	YVE_t_cor = cor(YVE_t_matrix,method=c("pearson"))
	YVE_t_dist = dist(YVE_t_cor)
	YVE_t_clust = hclust(YVE_t_dist, method="ward.D2")
	Clufa <- plot(as.dendrogram(YVE_t_clust), horiz=T)
	Clufa
})
# 

output$HotHeatMap <- renderPlot({
  clust_lista <-input$Clust_traits
  clust_temp <- Yve_Cast[,c("AccessionName", clust_lista)]
  clust_temp <- na.omit(clust_temp)
  YVE_matrix <- clust_temp[,2:ncol(clust_temp)]
  YVE_matrix = as.matrix(YVE_matrix)
  row.names(YVE_matrix) <- clust_temp$AccessionName
  colnames(YVE_matrix) <- colnames(clust_temp)[2:(length(clust_lista)+1)]
  YVE_t_matrix = t(YVE_matrix)
  YVE_t_cor = cor(YVE_t_matrix,method=c("pearson"))
  YVE_t_dist = dist(YVE_t_cor)
  YVE_t_clust = hclust(YVE_t_dist, method="ward.D2")  
  heatmap.2(YVE_t_matrix, Colv=as.dendrogram(YVE_t_clust), col=blue2red(100),scale=c("row"),density.info="none",trace="none", cexRow=0.7)
})

output$HotANOVA <- renderPlot({
  clust_lista <-input$Clust_traits
  clust_temp <- Yve_Cast[,c("AccessionName", clust_lista)]
  clust_temp <- na.omit(clust_temp)
  YVE_matrix <- clust_temp[,2:ncol(clust_temp)]
  YVE_matrix = as.matrix(YVE_matrix)
  row.names(YVE_matrix) <- clust_temp$AccessionName
  colnames(YVE_matrix) <- colnames(clust_temp)[2:(length(clust_lista)+1)]
  YVE_t_matrix = t(YVE_matrix)
  YVE_t_cor = cor(YVE_t_matrix,method=c("pearson"))
  YVE_t_dist = dist(YVE_t_cor)
  YVE_t_clust = hclust(YVE_t_dist, method="ward.D2")  
# cut_tree at $tree_cut value (but first make it numeric)
cluster <- as.data.frame(cutree(YVE_t_clust,h=as.numeric(input$tree_cut)))
names(cluster)[1] <- "cluster"
# To fuse the cluster with the initial data - since we have a chance of non-equal number of columns, we have to transform the entire data set into a matrix as well:
YVE_matrix_all <- Yve_Cast[,2:ncol(Yve_Cast)]
row.names(YVE_matrix_all) <- Yve_Cast[,"AccessionName"]
# and mering two matrixes by row.names - this is the only working solution I found
new <- merge(cluster, YVE_matrix_all, by="row.names")
names(new)[1] <- "AccessionName"

# then make subset of the file based on the trait chosen in here:
to_test <- new[,c("AccessionName","cluster",input$Clust_test)]
names(to_test)[3] <- "phenotype"
to_test[,2] <- as.factor(to_test[,2])
# then paste here ANOVA script with cluster as testing variable
amod <- aov(phenotype ~ cluster, data = to_test)
 tuk <- glht(amod, linfct = mcp(cluster = "Tukey"))
 tuk.cld <- cld(tuk)   
 old.par <- par( mai=c(1,1,1.25,1))
 plot(tuk.cld, las=1, col="plum3", ylab=input$Clust_test)
})

CLU1 <- function(){
  clust_lista <-input$Clust_traits
  clust_temp <- Yve_Cast[,c("AccessionName", clust_lista)]
  clust_temp <- na.omit(clust_temp)
  YVE_matrix <- clust_temp[,2:ncol(clust_temp)]
  YVE_matrix = as.matrix(YVE_matrix)
  row.names(YVE_matrix) <- clust_temp$AccessionName
  colnames(YVE_matrix) <- colnames(clust_temp)[2:(length(clust_lista)+1)]
  YVE_t_matrix = t(YVE_matrix)
  YVE_t_cor = cor(YVE_t_matrix,method=c("pearson"))
  YVE_t_dist = dist(YVE_t_cor)
  YVE_t_clust = hclust(YVE_t_dist, method="ward.D2")  
  # cut_tree at $tree_cut value (but first make it numeric)
  cluster <- as.data.frame(cutree(YVE_t_clust,h=as.numeric(input$tree_cut)))
  names(cluster)[1] <- "cluster"
  # To fuse the cluster with the initial data - since we have a chance of non-equal number of columns, we have to transform the entire data set into a matrix as well:
  YVE_matrix_all <- YVE_Cast[,2:ncol(Yve_Cast)]
  row.names(YVE_matrix_all) <- Yve_Cast[,"AccessionName"]
  # and mering two matrixes by row.names - this is the only working solution I found
  new <- merge(cluster, YVE_matrix_all, by="row.names")
  names(new)[1] <- "AccessionName"
new
}


output$dwnldClust <- downloadHandler(
filename=function(){
	paste("Accession clustering based on ", input$Clust_traits, ".csv", sep=" ")},
content <- function(file){
	write.csv(CLU1(), file)}
	)
# end of script here
}