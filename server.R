function(input,output){
	
output$dataAll <- downloadHandler(
filename = "Pailles et al Tomato seedlings screen.csv",
content <- function(file){
file.copy("Pailles et al Tomato seedlings screen.csv", file)
	},
contentType = "txt/csv")	


# - - - - - - - - >> INPUT ANOVA GRAPH << - - - - - - - - - - - - #

output$Select_trait_for_anova <- renderUI({
    traits_available <- unique(yve$Trait)
    tagList(
      selectizeInput("trait_compare", label=("Chose the phenotype"), choices= traits_available, multiple=F))    
  })

output$Select_condition_for_anova <- renderUI({
    traits_available <- subset(yve, yve$Trait == input$trait_compare)
    cond_available <- unique(traits_available$Condition)
    tagList(
      selectizeInput("cond_compare", label=("Chose the condition"), choices= cond_available, multiple=F))    
  })

output$Select_genotype_for_anova <- renderUI({
    list_of_acc <- unique(yve$AccessionName)
    
    tagList(
      selectizeInput("gen_compare", label=("Chose the genotypes to compare"), choices= list_of_acc, multiple= T,
                     selected = c( "LA0317", "LA0421", "LA0426")))    
  })

output$ANOVA_bar_error <- renderUI({
  if(input$ANOVA_graph_type == "Bar graph"){
    selectizeInput(
      inputId = "ANOVA_EB",
      label = "Error bars represent:",
      choices = c("Standard Error", "Standard Deviation"),
      multiple = F)
  }
  else{
    return()
  }
})

# - - - - - - - - - - - - >> ANOVA OUTPUT << - - - - - - - - - - - - - 

output$Table1 <- renderDataTable({
  traits_available <- subset(yve, yve$Trait == input$trait_compare)
  data_available <- subset(traits_available, traits_available$Condition == input$cond_compare)
  to_test <- subset(data_available, data_available$AccessionName %in% input$gen_compare)
  to_test <- na.omit(to_test)
  to_test$AccessionName <- as.factor(to_test$AccessionName)
  
  sum_test <- summaryBy(value ~ AccessionName + Trait + Condition, data = to_test, FUN = function(x) { c(m = mean(x), se = std.error(x), rep.no = length(x)) })
  colnames(sum_test)[4] <- "Mean"
  colnames(sum_test)[5] <- "StandardError"
  colnames(sum_test)[6] <- "NumberOfReplicates"
  sum_test
  })

output$ANOVA_message <- renderPrint({
  traits_available <- subset(yve, yve$Trait == input$trait_compare)
  data_available <- subset(traits_available, traits_available$Condition == input$cond_compare)
  to_test <- subset(data_available, data_available$AccessionName %in% input$gen_compare)
  to_test <- na.omit(to_test)
  to_test$AccessionName <- as.factor(to_test$AccessionName)
  to_test
  
  amod <- aov(value ~ AccessionName, data = to_test)
  cat("ANOVA report:")
  cat("\n")
  cat("\n")
  if(summary(amod)[[1]][[5]][1] < 0.05){
    cat("The effect of Genotype is SIGNIFICANT on ", input$Select_trait_for_anova, "with a p-value of ", summary(amod)[[1]][[5]][1], ".")
  }
  if(summary(amod)[[1]][[5]][1] > 0.05){
    cat("The effect of Genotype is NOT significant on ", input$Select_trait_for_anova, "with a p-value of ", summary(amod)[[1]][[5]][1], ".")
  }
})

output$Tukey_message <- renderPrint({
  traits_available <- subset(yve, yve$Trait == input$trait_compare)
  data_available <- subset(traits_available, traits_available$Condition == input$cond_compare)
  to_test <- subset(data_available, data_available$AccessionName %in% input$gen_compare)
  to_test <- na.omit(to_test)
  to_test$AccessionName <- as.factor(to_test$AccessionName)
  to_test
  
  fit_tukey <- aov(value ~ AccessionName, data = to_test)
  out <- HSD.test(fit_tukey, "AccessionName", group = T, alpha = 0.05)
  out_tukey<-as.data.frame(out$groups)
  out_tukey$AccessionName<-row.names(out_tukey)
  cat("Tukey test result with p-value threshold of 0.05:")
  cat("\n")
  cat("\n")
  print(as.data.frame(out_tukey), row.names=FALSE)
  
})


output$plot2a <- renderPlot({
  traits_available <- subset(yve, yve$Trait == input$trait_compare)
  data_available <- subset(traits_available, traits_available$Condition == input$cond_compare)
  to_test <- subset(data_available, data_available$AccessionName %in% input$gen_compare)
  to_test <- na.omit(to_test)
  to_test$AccessionName <- as.factor(to_test$AccessionName)
  
  if(input$ANOVA_graph_type == "Bar graph"){
    sum_test <- summaryBy(value ~ AccessionName + Trait + Condition, data = to_test, FUN = function(x) { c(m = mean(x), se = std.error(x), sd = sd(x)) })
    bam <- ggplot(data = sum_test, aes(x = AccessionName, y= value.m, fill = AccessionName))
    bam <- bam + geom_bar(stat = "identity", position=position_dodge(1))
    
    if(input$ANOVA_EB == "Standard Error"){
      bam <- bam + geom_errorbar(aes(ymin = value.m - value.se, ymax =value.m + value.se), position=position_dodge(1))
    }
    
    if(input$ANOVA_EB == "Standard Deviation"){
      bam <- bam + geom_errorbar(aes(ymin = value.m - value.sd, ymax =value.m + value.sd), position=position_dodge(1))
    }
  }
  
  if(input$ANOVA_graph_type == "Box plot"){
  bam <- ggplot(data = to_test, aes(x = AccessionName, y= value, fill = AccessionName))
  bam <- bam + geom_boxplot()}
  
  if(input$ANOVA_graph_type == "Scatter plot"){
    bam <- ggplot(data = to_test, aes(x = AccessionName, y= value, fill = AccessionName))
    bam <- bam + geom_point()}
  
  if(input$ANOVA_background == T){
    bam <- bam + theme_minimal()}
  
  if(input$ANOVA_grid == T){
    bam <- bam + theme(panel.grid.major = element_blank())}
  
  bam <- bam + ylab(input$trait_compare)
  bam <- bam + xlab("Accession")
  
  bam <- bam + guides(fill=guide_legend(title= "Accession"))
  
  bam <- bam + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  bam
})


# - - - - - - - - >> DOWNLOAD STUFF << - - - - - - - - - 

output$Download_ANOVA_table <- renderUI({
  downloadButton("Download_ANOVA_tab", label="Download the table")
})

output$Download_ANOVA_tab <- downloadHandler(
  filename = paste("TABLE Tomato Accessions ", input$trait_compare, "from Paillies et al.csv"),
  content <- function(file) {
    traits_available <- subset(yve, yve$Trait == input$trait_compare)
    data_available <- subset(traits_available, traits_available$Condition == input$cond_compare)
    to_test <- subset(data_available, data_available$AccessionName %in% input$gen_compare)
    to_test <- na.omit(to_test)
    to_test$AccessionName <- as.factor(to_test$AccessionName)
    
    sum_test <- summaryBy(value ~ AccessionName + Trait + Condition, data = to_test, FUN = function(x) { c(m = mean(x), se = std.error(x), rep.no = length(x)) })
    colnames(sum_test)[4] <- "Mean"
    colnames(sum_test)[5] <- "StandardError"
    colnames(sum_test)[6] <- "NumberOfReplicates"
    write.csv(sum_test, file)
})

CorrBiggie <- function(){
  pheno_cor <- Yve_Cast[,c("AccessionName",input$Trait_corr_graph_biggie)]
  pheno_nona <- na.omit(pheno_cor)
  pheno_ready <- pheno_nona[,2:ncol(pheno_nona)]
  pheno_ready_m <- as.matrix(pheno_ready)
  colnames(pheno_ready_m) <- colnames(pheno_ready)
  pheno_ready_m
  
  res <- rcorr(pheno_ready_m, type = input$corMethod)
  
  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  = (cormat)[ut],
      p = pmat[ut]
    )
  }
  
  result <- flattenCorrMatrix(res$r, res$P)
  result
}

output$BIG_cor_table <- renderDataTable({
  CorrBiggie()
  })

output$dwnldCorr <- downloadHandler(
  filename=function(){
    paste("Correlation table tomato Paillies et al.csv", sep=" ")},
  content <- function(file){
    write.csv(CorrBiggie(), file)}
)

# - - - - - - - - - >> BIG CORRELATION ANALYSIS << - - - - - - - - - -

output$BIG_correlation_graph <- renderPlot({
  pheno_cor <- Yve_Cast[,c("AccessionName",input$Trait_corr_graph_biggie)]
  pheno_nona <- na.omit(pheno_cor)
  pheno_ready <- pheno_nona[,2:ncol(pheno_nona)]
  pheno_ready_m <- as.matrix(pheno_ready)
  colnames(pheno_ready_m) <- colnames(pheno_ready)
  
  corrplot(
    cor(pheno_ready_m, method = "pearson"),
    method = input$corrplotMethod,
    type = input$corType,
    order = input$corOrder, 
    col = brewer.pal(n = 8, name = "PuOr"),
    tl.col = 'black', tl.cex=1, tl.offset = 3, tl.srt = 45
  )
})

# - - - - - - - - >> SCATTER PLOT << - - - - - - - - -

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

# - - - - - - - - >> CLUSTER ANALYSIS << - - - - - - - - -

# Cluster tree of ALL accessions based on three selected traits
output$ClusterTree <- renderPlot({
	# make a temporary subset based on Clust1, Clust2 and Clust3 (les traits)
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
	Clufa <- plot(as.dendrogram(YVE_t_clust), horiz=T)
	Clufa
})

output$Cluster_message <- renderPrint({
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
  
  cluster <- as.data.frame(cutree(YVE_t_clust,h=as.numeric(input$tree_cut)))
  names(cluster)[1] <- "cluster"
  clust_number <- length(unique(cluster$cluster))
  
  cat("Cutting the dengrodram at ", input$tree_cut, " will result in ", clust_number, " clusters.")
  cat("\n")
  cat("\n")
  cat("Please be aware that clustering your data into too many clusters might not be informative.")
  
})
# 

output$HotHeatMap <- renderPlot({
  clust_lista <-input$Clust_traits
  clust_temp <- Yve_Cast[,c("AccessionName", clust_lista)]
  clust_temp <- na.omit(clust_temp)
  YVE_matrix <- clust_temp[,2:ncol(clust_temp)]
  YVE_matrix <- scale(as.matrix(YVE_matrix))
  row.names(YVE_matrix) <- clust_temp$AccessionName
  colnames(YVE_matrix) <- colnames(clust_temp)[2:(length(clust_lista)+1)]
  #YVE_t_matrix = t(YVE_matrix)
  #YVE_t_cor = cor(YVE_t_matrix,method=c("pearson"))
  #YVE_t_dist = dist(YVE_t_cor)
  #YVE_t_clust = hclust(YVE_t_dist, method="ward.D2")  
  #heatmap.2(YVE_t_matrix, Colv=as.dendrogram(YVE_t_clust), col=blue2yellow(100),scale=c("row"),density.info="none",trace="none", cexRow=0.7)
  heatplot(YVE_matrix, scale = "none", dualScale = FALSE, margins=c(12,15))
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