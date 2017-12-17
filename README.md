# La_isla_de_tomato App user guide

### About the App
This App was designed to enhance interactive exploration of the data presented in the manuscript "Salinity responses in Galapagos tomatoes" by _Yveline Pailles, Mariam Awlia, Magdalena Julkowska, Luca Passone, Khadija Zemmouri, Sonia Negrao, Sandra M. Schmöckel, Mark Tester_

The manuscript is currently undergoing submission / revision, but this App is available to explore the data freely. The App can be accessed ![here](https://mmjulkowska.shinyapps.io/La_isla_de_tomato/) or run locally from your machine, by typing the following:
`library("shiny")`
`shiny::runGitHub("mmjulkowska/La_isla_de_tomato", "mmjulkowska")`

The App was coded with R / Shiny. The code is freely available at [github](https://github.com/mmjulkowska/La_isla_de_tomato) for use and reproduction or tweaking to your own results.

If you have any questions about the app - please dont hesitate to contact [me](https://github.com/mmjulkowska?tab=repositories)

## What can you do with this app?

### Compare the individual accessions and examine significant differences between them

The paper describes general changes in various phenotypes across a broad range of the tomatoes from Galapagos islands, but with this app, you can explore the phenotypes of individual lines and compare them for significant differences. 

Select the phenotype of interest, condition and as many genotypes of interest as you like in the side bar panel. In the main panel, you will see the graph representing the phenotypes of the chosen accessions. 

The grey box above the graph is a summary from the ANOVA test, and indicates whether the difference between the chosen accessions for the selected trait are significant. 

The grey box below the graph is representing pairwise Tukey test. The letters are representing significantly different groups as identified with Tukey tests with p-value threshold of 0.05. 

You can download the summary data table for the chosen accessions below the second grey box. 

![tab2](https://user-images.githubusercontent.com/14832460/34079981-9b25bd44-e348-11e7-84f3-b1a77120bf29.png)

### Examine the correlations between individual traits and identify extreme accessions

In order to examine the general correlation trends in the studied accessions, you can select the interesting traits in the side bar panel. You can remove the traits chosen by default. 

If you wish to examine the correlations between the chosen traits per species, you can do so by clicking a check-box in the side panel "Subset correlation per species" and chose the species from the drop down menu that will appear below

![tab3a](https://user-images.githubusercontent.com/14832460/34079982-9b45e790-e348-11e7-97f9-70d3855bd5f8.png)

If you want to examine which correlations are significant, you can mark the non-significant correlations by selecting the check box "Highlight non-significant correlations" in the side panel and chosing the p-value threshold. The non-significant correlations will be marked by the black cross.

![tab3a2](https://user-images.githubusercontent.com/14832460/34079983-9b66546c-e348-11e7-9c07-aa3ea31ee08a.png)

You can examine the most interesting correlations in detail in the sub-tab "Scatter plots", where you can select which traits are plotted on x- and y-axis in the side bar panel. The accession names will appear when you scroll the cursor through the graph. The p-value and correlation coefficient values are plotted above the graph. And different species are represented by dots of different colors. 

If you wish to examine the correlation per species, select the check box "Do you want to subset per species?". The correlation coefficient and p-value will change accordingly to the values that are plotted in the scatter graph below. 

![tab3b](https://user-images.githubusercontent.com/14832460/34079984-9b8e203c-e348-11e7-906a-3ba8542e8cab.png)

### Cluster the accessions based on the chosen phenotypes

The individual accessions will be clustered into different groups, depending on the selected traits. We have our favourite traits, reflecting salinity tolerance, but we do not exclude possibility that others would like group those accessions based on other phenotypic data collected.

You can chose the traits to be used for the hierarchical clustering in the side panel. And you can see as the heat map, and the clustering of your samples is changing while you add / remove the individual phenotypes. 

Again - you can perform this hierarchical cluster analysis on individual species separately, or on all species together - by clicking the checkbox "Do you want to subset per species?"

![tab4a](https://user-images.githubusercontent.com/14832460/34079985-9bac354a-e348-11e7-99ef-ec9fbb980a6f.png)

When you scroll down, you will see a more detailed dendrogram of the samples clustered based on the chosen traits. 

In order to select the number of the clusters, you need to enter the distance at which you would like to cut. Enter the number in the sidebar panel. As soon as you change the value, you will see the message in the grey message box above the dendrogram change, informing you how many clusters are formed if you cut at specific distance. 

You can download the table containing the information which accessions belongs to which cluster on the left side in the sidebard panel.

![tab4a2](https://user-images.githubusercontent.com/14832460/34079986-9bcb6992-e348-11e7-93f9-25d7c86da0e7.png)

The phenotypes representing indvidual clusters are represented in sub-tab "Cluster Validation", where you can select which traits will be displayed from the drop-down menu above the graph.

The letters above the graph represent the significant groups as identified with pariwide Tukey test with threshold p-value of 0.05.

![tab4b](https://user-images.githubusercontent.com/14832460/34079987-9bea7c88-e348-11e7-80f0-d1fd9f9a4b60.png)

_We hope you enjoyed this little instruction and will have fun exploring the data._
