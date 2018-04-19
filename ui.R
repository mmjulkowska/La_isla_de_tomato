fluidPage(
theme = shinytheme("flatly"),
navbarPage(title="Tomato seedlings screen", inverse=F,
           
# - - - - - - - start tabPanel # 1 - - - - - - - - - 
tabPanel("Background Information",icon = icon("book"),
sidebarPanel(
fluidRow(
"This Application was build to allow further exploration of the natural variation in salt stress response of tomato seedlings collected from Galapagos islands.",br(), br(), 

"This Application was developed to complement the manuscript entitled:",br(),br(),
strong("Salinity responses in Galapagos tomatoes"), br(),br(),
tags$i("Yveline Pailles, Mariam Awlia, Magdalena Julkowska, Luca Passone, Khadija Zemmouri, Sonia Negrao, Sandra M. Schmöckel, Mark Tester"),br(),br(),
"The publication is currently submitted for revision", 
br(), br(),
"The README / information on how to use this app is available ", a("HERE!", href="https://mmjulkowska.github.io/La_isla_de_tomato/"), br(),br(),
"The app was developed by Magdalena Julkowska", br(),
a("Magdalena.Julkowska@kaust.edu.sa", href="https://github.com/mmjulkowska"),br(),br(),
downloadButton("dataAll", label="Download underlying data")
)),
mainPanel(
navbarPage(title="",
tabPanel("Experimental Set-Up", icon = icon("flask"),
"The data presented here was obtained from the screen of 68 Galapagos tomato accessions (Pailles et al. 2017) for salinity tolerance, from which 41 are S. cheesmaniae and 27 are S.galapagense. Compared to three commercial tomato varieties: Heinz 1706, VF-36 and Moneymaker. Seeds were treated with 10% bleach solution for 10 to 30 minutes and washed, then sown onto two agar plugs, supported by black plastic pellets, in each pot.",
br(), br(),
"Six biological replicates were used for each control and salt treatment, another six replicates were harvested before the salt treatment to measure the trait changes due to the treatment. An ebb-and- flow hydroponics system was used. The nutrient solution was pumped in intervals of 15 minutes.", 
br(), br(),
"The salt treatment was applied at the 4 th leaf emergence. S. lycopersicum plants were treated at the 3 rd leaf emergence, to compensate for size difference. Treatment was administered in two increments: 75 mM NaCl for 12 h and 200 mM NaCl for 10 d. Supplemental CaCl 2 was added to maintain Ca 2+ activity.",
br(), br(),
"After treatment, plants were photographed and tissues harvested to measure traits related to plant growth, leaf area, and ion allocation.",
br(),br(),
img(src="experimental_setup1.tif", align="center"), 
br(),br(),
"References:",
br(),br(),
"Detailed methods are described in:", br(), 
"Pailles Y., Awlia M., Julkowska M., Zemmouri K., Passone L., Negrão S., Schmöckel S.M. & Tester M. (2018) Wide variation in responses to salinity in seedlings of Galapagos tomatoes. Submitted for peer-review",
br(),br(),
"Description of the Galapagos tomatoes collection is found in:",
br(),
"Pailles Y., Ho S., Pires I.S., Tester M., Negrão S. & Schmöckel S.M. (2017) Genetic diversity and population structure of two tomato species from the Galapagos Islands. Frontiers in Plant Science (8):1–11"

         ),
tabPanel("Traits", icon = icon("key"),
         "List of measured traits:", br(), br(),
         "- Days to germination", br(),
         "- Days to 4th leaf", br(),
         "- Leaf number", br(),
         "- Root length (cm)", br(),
         "- Stem length (cm)", br(),
         "- Stem thickness (mm)", br(),
         "- Shoot fresh mass (mg)", br(),
         "- Plant fresh mass (mg)", br(),
         "- Root dry mass (mg)", br(),
         "- Shoot dry mass (mg)", br(),
         "- Plant dry mass (mg)", br(),
         "- Water content (mg)", br(),
         "- Water concentration - water content/plant dry mass (mg/mg)", br(),
         "- Green pixel count initial", br(),
         "- Green pixel count final", br(),
         "- Green pixel count difference", br(),
         "- Leaf fresh mass (mg)", br(),
         "- Leaf dry mass (mg)", br(),
         "- Leaf area (cm2)", br(),
         "- Leaf Na concentration (mM)", br(),
         "- Leaf K concentration (mM)", br(),
         "- Leaf succulence - leaf water content/leaf area (mg/cm2)", br(),
         "- Leaf perimeter (cm)", br(),
         "- Leaf vertical length (cm)", br(),
         "- Leaf horizontal width (cm)", br(),
         "- Leaf elongation - width/length (cm)", br(),
         "- Root fresh mass (mg)", br(),
         "- Root Na concentration (mM)", br(),
         "- Root K concentration (mM)", br(),
         br(), br(),
         "Traits in salt treated plants relative to control treated plants were calculated as follows: ",br(),
         img(src="relative_calc.tif", align="center")
)))
# end tabPanel # 1
),

# - - - - - - - start tabPanel # 2 - - - - - - - - - 
tabPanel("Compare Individual Lines", icon=icon("leaf"),
sidebarPanel(
fluidRow(
helpText("Select genotypes to compare their salinity tolerance", br(), br(), "The boxplots represent the selected phenotype of eight chosen accessions, and the letters above the graphs represent the significant groups as calculated using Tukey pair-wise comparison with p-value < 0.05.", br(), br(),
strong("DISCLAIMER:"), "As the results presented in here are based only on few biological replicates, the significant differences between the accessions should be interpreted with caution."),
uiOutput("Select_trait_for_anova"),
uiOutput("Select_condition_for_anova"),
uiOutput("Select_genotype_for_anova"),
hr(),
checkboxInput("ANOVA_background", "Remove background?"),
checkboxInput("ANOVA_grid", "Remove major grid lines?"),
selectizeInput("ANOVA_graph_type", "Graph type:", choices = c("Box plot", "Bar graph", "Scatter plot"), selected = "Box plot"),
uiOutput("ANOVA_bar_error")
)),
mainPanel(
verbatimTextOutput("ANOVA_message"),
plotOutput("plot2a"),
verbatimTextOutput("Tukey_message"),
uiOutput("Download_ANOVA_table"),
dataTableOutput("Table1")) 
# end tabPanel # 2
),

# - - - - - - - start tabPanel # 3 - - - - - - - - - 
tabPanel("Examine correlations between individual traits", icon=icon("search-plus"),
  navbarPage("",
      tabPanel("General Correlations",
               sidebarPanel(
                 selectInput("Trait_corr_graph_biggie", label="Select traits to be included in the correlation analysis", choices= list_casted, multiple = T, selected = c("control_Green_pixel_count_final", "control_Leaf_area", "control_Leaf_fresh_mass", "control_Leaf_perimeter", "control_Plant_dry_mass", "control_Water_content", "salt_Green_pixel_count_final", "salt_Leaf_area", "salt_Leaf_fresh_mass", "salt_Leaf_perimeter", "salt_Plant_dry_mass", "salt_Water_content")),
                 checkboxInput("Big_Cor_subset_Q", "Subset the correlation per species"),
                 uiOutput("Big_cor_subset_S"),
                 selectInput("corrplotMethod", "Plot Method:", choices = c("circle", "square", "ellipse", "number", "shade", "color", "pie")),
                 selectInput("corType", "Plot Type:", choices = c("full", "lower", "upper")),
                 selectInput("corOrder", "Order the lables by:", choices = list("Original order" = "original","Angular order of eigenvectors" = "AOE", "First Principal Component order"  = "FPC", "Hierarchical clustering order"  = "hclust", "Alphabetical order" = "alphabet")),
                 checkboxInput("sig_cor", "Highlight non-significant correlations"),
                 uiOutput("sig_cor_input")
               ),
               mainPanel(
                 plotOutput("BIG_correlation_graph"),
                 downloadButton("dwnldCorr", "Download correlation table"),
                 dataTableOutput("BIG_cor_table")
               )
               ),       
      tabPanel("Scatter plots",
        sidebarPanel(
        fluidRow(
helpText("In this tab you can explore the correlations between individual traits. The values representing individual lines are based on the average calculated per accession per condition. The names of the lines are plotted next to the points representing the individual line. The Pearson correlation coefficient (r) and p-value are plotted above the graphs", br(),
"We hope that those correlation graphs will be helpful to select accessions showing extreme phenotypes, that can be used in, for example for bulked segregant analysis or in depth individual study of the specific lines"),
selectInput("Trait_cor1", label=("Trait 1"), choices= list_casted, selected = "salt_Leaf_area"),
selectInput("Trait_cor2", label=("Trait 2"), choices= list_casted, selected = "salt_Leaf_fresh_mass"),
checkboxInput("Scat_Cor_subset_Q", "Do you want to subset per species?"),
uiOutput("Scat_subset_S")
)),
mainPanel(column(4,helpText("The Pearson Correlation coefficient (r) is ", textOutput("corr"))),
          column(4,helpText("The p-value is ",textOutput("corpval"))),
          column(12,plotlyOutput("plot3")))))
# end tabPanel # 3
),

# - - - - - - - start tabPanel # 4 - - - - - - - - - 
tabPanel("Cluster Analysis", icon=icon("magic"),
sidebarPanel(
fluidRow(
"Cluster analysis can be performed for any set of chosen traits. Therefore, in this tab we integrated our dataset so that you can perform your own cluster analysis on the traits of your interest.", br(), br(),
br(),
"Please select three phenotypes that you would like to take into consideration for the cluster analysis.", 

selectizeInput("Clust_traits", label="Traits", choices=list_casted, multiple=T, selected = c("relative_Leaf_elongation", "relative_Leaf_Na_concentration", "relative_Leaf_succulence", "relative_Leaf_number", "relative_Root_K_concentration", "relative_Root_Na_concentration", "relative_Stem_length", "relative_Leaf_area", "relative_Plant_fresh_mass", "relative_Green_pixel_count_difference", "relative_Leaf_K_concentration", "relative_Root_length")),
checkboxInput("Clust_subset_Q", "Do you want to subset per species?"),
uiOutput("Clust_subset_S"),

br(),
"Have a look at the dendrogram and determine the value which should be used for splitting the accessions into individual clusters.", br(), br(),
strong("ALERT:"), "Please note that if you chose too many clusters this analysis will not be informative and the subsequent post-hoc analysis will not be possible. We advise for the optimal number of clusters between three to ten.", br(), br(),
"Below please enter the value at which you wish to cut the dendrogram",
numericInput("tree_cut", label="Cut tree at", value="4"),
br(),
helpText("Download the data to examine which accessions belong to which cluster"),
downloadButton("dwnldClust", label = "Download Cluster data")
)),
mainPanel(
navbarPage(title="",
tabPanel("Clustering Heat Map",
      helpText("This graph represents the clustering of the lines based on three selected traits. The colors represent the phenotype value which has been normalized per trait with Z-Fisher transformation. Please note that the data per accession is based on the average trait value collected from 4 replicas only. "),
      plotOutput("HotHeatMap", height = 800),
      helpText("The below graph represents a dendrogram of the accessions and how they are clustered based on the selected traits. The accessions are clustered using average (Unweighted Pair Group Method with Arithmetic Mean) linkage method and the x-axis is represents the distance between the individual accessions."),
      verbatimTextOutput("Cluster_message"),
      plotOutput("ClusterTree")),
tabPanel("Cluster Validation",
         strong("DISCLAIMER:"), "Note that the differences between the clusters may be due to the unequal distribution of the accessions between the samples. Therefore, the results presented in here should be interpreted with caution.", br(),br(),
         selectInput("Clust_test", label="chose a trait to test", choices=list_casted, selected = "salt_Leaf_Na_concentration"),   
      br(),
      plotOutput("HotANOVA"),
helpText("The boxplot represents the phenotype value per cluster. The average values per accessions were pulled depending on the cluter cut-off value. The letters above the graph represent the significant groups as calculated using Tukey pair-wise comparison with p-value < 0.05.")))
)))
# end of tabPanel#4
)