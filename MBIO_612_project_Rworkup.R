library(devtools)
create("DeepSeaPackage")

getwd()
setwd("D:\\Data science in R\\Project_MBIO612")
setwd("DeepSeaPackage")


All_Capstone <- data.frame(read.csv("CAPSTONE_Filtered_Taxa.csv"), stringsAsFactors = FALSE)
use_data(All_Capstone)


#' The Capstone Filtered Taxa csv file
#'
#' A dataset containing abundance data by species for CAPSTONE dives around Southeast Pacific 
#'
#' @format A data.frame with 5099 rows and 29 columns
#' \describe{
#'  \item{ScientificName}{Character values representing species names.}
#'  \item{TaxonRank}{Character values representing the smallest taxon rank for that count}
#'  \item{Class}{Character values representing class of corresponding species}
#'  \item{Order}{Character values representing order of corresponding species}
#'  \item{Family}{Character values representing family of corresponding species}
#'  \item{Subfamily}{Character values representing subfamily of corresponding species}
#'  \item{Genus}{Character values representing genus of corresponding species}
#'  \item{IndividualCount}{Integer values representing species count observed}
#'  \item{SciName_Count}{Character values representing species name and count observed in one column}
#'  \item{Primary.Habitat}{Character values representing primary habitat type where species was observed}
#'  \item{secondary.habitat}{Character values representing secondary habitat type where species was observed}
#'  \item{Temperature}{Numeric values representing temperature in Celcius of water when species was observed}
#'  \item{Salinity}{Numeric values representing salinity in ppm of water when species was observed}
#'  \item{Oxygen}{Numeric values representing dissolved oxygen in ppm of water when species was observed}
#'  \item{Ocean}{character values representing ocean in which species was observed}
#'  \item{Region}{character values representing region of ocean in which species was observed}
#'  \item{Locality.1}{character values representing locality of ocean in which species was observed}
#'  \item{Locality.2}{character values representing second locality of ocean in which species was observed}
#'  \item{Locality.3}{character values representing third locality of ocean in which species was observed}
#'  \item{Locality.4}{character values representing fourth locality of ocean in which species was observed}
#'  \item{Station}{character values representing dive number of which species was observed}
#'  \item{ObservationDate}{character values representing date on which species was observed}
#'  \item{ObservationYear}{character values representing year in which species was observed}
#'  \item{ObservationTime}{character values representing time at which species was observed}
#'  \item{Latitude}{numeric values representing latitude at which species was observed}
#'  \item{Longitude}{numeric values representing longitude at which species was observed}
#'  \item{DepthInMeters}{numeric values representing depth at which species was observed}
#'  \item{Mega.Habitat}{character values representing megahabitat at which species was observed}
#'  \item{Habitat2}{character values representing habitat type at which species was observed}
#' }
#' @source NOAA Capstone Dives 
"All_Capstone"
document()

data(All_Capstone)
?All_Capstone

#' Subset function to look at data for singular region
#'
#' Subsets any data frame to create a new data frame in the Global environment. 
#' You can choose the specific region to subset with this function. 
#'
#' @param df The name of the data frame you are subsetting
#' @param x The name of the region you would like to subset the data
#' @param new_name The new name of the data frame with the subsetted region 
#' @return The data frame with only the specific region chosen 
subset_region<- function(df, x, new_name){
  All_HI <- subset(df,df$Region==x, drop = F)
  assign(new_name,All_HI,.GlobalEnv)
}
dump("subset_region", file = "R/subset_region.R")

#' Count function for all localities (Locality.1)
#'
#' Sum function to calculate total counts for each Locality.1 and then view 
#' the data summarized across each locality. Running the function will open a
#' new window that shows the Locality.1 and the total count of each of those localities. 
#' 
#' @return A new window with the data summarized by Locality.1 
#' @import dplyr
count_func_locality <- function(){
  aggregate(All_HI$IndividualCount, by=list(Locality.1=All_HI$Locality.1), FUN=sum, na.rm = T) %>%
  view()
}
dump("count_func_locality", file = "R/count_func_locality.R")

#' Removes specified localities
#'
#' remove_row function removes any rows of the specified locality.1.
#' If the data is spatially clustered and you want to remove outliers or certain areas.
#' 
#' @param x The name of the Locality.1 that you want to remove from the data frame, in quotations
#' @return Updated All_HI dataframe with removed rows
#' @import dplyr
#' @import tidyverse
remove_row<- function(x){
  All_HI <- subset(All_HI, Locality.1 != x)
  assign("All_HI",All_HI,.GlobalEnv)
}
dump("remove_row", file = "R/remove_row.R")

#' Function for grouping localities into larger sub.regions
#'
#' This function allows you to group certain localities in order to look at the data differently. 
#' It creates a new data frame called All_HI_2 that will go into your environment and has a 30 columns. 
#' The new column is called sub.region and will contain all of the Locality.1's that you indicate with the first argument (x). 
#' List the regions in quotations - c("Name","Name2") for first argument (x)
#' 
#' @param x The name of Locality.1 that you want to change, in quotations. Multiple in list format. 
#' @param new_name The name of the sub.region for that locality.1 
#' @return New data frame named All_HI_2 with new column sub.region
#' @import dplyr
#' @import stringr
new_region <- function(x, new_name){
  All_HI <- All_HI %>% mutate(sub.region=ifelse(str_detect(Locality.1, x), new_name, All_HI$Region))
  assign("All_HI_2",All_HI,.GlobalEnv)
}
dump("remove_row", file = "R/remove_row.R")

#' Function to create new data frames for specific sub.regions
#'
#' This function allows you to create a new data frame with just one chosen sub.region. 
#' It will create a new data frame in your environment with the name you provide with all the rows where hte Sub.region is 
#' the chosen area. Allows you to focus on certain regions in your analysis.
#' 
#' @param x The name of the sub.region you want to focus on, in quotations. 
#' @param sub.region The name of the new data frame, in quotations
#' @return New data frame with name of your choice for specified sub.region
subregion_subset <- function(x,sub.region){
  subreg <- subset(All_HI_2, Sub.Region==x, drop = F)
  assign(sub.region,subreg,.GlobalEnv)
}
dump("subregion_subset", file = "R/subregion_subset.R")

#' Function for iNext rarefaction curves
#'
#' This function creates a rarefaction curve with the data grouped how you choose. 
#' 
#' @param df The name of the data frame you would like to use
#' @param x The name of the column you would like to group the data by for the curve
#' @param y The number of the column you would like to use for grouping 
#' @return A rarefaction curve 
#' @import iNEXT
#' @import tidyverse
iNext_func <- function(df,x,y){
  #Fix Bug
  source ("https://gist.githubusercontent.com/zdealveindy/f30fa1f0264eabe95828218f61e63df0/raw/333937c2155dc3b8afddd1663e266b6183062602/plot.iNEXT_quickfix")
  iNext_data <- function(){
    df[,c(1,8,y)] %>%
      pivot_wider(names_from =x, 
                  values_from ="IndividualCount", 
                  values_fn = length,
                  values_fill = 0
      ) %>%
      data.frame() 
  }
  
  HI_iNext_locality <- iNext_data()
  
  #organize data into matrix to put into iNext
  HI_iNext_locality_2 <- as.matrix(apply(HI_iNext_locality[,-1],2,as.integer))
  
  #must add species name for row names 
  row.names(HI_iNext_locality_2) <- HI_iNext_locality[,1]
  
  HI_locality_rare <- iNEXT(HI_iNext_locality_2, q=0, datatype="abundance", size=NULL, endpoint=NULL, knots=40,
                            se=TRUE, conf=0.95, nboot=50)
  ggiNEXT(HI_locality_rare)+
    theme_bw(base_size = 7)
}
dump("iNext_func", file = "R/iNext_func.R")

#' Function for 3 sub.region iNext rarefaction curves
#'
#' This function creates three rarefaction curves with the data grouped on each 
#' of the three main Sub.Regions (EEZ - Economic exclusive zone around the Hawaiians Isl.,
#' MHI - Main Hawaiian Islands, and NWHI - Northwest Hawaiian Islands). The function grid arranges the three  
#' curves; adjustment may be needed to visualize properly. In order to run this function, 
#' you must have a data set called All_HI_2
#' 
#' @return Three rarefaction curves for the EEZ, MHI, and NWHI
#' @import iNEXT
#' @import tidyverse
#' @import grid
#' @import gridExtra
habitat_by_region_iNext <- function(){
  subregion_subset <- function(x,sub.region){
    subreg <- subset(All_HI_2, Sub.Region==x, drop = F)
  }
  All_HI_2[,"Habitat2"] <- as.numeric(as.factor(All_HI_2[,"Habitat2"]))
  subregion_subset("Northwestern Hawaiian Islands", "NWHI")
  subregion_subset("Main Hawaiian Islands","MHI")
  subregion_subset("Exclusive Economic Zone around Hawaiian Islands", "EEZ")
  
  NWHI_iNext_Habitat_data <- function(){
    NWHI[,c(1,8,29)] %>%
      pivot_wider(names_from =Habitat2, 
                  values_from ="IndividualCount", 
                  values_fn = length,
                  values_fill = 0
      ) %>%
      data.frame() 
  }
  NWHI_iNext_Habitat <- NWHI_iNext_Habitat_data()
  
  #organize data into matrix to put into iNext
  NWHI_iNext_Habitat_2 <- as.matrix(apply(NWHI_iNext_Habitat[,-1],2,as.integer))
  
  #must add species name for row names 
  row.names(NWHI_iNext_Habitat_2) <- NWHI_iNext_Habitat[,1]
  
  NWHI_habitat_rare <- iNEXT(NWHI_iNext_Habitat_2, q=0, datatype="abundance", size=NULL, endpoint=NULL, knots=40,
                             se=TRUE, conf=0.95, nboot=50)
  
  NWHI_habitat_rare <- ggiNEXT(NWHI_habitat_rare)+
    theme_bw(base_size = 7)+
    ggtitle("NWHI")
  
  
  #iNext the MHI by feature 
  MHI_iNext_Habitat_data <- function(){
    MHI[,c(1,8,29)] %>%
      pivot_wider(names_from =Habitat2, 
                  values_from ="IndividualCount", 
                  values_fn = length,
                  values_fill = 0
      ) %>%
      data.frame() 
  }
  MHI_iNext_Habitat <- MHI_iNext_Habitat_data()
  
  #organize data into matrix to put into iNext
  MHI_iNext_Habitat_2 <- as.matrix(apply(MHI_iNext_Habitat[,-1],2,as.integer))
  
  #must add species name for row names 
  row.names(MHI_iNext_Habitat_2) <- MHI_iNext_Habitat[,1]
  
  MHI_habitat_rare <- iNEXT(MHI_iNext_Habitat_2, q=0, datatype="abundance", size=NULL, endpoint=NULL, knots=40,
                            se=TRUE, conf=0.95, nboot=50)
  
  MHI_habitat_rare <- ggiNEXT(MHI_habitat_rare)+
    theme_bw(base_size = 7)+
    ggtitle("MHI")
  
  #EEZ iNext by feature
  #iNext the NWHI by feature 
  EEZ_iNext_Habitat_data <- function(){
    EEZ[,c(1,8,29)] %>%
      pivot_wider(names_from =Habitat2, 
                  values_from ="IndividualCount", 
                  values_fn = length,
                  values_fill = 0
      ) %>%
      data.frame() 
  }
  EEZ_iNext_Habitat <- EEZ_iNext_Habitat_data()
  
  #organize data into matrix to put into iNext
  EEZ_iNext_Habitat_2 <- as.matrix(apply(EEZ_iNext_Habitat[,-1],2,as.integer))
  
  #must add species name for row names 
  row.names(EEZ_iNext_Habitat_2) <- EEZ_iNext_Habitat[,1]
  
  EEZ_habitat_rare <- iNEXT(EEZ_iNext_Habitat_2, q=0, datatype="abundance", size=NULL, endpoint=NULL, knots=40,
                            se=TRUE, conf=0.95, nboot=50)
  
  EEZ_habitat_rare<- ggiNEXT(EEZ_habitat_rare)+
    theme_bw(base_size = 7)+
    ggtitle("EEZ")
  
  grid.arrange(NWHI_habitat_rare, MHI_habitat_rare, EEZ_habitat_rare, nrow=3)
}
dump("habitat_by_region_iNext", file = "R/habitat_by_region_iNext.R")

#' Function for PCA
#'
#' This function takes any data frame and lets you run a PCA on certain 
#' variables. It takes any data frame, runs PCA on it and produces a biplot colored
#' how you specify. You can change a column in your data into a numeric column 
#' in order to include in PCA. You have to specify the numeric column numbers (including
#' the number of the column you changed to numeric). Lastly, you give it a column to group the data by.  
#' 
#' @param df The data frame to run the PCA on
#' @param x Column name to change to numeric to include as variable, use list to change multiple columns
#' @param y List of column numbers included in PCA (must be numeric, include the one(s) you change)
#' @param w Column name to group by, in quotation
#' @return One PCA biplot showing points and variables
#' @import devtools
#' @import ggplot2
#' @import factoextra
#' @import ggfortify
pca_func <- function(df,x,y,w){
  df[,x] <- as.numeric(as.factor(df[,x]))
  df_pca = prcomp(df[,y], scale = TRUE) #select col12:14 for temp, sal, oxy + col27 for depth + col31 for habitat
  autoplot(df_pca, data = df, colour = w,
           loadings = TRUE, loadings.colour = 'blue',
           loadings.label = TRUE, loadings.label.size = 3)+
    theme(text = element_text(size = 8))
}
dump("pca_func", file = "R/pca_func.R")


#' Function for Phylogram
#'
#' This function takes any data frame and lets you create a phylogram with the species
#' that data frame. You can choose the column to run for the phylogram, it must be species,
#' genus, family, etc. The function creates a list of all the species, removes the duplicate values 
#' and creates a phylogram on that list. This takes a long time to run.
#' 
#' @param df The data frame to create a phylogram
#' @param x the column to create phylogram on, can be species, genus, family, etc. 
#' @return One phylogram all of the species
#' @import phylogram
#' @import taxize
phylogram_func <- function(df,x){
  species_name <- df[,x] #make list of species in all of Hawaiian region
  species_name <- species_name[!duplicated(species_name)] #remove duplicate species names
  
  out <- classification(species_name, db='itis') #this takes a long time to run, finds associated taxa ratings in order to make phylo tree
  tr <- class2tree(out) #removes species without classification
  plot(tr, type = 'phylogram', no.margin= T,srt=0, label.offset=0.8, font = 1, cex=0.38, adj=0, node.pos=2,)
  title("Phylogram", line = -1.3, adj = 0.01)
}
dump("phylogram_func", file = "R/phylogram_func.R")

#' Function for Anosim test
#'
#' This function takes any data frame and run an anosim test across variables of your choice. 
#' Before running this function, the variable/column you choose must be a factor prior to running
#' the function. Additionally, any numeric columns (like Year, Date) should be changed to characters. 
#' The function takes the data and flips it, so that the samples are row numbers and individual counts
#' for each sample. The function then splits the data to a data frame with just the counts and samples. 
#' And then takes all the columns that are factors and puts in data frame for the environmental variables. 
#' The function takes the name of the column to run the anosim against to see significance across samples. 
#' 
#' @param df The data frame to use for ANOSIM
#' @param x The column name for sample types (i.e. dive numbers, sample numbers, etc)
#' @param y The column name that you want to run ANOSIM for, must convert to factor prior to running function
#' @return A data frame called df. anosim with the ANOSIM results
#' @import tidyverse
#' @import dplyr
#' @import vegan
anosim_func <- function(df,x,y){
  df.pivot <- df %>%
    pivot_wider(names_from = x, 
                values_from = "IndividualCount", 
                values_fn = length,
                values_fill = 0) %>%
    data.frame()
  df.1 <- select_if(df.pivot, is.integer)%>%
    lapply(as.numeric)%>%
    data.frame()
  df.env <- select(df.pivot,where(is.factor))
  a <- df.env[[y]]
  df.anosim <- adonis2(df.1~a, data = df.env, permutations = 999, method="bray")
  assign("df.anosim",df.anosim,.GlobalEnv)
  view(df.anosim)
}
dump("anosim_func", file = "R/anosim_func.R")

#' Function for Permanova test
#'
#' This function takes any data frame and runs a permanova and plots confidence ellipses. 
#' It will also assign the betadisper and permutest results to the global
#' environment. It can take any data frame, the column with sample types and the 
#' variable to run it against. This function may take a long time to run. 
#' 
#' 
#' @param df The data frame to use for permanova
#' @param x The column name for sample types (i.e. dive numbers, sample numbers, etc)
#' @param y The column name that you want to run permanova for, must convert to factor prior to running function
#' @return A data frame called dispersion with the betadisper results 
#' @return A plot of the confidence ellipses
#' @return A data frame called df.perma for permutest 
#' @import tidyverse
#' @import dplyr
#' @import vegan
perma_func <- function(df,x,y){
  df.pivot <- df %>%
    pivot_wider(names_from = all_of(x), 
                values_from = "IndividualCount", 
                values_fn = length,
                values_fill = 0) %>%
    data.frame()
  df.1 <- select_if(df.pivot, is.integer)%>%
    lapply(as.numeric)%>%
    data.frame()
  df.env <- select(df.pivot,where(is.factor))
  df.dist <- vegdist(df.1, method="bray")
  a <- df.env[,y]
  dispersion <- betadisper(df.dist, group=a)
  assign("dispersion",dispersion,.GlobalEnv)
  df.perma <- permutest(dispersion)
  assign("df.perma",df.perma,.GlobalEnv)
  plot(dispersion, hull=FALSE, ellipse=TRUE)
}
dump("perma_func", file = "R/perma_func.R")

### add the following DESCRIPTION File
Imports:
  dplyr
  tidyverse
  stringr
  iNext
  devtools
  ggplot2
  factoextra
  ggfortify
  phylogram 
  taxize
  vegan 

### Run the following to set the license to MIT. Change if needed
use_mit_license()

check()

build()
