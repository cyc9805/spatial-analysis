############################################ 
# Running a spatial autocorrelation 
############################################ 

# A spatial autocorrelation quantifies the degree of which objects are similar to nearby objects. 
# Variables are said to have a positive spatial autocorrelation when similar values tend to be 
# nearer together than dissimilar values. 
#  
# Waldo Tober’s first law of geography is that “Everything is related to everything else,  
# but near things are more related than distant things.” so we would expect most geographic 
# phenomena to exert a spatial autocorrelation of some kind. In population data this is  
# often the case as persons with similar characteristics tend to reside in similar neighbourhoods 
# due to a range of reasons including house prices, proximity to workplaces and cultural factors. 
#  
# We will be using the spatial autocorrelation functions available from the spdep package. 
# https://towardsdatascience.com/exploratory-spatial-data-analysis-esda-spatial-autocorrelation-7592edcbef9a 
# https://mgimond.github.io/Spatial/spatial-autocorrelation-in-r.html 
# https://mgimond.github.io/Spatial/spatial-autocorrelation.html 
# https://rpubs.com/quarcs-lab/spatial-autocorrelation 
# https://rspatial.org/raster/analysis/3-spauto.html# 
# https://bookdown.org/lexcomber/brunsdoncomber2e/Ch8.html 
# https://data.cdrc.ac.uk/system/files/practical9_0.html 
# https://mgimond.github.io/simple_moransI_example/
# https://desktop.arcgis.com/en/arcmap/10.7/tools/spatial-statistics-toolbox/h-how-cluster-and-outlier-analysis-anselin-local-m.html

rm(list=ls())
library(spdep)    # Spatial Dependence: Weighting Schemes, Statistics. 
library(sf)       # for sf data format 
library(tmap)     # for mapping 
library(GISTools) # for georgia data 

# load georgia data 
data(georgia) 

# convert the georgia sp object to sf: 
ga <- st_as_sf(georgia) 

# MedInc: median county income 
ga$MedInc 

# check the distribution of the attribute values. 
# Medinc: median county income 
hist(ga$MedInc, main=NULL) 
boxplot(ga$MedInc, horizontal=T) 

tm_shape(ga) + 
  tm_fill(col="MedInc", style="jenks", n=8, palette="Greens") +
  tm_borders(alpha=1) +
  tm_legend(outside=T) 

help("tm_fill")

################################################################## 
# Global Moran’s I analysis 
################################################################## 

# =========================================== 
# 1: Define neighboring polygons 
# =========================================== 

# The first step in a Moran’s I analysis requires that we define “neighboring” polygons. 
# This could refer to contiguous polygons, polygons within a certain distance, or it could 
# be non-spatial in nature and defined by social, political or cultural “neighbors”. 
# Here, we’ll adopt a contiguous neighbor definition. We’ll accept any contiguous polygons 
# that share at least one vertex; this is the “queen” case (if one chooses to adopt  
# the chess analogy) and it’s parameterized as queen = TRUE in the call to poly2nb. 
# If we required that just edges be shared between polygons then we would set 
# queen = FALSE (the rook analogy). 

nb <- poly2nb(ga, queen=TRUE) 
# For each polygon in our shape object, nb lists all neighboring polygons. 

# =========================================== 
# 2: Assign weights to the neighbors 
# =========================================== 

# Next, we need to assign weights to each neighboring polygon. In this example,  
# each neighboring polygon will be assigned equal weight when computing the neighboring  
# median income values. 

# Convert the neighbour data to a listw object 
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
lw$weights[1]
# "W": row normalized: 자신과 인접한 폴리곤들에게 부여된 가중치 합이 1
# zero.policy는 인접한 폴리곤이 없을때 null이 아닌 0으로 채워넣음
# "B": basic binary 
#lw <- nb2listw(nb, style="B", zero.policy=TRUE) 

# These are the weights each neighboring income value will be multiplied by before being summed. 
# If a polygon has 5 neighbors each neighbor will have a weight of 1/5 or 0.2 with "W" option. 

# ============================================ 
# 3: Computing the Moran’s I statistic 
# ============================================ 

# The Moran’s I statistic can be computed using the moran function. 
# Szero computes the total sum of weight matrix
I <- moran(ga$MedInc, lw, length(nb), Szero(lw))[1] 
I 

# The Moran I statistic is 0.505, we can, therefore, determine that there our MedInc 
# variable is positively autocorrelated in Georgia. In other words, the data does  
# spatially cluster. 
# We can also consider the p-value as a measure of the statistical significance of the model. 

# ======================================================== 
# 4: Performing a hypothesis test [analytical method] 
# ======================================================== 

# The hypothesis we are testing states that “the income values are randomly distributed across 
# counties following a completely random process”. There are two methods to testing this  
# hypothesis: an analytical method and a Monte Carlo method. We’ll explore both approaches 
# in the following examples. 
# To run the Moran’s I analysis using the analytical method, use the moran.test function. 

moran.test(ga$MedInc,lw, alternative="greater") 

# The Moran’s I statistic is 0.505. The p-value is very small suggesting  
# that there is a sufficient evidence that we can reject the null hypotheis,  
# which is the distribution of income is random. 
# alternative = "greater" means we adopted one-sided test instead of two-sided test. 

# ======================================================== 
# 5: Performing a hypothesis test [Monte Carlo method] 
# ======================================================== 

# The analytical approach to the Moran’s I analysis benefits from being fast.  
# But it may be sensitive to irregularly distributed polygons. 
# A safer approach to hypothesis testing is to run an MC simulation using  
# the moran.mc() function.  
# The moran.mc function takes an extra argument n, the number of simulations. 
# 999개의 랜덤 데이터를 통해 moran's I 계산 

MC<- moran.mc(ga$MedInc, lw, nsim=999, alternative="greater") 

# View results (including p-value) 
MC 

# The MC simulation generates a very small p-value, 0.001. This is not surprising given 
# that the income values are strongly clustered. We can see the results graphically  
# by passing the Moran’s I model to the plot function: 
# Plot the Null distribution (note that this is a density plot instead of a histogram) 
par(mar=c(1.5,1.5,1.5,1.5))
plot(MC) 

# The curve shows the distribution of Moran I values we could expect had the incomes been  
# randomly distributed across the counties. Note that our observed statistic, 0.505, falls way 
# to the right of the distribution suggesting that the income values are clustered  
# (a positive Moran’s I value suggests clustering whereas a negative Moran’s I value suggests  
# dispersion). 
# Now, had the Moran’s I statistic been negative (suggesting a dispersed pattern), you would  
# probably want to set the alternative argument to "less" thus giving you the fraction of 
# simulated I values more dispersed than your observed I value. 
#  
# A visual exercise that you can perform to assess how “typical” or “atypical” your pattern 
# may be relative to a randomly distributed pattern is to plot your observed pattern 
# alongside a few simulated patterns generated under the null hypothesis. 

ga$rand1 <- sample(ga$MedInc, length(ga$MedInc), replace = FALSE) 
ga$rand2 <- sample(ga$MedInc, length(ga$MedInc), replace = FALSE) 
ga$rand3 <- sample(ga$MedInc, length(ga$MedInc), replace = FALSE) 

tm_shape(ga) + 
  tm_fill(col=c("MedInc", "rand1", "rand2", "rand3"), 
          style="quantile", n=8, palette="Greens", legend.show = FALSE) + 
  tm_facets(nrow=1) 


################################################################## 
# Local Moran’s I analysis 
################################################################## 

# ==================================================== 
# 1: Moran scatterplot 
# ==================================================== 

# We will first create a moran plot which looks at each of the values plotted against 
# their spatially lagged values. It basically explores the relationship between the data 
# and their neighbours as a scatter plot. The style refers to how the weights are coded. 
# “W” weights are row standardised (sums over all links to n). 

par(mfrow=c(1,1))
moran <- moran.plot(ga$MedInc, lw)
moran                   

# ======================================================= 
# (Optional) An alternative method for Moran scatterplot 
# ======================================================= 

# We’ll first have R compute the average neighbor income value for each polygon.  
# These values are often referred to as spatially lagged values. 

inc.lag <- lag.listw(lw, ga$MedInc) 
inc.lag 

# You can plot the relationship between income and its spatially lagged counterpart as follows. 
# The fitted blue line added to the plot is the result of an OLS regression model. 
plot(inc.lag ~ ga$MedInc, pch=16, asp=1) 
M1 <- lm(inc.lag ~ ga$MedInc) 
abline(M1, col="blue") 
abline(v=mean(ga$MedInc), h=mean(inc.lag), col="red") 

# The slope of the line is the Moran’s I coefficient. You can extract its value from  
# the model object M1 as follows: 

M1$coefficients
coef(M1)[2] 

# The moran’s I coefficient is 0.505. The positive (upward) slope suggests that as the income 
# value of a said polygon increases, so does those of its neighboring polygons. 
# If the slope were negative (i.e. sloping downward), this would suggest a negative  
# relationship whereby increasing values in a said polygon would be surrounded by polygons 
# with decreasing income values. 

# ==================================================== 
# 2: Compute local Moran 
# ==================================================== 

local <- localmoran(x = ga$MedInc, listw = nb2listw(nb, style = "W")) 
?localmoran 
# By considering the help page for the localmoran function (run ?localmoran in R console) 
# we can observe the arguments and outputs. We get a number of useful statistics  
# from the model which are as defined: 
#    
# Ii      : local moran statistic 
# E.Ii    : expectation of local moran statistic 
# Var.Ii  : variance of local moran statistic 
# Z.Ii    : standard deviate of local moran statistic 
# Pr()    : p-value of local moran statistic 

local
ga$lisa <- attr(local, "quadr")[,3]

# ==================================================== 
# 4: Plot local Moran 
# ==================================================== 

# A map the local moran statistic (Ii). 
# A positive value for Ii indicates that the unit is surrounded by units with similar values. 

ga$li <- local[,1] 
ga$li 

ga$lisa <- attr(local, 'quadr')[,3]

tm_shape(ga) + 
  tm_fill(col = 'li', 
          style = "quantile", 
          title = "local morans I") + 
  tm_borders(alpha=.4) + 
  tm_legend(outside=TRUE, legend.text.size=0.6)

tm_shape(ga) + 
  tm_fill(col = 'li', 
          style = "quantile", 
          title = "local morans I") + 
  tm_borders(alpha=.4) + 
  tm_legend(outside=TRUE, legend.text.size=0.6)  

tm_shape(ga) +  
  tm_polygons(col='li', 
              title="Local Morans I", 
              legend.format=list(flag="+")) + 
  tm_legend(outside=TRUE, legend.text.size=0.6) 

# Add color-blind option and a scale_bar 
tm_shape(ga) +  
  tm_polygons(col='li', 
              title="Local Morans I", 
              legend.format=list(flag="+")) + 
  tm_legend(outside=TRUE, legend.text.size=0.6) + 
  tm_style('col_blind') + 
  tm_scale_bar(width= 0.15, position = c("LEFT", "BOTTOM")) + 
  tm_legend(outside=TRUE, legend.text.size=0.6) 

# ==================================================== 
# 5: Plot p-value 
# ==================================================== 


# From the map, it is possible to observe the variations in autocorrelation across space. 
# We can interpret that there seems to be a geographic pattern to the autocorrelation. 
# However, it is not possible to understand if these are clusters of high or low values. 
# One thing we could try to do is to create a map which labels the features based  
# on the types of relationships they share with their neighbours (i.e. high and high,  
# low and low, insignificant, etc…). 
# The following code will run this for you. Source: Brunsdon and Comber (2015) 

# Add the local p-values 
ga$pval <- local[,5] 

tm_shape(ga) +  
  tm_polygons(col='pval',title="p-value",breaks=c(0,0.01,0.05,0.10,1), 
              border.col = "black", 
              palette = "-Greens") + 
  tm_legend(outside=TRUE, legend.text.size=0.6) 


# visualizing by cluster type
ga$cluster <- ifelse(ga$pval < 0.05, as.character(ga$lisa), "Not Sig.")
tm_shape(ga) +  
  tm_polygons(col='cluster',title="LISA Cluster", 
              border.col = "black") + 
  tm_legend(outside=TRUE, legend.text.size=0.6)

# 소득 지도와 소득에 따른 군집 유형 지도를 같이 표시함
map1 <- tm_shape(ga) + 
  tm_fill(col="MedInc", style="quantile", n=8, palette="Greens") +
  tm_borders(alpha=0.4) +
  tm_legend()

map2 <- tm_shape(ga) +  
  tm_polygons(col='cluster',title="LISA Cluster", 
              border.col = "black") + 
  tm_legend()

tmap_arrange(map1, map2)
