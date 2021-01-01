##
## title: "Capstone (CYO) Project HarvardX PH125.9x"
## author: "Gail McDaniel"
## date: "12/31/2020"

#########################################
# ---------- Load Packages  ------------#
#########################################

if (!require("DataExplorer")) install.packages("DataExplorer", repos = "http://cran.us.r-project.org")
if (!require("caret")) install.packages("caret", repos = "http://cran.us.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if (!require("reshape2")) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if (!require("tidyverse")) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require("ggcorrplot")) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if (!require("summarytools")) install.packages("summarytools", repos = "http://cran.us.r-project.org")
if (!require("stargazer")) install.packages("stargazer", repos = "http://cran.us.r-project.org")
if (!require("Rmisc")) install.packages("Rmisc", repos = "http://cran.us.r-project.org")
if (!require("rpart")) install.packages("rpart", repos = "http://cran.us.r-project.org")
if (!require("ggfortify")) install.packages("ggfortify", repos = "http://cran.us.r-project.org")
if(!require("kableExtra"))install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require("factoextra"))install.packages("factoextra", repos = "http://cran.us.r-project.org")
if(!require("ggplot2"))install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require("rpart.plot"))install.packages("rpart.plot", repos = "http://cran.us.r-project.org")


library("tidyverse")
library("dplyr")
library("ggplot2")
library("ggfortify")
library("kableExtra")
library("rpart")
library("caret")
library("ggcorrplot")
library("summarytools")
library("caTools")
library("stargazer")

# need "dataExplorer for plot_histogram
library("DataExplorer") 

# need reshape2 for meld
library("reshape2")

# need for 'multiplot'
library("Rmisc")

# need for plotting pca 
library("factoextra")

# need for plotting decision tree
library("rpart.plot")

theme_set(theme_bw(12))

setwd("C:\\Work\\Harvard Data Science\\PowerGeneration")

# ** Load Annual Generation Data **
# Read the annual generation data
path = "annual_generation_state.csv"
db <- read.table(path, sep=",", header=TRUE)


#########################################
# ---------- Data Wrangling ------------#
#########################################

#Select only observations of "Total Electric Power Industry"
db_filtered <- subset(db, TYPE.OF.PRODUCER == "Total Electric Power Industry")

# Lets remove the "US" rows since they're an aggregate of the states
db_filtered <- subset(db_filtered, STATE != "US-TOTAL" )
db_filtered <- subset(db_filtered, STATE != "US-Total" )

# let's drop columns "X", "X.1", "X.2", "X.3", "X.4", and "X.5"
db_filtered <- db_filtered[, -c(6:11)]


# Rename some columns for ease of work
colnames(db_filtered)[colnames(db_filtered) == 'ï..YEAR'] <- 'Year'
colnames(db_filtered)[colnames(db_filtered) == 'STATE'] <- 'State'
colnames(db_filtered)[colnames(db_filtered) == 'TYPE.OF.PRODUCER'] <- 'TypeOfProducer'
colnames(db_filtered)[colnames(db_filtered) == 'ENERGY.SOURCE'] <- 'EnergySource'
colnames(db_filtered)[colnames(db_filtered) == 'GENERATION..Megawatthours.'] <- 'Generation'

# remove commas (,) from the generation column
db_filtered$Generation <- str_remove_all(db_filtered$Generation, "[,]")

# Let's make sure the 'Generation' column is numeric 
db_filtered$Generation <-sapply(db_filtered$Generation,as.numeric)

# let's replase NA's with 0
db_filtered$Generation <- replace_na(db_filtered$Generation, 0)

nrow_narrow <- nrow(db_filtered)
ncol_narrow <- ncol(db_filtered)

# reshape the data from narrow and Long to Wide and Short
wide_db <- spread(data = db_filtered,
              key = EnergySource,
              value = Generation)

# **Loading Annual Sales Data** 
# Read the usage data
path = "sales_annual.csv"
sales = read.csv(path, header = TRUE)
names(sales)

#Select only observations of "Total Electric Power Industry"
sales_filtered <- subset(sales, Industry.Sector.Category == "Total Electric Industry")


# let's drop columns "Residential","Commercial","Industrial","Transportation", and "Other" 
sales_filtered <- sales_filtered[, -c(4:8)]


# Lets remove the "US" rows since they're an aggregate of the states
sales_filtered <- subset(sales_filtered, State != "US")


# Rename some columns for ease of work
colnames(sales_filtered)[colnames(sales_filtered) == 'ï..Year'] <- 'Year'
colnames(sales_filtered)[colnames(sales_filtered) == 'Industry.Sector.Category'] <- 'IndustrySector'


# remove commas (,) from the generation column
sales_filtered$Total <- str_remove_all(sales_filtered$Total, "[,]")

# Let's make sure the 'Total' column is numeric 
sales_filtered <- sales_filtered %>% mutate_at('Total', as.numeric)

# let's replase NA's with 0 from the sales data
sales_filtered$Total <- replace_na(sales_filtered$Total, 0)

names(sales_filtered)

# Let's write the Annual Annual sales data to a csv file....
write.csv(file="sales_annual_updated.csv", x=wide_db)

# let's rename some of annual generation columns
colnames(wide_db)[colnames(wide_db) == "YEAR"] <- 'Year'
colnames(wide_db)[colnames(wide_db) == "STATE"] <- 'State'
colnames(wide_db)[colnames(wide_db) == "Hydroelectric Conventional"] <- 'ConvHydro'
colnames(wide_db)[colnames(wide_db) == "Natural Gas"] <- 'NatGas'
colnames(wide_db)[colnames(wide_db) == "Other Biomass"] <- 'Biomass'
colnames(wide_db)[colnames(wide_db) == "Other Gases"] <- 'OtherGases'
colnames(wide_db)[colnames(wide_db) == "Pumped Storage"] <- 'PSHydro'
colnames(wide_db)[colnames(wide_db) == "Solar Thermal and Photovoltaic"] <- 'Solar'
colnames(wide_db)[colnames(wide_db) == "Wood and Wood Derived Fuels"] <- 'Wood'

# let's replase NA's with 0 (Rnewable)
wide_db$Biomass <- replace_na(wide_db$Biomass, 0)
wide_db$ConvHydro <- replace_na(wide_db$ConvHydro, 0)
wide_db$Geothermal <- replace_na(wide_db$Geothermal, 0)
wide_db$Solar <- replace_na(wide_db$Solar, 0)
wide_db$Wind <- replace_na(wide_db$Wind, 0)
wide_db$Wood <- replace_na(wide_db$Wood, 0)

# let's replase NA's with 0 (Fossil)
wide_db$Coal <- replace_na(wide_db$Coal, 0)
wide_db$NatGas <- replace_na(wide_db$NatGas, 0)
wide_db$OtherGases <- replace_na(wide_db$OtherGases, 0)
wide_db$Petroleum <- replace_na(wide_db$Petroleum, 0)

# let's replase NA's with 0 (Other Energy Sources)
wide_db$Nuclear <- replace_na(wide_db$Nuclear, 0)
wide_db$Other <- replace_na(wide_db$Other, 0)
wide_db$PSHydro <- replace_na(wide_db$PSHydro, 0)


# Add calculated columns to the annual generation data
wide_db[,'Renewable'] <- (wide_db[,'Geothermal'] + wide_db[,'Biomass'] +
                            wide_db[,'ConvHydro'] + wide_db[,'Solar'] +
                            wide_db[,'Wind'] + wide_db[,'Wood'])

wide_db[,'Fossil'] <- (wide_db[,'Coal'] + wide_db[,'OtherGases'] +
                            wide_db[,'Petroleum'] + wide_db[,'NatGas'])

wide_db[,'Ratio'] <- (wide_db$Renewable/wide_db$Total)

wide_db[,'Level'] <- ifelse(wide_db$Ratio >= 0.75, "High", "Low")


# add usage column from sales data
wide_db[,'Usage'] <- sales_filtered$Total

# reorder columns in the Annual generation data
wide_db <- wide_db[c("Year","State", "TypeOfProducer", "Biomass", "Geothermal", "ConvHydro", 
  "Solar", "Wind", "Wood", "Coal",
  "OtherGases", "NatGas", "Petroleum", "Nuclear", "Other", "PSHydro",
  "Renewable", "Fossil", "Total", "Usage", "Ratio", "Level")]
  

# Let's write the Annual Generation data to a csv file....
write.csv(file="annual_generation_state_wide_updated.csv", x=wide_db)

# How many rows and columns do we have 
nrow_wide <- nrow(wide_db)
ncol_wide <- ncol(wide_db)

# Let's look at a summary
summary(wide_db)


#########################################
# ------- univariate Analysis ----------#
#########################################

# Renewable Data
renewable_data <- wide_db[, c("Biomass", "ConvHydro", "Geothermal", "Solar", "Wind", "Wood")]

# Plot some of the renewable data
stargazer(renewable_data , type="text", summary=TRUE, rownames = TRUE, 
      title="Renewable Sources", decimal.mark = ".", digits = 1,
	    digits.extra = 1, digit.separator = "")

# **Histogram of Renewable Sources** 
plot_histogram(renewable_data , title="Renewable Energy", ncol=3L)

# **BoxPlot of Renewable Energy Sources** 
# melt (pivot) renewable data
melted_renewable <- melt(renewable_data )

plot_boxplot(melted_renewable , by="variable", geom_boxplot_args  = list("outlier.color" = "red"))

# **10yr BoxPlot of Renewable Energy Sources** 
# box plot of last 10 years
target_years <- c("2010", "2011", "2012", "2013", "2014", 
                  "2015", "2016", "2017", "2018", "2019")

# Prep our data 
boxplot_data <- wide_db[, c("Year", "State", "Renewable")] %>%
	filter(Year %in% target_years) %>%
	group_by(State, Year)

# Display BoxPlot
plot_boxplot(boxplot_data, by="Year", title="Renewable Sources",
	geom_boxplot_args = list("outlier.color" = "red"))

# Fossil Data
fossil_data <- wide_db[, c("Coal","NatGas","OtherGases","Petroleum")]
stargazer(fossil_data , type="text", summary=TRUE, rownames = TRUE, 
      title="Fossil Fuel Sources", decimal.mark = ".", digits = 1,
	    digits.extra = 1, digit.separator = "")

# **Histogram of Fossil Fuel Sources** 
plot_histogram(fossil_data , title="Renewable Energy", ncol=3L)

# **BoxPlot of  Fossil Fuel Sources** 
# melt (pivot) renewable data
melted_fossil <- melt(fossil_data )

# Display Boxplot
plot_boxplot(melted_fossil, by="variable", geom_boxplot_args  = list("outlier.color" = "red"))

# **10yr BoxPlot of  Fossil Fuel Sources** 
# data prep
fossil_data <- wide_db[, c("Year", "State", "Fossil")] %>%
	filter(Year %in% target_years) %>%
	group_by(State, Year)

# box plot of last 10 years
plot_boxplot(fossil_data, by="Year", title="Fossil Fuel Sources (2010-2019)",
	geom_boxplot_args = list("outlier.color" = "red"))

### Other Energy Sources

# Other Sources Data
other_data <- wide_db[, c("Nuclear", "Other", "PSHydro")]

# Plot the data
stargazer(other_data , type="text", summary=TRUE, rownames = TRUE, 
      title="Alternate/Other Sources", decimal.mark = ".", digits = 1,
	    digits.extra = 1, digit.separator = "")

# **Histogram of Other Energy Sources** 
plot_histogram(other_data , title="Other Energy Sources", ncol=3L)

#melt (pivot) other data
melted_other <- melt(other_data)

# display boxplot of other energy sources
plot_boxplot(melted_other , by="variable", geom_boxplot_args  = list("outlier.color" = "red"))

# **10Yr Boxplot of Other Energy Sources** 
# box plot of last 10 years
target_years <- c("2010", "2011", "2012", "2013", "2014", 
                  "2015", "2016", "2017", "2018", "2019")

# prep data for other sources
other_data <- wide_db %>%
  filter(Year %in% target_years) %>%
  group_by(State, Year) %>%
  summarize(Year=Year, OtherSources = (Nuclear + Other + PSHydro)) 

# box plot of last 10 years
plot_boxplot(other_data, by="Year", title="Other Sources",
	geom_boxplot_args = list("outlier.color" = "red"))


#########################################
# ------- Bivariate Analysis -----------#
#########################################


# prep data for bivariate analysis
renewable_data <- wide_db[, c( "State", "Year", "Biomass", "ConvHydro", "Geothermal", "Solar", "Wind", "Wood")] %>%
    group_by(State)

# create vector of states
states <- unique(c(wide_db[,2]))
states <- as.vector(states)

# create state data.frame
state_db <- wide_db %>% 
  filter(State %in% states) 

# **Renewable Sources** 
# Biomass_plot
Biomass_plot <-  state_db %>% 
  ggplot( aes(x=Year, group=State, color=State)) +
  geom_line( aes( y=Biomass)) +
  theme(axis.text.y = element_blank(),
  axis.ticks.y = element_blank()) +
  xlab(NULL) + theme(legend.position = "none")

# ConvHydro_plot
ConvHydro_plot <- state_db %>% 
  ggplot( aes(x=Year, group=State, color=State)) +
  geom_line( aes( y=ConvHydro)) +
  theme(axis.text.y = element_blank(),
  axis.ticks.y = element_blank()) +
  xlab(NULL) + theme(legend.position = "none")

# Geothermal_plot
Geothermal_plot <- state_db %>% 
  ggplot( aes(x=Year, group=State, color=State)) +
  geom_line( aes( y=Geothermal)) +
  theme(axis.text.y = element_blank(),
  axis.ticks.y = element_blank()) +
  xlab(NULL) + theme(legend.position = "none")

# Solar_plot
Solar_plot <- state_db %>% 
  ggplot( aes(x=Year, group=State, color=State)) +
  geom_line( aes( y=Solar)) +
  theme(axis.text.y = element_blank(),
  axis.ticks.y = element_blank()) +
  xlab(NULL) + theme(legend.position = "none")

# Wind_plot
Wind_plot <- state_db %>% 
  ggplot( aes(x=Year, group=State, color=State)) +
  geom_line( aes( y=Wind)) +
  theme(axis.text.y = element_blank(),
  axis.ticks.y = element_blank()) +
  xlab(NULL) + theme(legend.position = "none")

# Wood_plot
Wood_plot <- state_db %>% 
  ggplot( aes(x=Year, group=State, color=State)) +
  geom_line( aes( y=Wood)) +
  theme(axis.text.y = element_blank(),
  axis.ticks.y = element_blank()) +
  xlab(NULL) + theme(legend.position = "none")

# Use multiplot to combine multiple plots in 3x2 
multiplot(Biomass_plot, ConvHydro_plot, Geothermal_plot, Solar_plot,Wind_plot,Wood_plot, cols=2)



# **Fossil Fuel** 
# Coal_plot
Coal_plot <-  state_db %>% 
  ggplot( aes(x=Year, group=State, color=State)) +
  geom_line( aes( y=Coal)) +
  theme(axis.text.y = element_blank(),
  axis.ticks.y = element_blank()) +
  xlab(NULL) + theme(legend.position = "none")

# NatGas_plot
NatGas_plot <- state_db %>% 
  ggplot( aes(x=Year, group=State, color=State)) +
  geom_line( aes( y=NatGas)) +
  theme(axis.text.y = element_blank(),
  axis.ticks.y = element_blank()) +
  xlab(NULL) + theme(legend.position = "none")

# OtherGases_plot
OtherGases_plot <- state_db %>% 
  ggplot( aes(x=Year, group=State, color=State)) +
  geom_line( aes( y=OtherGases)) +
  theme(axis.text.y = element_blank(),
  axis.ticks.y = element_blank()) +
  xlab(NULL) + theme(legend.position = "none")

# Petroleum_plot
Petroleum_plot <- state_db %>% 
  ggplot( aes(x=Year, group=State, color=State)) +
  geom_line( aes( y=Petroleum)) +
  theme(axis.text.y = element_blank(),
  axis.ticks.y = element_blank()) +
  xlab(NULL) + theme(legend.position = "none")


# Use multiplot to combine multiple plots in 3x2 
multiplot(Coal_plot, NatGas_plot, OtherGases_plot, Petroleum_plot, cols=2)
 
# **Other Energy Sources** 
# Nuclear_plot
Nuclear_plot <-  state_db %>% 
  ggplot( aes(x=Year, group=State, color=State)) +
  geom_line( aes( y=Nuclear)) +
  theme(axis.text.y = element_blank(),
  axis.ticks.y = element_blank()) +
  xlab(NULL) + theme(legend.position = "none")

# Other_plot
Other_plot <- state_db %>% 
  ggplot( aes(x=Year, group=State, color=State)) +
  geom_line( aes( y=Other)) +
  theme(axis.text.y = element_blank(),
  axis.ticks.y = element_blank()) +
  xlab(NULL) + theme(legend.position = "none")

# PSHydro_plot
PSHydro_plot <- state_db %>% 
  ggplot( aes(x=Year, group=State, color=State)) +
  geom_line( aes( y=PSHydro)) +
  theme(axis.text.y = element_blank(),
  axis.ticks.y = element_blank()) +
  xlab(NULL) + theme(legend.position = "none")


# Use multiplot to combine multiple plots in 3x2 
ggplot2::multiplot(Nuclear_plot, Other_plot, PSHydro_plot, cols=2)

# **Correlation Matrix of All Energy Sources** 
corr_db <- wide_db%>% dplyr::select(4:16)
corr <- round(cor(corr_db), 1)

# Visualize the correlation matrix         --
# method = "square" (default)
ggcorrplot(corr, lab = TRUE, outline.col = "white")


# Let's write the data to a csv file....
write.csv(file="annual_generation_state_wide_updated.csv", x=wide_db)

#########################################
# ---- Multivariate Analysis -----------#
#########################################

### Principal Component Analysis(PCA)
# run PCA on for columns 4-16 of the annual generation dataset
pca <- prcomp(wide_db[,c(4:16)], center = TRUE,scale. = TRUE)

#display results
pca

# **Summary of PCA Analysis** 
summary(pca)

# **Plot of PCA** 
autoplot(pca)

 
# **BarPlot with percentage of variances explained by each principal component.** 
fviz_eig(pca)


# **Graph of Sources. Sources with a similar profile are grouped together.** 
fviz_pca_ind(pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
             )

# **Biplot of sources and variables** 
fviz_pca_biplot(pca, repel = TRUE,
                max.overlaps = Inf,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
                )


 
## Split of Data

# set random seed to make reproducible results
set.seed(2020, sample.kind="Rounding") 

# split the data 80% training and 20% testing
split_index <- createDataPartition(y = wide_db$Level, times = 1, p = 0.2, list = FALSE)
train_db <- wide_db[-split_index,]
test_db <- wide_db[split_index,]

# little house-keeping
rm(split_index )

# check dimensions of the training and test data sets 
dim(train_db)
dim(test_db)

#########################################
# ------ Linear Regression -------------#
#########################################


# add column to both train and test LevelNum (1 or 0)
train_db$LevelNum = ifelse(train_db$Level == "High", 1,0)
test_db$LevelNum = ifelse(test_db$Level == "High", 1, 0)


# Let's look for the best model with glm
lm_model = step(glm(train_db$LevelNum ~ ., data = train_db %>%
	dplyr::select(Biomass, ConvHydro, Solar, Wind, Wood), 
	family=binomial(link='logit')),
	direction = "both")


### Model comparison with GLM using stepwise evaluation
 
### Code for Linear Regression Model
# linear Regression  - building model for train 
lm_train = lm(formula = train_db$LevelNum ~ Biomass + ConvHydro + Wood, data = train_db)


# linear Regression  - building model for test
lm_test = lm(formula = test_db$LevelNum ~ Biomass + ConvHydro + Wood, data = test_db)
 

# **Summary statistics for the Train Linear Model** 
# check summary of train model
summary(lm_train)


# **Summary statistics for the Test Linear Model** 
# check summary of test model
summary(lm_test)


# **Anova comparison of Train and Test** 
# run Anova test to compare the train/test models
anova.res <- anova(lm_train, lm_test, test="Chisq")

# graph resuls from the Anova results
stargazer(anova.res, type = "text", summary = FALSE, rownames = TRUE)

#########################################
# --------- Decision Tree  -------------#
#########################################

### R code for decision tree visualization and summary

# run the decision tree 
dt_fit <- rpart(Level ~ ConvHydro + Biomass + Wood - Year, 
	method="class", data=train_db)
 
#display the results
printcp(dt_fit)
 
# **Decision Tree cross Validation** 

#visualize cross-validation results
plotcp(dt_fit)


# **Decision Tree of the Renewable sources** 
# plot tree

rpart.plot(dt_fit)

# **Summary of the decision tree** 
# The summary lists the variables in their order of importance.  

# detailed summary of splits
summary(dt_fit)

### Confusion Matrix and Statistics for Train dataset
 
# run predict on the train decision tree model
train_pred <- predict(dt_fit, type = "class", newdata = train_db)
train_tree <- table(train_db$Level, train_pred)

# display confusion matrix 
confusionMatrix(train_tree)


### Confusion Matrix and Statistics for Test dataset

# run predict on the test decision tree model
test_pred <- predict(dt_fit, type = "class", newdata = test_db)
test_tree <- table(test_db$Level, test_pred)

# display confusion matrix 
confusionMatrix(test_tree)

### Accuracy of Decision Tree Train and Test dataset

# calculate accuracy of the test data
test_accuracy <- table(Predicted = test_pred, Actual = test_db$Level)
dtree_test_accuracy <- sum(diag(test_accuracy))/sum(test_accuracy)

# load accuracy in data.frame 
results <- data.frame(model="Test Data", Accuracy=dtree_test_accuracy)

# calculate accuracy of the test data
train_accuracy <- table(Predicted = train_pred, Actual = train_db$Level)
dtree_train_accuracy <- sum(diag(train_accuracy))/sum(train_accuracy)

# load accuracy in data.frame 
results <- results %>% add_row(model="Training Data", Accuracy=dtree_train_accuracy)

# display Accuracy results
results 


#########################################
# ----- Ratios / Top producers ---------#
#########################################


# Ratio Top Renewable energy states (desc order)
 top_ratio_renewable <- arrange(wide_db, desc(Ratio)) %>%
   dplyr::select(Year, State, Ratio)
 
 head(top_ratio_renewable) 
 

# Top states producing renewable energy (distinct states - best first)
 top_states_renewable <- top_ratio_renewable %>%
   dplyr::select(Year, State, Ratio)  %>%
   distinct(State) 
   
 head(top_states_renewable) 

