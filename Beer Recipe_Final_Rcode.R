##########################################################################################################################
# HarvardX PH125.9x Data Science Capstone 
# CYO project - Beer Recipe
#
# Le Minh Thinh
# https://github.com/thinhle1304
#
##########################################################################################################################


##########################################################################################################################
##########################################################################################################################
# Data Cleaning
##########################################################################################################################
##########################################################################################################################

## Remove and free up working space
rm(list = ls(all.names = TRUE))
gc()


## Load necessary packages for exploration and wrangling purposes
### To import, tidy, wrangle, visualize, model and communicate the data
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.r-project.org")

### To create summary statistics of variables
if(!require(skimr)) install.packages("skimr", repos = "http://cran.r-project.org")

### To create grid display for graphs
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.r-project.org")


## Load the raw dataset downloaded on Kaggle website
recipedata <- read_csv("dataset/recipeData.csv")


## Explore the raw dataset
### Check the names of all columns in the raw dataset
names(recipedata)

### Using "glimpse" function would provide general information of the dataset, especially what we expect about the types of variables and how missing values were represented in the dataset.
glimpse(recipedata)
                                                  
### Re-load the raw dataset with descriptions
recipedata <- read_csv("dataset/recipeData.csv",
                       na = c("", "NA", "N/A"),
                       col_types = cols(Style = col_factor(levels = NULL),
                                        `Size(L)` = col_number(),
                                        OG = col_number(),
                                        FG = col_number(),
                                        ABV = col_number(),
                                        IBU = col_number(),
                                        Color = col_number(),
                                        BoilSize = col_number(),
                                        BoilTime = col_number(),
                                        BoilGravity = col_number(),
                                        Efficiency = col_number(),
                                        MashThickness = col_number(),
                                        SugarScale = col_factor(levels = NULL),
                                        BrewMethod = col_factor(levels = NULL),
                                        PitchRate = col_number(),
                                        PrimaryTemp = col_number(),
                                        PrimingMethod = col_factor(levels = NULL),
                                        PrimingAmount = col_factor(levels = NULL),
                                        UserId = col_number()))

### Rename Size(L) to sizeL
recipedata <- recipedata %>% rename(sizeL = `Size(L)`)

### Summary statistics of the updated dataset
skim_with(numeric = list(hist = NULL))
skim(recipedata)

### Convert all Plato units to specific gravity (SG)
plato_to_sg <- function(x) {1 + (x / (258.6 - ((x/258.2) * 227.1)))}

recipedata_nest_sugarscale <- recipedata %>% nest(-SugarScale)

recipedata_nest_sugarscale$data[[2]] <- recipedata_nest_sugarscale$data[[2]] %>%
  mutate_at(vars(OG, FG, BoilGravity), plato_to_sg)

recipedata_sg_scale <- recipedata_nest_sugarscale %>%
  unnest(data) %>%
  mutate(SugarScale = as.factor("Specific_Gravity"))

### Drop SugarScale
recipedata_sg_scale <- recipedata_sg_scale %>% select(- SugarScale)
skim(recipedata_sg_scale)

### Handling missing values
#### Visualize missing values of variables
##### Create a function to calculate proportion of missing values
mean_missingvalue_func <- function(x) {mean(is.na(x)) * 100}

##### Show variables with NA values
mean_na <- recipedata_sg_scale %>%
  summarise_all(mean_missingvalue_func) %>%
  gather("Variable", "NA_average") %>%
  filter(NA_average > 0) # Return columns with missing values

##### Plot a barchart for the proportion of missing records in each variable
mean_na %>%
  mutate(na_vars = if_else(NA_average > 30, "gray", "pink")) %>% 
  ggplot(aes(x = reorder(Variable, -NA_average), y = NA_average, fill = I(na_vars))) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 30, color = "blue", linetype = 2) +
  theme(axis.text.x = element_text(angle = -25)) +
  labs(title = "Percent of missing records for a given variable\nRemove columns with 30% (blue line) or more NAs",
       x = "Variable", 
       y = "Missing values on average")

#### Drop those variables that had more than 30 percent of missing values
complete_var_recipedata_sg_scale <- mean_na$Variable[which(mean_na$NA_average > 30)]recipedata_sg_scale_dropNAabove30 <- recipedata_sg_scale %>%
  select(-c(which(colnames(.) %in% complete_var_recipedata_sg_scale)))

#### Missing values in dependent variable "Style"
##### Matching missing Style with Style ID
recipedata_sg_scale_dropNAabove30 %>% filter(is.na(Style)) %>% count(StyleID)
##### Drop rows with missing values in Style
recipedata_sg_scale_dropNAabove30_and_dropNAstyle <- recipedata_sg_scale_dropNAabove30 %>% 
  filter(Style != is.na(Style))
##### Double check the variable Style
skim(recipedata_sg_scale_dropNAabove30_and_dropNAstyle, Style)

#### Missing values in "BoilGravity"
##### Histogram and boxplot of Boil Gravity
hist_boigravity <- recipedata_sg_scale_dropNAabove30_and_dropNAstyle %>%
  ggplot(aes(BoilGravity)) +
  geom_histogram(binwidth = 0.004, fill = "blue") + 
  labs(title = "Histogram of BoilGravity in Specific Gravity scale", 
       x = "BoilGravity", 
       y = "Count")

boxplot_boilgravity <- recipedata_sg_scale_dropNAabove30_and_dropNAstyle %>%
  ggplot(aes(x = 1, y = BoilGravity)) +
  geom_boxplot(alpha = 0.1) +
  labs(title = "Boxplot of BoilGravity in Specific Gravity scale", 
       x = "", 
       y = "BoilGravity") +
  coord_flip()

grid.arrange(hist_boigravity,boxplot_boilgravity, nrow = 2)

##### Replace missing values by the median
recipedata_sg_scale_dropNAabove30_and_dropNAstyle$BoilGravity[which(is.na(recipedata_sg_scale_dropNAabove30_and_dropNAstyle$BoilGravity))] = median(recipedata_sg_scale_dropNAabove30_and_dropNAstyle$BoilGravity, na.rm = TRUE)
                                                  
##### Histogram and boxplot of Boil Gravity
hist_boigravity <- recipedata_sg_scale_dropNAabove30_and_dropNAstyle %>%
  ggplot(aes(BoilGravity)) +
  geom_histogram(binwidth = 0.004, fill = "blue") + 
  labs(title = "Histogram of BoilGravity in Specific Gravity scale", 
       x = "BoilGravity", 
       y = "Count")

boxplot_boilgravity <- recipedata_sg_scale_dropNAabove30_and_dropNAstyle %>%
  ggplot(aes(x = 1, y = BoilGravity)) +
  geom_boxplot(alpha = 0.1) +
  labs(title = "Boxplot of BoilGravity in Specific Gravity scale", 
       x = "", 
       y = "BoilGravity") +
  coord_flip()

grid.arrange(hist_boigravity,boxplot_boilgravity, nrow = 2)


### Create tidy dataset for the project
#### Drop variables such as "BeerID", "Name", "URL", and "StyleID" 
recipedata_tidy <- recipedata_sg_scale_dropNAabove30_and_dropNAstyle %>%
  select(-c("BeerID", "Name", "URL", "StyleID"))
#### Create and save the dataset that will be used in this project
top10_beer <- recipedata_tidy %>%
  group_by(Style) %>%
  summarise(n = n(),
            prop = round(100*(n/nrow(recipedata_tidy)), 
                         digits = 2)) %>%
  top_n(10, prop)

top10_beer %>%
  ggplot(aes(x = reorder(Style, prop), y = prop)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(title = "Top 10 Beer Recipes Submissions", 
       x = "Beer Styles", 
       y = "% of all submissions") +
  coord_flip()                        

#### Create "recipe_top10" dataset and save it
##### Create "recipe_top10" dataset
recipe_top10 <- recipedata_tidy %>%
  filter(Style %in% top10_beer$Style) %>%
  droplevels() # Important step to for createDataPartition to work 
##### Save "recipe_top10" dataset
save(recipe_top10, file = "recipe_top10.RData")


##########################################################################################################################
##########################################################################################################################
# Exploratory data analysis
##########################################################################################################################
##########################################################################################################################

## Glimpse at the dataset
glimpse(recipe_top10)

##  Summary statistics of the dataset
skim(recipe_top10)

## Make boxplots of every numerical variables in the recipe_top10 again Style
### Make boxplots of every numerical variables in the recipe_top10
box_plot_numeric_vars <- function(df, cols, col_x = "Style"){
  options(repr.plot.width = 4, repr.plot.height = 3.5) # Set the initial plot area dimensions
  for(col in cols){
    p <- ggplot(df, aes_string(col_x, col)) + 
      geom_boxplot(alpha = 0.1) +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
            axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0)) +
      labs(title = paste('Box plot of', col, '\n vs.', col_x), 
           x = "", 
           y = col)
    print(p)
  }
  }

num_cols <- c("Color", "sizeL", "BoilSize", "OG",
              "FG", "ABV", "IBU", "BoilTime",
              "BoilGravity", "Efficiency")

box_plot_numeric_vars(recipe_top10, num_cols)   

## Transform all numerical variables in the dataset with log10 to duel with extreme values
### log10 function with addional small value to handle zero
func_log10 <- function(x) (log10(x+0.0001))
### New dataset with log10 transformation for all numerical variables
recipe_top10_log10 <- recipe_top10 %>%
  mutate_if(is.numeric, func_log10)

## Boxplot of transformed dataset  
### Make boxplots of every numerical variables in the recipe_top10_log10
box_plot_numeric_vars <- function(df, cols, col_x = "Style"){
  options(repr.plot.width = 4, repr.plot.height = 3.5) # Set the initial plot area dimensions
  for(col in cols){
    p <- ggplot(df, aes_string(col_x, col)) + 
      geom_boxplot(alpha = 0.1) +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
            axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0)) +
      labs(title = paste('Box plot of', col, '\n vs.', col_x), 
           x = "", 
           y = col)
    print(p)
  }
  }

num_cols <- c("Color", "sizeL", "BoilSize", "OG", 
              "FG", "ABV", "IBU", "BoilTime", 
              "BoilGravity", "Efficiency")

box_plot_numeric_vars(recipe_top10_log10, num_cols)

## Violin plot of transformed dataset
violin_plot_numeric_vars <- function(df, cols, col_x = "Style"){
  options(repr.plot.width=4, repr.plot.height=3.5) # Set the initial plot area dimensions
  for(col in cols){
    p <- ggplot(df, aes_string(col_x, col)) + 
      geom_violin() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
            axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0)) +
      labs(title = paste('Box plot of', col, '\n vs.', col_x), 
           x = "", 
           y = col)
    print(p)
  }
  }

num_cols <- c("Color", "sizeL", "BoilSize", "OG",
              "FG", "ABV", "IBU", "BoilTime", 
              "BoilGravity", "Efficiency")

violin_plot_numeric_vars(recipe_top10_log10, num_cols)

## Histogram of the transformed data
hist_plot_numeric_vars <- function(df, cols, col_x = "Style"){
  options(repr.plot.width=4, repr.plot.height=3.5) # Set the initial plot area dimensions
  for(col in cols){
    p <- ggplot(df, aes_string(col)) +
      geom_histogram(aes(y=..density..), 
                     alpha = 0.5, 
                     bins = 200) +
      geom_density(aes(y=..density..), 
                   color = 'blue') + 
      geom_rug() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
            axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0)) +
      labs(title = paste('Histogram of', col), 
           x = col, 
           y = "Frequency")
    print(p)
  }
  }

num_cols <- c("Color", "sizeL", "BoilSize", "OG", 
              "FG", "ABV", "IBU", "BoilTime", 
              "BoilGravity", "Efficiency")

hist_plot_numeric_vars(recipe_top10_log10, num_cols)


## Scatter plot of transformed dataset
scatter_plot_numeric_vars <- function(df, cols, col_x = "Color"){
  options(repr.plot.width=4, repr.plot.height=3.5) # Set the initial plot area dimensions
  for(col in cols){
    p <- ggplot(df, aes_string(col_x, col)) +
      geom_jitter(aes(col = Style), alpha = 0.1, size = 3.5) +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
            axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0)) +
      labs(title = paste('Scatter plot of', col, '\n and', col_x), 
           x = "Color", 
           y = col)
    print(p)
  }
  }

num_cols <- c("sizeL", "BoilSize", "OG", 
              "FG", "ABV", "IBU", "BoilTime", 
              "BoilGravity", "Efficiency")

scatter_plot_numeric_vars(recipe_top10_log10, num_cols)


## Proportion of different brewing methods on Style
recipe_top10_log10 %>% 
  ggplot(aes(x = Style, fill = BrewMethod)) + 
  geom_bar(position = "fill") +
  ggtitle("Histogram of Style vs BrewMethod") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

## The proportions of Beer styles
### Proportions of Beer Styles
recipe_top10_log10 %>% group_by(Style) %>% 
  summarize(n_style = n(), prop_style = n_style/nrow(recipe_top10_log10)) %>%
  arrange(desc(prop_style))
### Visualize proportions of beer styles
recipe_top10_log10 %>%
  select(Style) %>%
  gather("cols", "sets") %>%
  ggplot(aes(x = cols, fill = sets)) +
  geom_bar(position = "fill") +
  labs(title = "Proportions of categorical variables", 
       x = "", 
       y = "Proportion") +
  guides(fill = guide_legend(title=""))


## correlation matrices of all numerical variables
### visualize correlation matrices
if(!require(corrplot))install.packages("corrplot", repos = "http://cran.us.r-project.org")
### Explore numeric features
numeric_vars <- recipe_top10_log10 %>%
  select(which(sapply(., class) != "factor"))
corrplot(cor(numeric_vars, use = "complete.obs"), method = "number")

## Drop "sizeL"
recipe_top10_log10_tidy <- recipe_top10_log10 %>% select(- sizeL)
## Save "recipe_top10_log10_tidy" dataset
save(recipe_top10_log10_tidy, file = "recipe_top10_log10_tidy.RData")



##########################################################################################################################
##########################################################################################################################
# Modelling approaches
##########################################################################################################################
##########################################################################################################################


## Glance at the tidy dataset
glimpse(recipe_top10_log10_tidy)

## Create training data for modelling approaches and testing data for validation step
### Load caret package
if(!require(caret))install.packages("caret", repos = "http://cran.us.r-project.org")
### Create train_data and test_data
set.seed(2019)
test_index <- createDataPartition(recipe_top10_log10_tidy$Style, time = 1, p = 0.5, list = FALSE)
train_data <- recipe_top10_log10_tidy %>% slice(test_index)
test_data <- recipe_top10_log10_tidy %>% slice(-test_index)
### Check the dimension of train_data and test_data
dim(train_data)
dim(test_data)

## Scaling the numeric variables
num_cols <- c("Color", "BoilSize", "OG", 
              "FG", "ABV", "IBU", 
              "BoilTime", "BoilGravity", "Efficiency")

preprocess_values <- preProcess(train_data[,num_cols], method = c("center", "scale"))
train_data[,num_cols] <- predict(preprocess_values, train_data[,num_cols])
test_data[,num_cols] <- predict(preprocess_values, test_data[,num_cols])

### Save the training data and testing data for the modelling step
save(train_data, file = "train_data_beerrecipe.RData")
save(test_data, file = "test_data_beerrecipe.RData")

### Glance at the scaled train_data
head(train_data[,num_cols])

##########################################################################################################################
## Naive Bayes approach
##########################################################################################################################

### Naive Bayes approach using naive_bayes method in caret package with imbalanced dataset

# Remove and free up working space
rm(list = ls(all.names = TRUE))
gc()


# Load necessary packages
if(!require(tidyverse))install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tictoc))install.packages("tictoc", repos = "http://cran.us.r-project.org")
if(!require(caret))install.packages("caret", repos = "http://cran.us.r-project.org")

# Load the datasets
tic()
load("train_data_beerrecipe.RData")


# Set up cross validation method for the Naive Bayes model 
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,  ## 5-fold CV
                           repeats = 3, ## repeated three times
                           p = 0.8)

# Run the Naive Bayes model
set.seed(201955)

model_nb <- train(Style ~ .,
                  data = train_data,
                  method = "naive_bayes",
                  trControl = fitControl,
                  verbose= FALSE)


# Result of Naive Bayes approach
max_accuracy_model_nb <- model_nb$results %>% filter(Accuracy == max(Accuracy))
max_accuracy_model_nb

# Save the maximum of accuracy to conduct Table of results
nb_accuracy <- max(model_nb$results[4])
nb_accuracy
save(nb_accuracy, file = "nb_accuracy.RData")

# Memory usage of running Naive Bayes model
memory.size()

# Time spending in running Naive Bayes model
runtime <- toc()
nb_runtime <- runtime$toc - runtime$tic
save(nb_runtime, file = "nb_runtime.RData")


##########################################################################################################################
### Naive Bayes approach using naive_bayes method and "up" sampling in caret package

# Remove and free up working space
rm(list = ls(all.names = TRUE))
gc()


# Load necessary packages
if(!require(tidyverse))install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tictoc))install.packages("tictoc", repos = "http://cran.us.r-project.org")
if(!require(caret))install.packages("caret", repos = "http://cran.us.r-project.org")

# Load the datasets
tic()
load("train_data_beerrecipe.RData")


# Set up cross validation method for the Naive Bayes model 
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,  ## 5-fold CV
                           repeats = 3, ## repeated three times
                           p = 0.8,
                           sampling = "up" ## randomly subset all the classes to match the
                           ## frequencies of the higest prevalent class
)

# Run the Naive Bayes model with "up" sampling method 
set.seed(201955)

model_nb_upsampling <- train(Style ~ .,
                             data = train_data,
                             method = "naive_bayes",
                             trControl = fitControl,
                             verbose= FALSE)

# Result of Naive Bayes approach with "up" sampling method 
max_accuracy_model_nb_upsampling <- model_nb_upsampling$results %>% 
  filter(Accuracy == max(Accuracy))

max_accuracy_model_nb_upsampling

# Save the maximum of accuracy to conduct Table of results
nb_upsampling_accuracy <- max(model_nb_upsampling$results[4])
nb_upsampling_accuracy
save(nb_upsampling_accuracy, file = "nb_upsampling_accuracy.RData")

# Memory usage of running Naive Bayes model with "up" sampling method 
memory.size()

# Time spending in running Naive Bayes model with "up" sampling method 
runtime <- toc()
nb_upsampling_runtime <- runtime$toc - runtime$tic
save(nb_upsampling_runtime, file = "nb_upsampling_runtime.RData")



##########################################################################################################################
## Decision tree approach
##########################################################################################################################

### Decision tree approach using rpart method in caret package with imbalanced dataset

# Remove and free up working space
rm(list = ls(all.names = TRUE))
gc()

# Load necessary packages
if(!require(tidyverse))install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tictoc))install.packages("tictoc", repos = "http://cran.us.r-project.org")
if(!require(caret))install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rpart))install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot))install.packages("rpart.plot", repos = "http://cran.us.r-project.org")

# Load the datasets
tic()
load("train_data_beerrecipe.RData")


# Set up cross validation method for the Decision tree model 
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,  ## 5-fold CV
                           repeats = 3, ## repeated three times
                           p = 0.8,
                           returnResamp="all")

# Run the Decision tree model
set.seed(201955)

model_rpart <- train(Style ~ .,
                     data = train_data,
                     method = "rpart",
                     trControl = fitControl)

# Plot the decision tree
rpart.plot.version1(model_rpart$finalModel) 
title("Decision Tree")

# Result of Decision tree approach
max_accuracy_model_rpart <- model_rpart$results %>% filter(Accuracy == max(Accuracy))
max_accuracy_model_rpart

# Save the maximum of accuracy to conduct Table of results
rpart_accuracy <- max(model_rpart$results[2])
save(rpart_accuracy, file = "rpart_accuracy.RData")
rpart_accuracy

# Memory usage of running Decision tree model
memory.size()

# Time spending in running Decision tree model
runtime <- toc()
rpart_runtime <- runtime$toc - runtime$tic
save(rpart_runtime, file = "rpart_runtime.RData")


##########################################################################################################################
### Decision tree approach using rpart method in caret package with balanced dataset

# Remove and free up working space
rm(list = ls(all.names = TRUE))
gc()

# Load necessary packages
if(!require(tidyverse))install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tictoc))install.packages("tictoc", repos = "http://cran.us.r-project.org")
if(!require(caret))install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rpart))install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot))install.packages("rpart.plot", repos = "http://cran.us.r-project.org")

# Load the datasets
tic()
load("train_data_beerrecipe.RData")

# Set up cross validation method for the Decision tree model 
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,  ## 5-fold CV
                           repeats = 3, ## repeated three times
                           p = 0.8,
                           sampling = "up", ## randomly subset all the classes to match the 
                           ## frequencies of the higest prevalent class
                           returnResamp="all")

# Run the Decision tree model  with "up" sampling method 
set.seed(201955)

model_rpart_upsampling <- train(Style ~ .,
                                data = train_data,
                                method = "rpart",
                                trControl = fitControl)

# Plot the decision tree
rpart.plot.version1(model_rpart_upsampling$finalModel)
title("Decision Tree with balanced dataset")

# Result of Decision tree approach with "up" sampling method 
max_accuracy_model_rpart_upsampling <- model_rpart_upsampling$results %>% 
  filter(Accuracy == max(Accuracy))

max_accuracy_model_rpart_upsampling

# Save the maximun of accuracy to conduct Table of results
rpart_upsampling_accuracy <- max(model_rpart_upsampling$results[2])
save(rpart_upsampling_accuracy, file = "rpart_upsampling_accuracy.RData")
rpart_upsampling_accuracy

# Memory usage of running Decision tree model with "up" sampling method 
memory.size()

# Time spending in running Decision tree model with "up" sampling method 
runtime <- toc()
rpart_upsampling_runtime <- runtime$toc - runtime$tic
save(rpart_upsampling_runtime, file = "rpart_upsampling_runtime.RData")


##########################################################################################################################
## Ensemble method for decision trees
##########################################################################################################################

### Bagging technique 
#### Random forest using "rf" method in caret package

# Remove and free up working space
rm(list = ls(all.names = TRUE))
gc()

# Load necessary packages
if(!require(tidyverse))install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tictoc))install.packages("tictoc", repos = "http://cran.us.r-project.org")
if(!require(caret))install.packages("caret", repos = "http://cran.us.r-project.org")

# Load the datasets
tic()
load("train_data_beerrecipe.RData")

# Set up cross validation method for the random forest model 
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,  ## 5-fold CV
                           repeats = 3, ## repeated three times
                           p = 0.8,
                           returnResamp="all"
)

# Run the random forest model
set.seed(201955)
model_rf <- train(Style ~ .,
                  data = train_data,
                  method = "rf",
                  trControl = fitControl,
                  verbose= FALSE)

# Show variable important score in the random forest model
var_imp_rf <- varImp(model_rf)

# plot the random forest model
ggplot(var_imp_rf) + 
  ggtitle("Important Variables in Random Forest model")

# Result of random forest approach
max_accuracy_model_rf <- model_rf$results %>% filter(Accuracy == max(Accuracy))
max_accuracy_model_rf

# Save the maximum of accuracy to conduct Table of results
rf_accuracy <- max(model_rf$results[2])
save(rf_accuracy, file = "rf_accuracy.RData")
rf_accuracy

# Memory usage of running random forest model
memory.size()

# Time spending in running random forest model
runtime <- toc()
rf_runtime <- runtime$toc - runtime$tic
save(rf_runtime, file = "rf_runtime.RData")


##########################################################################################################################
#### Random forest using "rf" method and "up" sampling in caret package
# Remove and free up working space
rm(list = ls(all.names = TRUE))
gc()

# Load necessary packages
if(!require(tidyverse))install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tictoc))install.packages("tictoc", repos = "http://cran.us.r-project.org")
if(!require(caret))install.packages("caret", repos = "http://cran.us.r-project.org")

# Load the datasets
tic()
load("train_data_beerrecipe.RData")

# Set up cross validation method for the Random forest model 
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,  ## 5-fold CV
                           repeats = 3, ## repeated three times
                           p = 0.8,
                           returnResamp = "all",
                           sampling = "up" ## randomly subset all the classes to match the 
                           ## frequencies of the higest prevalent class
)

# Run the Random forest model  with "up" sampling method 
set.seed(201955)
model_rf_upsampling <- train(Style ~ .,
                             data = train_data,
                             method = "rf",
                             trControl = fitControl,
                             verbose= FALSE)


# Show variable important score in the random forest model
var_imp_rf_upsampling <- varImp(model_rf_upsampling)

# plot the Random forest model
ggplot(var_imp_rf_upsampling) + 
  ggtitle("Important Variables in Random Forest model /n balanced dataset")

# Result of Random forest approach with "up" sampling method 
max_accuracy_model_rf_upsampling <- model_rf_upsampling$results %>% 
  filter(Accuracy == max(Accuracy))

max_accuracy_model_rf_upsampling

# Save the maximum of accuracy to conduct Table of results
rf_upsampling_accuracy <- max(model_rf_upsampling$results[2])
save(rf_upsampling_accuracy, file = "rf_upsampling_accuracy.RData")
rf_upsampling_accuracy

# Memory usage of running Random forest model with "up" sampling method 
memory.size()

# Time spending in running Random forest model with "up" sampling method 
runtime <- toc()
rf_upsampling_runtime <- runtime$toc - runtime$tic
save(rf_upsampling_runtime, file = "rf_upsampling_runtime.RData")


##########################################################################################################################
### Boosting technique
##########################################################################################################################

#### EXtreme Gradient Boosting model using "xgbTree" method in caret package

# Remove and free up working space
rm(list = ls(all.names = TRUE))
gc()

# Load necessary packages
if(!require(tidyverse))install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tictoc))install.packages("tictoc", repos = "http://cran.us.r-project.org")
if(!require(caret))install.packages("caret", repos = "http://cran.us.r-project.org")

# Load the datasets
tic()
load("train_data_beerrecipe.RData")

# Set up cross validation method for the EXtreme Gradient Boosting model 
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,  ## 5-fold CV
                           repeats = 3, ## repeated three times
                           p = 0.8,
                           returnResamp="all"
)

# Run the EXtreme Gradient Boosting model
set.seed(201955)
model_xgbTree <- train(Style ~ .,
                       data = train_data,
                       method = "xgbTree",
                       trControl = fitControl,
                       verbose= FALSE)


# Show variable important score in the EXtreme Gradient Boosting model
var_imp_xgbTree <- varImp(model_xgbTree)

# plot the EXtreme Gradient Boosting model
ggplot(var_imp_xgbTree) + 
  ggtitle("Important Variables in EXtreme Gradient Boosting Model")

# Result of EXtreme Gradient Boosting approach
max_accuracy_model_xgbTree <- model_xgbTree$results %>% filter(Accuracy == max(Accuracy))
max_accuracy_model_xgbTree

# Save the maximum of accuracy to conduct Table of results
xgbTree_accuracy <- max(model_xgbTree$results[8])
save(xgbTree_accuracy, file = "xgbTree_accuracy.RData")
xgbTree_accuracy

# Memory usage of running EXtreme Gradient Boosting model
memory.size()

# Time spending in running EXtreme Gradient Boosting model
runtime <- toc()
xgbTree_runtime <- runtime$toc - runtime$tic
save(xgbTree_runtime, file = "xgbTree_runtime.RData")


##########################################################################################################################
#### EXtreme Gradient Boosting model using "xgbTree" method and "up" sampling in caret package

# Remove and free up working space
rm(list = ls(all.names = TRUE))
gc()

# Load necessary packages
if(!require(tidyverse))install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tictoc))install.packages("tictoc", repos = "http://cran.us.r-project.org")
if(!require(caret))install.packages("caret", repos = "http://cran.us.r-project.org")

# Load the datasets
tic()
load("train_data_beerrecipe.RData")

# Set up cross validation method for the EXtreme Gradient Boosting model 
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,  ## 5-fold CV
                           repeats = 3, ## repeated three times
                           p = 0.8,
                           returnResamp="all",
                           sampling = "up" ## randomly subset all the classes to match the 
                           ## frequencies of the higest prevalent class
)

# Run the EXtreme Gradient Boosting model  with "up" sampling method 
set.seed(201955)
model_xgbTree_upsampling <- train(Style ~ .,
                                  data = train_data,
                                  method = "xgbTree",
                                  trControl = fitControl,
                                  verbose= FALSE)

# Show variable important score in the EXtreme Gradient Boosting model
var_imp_xgbTree_upsampling <- varImp(model_xgbTree_upsampling)

# plot the EXtreme Gradient Boosting model
ggplot(var_imp_xgbTree_upsampling) + 
  ggtitle("Important Variables in EXtreme Gradient Boosting Model balanced dataset")

# Result of EXtreme Gradient Boosting approach with "up" sampling method 
max_accuracy_model_xgbTree_upsampling <- model_xgbTree_upsampling$results %>% 
  filter(Accuracy == max(Accuracy))

max_accuracy_model_xgbTree_upsampling

# Save the maximun of accuracy to conduct Table of results
xgbTree_upsampling_accuracy <- max(model_xgbTree_upsampling$results[8])
save(xgbTree_upsampling_accuracy, file = "xgbTree_upsampling_accuracy.RData")
xgbTree_upsampling_accuracy

# Memory usage of running EXtreme Gradient Boosting model with "up" sampling method 
memory.size()

# Time spending in running EXtreme Gradient Boosting model with "up" sampling method 
runtime <- toc()
xgbTree_upsampling_runtime <- runtime$toc - runtime$tic
save(xgbTree_upsampling_runtime, file = "xgbTree_upsampling_runtime.RData")



##########################################################################################################################
##########################################################################################################################
# Choosing a model for validation
##########################################################################################################################
##########################################################################################################################

load("nb_accuracy.RData")
load("nb_runtime.RData")
load("nb_upsampling_accuracy.RData")
load("nb_upsampling_runtime.RData")
load("rpart_accuracy.RData")
load("rpart_runtime.RData")
load("rpart_upsampling_accuracy.RData")
load("rpart_upsampling_runtime.RData")
load("rf_upsampling_runtime.RData")
load("rf_accuracy.RData")
load("rf_runtime.RData")
load("rf_upsampling_accuracy.RData")
load("rf_upsampling_runtime.RData")
load("xgbTree_accuracy.RData")
load("xgbTree_runtime.RData")
load("xgbTree_upsampling_accuracy.RData")
load("xgbTree_upsampling_runtime.RData")


# Table of Accuracy of different machine learning approaches
accuracy_results <- data_frame(Model = "Naive Bayes", 
                               Accuracy = nb_accuracy, 
                               Runtime = nb_runtime)

accuracy_results <- bind_rows(accuracy_results,
                              data_frame(Model = "Naive Bayes Balanced dataset",  
                                         Accuracy = nb_upsampling_accuracy, 
                                         Runtime = nb_upsampling_runtime))

accuracy_results <- bind_rows(accuracy_results,
                              data_frame(Model = "Decision Tree",  
                                         Accuracy = rpart_accuracy, 
                                         Runtime = rpart_runtime))

accuracy_results <- bind_rows(accuracy_results,
                              data_frame(Model = "Decision Tree Balanced dataset",  
                                         Accuracy = rpart_upsampling_accuracy, 
                                         Runtime = rpart_upsampling_runtime))

accuracy_results <- bind_rows(accuracy_results,
                              data_frame(Model = "Random Forest",  
                                         Accuracy = rf_accuracy, 
                                         Runtime = rf_runtime))

accuracy_results <- bind_rows(accuracy_results,
                              data_frame(Model = "Random Forest Balanced dataset",  
                                         Accuracy = rf_upsampling_accuracy, 
                                         Runtime = rf_upsampling_runtime))

accuracy_results <- bind_rows(accuracy_results,
                              data_frame(Model = "EXtreme Gradient Boosting model",  
                                         Accuracy = xgbTree_accuracy, 
                                         Runtime = xgbTree_runtime))

accuracy_results <- bind_rows(accuracy_results,
                              data_frame(Model = "EXtreme Gradient Boosting model Balanced dataset",  
                                         Accuracy = xgbTree_upsampling_accuracy, 
                                         Runtime = xgbTree_upsampling_runtime))
accuracy_results %>% knitr::kable()


##########################################################################################################################
##########################################################################################################################
# Validate the highest accuracy predicting model of Beer Styles
##########################################################################################################################
##########################################################################################################################


## Remove and free up working space
rm(list = ls(all.names = TRUE))
gc()

## Load the test_data
load("train_data_beerrecipe.RData")
load("test_data_beerrecipe.RData")

## Load necessary packages
if(!require(tidyverse))install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tictoc))install.packages("tictoc", repos = "http://cran.us.r-project.org")
if(!require(caret))install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(Metrics))install.packages("Metrics", repos = "http://cran.us.r-project.org")

## Set up cross validation method for the EXtreme Gradient Boosting model 
tic()
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,  ## 5-fold CV
                           repeats = 3, ## repeated three times
                           p = 0.8,
                           returnResamp="all")

## Set up xgbTree parameters
xgbTree_Grid <- expand.grid(subsample = 1,
                            nrounds = 100,
                            min_child_weight = 1,
                            colsample_bytree = 0.6,
                            gamma = 0,
                            max_depth = 2,
                            eta = 0.3)

## Run the EXtreme Gradient Boosting model
set.seed(201955)
final_model_xgbTree <- train(Style ~ .,
                             data = train_data,
                             method = "xgbTree",
                             trControl = fitControl,
                             tuneGrid = xgbTree_Grid, ## Now specify the exact models 
                             ## to evaluate
                             verbose= FALSE)

## Show variable important score in the EXtreme Gradient Boostingt model
var_imp_final_model_xgbTree <- varImp(final_model_xgbTree)

## plot the EXtreme Gradient Boosting model
ggplot(var_imp_final_model_xgbTree) + 
  ggtitle("Important Variables in EXtreme Gradient Boosting model")

## Result of EXtreme Gradient Boosting approach in the test_data
actual_style <- test_data$Style
predict_style <- predict(final_model_xgbTree, test_data)
final_model_xgbTree_accuracy <- accuracy(actual_style,predict_style)
final_model_xgbTree_accuracy

## Save the maximum of accuracy to conduct Table of results
save(final_model_xgbTree_accuracy, file = "final_model_xgbTree_accuracy.RData")

## Memory usage of running EXtreme Gradient Boosting model
memory.size()

## Time spending in running EXtreme Gradient Boosting model
runtime <- toc()
final_model_xgbTree_runtime <- runtime$toc - runtime$tic
save(final_model_xgbTree_runtime, file = "final_model_xgbTree_runtime.RData")



#################################################################################################################
#################################################################################################################
# Report Accuracy of the best model in the validation step
#################################################################################################################
#################################################################################################################

# Final result
Final_result <- data_frame(Model = "EXtreme Gradient Boosting", 
                           Accuracy = final_model_xgbTree_accuracy,
                           Runtime_second = final_model_xgbTree_runtime)

Final_result %>% knitr::kable()
