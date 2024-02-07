# ----- B1705 Week 5 | MANOVA and MANCOVA | 07.02.2024 -----

# ----- LECTURE TASKS -----
# ----- 1. Path Analysis: Demonstration -----
##### 1.1. Loading and Generating Data #####
rm(list=ls())
# Load necessary library
library(tidyverse)

# Set seed for reproducibility
set.seed(123)

# Generate synthetic dataset
n <- 200 # Number of observations

# Independent Variables
PossessionTime <- round(rnorm(n, mean=30, sd=5),2)
Tackles <- round(rpois(n, lambda=20),0)
SuccessfulPasses <- round(rpois(n, lambda=150),0)
TerritorialGain <- round(rnorm(n, mean=1000, sd=200),2)
PenaltiesConceded <- round(rpois(n, lambda=10),0)

# Generating PerformanceScore with associations

PerformanceScore <- round(50 + (PossessionTime * 0.5) + (SuccessfulPasses * 0.05) - (PenaltiesConceded * 1) + rnorm(n, mean=0, sd=5),2)

df <- tibble(PossessionTime, Tackles, SuccessfulPasses, TerritorialGain, PenaltiesConceded, PerformanceScore)

# Print out the first few rows to check
head(df)
rm(n, PenaltiesConceded, PerformanceScore, PossessionTime, SuccessfulPasses, Tackles, TerritorialGain)

#The dataset contains data for 200 rugby games, and has been reduced to the following variables:
  
# PossessionTime - Time in possession of the ball (in minutes).
# Tackles - Number of tackles made.
# SuccessfulPasses - Number of successful passes.
# TerritorialGain - Total territorial gain (in meters).
# PenaltiesConceded - Number of penalties conceded.
# PerformanceScore - Game evaluation by three independent coaches.

# Provide a codebook for explanations of variables if/when necesary


##### 1.2. Exploratory Data Analysis (EDA) #####

# Load libraries
library(dplyr)
library(ggplot2)
library(GGally)

# Basic summary stats
summary_stats <- df %>% summary()
print(summary_stats)

library(corrplot)
cor_matrix <- cor(df) # create the correlation matrix
cov_matrix <- cov(df) # create the covariance matrix

# Visualise  correlation matrix
corrplot(cor_matrix, method = "number")


# Assuming df is your dataframe and PerformanceScore is the continuous variable
# Calculate the median of PerformanceScore for the threshold
median_score <- median(df$PerformanceScore)

# Create a new variable based on PerformanceScore being high or low
df$PerformanceCategory <- ifelse(df$PerformanceScore > median_score, 'high', 'low')

# Check the first few rows to verify the new variable
head(df)

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Creating a list to store plots
plots_list <- list()

# Iterate over each independent variable to create plots
for (var in names(df)[-6]) { # Excluding the PerformanceCategory variable
  # Boxplot
  boxplot <- ggplot(df, aes_string(x="factor(PerformanceCategory)", y=var, fill="factor(PerformanceCategory)")) +
    geom_boxplot() +
    labs(title=paste("Boxplot of", var, "by PerformanceCategory"),
         x="Performance Category",
         y=var,
         fill="Performance Category") +
    theme_minimal()
  
  # Density plot
  density_plot <- ggplot(df, aes_string(x=var, fill="factor(PerformanceCategory)")) +
    geom_density(alpha=0.7) +
    labs(title=paste("Density Plot of", var, "by PerformanceCategory"),
         x=var,
         y="Density",
         fill="Performance Category") +
    theme_minimal()
  
  # Store plots in the list
  plots_list[[paste("Boxplot", var)]] <- boxplot
  plots_list[[paste("Density", var)]] <- density_plot
}

# Print the plots
for (plot_name in names(plots_list)) {
  print(plots_list[[plot_name]])
}

##### 1.3. Conducting Path Analysis #####

library(lavaan)

# Define our path analysis model
model <- '
  PerformanceScore ~ PossessionTime + Tackles + SuccessfulPasses + TerritorialGain + PenaltiesConceded
'
# Fit model
fit <- sem(model, data=df, estimator="MLR") # Using Maximum Likelihood estimation for robust estimation

# Summarise results
summary(fit, standardized = TRUE, fit.measures = TRUE)


##### 1.4. Interpreting Results #####

# CFI and TLI above 0.9 or 0.95 indicates a good fit

# RMSEA below 0.05 indicate a close fit, and values up to 0.08 represent a reasonable error of approximation

# Standardized Root Mean Square Residual (SRMR): Values less than 0.08 are generally considered good

# The output also provides two fit indices, which help in assessing the model fit from
# different perspectives. They are not particularly useful by themselves, but they help you to compare different models.

# They lie outside the scope of this practical, but a general rule of thumb is that 
# ‘lower is better’. In other words, a lower AIC for Model 2 compared with Model 1 
# suggests that Model 2 is ‘better’ at explaining the data, compared with Model 1

##### 1.5. Visualising the Model #####

# Load necessary libraries
library(semPlot)

# Visualize the path diagram
semPaths(fit, whatLabels="est", edge.label.cex = 0.75, layout="tree")


# ----- 2. Path Analysis: Practical -----
##### 2.1 Loading and Getting Data #####
rm(list=ls())
swimming_data <- read.csv('https://www.dropbox.com/scl/fi/kmzsi84wltvr9hycwnr3z/31_02.csv?rlkey=7bml7ukawcygwotuy6aemy4fr&dl=1')

swimming_data$X <- NULL

head(swimming_data) # display the first six rows

# Load lavaan package
library(lavaan)

# Define the model (specifying the paths)
model <- '
  # Direct paths
  average_speed ~ experience_years + age + stamina
  # Mediation paths
  stamina ~ training_hours + diet_quality
'

# Fit model
fit <- sem(model, data = swimming_data)

# Summarise results
summary(fit, standardized = TRUE, fit.measures = TRUE)

# Create the path diagram
semPaths(fit, whatLabels = "est", layout = "tree", edge.label.cex = 0.8)

##### 2.2. Repeat the basic model for swimming_data #####





# ----- 3. SEM Demonstration -----
##### 3.1. Load Data #####

rm(list=ls())

set.seed(123) # Ensure reproducibility

# Number of observations
n <- 500

# Simulating independent variables
player_age <- round(rnorm(n, 25, 4),0) # Player age in years
years_in_NFL <- round(rnorm(n, 3, 2),0) # Years in NFL
games_played <- round(rpois(n, 16),0) # Games played in a season
total_yards <- round(rnorm(n, 500, 150),0) # Total yards
touchdowns <- round(rpois(n, 5),0) # Touchdowns
interceptions <- round(rpois(n, 2),0) # Interceptions
tackles <- rpois(n, 40) # Tackles made
sacks <- rpois(n, 5) # Sacks

# Simulating latent variables "Performance" and "Experience"
# For simulation purposes, we will create proxies for these latent constructs.
performance_proxy <- total_yards + touchdowns*10 - interceptions*5 + tackles + sacks*2
experience_proxy <- player_age + years_in_NFL*2

# Normalise proxies to have them on a similar scale
performance_proxy <- scale(performance_proxy)
experience_proxy <- scale(experience_proxy)

# Simulating "Player Value" influenced by "Performance" and "Experience"
player_value <- round(5*performance_proxy + 3*experience_proxy + rnorm(n, 0, 1),2) # Add noise

# Create dataframe
data <- data.frame(player_age, years_in_NFL, games_played, total_yards, touchdowns, interceptions, tackles, sacks, player_value)

# View the first few rows of the dataset
head(data)

##### 3.2. EDA #####
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lavaan)

# Exploratory Data Analysis
library(corrplot)
cor_matrix <- cor(data) # create the correlation matrix
cov_matrix <- cov(data) # create the covariance matrix

# Visualise  correlation matrix
corrplot(cor_matrix, method = "number")

rm(cor_matrix, cov_matrix)

##### 3.3. Specify the Structural Equation Model (SEM) #####

library(lavaan)
# Define our path analysis model
model <- '
  player_value ~ total_yards + touchdowns + interceptions + tackles + sacks + player_age + years_in_NFL
'
# Fit model
fit <- sem(model, data=data, estimator="MLR") # Using Maximum Likelihood estimation for robust estimation

# Summarise results
summary(fit, standardized = TRUE, fit.measures = TRUE)

# Create the path diagram
semPaths(fit, whatLabels = "est", layout = "tree", edge.label.cex = 0.8)

rm(model,fit)

library(lavaan)

# Define the SEM model
model <- '
  # Latent variable definitions
  Performance =~ total_yards + touchdowns
  Experience =~ player_age + years_in_NFL
  
  # Outcome variable
  PlayerValue =~ player_value
  
  # Relationships
  PlayerValue ~ Performance + Experience
'


##### 3.4. Fitting the Model #####

# Scale the variables
data$player_age <- scale(data$player_age)
data$years_in_NFL <- scale(data$years_in_NFL)
data$games_played <- scale(data$games_played)
data$total_yards <- scale(data$total_yards)
data$touchdowns <- scale(data$touchdowns)
data$interceptions <- scale(data$interceptions)
data$tackles <- scale(data$tackles)
data$sacks <- scale(data$sacks)
data$player_value <- scale(data$player_value)

# Fit the model
fit <- sem(model, data = data, estimator = "MLR", control = list(maxit = 15000)) # Increase max iterations to 10000

# Model summary
summary(fit, fit.measures = TRUE)

##### 3.5. Inspect the Model #####

# Model Estimation Summary Estimates: This section shows the estimated parameters of the model. In SEM, these include factor loadings 
# (relationships between observed variables and latent variables), regression weights (relationships among latent variables), and variances and covariances of the variables.

# Factor Loadings: Indicate how strongly each observed variable represents the latent variable. Higher absolute values indicate a stronger relationship.
# Regression Weights: Show the direction and strength of the relationships between latent variables.
# Significance: Usually indicated by p-values. A p-value less than 0.05 typically suggests that the parameter is significantly different from zero, lending support to that part of the model.

# Fit Indices The output provides various fit indices which help in evaluating how well the model fits the data. Common indices include:
# Chi-Square Test of Model Fit (χ²): A non-significant chi-square (p > 0.05) suggests a good fit. However, it’s sensitive to sample size.
# Comparative Fit Index (CFI): Values closer to 1 indicate a good fit, with values above 0.90 or 0.95 often considered indicative of a good fit.
# Tucker-Lewis Index (TLI): Similar to CFI, with values close to 1 suggesting a good fit. Values above 0.90 are typically considered good.
# Root Mean Square Error of Approximation (RMSEA): Values less than 0.05 indicate a close fit, values up to 0.08 represent a reasonable error of approximation, and values greater than 0.10 suggest a poor fit.
# Standardized Root Mean Square Residual (SRMR): Represents the difference between the observed correlation and the model-predicted correlation. Values less than 0.08 are generally considered good.

##### 3.6. Evaluate the Model Fit ##### 
# Fit indices
fitMeasures(fit)


##### 3.7. Visual Output #####

library(semPlot)

# SEM plot
semPaths(fit, whatLabels = "est", layout = "tree")

# ----- 4. SEM Practice -----



