# ----- B1705 Week 5 | MANOVA and MANCOVA | 07.02.2024 -----

# ----- PRE-LECTURE TASKS -----
# ----- 1. MANOVA -----

# Store the IRIS data in a dataframe 'my_data'
my_data <- iris

# MANOVA test
res.man <- manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris)
summary(res.man)

# Look to see how the groups differ
summary.aov(res.man)

# ----- LECTURE TASKS -----
# ----- 2. Load Data -----

rm(list=ls())
df <- read.csv('https://www.dropbox.com/scl/fi/jjh1sohgsv590f04x75pv/29_01.csv?rlkey=vhlxu0pziwymec3n7fiki2x1x&dl=1')

head(df) # display the first six rows

# ----- 3. Exploring Data -----
library(dplyr)

group_summary <- df %>%
  group_by(training_program) %>%
  summarise(
    Mean_Sprint_Time = mean(sprint_time),
    SD_Sprint_Time = sd(sprint_time),
    Mean_Jump_Height = mean(jump_height),
    SD_Jump_Height = sd(jump_height),
    Mean_Throw_Distance = mean(throw_distance),
    SD_Throw_Distance = sd(throw_distance)
  )
print(group_summary)

# Sprint Time Boxplot
boxplot(sprint_time ~ training_program, data = df,
        main = "Sprint Time by Training Program",
        xlab = "Group", ylab = "Sprint Time (seconds)")

# Jump Height Boxplot
boxplot(jump_height ~ training_program, data = df,
        main = "Jump Height by Training Program",
        xlab = "Group", ylab = "Jump Height (meters)")

# Throw Distance Boxplot
boxplot(throw_distance ~ training_program, data = df,
        main = "Throw Distance by Training Program",
        xlab = "Group", ylab = "Throw Distance (meters)")

# ----- 4. Running MANOVA -----

# Conduct MANOVA
manova_fit <- manova(cbind(sprint_time, jump_height, throw_distance) ~ training_program, data = df)

# Get summary of the MANOVA
summary(manova_fit)

# For univariate effects
summary.aov(manova_fit) 

# ----- 5. Tukeys HSD -----

# Load necessary libraries
library(dplyr)
library(multcomp)

# Conducting Post-Hoc Tests
# Tukey's HSD for Sprint_Time
tukey_sprint <- TukeyHSD(aov(sprint_time ~ training_program, data = df))
print("Tukey's HSD for Sprint_Time:")

print(tukey_sprint)

# ----- MANOVA: PRACTICE -----
# Data loading
df_2 <- read.csv('https://www.dropbox.com/scl/fi/3d5itbmyp5wgqbkajhctm/basketball_dataset.csv?rlkey=6wrmvey8uhvu67egh8g8twnmr&dl=1')


# Data preparation
df_2$X <- NULL
str(df_2)

df_2$group <- as.factor(df_2$group)
str(df_2)


# Data visualisation
library(ggplot2)

# Boxplot for points scored by group
ggplot(df_2, aes(x = group, y = points_scored, fill = group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Points Scored by Group",
       x = "Group",
       y = "Points Scored") +
  theme_minimal()

# Boxplot for assists by group
ggplot(df_2, aes(x = group, y = assists, fill = group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Assists by Group",
       x = "Group",
       y = "Assists") +
  theme_minimal()


# Conduct MANOVA
manova_fit <- manova(cbind(points_scored, assists) ~ group, data = df_2)

# Get summary of the MANOVA
summary(manova_fit)

# For univariate effects
summary.aov(manova_fit) 

# Explore group-based differences using Post-Hoc tests
# Load  libraries
library(multcomp)

# Tukey's HSD for Sprint_Time
tukey_assists <- TukeyHSD(aov(assists ~ group, data = df_2))
print("Tukey's HSD for Assists:")

print(tukey_assists)





