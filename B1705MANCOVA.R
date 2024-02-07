# ----- B1705 Week 5 | MANOVA and MANCOVA | 07.02.2024 -----

# ----- LECTURE TASKS -----
# ----- 1. MANCOVA: Demo -----

##### 1.1. Loading Data #####
rm(list=ls())
df <- read.csv('https://www.dropbox.com/scl/fi/xe2n37vfyck76i7hw4gnn/mancova_cycling.csv?rlkey=1khog9n5p1sh9evgsi6ovr5ta&dl=1')
df$X <- NULL
head(df) # display the first six rows

##### 1.2. Examine potential interactions #####

# Time Boxplot
boxplot(time ~ training_program, data = df,
        main = "Time by Training Program",
        xlab = "Group", ylab = "Time (seconds)")

# Elevation Boxplot
boxplot(elevation ~ training_program, data = df,
        main = "Elevation by Training Program",
        xlab = "Group", ylab = "Elevation (ft)")

# Speed Boxplot
boxplot(speed ~ training_program, data = df,
        main = "Speed by Training Program",
        xlab = "Group", ylab = "Speed (km/H")

# Interaction of Program, Age, and Speed
interaction.plot(df$training_program, df$age, df$speed,
                 main="Interaction of Program and Age on Speed",
                 xlab="Training Program", ylab="Sprint Time", col=c("red","blue","green"))

# Interaction of Program, Age, and Elevation
interaction.plot(df$training_program, df$age, df$elevation,
                 main="Interaction of Program and Age on Elevation",
                 xlab="Training Program", ylab="Elevation", col=c("red","blue","green"))

# Interaction of Program, Age, and Time
interaction.plot(df$training_program, df$age, df$time,
                 main="Interaction of Program and Age on Time",
                 xlab="Training Program", ylab="Time", col=c("red","blue","green"))


# Different Examples of how to visualise some of the data 

library(ggplot2)
# Interaction plot for Speed as a function of Training Program and Age
ggplot(df, aes(x=training_program, y=speed, color=age)) +
  geom_point() +
  geom_line(aes(group=age)) +
  theme_minimal() +
  labs(title="Interaction of Training Program and Age on Speed",
       y="Speed (km/h)",
       x="Training Program")

df_long <- tidyr::pivot_longer(df, cols=c(speed, elevation, time), names_to="PerformanceMetric", values_to="Value")

ggplot(df_long, aes(x=training_program, y=Value, color=age)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  facet_wrap(~PerformanceMetric, scales="free_y") +
  theme_minimal() +
  labs(title="Interactions Across Performance Metrics",
       y="Metric Value",
       x="Training Program")

library(lattice)
coplot(speed ~ elevation | cut(age, 2), data=df, panel=panel.smooth, xlab="Elevation (meters)", ylab="Speed (km/h)")


# Fit a linear model for demonstration
model <- lm(time ~ training_program * age, data = df)

# Create interaction plots based on model predictions
library(effects)
plot(allEffects(model))


##### 1.3. MANCOVA Run #####

# MANCOVA model with Age as a covariate
mancova_fit <- manova(cbind(time, elevation, speed) ~ training_program + age, data = df)

# Viewing the Results
summary(mancova_fit)

# For univariate effects
summary.aov(mancova_fit) 


# ----- MANCOVA: Practical -----

rm(list=ls())
df <- read.csv('https://www.dropbox.com/scl/fi/jjh1sohgsv590f04x75pv/29_01.csv?rlkey=vhlxu0pziwymec3n7fiki2x1x&dl=1')
df$X <- NULL
head(df) # display the first six rows

#| code-fold: true
#| code-summary: Show solution


# Interaction of Program, Age, and Sprint_Time
interaction.plot(df$training_program, df$age, df$sprint_time,
                 main="Interaction of Program and Age on Sprint Time",
                 xlab="Training Program", ylab="Sprint Time", col=c("red","blue","green"))

# Interaction of Program, Age, and Jump_Height
interaction.plot(df$training_program, df$age, df$jump_height,
                 main="Interaction of Program and Age on Jump Height",
                 xlab="Training Program", ylab="Jume Height", col=c("red","blue","green"))

# Interaction of Program, Age, and Throw_Distance
interaction.plot(df$training_program, df$age, df$throw_distance,
                 main="Interaction of Program and Age on Throw Distance",
                 xlab="Training Program", ylab="Throw Distance", col=c("red","blue","green"))


# Running MANCOVA Model 
# MANCOVA model with Age as a covariate
mancova_fit <- manova(cbind(sprint_time, jump_height, throw_distance) ~ training_program + age, data = df)

# Viewing the Results
summary(mancova_fit)
# Univariate results
summary.aov(mancova_fit) 




