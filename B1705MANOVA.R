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












