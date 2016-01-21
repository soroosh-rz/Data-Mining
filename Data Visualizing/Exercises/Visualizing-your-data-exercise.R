############################################
#####  Introduction to Data Mining I  ######
#####  Session #3 Exercise: Lattice   ###### 
#####  Conditioning Plots: Type and   ######
#####  Form of Plot DOES Matter       ######
############################################

# VISUALIZING TABULAR DATA WITH CONDITIONING

# Tables form an important class of statistical data. 
# Popular visualization methods designed for tables
# include bar charts and Cleveland dot plots.

# For illustration, we use the VADeaths dataset, 
# which gives death rates in the U.S. state of Virginia 
# in 1941 among different population subgroups. 

# VADeaths is a matrix
VADeaths
class(VADeaths)

# To use the lattice formula interface, we convert it 
# into a data frame
VADeathsDF <- as.data.frame.table(VADeaths, responseName = "Rate")
class(VADeathsDF)
VADeathsDF
VADeaths

?as.data.frame.table

# Bar charts are produced by the barchart() function, 
# and Cleveland dot plots by dotplot(). Both allow a
# formula of the form y ~ x (plus additional conditioning
# and grouping variables), where one of x and y
# should be a factor.

# Examine Var1 and Var2 in the VADeaths data frame:
VADeathsDF$Var1
VADeathsDF$Var2

# Draw a bar chart of the deaths in each of the age
# groups in the VADeathsDF data conditioned by
# Rural Male, Rural Female, Urban Male, Urban Female:
barchart(Var1 ~ Rate | Var2, VADeathsDF, layout = c(4, 1))

# This plot is potentially misleading, because a strong
# visual effect in the plot is the comparison of the 
# areas of the shaded bars, which do not mean anything. 

# This problem can be addressed by making the areas
# proportional to the values they encode. Do this
# by simply adding the argument "origin = 0" to
# your plot:
barchart(Var1 ~ Rate | Var2, VADeathsDF, layout = c(4, 1), origin = 0)

# A better design is to forego the bars, which distract
# from the primary comparison of the endpoint positions.
# Do this instead using a dot plot:
dotplot(Var1 ~ Rate | Var2, VADeathsDF, layout = c(4, 1))

# For this example, this display is more effective if 
# we use Var2 as a grouping variable, and join the
# points within each group (I give you this one):
dotplot(Var1 ~ Rate, data = VADeathsDF, 
        groups = Var2, type = "o",
        auto.key = list(space = "right", 
                        points = TRUE, 
                        lines = TRUE))

# The plot more clearly shows that the pattern of death 
# rate by age is virtually identical for urban and rural
# females, with an increased rate for rural males, and 
# a further increase for urban males. This interaction is
# difficult to see in the earlier plots.

# GENERIC FUNCTIONS AND METHODS

# High-level lattice functions are actually generic 
# functions, with specific methods doing the actual work.

# So far, our examples have used the "formula" methods; 
# that is, the method called when the first
# argument is a formula. Because barchart() and dotplot()
# are frequently used for multiway tables stored
# as arrays, lattice also includes suitable methods 
# that bypass the conversion to a data frame that would
# otherwise be required. 

# For example, an identical alternative plot 
# to the last example is
dotplot(VADeaths, type = "o",
        auto.key = list(points = TRUE, 
                        lines = TRUE, 
                        space = "right"))

# Methods available for a particular generic can be 
# listed using:
methods(generic.function = "dotplot")