##########################################
#####  Introduction to Data Mining  ######
#####     Input-Ouput Exercises     ######
##########################################

# Exercise #1 (easy): Using the scan() function

# Use the scan() function to manually input x, y and z
# objects into your R session such that when you
# print() or otherwise 'express' the value of
# the inputted object, you see the following:

# x
# [1] 3 5 9 12 57

# y
# [1] 3.0 6.0 3.2 0.8 16.2

# z 
# [1] "9"   "10"   "a"    "irving"

# In all cases, that is, for x and y and z
# query the class(), the typeof() and the mode().
# What is the class, type, and mode of each?

x <- scan()

# Put focus of control in Console window first !
# enter 3 then 5 then 9 then 12 then 57, hitting return
# just once after each number is typed onto the console,
# and then hit enter twice at the end.
x
# [1]  3  5  9 12 57
class(x)
# [1] "numeric"
typeof(x)
# [1] "double"
mode(x)
# [1] "numeric"

y <- scan()
# Put focus of control in Console window first !
# enter 3.0 then 6.0 then 3.2 then 0.8 then 16.2, hitting
# return just once after each number is typed onto the
# console, and then hit enter twice at the end.
y
# [1] 3.0 6.0 3.2 0.8 16.2
class(y)
# [1] "numeric"
typeof(y)
# [1] "double"
mode(y)
# [1] "numeric"

z <- scan(what=" ")
# Put focus of control in Console window first !
# enter '9' then '10' then 'a' then 'irving' (no single 
# quate marks are necessary), hitting return just
# once after each string is typed onto the
# console, and then hit enter twice at the end.
z
# [1] "9"    "10"    "a"     "irving"
class(z)
# [1] "character"
typeof(z)
# [1] "character"
mode(z)
[1] "character"


# Exercise #2 (moderate): Reading .txt and .csv files
# directly into your worksession and then writing them
# out onto disk with another name (but the same extension).

# Take the two provided files, "flea.csv" and "flies.txt",
# and put them in a folder on your hard drive somewhere.

# Then use the appropriate R functions (carefully using the
# 'header=' argument) to directly read in each file as a
# dataframe with the same name in your R session. Then use
# the appropriate R function to read each on back out onto
# your disk, but to save each with a different name and 
# extension.

# interactively read in flea.csv as .csv file,
# save it as a dataframe named 'flea'.
# Don't need to mess with header argument as
# read.csv assumes that header=TRUE as it does
# with this file since it has headers in the file.
flea <- read.csv(file.choose())

# interactively read in flies.txt as .txt file,
# save it as a dataframe named 'flea'. When using the
# R function read.table(), R assumes that there is no
# header information in the file, so, unlike read.csv,
# the default setting is that header=FALSE. So if
# your inputted .txt data file ACTUALLY DOES have
# headers, you must proactively remind the read.table
# function of that (unlike with the read.csv() function).
flies <- read.table(file.choose(), header=TRUE)

# make sure to set your directory, for example, use
choose.dir() # and drill down and select your directory location.

# Read out and save the (now) R dataframe object "flea"
# as 'fleas.txt'(and not as 'flea.csv') onto your
# hard drive in your default directory. 
write.table(x="flea", file="fleas.txt", row.names = FALSE)
file.exists("fleas.txt")
# [1] TRUE

# Read out and save the (now) R dataframe object "flies"
# as 'fly.csv' (and not as 'flies.txt') onto your
# default directory.
write.csv(x="flies", file="fly.csv", row.names = FALSE)
file.exists("fly.csv")
# [1] TRUE


# Exercise #3 (harder): Using readline() in your
# own user-defined function, that is, in the function
# that you build yourself which satisfies the 
# following requirements:

# Build a function that, when executed, prompts the user
# with any string statement, such as "How are you 
# feeling today?" and then returns, that is, prints
# to the R console, the user's response to that
# question. The function should be constructed so that
# it accepts any string expression as the single argument
# and then uses that expression as the prompt. For example,
# build a function, such as get.response(), that, when
# called with the following R command, will prompt the
# user with that string and then return the user's response,
# such as:

# build the function get.resonse() so that you might 
# see the following dialog if the user responds to
# the initial prompt of "Do you think it will rain
# tomorrow?" with "Yes, I do.":

# for example:
get.response(prompt = "Do you think it will rain tomorrow?")
# [1] "Yes, I do."


# Solution, here is the minimal function that will work:
get.response <- function(prompt = " ") {
  readline(prompt)
}

# Then call the get.response() function:
get.response(prompt = "Do you think it will rain tomorrow?")
# [1] "Yes, I do."

# or you could do:

x <- "How are you?"

get.response(x)
# How are you? <you respond>:  I am fine, thank you.
# [1] "I am fine, thank you."