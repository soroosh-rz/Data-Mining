#################################################
#####                                       #####
#####       DATA INPUT AND OUTPUT           #####
#####                                       #####
#################################################

## Are many ways to input and output data in R 
## Here we look at several of most used commands
## for input and output, dividing it up into
## 1) interactively (keyboard); 
## 2) file input / output; and
## 3) accessing data in packages.

## This is not exhaustive, that would
## require days of time and demonstrations.

## 1) Keyboard Input: scan(); readline().

## Useful if number of values is small
## Can use scan() function
## scan() can also input files
## Interactive keyboard input ot useful for large data sets

x <- scan()
x

## scan() can read data from a file
## scan() reads a vector of values.
## scan() also returns a vector of values.

## General form of scan():

## scan(file="", what=0, n= -1, sep="", skip=0, quiet=FALSE)

## all arguments are optional. Defaults:
## file: file to read from; "" (nothing) is keyboard
## what: example of mode of data;
## what: 0 default numeric; " " for character
## n: number elements to read; n = -1 is to end of file
## sep: character to separate values; "" means 'any'
## skip: # lines to skip before read, default is 0 # for example, have descriptive text on top
## quiet: whether scan() reports number values read (F is default)

## Example reading in string (character) data

names <- scan(what=" ")

# 1: jeff linda
# 3: irving mary bill
# 6: louis
# 7: 
#   Read 6 items
# > names
# [1] "jeff"   "linda"  "irving" "mary"   "bill"   "louis" 
# > mode(names)

## 'scan.txt' contains

# 12 bobby
# 24 kate
# 35 david
# 20 michael

## Importing data files using the scan() function
## The scan() function is an extremely flexible 
## tool for importing data.  Unlike the read.table() 
## function, however, which returns a data frame, 
## the scan() function returns a list or a vector.  

x <- scan("c:/temp/scan.txt", what=list(age=0, name=""))

# Read 4 records

x

# $age
# [1] 12 24 35 20

# $name
# [1] "bobby"   "kate"    "david"   "michael"

# using the same text file and saving only the names as a vector
x <- scan("c:/temp/scan.txt", 
          what=list(NULL, name=character()))

# Read 4 records

x # is a list, first component is null

unlist(x) # gets rid of first null component
          # what reamins is a named vector

x # but x is unchanged, unless we do:

x <- unlist(x)
x

## Can look and see what is in a directory

list.files(path = "c:/temp") # to list files in any directory

# To interactively select a folder
list.files(path = choose.dir())

## To read a file into memory using scan():
## assuming that the file 'file_name' is numeric

## Here is a trick:
file_name = "c://temp/data.txt"
file_name

# Read from file using scan
d1 <- scan(file = file_name)
d1

## Or can name (numeric) file directly:

d <- scan(file = "c://temp//data.txt")
d

# To choose a file inteactively
d <- scan(file = file.choose())
d

## Can also use readline() function to read
## a line from keyboard interactively
## readline(prompt = "")

readline(prompt = "What do you like the most about R?: ")

## Couple it with an assignment:

what.I.like <- readline(prompt = "What do you like the most about R?: ")
what.I.like

## 2) File Input and Output: read.table(); and read.csv().

## Tabular Data Input directly from a file
## Most used functions are read.table() and read.csv()
## read.table expects a text file and converts to data frame

## If you Want to see files in a certain directory:

list.files("c:/temp")
read.table("c://temp/daphnia.txt", header = T)

## Couple with assignment and put into parentheses (to print):
(daphnia.data <- read.table("c://temp/daphnia.txt", header = T))

## To see what default directory is:
getwd()

## To change it to c:/temp (or any other directory):
setwd(choose.dir())
setwd("c://temp")

## To verify what directory is now:
getwd()

## list.files is non-interactive:
list.files()

## file.choose() coupled with read.table() is interactive:
chosen.txt <- read.table(file.choose(), header = T)
chosen.txt
head(chosen.txt) # just first six rows

## What is class of chosen.file ?
class(chosen.txt)
## Structure of object which shows metadata info
str(chosen.txt)  

## read.table() and read.csv() change input to dataframe

## read.csv() expects a csv file
chosen.csv <- read.csv(file.choose())
head(chosen.csv)

## What is class of chosen.csv ?
class(chosen.csv)

####################    OUTPUT: WRITING TO FILES

## Most used functions to write data to output files 
## are write.table() and write.csv() to output 
## data frame into a plain text files
?write.csv
write.csv(chosen.csv, "geoff.csv")
list.files()
file.exists("geoff.csv")

write.table(chosen.txt, "geoff.txt")
file.exists("geoff.txt")

## 3) Accessing Data in R Packages
## Many packages contain data sets
## Two approaches to access data in packages:
## Generally, (not always) must first
## load package using library() command.
## (Package must be installed first always).

## Then use either: 1) data(); or 2) attach() commands

## Give the list of loaded packages
search()

## First load the package
library(car)

data(package="car")

## A data set can be read into global environment using data() command
data(Prestige)
ls()
View(Prestige)

## Or by referencing the package if not loaded
data(clouds, package = "HSAUR2")
ls()

## To see data in particular packages
data(package="HSAUR2")

## Data sets in packages are loaded as data frames

## Other way is to use attach() command
## Attaching makes a copy of the data frame
search()
library("HSAUR2")
attach(Forbes2000)
search()    # Note that Forbes2000 is now #2 in search path

detach(Forbes2000)
search()

str(heptathlon)
plot(run200m,run800m)
plot(heptathlon$run200m,heptathlon$run800m)
