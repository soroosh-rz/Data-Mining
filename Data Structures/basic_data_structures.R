###############################################
#####                                     #####
#####        BASIC DATA STRUCTURES        #####
#####                                     #####
###############################################

## BASIC DATA STRUCTURES
# vectors
c(1,2,6,4,8)
x <- c(88,5,15,44)
x

# vectors are addressable
x <- c(88,5,15,44)
x[1:3]
x <- c(x[1:3],168,x[4])
x

# vector elements can have names
v <- c(10,20,30)
length(v)
v
names(v) <- c("Moe","Larry","Curly")

# note that the R presentation of 'v' is now different
# v now is a 'named vector'
v
print(v) # note: same as typing "v" at prompt

# can access by index
v[1]

# or by name
v["Moe"]

# or by index
v[2:3]

# or by name
c(v["Moe"],v["Curly"])

# make it a "persistent object"
persistent.v <- c(v["Moe"],v["Curly"])
persistent.v
  
# matrix is a vector with two dimensions
A <- 1:6 # A is a vector at this point
dim(A)
print(A) # same as expressing, or evaluating, A
dim(A) <- c(2,3) # Force vector A to be 2 x 3
print(A) # Now A is a matrix, no longer a vector

matrix(A, nrow = 2, ncol = 3)

# list is an ordered collection of objects
e <- list(thing="hat",size=8.25, mat=matrix(A, nrow = 2, ncol = 3))
e

e <- list(thing="hat",
          size=8.25, 
          mat=matrix(A, nrow = 2, ncol = 3), 
          e.list=e)
e

# can reference list components and elements with subscripts
e

# first component of e, call by index
e[1]

# what is in first component of e is a character vector
# with one element "hat"
e[[1]]

# call first component of e by name
e["thing"]

e["thing"][[1]]

# remember, what is the list 'e':
e

# call second component of e by index
e[2]

# call second component of e by name
e["size"]

# this calls the vector that is 
# in the second component of e
e[[2]]

# remember what is list 'e'?:
e

# there is no third component of list 'e'
e[3]

# can combine data heterogeneous
# R data structures using list() function
x1 <- c(1, 2, 3);x1

# R tries to 'coerce' or resolve the
# inherent conflict of putting characters
# and integers into the same vector

# this is a character vector:
c("a", "b", "c", "d")
typeof(c("a", "b", "c", "d"))
mode(c("a", "b", "c", "d"))

# this is a vector of mode numeric
# but is of type "double":
c(1.33,2.5,3.25)
typeof(c(1.33,2.5,3.25))
mode(c(1.33,2.5,3.25))

typeof(c(1, 2, 3))
typeof(as.integer(c(1, 2, 3)))
mode(as.integer(c(1, 2, 3)))

# what happens when you try to combine
# the characters and real numbers in
# one vector?:
x2 <- c(c("a", "b", "c", "d"),c(1.33,2.5,3.25));x2
x3 <- 3;x3

# this matrix has no valued elements
x4 <- matrix(nrow = 2, ncol = 2);x4

# we assign values to first column only
x4[,1] <- c(1, 2);x4

# we assign values to second column
x4[,2] <- c(3, 4);x4

x4[1,2]

# we add each of the above objects as
# components to the list Y
Y <- list(x1 = x1, x2 = x2, x3 = x3, x4 = x4)
Y  # What appears when we type Y at R prompt?

# data frame is a list
# here is data frame for wins/losses 
# in National League East:
teams <- c("PHI","NYM","FLA","ATL","WSN") # character vector
teams
w <- c(92, 89, 94, 72, 59) # numeric vector
w
l <- c(70, 73, 77, 90, 102) # numeric vector
l
nleast <- data.frame(teams,w,l) # data frame
nleast

# can refer to components (or columns) 
# using the $ operator
nleast$w
nleast[,2]

nleast$teams == "FLA"
nleast$l[nleast$teams=="FLA"]

#############################################################################

