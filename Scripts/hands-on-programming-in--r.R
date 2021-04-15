# Project 1: weighted dice ###################

8*3
24-6
18/3

#create a group of numbers from 1-6
1:6
x <- 6
die <- 1:6



#see which object names you've assigned
ls()

die - 1
die * die

# if you perform operations on 2 vectors of diff. lengths, r will repeat the shorter vector until it is as long as the longer vector, then do the operation
a <- 1:2
die + a
die + 1:4

# to do inner matrix multiplication
die %*% die

# to do inner matrix multiplication
die %o% die


# transpose a matrix
t(die)


#to round a number
round(3.14578)


#to get a number's factorial
factorial(3)

#r performs embedded functions form innner most to outermost
round(mean(die))


# you can roll the die using sample
sample(die, size = 1)

# to look up a functions arguments
args(round)



# to perform sample with replacement (by default it samples w/o replacement)
sample(die, size = 3, replace = TRUE)


# to simulate rolling a pair of dice
dice <- sample(die, size = 2, replace = TRUE)






############# CREATE FUNCTIONS ###########################

# create a function that will roll the pair of dice
#all functions have 3 basic parts a name, body of code, and set of arguments
roll <- function(){
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
}
roll()

# to add arguments to your function, put them inside the  ()
roll2 <- function(bones){
  dice <- sample(bones, size = 2, replace = TRUE)
  sum(dice)
}

#now roll2 will work as long as you supply a value for bones with you call roll2
roll2(bones = 1:4)

# to give bones arg a default value, put it inside the ()
roll3 <- function(bones = 1:6){
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
}





################# CHAPTER 3:PACKAGES AND HELP PAGES #################

# to check whether the dice are fair use two things replication and vizualization

library(ggplot2)


x <- c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
y <- x^3

qplot(x,y)


# qplot() will produce a histogram whenever you give it a single vector of values
xx <- c(1, 2, 2, 2, 3, 3)
qplot(xx, binwidth = 1)


x2 <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4)
qplot(x2, binwidth = 1)



# to run a command multiple times use replicate()
replicate(2, 1 + 1)

replicate(10, roll())


# simulate rolling the dice 10,000 times
rolls <- replicate(10000, roll())
qplot(rolls, binwidth = 1)




# make the roll function so it represents weighted dice
wroll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE,
                 prob = c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
  sum(dice)
}



wrolls <- replicate(10000, wroll())
qplot(wrolls, binwidth = 1)





###############################################
# chapter 4: project 2 playing cards
##############################################







