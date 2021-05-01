############ jenny bryan's purrr tutorial #############
# https://jennybc.github.io/purrr-tutorial/

############ lessons: vectors and lists ###############

##### atomic vectors

(v_log <-  c(TRUE, FALSE, FALSE))
is.vector(av1) #T
is.logical(av1) #T

(v_int <- 1:5)
is.numeric(int) #T
is.integer(int) #T
is.double(int) #F

(v_doub <- 1:5 * 1.2)
is.numeric(float) #T
is.integer(float) #F
is.double(float) #T

(v_char <-  letters[1:4])

#You can construct a vector “by hand” with the c() function. 

### INDEXING A VECTOR #############

# To “index a vector” means to address specific elements or atoms, either for reading or writing. We index a vector using square brackets, like so: x[something]


# What happens when you request the zero-th element of one of our vectors?
v_int[0]
v_doub[0]
v_char[0]
v_log[0]
# it returns the class of the object



#   What happens when you ask for an element that is past the end of the vector, i.e. request x[k] when the length of x is less than k?

v_int[9]
v_doub[9]
v_char[9]
# it returns NA



#   What happens if the indexing vector is shorter than x?
v_log[2,3]
v_char[2, 3]
v_int[2, 3]
# error: incorrect number of dimensions

v_log[2]
v_char[2]
# these return the 2nd element in the vector


