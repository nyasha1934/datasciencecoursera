# PRACTICE ASSIGNMENT
<<<<<<< HEAD
=======

# Getting Started
>>>>>>> 969e36212411dc62ecd31f18fe902d0148213270

# Getting Started

  # download and unzip data file as instucted
dataset_url <- "http://s3.amazonaws.com/practice_assignment/diet_data.zip"
download.file(dataset_url, "diet_data.zip")
unzip("diet_data.zip", exdir = "diet_data")

  # use 'getwd' and 'setwd' to make sure you can access the folder containing your data
getwd()
setwd("/Users/MB/Documents/Coursera/Data Science Specialization/datasciencecoursera/practice_assignment1")

  # list the files in the folder
list.files("diet_data")

  # set variable for each file 
andy <- read.csv(diet_data/Andy.csv)

  # check nrows using length of "day" column
length(andy$Day)

  # or you can check the dimension of the file
dim(andy)



# Commands To Get A Feel For The Data Files

  # str() compactly displays the internal stucture of the R object
str(andy)

  # summary() provides a summary of base stat calculations
summary(andy)

  # names() gives you the name of each column
names(andy)


<<<<<<< HEAD
# GETTING FAMILIAR WITH THE DATASET

# Subsetting the data

  # the first row of the "weight" column
andy[1, "Weight"]

  # the final weight on Day 30
andy[30, "Weight"]

  # you can also create a subset of "Weight" column where the data "Day" = 30
andy[which(andy$Day == 30), "Weight"]

andy[which(andy[, "Day"] == 30), "Weight"]

# Note there are many ways to get from A to B, but you need to understand some of the various approaches to subsetting data

  # assign Andy's starting and ending weight to vectors: 
andy_start <- andy[1, "Weight"]
andy_end <- andy[30, "Weight"]

  # to find out how much weight he lost, we can subract the vectors
andy_loss <- andy_start - andy_end
andy_loss


# What if we want to look at other subjects, or even everyone at once??
  # use '?list.files' to learn about the function

  # take the output of list.files() and store it in a variable 'files'
  #you may need to setwd() and make sure files are accessible
files <- list.files("diet_data")
files

  # "files" is a list of the contents of "diet_data" in alphabetical order
  # we can call a specific file by subsetting it:
files[1]    ## will list "Andy.csv" file
files[2]    ## will list "David.csv" file
files[3:5]  ## will list files 3, 4, 5, in alphabetial order


# Let's take a look at "John.csv"
head(read.csv(files[3]))
  # Error message because "John.csv" is in the "diet_data" folder not in our "/practice assignment1" working directory
  # therefore, the file is not in our working directory
  # to fix: append the directory to the beginning of the file name
  # you can use 'paste()' or 'sprintf()' functions
  # but in "?list.files" you'll see "full.names" will append the path to the file name for us
files_full <- list.files("diet_data", full.names = TRUE)
files_full

  # now we can try to look at "John.csv" again
head(read.csv(files_full[3]))


# What if we want to create one big data frame with everyone's data in it? We use rbind and a loop
  #start with the rbind
andy_david <- rbind(andy, read.csv(files_full[2]))    ## took the existing "Andy" data frame and added the rows from "David.csv" to it.
  #to check
head(andy_david)      ## shows the top of the data frame, Andy's data
tail(andy_david)      ## shows the tail of the data frame, David's data
  # note: rbind needs 2 arguments; the first is an existing data frame adn the second is what you append to it
  # as such, you may need to create an empty data set just so there is something to use as the existing data fram in the rbind argument


# Create a subset of the data frame that only shows the 25th day for Andy and David
day_25 <- andy_david[which(andy_david$Day == 25), ]
day_25


# You could manually append everyone's data to the same data frame using rbind (becomes tedious with many files)
  # let's use a loop instead
  # first let's understand what happens in a loop
for (i in 1:5) {print(i)}
# [1] 1
# [1] 2
# [1] 3
# [1] 4
# [1] 5             ## for each pass of the loop, 'i' increases by 1 from 1 through 5

# We can apply the same concept to our list of files
for (i in 1:5) {
    dat <- rbind(dat, read.csv(files_full[i]))
    
}                   ## Error: object 'dat' not found; you cannot rbind something into a file that does not exist yet

  # let's create an empty data frame called 'dat' before running the loop
dat <- data.frame()
for (i in 1:5) {
    dat <- rbind(dat, read.csv(files_full[i]))
}
str(dat)            ## created a data frame 'dat' will all our data in it

  # What would happen if we put 'dat <- data.frame() inside the loop? 
for (i in 1:5) {
        dat2 <- data.frame()
        dat2 <- rbind(dat2, read.csv(files_full[i]))
}
str(dat2)
head(dat2)
#   Patient.Name Age Weight Day
# 1        Steve  55    225   1
# 2        Steve  55    225   2
# 3        Steve  55    225   3
# 4        Steve  55    224   4
# 5        Steve  55    224   5
# 6        Steve  55    224   6
            
          ## because we put dat2 <- data.frame() inside the loop, 'dat2' is being rewritten with each apps of the loop
          ## so we only end up with the data from the last file in our list

# To find the median weigjht of all the data, use median() function
median(dat$Weight)
# [1] NA        ## print out 'dat'; we have some missing data from John labelled NA by R; we need to remove those missing values

# To remove NAs 
  # 1. subset data using 'complete.cases()' or 'is.na()'
  # 2. if you ?median, you'll see there is an argument called 'na.rm' that will remove NA values
median(dat$Weight, na.rm = TRUE)

# to take the median weight of day 30, take the subset of the data where Day = 30
dat_30 <- dat[which(dat[, "Day"] == 30),]
dat_30
# Patient.Name Age Weight Day
# 30          Andy  30    135  30
# 60         David  35    201  30
# 90          John  22    177  30
# 120         Mike  40    192  30
# 150        Steve  55    214  30
median(dat_30$Weight)



# CREATING FUNCTIONS

# Build a function that will return the median for any given day
  # define the arguments of the function, parameters the user will define
  # first parameter is the 'directory' hoding the data
  # second parameter is the 'day' for which we want to calculate the median
weightmedian <- function(directory, day) {#content of the function}
  
  # What goes in the "content of the function"?
  # 1. we need a data frame with allteh data from the CSVs
  # 2. we'll subset the data frame using the argument 'day' 
  # 3. take the median of that subset
  
  # to get all the data into a single data frame we can use the method before using list.files() and rbind()
  # basically we will combine all the manual computations we have already done into a function
weightmedian <- function(directory, day) {
    files_list <- list.files(directory, full.names = TRUE)    ## creates a list of files
    dat <- data.frame()   ## creates an empty data frame
    for (i in 1:5) {
        ## loops through the files, rbinding them together
        dat <- rbind(dat, read.csv(files_list[i]))
    }
    dat_subset <- dat[which(dat[, "Day"] == day), ]          ## subsets the rows that match the 'day' argument
    median(dat_subset[, "Weight"], na.rm = TRUE)             ## identifies the median weight while stripping out the NAs
}

# Now to test the function

weightmedian(directory = "diet_data", day = 20)
weightmedian("diet_data", 4)
weightmedian("diet_data", 17)
=======
# Playing Around With A Couple of Concepts
>>>>>>> 969e36212411dc62ecd31f18fe902d0148213270
=======
# Playing Around With A Couple of Concepts
>>>>>>> 969e36212411dc62ecd31f18fe902d0148213270
