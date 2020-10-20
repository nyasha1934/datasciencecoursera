# Practice Assignment

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



# other commands to get a feel of the data file

# str() compactly displays the internal stucture of the R object
str(andy)

# summary() provides a summary of base stat calculations
summary(andy)

# names() gives you the name of each column
names(andy)


# play around with a couple of concepts
