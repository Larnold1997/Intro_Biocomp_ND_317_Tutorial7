#######Problem 1

#Initializing Data vectors to empty
sequenceID <- NULL
sequenceLENGTH <- NULL
percentCG <- NULL

#Reading the File
fileName <- "Lecture11.fasta"
readFile<- file(fileName,open="r") #Makng fileName readable
linn <-readLines(readFile) #Setting linn to the lines of conn


#Looping through contents of file
j = 1
k = 1
for (i in 1:length(linn)){
  linn_split <- strsplit(linn[i], "")[[1]] #Splits line into series of characters to be accessed
  
  #Conditional to see if the ith line is a sequence ID
  if(linn_split[1] == ">"){    
    sequenceID[j] = substring(linn[i],5)
    j = j + 1
    
    #Calculations for the base pairs
  } else {
    
    #Getting length of sequence
    sequenceLENGTH[k] = nchar(linn[i], type = "chars")
    
    #Calculating percentCG
    CG = 0
    for (base in linn_split) {
      if (base == "G" || base == "C") {
        CG = CG + 1
      }
    }
    percentCG[k] = CG/sequenceLENGTH[k]*100

    k = k + 1
  }
}
close(readFile)

#Putting everything into a table called DNA.dat
library(ggplot2)
DNA.dat <- data.frame(sequenceID, sequenceLENGTH, percentCG)

#Histogram of Sequence Length
length.hist <- ggplot(data = DNA.dat, aes(sequenceLENGTH))
length.hist = length.hist + geom_histogram(binwidth = 5) + coord_cartesian()  
length.hist

#Histogram of GC Content
GC.hist <- ggplot(data = DNA.dat, aes(percentCG))
GC.hist = GC.hist + geom_histogram(binwidth = 5) + coord_cartesian()  
GC.hist


#######Problem 2
library(ggplot2)
library(grid)

#read the data file
data2 <- read.table(file = "baseball2011_Win&HR.txt", header = TRUE, sep="\t")

#plot the proportion of wins as a function of HRs
a = ggplot(data = data2, aes(x = HR , y = Win))
a = a + geom_point() + coord_cartesian() + theme_bw() + xlab("Homeruns") + ylab("Win Percentage")

#adding in the trend line
a = a + geom_point() + coord_cartesian() + theme_bw() + xlab("Homeruns") + ylab("Win Percentage") + stat_smooth(method = "lm")




#######Problem 3

#Read Data into Table
data3 <- read.table(file = "data.txt", header = TRUE, sep=",")
attach(data3)

#Initialize Values to 0
north.total <- 0
south.total <- 0
east.total  <- 0
west.total  <- 0
north.counter <- 0
south.counter <- 0
east.counter  <- 0
west.counter  <- 0

#Go through data and add to appopriate region
for (i in 1:nrow(data3)){
  if (data3[i,1] == "north"){
    north.total = north.total + data3[i,2]
    north.counter = north.counter + 1
  }
  else if (data3[i,1] == "south"){
    south.total =  south.total + data3[i,2]
    south.counter = south.counter + 1
  }
  else if (data3[i,1] == "east"){
    east.total = east.total + data3[i,2]
    east.counter = east.counter + 1
  }
  else if (data3[i,1] == "west"){
    west.total = west.total + data3[i,2]
    west.counter = west.counter + 1
  }
  
}

#Compute Averages
north.average <- north.total/north.counter
south.average <- south.total/south.counter
east.average  <- east.total/east.counter
west.average  <- west.total/west.counter
averages <- c(north.average, south.average, east.average, west.average)
averages <- data.frame(averages)


#Bar Graph of Means
mean.bar <- ggplot(data = averages, aes(averages))
mean.bar = mean.bar + geom_bar() + coord_cartesian()  
mean.bar

#Scatter Plot of all Observations
scatter.observations <- ggplot(data = data3, aes(as.factor(region), observations))
scatter.observations = scatter.observations + geom_jitter() + coord_cartesian()
scatter.observations  

##The bar graph and scatter plot tell very different stories. 
#Some of the regions have observations with little variance, while others have very large variance. 
#This is only captured by the scatterplot. 
