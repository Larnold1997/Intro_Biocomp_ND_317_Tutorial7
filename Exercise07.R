#Problem 1

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