
getDataset <- function() {
        if(!file.exists("household_power_consumption.txt")) {
                url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
                download.file(url, "power.zip")
                unzip("power.zip")
                
        }
}


getPowerData <- function() {
        ## Use an empty working directory to run the code
        getDataset()
        
        
        fileNames <- list.files()
        
        # let's have an initial look at the data file
        initial <- read.table(fileNames[1], sep=";", dec=".", nrow=10, skip=1)
        classes <- sapply(initial, class)
        
        
        # cool function to get number of columns in data file (source="http://stackoverflow.com/questions/5788117/only-read-limited-number-of-columns-in-r")
        colNum <- max(count.fields(fileNames[1], sep=";"))
        
        
        # another cool function to load only the first column of the data file (source= "http://stackoverflow.com/questions/5788117/only-read-limited-number-of-columns-in-r")
        dates <- read.table(fileNames[1], sep=";", header=T, colClasses=c(classes[1], rep("NULL", colNum-1)))
        
        # get a list of logicals, and you can see that I don't favour oneliners. :)
        day1 <- grepl("^1/2/2007", dates[,1])
        day2 <- grepl("^2/2/2007", dates[,1])
        days <- day1 + day2
        days <- as.logical(days)
        
        # load the whole dataset, it is gonna take for a while
        powerData <- read.table(fileNames[1], header=T, sep=";", quote="", dec=".", stringsAsFactors=F, nrow=length(days))
        
        
        # get the smaller dataset
        dat <- powerData[days,]
        
        names(dat) <- gsub("_", " ", names(dat))
        
        datetime <- paste(unclass(dat[, 1]), unclass(dat[, 2]), sep=" ")
        
        
        datetime <- strptime(datetime, format="%d/%m/%Y %H:%M:%S")
        
        dat[, 3:9] <- lapply(dat[, 3:9], as.numeric)
        
        dat <<- cbind(datetime, dat[, 3:9])
        
}

makePlot1 <- function() {
        objects <- ls()
        if(!sum(grepl("^dat", objects))) {
                getPowerData()
        }
        
        png(file="plot1.png", width=480, height=480)
        hist(dat[,"Global active power"], col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)")
        dev.off()
}