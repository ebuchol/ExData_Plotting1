is.installed <- function(package) {
  ## This function tests whether a needed package is installed on your machine.
    is.element(package, installed.packages()[,1])
}

fileDownload <- function() {
  ## This function checks whether the desired data is downloaded. If not,
  ## it is downloaded and extracted.
    if(!file.exists("data")) {dir.create("data")}
    fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    zipFile <- "./data/hpcData.zip"
    path <- sub(".zip", "", zipFile)
    if(!file.exists(file.path(path, "household_power_consumption.txt"))) {
        download.file(fileURL, destfile = zipFile)
        unzip(zipFile, exdir = path)
    }
}

readData <- function(path) {
  ## This function reads in the desired data and returns only the data that
  ## corresponds to the dates "2007-02-01" and "2007-02-02"
    data <- read.table(path, sep = ";", header = TRUE, na.strings = "?")
    data$Date <- as.Date(strptime(data$Date, "%d/%m/%Y"))
    dates <- c("2007-02-01", "2007-02-02")
    dates <- as.Date(strptime(dates, "%Y-%m-%d"))
    filterDate <- data$Date == dates[1] | data$Date == dates[2]
    return(data[filterDate, ])
}

plot3 <- function() {
  ## This function downloads a data set on household power consumption and generates
  ## a line graph of 3 different types of energy sub metering readings from different
  ## rooms of a house.

  ## Checks if "png" package is installed, installs it if needed, and loads it.
  ## Then downloads and extracts data set if needed.
    if(!is.installed("png")) {
        print("Installing png package...")
        install.packages("png")
    }
    library(png)
    fileDownload()

  ## Read and modify data set.
    path <- "./data/hpcData/household_power_consumption.txt"
    data <- readData(path)
    dt <- strptime(paste(data$Date, data$Time), "%Y-%m-%d %H:%M:%S")

  ## Generate and write to PNG file the 3 line graphs of energy sub metering readings.
    x <- dt
    y1 <- data$Sub_metering_1
    y2 <- data$Sub_metering_2
    y3 <- data$Sub_metering_3
    png(filename = "plot3.png")
    plot(x, y1, type = "n", xlab = "",
        ylab = "Energy sub metering")
    lines(x, y1, col = "black")
    lines(x, y2, col = "red")
    lines(x, y3, col = "blue")
    legend("topright", lty = "solid", col = c("black", "red", "blue"),
        legend = colnames(data[7:9]))
    dev.off()
}
