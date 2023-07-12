library(tidyverse)
library(rvest)
library(imager)

## Question a)
data <- as_tibble(iris)
height <- dim(data)[1]
runningSpecies = data$Species[1]
lastCount = 1
par(mar=c(7,2,1,1))
for(i in 2:height){
    if(data$Species[i] != runningSpecies){
        boxplot(data[lastCount:i-1, 1:4], main = runningSpecies, las = 2)
        runningSpecies = data$Species[i]
        lastCount = i
    }
}
boxplot(data[lastCount:i-1, 1:4], main = runningSpecies, las = 2)

ggplot(iris, aes(x = 1:150, y = Sepal.Length, color=Species)) + geom_point(size=3) + ggtitle("Sepal Length")
ggplot(iris, aes(x = 1:150, y = Petal.Length, color=Species)) + geom_point(size=3) + ggtitle("Petal Length")

print("It can be concluded that Sepal length and Petal length are correlated")

# Question b)
plot(boats)
flip <- function(image){
    flipped = mirror(image, "x")
    return(flipped)
}
plot(flip(boats))

# Question c)

library(MASS)

print("I think a more reliable metric to find the 'untrustworthiness' of a type of ship is to look at the rate of incidents (incidents/months of service) that just incidents")

shipData <- as.tibble(ships)
shipData <- cbind(shipData,shipData$incidents/shipData$service)
names(shipData) <- c("type", "year", "period", "service", "incidents", "ROI")
for (i in 1:dim(shipData)[1]){
    ## To handle 0 months of service
    if(shipData$service[i] == 0){
        shipData$ROI[i] = 0
    }
}
ggplot(shipData, aes(x = service, y = incidents, color=type)) + geom_point(size = 3) + ggtitle("Plot 1")
ggplot(shipData, aes(x = 1:40, y = ROI, color=type)) + geom_point(size = 3) + ggtitle("Plot 2")

print("Plot 1 is misleading, Plot 2 shows that B-type ships are among the most reliable, while E-type ships have a lot of incidents in short spans of service")
vecAvg = 0
currType = "A"
lastType = "A"
aROI = 0
count = 0
for(i in 1:dim(shipData)[1]){
    if(currType == lastType){
        aROI = aROI + shipData$ROI[i]
        count = count + 1
    }
    else{
        vecAvg = c(vecAvg, aROI / count)
        count = 0
        lastType = currType
        aROI = shipData$ROI[i]
    }
    currType = shipData$type[i]
    }

vecAvg = c(vecAvg, aROI/count)
vecAvg = vecAvg[-1]
vecAvg
print(vecAvg[5]/vecAvg[2])
print("E-type ships are almost 3 times as untrustworthy as B-types")

# Question d)

stackEx <- read_html("https://stats.stackexchange.com/questions?tab=Votes")
Titles = stackEx %>% html_elements(".s-post-summary--content-title a") %>% html_text()
Stats = stackEx %>% html_elements(".s-post-summary--stats-item-number") %>% html_text()
Votes = "Votes"
Views = "Views"
Answers = "Answers"
for (i in 1:45){
    if(i %% 3 == 0){
        Views = c(Views, Stats[i])
    }
    else if (i %% 3 == 1){
        Votes = c(Votes, Stats[i])
    }
    else{
        Answers = c(Answers, Stats[i])
    }
}
dframe = tibble(Votes, Views, Answers)
dframe = dframe[-1,]
dframe = cbind.data.frame(dframe, Titles)
dframe = dframe[,c(4, 1, 3, 2)]
dframe

# Question e)

pills <- function(x){
    count = 0
    choice = sample(x, 1)
    while(choice == 'A'){
        choice = sample(x, 1)
        x[length(x) - count] = 'B'
        count = count + 1
    }
    return(count)
}
average = 0
for(i in 1:1000){
    vec = rep('A', times=100)
    average = average + pills(vec)
}
average = average/1000
answer <- round(average, digits=0)
answer
