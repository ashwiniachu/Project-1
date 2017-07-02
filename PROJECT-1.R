######Part 1############


library(data.table)
library(dplyr)
library(plyr)
setwd("C:/ASHWINI/Personal/ACAD/New folder/DAT File")
allfiles= list.files(path="C:/ASHWINI/Personal/ACAD/New folder/DAT File",pattern="*.dat")
length(allfiles)
for (file in allfiles){
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset"))
  {
    dataset <- read.delim(file, header=TRUE, skip=9 ,sep="\t")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.delim(file, header=TRUE, skip=9 ,sep="\t")
    dataset<-rbind(dataset, temp_dataset) ##store the data in a temp file
    rm(temp_dataset)
  }
}
dataset
str(dataset)   ###shows the type of file





#######Part 2#########

library(XML)
url<-"C:/ASHWINI/Personal/ACAD/New folder/iris/iris.xml"
df <- xmlToDataFrame(url)   ###converts xml to dataframe
df
















######Part 3######

library(rjson)
jsonformat<-toJSON(iris) ###converts to JSON format
jsondata<-data.frame(fromJSON(toJSON(df))) ###converts from JSON to Dataframe
iris_data<-data.frame(jsondata,stringsAsFactors = F)
iris_data
str(iris_data)















###Part IV###

str(iris_data)###it projects your sorted data
glimpse(iris_data)###it projects same as your dataframe without any order

iristb<-tbl_df(iris_data)
iristb

####Implement select

iris_data
select(iris_data,3)
select(iris_data,c(Sepal.Length,Petal.Width))
select(iris_data,contains("."))
select(iris_data,ends_with("Length"))
select(iris_data,matches("Length"))


####Implement filter 

library(dplyr)
filter(iris_data,Sepal.Length >4.8)
filter(iris_data,Sepal.Length > 4.4 & Petal.Width > 0.3)
iris_data$Petal.Width<-as.numeric(iris_data$Petal.Width)
iris_data$Petal.Width<-c(0.6,0.3,0.4)
str(iris_data)

################## mutate() in dplyr

iris_data$Petal.Width<-as.numeric(iris_data$Petal.Width)
iris_data$Petal.Width<-c(0.6,0.3,0.4)
iris_data$Sepal.Width<-as.numeric(iris_data$Sepal.Width)
iris_data$Sepal.Width<-c(3.5,3.2,3.0)
iris_data$Petal.Length<-as.numeric(iris_data$Petal.Length)
iris_data$Petal.Length<-c(1.5,1.1,1.7)
iris_data$Sepal.Length<-as.numeric(iris_data$Sepal.Length)
iris_data$Sepal.Length<-c(4.4,5.0,4.8)

iris_data %>% 
  mutate(iris_data, 
         Sepal.Area = Sepal.Width * Sepal.Length,
         Petal.Area = Petal.Width * Petal.Length,
         Area.Ratio = Petal.Area / Petal.Area)

str(iris_data)

iris_data %>% 
  mutate(Petalarea = round(Petal.Length /Petal.Width),2)%>% 
  filter(Petalarea<3)

################## match in dplyr

select(iris_data,matches("Length"))

################## arrange in dplyr

iris_data %>% 
  select(Sepal.Length:Petal.Width)%>%
  rename(SepalLength=Sepal.Length)

################## rename in dplyr
### using select

select(iris_data, Species, PetalLength = Petal.Length, PetalWidth = Petal.Width)

### using rename

rename(iris_data,c("Species"= "Speciesnew"))


################## summary of iris_data

iris_data%>%
  mutate(iris_data, 
         Sepal.Area = Sepal.Width * Sepal.Length,
         Petal.Area = Petal.Width * Petal.Length,
         Area.Ratio = Petal.Area / Petal.Area)%>%
  summarise(mean = mean(Sepal.Length),
            sum = sum(Sepal.Area),
            min = min(Petal.Area),
            Mn.Md.PL = mean(Petal.Length) / median(Petal.Length),
            Num.PL.Vals = n_distinct(Petal.Length))

summary(iris_data)




library(ggplot2)
ggplot(iris_data, aes(x = Petal.Length, y = Sepal.Length, colour = Species)) + 
  geom_point() +
  ggtitle('Iris Species by Petal and Sepal Length')































