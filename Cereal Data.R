## This is our first capstone project.
#  Step-1: read the .csv file from directory and assign it to a variable
#  "cereals"

cereals <- read.csv("cereals_data.csv")

# Step-2: Check the dimension and structure of the data

View(cereals)
dim(cereals)
str(cereals)

# Missing value treatment.

library(VIM)
cereals <- kNN(cereals)

# Removal of extra variables generated because of kNN function by 
# subseting the data frame into a new data frame of 16 variables.

cereals <- cereals[,1:16]
str(cereals)

# Sample statistics to all 16 variables
library(psych)
library(ggplot2)
describe(cereals$calories)
describe(cereals$protein)
describe(cereals$fat)
describe(cereals$sodium)
describe(cereals$fiber)
describe(cereals$carbo)
describe(cereals$sugars)
describe(cereals$potass)
describe(cereals$vitamins)
describe(cereals$shelf)
describe(cereals$weight)
describe(cereals$cups)
describe(cereals$rating)

# Calories per serving Vs. manufacturer

calories <- cereals[order(cereals$calories),]
calories$mfr <- factor(calories$mfr)
calories$color[calories$mfr == "A"] <- "red"
calories$color[calories$mfr == "G"] <- "blue"
calories$color[calories$mfr == "K"] <- "coral4"
calories$color[calories$mfr == "N"] <- "purple"
calories$color[calories$mfr == "P"] <- "cyan4"
calories$color[calories$mfr == "Q"] <- "brown"
calories$color[calories$mfr == "R"] <- "coral"
dotchart(cereals$calories, labels = row.names(calories),
         cex = 0.5, groups = calories$mfr,
         main = "Dot chart for calories grouped by Manufacturer",
         xlab = "Calories per serving", color = calories$color)

ggplot(data = cereals)+
        geom_point(mapping = aes(x = cereals$mfr, y = cereals$rating, color = cereals$mfr)) + 
        facet_wrap(~cereals$mfr, nrow = 3)

# Data Manipulation to find out best product rating-wise
library(dplyr)
rating <- arrange(cereals, desc(rating))
View(rating)

rating <- arrange(cereals, mfr)
View(rating)
by(rating[,-(1:3)], rating$mfr, colMeans)

#lets see what can be done with calories
calories_150 <- subset(calories, calories >=150)
calories_150 <- mutate(calories_150, calories1= calories/weight)
head(calories_150)
View(calories_150)
calories_50 <- subset(calories, calories <= 50)
calories_50 <- mutate(calories_50, calories2 = calories/weight)
View(calories_50)
calories_50 <- subset(calories, calories <= 50 & protein >= 4)
View(calories_50)

## Lets create some interesting word cloud
# read the data
readLines("Cereals.txt")
Cer_1<-readLines("Cereals.txt")
Cer_2<-paste(Cer_1,collapse=" ")
head(Cer_2)

##Cleaning the data
#let us now lower case this data
clean_Cer_1<-tolower(Cer_2)
head(clean_Cer_1)

#Cleaning the Punctuation
clean_Cer_2<-gsub(pattern="\\W",replace=" ",clean_Cer_1)
head(clean_Cer_2)

##Cleaning the digits,pattern="\\d"
clean_Cer_3<-gsub(pattern="\\d",replace=" ",clean_Cer_2)
head(clean_Cer_3)

#cleaning the stopwords
library(tm)
stopwords()

#Lets remove Stopwords
clean_Cer_4<-removeWords(clean_Cer_3,stopwords())
head(clean_Cer_4)

#Let us remove single letters
clean_Cer_5<-gsub(pattern="\\b[A-z]\\b{1}",replace=" ",clean_Cer_4)
clean_Cer_5

#Finally removal of whitespaces using
clean_Cer_6<-stripWhitespace(clean_Cer_5)
clean_Cer_7<-strsplit(clean_Cer_6," ")
head(clean_Cer_7)
word_freq_Cer<-table(clean_Cer_7)
head(word_freq_Cer)
word_freq_Cer2<-cbind(names(word_freq_Cer),as.integer(word_freq_Cer))
head(word_freq_Cer2)
write.csv(word_freq_Cer2,"Word_frequency.csv")
# word cloud library
library(RColorBrewer)
install.packages("wordcloud2")
library(wordcloud2)     
word_cloud_Cer<-unlist(word_freq_Cer2)
head(word_cloud_Cer)

df<-data.frame(names(word_freq_Cer),as.numeric(word_freq_Cer))
wordcloud2(df,shape='diamond',size=0.75)

df<-data.frame(names(word_freq_Cer),as.numeric(word_freq_Cer))
wordcloud2(df,shape='pentagon',size=0.5)


## cluster Analysis##
View(cereals)
cereals <- cereals[,4:16]
View(cereals)
df <- cereals
dim(df)
df <- scale(df)
library(cluster)
library(factoextra)

set.seed(123)
ss <- sample(1:77,15)
df1 <- cereals[ss, ]
df1.scaled <- scale(df1)
head(df1.scaled, 3)
View(df1.scaled)

dist.eucl_15 <- dist(df1.scaled, method = 'euclidean')
head(dist.eucl_15)
round(as.matrix(dist.eucl_15)[1:3,1:3])
fviz_dist(dist.eucl_15)
fviz_nbclust(df,kmeans,method = 'wss') +
        geom_vline(xintercept = 4,linetype = 5,col = 'red')

set.seed(123)
km.res <- kmeans(df,4,nstart = 25)
km.res

km.res$totss
km.res$betweenss
aggregate(cereals, by = list(cluster = km.res$cluster),mean)
df_m <- cbind(cereals,cluster = km.res$cluster)
View(df_m)
write.csv(df_m, "df_m.csv")
fviz_cluster(km.res, data = df,
             palette = c('#2E9FDF','#00AFBB','#E7B800','#FC4E07'),
             ellipse.type = "convex",
             star.plot = TRUE,
             repel = TRUE,
             show.clust.cent = FALSE,
             ggtheme= theme_classic())
