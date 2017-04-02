setwd("~/Documents/STAT306/Project/Movie_Project")

#install.packages('plyr')
#install.packages("stringr", repos='http://cran.us.r-project.org')
library(stringr)
library(plyr)
#read data from csv file sorted by year, with primary genres selected
data = read.csv("moviedata_sortedby_year_split_genre.csv", colClasses=c("genres"="character"))
data

typeof(data$genre)

options(na.action=na.omit)
count(data,"primary_genre")
#produces frewuency tabl eoutput linke on page 110:
table(data$primary_genre)
count(data$primary_genre)
count(data,"content_rating")

genrevector <- data$genres 
countAction = 0; countAdventure = 0; countAnimation = 0; countBiography = 0; countComedy = 0;
countCrime = 0; countDocumentary = 0; countDrama = 0; countFamily = 0; 
countFantasy = 0; countFilmNoir = 0; countGameShow = 0;
countHistory = 0; countHorror = 0; countMusic = 0; countMusical = 0; countMystery = 0;
countRomance = 0; countSciFi = 0; countThriller = 0; countWestern = 0; countSport = 0; countNews = 0; countRealityTV = 0;

for (i in genrevector ) {
  if (grepl(i, "Action")) 
    countAction = countAction +1;
  if (grepl(i, "Adventure")) 
    countAdventure = countAdventure +1;
  if (grepl(i, "Animation")) 
    countAnimation = countAnimation +1;
  if (grepl(i, "Biography")) 
    countBiography = countBiography +1;
  if (grepl(i, "Comedy")) 
    countComedy = countComedy +1;
  if (grepl(i, "Crime")) 
    countCrime = countCrime +1;
  if (grepl(i, "Documentary")) 
    countDocumentary = countDocumentary +1;
  if (grepl(i, "Drama")) 
    countDrama = countDrama +1;
  if (grepl(i, "Family")) 
    countFamily = countFamily +1;
  if (grepl(i, "Fantasy")) 
    countFantasy = countFantasy +1;
  if (grepl(i, "Film-Noir")) 
    countFilmNoir = countFilmNoir +1;
  if (grepl(i, "Game-Show")) 
    countGameShow = countGameShow +1;
  if (grepl(i, "History")) 
    countHistory = countHistory +1;
  if (grepl(i, "Horror")) 
    countHorror = countHorror +1;
  if (grepl(i, "Music")) 
    countMusic = countMusic +1;
  if (grepl(i, "Musical")) 
    countMusical = countMusical +1;
  if (grepl(i, "Mystery")) 
    countMystery = countMystery +1;
  if (grepl(i, "Romance")) 
    countRomance = countRomance +1;
  if (grepl(i, "Sci-Fi")) 
    countSciFi = countSciFi +1;
  if (grepl(i, "Thriller")) 
    countThriller = countThriller +1;
  if (grepl(i, "Western")) 
    countWestern = countWestern +1;
  if (grepl(i, "War")) 
    countWar = countWar +1;
  if (grepl(i, "Sport")) 
    countSport = countSport +1;
  if (grepl(i, "News")) 
    countNews = countNews +1;
  if (grepl(i, "Reality-TV")) 
    countRealityTV = countRealityTV +1;
}
countAction; countAdventure; countAnimation; countBiography; countComedy ;
countCrime ; countDocumentary ; countDrama ; countFamily ; 
countFantasy ; countFilmNoir ; countGameShow ;
countHistory ; countHorror ; countMusic ; countMusical ; countMystery ;
countRomance ; countSciFi ; countThriller ; countWestern ; countWar; countSport; countNews; countRealityTV;

#classify each movie genre as being common or uncommon using regex's
options(max.print=99999)
d = gsub(".*(Documentary|Film-Noir|Game-Show|Western).*", "Un-common", genrevector)
d
d = gsub(".*(Action|Adventure|Animation|Biography|Comedy|Crime|Drama|Family|Fantasy|History|Horror|Music|Musical|Mystery|Romance|Sci-Fi|Thriller|War).*", "Common", d)
d
# may need to convert to factors?
data["commonGenre"]


dataSubSetByColumns <- data[,c(2,3,4,9,11,12,14,23,24,27)]
summary(dataSubSetByColumns)
dataSubSetByNumeric <- data[,c(3,4,9,14,24,27)]
pairs(dataSubSetByNumeric)
print(cor(dataSubSetByNumeric, use="complete.obs"))

#summary statistics for total data set, may want to get frequency tables in a nicer form
summary(data)
#check the number of factors in primary genre
factor(data$primary_genre)
factor(data$genre)
table(data$genre)
count(data$genre)


# create subset of first 10 movies for year 2016
tenMovies <- data[c(1:10),]

tenMovies
summary(tenMovies)
tenMoviesSubSet <- tenMovies[,c(2,3,4,9,11,12,14,23,24,27)]
tenMoviesSubSetOnlyNumeric <- tenMovies[,c(3,4,9,14,24,27)]
tenMoviesSubSetOnlyNumeric
summary(tenMoviesSubSetOnlyNumeric)
print(summary(tenMoviesSubSetOnlyNumeric))
# create subset for first 100 movies by year
oneHundredMovies <- data[c(1:100),]
# create subset for 1043 movies from the start of 2012 onwards
from2012 <- data[c(1:1043),]
# install.packages("leaps")
# library(leaps)
regressionTenMovies <- lm(gross~ duration + budget + imdb_score , data=tenMovies)
summary(regressionTenMovies)
regressionOneHundredMovies <- lm(gross~ duration + budget + imdb_score , data=oneHundredMovies)
summary(regressionOneHundredMovies)

regressionFrom2012One <- lm(gross~ duration + budget + imdb_score , data=from2012)
summary(regressionFrom2012One)
regressionFrom2012Two <- lm(log(gross)~duration + log(budget) + imdb_score + primary_genre
                            + content_rating, data = 
                              from2012)
summary(regressionFrom2012Two)

# create new vector with primary category only for genre

# count number of each category

countAction <- sum(str_count(data$primary_genre, "Action"))
countAdventure <- sum(str_count(data$primary_genre, "Adventure"))
countAnimation <- sum(str_count(data$primary_genre, "Animation"))
countBiography <- sum(str_count(data$primary_genre, "Biography"))
countComedy <- sum(str_count(data$primary_genre, "Comedy"))
countCrime <- sum(str_count(data$primary_genre, "Crime"))
countDocumentary <- sum(str_count(data$primary_genre, "Documentary"))
countDrama <- sum(str_count(data$primary_genre, "Drama"))
countFantasy <- sum(str_count(data$primary_genre, "Fantasy"))
countHorror <- sum(str_count(data$primary_genre, "Horror"))
countMystery <- sum(str_count(data$primary_genre, "Mystery"))
countSciFi <- sum(str_count(data$primary_genre, "Sci-Fi"))
countThriller <- sum(str_count(data$primary_genre, "Thriller"))
countWestern <- sum(str_count(data$primary_genre, "Western"))
countRomance <- sum(str_count(data$primary_genre, "Romance"))
countMusic <- sum(str_count(data$primary_genre, "Music"))
countMusical <- sum(str_count(data$primary_genre, "Musical"))
countHistory <- sum(str_count(data$primary_genre, "History"))
countWar <- sum(str_count(data$primary_genre, "War"))
sum(c(countAction,
countAdventure,
countAnimation,
countBiography,
countComedy,
countCrime,
countDocumentary,
countDrama,
countFantasy,
countHorror,
countMystery,
countSciFi,
countThriller, 
countWestern,
countRomance))

typeof(data$genre)
print(data$genre)
toString(data$genre)
print(data$primary_genre)

#plot with full data set from 1916
par(mfrow=c(3,3))
plot(newdata$duration, log(newdata$gross))
plot(log(newdata$budget), log(newdata$gross))
plot(newdata$imdb_score, log(newdata$gross))






