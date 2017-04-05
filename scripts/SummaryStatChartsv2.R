# IN ORDER TO USE THIS SORTING FUNCTION strings must not be read as factors
# stringAsFactors = FALSE must be the case when the csv is read into R
dat = read.csv("data/moviedata_sortedby_year.csv", stringsAsFactors = FALSE)

# Use these lines to filter out by country and language
dat<-dat[(dat$country=="USA"| dat$country=="UK" | dat$country=="Canada" | 
             dat$country=="New Zealand" | dat$country=="Australia" | dat$country=="Ireland" |
             (dat$country=="Germany" & dat$language=="English") | (dat$country=="France" & dat$language=="English") |
             (dat$country=="Italy" & dat$language=="English") | (dat$country=="Spain" & dat$language=="English")),]

oneMill = 1000000;

gsub("GP", "PG", dat$content_rating)


dat$lngross <- log(dat$gross)
dat$lnnum_critic_reviews <- log(dat$num_critic_for_reviews)
dat$lnnum_voted_users <- log(dat$num_voted_users)
dat$gsubcontentrating <- gsub("GP", "PG", dat$content_rating)
tablewithoutblankss <- dat$gsubcontentrating[dat$gsubcontentrating != ""]
table(tablewithoutblanks)

# make plots
# par(mfrow=c(2,2))

# par(mfrow=c(4,3))

# num_voted_users vs gross
par(mfrow=c(1,3))
dat$lnnum_voted_users <- log(dat$num_voted_users)
plot(dat$num_voted_users, dat$gross/oneMill, xlab = "Number of User Votes", ylab = "Gross Revenue, USD (millions)")
plot(dat$num_voted_users, log(dat$gross), xlab = "Number of User Votes", ylab = "log(Gross Revenue, USD)")
plot(log(dat$num_voted_users), log(dat$gross), xlab = "log(Number of User Votes)", ylab = "log(Gross Revenue, USD)")

# num_critic_reviews vs gross
par(mfrow=c(1,3))
plot(dat$num_critic_for_reviews, dat$gross/oneMill, xlab = "Number of Critic Reviews", ylab = "Gross Revenue, USD (millions)")
plot(dat$num_critic_for_reviews, log(dat$gross), xlab = "Number of Critic Reviews", ylab = "log(Gross Revenue, USD)")
plot(log(dat$num_critic_for_reviews), log(dat$gross), xlab = "log(Number of Critic Reviews)", ylab = "log(Gross Revnue, USD)")

# duration vs gross
par(mfrow=c(1,2))
plot(dat$duration, log(dat$gross), xlab = "Duration (Minutes)", ylab = "log(Gross)")
plot(log(dat$duration), log(dat$gross), xlab = "log(Duration)", ylab = "log(Gross)")

# budget vs gross
par(mfrow=c(1,3))
plot(dat$budget/oneMill, dat$gross/oneMill, xlab = "Budget, USD (Millions)", ylab = "Gross Revenue, USD (Millions)")
plot(dat$budget/oneMill, log(dat$gross), xlab = "Budget USD (Millions)", ylab = "log(Gross Revenue)")
plot(log(dat$budget), log(dat$gross), xlab = "log(Budget)", ylab = "log(Gross Revenue)")

# imdb_score vs gross
par(mfrow=c(1,2))
plot(dat$imdb_score, dat$gross/oneMill, xlab = "Imdb Score", ylab = "Gross Revenue, USD (Millions)")
plot(dat$imdb_score, log(dat$gross), xlab = "Imdb Score", ylab = "log(Gross Revenue")

# Content Rating vs lngross
par(mfrow=c(1,1))
boxplot(dat$lngross ~ dat$gsubcontentrating, xlab = "Content Rating", ylab = "ln(Gross Revenue, USD)")

#substet data frame for num_critic_reviews, duration, gross, num_voted_users, num_voted_users budget, imdb_score
dataSubSetByColumns <- dat[,c(3,4,9,13,19,23,26,29,30,31)]
otherdataSubSetByColumns <- dat[,c(3,4,5,6,8,9,13,14,16,19,23,24,25,26,27,28,29,30,31)]
otherdataSubSetByColumns
summary(otherdataSubSetByColumns)
dataSubSetByColumns
summary(dataSubSetByColumns)
dat
# print out summary

#substet data frame for num_critic_reviews, duration, gross, num_voted_users, budget, imdb_score
datNumeric <- dat[,c(3,4,9,14,24)]
datNumeric
sumNumeric <- summary(datNumeric)
sumNumeric

#produces correlation matrix for all numerical data items that contain all items
table(dat$gsubcontentrating)
print(cor(dataSubSetByColumns, use="complete.obs"))
print(cor(dat, use="complete.obs"))
print(cor(otherdataSubSetByColumns, use="complete.obs"))



#summary statistics for total data set, may want to get frequency tables in a nicer form
summary(dat)
#check the number of factors in a vector
# factor(obj)
# count(obj) counts numers of specific items
# table(obj) presents data in a vector in table form


