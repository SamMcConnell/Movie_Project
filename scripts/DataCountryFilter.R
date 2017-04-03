# IN ORDER TO USE THIS SORTING FUNCTION strings must not be read as factors
# stringAsFactors = FALSE must be the case when the csv is read into R
dat = read.csv("data/moviedata_sortedby_year.csv", stringsAsFactors = FALSE)

# Use these lines to filter out by country and language
dat<-dat[(dat$country=="USA"| dat$country=="UK" | dat$country=="Canada" | 
            dat$country=="New Zealand" | dat$country=="Australia" | dat$country=="Ireland" |
            (dat$country=="Germany" & dat$language=="English") | (dat$country=="France" & dat$language=="English") |
            (dat$country=="Italy" & dat$language=="English") | (dat$country=="Spain" & dat$language=="English")),]