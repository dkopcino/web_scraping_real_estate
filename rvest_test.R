# CSS: + means the sibling immediately after
# CSS: > means all direct children
# CSS: ~ means all siblings after, not just the immediate one like in +

# https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/
install.packages('rvest')
library(rvest)

#Specifying the url for desired website to be scrapped
url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrap the rankings section
rank_data_html <- html_nodes(webpage, '.text-primary')

#Converting the ranking data to text
rank_data <- html_text(rank_data_html)

#Let's have a look at the rankings
head(rank_data)

#Data-Preprocessing: Converting rankings to numerical
rank_data <- as.numeric(rank_data)

#Let's have another look at the rankings
head(rank_data)

#Using CSS selectors to scrap the title section
title_data_html <- html_nodes(webpage, '.lister-item-header a')

#Converting the title data to text
title_data <- html_text(title_data_html)

#Let's have a look at the title
head(title_data)

#Using CSS selectors to scrap the description section
description_data_html <- html_nodes(webpage, '.ratings-bar + .text-muted')

#Converting the description data to text
description_data <- html_text(description_data_html)

#Let's have a look at the description data
head(description_data)

#Data-Preprocessing: removing '\n'
description_data <- gsub("\n", "", description_data)

#Let's have another look at the description data 
head(description_data)

#Using CSS selectors to scrap the Movie runtime section
runtime_data_html <- html_nodes(webpage, '.text-muted .runtime')

#Converting the runtime data to text
runtime_data <- html_text(runtime_data_html)

#Let's have a look at the runtime
head(runtime_data)

#Data-Preprocessing: removing mins and converting it to numerical
runtime_data <- gsub(" min", "", runtime_data)
runtime_data <- as.numeric(runtime_data)

#Let's have another look at the runtime data
head(runtime_data)

#Using CSS selectors to scrap the Movie genre section
genre_data_html <- html_nodes(webpage, '.genre')

#Converting the genre data to text
genre_data <- html_text(genre_data_html)

#Let's have a look at the runtime
head(genre_data)

#Data-Preprocessing: removing \n
genre_data <- gsub("\n", "", genre_data)

#Data-Preprocessing: removing excess spaces
genre_data <- gsub(" ", "", genre_data)

#taking only the first genre of each movie
genre_data<-gsub(",.*", "", genre_data)

#Converting each genre from text to factor
genre_data <- as.factor(genre_data)

#Let's have another look at the genre data
head(genre_data)

#Using CSS selectors to scrap the IMDB rating section
rating_data_html <- html_nodes(webpage, '.ratings-imdb-rating strong')

#Converting the ratings data to text
rating_data <- html_text(rating_data_html)

#Let's have a look at the ratings
head(rating_data)

#Data-Preprocessing: converting ratings to numerical
rating_data <- as.numeric(rating_data)

#Let's have another look at the ratings data
head(rating_data)

#Using CSS selectors to scrap the votes section
votes_data_html <- html_nodes(webpage, '.sort-num_votes-visible span:nth-child(2)')

#Converting the votes data to text
votes_data <- html_text(votes_data_html)

#Let's have a look at the votes data
head(votes_data)

#Data-Preprocessing: removing commas
votes_data <- gsub(",", "", votes_data)

#Data-Preprocessing: converting votes to numerical
votes_data <- as.numeric(votes_data)

#Let's have another look at the votes data
head(votes_data)

#Using CSS selectors to scrap the directors section
directors_data_html <- html_nodes(webpage, '.text-muted + p a:nth-child(1)')

#Converting the directors data to text
directors_data <- html_text(directors_data_html)

#Let's have a look at the directors data
head(directors_data)

#Data-Preprocessing: converting directors data into factors
directors_data <- as.factor(directors_data)

#Using CSS selectors to scrap the actors section
actors_data_html <- html_nodes(webpage, '.lister-item-content .ghost + a')

#Converting the gross actors data to text
actors_data <- html_text(actors_data_html)

#Let's have a look at the actors data
head(actors_data)

#Data-Preprocessing: converting actors data into factors
actors_data <- as.factor(actors_data)

#Using CSS selectors to scrap the metascore section
metascore_data_html <- html_nodes(webpage, '.metascore')

#Converting the runtime data to text
metascore_data <- html_text(metascore_data_html)

#Let's have a look at the metascore data
head(metascore_data)

#Data-Preprocessing: removing extra space in metascore
metascore_data <- gsub(" ", "", metascore_data)

#Lets check the length of metascore data
length(metascore_data)

#We have 100 movies and 98 metascores. It's because metascores are missing for 2 movies.
length(title_data)

#We will set NA for the missing metascores. We check (visually) that we're missing metascores for movies 56 and 66.
for (i in c(56, 66)) {
  a <- metascore_data[1:(i-1)]
  b <- metascore_data[i:length(metascore_data)]
  metascore_data <- append(a, list("NA"))
  metascore_data <- append(metascore_data, b)
}

#Data-Preprocessing: converting metascore to numerical
metascore_data <- as.numeric(metascore_data)

#Let's have another look at length of the metascore data
length(metascore_data)

#Let's look at summary statistics
summary(metascore_data)

#Using CSS selectors to scrap the gross revenue section
gross_data_html <- html_nodes(webpage, '.ghost ~ .text-muted + span')

#Converting the gross revenue data to text
gross_data <- html_text(gross_data_html)

#Let's have a look at the votes data
head(gross_data)

#Data-Preprocessing: removing '$' and 'M' signs
gross_data <- gsub("\\$", "", gross_data)
gross_data <- gsub("M", "", gross_data)
#gross_data <- substring(gross_data, 2, 7)

#Let's check the length of gross data
length(gross_data)

#Filling missing entries with NA
for (i in c(43, 56, 70, 96, 97, 99)) {
  a <- gross_data[1:(i-1)]
  b <- gross_data[i:length(gross_data)]
  gross_data <- append(a, list("NA"))
  gross_data <- append(gross_data, b)
}

#Data-Preprocessing: converting gross to numerical
gross_data <- as.numeric(gross_data)

#Let's have another look at the length of gross data
length(gross_data)

summary(gross_data)


#Combining all the lists to form a data frame
movies_df <- data.frame(Rank = rank_data,
                        Title = title_data,
                        Description = description_data,
                        Runtime = runtime_data,
                        Genre = genre_data,
                        Rating = rating_data,
                        Metascore = metascore_data,
                        Votes = votes_data,
                        Gross_Earning_in_Mil = gross_data,
                        Director = directors_data,
                        Actor = actors_data
                        )

#Structure of the data frame
str(movies_df)

# Analysis
library('ggplot2')
qplot(data = movies_df, Runtime, fill = Genre, bins = 30)
ggplot(movies_df, aes(x = Runtime, y = Rating)) + geom_point(aes(size = Votes, col = Genre))
ggplot(movies_df, aes(x = Runtime, y = Gross_Earning_in_Mil)) + geom_point(aes(size = Rating, col = Genre))



