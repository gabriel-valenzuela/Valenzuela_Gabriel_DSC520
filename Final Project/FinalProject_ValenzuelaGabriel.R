# Assignment: Final Project

# Name: Valenzuela, Gabriel

# Date: 12 02 2020

# Importing the data
marketplace <- read.csv("amazon_co-ecommerce_sample.csv")

# Removing uncessary columns
keeps <- c("product_name", "manufacturer", "price", "number_available_in_stock", "average_review_rating", 
           "amazon_category_and_sub_category","number_of_reviews")
needed_market <- marketplace[keeps]

# Clean price variable by removing extra characters and blanks then switched to numeric
needed_market$price <- gsub("^.{0,2}","",needed_market$price)
needed_market <- needed_market[!(needed_market$price==""),]
needed_market$price <- substr(needed_market$price,1,6)
# Extra Character cases
needed_market$price[1555] <- substr(needed_market$price[1555],1,4)
needed_market$price[2730] <- substr(needed_market$price[2730],1,4)
needed_market$price[2816] <- substr(needed_market$price[2816],1,4)

needed_market$price <- as.numeric(needed_market$price)

needed_market <- needed_market[!is.na((needed_market$price)),]

# Clean the avaiable toys varaiable by removing characters and replace empty values with a zero then switched to numeric
needed_market$number_available_in_stock <- stringr::str_remove_all(needed_market$number_available_in_stock,"Â new")
needed_market$number_available_in_stock <- sub("^$","0",needed_market$number_available_in_stock)
needed_market$number_available_in_stock <- as.numeric(needed_market$number_available_in_stock)

# Clean the review rating variable by removing extra characteres and replace empty values with a zero then switched to numeric
needed_market$average_review_rating <- stringr::str_remove_all(needed_market$average_review_rating," out of 5 stars")
needed_market$average_review_rating <- sub("^$","0",needed_market$average_review_rating)
needed_market$average_review_rating <- as.numeric(needed_market$average_review_rating)

# Clean the the number of reviews by replace empty values with a zero then switched to numeric
needed_market$number_of_reviews <- sub("^$","0",needed_market$number_of_reviews)
needed_market$number_of_reviews <- as.numeric(needed_market$number_of_reviews)

# Change product name variable from vector to characters
needed_market$product_name <- as.character(needed_market$product_name)


# Change manufacturer variable from vector to characters
#needed_market$manufacturer <- sub(" ","",needed_market$manufacturer)
needed_market$manufacturer <- as.character(needed_market$manufacturer)
needed_market$manufacturer <- stringr::str_remove_all(needed_market$manufacturer,"The")


####### ANALYSIS #########################################################



# Summar of the cleaned market
summary(needed_market)

# Pearson Correlation Tests
cor.test(needed_market$number_available_in_stock, needed_market$average_review_rating, method = "pearson")

cor.test(needed_market$number_available_in_stock, needed_market$price, method = "pearson")

cor.test(needed_market$price, needed_market$average_review_rating, method = "pearson")

# Prices of Toys and Reviews
plot(needed_market$average_review_rating, needed_market$price, main = "Ratings VS. Price", 
     xlab = "Average Rating",ylab = "Price of Toys")

#Available Toys and Price
plot(needed_market$number_available_in_stock, needed_market$price, main = "Available VS. Price", 
     xlab = "Products Available",ylab = "Price of Toys")

#Available Toys and Rating
plot(needed_market$number_available_in_stock, needed_market$average_review_rating, main = "Ratings VS. Available", 
     xlab = "Products Available",ylab = "Average Rating")


             
#### Top Manufaturers ####

# Word Cloud 
text <- needed_market$manufacturer
docs <- tm::Corpus(tm::VectorSource(text))

dtm <- tm::TermDocumentMatrix(docs)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix),decreasing = TRUE)
df <- data.frame(word = names(words),freq=words)

set.seed(1234)

wordcloud2::wordcloud2(df, size = 0.3, shape='star')


# Word "toy" association 
tm::findAssocs(dtm, terms = "toy",corlimit=0.0)

# Top 10 Manufacturers
topManufacturers <- df[1:10,]$word
topManufacturers[] <- lapply(topManufacturers, as.character)

barplot(df[1:10,]$freq, las = 2, names.arg = df[1:10,]$word,
        col ="lightblue", main ="Top Manufacturer Names Frequencies", xlab = "Words",
        ylab = "Word frequencies")

library(dplyr)
number_manufacturers <- plyr::count(needed_market, "manufacturer") %>% arrange(desc(freq))
top10_manufacturers <- number_manufacturers$manufacturer[1:10]


top10_market <- needed_market[needed_market$manufacturer == c("Oxford Diecast","LEGO","Disney","Playmobil",
                                                              " Puppet Company","MyTinyWorld","Star Wars","Mattel",
                                                              "Hasbro","Corgi"),]

#Top 10 Manufacturers and the mean prices of their products

top10_market %>%
        dplyr::group_by(manufacturer) %>%
        dplyr::summarise(price=mean(price))


ggplot2::ggplot(top10_market,ggplot2::aes(x=average_review_rating,y=number_of_reviews,fill = average_review_rating)) +
        ggplot2::geom_bar(position = "dodge",stat="identity") + 
        ggplot2::ggtitle("Manufacturer Ratings") +
        ggplot2::facet_wrap(~manufacturer) +
        ggplot2::theme_classic() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::xlab("")



# Top Toys
text_toy <- needed_market$product_name
docs_toy <- tm::Corpus(tm::VectorSource(text_toy))
toSpace <- tm::content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs_toy <- tm::tm_map(docs_toy, toSpace, "/")
docs_toy <- tm::tm_map(docs_toy, toSpace, "@")
docs_toy <- tm::tm_map(docs_toy, toSpace, "\\|")
docs_toy <- tm::tm_map(docs_toy, tm::content_transformer(tolower))
docs_toy <- tm::tm_map(docs_toy, tm::removeNumbers)
docs_toy <- tm::tm_map(docs_toy, tm::removeWords, c("the", "and","with","for")) 
docs_toy <- tm::tm_map(docs_toy,tm::removePunctuation)
docs_toy <- tm::tm_map(docs_toy,tm::stripWhitespace)
docs_toy <- tm::tm_map(docs_toy,tm::removeWords,tm::stopwords("english"))
dtm_toy <- tm::TermDocumentMatrix(docs_toy)
matrix_toy <- as.matrix(dtm_toy)
words_toy <- sort(rowSums(matrix_toy),decreasing = TRUE)
df_toy <- data.frame(word = names(words_toy),freq=words_toy)


tm::findAssocs(dtm_toy, terms = "toy",corlimit=0.10)

head(df_toy,10)

barplot(df_toy[1:10,]$freq, las = 2, names.arg = df_toy[1:10,]$word,
        col ="orange", main ="Top Toy Names Frequencies",xlab = "Words",
        ylab = "Word frequencies")


df_toy$word <- as.character(df_toy$word)



#aggregate(price ~ product_name + manufacturer,data=needed_market,mean)







