library(tidyverse)
library(stringr)
library(rebus)
library(lubridate)
library(rvest)

URL <-'https://www.trustpilot.com/review/redcrossfirstaidtraining.co.uk'

firstpage <- read_html('https://www.trustpilot.com/review/redcrossfirstaidtraining.co.uk')
 (LatestPageNumber <- GetLastPage(FirstPage))

GetLastPage <- function(html){ 
      
      PagesData <- html %>%
            html_nodes('.pagination-page') %>%
            html_text()
      
      PagesData[(length(PagesData)-1)] %>%
            unname() %>%
            as.numeric()
      
}
FirstPage <- firstpage

LatestPageNumber <- GetLastPage(FirstPage)

ListOfPages <- str_c(URL, '?page=', (1:LatestPageNumber))

GetReviews <- function(html){
      
      ScrapedReviews <- (html %>% 
                               html_nodes('.review-info__body__text') %>%
                               html_text() %>%
                               str_trim() %>%
                               unlist())
      
}

GetNames <- function(html){
      
      ScrapedNames <- (html %>% 
                             html_nodes('.consumer-info__details__name') %>%
                             html_text() %>%
                             str_trim() %>%
                             unlist())
      print(ScrapedNames)     
      
}    

GetReviewDates <- function(html){
      
      status <- html %>% 
            html_nodes('time') %>%
            html_attrs() %>%
            map(2) %>%
            unlist()
      
      dates <- html %>%
            html_nodes('time') %>%
            html_attrs() %>%
            map(1) %>%
            ymd_hms() %>%
            unlist()
      
      ReturnDates <- tibble(status = status, dates= dates) %>%
            filter(status == 'ndate') %>% 
            pull(dates) %>%
            as.POSIXct(origin = '1970-01-01 00:00:00')
      
      length_reviews <- length(GetReviews(html))
      
      return_reviews <- if(length(ReturnDates) > length_reviews){
            ReturnDates[1:length_reviews]
      } else{
            ReturnDates
      }
      return_reviews
}

GetStarRating <- function(html){
      
      pattern = 'rating-'%R% capture(DIGIT)
      
      Ratings <- html %>% 
            html_nodes('.star-rating') %>%
            html_attrs() %>%
            map(str_match, pattern = pattern) %>%
            map(2) %>%
            unlist()
      tail(-2)
      
      Rating <- tail(Ratings, -2)
      
      print(Rating)
      
}

GetDataTable <- function(html, company){
      
     Combined <- tibble( Reviews = GetReviews(html),
                        Dates = GetReviewDates(html),
                         Names = GetNames(html),
                        Stars = GetStarRating(html),
                        )
      
     Combined <<- Combined %>%
           mutate(Company = company) %>%
           select(Company, Dates, Names, Stars, Reviews)
           
}

GetDataFromURL <- function(URL, CompanyName){
      
      html <- read_html(URL)
      GetDataTable(html, CompanyName)
}


      


ScrapeWriteTable <- function(URL, CompanyName){
      
      FirstPage<- read_html(URL)
      LastPageNumber <- GetLastPage(FirstPage)
      ListOfPages <- str_c(URL, '?page=', (1:LatestPageNumber))
      
      ListOfPages %>% 
            map(GetDataFromURL, CompanyName) %>%
            bind_rows() %>%
            write_tsv(str_c(CompanyName,'.tsv'))
      
}

RedCross <- read_tsv("Red Cross.tsv")
