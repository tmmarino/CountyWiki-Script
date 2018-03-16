#install.packages("rvest")
library(rvest)
library(dplyr)

url <- "https://en.wikipedia.org/wiki/List_of_United_States_counties_and_county_equivalents"
counties <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>% 
  html_table()

fips <- counties[[1]]$INCITS

links <- url %>% 
  read_html() %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>% 
  html_nodes("td:nth-child(2) a") %>%
  html_attr("href")

citationLinks=grep(pattern = "#cite_",x=links,value=F)
links <- links[-citationLinks]

countyLinks <- tibble(fips, links)

##Creating matrix for links between county wiki pages
numCounties <- nrow(countyLinks)
graphMatrix <- matrix(as.vector(rep(0, numCounties)),nrow= numCounties, ncol=numCounties)
rownames(graphMatrix) <- as.character(countyLinks$fips)
colnames(graphMatrix) <- as.character(countyLinks$fips)

##Loop over all 3142 county wiki pages
for(i in 1:nrow(countyLinks)){
  #Just a counter for checking progress
  print(i)
  
  nextUrl <- paste("https://en.wikipedia.org", countyLinks[i,2], sep = "")
  
  allLinks <- nextUrl %>% 
    read_html() %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  ##Removing links that don't start with the /wiki/
  allLinks <- allLinks[grepl("^/wiki/", allLinks)]
  
  
  ##This part is definitely not optimal
  ##Looping over all links on the specific county page
  for(link in allLinks){
    possibleIndex = which(countyLinks$links == link)
    if(length(possibleIndex) > 0){
      graphMatrix[i, possibleIndex] <- 1
    }
  }
}

  
##The graph data is stored in graphMatrix.
##If you want to see if a specific county has a link to another county,
## you can index by fips code (as character) or by the original indexes (1 to 3142)

## example: Does Weston County, Wyoming have link to Pennington County,South Dakota?
## By fips code: graphMatrix["56045","46103"]
## By original index: graphMatrix[3142,2414]
  
  
