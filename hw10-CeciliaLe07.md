Homework 10: Getting data from the Web
================
Cecilia Leon

-   [Scrape data](#scrape-data)
-   [Make API queries](#make-api-queries)

All dependencies neddeed for this assingment:

``` r
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(xlsx))
suppressPackageStartupMessages(library(kableExtra))
```

Scrape data
-----------

Use the `rvest` package to scrape data from the web to make two data objects.

Requirements:

-   At least one of your data objects should be a data frame that contains at least 2 rows and 2 columns, and this should not be obtained from activities done in class.

-   You should do some CSS selection (use html\_nodes() or html\_node()) in at least one case.

To select a table about some characteristics of the characters on "The Simpsons" serie, we can use the command `html_table` a take advantage of the information provided by wikipedia. For example:

``` r
Simpson_characters <- read_html("https://en.wikipedia.org/wiki/List_of_The_Simpsons_characters") %>% 
  html_table(fill=TRUE)
```

After that, we can show only few rows of the obtained object, which also will be loaded in a xlsx file by the following code:

``` r
#Printing the first 5 entries of the table
Simpson_characters[[1]] %>% 
  head(5)
```

    ##        Character
    ## 1  Homer Simpson
    ## 2  Marge Simpson
    ## 3   Bart Simpson
    ## 4   Lisa Simpson
    ## 5 Maggie Simpson
    ##                                                                                                        Voice actor(s)
    ## 1                                                                                                 Dan Castellaneta[2]
    ## 2                                                                                                    Julie Kavner [2]
    ## 3                                                                                                 Nancy Cartwright[2]
    ## 4                                                                                                   Yeardley Smith[2]
    ## 5 Liz Georges Gábor Csupó Harry Shearer Yeardley SmithNancy Cartwright Elizabeth Taylor James Earl Jones Jodie Foster
    ##                                                                  Character's role
    ## 1                             Husband of Marge; father of Bart, Lisa, and Maggie.
    ## 2                                Wife of Homer; mother of Bart, Lisa, and Maggie.
    ## 3       Oldest child and only son of Homer and Marge; brother of Lisa and Maggie.
    ## 4 Middle child and eldest daughter of Homer and Marge; sister of Bart and Maggie.
    ## 5        Youngest child and daughter of Homer and Marge; sister of Bart and Lisa.
    ##                          Episode debut Original air date
    ## 1    "Good Night (The Simpsons short)"        1987-04-19
    ## 2     "Good Night (The Simpsons short)        1987-04-19
    ## 3 "Good Night (The Simpsons short)"[3]        1987-04-19
    ## 4 "Good Night (The Simpsons short)"[3]        1987-04-19
    ## 5 "Good Night (The Simpsons short)"[3]        1987-04-19

``` r
write.xlsx(Simpson_characters[[1]],"Simpson_characters.xlsx")
```

Furthermore, we can construct a data frame when scraped data is not in table format. For instance, we can construct a table with the summary of the best 5 episodes of simpsons presented at [these web page](https://www.pastemagazine.com/articles/2014/05/the-top-25-simpsons-episodes-of-all-time.html)

``` r
titles <- read_html("https://www.pastemagazine.com/articles/2014/05/the-top-25-simpsons-episodes-of-all-time.html") %>% 
  html_nodes(".big") %>% 
  html_text()

to_search <- paste0("p:nth-child(",c(seq(3,13,by=2),
                                    seq(16,26,by=2),
                                    seq(29,41,by=2),
                                    seq(44,52,by=2),55),")")

description <- read_html("https://www.pastemagazine.com/articles/2014/05/the-top-25-simpsons-episodes-of-all-time.html") %>% 
  html_nodes(toString(to_search)) %>% 
  html_text()
  
summary_table <- tibble(title=rev(titles),
                        description=rev(description))
```

Again, we can show only few rows of the data frame we built, which also will be loaded in a xlsx file by the following code:

``` r
summary_table %>% 
  head(5)
```

    ## # A tibble: 5 x 2
    ##   title                     description                                   
    ##   <chr>                     <chr>                                         
    ## 1 1. The Itchy & Scratchy ~ Okay, this episode may be a bit too meta for ~
    ## 2 2. Homie the Clown        Again, for folks who like emotion, or family ~
    ## 3 3. Bart Gets an Elephant  Speaking of silly, there’s not a lot of emoti~
    ## 4 4. Homer Goes to College  You will learn a valuable lesson watching thi~
    ## 5 5. Bart’s Inner Child     George Meyer may be a comedy writing legend, ~

``` r
write.xlsx(Simpson_characters[[1]],"Simpson_characters.xlsx")
```

Finally, we are going to make two different scraping to obtain "real-time" data. The first one is to obatin the most recent news published by **New York times** regarding three different fields: `world`, `politics` and `business`. To do it I created the following function, which input could be any of the preciuos mentioned fiels, being `world` the default value.

``` r
last_news <- function(field="world"){
  
  if(field=="world"){
    news <- read_html("https://www.nytimes.com/section/world") %>% 
            html_nodes(".headline a") %>% 
            html_text()
  }else{
    last_url <- paste0("https://www.nytimes.com/section/",field)
    news <- read_html(last_url) %>% 
            html_nodes(".e1xfvim30") %>% 
            html_text()
  }
  
  return(kable(news,col.names = paste0("Last news about ", field, ":")))
}
```

To illustrate the usege and output of this function:

``` r
last_news("business")
```

<table>
<thead>
<tr>
<th style="text-align:left;">
Last news about business:
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
With the Economy Uncertain, Tech ‘Unicorns’ Rush Toward I.P.O.
</td>
</tr>
<tr>
<td style="text-align:left;">
At ‘60 Minutes,’ Independence Led to Trouble, Investigators Say
</td>
</tr>
<tr>
<td style="text-align:left;">
For-Profit College Chain Closes, Shutting Out Nearly 20,000 Students
</td>
</tr>
<tr>
<td style="text-align:left;">
Why Trump Might Be Right About Interest Rates
</td>
</tr>
<tr>
<td style="text-align:left;">
‘Yellow Vest’ Protests Shook France. Here’s the Lesson for Climate Change.
</td>
</tr>
<tr>
<td style="text-align:left;">
Why Is Obamacare Enrollment Down?
</td>
</tr>
<tr>
<td style="text-align:left;">
Tumblr Fans Abandon Ship as Tumblr Bans Porn
</td>
</tr>
<tr>
<td style="text-align:left;">
Ex-Tesco Executives Cleared on Fraud and False Accounting Charges
</td>
</tr>
<tr>
<td style="text-align:left;">
What Are Britain’s ‘Golden Visas’ and Why Are They Being Suspended?
</td>
</tr>
<tr>
<td style="text-align:left;">
A New Editor, and a New Start, for Troubled Billboard Magazine
</td>
</tr>
</tbody>
</table>
**Note:** If the css style change in [the New York times page](https://www.nytimes.com) this function could not work properly.

Other example is given bu the following function which provides the current weather of Vancouver at the moment is called, this function doesn't need any parameter to be passed.

``` r
vancouver_weather <- function(){
  current_temp <- read_html("https://www.accuweather.com/en/ca/vancouver/v6c/hourly-weather-forecast/53286") %>% 
    html_nodes(".local-temp") %>% 
    html_text()
  
  return(paste("The current temperature at Vancouver is:",current_temp))
}
```

To illustrate the usage and output of this function:

``` r
vancouver_weather()
```

    ## [1] "The current temperature at Vancouver is: 2°C"

Make API queries
----------------

Make two requests for data two make two data objects.

Requirements:

-   At least one of your data objects should be a data frame that contains at least 2 rows and 2 columns, and this should not be obtained from the GitHub API or the OMDb API that we used in class.
-   Use the httr package to do the retrieval (or even the RCurl package if you’d like).
-   Don’t use R packages that are specifically designed to wrap a specific API (such as rebird or geonames, as listed in the cm112 notes).
    -   You can use these, but not until you’ve completed the original task.
    -   You can use the API’s associated with these, though.
