Homework 10: Getting data from the Web
================
Cecilia Leon

-   [Scrape data](#scrape-data)
-   [Make API queries](#make-api-queries)

All dependencies nedeed for this assignment:

``` r
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(xlsx))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(ggplot2))
```

Scrape data
-----------

> Use the `rvest` package to scrape data from the web to make two data objects.

> Requirements:

> -   At least one of your data objects should be a data frame that contains at least 2 rows and 2 columns, and this should not be obtained from activities done in class.

> -   You should do some CSS selection (use html\_nodes() or html\_node()) in at least one case.

To select a table about some characteristics of the characters on "The Simpsons" serie, we can use the command `html_table` a take advantage of the information provided by [wikipedia](https://en.wikipedia.org/wiki/List_of_The_Simpsons_characters). For example:

``` r
Simpson_characters <- read_html("https://en.wikipedia.org/wiki/List_of_The_Simpsons_characters") %>% 
  html_table(fill=TRUE)
```

After that, we can show only a few rows of the obtained object, which also will be saved in an xlsx file by the following code:

``` r
#Printing the first 5 entries of the table
Simpson_characters[[1]] %>% 
  head(3)
```

    ##       Character      Voice actor(s)
    ## 1 Homer Simpson Dan Castellaneta[2]
    ## 2 Marge Simpson    Julie Kavner [2]
    ## 3  Bart Simpson Nancy Cartwright[2]
    ##                                                            Character's role
    ## 1                       Husband of Marge; father of Bart, Lisa, and Maggie.
    ## 2                          Wife of Homer; mother of Bart, Lisa, and Maggie.
    ## 3 Oldest child and only son of Homer and Marge; brother of Lisa and Maggie.
    ##                          Episode debut Original air date
    ## 1    "Good Night (The Simpsons short)"        1987-04-19
    ## 2     "Good Night (The Simpsons short)        1987-04-19
    ## 3 "Good Night (The Simpsons short)"[3]        1987-04-19

``` r
write.xlsx(Simpson_characters[[1]],"Simpson_characters.xlsx")
```

Furthermore, we can construct a data frame when scraped data is not in table format. For instance, we can construct a table with the summary of the best 25 episodes of Simpsons presented at [these web page](https://www.pastemagazine.com/articles/2014/05/the-top-25-simpsons-episodes-of-all-time.html)

``` r
titles <- read_html("https://www.pastemagazine.com/articles/2014/05/the-top-25-simpsons-episodes-of-all-time.html") %>% 
  html_nodes(".big") %>% 
  html_text()

to_search <- paste0("p:nth-child(",c(seq(3,13,by=2),
                                     seq(16,26,by=2),
                                     seq(29,41,by=2),
                                     seq(44,52,by=2),
                                     55),
                    ")")

description <- read_html("https://www.pastemagazine.com/articles/2014/05/the-top-25-simpsons-episodes-of-all-time.html") %>% 
  html_nodes(toString(to_search)) %>% 
  html_text()

#To eliminate the numbers in titles of chapter I used "gsub"
summary_table <- tibble(title=rev(gsub('[0-9]+. ', '', titles)),
                        description=rev(description))
```

Again, we can show only a few rows of the data frame we built, which also will be loaded in an xlsx file by the following code:

``` r
summary_table %>% 
  head(5)
```

    ## # A tibble: 5 x 2
    ##   title                   description                                     
    ##   <chr>                   <chr>                                           
    ## 1 The Itchy & Scratchy &~ Okay, this episode may be a bit too meta for so~
    ## 2 Homie the Clown         Again, for folks who like emotion, or family bo~
    ## 3 Bart Gets an Elephant   Speaking of silly, there’s not a lot of emotion~
    ## 4 Homer Goes to College   You will learn a valuable lesson watching this ~
    ## 5 Bart’s Inner Child      George Meyer may be a comedy writing legend, bu~

``` r
write.xlsx(summary_table[[1]],"Simpson_best_chapters.xlsx")
```

Finally, we are going to make two different scraping to obtain "real-time" data. The first one is to obtain the most recent news published by **New York times** regarding three different fields: `world`, `politics` and `business`. To do it I created the following function, which input could be any of the previously mentioned fields, being `world` the default value.

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

To illustrate the usage and output of this function:

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
For-Profit College Chain Closes, Shutting Out Nearly 20,000 Students
</td>
</tr>
<tr>
<td style="text-align:left;">
America’s Tariff Men: Connecting McKinley to Trump
</td>
</tr>
<tr>
<td style="text-align:left;">
Why Trump Might Be Right About Interest Rates
</td>
</tr>
<tr>
<td style="text-align:left;">
‘Yellow Vest’ Protests Shake France. Here’s the Lesson for Climate Change.
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
Lyft, Racing Uber Toward I.P.O., Takes a Crucial Step
</td>
</tr>
<tr>
<td style="text-align:left;">
A New Editor, and a New Start, for Troubled Billboard Magazine
</td>
</tr>
</tbody>
</table>
**Note:** If the css style change in [the New York times page](https://www.nytimes.com) this function won't work properly.

Another example is given by the following function which provides the current weather of Vancouver at the moment is called, this function doesn't need any parameter to be passed.

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

    ## [1] "The current temperature at Vancouver is: 0°C"

Make API queries
----------------

> Make two requests for data two make two data objects.

> Requirements:

> -   At least one of your data objects should be a data frame that contains at least 2 rows and 2 columns, and this should not be obtained from the GitHub API or the OMDb API that we used in class.
> -   Use the httr package to do the retrieval (or even the RCurl package if you’d like).
> -   Don’t use R packages that are specifically designed to wrap a specific API (such as rebird or geonames, as listed in the cm112 notes).
> -   You can use these, but not until you’ve completed the original task.
> -   You can use the API’s associated with these, though.

For this exercises, I'm going to use the API provided by [potterapi](https://www.potterapi.com/) which contains information about different characters, schools, and spells of Harry Potter saga. This site requires you to generate an API key, which won't be published on this file.

The following request doesn't work unless an API key is provided, because of that, I provide a csv file with the content I obtained using my API key:

``` r
#To run this code, replace "myapikey" with a valid API key
my_apy_key <- "Insert your API key here"
query_students <- paste0("https://www.potterapi.com/v1/characters/?key=",
                         my_apy_key,
                         "&house=Gryffindor")

Gryffindor_students <- fromJSON(query_students)
write.csv(Gryffindor_students,"Gryffindor_students.csv")
```

Result was a data frame of 41 observations and 17 variables that was loaded in the file `Gryffindor_students.csv`. In order to show this data, I'm going to display part of this data frame:

``` r
read_Gryffindor_students <- read.csv("Gryffindor_students.csv")

read_Gryffindor_students %>% 
  str()
```

    ## 'data.frame':    41 obs. of  18 variables:
    ##  $ X                : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ X_id             : Factor w/ 41 levels "5a0fa648ae5bc100213c2332",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ name             : Factor w/ 41 levels "Aberforth Dumbledore",..: 23 11 40 24 10 14 1 2 39 21 ...
    ##  $ role             : Factor w/ 16 levels "Auror (formerly)",..: 15 13 NA 15 16 16 11 10 15 15 ...
    ##  $ house            : Factor w/ 1 level "Gryffindor": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ school           : Factor w/ 1 level "Hogwarts School of Witchcraft and Wizardry": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ boggart          : Factor w/ 12 levels "Aragog","Ariana (sister)",..: 8 NA NA 10 NA NA NA 2 3 6 ...
    ##  $ X__v             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ministryOfMagic  : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ orderOfThePhoenix: logi  FALSE FALSE TRUE FALSE FALSE FALSE ...
    ##  $ dumbledoresArmy  : logi  FALSE FALSE FALSE TRUE TRUE TRUE ...
    ##  $ deathEater       : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ bloodStatus      : Factor w/ 4 levels "half-blood","muggle-born",..: 3 4 3 3 2 2 1 1 1 2 ...
    ##  $ species          : Factor w/ 4 levels "ghost","half-giant",..: 3 1 3 3 3 3 3 3 3 3 ...
    ##  $ alias            : Factor w/ 12 levels "Ab","Bill","Charlie",..: NA NA 7 NA NA NA 1 NA NA NA ...
    ##  $ animagus         : Factor w/ 4 levels "black dog","rat",..: NA NA 1 NA NA NA NA NA NA NA ...
    ##  $ patronus         : Factor w/ 10 levels "cat","doe","goat",..: NA NA NA NA NA NA 3 7 NA 6 ...
    ##  $ wand             : Factor w/ 12 levels "Ash, 12\", unicorn hair tail",..: NA NA NA NA NA NA NA 5 NA 10 ...

``` r
read_Gryffindor_students %>% 
  head(3)
```

    ##   X                     X_id           name                        role
    ## 1 1 5a0fa648ae5bc100213c2332     Katie Bell                     student
    ## 2 2 5a0fa67dae5bc100213c2333 Cuthbert Binns Professor, History of Magic
    ## 3 3 5a0fa7dcae5bc100213c2338   Sirius Black                        <NA>
    ##        house                                     school        boggart
    ## 1 Gryffindor Hogwarts School of Witchcraft and Wizardry Lord Voldemort
    ## 2 Gryffindor Hogwarts School of Witchcraft and Wizardry           <NA>
    ## 3 Gryffindor Hogwarts School of Witchcraft and Wizardry           <NA>
    ##   X__v ministryOfMagic orderOfThePhoenix dumbledoresArmy deathEater
    ## 1    0           FALSE             FALSE           FALSE      FALSE
    ## 2    0           FALSE             FALSE           FALSE      FALSE
    ## 3    0           FALSE              TRUE           FALSE      FALSE
    ##   bloodStatus species   alias  animagus patronus wand
    ## 1  pure-blood   human    <NA>      <NA>     <NA> <NA>
    ## 2     unknown   ghost    <NA>      <NA>     <NA> <NA>
    ## 3  pure-blood   human Padfoot black dog     <NA> <NA>

We can also make some plots with information provided by this API:

``` r
read_Gryffindor_students %>% 
  ggplot(aes(bloodStatus,1,fill=bloodStatus)) +
  geom_bar(stat = "identity") +
  ggtitle("Counts by blood Status on Gryffindor") +
  ylab("Counts")
```

![](hw10-CeciliaLe07_files/figure-markdown_github/showing%20graph%20API%201-1.png)

We can use the package `httr` to make a **GET** call and obtain the spells provided by this API

``` r
query_spells <- paste0("https://www.potterapi.com/v1/spells/?key=",
                       my_apy_key)
  
spells <- GET(query_spells)
content(spells)
```

The data was obtained in `list` format, thus is going to be converted to a data frame and saved as csv file in the following lines:

``` r
spells_df <- data.frame(id =sapply(content(spells),function(i){i[[1]]}),
                        spell=sapply(content(spells),function(i){i[[2]]}),
                        type=sapply(content(spells),function(i){i[[3]]}),
                        effect=sapply(content(spells),function(i){i[[4]]}),
                        row.names = NULL )
write.csv(spells_df,"spells.csv")
```

Some of these results are displayed to show the obtained data:

``` r
read_spells <- read.csv("spells.csv") 

read_spells %>% 
  head(8)
```

    ##   X                       id            spell        type
    ## 1 1 5b74ebd5fb6fc0739646754c           Aberto       Charm
    ## 2 2 5b74ecfa3228320021ab622b            Accio       Charm
    ## 3 3 5b74ed2f3228320021ab622c         Age Line Enchantment
    ## 4 4 5b74ed453228320021ab622d        Aguamenti       Charm
    ## 5 5 5b74ed583228320021ab622e Alarte Ascendare       Spell
    ## 6 6 5b74ed6d3228320021ab622f        Alohomora       Charm
    ## 7 7 5b74ed823228320021ab6230          Anapneo       Spell
    ## 8 8 5b74ed933228320021ab6231     Anteoculatia         Hex
    ##                             effect
    ## 1                    opens objects
    ## 2                Summons an object
    ## 3 Hides things from younger people
    ## 4           shoots water from wand
    ## 5    shoots things high in the air
    ## 6             opens locked objects
    ## 7       clears the target's airway
    ## 8     turns head hair into antlers

Furthermore, it is possible to show this data in JSON format, which is the original format we obtained using the command `toJson`

``` r
read_spells[,-1] %>% 
  head(3) %>% 
  toJSON()
```

    ## [{"id":"5b74ebd5fb6fc0739646754c","spell":"Aberto","type":"Charm","effect":"opens objects"},{"id":"5b74ecfa3228320021ab622b","spell":"Accio","type":"Charm","effect":"Summons an object"},{"id":"5b74ed2f3228320021ab622c","spell":"Age Line","type":"Enchantment","effect":"Hides things from younger people"}]
