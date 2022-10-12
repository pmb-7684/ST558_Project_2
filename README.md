ST 558 - Project 2
================
Yi Ren and Paula Bailey
2022-09-22

-   This document will walk the user through the process of connecting
    to & collecting data from the [pokemon API](https://pokeapi.co/)
    within R.

## Reference to Render Code

``` r
rmarkdown::render("Project2.Rmd", "github_document","README.md")
```

## Packages for Vignette

-   The following packages are required for connecting and retrieving
    data from the API.

1.  `httr` - Retrieves Data from an API
2.  `jsonlite` - Parses Results from an API Query
3.  `dplyr` - A part of the `tidyverse` used for manipulating data
4.  `tidyr` - A part of the `tidyverse` used for data cleaning
5.  `ggplot2` - A part of the `tidyverse` used for creating graphics

``` r
# Read in Required Packages 
library(httr)
library(jsonlite)
library(tidyverse)
library(ggplot2)
```

## 6. Function for moves information

``` r
get_moves <- function(type){
  base<-"https://pokeapi.co/api/v2/move/"
  url<-(paste0(base,type,"/"))
  move_api<-GET(url) %>% content("text") %>% fromJSON(flatten=TRUE)
  moves <- tibble(idMove =move_api[["id"]],
  nameMove=move_api[["name"]],
  accuracy = move_api[["accuracy"]],
  power = move_api[["power"]],
  PowerPoint = move_api[["pp"]])
  return(moves)
}
```

## 7. Function to get your pokemon

``` r
get_pokemon <- function(pokemon = 132, tran =1, stat = 1, move = 1, berry=0){
    poke_One<-NULL
    
  if (is.numeric(pokemon) == "TRUE"){
    pokemon_mini<-lookname(lookid(pokemon))
  } else {
    pokemon_mini<-lookname(pokemon)
  }

  poke_One<-NULL  
  if(move == 0 & tran == 1 & stat == 0){
    basic <-basic(pokemon_mini)                  
    traning<-traning(pokemon_mini)
    poke_One<-merge(basic,traning)
  }else if(move == 0 & tran == 0 & stat == 1){
    basic <-basic(pokemon_mini)         
    stat<-stats(pokemon_mini)                   
    poke_One<-merge(basic,stat)
  }else if(move == 1 & tran == 1 & stat == 1){
    poke_One[1]<-lapply(pokemon, FUN=info)      
  }else{
    poke_One[1]<-lapply(pokemon, FUN=info)   
  }
   
  if(berry == 1){
    berry <- get_berry()
  }

return(list(poke_One, berry))
}
```

## Function for berries

-   This function does not require any input arguments. The function
    loops through the pokemon api and pulls the information for the 64
    berries. There is not a direct link to pokemon data itself. It will
    be used for future analytical purposes.

``` r
get_berry <-function(){
  api_json<-GET("https://pokeapi.co/api/v2/berry/1/") %>% content("text") %>% fromJSON(flatten=TRUE)
  berries_df <- as.data.frame(api_json)
  for(i in 2:64){
    base <- "https://pokeapi.co/api/v2/berry/"
    call_next <- (paste0(base,i,"/"))
    api_json2 <- GET(call_next) %>% content("text") %>% fromJSON(flatten=TRUE)
    next_df <- as.data.frame(api_json2)
    berries_df <- rbind(berries_df,next_df)
  }
    berries_df <<-berries_df %>% 
                select(- firmness.url,-flavors.flavor.url,-item.url, -natural_gift_type.url )
}
```

## Get Pokemon

To request information from the api, the user has 3 options:

1.  Get Basic information and Training
2.  Get Basic information and Stats
3.  Get All - Basic information, Training, and Stats
4.  Get Berry information.

The default arguments are (pokemon = 132, tran = 1, stat = 1, move = 1,
berry=0), where 1 is return this information and 0 do not return this
information. \#change EVAL later

``` r
get_pokemon("DITTO")
```

``` r
get_pokemon(18, tran = 1, stat = 0, move = 0, berry=0)
```

``` r
get_pokemon(132, tran = 0, stat = 1, move = 0, berry = 0)
```

## Berries subset

``` r
get_berry()
```

## Scatter plot

``` r
ggplot(berries_df,aes(x = growth_time, y = soil_dryness)) + 
  geom_smooth(formula = y~x, method = "loess") + geom_point() + 
  labs(x = "Growth Time", y= "Soil Dryness",title = "Impact of Soil Dryness on Growth") +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6), plot.title = element_text(hjust = 0.5))
```

<img src="README_files/figure-gfm/berscatter-1.png" style="display: block; margin: auto;" />

-   This scatter plot shows the relationship between berry growth and
    soil dryness. It visually verifies the negative relationship between
    the growth of berries and soil dryness. As the berry grows, the soil
    becomes drier.

    -   This negative relationship is also confirmed in the correlation
        score of -.68.

``` r
cor(berries_df$growth_time,berries_df$soil_dryness)
```

    ## [1] -0.6768502

``` r
ggplot(sep, aes(x = round((weight/height),2), y = speed)) +
    geom_point(aes(color = main.type)) +
    labs(x = "BMI", y= "Speed",title = "Body Mass Index (BMI) and Speed") +
    theme(legend.title = element_text(size = 5), legend.text = element_text(size = 5))
```

-   This scatter plot shows the relationship between BMI (weight/height)
    and speed. We used type of pokemon to highlight the color. This
    chart shows no clear relationship between a higher BMI and moving at
    slower speeds. We can see a few pokemon to the right with BMIs
    around 50; However, they are not the slowest pokemon.

2.  Numeric summaries for berries

``` r
berrySummary <- berries_df %>% select(growth_time, max_harvest, size, smoothness, soil_dryness) %>% apply(2, function(x){summary(x[!is.na(x)])}) 
knitr::kable(berrySummary, caption = 'Summary of Berry Stats', digits = 2)
```

|         | growth_time | max_harvest |   size | smoothness | soil_dryness |
|:--------|------------:|------------:|-------:|-----------:|-------------:|
| Min.    |        2.00 |        5.00 |  20.00 |      20.00 |          4.0 |
| 1st Qu. |        5.00 |        5.00 |  45.75 |      25.00 |          6.0 |
| Median  |       15.00 |        5.00 |  98.50 |      30.00 |          8.0 |
| Mean    |       12.86 |        6.48 | 120.53 |      31.56 |         10.2 |
| 3rd Qu. |       18.00 |        5.00 | 155.25 |      35.00 |         10.0 |
| Max.    |       24.00 |       15.00 | 300.00 |      60.00 |         35.0 |

Summary of Berry Stats

The summary statistics for growth time, maximum harvest, size,
smoothness, and soil dryness.

-   Growth Time : The difference between the mean (12.86) and median
    (15.00) is small, so we can infer HP is symmetrically distributed
    and slightly skewed to the left. Median is higher than mean.

-   For Maximum Harvest and Smoothness, the difference between their
    respective mean and median is small, so we can infer the shape is
    symmetrically distributed; however, slightly skewed to the right.
    The mean is higher than median.

-   For Size and Soil Dryness, the difference between each respective
    mean and median is relatively large. Since the mean is greater than
    the median, we can infer the shape is right skewed.
