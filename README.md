Project 2 - Pokemon API
================
Yi Ren and Paula Bailey
2022-10-14

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

# Create functions

-   First, we creates functions for users to easily look up pokemon by
    its id or name. Then we proceed to obtain the related basic,
    training, base stats and moves information. Lastly, we wrapped all
    functions as a combined function to effectively track the desired
    data information.

## 1. Function for look up by id

``` r
lookid<-function(id){
  base<-"https://pokeapi.co/api/v2/pokemon-species/"
  url<-(paste0(base,id,"/"))
  pokemon_api<-GET(url) %>% content("text") %>% fromJSON(flatten=TRUE)
  pokemon<-pokemon_api[["name"]]
  return(pokemon)
}
```

## 2. Function for look up by name

``` r
lookname<-function(pokemon){
  base<-"https://pokeapi.co/api/v2/pokemon/"
  url<-tolower((paste0(base,pokemon,"/")))
  pokemon_api<<-GET(url) %>% content("text") %>% fromJSON(flatten=TRUE)
  return(pokemon_api)
}
```

## 3. Function for basic information

``` r
basic<-function(pokemon_api){
  species_api<-GET(pokemon_api[["species"]][["url"]]) %>% content("text") %>% fromJSON(flatten=TRUE)
  if (length(pokemon_api[["types"]][["type.name"]]) >1) {
    type=paste(pokemon_api[["types"]][["type.name"]], collapse = '/')
  } else{
    type=pokemon_api[["types"]][["type.name"]]
  }
  basic<-data.frame(id=pokemon_api[["id"]], name=pokemon_api[["name"]], type=type, height=pokemon_api[["height"]], weight=pokemon_api[["weight"]], habitat=species_api[["habitat"]][["name"]], shape=species_api[["shape"]][["name"]], color=species_api[["color"]][["name"]])
  return(basic)
}
```

## 4. Function for training information

``` r
traning<-function(pokemon_api){
  species_api<-GET(pokemon_api[["species"]][["url"]]) %>% content("text") %>% fromJSON(flatten=TRUE)
  traning<-data.frame(base_happiness=species_api[["base_happiness"]], capture_rate=species_api[["capture_rate"]],growth_rate=species_api[["growth_rate"]][["name"]],gender_rate=species_api[["gender_rate"]])
  return(traning)
}
```

## 5. Function for base stats

``` r
stats<-function(pokemon_api){
  stats<-pokemon_api[["stats"]] %>% select(stat.name,base_stat) %>% pivot_wider(names_from = "stat.name", values_from = "base_stat") 
  return(stats)
}
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
get_pokemon <- function(pokemon = 132, tran = 1, stat = 1, move = 1, berry = 0){
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

# Wrapper function

``` r
info<-function(pokemon){
  if (is.numeric(pokemon) == "TRUE"){
    pokemon_api<-lookname(lookid(pokemon))
  } else {
    pokemon_api<-lookname(pokemon)
  }
  basic<-basic(pokemon_api)
  traning<-traning(pokemon_api)
  stats<-stats(pokemon_api)
  move <- get_moves(pokemon_api[["id"]])
  info<-merge(basic,traning) %>% merge(stats) %>% merge(move)
  return(info)
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

# Subset dataframes for anaysis

-   We generated first 200 pokemon and all 64 berries from pokemon api
    through our functions. We noticed that some pokemon have dual-type.
    For convenience, we only include main type as our interest. Then we
    saved them as data frames to efficiently handle the actual
    exploratory data analysis.

## Function for subset

``` r
sub<-function(n){
  poke<-NULL
  for (i in 1:n){
    poke[i]<-lapply(i, FUN=info)
  }
  poke_com<-poke[[1]]
  for (i in 2:n){
    poke_com<-bind_rows(poke_com,poke[[i]])
  }
  return(poke_com)
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
information.

``` r
get_pokemon("DITTO")
```

    ## [[1]]
    ## [[1]][[1]]
    ##    id  name   type height weight habitat shape  color base_happiness capture_rate growth_rate gender_rate hp
    ## 1 132 ditto normal      3     40   urban  ball purple             50           35      medium          -1 48
    ##   attack defense special-attack special-defense speed idMove  nameMove accuracy power PowerPoint
    ## 1     48      48             48              48    48    132 constrict      100    10         35
    ## 
    ## 
    ## [[2]]
    ## [1] 0

``` r
get_pokemon(18, tran = 1, stat = 0, move = 0, berry=0)
```

    ## [[1]]
    ##   id    name          type height weight habitat shape color base_happiness capture_rate growth_rate
    ## 1 18 pidgeot normal/flying     15    395  forest wings brown             70           45 medium-slow
    ##   gender_rate
    ## 1           4
    ## 
    ## [[2]]
    ## [1] 0

``` r
get_pokemon(132, tran = 0, stat = 1, move = 0, berry = 0)
```

    ## [[1]]
    ##    id  name   type height weight habitat shape  color hp attack defense special-attack special-defense speed
    ## 1 132 ditto normal      3     40   urban  ball purple 48     48      48             48              48    48
    ## 
    ## [[2]]
    ## [1] 0

## Pokemon subset

``` r
poke<-sub(200)
sep<-separate(poke, col=type, into=c('main.type', 'co.type'), sep='/')
```

## Berries subset

``` r
get_berry()
```

# Basic Exploratory Data Analysis (EDA)

## Bar plot

``` r
ggplot(poke,aes(y = habitat, fill = color)) + geom_bar(position="dodge") + 
  labs(y="Habitat", title ="Bar Plot of Pokemon Habitat by Color") +
  scale_fill_discrete(name="Color") + 
  theme(legend.text = element_text(size = 6), plot.title = element_text(hjust = 0.5))
```

<img src="README_files/figure-gfm/basebar-1.png" style="display: block; margin: auto;" />

-   This bar plot shows the relationship between habitat and the
    pokemon’s color. According to
    [Bulbadpedia](https://bulbapedia.bulbagarden.net/wiki/List_of_Pok%C3%A9mon_by_color),
    color is “usually the color most apparent or covering each pokemon’s
    body”. We believe color is based on where the pokemon lives.  
-   We observed above blue pokemons are mainly creatures that live by
    the water’s edge. We can confirm this by viewing images. These
    pokemon are aquatic or living by the water’s edge.
-   The most diverse region is grassland with seven colors (types) of
    pokemon living in that habitat. The missing habitat is white. Most
    of these pokemon have an arctic appearance.

``` r
ggplot(sep, aes(x = habitat, fill = main.type)) +
  geom_bar(position="dodge") +
  facet_grid(. ~growth_rate, labeller=label_both) + scale_fill_discrete(name="Main Type") + 
  labs(x="Habitat", title ="Side-by-side Bar Plot of Pokemon Growth Rate by Habitat") + theme(axis.text.x = element_text(angle=45, size=5), legend.text = element_text(size = 6), plot.title = element_text(hjust = 0.5) )
```

<img src="README_files/figure-gfm/sepbar-1.png" style="display: block; margin: auto;" />

-   This side-by-side bar plot of pokemon growth rate indicates that
    generally the grass-type pokemon commonly lives in grassland has
    lower-middle growth rate. Most of fairy-type and bug-type pokemon
    grow relatively faster than others. In the contrast, dragon-type
    pokemon grows relatively slower.  
-   We can also observed that it seems like there is strong relationship
    between pokemon’s habitat and their type. We will explore that
    information later in our contingency analysis.

## Contingency Table

``` r
  table(sep$habitat,sep$main.type)
```

    ##                
    ##                 bug dark dragon electric fairy fighting fire ghost grass ground ice normal poison psychic
    ##   cave            0    0      0        0     0        0    0     4     0      2   0      0      3       0
    ##   forest         16    1      0        3     2        0    0     0     5      0   0      6      0       2
    ##   grassland       1    0      0        4     0        0    9     0    16      0   0     13      8       2
    ##   mountain        0    0      0        0     3        5    4     0     0      2   0      1      0       0
    ##   rare            0    0      0        1     0        0    1     0     0      0   1      0      0       2
    ##   rough-terrain   0    0      0        2     0        0    0     0     0      4   0      2      0       0
    ##   sea             0    0      0        0     0        0    0     0     0      0   0      0      0       0
    ##   urban           0    1      0        3     0        2    1     0     0      0   1      6      4       5
    ##   waters-edge     0    0      3        0     0        0    0     0     0      0   0      0      0       0
    ##                
    ##                 rock water
    ##   cave             1     0
    ##   forest           1     0
    ##   grassland        0     0
    ##   mountain         4     0
    ##   rare             0     0
    ##   rough-terrain    0     0
    ##   sea              4    13
    ##   urban            0     1
    ##   waters-edge      0    25

-   As expected, bug-type pokemon lives in forest, grass-type pokemon
    lives in grassland, and water-type pokemon lives in sea or
    waters-edge.  
-   The other type of pokemon’s habitat also make lot of sense, where
    rock-type pokemon lives near mountain and sea, but they also can
    find near cave and forest. In that case, those pokemon might have
    dual-type based on their habitat.

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

<img src="README_files/figure-gfm/bmi-1.png" style="display: block; margin: auto;" />

-   This scatter plot shows the relationship between BMI (weight/height)
    and speed. We used type of pokemon to highlight the color. This
    chart shows no clear relationship between a higher BMI and moving at
    slower speeds. We can see a few pokemon to the right with BMIs
    around 50; However, they are not the slowest pokemon.

``` r
cor(sep$weight,sep$height)
```

    ## [1] 0.5966108

``` r
  ggplot(sep, aes(x=weight, y=height)) + geom_point(aes(color = main.type, shape = habitat)) + geom_smooth(method=lm, formula = y~x) + 
  labs(x = "Weight", y= "Height",title = "Weight vs. Height") +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6), plot.title = element_text(hjust = 0.5))
```

<img src="README_files/figure-gfm/pokescatter-1.png" style="display: block; margin: auto;" />

-   Correlation between weight and height is 0.5966, which considered as
    moderate correlations. From the scatter plot, we can conclude the
    same, which weight and height have relationship but not that strong.

## Histogram

``` r
ggplot(poke, aes(x=shape)) + 
  geom_histogram(binwidth=5,stat="count",colour="black", fill="steelblue") + 
  labs(x = "Shape", y= "Density", title = 'Histogram for Pokemon Shape') + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + theme(axis.text.x = element_text(angle=45)) + theme(plot.title = element_text(hjust = 0.5))
```

<img src="README_files/figure-gfm/histshape-1.png" style="display: block; margin: auto;" />

-   This histogram shows upright, quadruped, humanoid and wings are the
    most common shapes for first 200 pokemon. Arms, heads and tentacles
    are the most rare shapes.

## Box plot

``` r
poke %>% ggplot(aes(x=growth_rate, y=capture_rate)) + geom_boxplot() + geom_jitter(aes(colour=growth_rate)) + labs(title = 'Boxplot for Capture Rate') + theme(legend.title = element_text(size = 5), legend.text = element_text(size = 5))
```

<img src="README_files/figure-gfm/box-1.png" style="display: block; margin: auto;" />

-   This box plot shows capture rate segmented by growth rates -
    medium-slow, medium and fast. The medium growth rate has the largest
    median value of the three at about 125. It also has more outliers.
    It is interesting, the medium-slow box plot does not have a first
    quantile. All of its values lie between the median and 3rd quantile.

## Numeric summaries

1.  Numeric summaries for pokemon base stats

``` r
pokestats <- poke %>% select(hp, attack, defense, 'special-attack','special-defense',speed) %>% apply(2, function(x){summary(x[!is.na(x)])}) 
knitr::kable(pokestats, caption = 'Summary of Pokemon Stats', digits = 2)
```

|         |     hp | attack | defense | special-attack | special-defense |  speed |
|:--------|-------:|-------:|--------:|---------------:|----------------:|-------:|
| Min.    |  10.00 |   5.00 |    5.00 |          15.00 |           20.00 |  15.00 |
| 1st Qu. |  45.00 |  50.00 |   48.75 |          45.00 |           50.00 |  45.00 |
| Median  |  60.00 |  65.00 |   65.00 |          60.50 |           65.00 |  65.00 |
| Mean    |  64.58 |  69.38 |   66.30 |          66.26 |           66.75 |  66.72 |
| 3rd Qu. |  80.00 |  86.25 |   80.00 |          85.00 |           83.50 |  87.75 |
| Max.    | 250.00 | 134.00 |  180.00 |         154.00 |          130.00 | 150.00 |

Summary of Pokemon Stats

The summary statistics for hp, attack, defense, special-attack,
special-defense, and speed.

-   HP : The difference between the mean (60.00) and median (60.04) is
    quite small. So we can infer, the shape is symmetrically
    distributed. The range is between 10 and 140.

-   For Attack, Defense, Special Attack, and Speed, their respective
    mean is greater than the median. So, we can expect the shape to be
    slightly skewed to the right. The outliers are also in the right
    tail of the distribution.

-   Special-Defense : The difference between the mean (60.88) and median
    (64.50). We can infer the shape is symmetrically distributed;
    however, skewed to the left. The median is higher than mean.

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
