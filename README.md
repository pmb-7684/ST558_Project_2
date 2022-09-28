ST 558 - Project 2
================
Paula Bailey and Yi Ren
2022-09-22

Pokémon inhabit the world of the Pokémon games. The franchise was
created by Satoshi Tajiri in 1996. These fictional creatures can be
caught using Pokéballs and trained by battling with other Pokémon. There
are current 920 species according to Wikipedia.

This vignette will walk the reader through the process of connecting to
& collecting data from PokeAPI <https://pokeapi.co/> within R
programming environment.

## Reference to Render Code

``` r
rmarkdown::render("Project2.Rmd", "github_document","README.md")
```

## Packages for Vignette

The following packages are required for connecting and retrieving data
from the API.

1.  `httr` - *Retrieves Data from an API*
2.  `jsonlite` - *Parses Results from an API Query*
3.  `dplyr` - *A part of the `tidyverse` used for manipulating data*
4.  `tidyr` - *A part of the `tidyverse` used for data cleaning and
    ‘tidying’*
5.  `ggplot2` - *A part of the `tidyverse` used for creating graphics*

``` r
# Read in Required Packages 
library(httr)
library(jsonlite)
library(tidyverse)
library(ggplot2)
```

``` r
#reading encounter data
encounter_api = GET("https://pokeapi.co/api/v2/pokemon/ditto/encounters") 
encounter_text<-content(encounter_api,"text")
encounter_json<-fromJSON(encounter_text,flatten=TRUE)
```

``` r
#reading forms data
forms_api = GET("https://pokeapi.co/api/v2/pokemon-form/ditto/") 
api_forms<-content(forms_api,"text")
api_json2<-fromJSON(api_forms,flatten=TRUE)
```

``` r
#reading types data
types_api<-GET(api_json2[["types"]][["type.url"]][[1]])
types_text<-content(types_api,"text")
df_types<-fromJSON(types_text,flatten=TRUE)
```

``` r
#where you can encounter this pokeman
encounter_json$location_area.name
```

    ##  [1] "sinnoh-route-218-area"          "johto-route-34-area"           
    ##  [3] "johto-route-35-area"            "johto-route-47-area"           
    ##  [5] "kanto-route-13-area"            "kanto-route-14-area"           
    ##  [7] "kanto-route-15-area"            "cerulean-cave-1f"              
    ##  [9] "cerulean-cave-2f"               "cerulean-cave-b1f"             
    ## [11] "kanto-route-23-area"            "pokemon-mansion-b1f"           
    ## [13] "desert-underpass-area"          "giant-chasm-forest"            
    ## [15] "giant-chasm-forest-cave"        "pokemon-village-area"          
    ## [17] "johto-safari-zone-zone-wetland"

``` r
#obtain moves and damage info about Pokémon
#df_types[["moves"]]['name']
df_types[["move_damage_class"]]['name']
```

    ## $name
    ## [1] "physical"

``` r
df_types[["damage_relations"]][["double_damage_from"]]['name']
```

    ##       name
    ## 1 fighting

``` r
df_types[["damage_relations"]][["half_damage_to"]]['name']
```

    ##    name
    ## 1  rock
    ## 2 steel

``` r
df_types[["damage_relations"]][["no_damage_from"]]['name']
```

    ##    name
    ## 1 ghost

``` r
df_types[["damage_relations"]][["no_damage_to"]]['name']
```

    ##    name
    ## 1 ghost

``` r
#df_types[["move_damage_class"]]
```
