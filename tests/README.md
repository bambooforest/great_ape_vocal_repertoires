Great apes vocal repertoires data checks
================
Steven Moran
(May 27, 2022)

``` r
library(tidyverse)
library(bib2df)
```

Read in the spreadsheet data and check for typos.

``` r
df <- read_csv('../Summary_Vocal_Repertoires_Great_Apes.csv')
```

``` r
table(df$Genus)
```

    ## 
    ## Gorilla     Pan   Pongo 
    ##      49      71      66

``` r
table(df$Species)
```

    ## 
    ##      abelii    beringei     gorilla    paniscus    pygmaeus troglodytes 
    ##          31          25          24          42          35          29

``` r
table(df$Call)
```

    ## 
    ##                                   Ahh                             Ahor call 
    ##                                     2                                     2 
    ##                             Alert hoo                     Atonal grunt (a1) 
    ##                                     1                                     2 
    ##                    Bared-theet scream                                  Bark 
    ##                                     2                                     6 
    ## Bark (Hiccup + question + hoot barks)                     Bark (hoot barks) 
    ##                                     2                                     2 
    ##                                 Chomp                                 Click 
    ##                                     2                                     1 
    ##                          Complex call                         Composed bark 
    ##                                     2                                     3 
    ##                           Contact uff                       Contest hooting 
    ##                                     2                                     2 
    ##                       Copulation call                      Copulation grunt 
    ##                                     1                                     2 
    ##             Cough grunt (Pant series)              Cough grunt (Pig grunts) 
    ##                                     2                                     2 
    ##                                 Croak                                   Cry 
    ##                                     1                                     2 
    ##                                Crying                                Cuckle 
    ##                                     2                                     2 
    ##                        Extended grunt                        Fast long call 
    ##                                     1                                     2 
    ##                           Faux-speech                           Fear squeak 
    ##                                     1                                     2 
    ##                    Frustration scream                     Frustration whine 
    ##                                     2                                     2 
    ##                           Full scream                                Gorkum 
    ##                                     1                                     2 
    ##                              Grinding                                 Growl 
    ##                                     1                                     1 
    ##                               Grumble                          Grumble (t4) 
    ##                                     4                                     6 
    ##                                Grumph                                 Grunt 
    ##                                     2                                     4 
    ##                                Hiccup                          High hooting 
    ##                                     3                                     3 
    ##                             Hoo grunt                     Hoot + chest beat 
    ##                                     2                                     2 
    ##                           Hoot series                             Hunt bark 
    ##                                     2                                     2 
    ##                           Kiss-squeak                                Kisses 
    ##                                     2                                     1 
    ##                               Laugher                             Lip smack 
    ##                                     1                                     1 
    ##                             Long call                         Long hum (t3) 
    ##                                     2                                     2 
    ##                                  Lork                           Low hooting 
    ##                                     2                                     1 
    ##                        Mating squeals                          Muffled bark 
    ##                                     2                                     1 
    ##                            Nestsmacks                                  Pant 
    ##                                     2                                     3 
    ##                            Pant grunt                             Pant hoot 
    ##                                     1                                     3 
    ##                         Panting laugh                                  Peep 
    ##                                     1                                     5 
    ##                             Peep-yelp                              Play hoo 
    ##                                     1                                     2 
    ##                             Pout moan                           Rasp scream 
    ##                                     3                                     2 
    ##                             Raspberry                              Rest hoo 
    ##                                     4                                     1 
    ##                                  Roar                           Roar (Roar) 
    ##                                     2                                     2 
    ##                         Roar (Wraagh)                          Rolling call 
    ##                                     2                                     2 
    ##              Rough grunt (Food grunt)                                Scream 
    ##                                     1                                     5 
    ##                           Scream-bark                        Scream-whistle 
    ##                                     2                                     1 
    ##                            Sex-whinny                        Short hum (t1) 
    ##                                     1                                     2 
    ##                                  Sing                             Soft bark 
    ##                                     2                                     4 
    ##                             Soft hoot                                Squeak 
    ##                                     2                                     3 
    ##                          Teeth chomps                          Throatscrape 
    ##                                     1                                     1 
    ##                      Tonal grunt (t2)                           Travel bark 
    ##                                     2                                     3 
    ##                            Travel hoo                               Whimper 
    ##                                     1                                     3 
    ##                                 Whine                                Whinny 
    ##                                     2                                     2 
    ##                               Whistle                          Whistle-bark 
    ##                                     3                                     1 
    ##                             Whistling                                  Yelp 
    ##                                     2                                     1

``` r
table(df$Call_type)
```

    ## 
    ##         Close call Long-distance call 
    ##                110                 76

``` r
table(df$Sex)
```

    ## 
    ##   Both Female   Male 
    ##    144     16     26

``` r
table(df$Hierarchy)
```

    ## 
    ##        Both    Dominant Subordinate 
    ##         169          10           7

``` r
table(df$Context)
```

    ## 
    ## Affiliative   Agonistic       Alarm  Copulation    Distress     Feeding 
    ##          21          31          36          18           9          26 
    ##    Grooming        Hunt     Nesting        Play      Travel     Unknown 
    ##           6           1           5          22           3           8

``` r
table(df$Environment)
```

    ## 
    ## Captive    Wild 
    ##      13     173

``` r
table(df$Facial_expression)
```

    ## 
    ##                                                              Changing of facial expression 
    ##                                                                                          1 
    ##                                                                Chewing movements with jaws 
    ##                                                                                          2 
    ##                                                                             Clacking teeth 
    ##                                                                                          1 
    ##                 Jaw closed with very protruded lips switching from little and very rounded 
    ##                                                                                          1 
    ##                                                               Lips movement with jaw close 
    ##                                                                                          1 
    ##                                              Little protruded unrounded lips with jaw open 
    ##                                                                                          3 
    ##                                                                      May have teeth-baring 
    ##                                                                                          1 
    ##                                                                               Mouth closed 
    ##                                                                                         14 
    ##                                  Mouth opened with slightly pouted lips covering the teeth 
    ##                                                                                          3 
    ##                              Mouth widely open and lips slightly pouted covering the teeth 
    ##                                                                                          2 
    ##                                                                          Moving the tongue 
    ##                                                                                          1 
    ##                                                                       No facial expression 
    ##                                                                                          7 
    ##                                                                      No play face involved 
    ##                                                                                          1 
    ##                                                                                 Open mouth 
    ##                                                                                          4 
    ##                                                                                  Play face 
    ##                                                                                          1 
    ##                                                                                  Pout face 
    ##                                                                                          3 
    ##                                                                             Protruded lips 
    ##                                                                                          4 
    ##                                                                                Pursed lips 
    ##                                                                                          1 
    ##                                                                   Pursed trumpet-like lips 
    ##                                                                                          2 
    ##                                                                         Relaxed open mouth 
    ##                                                                                          2 
    ##                                                                           Resemble a smile 
    ##                                                                                          2 
    ##                                                               Retracted and unrounded lips 
    ##                                                                                          1 
    ##                                                       Silent teeth-baring with pouted lips 
    ##                                                                                          1 
    ##                                                                        Single sharp squeak 
    ##                                                                                          2 
    ##                                                          String of rolling guttural noises 
    ##                                                                                          2 
    ##                                                                             Supralaryngeal 
    ##                                                                                          1 
    ##                                                                       Swollen throat pouch 
    ##                                                                                          2 
    ##                                                                             Teeth grinding 
    ##                                                                                          1 
    ##                                                     Teeth-baring in the middle of the call 
    ##                                                                                          2 
    ##                                                             Teeth-baring with closed mouth 
    ##                                                                                          1 
    ## Teeth-baring with lips retraction exposing both teeth and gums. The mouth may be wide open 
    ##                                                                                          4 
    ##                                             Unprotruded little rounded lips with jaw close 
    ##                                                                                          1 
    ##                                              Unprotruded little rounded lips with jaw open 
    ##                                                                                          5 
    ##                                                  Unprotruded unrounded lips with jaw close 
    ##                                                                                          3 
    ##        Unprotruded unrounded lips with jaw close combined with very protruded rounded lips 
    ##                                                                                          2 
    ##                                                                Very protruded rounded lips 
    ##                                                                                          5 
    ##                                                        Wide open mouth with lips protruded 
    ##                                                                                          2 
    ##                                                Wide open mouth with teeth and gums exposed 
    ##                                                                                          2

Check the references

``` r
references <- str_split(df$References, ",")
references <- references %>% unlist()
references <- str_trim(references)
references <- unique(references)
references <- sort(references)
```

Get the bibtex IDs from the bibtex file:

``` r
path <- '../References/Table_references.bib'
all <- bib2df(path)

bib <- c(all$BIBTEXKEY)
bib <- sort(bib)
```

Check for duplicates:

``` r
table(bib)
```

    ## bib
    ##                    Arcadi1996            Bermejo_Omedes1999 
    ##                             1                             1 
    ##            Clark_Wrangham1993 Clay_Archbold_Zuberbuhler2015 
    ##                             1                             1 
    ##          Clay_Zuberbuhler2009         Clay_Zuberbuhler2011a 
    ##                             1                             1 
    ##          Crockford_Boesch2005                 Crockford2003 
    ##                             1                             1 
    ##                 Crockford2004                 Crockford2018 
    ##                             1                             1 
    ##               Davila-Ross2002                   Delgado2010 
    ##                             1                             1 
    ##                    DeWaal1988                 Dezecache2020 
    ##                             1                             1 
    ##                   Fedurek2017                    Fossey1972 
    ##                             1                             1 
    ##           Galdikas_Insley1988                  Galdikas1983 
    ##                             1                             1 
    ##                     Genty2014                     Genty2019 
    ##                             1                             1 
    ##                   Goodall1964                 Grawunder2022 
    ##                             1                             1 
    ##                  Harcourt1993                    Hardus2009 
    ##                             1                             1 
    ##                   Hardus2009a                    Hedwig2014 
    ##                             1                             1 
    ##                   Hohmann1994                   Hopkins2007 
    ##                             1                             1 
    ##                    Keenan2020                   Lameira2008 
    ##                             1                             1 
    ##                   Lameira2012                   Lameira2013 
    ##                             1                             1 
    ##                  Lameira2013a                   Lameira2015 
    ##                             1                             1 
    ##                   Lameira2017                   Lameira2021 
    ##                             1                             1 
    ##       Laporte_Zuberbuhler2010                      Luef2016 
    ##                             1                             1 
    ##                 MacKinnon1974            Marler_Hobbett1975 
    ##                             1                             1 
    ##             Marler_Tenaza1977                  Marshall1999 
    ##                             1                             1 
    ##                    Mitani1985                    Mitani1995 
    ##                             1                             1 
    ##                    Mitani1996                MitraSetia2007 
    ##                             1                             1 
    ##                    Notman2005                   Pereira2020 
    ##                             1                             1 
    ##             Perlman_Clark2015                   Perlman2017 
    ##                             1                             1 
    ##                    Plooij1984                   Rijksen1978 
    ##                             1                             1 
    ##                   Robbins2016                     Salmi2013 
    ##                             1                             1 
    ##                     Salmi2014                     Salmi2020 
    ##                             1                             1 
    ##           Sayfarth_Cheney2012                 Schamberg2017 
    ##                             1                             1 
    ##                    Schel2013a                    Schel2013b 
    ##                             1                             1 
    ##                  Seyfarth1994              Siebert_Parr2003 
    ##                             1                             1 
    ##      Slocombe_Zuberbuhler2007      Slocombe_Zuberbuhler2010 
    ##                             1                             1 
    ##                 Spillmann2010                   Stewart1994 
    ##                             1                             1 
    ##              Taglialatela2012      Townsend_Zuberbuhler2009 
    ##                             1                             1 
    ##            VanKrunkelsven1996                    Watson2015 
    ##                             1                             1

Here are the duplicates:

``` r
bib[which(duplicated(bib))]
```

    ## character(0)

Check what’s listed in the table, but is not in the references.

``` r
setdiff(references, bib)
```

    ## character(0)

Check what’s in the bibtex references but not listed in the table.

``` r
setdiff(bib, references)
```

    ## [1] "Goodall1964"  "Harcourt1993" "Seyfarth1994" "Stewart1994"
