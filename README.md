An analysis of the ICA 2020 program
================
Chung-hong Chan [1]

``` r
require(tidyverse)
```

    ## Loading required package: tidyverse

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.3
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.4
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
require(textclean)
```

    ## Loading required package: textclean

``` r
require(quanteda)
```

    ## Loading required package: quanteda

    ## Package version: 2.0.0

    ## Parallel computing: 2 of 4 threads used.

    ## See https://quanteda.io for tutorials and examples.

    ## 
    ## Attaching package: 'quanteda'

    ## The following object is masked from 'package:utils':
    ## 
    ##     View

``` r
ica_raw <- rio::import('70th Annual ICA Conference_29FEB2020.csv') %>% as_tibble
ica_raw
```

    ## # A tibble: 2,882 x 19
    ##    `Session or Eve… `Session or Eve… `Session or Eve… `Session or Eve…
    ##    <chr>            <chr>            <chr>            <chr>           
    ##  1 ICA Executive C… Business Meeting Sponsored Sessi… 19-May-2020     
    ##  2 Board of Direct… Breakfast        Sponsored Sessi… 20-May-2020     
    ##  3 Annual ICA Boar… Business Meeting Sponsored Sessi… 20-May-2020     
    ##  4 Board meeting l… Reception        Sponsored Sessi… 20-May-2020     
    ##  5 Volunteer Orien… Meeting          Sponsored Sessi… 20-May-2020     
    ##  6 Annual ICA Boar… Business Meeting Sponsored Sessi… 21-May-2020     
    ##  7 ICA Inclusion, … Meeting          Sponsored Sessi… 21-May-2020     
    ##  8 WELCOME TO COUN… Panel Session    Sponsored Sessi… 21-May-2020     
    ##  9 OPENING RECEPTI… Reception        Sponsored Sessi… 21-May-2020     
    ## 10 OPENING RECEPTI… Reception        Sponsored Sessi… 21-May-2020     
    ## # … with 2,872 more rows, and 15 more variables: `Session or Event Start
    ## #   Time` <chr>, `Session or Event End Time` <chr>, `Session or Event
    ## #   Location` <chr>, `Session or Event Details` <chr>, `Session or Event
    ## #   Participants/Hosts` <chr>, `Submission or Placeholder Title` <chr>,
    ## #   `Submission or Placeholder Start Time` <chr>, `Submission or Placeholder
    ## #   End Time` <chr>, `Submission Presenter Name` <chr>, `Submission
    ## #   Authors` <chr>, `Submission Status` <chr>, `Institutions All` <chr>,
    ## #   `Submission Body` <chr>, `Session Submission Sort Order` <int>, `The
    ## #   Program Report was last updated February 29, 2020 at 12:13 AM EST. To view
    ## #   the most recent meeting schedule online, visit
    ## #   https://ica2020.abstractcentral.com/planner.jsp` <lgl>

``` r
colnames(ica_raw)
```

    ##  [1] "Session or Event Title"                                                                                                                                                       
    ##  [2] "Session or Event Type"                                                                                                                                                        
    ##  [3] "Session or Event Division/Interest Group"                                                                                                                                     
    ##  [4] "Session or Event Date"                                                                                                                                                        
    ##  [5] "Session or Event Start Time"                                                                                                                                                  
    ##  [6] "Session or Event End Time"                                                                                                                                                    
    ##  [7] "Session or Event Location"                                                                                                                                                    
    ##  [8] "Session or Event Details"                                                                                                                                                     
    ##  [9] "Session or Event Participants/Hosts"                                                                                                                                          
    ## [10] "Submission or Placeholder Title"                                                                                                                                              
    ## [11] "Submission or Placeholder Start Time"                                                                                                                                         
    ## [12] "Submission or Placeholder End Time"                                                                                                                                           
    ## [13] "Submission Presenter Name"                                                                                                                                                    
    ## [14] "Submission Authors"                                                                                                                                                           
    ## [15] "Submission Status"                                                                                                                                                            
    ## [16] "Institutions All"                                                                                                                                                             
    ## [17] "Submission Body"                                                                                                                                                              
    ## [18] "Session Submission Sort Order"                                                                                                                                                
    ## [19] "The Program Report was last updated February 29, 2020 at 12:13 AM EST. To view the most recent meeting schedule online, visit https://ica2020.abstractcentral.com/planner.jsp"

I am only interested in a few columns.

``` r
colnames(ica_raw)[2] <- "event_type"
colnames(ica_raw)[3] <- "event_group"
colnames(ica_raw)[5] <- "start_time"
colnames(ica_raw)[8] <- "event_info"
colnames(ica_raw)[17] <- "abstract"
ica_raw %>% count(event_type) -> all_event_types
### I am only interested in these sessions.
all_event_types[c(5, 6, 7, 8, 9, 11, 14),]
```

    ## # A tibble: 7 x 2
    ##   event_type                      n
    ##   <chr>                       <int>
    ## 1 Extended Session               20
    ## 2 High-Density Paper Session    194
    ## 3 Hybrid High-Density Session   197
    ## 4 Innovative Format              17
    ## 5 Interactive Paper Session     250
    ## 6 Panel Session                 914
    ## 7 Standard Paper Session       1121

``` r
##probably not the cleanest.

ica_raw %>% filter(event_type %in% all_event_types$event_type[c(5, 6, 7, 8, 9, 11, 14)]) %>% mutate(abstract = str_remove(replace_html(abstract), "^Abstracts? ?B?o?d?y?:? ?")) %>% filter(abstract != "") -> ica
ica %>% count(event_group, sort = TRUE) %>% add_count(wt = n, name = "total") %>% mutate(percent = round((n / total) * 100,2)) %>% select(-total) %>% knitr::kable()
```

| event\_group                                          |    n|  percent|
|:------------------------------------------------------|----:|--------:|
| Health Communication                                  |  267|     9.93|
| Communication and Technology                          |  232|     8.63|
| Journalism Studies                                    |  199|     7.40|
| Political Communication                               |  192|     7.14|
| Mass Communication                                    |  153|     5.69|
| Information Systems                                   |  141|     5.25|
| Public Relations                                      |   88|     3.27|
| Interpersonal Communication                           |   78|     2.90|
| Computational Methods                                 |   71|     2.64|
| Media Industry Studies                                |   71|     2.64|
| Environmental Communication                           |   69|     2.57|
| Organizational Communication                          |   69|     2.57|
| Sponsored Sessions                                    |   68|     2.53|
| Activism, Communication and Social Justice            |   62|     2.31|
| Mobile Communication                                  |   59|     2.19|
| Children, Adolescents and the Media                   |   58|     2.16|
| Global Communication and Social Change                |   58|     2.16|
| Instructional and Developmental Communication         |   58|     2.16|
| Popular Communication                                 |   58|     2.16|
| Ethnicity and Race in Communication                   |   57|     2.12|
| Game Studies                                          |   57|     2.12|
| Feminist Scholarship                                  |   55|     2.05|
| Human-Machine Communication                           |   54|     2.01|
| Visual Communication Studies                          |   52|     1.93|
| Sports Communication                                  |   45|     1.67|
| Philosophy, Theory and Critique                       |   44|     1.64|
| Lesbian, Gay, Bisexual, Transgender and Queer Studies |   36|     1.34|
| Intercultural Communication                           |   35|     1.30|
| Communication History                                 |   34|     1.26|
| Language and Social Interaction                       |   33|     1.23|
| Communication Science, and Biology                    |   30|     1.12|
| Communication Law and Policy                          |   28|     1.04|
| Theme                                                 |   28|     1.04|
| Intergroup Communication                              |   25|     0.93|
| Public Diplomacy                                      |   24|     0.89|

``` r
abstract_corpus <- corpus(ica$abstract)
docvars(abstract_corpus, "group") <- ica$event_group
```

``` r
dfm(abstract_corpus, tolower = TRUE, stem = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords('en')) %>% dfm_select("^[A-Za-z]+$", valuetype = 'regex') -> abstract_dfm
```

Top Features of all ICA abstracts.

``` r
topfeatures(abstract_dfm, n = 50)
```

    ##        media        studi       social          use     communic     research 
    ##         4052         3732         3375         2938         2224         2198 
    ##         news       inform       effect        polit       public       examin 
    ##         2093         1665         1571         1461         1459         1307 
    ##       result        onlin       differ     particip         find         data 
    ##         1288         1234         1227         1200         1197         1196 
    ##         also          can        paper      analysi        relat        peopl 
    ##         1143         1096         1071         1059         1033         1028 
    ##      content       health       messag       cultur        digit     influenc 
    ##          995          980          978          954          954          938 
    ##       experi      practic      discuss     behavior      perceiv          one 
    ##          935          908          903          873          873          872 
    ## relationship          new        posit         user       theori         show 
    ##          854          853          843          839          834          825 
    ##        engag   understand     individu       provid      increas     interact 
    ##          821          815          799          784          784          783 
    ##        model          two 
    ##          783          780

What the "big 5" divisions are writing?
=======================================

Health Communication
--------------------

``` r
textstat_keyness(abstract_dfm, target = docvars(abstract_dfm, "group") == "Health Communication") %>% textplot_keyness
```

![](README_files/figure-markdown_github/hc-1.png)

CAT
---

``` r
textstat_keyness(abstract_dfm, target = docvars(abstract_dfm, "group") == "Communication and Technology") %>% textplot_keyness
```

![](README_files/figure-markdown_github/cat-1.png)

JSD
---

``` r
textstat_keyness(abstract_dfm, target = docvars(abstract_dfm, "group") == "Journalism Studies") %>% textplot_keyness
```

![](README_files/figure-markdown_github/jsd-1.png)

POLCOM
------

``` r
textstat_keyness(abstract_dfm, target = docvars(abstract_dfm, "group") == "Political Communication") %>% textplot_keyness
```

![](README_files/figure-markdown_github/polcom-1.png)

MASSCOM
-------

``` r
textstat_keyness(abstract_dfm, target = docvars(abstract_dfm, "group") == "Mass Communication") %>% textplot_keyness
```

![](README_files/figure-markdown_github/masscom-1.png)

and of course,

Computational methods
---------------------

``` r
textstat_keyness(abstract_dfm, target = docvars(abstract_dfm, "group") == "Computational Methods") %>% textplot_keyness
```

![](README_files/figure-markdown_github/comp-1.png)

and

theme
-----

``` r
textstat_keyness(abstract_dfm, target = docvars(abstract_dfm, "group") == "Theme") %>% textplot_keyness
```

![](README_files/figure-markdown_github/theme-1.png)

Similarity between groups
=========================

``` r
uni_groups <- unique(docvars(abstract_dfm, "group"))
group_dfm <- map(uni_groups, ~ apply(dfm_subset(abstract_dfm, group == .), 2, sum))
names(group_dfm) <- uni_groups

## How similar is PolCom 3 and JSD 12

require(lsa)
```

    ## Loading required package: lsa

    ## Loading required package: SnowballC

``` r
## So ugly
cosine(group_dfm['Political Communication'][[1]], group_dfm['Journalism Studies'][[1]])
```

    ##          [,1]
    ## [1,] 0.770592

``` r
## Polcom 3 and Comm Law 4
cosine(group_dfm['Political Communication'][[1]], group_dfm['Communication Law and Policy'][[1]])
```

    ##           [,1]
    ## [1,] 0.5040687

``` r
t(combn(uni_groups, 2)) %>% as_tibble(.name_repair = "minimal") -> pairs
colnames(pairs) <- c('gp1', 'gp2')

get_cosine <- function(gp1, gp2, group_dfm) {
    cosine(group_dfm[gp1][[1]], group_dfm[gp2][[1]])[1,1]
}

get_cosine("Political Communication", "Theme", group_dfm)
```

    ## [1] 0.4353212

``` r
pairs %>% mutate(weight = map2_dbl(gp1, gp2, get_cosine, group_dfm = group_dfm)) -> pairs
require(igraph)
```

    ## Loading required package: igraph

    ## 
    ## Attaching package: 'igraph'

    ## The following object is masked from 'package:quanteda':
    ## 
    ##     as.igraph

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     as_data_frame, groups, union

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     compose, simplify

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     crossing

    ## The following object is masked from 'package:tibble':
    ## 
    ##     as_data_frame

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

``` r
ica_graph <- graph_from_data_frame(pairs, directed = FALSE)
### Not informative at all
plot(ica_graph)
```

![](README_files/figure-markdown_github/network-1.png)

Most similar

``` r
pairs %>% arrange(desc(weight)) %>% head(n = 50) %>% knitr::kable()
```

| gp1                                        | gp2                                                   |     weight|
|:-------------------------------------------|:------------------------------------------------------|----------:|
| Mass Communication                         | Communication and Technology                          |  0.8600901|
| Information Systems                        | Communication and Technology                          |  0.8597381|
| Political Communication                    | Mass Communication                                    |  0.8419335|
| Mass Communication                         | Information Systems                                   |  0.8408760|
| Health Communication                       | Information Systems                                   |  0.8327283|
| Communication and Technology               | Computational Methods                                 |  0.8247976|
| Political Communication                    | Communication and Technology                          |  0.8053256|
| Communication and Technology               | Mobile Communication                                  |  0.7996365|
| Health Communication                       | Mass Communication                                    |  0.7858658|
| Health Communication                       | Communication and Technology                          |  0.7759992|
| Political Communication                    | Journalism Studies                                    |  0.7705920|
| Health Communication                       | Interpersonal Communication                           |  0.7680352|
| Public Relations                           | Organizational Communication                          |  0.7605388|
| Information Systems                        | Communication Science, and Biology                    |  0.7566175|
| Mass Communication                         | Computational Methods                                 |  0.7555553|
| Communication and Technology               | Children, Adolescents and the Media                   |  0.7507227|
| Political Communication                    | Computational Methods                                 |  0.7491128|
| Interpersonal Communication                | Communication and Technology                          |  0.7403624|
| Philosophy, Theory and Critique            | Activism, Communication and Social Justice            |  0.7394289|
| Activism, Communication and Social Justice | Ethnicity and Race in Communication                   |  0.7391575|
| Communication and Technology               | Sponsored Sessions                                    |  0.7369356|
| Popular Communication                      | Intercultural Communication                           |  0.7366973|
| Information Systems                        | Computational Methods                                 |  0.7343714|
| Global Communication and Social Change     | Sponsored Sessions                                    |  0.7342234|
| Mass Communication                         | Environmental Communication                           |  0.7341166|
| Public Relations                           | Sponsored Sessions                                    |  0.7316993|
| Communication and Technology               | Communication Science, and Biology                    |  0.7239485|
| Activism, Communication and Social Justice | Communication and Technology                          |  0.7236737|
| Philosophy, Theory and Critique            | Global Communication and Social Change                |  0.7223500|
| Popular Communication                      | Media Industry Studies                                |  0.7218148|
| Activism, Communication and Social Justice | Global Communication and Social Change                |  0.7211085|
| Interpersonal Communication                | Mass Communication                                    |  0.7197884|
| Mass Communication                         | Children, Adolescents and the Media                   |  0.7159478|
| Philosophy, Theory and Critique            | Communication and Technology                          |  0.7144059|
| Mass Communication                         | Journalism Studies                                    |  0.7132753|
| Sponsored Sessions                         | Intercultural Communication                           |  0.7126904|
| Environmental Communication                | Communication and Technology                          |  0.7121789|
| Organizational Communication               | Sponsored Sessions                                    |  0.7102609|
| Political Communication                    | Activism, Communication and Social Justice            |  0.7101507|
| Political Communication                    | Information Systems                                   |  0.7084504|
| Communication and Technology               | Public Relations                                      |  0.7077545|
| Environmental Communication                | Sponsored Sessions                                    |  0.7076118|
| Communication and Technology               | Lesbian, Gay, Bisexual, Transgender and Queer Studies |  0.7049189|
| Activism, Communication and Social Justice | Lesbian, Gay, Bisexual, Transgender and Queer Studies |  0.7040059|
| Computational Methods                      | Sponsored Sessions                                    |  0.7028721|
| Philosophy, Theory and Critique            | Political Communication                               |  0.7019654|
| Mass Communication                         | Communication Science, and Biology                    |  0.6983418|
| Communication and Technology               | Intercultural Communication                           |  0.6980845|
| Interpersonal Communication                | Information Systems                                   |  0.6979278|
| Ethnicity and Race in Communication        | Sponsored Sessions                                    |  0.6964196|

Newsmap
=======

Classify all abstracts by the geographical prediction algorithm by Watanabe (2017) [2].

``` r
require(newsmap)
```

    ## Loading required package: newsmap

``` r
toks <- tokens(abstract_corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% tokens_tolower %>% tokens_remove(stopwords('english'), valuetype = 'fixed', padding = TRUE)
country_label <- tokens_lookup(toks, data_dictionary_newsmap_en, levels = 3)
dfmt_label <- dfm(country_label)

dfmt_feat <- dfm(toks) %>% dfm_select(pattern = "^[a-z]", selection = "keep", valuetype = "regex")

model <- textmodel_newsmap(dfmt_feat, dfmt_label)
coef(model, n = 7)[c("us", "gb", "de", "br", "jp", "hk", "cn")]
```

    ## $us
    ##          us    american        york   americans trafficking  washington 
    ##    6.374726    6.022749    4.969946    4.571916    4.061091    3.900748 
    ##       e-cig 
    ##    3.856296 
    ## 
    ## $gb
    ##          uk      london   migrators       e-cig franchisees     tapinto 
    ##    5.881744    5.639182    5.233717    4.798399    4.479945    4.415406 
    ##    laughing 
    ##    4.415406 
    ## 
    ## $de
    ##     germany      german         und        nfcc instamancer       meier 
    ##    6.217457    5.917353    5.629671    5.224205    4.793422    4.713380 
    ##       gesis 
    ##    4.626368 
    ## 
    ## $br
    ##      brazil venezuelans        ncis     orleans   brazilian     roraima 
    ##    5.829737    4.625764    4.507981    4.507981    4.374449    4.374449 
    ##      fixers 
    ##    4.374449 
    ## 
    ## $jp
    ##         japan      japanese          midi         tokyo       koreans 
    ##      5.696188      5.310525      5.310525      4.269071      4.269071 
    ##       anime's skateboarding 
    ##      4.086750      4.086750 
    ## 
    ## $hk
    ##            kong              hk             yue     hongkongers     blue-ribbon 
    ##        5.892231        4.864445        4.496720        4.378937        4.378937 
    ## countermovement          youths 
    ##        4.245406        4.245406 
    ## 
    ## $cn
    ##       chinese         china     migrators           tcm           npd 
    ##      7.554475      7.208458      5.089371      4.930307      4.698505 
    ##          midi study-with-me 
    ##      4.396224      4.335600

How US-centric is ICA?

``` r
country <- predict(model)
tibble(country) %>% count(country, sort = TRUE) %>% add_count(wt = n, name = 'totaln') %>% mutate(percent = round((n / totaln) * 100, 2)) %>% select(country, percent) %>% knitr::kable()
```

| country |  percent|
|:--------|--------:|
| us      |    61.79|
| cn      |    24.96|
| au      |     2.83|
| gb      |     2.16|
| de      |     1.34|
| in      |     1.34|
| hk      |     1.00|
| nz      |     0.52|
| tr      |     0.48|
| kr      |     0.37|
| sg      |     0.37|
| fr      |     0.22|
| ke      |     0.22|
| br      |     0.15|
| il      |     0.15|
| no      |     0.15|
| ph      |     0.15|
| pl      |     0.15|
| tw      |     0.15|
| dk      |     0.11|
| eg      |     0.11|
| ng      |     0.11|
| nl      |     0.11|
| ru      |     0.11|
| sy      |     0.11|
| be      |     0.07|
| ca      |     0.07|
| es      |     0.07|
| mx      |     0.07|
| ua      |     0.07|
| ae      |     0.04|
| at      |     0.04|
| ch      |     0.04|
| fi      |     0.04|
| gh      |     0.04|
| id      |     0.04|
| ir      |     0.04|
| it      |     0.04|
| jp      |     0.04|
| rs      |     0.04|
| sa      |     0.04|
| za      |     0.04|

Rank division/IG by percetnage of non-US abstracts

``` r
tibble(group = ica$event_group, country) %>% mutate(nonus = country != "us") %>% group_by(group) %>% summarise(totalnonus = sum(nonus), n = n()) %>% mutate(percent = round((totalnonus / n) * 100)) %>% arrange(desc(percent)) %>% knitr::kable()
```

| group                                                 |  totalnonus|    n|  percent|
|:------------------------------------------------------|-----------:|----:|--------:|
| Lesbian, Gay, Bisexual, Transgender and Queer Studies |          24|   36|       67|
| Media Industry Studies                                |          47|   71|       66|
| Interpersonal Communication                           |          48|   78|       62|
| Public Diplomacy                                      |          15|   24|       62|
| Human-Machine Communication                           |          33|   54|       61|
| Intercultural Communication                           |          21|   35|       60|
| Global Communication and Social Change                |          34|   58|       59|
| Popular Communication                                 |          33|   58|       57|
| Activism, Communication and Social Justice            |          32|   62|       52|
| Intergroup Communication                              |          12|   25|       48|
| Health Communication                                  |         118|  267|       44|
| Mobile Communication                                  |          26|   59|       44|
| Sponsored Sessions                                    |          30|   68|       44|
| Sports Communication                                  |          20|   45|       44|
| Game Studies                                          |          24|   57|       42|
| Communication and Technology                          |          94|  232|       41|
| Children, Adolescents and the Media                   |          23|   58|       40|
| Ethnicity and Race in Communication                   |          23|   57|       40|
| Communication Law and Policy                          |          11|   28|       39|
| Feminist Scholarship                                  |          21|   55|       38|
| Mass Communication                                    |          56|  153|       37|
| Environmental Communication                           |          25|   69|       36|
| Language and Social Interaction                       |          12|   33|       36|
| Visual Communication Studies                          |          18|   52|       35|
| Public Relations                                      |          28|   88|       32|
| Information Systems                                   |          43|  141|       30|
| Philosophy, Theory and Critique                       |          13|   44|       30|
| Organizational Communication                          |          19|   69|       28|
| Political Communication                               |          53|  192|       28|
| Communication History                                 |           8|   34|       24|
| Computational Methods                                 |          13|   71|       18|
| Journalism Studies                                    |          36|  199|       18|
| Instructional and Developmental Communication         |           9|   58|       16|
| Communication Science, and Biology                    |           3|   30|       10|
| Theme                                                 |           2|   28|        7|

[1] University of Mannheim

[2] Watanabe, K. (2018). Newsmap: A semi-supervised approach to geographical news classification. Digital Journalism, 6(3), 294-309.
