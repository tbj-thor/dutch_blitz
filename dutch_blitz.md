Dutch Blitz bonanza
================
Thor
2020

-   [The following is an absolute waste of time](#the-following-is-an-absolute-waste-of-time)
    -   [Load packages and scores](#load-packages-and-scores)
    -   [</br>How much I suck](#how-much-i-suck)
    -   [</br> when ($round==7) { 4\_player\_game = 2\_player\_game }](#when-round7-4_player_game-2_player_game)
    -   [</br>Hvad hvis man spillede med skæve istedet for negative point](#hvad-hvis-man-spillede-med-skæve-istedet-for-negative-point)
    -   [(Minus point for en spiller giver i stedet plus til alle andre)](#minus-point-for-en-spiller-giver-i-stedet-plus-til-alle-andre)
    -   [(Er jævnt besværligt at holde score på den måde)](#er-jævnt-besværligt-at-holde-score-på-den-måde)
    -   [Also, Claus og Mette er gode til at få point i de samme runder mens Mark som sædvanlig er på tværs af alle andre og helst vil have point når vi andre mister og miste når vi andre vinder](#also-claus-og-mette-er-gode-til-at-få-point-i-de-samme-runder-mens-mark-som-sædvanlig-er-på-tværs-af-alle-andre-og-helst-vil-have-point-når-vi-andre-mister-og-miste-når-vi-andre-vinder)
    -   [Og til sidst, en kompliceret udregning af hvor mange runder vi skulle spille hvis jeg skulle vinde](#og-til-sidst-en-kompliceret-udregning-af-hvor-mange-runder-vi-skulle-spille-hvis-jeg-skulle-vinde)

The following is an absolute waste of time
------------------------------------------

#### Load packages and scores

``` r
library(openxlsx)
library(ggplot2)
library(reshape)
library(gplots)
```

    ## 
    ## Attaching package: 'gplots'

    ## The following object is masked from 'package:stats':
    ## 
    ##     lowess

``` r
library(colorspace)
d = read.xlsx("https://raw.githubusercontent.com/tbj-thor/dutch_blitz/master/dutch_blitz.xlsx",colNames = T)
d
```

    ##    Mark Claus Mette Thor
    ## 1    20     7     5    6
    ## 2   -12    10     8   -4
    ## 3     0    11    15   13
    ## 4    16    -7   -14    0
    ## 5   -15   -12     9   -8
    ## 6    23    -2    -8   -9
    ## 7    -3     2     4   22
    ## 8    13     0    10   -7
    ## 9    10     6    10    0
    ## 10   -1    -9    11    6
    ## 11   10    16    10   -7
    ## 12   13    -2    -7    3
    ## 13    9    -3    -1  -13

``` r
melt_d = melt(as.matrix(d))
colnames(melt_d) = c("round","player","points")
```

#### </br>How much I suck

``` r
p = ggplot(melt_d,aes(x=player,y=points,fill=player)) + geom_boxplot() + 
  scale_fill_manual(values=RColorBrewer::brewer.pal(4,"Set1")[c(3,2,1,4)]) +
  ggtitle("Distribution of points per round")
p
```

![](dutch_blitz_files/figure-markdown_github/unnamed-chunk-2-1.png)

#### </br> when ($round==7) { 4\_player\_game = 2\_player\_game }

``` r
d_sum = apply(d,2,cumsum)
melt_d_sum = melt(as.matrix(d_sum))
colnames(melt_d_sum) = c("round","player","total.points")

p = ggplot(melt_d_sum,aes(x=round,y=total.points,color=player)) + geom_point() + geom_line() +
  scale_color_manual(values=RColorBrewer::brewer.pal(4,"Set1")[c(3,2,1,4)]) +
  ggtitle("Accumulated points per round")
p
```

![](dutch_blitz_files/figure-markdown_github/unnamed-chunk-3-1.png)

#### </br>Hvad hvis man spillede med skæve istedet for negative point

#### (Minus point for en spiller giver i stedet plus til alle andre)

``` r
d_test = apply(d,1,calculate_points)
d_test = t(d_test)
colnames(d_test) = colnames(d)

d_test_sum = apply(d_test,2,cumsum)
melt_d_test_sum = melt(as.matrix(d_test_sum))
colnames(melt_d_test_sum) = c("round","player","total.points")

p = ggplot(melt_d_test_sum,aes(x=round,y=total.points,color=player)) + geom_point() + geom_line() +
  scale_color_manual(values=RColorBrewer::brewer.pal(4,"Set1")[c(3,2,1,4)]) +
  ggtitle("Accumulated points using skæve")
p
```

![](dutch_blitz_files/figure-markdown_github/unnamed-chunk-5-1.png)

#### (Er jævnt besværligt at holde score på den måde)

#### Also, Claus og Mette er gode til at få point i de samme runder mens Mark som sædvanlig er på tværs af alle andre og helst vil have point når vi andre mister og miste når vi andre vinder

``` r
heatmap.2(as.matrix(t(d)), trace="none",
          col = rev(colorspace::diverge_hsv(50)),
          Colv = F, dendrogram = "row",
          main = "Points gained each round", xlab = "Round")
```

![](dutch_blitz_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
cor_mat = cor(d)
heatmap.2(as.matrix(cor_mat), trace="none",
          col = rev(colorspace::diverge_hsv(100)),
          main = "Points gained correlation")
```

![](dutch_blitz_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
p = ggplot(d,aes(x=Mette,y=Claus)) + geom_point() + 
  ggtitle("Points gained in rounds")
p
```

![](dutch_blitz_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
cor.test(d$Mette,d$Claus)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  d$Mette and d$Claus
    ## t = 1.5007, df = 11, p-value = 0.1616
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.1795275  0.7849339
    ## sample estimates:
    ##       cor 
    ## 0.4122349

``` r
p = ggplot(d,aes(x=Mark,y=Mette)) + geom_point() + 
  ggtitle("Points gained in rounds")
p
```

![](dutch_blitz_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
cor.test(d$Mark,d$Mette)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  d$Mark and d$Mette
    ## t = -2.0968, df = 11, p-value = 0.05993
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.83848434  0.02353837
    ## sample estimates:
    ##        cor 
    ## -0.5343774

#### Og til sidst, en kompliceret udregning af hvor mange runder vi skulle spille hvis jeg skulle vinde

``` r
75/2
```

    ## [1] 37.5
