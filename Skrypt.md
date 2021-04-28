``` r
library(data.table)
```

    ## Warning: package 'data.table' was built under R version 4.0.5

``` r
dochody <- read.csv("dane_dochody.csv", row.names = 1)
dochody <- as.data.table(dochody)
head(dochody)
```

    ##     rok doch_praca doch_dg doch_gielda
    ## 1: 2021      63601   13294       -7834
    ## 2: 2021          0       0           0
    ## 3: 2021      32547       0           0
    ## 4: 2021      47582       0           0
    ## 5: 2021      42080   52855           0
    ## 6: 2021      45063       0           0

Przydzielamy podatkników do grup podatkowych w zależności od ich dochodu
z pracy najemnej.

``` r
dochody[doch_praca < 10000, prog_doch := 1]
dochody[doch_praca >= 10000 & doch_praca < 70000, prog_doch := 2]
dochody[doch_praca >= 70000, prog_doch := 3]
head(dochody)
```

    ##     rok doch_praca doch_dg doch_gielda prog_doch
    ## 1: 2021      63601   13294       -7834         2
    ## 2: 2021          0       0           0         1
    ## 3: 2021      32547       0           0         2
    ## 4: 2021      47582       0           0         2
    ## 5: 2021      42080   52855           0         2
    ## 6: 2021      45063       0           0         2

Liczymy wysokość podatków dla poszczególnych źródeł dochodu

``` r
dochody[, pod_praca := 0]
dochody[prog_doch == 2, pod_praca := (doch_praca - 10000) * 0.2]
dochody[prog_doch == 3, pod_praca := (70000-10000) * 0.2 + (doch_praca - 70000) * 0.3]
dochody[, pod_dg := 0]
dochody[doch_dg > 0, pod_dg := doch_dg * 0.2]
dochody[, pod_gielda := 0]
dochody[doch_gielda > 0, pod_gielda := doch_gielda * 0.2]
dochody[, podatki_razem:= pod_praca + pod_dg + pod_gielda]
head(dochody)
```

    ##     rok doch_praca doch_dg doch_gielda prog_doch pod_praca  pod_dg pod_gielda
    ## 1: 2021      63601   13294       -7834         2   10720.2  2658.8          0
    ## 2: 2021          0       0           0         1       0.0     0.0          0
    ## 3: 2021      32547       0           0         2    4509.4     0.0          0
    ## 4: 2021      47582       0           0         2    7516.4     0.0          0
    ## 5: 2021      42080   52855           0         2    6416.0 10571.0          0
    ## 6: 2021      45063       0           0         2    7012.6     0.0          0
    ##    podatki_razem
    ## 1:       13379.0
    ## 2:           0.0
    ## 3:        4509.4
    ## 4:        7516.4
    ## 5:       16987.0
    ## 6:        7012.6

Zatem całkowity dochód Państwa z podatków wyniesie:

``` r
wplywy.podatki <- sum(dochody$podatki_razem, na.rm = TRUE)
wplywy.podatki
```

    ## [1] 1630164786

Teraz możemy zabrać się za reformę:)

Na początek tworzymy kolumnę z obliczonymi ulgami dla podatników.

``` r
dochody[, ulga := 0]
dochody[pod_praca >= 1000, ulga := 1000]
dochody[pod_praca < 1000, ulga := pod_praca]
```

Dodajemy kolumne obliczeniową z wysokością podatków z tytułu pracy
najmnej po reformie

``` r
dochody[, pod_praca_akt := pod_praca - ulga]
head(dochody[pod_praca<1000 & pod_praca>0])
```

    ##     rok doch_praca doch_dg doch_gielda prog_doch pod_praca  pod_dg pod_gielda
    ## 1: 2021      10723       0       -6435         2     144.6     0.0          0
    ## 2: 2021      11134       0           0         2     226.8     0.0          0
    ## 3: 2021      12739       0           0         2     547.8     0.0          0
    ## 4: 2021      14835       0           0         2     967.0     0.0          0
    ## 5: 2021      14995       0           0         2     999.0     0.0          0
    ## 6: 2021      11715   70917           0         2     343.0 14183.4          0
    ##    podatki_razem  ulga pod_praca_akt
    ## 1:         144.6 144.6             0
    ## 2:         226.8 226.8             0
    ## 3:         547.8 547.8             0
    ## 4:         967.0 967.0             0
    ## 5:         999.0 999.0             0
    ## 6:       14526.4 343.0             0

Obliczamy wpływy do Państwa z tytułu podatków po reformie

``` r
dochody[, podatki_razem_ref:= pod_praca_akt + pod_dg + pod_gielda]
wplywy.podatki.ref <- sum(dochody$podatki_razem_ref)
wplywy.podatki.ref
```

    ## [1] 1470958229

Teraz obliczamy różnicę między wpływami do reformy i po reformie.

To będzie KOSZT REFORMY.

``` r
koszt <- wplywy.podatki - wplywy.podatki.ref
koszt
```

    ## [1] 159206557

Mając koszt reformy nie trudno już policzyć kwotę, którą należy
opodatkować 40% podatkiem, żeby wpływy podatkowe pokryły koszt reformy.

40% \* x = koszt

``` r
x <- koszt * (1/0.4)
x
```

    ## [1] 398016393

Świetnie! Zostało już tylko dopasować próg podatkowy do tej kwoty.

Otwórzmy w tym celu wejściową tabelę raz jeszcze, gdyż nasza zrobiła się
już bardzo szeroka i mało czytelna, i dodajmy interesujące nas kolumny
obliczeniowe

``` r
dochody.nieujemne <- read.csv("dane_dochody.csv", row.names = 1)
dochody.nieujemne <- as.data.table(dochody.nieujemne)
```

Zerujemy komórki z wartościami ujemnymi

``` r
dochody.nieujemne[doch_dg < 0, doch_dg := 0]
dochody.nieujemne[doch_gielda < 0, doch_gielda := 0]
dochody.nieujemne[, razem := doch_praca + doch_dg + doch_gielda]
head(dochody.nieujemne)
```

    ##     rok doch_praca doch_dg doch_gielda razem
    ## 1: 2021      63601   13294           0 76895
    ## 2: 2021          0       0           0     0
    ## 3: 2021      32547       0           0 32547
    ## 4: 2021      47582       0           0 47582
    ## 5: 2021      42080   52855           0 94935
    ## 6: 2021      45063       0           0 45063

Ponieważ mamy do przetestowania ogromny zakres prawdopodobnych wysokości
dochodów, będących potencjalnymi progami podatkowymi, nie będziemy
sprawdzać każdej z nich po kolei w pętli.

Zastosujemy lecz znacznie wydajniejszy algorytm wyszukiwania binarnego.

``` r
library("gtools")
func <- function(X) sum(dochody.nieujemne[razem > X, razem - X])
odp = binsearch( func, range = 0:max(dochody.nieujemne$razem), target = x )
odp
```

    ## $call
    ## binsearch(fun = func, range = 0:max(dochody.nieujemne$razem), 
    ##     target = x)
    ## 
    ## $numiter
    ## [1] 19
    ## 
    ## $flag
    ## [1] "Between Elements"
    ## 
    ## $where
    ## [1] 103786 103787
    ## 
    ## $value
    ## [1] 398027680 398012842

Udało się! Zamiast kilkaset tysięcy iteracji program uporał się z
zadaniem robiąc tylko 19.

Kilka słów wyjaśnień:

1.  Funkcja func liczy dochody, które przekroczyły próg podany jako
    argument. przy czym sumuje tylko część powyżej tego progu (z dochodu
    120 tys. z progiem 100 tys. kwota do opodatkowanie wyniesi 20 tys.)

2.  Funkcja binsearch podstawia kolejne prawdopodobne progi podatkowe z
    zakresu od 0 do maksymalnej wysokości dochodu osiągniętego przez co
    najmniej jednego podatnika aż do momentu kiedy znajdzie odpowiedni
    próg.

W naszym przypadku wynosi on 1.03786^{5} jednostek pieniężnych.
