# TigerTest
R package : solving POMDP problem When to stop managing or surveying cryptic threatened species ?

``` r
library(TigerTest)
```

## Problem definition

Our problem is defined by a state space, `states`, representing the 
current state of the species (1 for extant, 2 for extinct), an action space, `actions`
representing the lead action (1 for manage, 2 for survey, 3 for nothing), and an observation 
space, 'observations', representing the observation of the species (1 for seen, 2 for not seen)

To solve the POMDP problem of: When to stop managing or surveying cryptic threatened species ?

## Sumatran tiger example
``` r
pen = 0.1 #local probability of extinction (survey or nothing)
p0 = 1-pen #local probabili
pem = 0.05816
pm = 1 - pem
V = 175.133
Cm = 18.784
Cs = 10.840
d0 = 0.01
d = 0.78193
```
