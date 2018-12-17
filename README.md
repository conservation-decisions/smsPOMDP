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
### Parameters
``` r
pen = 0.1 #local probability of extinction P(extinct/extant, survey or nothing)
p0 = 1-pen #local probability of persitance P(extant/extant, manage)
pem = 0.05816 #local probability of extinction if managed P(extinct/extant, manage)
pm = 1 - pem #local probability of persistance if managed P(extant/extant, manage)
d0 = 0.01 #local probability of detection P(present/extant, manage or nothing)
d = 0.78193 #local probability of detection if surveyed P(present/extant, survey)
V = 175.133 #Estimated economic value of the species ($/yr)
Cm = 18.784 #Estimated cost of managing ($/yr)
Cs = 10.840 #Estimated cost of surveying ($/yr)

```

#
