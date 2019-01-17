Import Data
-----------

``` r
#Install and load all necessary packages
#install.packages("MASS")
library(MASS)

#Load the data
ppra.2015 <- read.csv("2015_master.csv")
ppra.2014 <- read.csv("2014_master.csv")
df.yield <- read.csv("combined_year_yield.csv")
df.SDS.DX <- read.csv("combined_year_DX.csv")
df.yield$Year <- as.factor(df.yield$Year)
df.SDS.DX$Year <- as.factor(df.SDS.DX$Year)
```

Multiple Regression
-------------------

``` r
#Determine n, number of observations
n <- length(df.yield[,1])

#Generate the full linear model containing the whole data set
full.fit <- lm(Yield ~ Year+
                      Pre.ratio+
                      PreSCN.cysts+ 
                      PreSCN.eggs+
                      PreSCN.juvs+ 
                      Pre.spiral+  
                      #Pre.lesion+
                      #Pre.dagger+
                      V3.qPCR+
                      V3.DW+
                      V3.foliar+
                      V3.root+
                      R5.DW+
                      R5.foliar+
                      R5.root+
                      R5.qPCR+      
                      R5.DX,
                      #V3.Phi2+
                      #V3.PhiNPQ+
                      #V3.PhiNO+
                      #V3.SPAD+
                      #V3.NPQt+
                      #V3.LEF+
                      #V3.qL+
                      #R1.Phi2+
                      #R1.PhiNPQ+
                      #R1.PhiNO+
                      #R1.SPAD+
                      #R1.NPQt+
                      #R1.LEF+
                      #R1.qL,
                      data = df.yield)
#Determine best version of each model using stepwise AIC and BIC
best.AIC.full <- stepAIC(full.fit, direction = "both")
```

    ## Start:  AIC=238.79
    ## Yield ~ Year + Pre.ratio + PreSCN.cysts + PreSCN.eggs + PreSCN.juvs + 
    ##     Pre.spiral + V3.qPCR + V3.DW + V3.foliar + V3.root + R5.DW + 
    ##     R5.foliar + R5.root + R5.qPCR + R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - R5.qPCR       1     0.011 1272.5 236.79
    ## - PreSCN.eggs   1     0.220 1272.7 236.80
    ## - PreSCN.cysts  1     3.474 1276.0 236.99
    ## - V3.DW         1     4.050 1276.6 237.02
    ## - R5.root       1     7.719 1280.2 237.23
    ## - V3.qPCR       1     8.341 1280.9 237.26
    ## - Pre.spiral    1    25.130 1297.7 238.20
    ## - V3.root       1    25.441 1298.0 238.22
    ## - V3.foliar     1    29.279 1301.8 238.43
    ## - PreSCN.juvs   1    29.697 1302.2 238.45
    ## <none>                      1272.5 238.79
    ## - Pre.ratio     1    74.019 1346.5 240.86
    ## - R5.foliar     1    78.076 1350.6 241.08
    ## - R5.DW         1    83.347 1355.9 241.36
    ## - Year          1    93.497 1366.0 241.90
    ## - R5.DX         1   224.918 1497.4 248.51
    ## 
    ## Step:  AIC=236.79
    ## Yield ~ Year + Pre.ratio + PreSCN.cysts + PreSCN.eggs + PreSCN.juvs + 
    ##     Pre.spiral + V3.qPCR + V3.DW + V3.foliar + V3.root + R5.DW + 
    ##     R5.foliar + R5.root + R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - PreSCN.eggs   1     0.226 1272.8 234.80
    ## - PreSCN.cysts  1     3.491 1276.0 234.99
    ## - V3.DW         1     4.045 1276.6 235.02
    ## - R5.root       1     7.709 1280.2 235.23
    ## - V3.qPCR       1     8.580 1281.1 235.28
    ## - Pre.spiral    1    25.241 1297.8 236.21
    ## - V3.root       1    28.054 1300.6 236.36
    ## - V3.foliar     1    30.401 1302.9 236.49
    ## - PreSCN.juvs   1    30.462 1303.0 236.49
    ## <none>                      1272.5 236.79
    ## + R5.qPCR       1     0.011 1272.5 238.79
    ## - R5.foliar     1    79.600 1352.1 239.16
    ## - R5.DW         1    83.755 1356.3 239.38
    ## - Pre.ratio     1    92.218 1364.8 239.83
    ## - Year          1   100.598 1373.1 240.27
    ## - R5.DX         1   236.438 1509.0 247.06
    ## 
    ## Step:  AIC=234.8
    ## Yield ~ Year + Pre.ratio + PreSCN.cysts + PreSCN.juvs + Pre.spiral + 
    ##     V3.qPCR + V3.DW + V3.foliar + V3.root + R5.DW + R5.foliar + 
    ##     R5.root + R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - V3.DW         1     4.014 1276.8 233.03
    ## - R5.root       1     7.771 1280.5 233.24
    ## - V3.qPCR       1     9.761 1282.5 233.35
    ## - PreSCN.cysts  1    15.423 1288.2 233.67
    ## - Pre.spiral    1    25.074 1297.8 234.21
    ## - V3.root       1    27.862 1300.6 234.36
    ## - PreSCN.juvs   1    30.841 1303.6 234.53
    ## - V3.foliar     1    32.959 1305.7 234.64
    ## <none>                      1272.8 234.80
    ## + PreSCN.eggs   1     0.226 1272.5 236.79
    ## + R5.qPCR       1     0.016 1272.7 236.80
    ## - R5.foliar     1    86.357 1359.1 237.53
    ## - R5.DW         1    88.931 1361.7 237.67
    ## - Pre.ratio     1    97.545 1370.3 238.12
    ## - Year          1   102.100 1374.8 238.36
    ## - R5.DX         1   250.899 1523.7 245.76
    ## 
    ## Step:  AIC=233.03
    ## Yield ~ Year + Pre.ratio + PreSCN.cysts + PreSCN.juvs + Pre.spiral + 
    ##     V3.qPCR + V3.foliar + V3.root + R5.DW + R5.foliar + R5.root + 
    ##     R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - V3.qPCR       1     7.994 1284.8 231.48
    ## - R5.root       1    10.201 1287.0 231.60
    ## - PreSCN.cysts  1    16.040 1292.8 231.93
    ## - V3.root       1    24.010 1300.8 232.37
    ## - Pre.spiral    1    27.416 1304.2 232.56
    ## - PreSCN.juvs   1    30.613 1307.4 232.74
    ## <none>                      1276.8 233.03
    ## - V3.foliar     1    36.221 1313.0 233.04
    ## + V3.DW         1     4.014 1272.8 234.80
    ## + PreSCN.eggs   1     0.195 1276.6 235.02
    ## + R5.qPCR       1     0.002 1276.8 235.03
    ## - R5.foliar     1    82.346 1359.1 235.53
    ## - R5.DW         1    86.263 1363.0 235.74
    ## - Pre.ratio     1    94.595 1371.4 236.18
    ## - Year          1    98.183 1375.0 236.36
    ## - R5.DX         1   263.293 1540.1 244.53
    ## 
    ## Step:  AIC=231.48
    ## Yield ~ Year + Pre.ratio + PreSCN.cysts + PreSCN.juvs + Pre.spiral + 
    ##     V3.foliar + V3.root + R5.DW + R5.foliar + R5.root + R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - R5.root       1    10.289 1295.0 230.05
    ## - PreSCN.cysts  1    13.218 1298.0 230.22
    ## - Pre.spiral    1    22.409 1307.2 230.72
    ## - PreSCN.juvs   1    26.392 1311.2 230.94
    ## - V3.root       1    27.126 1311.9 230.98
    ## - V3.foliar     1    30.623 1315.4 231.18
    ## <none>                      1284.8 231.48
    ## + V3.qPCR       1     7.994 1276.8 233.03
    ## + V3.DW         1     2.247 1282.5 233.35
    ## + PreSCN.eggs   1     1.211 1283.5 233.41
    ## + R5.qPCR       1     0.177 1284.6 233.47
    ## - R5.DW         1    79.475 1364.2 233.80
    ## - R5.foliar     1    83.399 1368.2 234.01
    ## - Pre.ratio     1    87.626 1372.4 234.23
    ## - Year          1    93.148 1377.9 234.52
    ## - R5.DX         1   255.754 1540.5 242.55
    ## 
    ## Step:  AIC=230.05
    ## Yield ~ Year + Pre.ratio + PreSCN.cysts + PreSCN.juvs + Pre.spiral + 
    ##     V3.foliar + V3.root + R5.DW + R5.foliar + R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - PreSCN.cysts  1    13.095 1308.2 228.78
    ## - Pre.spiral    1    18.903 1314.0 229.10
    ## - PreSCN.juvs   1    30.083 1325.1 229.71
    ## - V3.foliar     1    31.389 1326.4 229.78
    ## - V3.root       1    35.755 1330.8 230.01
    ## <none>                      1295.0 230.05
    ## + R5.root       1    10.289 1284.8 231.48
    ## + V3.qPCR       1     8.083 1287.0 231.60
    ## + V3.DW         1     4.141 1290.9 231.82
    ## + R5.qPCR       1     0.037 1295.0 232.05
    ## + PreSCN.eggs   1     0.008 1295.0 232.05
    ## - Pre.ratio     1    80.972 1376.0 232.42
    ## - Year          1    83.209 1378.3 232.54
    ## - R5.foliar     1    90.931 1386.0 232.94
    ## - R5.DW         1   158.904 1454.0 236.39
    ## - R5.DX         1   253.811 1548.9 240.94
    ## 
    ## Step:  AIC=228.78
    ## Yield ~ Year + Pre.ratio + PreSCN.juvs + Pre.spiral + V3.foliar + 
    ##     V3.root + R5.DW + R5.foliar + R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - Pre.spiral    1    12.793 1320.9 227.48
    ## - PreSCN.juvs   1    20.595 1328.7 227.90
    ## - V3.foliar     1    29.240 1337.4 228.37
    ## - V3.root       1    29.831 1338.0 228.40
    ## <none>                      1308.2 228.78
    ## + PreSCN.cysts  1    13.095 1295.0 230.05
    ## + R5.root       1    10.166 1298.0 230.22
    ## + PreSCN.eggs   1     7.814 1300.3 230.35
    ## + V3.qPCR       1     5.254 1302.9 230.49
    ## + V3.DW         1     5.052 1303.1 230.50
    ## + R5.qPCR       1     0.308 1307.8 230.76
    ## - Year          1    88.678 1396.8 231.50
    ## - Pre.ratio     1    89.129 1397.3 231.52
    ## - R5.foliar     1    89.210 1397.4 231.53
    ## - R5.DW         1   149.280 1457.4 234.56
    ## - R5.DX         1   254.631 1562.8 239.58
    ## 
    ## Step:  AIC=227.48
    ## Yield ~ Year + Pre.ratio + PreSCN.juvs + V3.foliar + V3.root + 
    ##     R5.DW + R5.foliar + R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - PreSCN.juvs   1    23.148 1344.1 226.73
    ## - V3.foliar     1    28.812 1349.8 227.03
    ## <none>                      1320.9 227.48
    ## - V3.root       1    44.114 1365.0 227.84
    ## + Pre.spiral    1    12.793 1308.2 228.78
    ## + R5.root       1     7.164 1313.8 229.09
    ## + PreSCN.cysts  1     6.985 1314.0 229.10
    ## + V3.DW         1     6.791 1314.2 229.11
    ## + PreSCN.eggs   1     2.896 1318.0 229.32
    ## + V3.qPCR       1     2.458 1318.5 229.34
    ## + R5.qPCR       1     0.744 1320.2 229.44
    ## - Year          1    75.901 1396.8 229.50
    ## - R5.foliar     1    80.777 1401.7 229.75
    ## - Pre.ratio     1    81.920 1402.9 229.81
    ## - R5.DW         1   141.118 1462.1 232.79
    ## - R5.DX         1   247.373 1568.3 237.84
    ## 
    ## Step:  AIC=226.73
    ## Yield ~ Year + Pre.ratio + V3.foliar + V3.root + R5.DW + R5.foliar + 
    ##     R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - V3.foliar     1    21.284 1365.4 225.86
    ## <none>                      1344.1 226.73
    ## + PreSCN.juvs   1    23.148 1320.9 227.48
    ## - Year          1    59.269 1403.4 227.84
    ## + Pre.spiral    1    15.346 1328.7 227.90
    ## + R5.root       1    12.294 1331.8 228.07
    ## - V3.root       1    63.902 1408.0 228.07
    ## - Pre.ratio     1    67.187 1411.3 228.24
    ## + PreSCN.eggs   1     7.200 1336.9 228.34
    ## + PreSCN.cysts  1     6.846 1337.2 228.36
    ## + V3.DW         1     6.260 1337.8 228.39
    ## + R5.qPCR       1     1.429 1342.7 228.65
    ## + V3.qPCR       1     1.225 1342.9 228.66
    ## - R5.foliar     1    96.048 1440.1 229.70
    ## - R5.DW         1   147.046 1491.1 232.21
    ## - R5.DX         1   250.985 1595.1 237.06
    ## 
    ## Step:  AIC=225.86
    ## Yield ~ Year + Pre.ratio + V3.root + R5.DW + R5.foliar + R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## <none>                      1365.4 225.86
    ## - V3.root       1    43.205 1408.6 226.10
    ## + V3.foliar     1    21.284 1344.1 226.73
    ## + PreSCN.juvs   1    15.619 1349.8 227.03
    ## + Pre.spiral    1    14.485 1350.9 227.09
    ## - Pre.ratio     1    63.418 1428.8 227.13
    ## + R5.root       1    12.009 1353.4 227.22
    ## + V3.DW         1    10.218 1355.2 227.32
    ## + R5.qPCR       1     4.917 1360.5 227.60
    ## + PreSCN.cysts  1     4.248 1361.1 227.64
    ## + PreSCN.eggs   1     3.622 1361.8 227.67
    ## + V3.qPCR       1     0.043 1365.3 227.86
    ## - Year          1    85.204 1450.6 228.22
    ## - R5.foliar     1    87.203 1452.6 228.32
    ## - R5.DW         1   167.794 1533.2 232.21
    ## - R5.DX         1   248.168 1613.5 235.88

``` r
best.BIC.full <- stepAIC(full.fit, direction = "both", k=log(n))
```

    ## Start:  AIC=275.22
    ## Yield ~ Year + Pre.ratio + PreSCN.cysts + PreSCN.eggs + PreSCN.juvs + 
    ##     Pre.spiral + V3.qPCR + V3.DW + V3.foliar + V3.root + R5.DW + 
    ##     R5.foliar + R5.root + R5.qPCR + R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - R5.qPCR       1     0.011 1272.5 270.94
    ## - PreSCN.eggs   1     0.220 1272.7 270.95
    ## - PreSCN.cysts  1     3.474 1276.0 271.14
    ## - V3.DW         1     4.050 1276.6 271.17
    ## - R5.root       1     7.719 1280.2 271.38
    ## - V3.qPCR       1     8.341 1280.9 271.41
    ## - Pre.spiral    1    25.130 1297.7 272.35
    ## - V3.root       1    25.441 1298.0 272.37
    ## - V3.foliar     1    29.279 1301.8 272.58
    ## - PreSCN.juvs   1    29.697 1302.2 272.60
    ## - Pre.ratio     1    74.019 1346.5 275.01
    ## <none>                      1272.5 275.22
    ## - R5.foliar     1    78.076 1350.6 275.23
    ## - R5.DW         1    83.347 1355.9 275.51
    ## - Year          1    93.497 1366.0 276.05
    ## - R5.DX         1   224.918 1497.4 282.66
    ## 
    ## Step:  AIC=270.94
    ## Yield ~ Year + Pre.ratio + PreSCN.cysts + PreSCN.eggs + PreSCN.juvs + 
    ##     Pre.spiral + V3.qPCR + V3.DW + V3.foliar + V3.root + R5.DW + 
    ##     R5.foliar + R5.root + R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - PreSCN.eggs   1     0.226 1272.8 266.68
    ## - PreSCN.cysts  1     3.491 1276.0 266.86
    ## - V3.DW         1     4.045 1276.6 266.89
    ## - R5.root       1     7.709 1280.2 267.10
    ## - V3.qPCR       1     8.580 1281.1 267.15
    ## - Pre.spiral    1    25.241 1297.8 268.08
    ## - V3.root       1    28.054 1300.6 268.23
    ## - V3.foliar     1    30.401 1302.9 268.36
    ## - PreSCN.juvs   1    30.462 1303.0 268.37
    ## <none>                      1272.5 270.94
    ## - R5.foliar     1    79.600 1352.1 271.03
    ## - R5.DW         1    83.755 1356.3 271.25
    ## - Pre.ratio     1    92.218 1364.8 271.70
    ## - Year          1   100.598 1373.1 272.14
    ## + R5.qPCR       1     0.011 1272.5 275.22
    ## - R5.DX         1   236.438 1509.0 278.93
    ## 
    ## Step:  AIC=266.68
    ## Yield ~ Year + Pre.ratio + PreSCN.cysts + PreSCN.juvs + Pre.spiral + 
    ##     V3.qPCR + V3.DW + V3.foliar + V3.root + R5.DW + R5.foliar + 
    ##     R5.root + R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - V3.DW         1     4.014 1276.8 262.63
    ## - R5.root       1     7.771 1280.5 262.84
    ## - V3.qPCR       1     9.761 1282.5 262.95
    ## - PreSCN.cysts  1    15.423 1288.2 263.27
    ## - Pre.spiral    1    25.074 1297.8 263.81
    ## - V3.root       1    27.862 1300.6 263.96
    ## - PreSCN.juvs   1    30.841 1303.6 264.12
    ## - V3.foliar     1    32.959 1305.7 264.24
    ## <none>                      1272.8 266.68
    ## - R5.foliar     1    86.357 1359.1 267.13
    ## - R5.DW         1    88.931 1361.7 267.26
    ## - Pre.ratio     1    97.545 1370.3 267.72
    ## - Year          1   102.100 1374.8 267.96
    ## + PreSCN.eggs   1     0.226 1272.5 270.94
    ## + R5.qPCR       1     0.016 1272.7 270.95
    ## - R5.DX         1   250.899 1523.7 275.36
    ## 
    ## Step:  AIC=262.63
    ## Yield ~ Year + Pre.ratio + PreSCN.cysts + PreSCN.juvs + Pre.spiral + 
    ##     V3.qPCR + V3.foliar + V3.root + R5.DW + R5.foliar + R5.root + 
    ##     R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - V3.qPCR       1     7.994 1284.8 258.80
    ## - R5.root       1    10.201 1287.0 258.92
    ## - PreSCN.cysts  1    16.040 1292.8 259.25
    ## - V3.root       1    24.010 1300.8 259.69
    ## - Pre.spiral    1    27.416 1304.2 259.88
    ## - PreSCN.juvs   1    30.613 1307.4 260.06
    ## - V3.foliar     1    36.221 1313.0 260.36
    ## <none>                      1276.8 262.63
    ## - R5.foliar     1    82.346 1359.1 262.85
    ## - R5.DW         1    86.263 1363.0 263.06
    ## - Pre.ratio     1    94.595 1371.4 263.50
    ## - Year          1    98.183 1375.0 263.68
    ## + V3.DW         1     4.014 1272.8 266.68
    ## + PreSCN.eggs   1     0.195 1276.6 266.89
    ## + R5.qPCR       1     0.002 1276.8 266.90
    ## - R5.DX         1   263.293 1540.1 271.85
    ## 
    ## Step:  AIC=258.8
    ## Yield ~ Year + Pre.ratio + PreSCN.cysts + PreSCN.juvs + Pre.spiral + 
    ##     V3.foliar + V3.root + R5.DW + R5.foliar + R5.root + R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - R5.root       1    10.289 1295.0 255.10
    ## - PreSCN.cysts  1    13.218 1298.0 255.26
    ## - Pre.spiral    1    22.409 1307.2 255.77
    ## - PreSCN.juvs   1    26.392 1311.2 255.99
    ## - V3.root       1    27.126 1311.9 256.03
    ## - V3.foliar     1    30.623 1315.4 256.22
    ## <none>                      1284.8 258.80
    ## - R5.DW         1    79.475 1364.2 258.85
    ## - R5.foliar     1    83.399 1368.2 259.05
    ## - Pre.ratio     1    87.626 1372.4 259.27
    ## - Year          1    93.148 1377.9 259.56
    ## + V3.qPCR       1     7.994 1276.8 262.63
    ## + V3.DW         1     2.247 1282.5 262.95
    ## + PreSCN.eggs   1     1.211 1283.5 263.01
    ## + R5.qPCR       1     0.177 1284.6 263.07
    ## - R5.DX         1   255.754 1540.5 267.59
    ## 
    ## Step:  AIC=255.1
    ## Yield ~ Year + Pre.ratio + PreSCN.cysts + PreSCN.juvs + Pre.spiral + 
    ##     V3.foliar + V3.root + R5.DW + R5.foliar + R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - PreSCN.cysts  1    13.095 1308.2 251.54
    ## - Pre.spiral    1    18.903 1314.0 251.86
    ## - PreSCN.juvs   1    30.083 1325.1 252.47
    ## - V3.foliar     1    31.389 1326.4 252.54
    ## - V3.root       1    35.755 1330.8 252.78
    ## <none>                      1295.0 255.10
    ## - Pre.ratio     1    80.972 1376.0 255.19
    ## - Year          1    83.209 1378.3 255.30
    ## - R5.foliar     1    90.931 1386.0 255.71
    ## + R5.root       1    10.289 1284.8 258.80
    ## + V3.qPCR       1     8.083 1287.0 258.92
    ## + V3.DW         1     4.141 1290.9 259.14
    ## - R5.DW         1   158.904 1454.0 259.15
    ## + R5.qPCR       1     0.037 1295.0 259.37
    ## + PreSCN.eggs   1     0.008 1295.0 259.37
    ## - R5.DX         1   253.811 1548.9 263.71
    ## 
    ## Step:  AIC=251.55
    ## Yield ~ Year + Pre.ratio + PreSCN.juvs + Pre.spiral + V3.foliar + 
    ##     V3.root + R5.DW + R5.foliar + R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - Pre.spiral    1    12.793 1320.9 247.97
    ## - PreSCN.juvs   1    20.595 1328.7 248.39
    ## - V3.foliar     1    29.240 1337.4 248.86
    ## - V3.root       1    29.831 1338.0 248.89
    ## <none>                      1308.2 251.54
    ## - Year          1    88.678 1396.8 251.99
    ## - Pre.ratio     1    89.129 1397.3 252.01
    ## - R5.foliar     1    89.210 1397.4 252.02
    ## - R5.DW         1   149.280 1457.4 255.05
    ## + PreSCN.cysts  1    13.095 1295.0 255.10
    ## + R5.root       1    10.166 1298.0 255.26
    ## + PreSCN.eggs   1     7.814 1300.3 255.39
    ## + V3.qPCR       1     5.254 1302.9 255.53
    ## + V3.DW         1     5.052 1303.1 255.54
    ## + R5.qPCR       1     0.308 1307.8 255.81
    ## - R5.DX         1   254.631 1562.8 260.07
    ## 
    ## Step:  AIC=247.97
    ## Yield ~ Year + Pre.ratio + PreSCN.juvs + V3.foliar + V3.root + 
    ##     R5.DW + R5.foliar + R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - PreSCN.juvs   1    23.148 1344.1 244.94
    ## - V3.foliar     1    28.812 1349.8 245.25
    ## - V3.root       1    44.114 1365.0 246.06
    ## - Year          1    75.901 1396.8 247.72
    ## - R5.foliar     1    80.777 1401.7 247.97
    ## <none>                      1320.9 247.97
    ## - Pre.ratio     1    81.920 1402.9 248.03
    ## - R5.DW         1   141.118 1462.1 251.00
    ## + Pre.spiral    1    12.793 1308.2 251.54
    ## + R5.root       1     7.164 1313.8 251.85
    ## + PreSCN.cysts  1     6.985 1314.0 251.86
    ## + V3.DW         1     6.791 1314.2 251.88
    ## + PreSCN.eggs   1     2.896 1318.0 252.09
    ## + V3.qPCR       1     2.458 1318.5 252.11
    ## + R5.qPCR       1     0.744 1320.2 252.21
    ## - R5.DX         1   247.373 1568.3 256.05
    ## 
    ## Step:  AIC=244.94
    ## Yield ~ Year + Pre.ratio + V3.foliar + V3.root + R5.DW + R5.foliar + 
    ##     R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - V3.foliar     1    21.284 1365.4 241.80
    ## - Year          1    59.269 1403.4 243.77
    ## - V3.root       1    63.902 1408.0 244.01
    ## - Pre.ratio     1    67.187 1411.3 244.18
    ## <none>                      1344.1 244.94
    ## - R5.foliar     1    96.048 1440.1 245.64
    ## + PreSCN.juvs   1    23.148 1320.9 247.97
    ## - R5.DW         1   147.046 1491.1 248.14
    ## + Pre.spiral    1    15.346 1328.7 248.39
    ## + R5.root       1    12.294 1331.8 248.56
    ## + PreSCN.eggs   1     7.200 1336.9 248.83
    ## + PreSCN.cysts  1     6.846 1337.2 248.85
    ## + V3.DW         1     6.260 1337.8 248.88
    ## + R5.qPCR       1     1.429 1342.7 249.14
    ## + V3.qPCR       1     1.225 1342.9 249.15
    ## - R5.DX         1   250.985 1595.1 252.99
    ## 
    ## Step:  AIC=241.8
    ## Yield ~ Year + Pre.ratio + V3.root + R5.DW + R5.foliar + R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - V3.root       1    43.205 1408.6 239.76
    ## - Pre.ratio     1    63.418 1428.8 240.79
    ## <none>                      1365.4 241.80
    ## - Year          1    85.204 1450.6 241.88
    ## - R5.foliar     1    87.203 1452.6 241.98
    ## + V3.foliar     1    21.284 1344.1 244.94
    ## + PreSCN.juvs   1    15.619 1349.8 245.25
    ## + Pre.spiral    1    14.485 1350.9 245.31
    ## + R5.root       1    12.009 1353.4 245.44
    ## + V3.DW         1    10.218 1355.2 245.53
    ## + R5.qPCR       1     4.917 1360.5 245.81
    ## + PreSCN.cysts  1     4.248 1361.1 245.85
    ## - R5.DW         1   167.794 1533.2 245.87
    ## + PreSCN.eggs   1     3.622 1361.8 245.88
    ## + V3.qPCR       1     0.043 1365.3 246.07
    ## - R5.DX         1   248.168 1613.5 249.54
    ## 
    ## Step:  AIC=239.76
    ## Yield ~ Year + Pre.ratio + R5.DW + R5.foliar + R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - Pre.ratio     1    68.403 1477.0 238.90
    ## - Year          1    72.635 1481.2 239.11
    ## <none>                      1408.6 239.76
    ## - R5.foliar     1    94.233 1502.8 240.15
    ## + V3.root       1    43.205 1365.4 241.80
    ## + PreSCN.juvs   1    35.877 1372.7 242.18
    ## + Pre.spiral    1    33.558 1375.0 242.31
    ## + R5.root       1    25.253 1383.3 242.74
    ## + PreSCN.eggs   1    21.558 1387.0 242.93
    ## + PreSCN.cysts  1    20.796 1387.8 242.97
    ## + R5.qPCR       1    14.834 1393.7 243.28
    ## + V3.qPCR       1     1.744 1406.8 243.95
    ## + V3.foliar     1     0.586 1408.0 244.01
    ## + V3.DW         1     0.069 1408.5 244.04
    ## - R5.DW         1   235.948 1644.5 246.64
    ## - R5.DX         1   270.351 1678.9 248.13
    ## 
    ## Step:  AIC=238.9
    ## Yield ~ Year + R5.DW + R5.foliar + R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - Year          1     6.884 1483.9 234.96
    ## - R5.foliar     1    52.391 1529.4 237.13
    ## <none>                      1477.0 238.90
    ## + Pre.ratio     1    68.403 1408.6 239.76
    ## + R5.qPCR       1    51.010 1426.0 240.65
    ## + V3.root       1    48.189 1428.8 240.79
    ## + Pre.spiral    1    22.208 1454.8 242.09
    ## + PreSCN.juvs   1    19.355 1457.6 242.23
    ## + R5.root       1    13.974 1463.0 242.49
    ## + PreSCN.cysts  1     7.321 1469.7 242.82
    ## + PreSCN.eggs   1     5.843 1471.1 242.89
    ## + V3.DW         1     0.460 1476.5 243.16
    ## + V3.foliar     1     0.039 1476.9 243.18
    ## + V3.qPCR       1     0.021 1477.0 243.18
    ## - R5.DW         1   228.770 1705.8 244.99
    ## - R5.DX         1   230.408 1707.4 245.06
    ## 
    ## Step:  AIC=234.96
    ## Yield ~ R5.DW + R5.foliar + R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - R5.foliar     1    54.056 1537.9 233.26
    ## <none>                      1483.9 234.96
    ## + R5.qPCR       1    39.900 1444.0 237.27
    ## + V3.root       1    33.280 1450.6 237.60
    ## + PreSCN.juvs   1     9.665 1474.2 238.77
    ## + Year          1     6.884 1477.0 238.90
    ## + R5.root       1     5.693 1478.2 238.96
    ## + Pre.spiral    1     4.035 1479.8 239.04
    ## + V3.foliar     1     2.985 1480.9 239.09
    ## + PreSCN.eggs   1     2.683 1481.2 239.11
    ## + Pre.ratio     1     2.652 1481.2 239.11
    ## + PreSCN.cysts  1     2.151 1481.7 239.13
    ## + V3.DW         1     1.002 1482.9 239.19
    ## + V3.qPCR       1     0.068 1483.8 239.23
    ## - R5.DW         1   231.515 1715.4 241.12
    ## - R5.DX         1   232.354 1716.2 241.16
    ## 
    ## Step:  AIC=233.26
    ## Yield ~ R5.DW + R5.DX
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## <none>                      1537.9 233.26
    ## + R5.foliar     1     54.06 1483.9 234.96
    ## + V3.root       1     35.46 1502.5 235.86
    ## + PreSCN.juvs   1     21.31 1516.6 236.53
    ## + R5.root       1     14.27 1523.7 236.87
    ## + R5.qPCR       1     13.44 1524.5 236.90
    ## + Year          1      8.55 1529.4 237.13
    ## + PreSCN.cysts  1      8.08 1529.8 237.16
    ## + PreSCN.eggs   1      7.15 1530.8 237.20
    ## + V3.DW         1      6.31 1531.6 237.24
    ## + Pre.spiral    1      1.77 1536.2 237.45
    ## + V3.foliar     1      1.61 1536.3 237.46
    ## + V3.qPCR       1      1.46 1536.5 237.47
    ## + Pre.ratio     1      0.00 1537.9 237.54
    ## - R5.DW         1    267.77 1805.7 240.54
    ## - R5.DX         1    597.86 2135.8 252.63

``` r
#Check out the models before and after selection
summary(best.AIC.full)
```

    ## 
    ## Call:
    ## lm(formula = Yield ~ Year + Pre.ratio + V3.root + R5.DW + R5.foliar + 
    ##     R5.DX, data = df.yield)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.7980  -2.2146  -0.3189   2.5004   9.8488 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.034e+01  3.542e+00   8.567 2.88e-12 ***
    ## Year2015    -5.317e+00  2.640e+00  -2.014  0.04815 *  
    ## Pre.ratio    1.079e+06  6.211e+05   1.738  0.08703 .  
    ## V3.root     -6.850e-02  4.777e-02  -1.434  0.15632    
    ## R5.DW        1.742e+00  6.164e-01   2.826  0.00625 ** 
    ## R5.foliar   -2.578e+00  1.265e+00  -2.038  0.04567 *  
    ## R5.DX       -2.876e-01  8.367e-02  -3.437  0.00103 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.583 on 65 degrees of freedom
    ## Multiple R-squared:  0.5953, Adjusted R-squared:  0.558 
    ## F-statistic: 15.94 on 6 and 65 DF,  p-value: 3.634e-11

``` r
summary(best.BIC.full)
```

    ## 
    ## Call:
    ## lm(formula = Yield ~ R5.DW + R5.DX, data = df.yield)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.5384  -2.3318   0.4424   2.2014  11.0966 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 26.00298    2.62922   9.890 7.35e-15 ***
    ## R5.DW        1.95085    0.56284   3.466 0.000913 ***
    ## R5.DX       -0.35056    0.06769  -5.179 2.10e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.721 on 69 degrees of freedom
    ## Multiple R-squared:  0.5442, Adjusted R-squared:  0.531 
    ## F-statistic: 41.19 on 2 and 69 DF,  p-value: 1.693e-12

``` r
best.BIC.full$anova
```

    ## Stepwise Model Path 
    ## Analysis of Deviance Table
    ## 
    ## Initial Model:
    ## Yield ~ Year + Pre.ratio + PreSCN.cysts + PreSCN.eggs + PreSCN.juvs + 
    ##     Pre.spiral + V3.qPCR + V3.DW + V3.foliar + V3.root + R5.DW + 
    ##     R5.foliar + R5.root + R5.qPCR + R5.DX
    ## 
    ## Final Model:
    ## Yield ~ R5.DW + R5.DX
    ## 
    ## 
    ##              Step Df   Deviance Resid. Df Resid. Dev      AIC
    ## 1                                      56   1272.517 275.2168
    ## 2       - R5.qPCR  1  0.0109113        57   1272.528 270.9408
    ## 3   - PreSCN.eggs  1  0.2257316        58   1272.753 266.6769
    ## 4         - V3.DW  1  4.0141597        59   1276.768 262.6269
    ## 5       - V3.qPCR  1  7.9939511        60   1284.762 258.7997
    ## 6       - R5.root  1 10.2893800        61   1295.051 255.0973
    ## 7  - PreSCN.cysts  1 13.0951761        62   1308.146 251.5451
    ## 8    - Pre.spiral  1 12.7929996        63   1320.939 247.9691
    ## 9   - PreSCN.juvs  1 23.1476189        64   1344.087 244.9432
    ## 10    - V3.foliar  1 21.2836926        65   1365.370 241.7977
    ## 11      - V3.root  1 43.2046710        66   1408.575 239.7641
    ## 12    - Pre.ratio  1 68.4031947        67   1476.978 238.9016
    ## 13         - Year  1  6.8835436        68   1483.862 234.9597
    ## 14    - R5.foliar  1 54.0556706        69   1537.917 233.2593

``` r
best.BIC.full$call
```

    ## lm(formula = Yield ~ R5.DW + R5.DX, data = df.yield)

``` r
#Best model is lm(formula = Yield ~ R5.DW + R5.DX, data = df.yield)
#Check to see if adding the interaction term R5.DW:R5.DX helps
best.BIC <- lm(Yield ~ R5.DX*R5.DW, data = df.yield)
summary(best.BIC)
```

    ## 
    ## Call:
    ## lm(formula = Yield ~ R5.DX * R5.DW, data = df.yield)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.5476  -2.4654   0.3954   2.6070  11.3465 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 26.82876    2.84829   9.419 5.94e-14 ***
    ## R5.DX       -0.49456    0.19960  -2.478   0.0157 *  
    ## R5.DW        1.70712    0.64777   2.635   0.0104 *  
    ## R5.DX:R5.DW  0.04804    0.06262   0.767   0.4456    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.735 on 68 degrees of freedom
    ## Multiple R-squared:  0.5481, Adjusted R-squared:  0.5281 
    ## F-statistic: 27.49 on 3 and 68 DF,  p-value: 9.32e-12

``` r
#Adding the interaction term doesn't help.
```

``` r
#Like in previous analysis, convert the units of Fv DNA to fg / ng
df.SDS.DX$Pre.ratio <- df.SDS.DX$Pre.ratio * 1000000
#Determine n, number of observations
n <- length(df.SDS.DX[,1])

#Remove non-predictive variables (Anything R5)
R5.DX.fit <- lm(R5.DX ~ Year +
                         V3.DW+
                         #R5.DW+
                         V3.foliar+
                         V3.root+
                         #R5.foliar+
                         #R5.root+
                         #R5.DX+
                         Pre.ratio+
                         PreSCN.cysts+ 
                         PreSCN.eggs+
                         PreSCN.juvs+ 
                         Pre.spiral+
                         V3.qPCR,
                         #R5.qPCR
                         data = df.SDS.DX)
#Determine the best version of each model using stepwise AIC and BIC
best.AIC.DX <- stepAIC(R5.DX.fit, direction = "both")
```

    ## Start:  AIC=522.86
    ## R5.DX ~ Year + V3.DW + V3.foliar + V3.root + Pre.ratio + PreSCN.cysts + 
    ##     PreSCN.eggs + PreSCN.juvs + Pre.spiral + V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - V3.DW         1      3.23 7799.0 520.91
    ## - PreSCN.cysts  1     11.09 7806.9 521.03
    ## - V3.root       1     42.03 7837.8 521.51
    ## - PreSCN.juvs   1     45.05 7840.8 521.55
    ## - PreSCN.eggs   1     95.90 7891.7 522.33
    ## <none>                      7795.8 522.86
    ## - V3.foliar     1    166.58 7962.4 523.40
    ## - Pre.spiral    1    236.67 8032.5 524.45
    ## - V3.qPCR       1    265.07 8060.9 524.87
    ## - Year          1    468.77 8264.6 527.87
    ## - Pre.ratio     1   1319.40 9115.2 539.62
    ## 
    ## Step:  AIC=520.91
    ## R5.DX ~ Year + V3.foliar + V3.root + Pre.ratio + PreSCN.cysts + 
    ##     PreSCN.eggs + PreSCN.juvs + Pre.spiral + V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - PreSCN.cysts  1     11.48 7810.5 519.09
    ## - V3.root       1     38.85 7837.9 519.51
    ## - PreSCN.juvs   1     44.26 7843.3 519.59
    ## - PreSCN.eggs   1     98.37 7897.4 520.42
    ## <none>                      7799.0 520.91
    ## - V3.foliar     1    163.45 7962.5 521.40
    ## - Pre.spiral    1    233.69 8032.7 522.45
    ## + V3.DW         1      3.23 7795.8 522.86
    ## - V3.qPCR       1    277.54 8076.6 523.11
    ## - Year          1    488.54 8287.6 526.20
    ## - Pre.ratio     1   1320.49 9119.5 537.68
    ## 
    ## Step:  AIC=519.09
    ## R5.DX ~ Year + V3.foliar + V3.root + Pre.ratio + PreSCN.eggs + 
    ##     PreSCN.juvs + Pre.spiral + V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - PreSCN.juvs   1     32.95 7843.5 517.59
    ## - V3.root       1     38.89 7849.4 517.68
    ## <none>                      7810.5 519.09
    ## - PreSCN.eggs   1    149.14 7959.6 519.36
    ## - V3.foliar     1    177.99 7988.5 519.79
    ## - Pre.spiral    1    244.28 8054.8 520.78
    ## + PreSCN.cysts  1     11.48 7799.0 520.91
    ## + V3.DW         1      3.62 7806.9 521.03
    ## - V3.qPCR       1    300.77 8111.3 521.62
    ## - Year          1    477.67 8288.2 524.21
    ## - Pre.ratio     1   1332.73 9143.2 535.99
    ## 
    ## Step:  AIC=517.59
    ## R5.DX ~ Year + V3.foliar + V3.root + Pre.ratio + PreSCN.eggs + 
    ##     Pre.spiral + V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - V3.root       1     32.81 7876.3 516.09
    ## <none>                      7843.5 517.59
    ## - V3.foliar     1    200.25 8043.7 518.62
    ## + PreSCN.juvs   1     32.95 7810.5 519.09
    ## - Pre.spiral    1    262.42 8105.9 519.54
    ## + V3.DW         1      2.50 7841.0 519.55
    ## + PreSCN.cysts  1      0.18 7843.3 519.59
    ## - V3.qPCR       1    299.91 8143.4 520.10
    ## - Year          1    546.06 8389.5 523.67
    ## - PreSCN.eggs   1    741.01 8584.5 526.43
    ## - Pre.ratio     1   1357.23 9200.7 534.74
    ## 
    ## Step:  AIC=516.09
    ## R5.DX ~ Year + V3.foliar + Pre.ratio + PreSCN.eggs + Pre.spiral + 
    ##     V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## <none>                      7876.3 516.09
    ## + V3.root       1     32.81 7843.5 517.59
    ## + PreSCN.juvs   1     26.88 7849.4 517.68
    ## - Pre.spiral    1    253.87 8130.1 517.90
    ## + PreSCN.cysts  1      0.45 7875.8 518.09
    ## + V3.DW         1      0.03 7876.2 518.09
    ## - V3.foliar     1    282.99 8159.3 518.33
    ## - V3.qPCR       1    303.31 8179.6 518.63
    ## - Year          1    546.30 8422.6 522.14
    ## - PreSCN.eggs   1    997.74 8874.0 528.41
    ## - Pre.ratio     1   1347.80 9224.1 533.05

``` r
best.BIC.DX <- stepAIC(R5.DX.fit, direction = "both", k=log(n))
```

    ## Start:  AIC=553.52
    ## R5.DX ~ Year + V3.DW + V3.foliar + V3.root + Pre.ratio + PreSCN.cysts + 
    ##     PreSCN.eggs + PreSCN.juvs + Pre.spiral + V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - V3.DW         1      3.23 7799.0 548.79
    ## - PreSCN.cysts  1     11.09 7806.9 548.91
    ## - V3.root       1     42.03 7837.8 549.38
    ## - PreSCN.juvs   1     45.05 7840.8 549.43
    ## - PreSCN.eggs   1     95.90 7891.7 550.20
    ## - V3.foliar     1    166.58 7962.4 551.27
    ## - Pre.spiral    1    236.67 8032.5 552.33
    ## - V3.qPCR       1    265.07 8060.9 552.75
    ## <none>                      7795.8 553.52
    ## - Year          1    468.77 8264.6 555.74
    ## - Pre.ratio     1   1319.40 9115.2 567.50
    ## 
    ## Step:  AIC=548.79
    ## R5.DX ~ Year + V3.foliar + V3.root + Pre.ratio + PreSCN.cysts + 
    ##     PreSCN.eggs + PreSCN.juvs + Pre.spiral + V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - PreSCN.cysts  1     11.48 7810.5 544.18
    ## - V3.root       1     38.85 7837.9 544.60
    ## - PreSCN.juvs   1     44.26 7843.3 544.68
    ## - PreSCN.eggs   1     98.37 7897.4 545.50
    ## - V3.foliar     1    163.45 7962.5 546.49
    ## - Pre.spiral    1    233.69 8032.7 547.54
    ## - V3.qPCR       1    277.54 8076.6 548.20
    ## <none>                      7799.0 548.79
    ## - Year          1    488.54 8287.6 551.29
    ## + V3.DW         1      3.23 7795.8 553.52
    ## - Pre.ratio     1   1320.49 9119.5 562.77
    ## 
    ## Step:  AIC=544.18
    ## R5.DX ~ Year + V3.foliar + V3.root + Pre.ratio + PreSCN.eggs + 
    ##     PreSCN.juvs + Pre.spiral + V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - PreSCN.juvs   1     32.95 7843.5 539.89
    ## - V3.root       1     38.89 7849.4 539.98
    ## - PreSCN.eggs   1    149.14 7959.6 541.66
    ## - V3.foliar     1    177.99 7988.5 542.09
    ## - Pre.spiral    1    244.28 8054.8 543.08
    ## - V3.qPCR       1    300.77 8111.3 543.92
    ## <none>                      7810.5 544.18
    ## - Year          1    477.67 8288.2 546.51
    ## + PreSCN.cysts  1     11.48 7799.0 548.79
    ## + V3.DW         1      3.62 7806.9 548.91
    ## - Pre.ratio     1   1332.73 9143.2 558.29
    ## 
    ## Step:  AIC=539.89
    ## R5.DX ~ Year + V3.foliar + V3.root + Pre.ratio + PreSCN.eggs + 
    ##     Pre.spiral + V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - V3.root       1     32.81 7876.3 535.61
    ## - V3.foliar     1    200.25 8043.7 538.13
    ## - Pre.spiral    1    262.42 8105.9 539.05
    ## - V3.qPCR       1    299.91 8143.4 539.61
    ## <none>                      7843.5 539.89
    ## - Year          1    546.06 8389.5 543.18
    ## + PreSCN.juvs   1     32.95 7810.5 544.18
    ## + V3.DW         1      2.50 7841.0 544.64
    ## + PreSCN.cysts  1      0.18 7843.3 544.68
    ## - PreSCN.eggs   1    741.01 8584.5 545.94
    ## - Pre.ratio     1   1357.23 9200.7 554.26
    ## 
    ## Step:  AIC=535.61
    ## R5.DX ~ Year + V3.foliar + Pre.ratio + PreSCN.eggs + Pre.spiral + 
    ##     V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - Pre.spiral    1    253.87 8130.1 534.63
    ## - V3.foliar     1    282.99 8159.3 535.06
    ## - V3.qPCR       1    303.31 8179.6 535.35
    ## <none>                      7876.3 535.61
    ## - Year          1    546.30 8422.6 538.87
    ## + V3.root       1     32.81 7843.5 539.89
    ## + PreSCN.juvs   1     26.88 7849.4 539.98
    ## + PreSCN.cysts  1      0.45 7875.8 540.39
    ## + V3.DW         1      0.03 7876.2 540.39
    ## - PreSCN.eggs   1    997.74 8874.0 545.13
    ## - Pre.ratio     1   1347.80 9224.1 549.77
    ## 
    ## Step:  AIC=534.63
    ## R5.DX ~ Year + V3.foliar + Pre.ratio + PreSCN.eggs + V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - V3.foliar     1    218.41 8348.6 533.02
    ## - V3.qPCR       1    265.55 8395.7 533.70
    ## <none>                      8130.1 534.63
    ## - Year          1    384.22 8514.4 535.38
    ## + Pre.spiral    1    253.87 7876.3 535.61
    ## + PreSCN.juvs   1     44.06 8086.1 538.76
    ## + V3.root       1     24.27 8105.9 539.05
    ## + PreSCN.cysts  1      1.46 8128.7 539.39
    ## + V3.DW         1      1.01 8129.1 539.40
    ## - PreSCN.eggs   1    869.84 9000.0 542.04
    ## - Pre.ratio     1   1400.30 9530.5 548.91
    ## 
    ## Step:  AIC=533.02
    ## R5.DX ~ Year + Pre.ratio + PreSCN.eggs + V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - V3.qPCR       1    178.12 8526.7 530.77
    ## <none>                      8348.6 533.02
    ## + V3.foliar     1    218.41 8130.1 534.63
    ## + Pre.spiral    1    189.29 8159.3 535.06
    ## + V3.root       1     89.31 8259.3 536.52
    ## + PreSCN.juvs   1     62.18 8286.4 536.91
    ## + V3.DW         1     20.25 8328.3 537.52
    ## + PreSCN.cysts  1      5.41 8343.1 537.73
    ## - Year          1    704.92 9053.5 537.96
    ## - PreSCN.eggs   1   1151.52 9500.1 543.74
    ## - Pre.ratio     1   1379.36 9727.9 546.58
    ## 
    ## Step:  AIC=530.77
    ## R5.DX ~ Year + Pre.ratio + PreSCN.eggs
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## <none>                      8526.7 530.77
    ## + V3.qPCR       1    178.12 8348.6 533.02
    ## + Pre.spiral    1    173.45 8353.2 533.09
    ## + V3.foliar     1    130.98 8395.7 533.70
    ## + V3.root       1     75.95 8450.7 534.48
    ## + PreSCN.juvs   1     55.47 8471.2 534.77
    ## - Year          1    691.45 9218.1 535.33
    ## + PreSCN.cysts  1     15.01 8511.7 535.34
    ## + V3.DW         1      3.76 8522.9 535.50
    ## - PreSCN.eggs   1   1256.15 9782.8 542.47
    ## - Pre.ratio     1   1310.11 9836.8 543.13

``` r
best.BIC.DX$anova
```

    ## Stepwise Model Path 
    ## Analysis of Deviance Table
    ## 
    ## Initial Model:
    ## R5.DX ~ Year + V3.DW + V3.foliar + V3.root + Pre.ratio + PreSCN.cysts + 
    ##     PreSCN.eggs + PreSCN.juvs + Pre.spiral + V3.qPCR
    ## 
    ## Final Model:
    ## R5.DX ~ Year + Pre.ratio + PreSCN.eggs
    ## 
    ## 
    ##             Step Df   Deviance Resid. Df Resid. Dev      AIC
    ## 1                                    109   7795.795 553.5242
    ## 2        - V3.DW  1   3.226164       110   7799.021 548.7863
    ## 3 - PreSCN.cysts  1  11.484049       111   7810.505 544.1754
    ## 4  - PreSCN.juvs  1  32.954431       112   7843.459 539.8932
    ## 5      - V3.root  1  32.814950       113   7876.274 535.6067
    ## 6   - Pre.spiral  1 253.874814       114   8130.149 534.6261
    ## 7    - V3.foliar  1 218.407429       115   8348.557 533.0197
    ## 8      - V3.qPCR  1 178.116968       116   8526.674 530.7655

``` r
#Check out the models before and after selection
summary(R5.DX.fit)
```

    ## 
    ## Call:
    ## lm(formula = R5.DX ~ Year + V3.DW + V3.foliar + V3.root + Pre.ratio + 
    ##     PreSCN.cysts + PreSCN.eggs + PreSCN.juvs + Pre.spiral + V3.qPCR, 
    ##     data = df.SDS.DX)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -20.292  -5.157  -1.741   3.256  27.486 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   5.197647   6.474160   0.803   0.4238    
    ## Year2015     -9.109334   3.558133  -2.560   0.0118 *  
    ## V3.DW         1.380973   6.502178   0.212   0.8322    
    ## V3.foliar     2.520226   1.651379   1.526   0.1299    
    ## V3.root       0.051825   0.067608   0.767   0.4450    
    ## Pre.ratio     2.531155   0.589315   4.295 3.81e-05 ***
    ## PreSCN.cysts -0.083647   0.212459  -0.394   0.6946    
    ## PreSCN.eggs   0.002231   0.001926   1.158   0.2494    
    ## PreSCN.juvs   0.005616   0.007076   0.794   0.4291    
    ## Pre.spiral   -0.086279   0.047430  -1.819   0.0716 .  
    ## V3.qPCR      -0.650881   0.338092  -1.925   0.0568 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.457 on 109 degrees of freedom
    ## Multiple R-squared:  0.3906, Adjusted R-squared:  0.3347 
    ## F-statistic: 6.986 on 10 and 109 DF,  p-value: 2.178e-08

``` r
summary(best.AIC.DX)
```

    ## 
    ## Call:
    ## lm(formula = R5.DX ~ Year + V3.foliar + Pre.ratio + PreSCN.eggs + 
    ##     Pre.spiral + V3.qPCR, data = df.SDS.DX)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -17.061  -5.201  -1.906   3.616  26.992 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  7.2753290  3.4412216   2.114 0.036700 *  
    ## Year2015    -9.3943497  3.3556245  -2.800 0.006019 ** 
    ## V3.foliar    3.0256765  1.5016183   2.015 0.046285 *  
    ## Pre.ratio    2.5520949  0.5803705   4.397 2.49e-05 ***
    ## PreSCN.eggs  0.0024108  0.0006372   3.783 0.000249 ***
    ## Pre.spiral  -0.0882643  0.0462484  -1.908 0.058866 .  
    ## V3.qPCR     -0.6827088  0.3272773  -2.086 0.039227 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.349 on 113 degrees of freedom
    ## Multiple R-squared:  0.3843, Adjusted R-squared:  0.3516 
    ## F-statistic: 11.75 on 6 and 113 DF,  p-value: 3.306e-10

``` r
summary(best.BIC.DX)
```

    ## 
    ## Call:
    ## lm(formula = R5.DX ~ Year + Pre.ratio + PreSCN.eggs, data = df.SDS.DX)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -14.023  -5.723  -1.534   3.803  26.571 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  2.8069519  2.5220653   1.113  0.26803    
    ## Year2015    -9.4603673  3.0845187  -3.067  0.00269 ** 
    ## Pre.ratio    2.5055165  0.5934764   4.222 4.84e-05 ***
    ## PreSCN.eggs  0.0025848  0.0006253   4.134 6.77e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.574 on 116 degrees of freedom
    ## Multiple R-squared:  0.3334, Adjusted R-squared:  0.3162 
    ## F-statistic: 19.34 on 3 and 116 DF,  p-value: 3.079e-10

``` r
#So, the best model (according to BIC) to predict R5.DX is:
# R5.DX = Year + Pre.ratio + Pre.SCN.eggs
best.BIC.DX2 <- lm(R5.DX ~ Year*Pre.ratio*PreSCN.eggs, data = df.SDS.DX)
summary(best.BIC.DX2)
```

    ## 
    ## Call:
    ## lm(formula = R5.DX ~ Year * Pre.ratio * PreSCN.eggs, data = df.SDS.DX)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -15.234  -5.476  -1.634   4.102  26.533 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)                    -30.140597  23.242331  -1.297   0.1974  
    ## Year2015                        24.700535  23.736195   1.041   0.3003  
    ## Pre.ratio                       41.139295  23.419554   1.757   0.0817 .
    ## PreSCN.eggs                      0.009137   0.007250   1.260   0.2102  
    ## Year2015:Pre.ratio             -39.036447  23.446640  -1.665   0.0987 .
    ## Year2015:PreSCN.eggs            -0.006976   0.007713  -0.905   0.3677  
    ## Pre.ratio:PreSCN.eggs           -0.008040   0.006975  -1.153   0.2515  
    ## Year2015:Pre.ratio:PreSCN.eggs   0.008185   0.006996   1.170   0.2445  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.537 on 112 degrees of freedom
    ## Multiple R-squared:  0.3618, Adjusted R-squared:  0.3219 
    ## F-statistic: 9.072 on 7 and 112 DF,  p-value: 7.742e-09

``` r
#Adding the interaction term slightly helps R2, doesn't help P
```

``` r
#Remove non-predictive variables from this model (Anything R5)
R5.DW.fit <- lm(R5.DW ~ Year +
                         V3.DW+
                         #R5.DW+
                         V3.foliar+
                         V3.root+
                         #R5.foliar+
                         #R5.root+
                         #R5.DX+
                         Pre.ratio+
                         PreSCN.cysts+ 
                         PreSCN.eggs+
                         PreSCN.juvs+ 
                         Pre.spiral+
                         Pre.ratio+
                         V3.qPCR,
                         #R5.qPCR,
                         data = df.SDS.DX)

#Determine the best version of each model using stepwise AIC and BIC
best.AIC.DW <- stepAIC(R5.DW.fit, direction = "both")
```

    ## Start:  AIC=58.96
    ## R5.DW ~ Year + V3.DW + V3.foliar + V3.root + Pre.ratio + PreSCN.cysts + 
    ##     PreSCN.eggs + PreSCN.juvs + Pre.spiral + Pre.ratio + V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - PreSCN.eggs   1    0.0013 163.28 56.961
    ## - V3.foliar     1    0.2582 163.54 57.149
    ## - V3.DW         1    0.3264 163.61 57.199
    ## - PreSCN.juvs   1    0.7667 164.05 57.522
    ## - PreSCN.cysts  1    1.6623 164.95 58.175
    ## <none>                      163.28 58.960
    ## - Pre.ratio     1    4.2609 167.54 60.051
    ## - V3.qPCR       1    6.9105 170.19 61.934
    ## - Year          1    7.8612 171.15 62.602
    ## - Pre.spiral    1   10.4658 173.75 64.415
    ## - V3.root       1   14.7784 178.06 67.357
    ## 
    ## Step:  AIC=56.96
    ## R5.DW ~ Year + V3.DW + V3.foliar + V3.root + Pre.ratio + PreSCN.cysts + 
    ##     PreSCN.juvs + Pre.spiral + V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - V3.foliar     1    0.2569 163.54 55.149
    ## - V3.DW         1    0.3250 163.61 55.199
    ## - PreSCN.juvs   1    0.7717 164.06 55.526
    ## <none>                      163.28 56.961
    ## - Pre.ratio     1    4.3191 167.60 58.094
    ## - PreSCN.cysts  1    4.5991 167.88 58.294
    ## + PreSCN.eggs   1    0.0013 163.28 58.960
    ## - V3.qPCR       1    7.0466 170.33 60.031
    ## - Year          1    8.0958 171.38 60.768
    ## - Pre.spiral    1   10.4699 173.75 62.419
    ## - V3.root       1   15.2531 178.54 65.677
    ## 
    ## Step:  AIC=55.15
    ## R5.DW ~ Year + V3.DW + V3.root + Pre.ratio + PreSCN.cysts + PreSCN.juvs + 
    ##     Pre.spiral + V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - V3.DW         1    0.2459 163.79 53.330
    ## - PreSCN.juvs   1    0.6499 164.19 53.625
    ## <none>                      163.54 55.149
    ## - Pre.ratio     1    4.3109 167.85 56.272
    ## - PreSCN.cysts  1    4.4105 167.95 56.343
    ## + V3.foliar     1    0.2569 163.28 56.961
    ## + PreSCN.eggs   1    0.0000 163.54 57.149
    ## - V3.qPCR       1    6.7902 170.33 58.031
    ## - Year          1    9.6436 173.19 60.025
    ## - Pre.spiral    1   10.2142 173.76 60.419
    ## - V3.root       1   17.9020 181.44 65.615
    ## 
    ## Step:  AIC=53.33
    ## R5.DW ~ Year + V3.root + Pre.ratio + PreSCN.cysts + PreSCN.juvs + 
    ##     Pre.spiral + V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - PreSCN.juvs   1    0.7013 164.49 51.842
    ## <none>                      163.79 53.330
    ## - Pre.ratio     1    4.3468 168.13 54.473
    ## - PreSCN.cysts  1    4.5190 168.31 54.596
    ## + V3.DW         1    0.2459 163.54 55.149
    ## + V3.foliar     1    0.1778 163.61 55.199
    ## + PreSCN.eggs   1    0.0008 163.79 55.329
    ## - V3.qPCR       1    7.5040 171.29 56.705
    ## - Year          1   10.0018 173.79 58.442
    ## - Pre.spiral    1   10.0540 173.84 58.479
    ## - V3.root       1   18.3135 182.10 64.049
    ## 
    ## Step:  AIC=51.84
    ## R5.DW ~ Year + V3.root + Pre.ratio + PreSCN.cysts + Pre.spiral + 
    ##     V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## <none>                      164.49 51.842
    ## - Pre.ratio     1    4.1258 168.62 52.815
    ## + PreSCN.juvs   1    0.7013 163.79 53.330
    ## + V3.DW         1    0.2973 164.19 53.625
    ## + V3.foliar     1    0.0739 164.41 53.789
    ## + PreSCN.eggs   1    0.0005 164.49 53.842
    ## - PreSCN.cysts  1    6.6754 171.16 54.616
    ## - V3.qPCR       1    7.3593 171.85 55.095
    ## - Year          1    9.4973 173.99 56.578
    ## - Pre.spiral    1    9.6420 174.13 56.678
    ## - V3.root       1   18.4505 182.94 62.600

``` r
best.BIC.DW <- stepAIC(R5.DW.fit, direction = "both", k=log(n))
```

    ## Start:  AIC=89.62
    ## R5.DW ~ Year + V3.DW + V3.foliar + V3.root + Pre.ratio + PreSCN.cysts + 
    ##     PreSCN.eggs + PreSCN.juvs + Pre.spiral + Pre.ratio + V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - PreSCN.eggs   1    0.0013 163.28 84.836
    ## - V3.foliar     1    0.2582 163.54 85.024
    ## - V3.DW         1    0.3264 163.61 85.074
    ## - PreSCN.juvs   1    0.7667 164.05 85.397
    ## - PreSCN.cysts  1    1.6623 164.95 86.050
    ## - Pre.ratio     1    4.2609 167.54 87.926
    ## <none>                      163.28 89.622
    ## - V3.qPCR       1    6.9105 170.19 89.809
    ## - Year          1    7.8612 171.15 90.477
    ## - Pre.spiral    1   10.4658 173.75 92.290
    ## - V3.root       1   14.7784 178.06 95.232
    ## 
    ## Step:  AIC=84.84
    ## R5.DW ~ Year + V3.DW + V3.foliar + V3.root + Pre.ratio + PreSCN.cysts + 
    ##     PreSCN.juvs + Pre.spiral + V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - V3.foliar     1    0.2569 163.54 80.237
    ## - V3.DW         1    0.3250 163.61 80.287
    ## - PreSCN.juvs   1    0.7717 164.06 80.614
    ## - Pre.ratio     1    4.3191 167.60 83.181
    ## - PreSCN.cysts  1    4.5991 167.88 83.381
    ## <none>                      163.28 84.836
    ## - V3.qPCR       1    7.0466 170.33 85.118
    ## - Year          1    8.0958 171.38 85.855
    ## - Pre.spiral    1   10.4699 173.75 87.506
    ## + PreSCN.eggs   1    0.0013 163.28 89.622
    ## - V3.root       1   15.2531 178.54 90.765
    ## 
    ## Step:  AIC=80.24
    ## R5.DW ~ Year + V3.DW + V3.root + Pre.ratio + PreSCN.cysts + PreSCN.juvs + 
    ##     Pre.spiral + V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - V3.DW         1    0.2459 163.79 75.630
    ## - PreSCN.juvs   1    0.6499 164.19 75.925
    ## - Pre.ratio     1    4.3109 167.85 78.571
    ## - PreSCN.cysts  1    4.4105 167.95 78.643
    ## <none>                      163.54 80.237
    ## - V3.qPCR       1    6.7902 170.33 80.331
    ## - Year          1    9.6436 173.19 82.325
    ## - Pre.spiral    1   10.2142 173.76 82.719
    ## + V3.foliar     1    0.2569 163.28 84.836
    ## + PreSCN.eggs   1    0.0000 163.54 85.024
    ## - V3.root       1   17.9020 181.44 87.915
    ## 
    ## Step:  AIC=75.63
    ## R5.DW ~ Year + V3.root + Pre.ratio + PreSCN.cysts + PreSCN.juvs + 
    ##     Pre.spiral + V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - PreSCN.juvs   1    0.7013 164.49 71.355
    ## - Pre.ratio     1    4.3468 168.13 73.985
    ## - PreSCN.cysts  1    4.5190 168.31 74.108
    ## <none>                      163.79 75.630
    ## - V3.qPCR       1    7.5040 171.29 76.218
    ## - Year          1   10.0018 173.79 77.955
    ## - Pre.spiral    1   10.0540 173.84 77.991
    ## + V3.DW         1    0.2459 163.54 80.237
    ## + V3.foliar     1    0.1778 163.61 80.287
    ## + PreSCN.eggs   1    0.0008 163.79 80.416
    ## - V3.root       1   18.3135 182.10 83.561
    ## 
    ## Step:  AIC=71.35
    ## R5.DW ~ Year + V3.root + Pre.ratio + PreSCN.cysts + Pre.spiral + 
    ##     V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - Pre.ratio     1    4.1258 168.62 69.540
    ## - PreSCN.cysts  1    6.6754 171.16 71.341
    ## <none>                      164.49 71.355
    ## - V3.qPCR       1    7.3593 171.85 71.820
    ## - Year          1    9.4973 173.99 73.303
    ## - Pre.spiral    1    9.6420 174.13 73.403
    ## + PreSCN.juvs   1    0.7013 163.79 75.630
    ## + V3.DW         1    0.2973 164.19 75.925
    ## + V3.foliar     1    0.0739 164.41 76.088
    ## + PreSCN.eggs   1    0.0005 164.49 76.142
    ## - V3.root       1   18.4505 182.94 79.325
    ## 
    ## Step:  AIC=69.54
    ## R5.DW ~ Year + V3.root + PreSCN.cysts + Pre.spiral + V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - Year          1    5.3823 174.00 68.523
    ## - V3.qPCR       1    6.7383 175.35 69.455
    ## <none>                      168.62 69.540
    ## + Pre.ratio     1    4.1258 164.49 71.355
    ## - Pre.spiral    1   10.1555 178.77 71.771
    ## - PreSCN.cysts  1   11.7314 180.35 72.824
    ## + PreSCN.juvs   1    0.4803 168.13 73.985
    ## + V3.DW         1    0.3258 168.29 74.096
    ## + V3.foliar     1    0.0801 168.53 74.271
    ## + PreSCN.eggs   1    0.0677 168.55 74.279
    ## - V3.root       1   18.4715 187.09 77.227
    ## 
    ## Step:  AIC=68.52
    ## R5.DW ~ V3.root + PreSCN.cysts + Pre.spiral + V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - Pre.spiral    1    5.6181 179.62 67.549
    ## <none>                      174.00 68.523
    ## - V3.qPCR       1    7.1568 181.15 68.573
    ## + Year          1    5.3823 168.62 69.540
    ## + V3.foliar     1    1.2324 172.76 72.458
    ## + V3.DW         1    0.6446 173.35 72.865
    ## + PreSCN.juvs   1    0.1948 173.80 73.176
    ## + PreSCN.eggs   1    0.1902 173.81 73.179
    ## + Pre.ratio     1    0.0108 173.99 73.303
    ## - PreSCN.cysts  1   15.2447 189.24 73.814
    ## - V3.root       1   21.1723 195.17 77.515
    ## 
    ## Step:  AIC=67.55
    ## R5.DW ~ V3.root + PreSCN.cysts + V3.qPCR
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## - V3.qPCR       1    6.1790 185.79 66.820
    ## <none>                      179.62 67.549
    ## + Pre.spiral    1    5.6181 174.00 68.523
    ## - PreSCN.cysts  1   11.7369 191.35 70.357
    ## + Year          1    0.8450 178.77 71.771
    ## + Pre.ratio     1    0.6158 179.00 71.925
    ## + V3.DW         1    0.2339 179.38 72.180
    ## + V3.foliar     1    0.1102 179.50 72.263
    ## + PreSCN.juvs   1    0.0987 179.52 72.271
    ## + PreSCN.eggs   1    0.0097 179.61 72.330
    ## - V3.root       1   18.0479 197.66 74.251
    ## 
    ## Step:  AIC=66.82
    ## R5.DW ~ V3.root + PreSCN.cysts
    ## 
    ##                Df Sum of Sq    RSS    AIC
    ## <none>                      185.79 66.820
    ## + V3.qPCR       1    6.1790 179.62 67.549
    ## + Pre.spiral    1    4.6403 181.15 68.573
    ## - PreSCN.cysts  1   12.9546 198.75 70.121
    ## + Year          1    1.1865 184.61 70.839
    ## + V3.DW         1    0.8927 184.90 71.030
    ## + Pre.ratio     1    0.3027 185.49 71.412
    ## + PreSCN.juvs   1    0.0594 185.74 71.570
    ## + PreSCN.eggs   1    0.0347 185.76 71.585
    ## + V3.foliar     1    0.0052 185.79 71.605
    ## - V3.root       1   17.7006 203.50 72.953

``` r
best.BIC.DW$anova
```

    ## Stepwise Model Path 
    ## Analysis of Deviance Table
    ## 
    ## Initial Model:
    ## R5.DW ~ Year + V3.DW + V3.foliar + V3.root + Pre.ratio + PreSCN.cysts + 
    ##     PreSCN.eggs + PreSCN.juvs + Pre.spiral + Pre.ratio + V3.qPCR
    ## 
    ## Final Model:
    ## R5.DW ~ V3.root + PreSCN.cysts
    ## 
    ## 
    ##            Step Df    Deviance Resid. Df Resid. Dev      AIC
    ## 1                                    109   163.2838 89.62215
    ## 2 - PreSCN.eggs  1 0.001314133       110   163.2851 84.83562
    ## 3   - V3.foliar  1 0.256932248       111   163.5420 80.23680
    ## 4       - V3.DW  1 0.245901630       112   163.7879 75.62961
    ## 5 - PreSCN.juvs  1 0.701342494       113   164.4893 71.35486
    ## 6   - Pre.ratio  1 4.125770265       114   168.6150 69.54012
    ## 7        - Year  1 5.382321034       115   173.9973 68.52325
    ## 8  - Pre.spiral  1 5.618144983       116   179.6155 67.54916
    ## 9     - V3.qPCR  1 6.178996265       117   185.7945 66.82040

``` r
#Check out the models before and after selection
summary(R5.DW.fit)
```

    ## 
    ## Call:
    ## lm(formula = R5.DW ~ Year + V3.DW + V3.foliar + V3.root + Pre.ratio + 
    ##     PreSCN.cysts + PreSCN.eggs + PreSCN.juvs + Pre.spiral + Pre.ratio + 
    ##     V3.qPCR, data = df.SDS.DX)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.3624 -0.7430 -0.0761  0.5487  3.6489 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   4.362e+00  9.370e-01   4.655 9.17e-06 ***
    ## Year2015      1.180e+00  5.149e-01   2.291  0.02390 *  
    ## V3.DW        -4.392e-01  9.410e-01  -0.467  0.64161    
    ## V3.foliar    -9.923e-02  2.390e-01  -0.415  0.67883    
    ## V3.root      -3.073e-02  9.784e-03  -3.141  0.00217 ** 
    ## Pre.ratio    -1.438e-01  8.529e-02  -1.687  0.09455 .  
    ## PreSCN.cysts -3.239e-02  3.075e-02  -1.053  0.29449    
    ## PreSCN.eggs   8.258e-06  2.788e-04   0.030  0.97643    
    ## PreSCN.juvs   7.327e-04  1.024e-03   0.715  0.47590    
    ## Pre.spiral    1.814e-02  6.864e-03   2.643  0.00942 ** 
    ## V3.qPCR       1.051e-01  4.893e-02   2.148  0.03394 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.224 on 109 degrees of freedom
    ## Multiple R-squared:  0.3346, Adjusted R-squared:  0.2736 
    ## F-statistic: 5.482 on 10 and 109 DF,  p-value: 1.453e-06

``` r
summary(best.AIC.DW)
```

    ## 
    ## Call:
    ## lm(formula = R5.DW ~ Year + V3.root + Pre.ratio + PreSCN.cysts + 
    ##     Pre.spiral + V3.qPCR, data = df.SDS.DX)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.3206 -0.7394 -0.0913  0.5983  3.7186 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   4.035719   0.550862   7.326 3.77e-11 ***
    ## Year2015      1.216013   0.476066   2.554 0.011971 *  
    ## V3.root      -0.030816   0.008656  -3.560 0.000543 ***
    ## Pre.ratio    -0.139886   0.083091  -1.684 0.095032 .  
    ## PreSCN.cysts -0.020911   0.009765  -2.141 0.034386 *  
    ## Pre.spiral    0.017114   0.006649   2.574 0.011356 *  
    ## V3.qPCR       0.103253   0.045921   2.248 0.026485 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.207 on 113 degrees of freedom
    ## Multiple R-squared:  0.3297, Adjusted R-squared:  0.2941 
    ## F-statistic: 9.264 on 6 and 113 DF,  p-value: 2.994e-08

``` r
summary(best.BIC.DW)
```

    ## 
    ## Call:
    ## lm(formula = R5.DW ~ V3.root + PreSCN.cysts, data = df.SDS.DX)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.0670 -0.9527 -0.1847  0.5452  3.8162 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   5.450856   0.287285  18.974  < 2e-16 ***
    ## V3.root      -0.029496   0.008835  -3.339  0.00113 ** 
    ## PreSCN.cysts -0.026222   0.009181  -2.856  0.00507 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.26 on 117 degrees of freedom
    ## Multiple R-squared:  0.2429, Adjusted R-squared:   0.23 
    ## F-statistic: 18.77 on 2 and 117 DF,  p-value: 8.518e-08

``` r
#So, the best model (according to BIC) to predict R5.DW is:
# R5.DW = V3.root + Pre.SCN.cysts
best.BIC.DW2 <- lm(R5.DW ~ V3.root*PreSCN.cysts, data = df.SDS.DX)
summary(best.BIC.DW2)
```

    ## 
    ## Call:
    ## lm(formula = R5.DW ~ V3.root * PreSCN.cysts, data = df.SDS.DX)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.1943 -0.8670 -0.1693  0.5441  3.7202 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           6.4864985  0.5291787  12.258  < 2e-16 ***
    ## V3.root              -0.0595772  0.0156324  -3.811 0.000223 ***
    ## PreSCN.cysts         -0.0758856  0.0232861  -3.259 0.001468 ** 
    ## V3.root:PreSCN.cysts  0.0013056  0.0005644   2.313 0.022478 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.237 on 116 degrees of freedom
    ## Multiple R-squared:  0.2763, Adjusted R-squared:  0.2576 
    ## F-statistic: 14.76 on 3 and 116 DF,  p-value: 3.326e-08

``` r
#Adding the interaction term slightly helps R2, doesn't help P
```

``` r
#Check each model
#Creates 3 plots for each model
model.check <- function(model){
  library(MASS)
  par(mfrow=c(1, 1))
  sresid <- studres(model) 
  hist(sresid, freq=FALSE, 
       main="Distribution of Studentized Residuals")
  xfit<-seq(min(sresid),max(sresid),length=100) 
  yfit<-dnorm(xfit) 
  lines(xfit, yfit)
  par(mfrow=c(2, 2))
  plot(model)
  #Check for autocorrelation of residuals
  par(mfrow=c(1, 1))
  acf(model$residuals)
}

model.check(best.BIC.full)
```

![](Multiple_Regression_files/figure-markdown_github/Double%20Check%20Models-1.png)![](Multiple_Regression_files/figure-markdown_github/Double%20Check%20Models-2.png)![](Multiple_Regression_files/figure-markdown_github/Double%20Check%20Models-3.png)

``` r
model.check(best.BIC.DX)
```

![](Multiple_Regression_files/figure-markdown_github/Double%20Check%20Models-4.png)![](Multiple_Regression_files/figure-markdown_github/Double%20Check%20Models-5.png)![](Multiple_Regression_files/figure-markdown_github/Double%20Check%20Models-6.png)

``` r
model.check(best.BIC.DW)
```

![](Multiple_Regression_files/figure-markdown_github/Double%20Check%20Models-7.png)![](Multiple_Regression_files/figure-markdown_github/Double%20Check%20Models-8.png)![](Multiple_Regression_files/figure-markdown_github/Double%20Check%20Models-9.png)

``` r
#All models look pretty good
```

``` r
#install.packages("lavaan")
#install.packages("gendata")
#install.packages("semPlot")
library(lavaan)
```

    ## This is lavaan 0.6-3

    ## lavaan is BETA software! Please report any bugs.

``` r
library(semPlot)


#define the models based on the multiple linear regression above
model <- '
Yield ~ R5.DX + R5.DW
R5.DX ~ Pre.ratio + PreSCN.eggs
R5.DW ~ PreSCN.cysts + V3.root'

df.yield.14 <- df.yield[df.yield$Year=="2014",]
df.yield.15 <- df.yield[df.yield$Year=="2015",]

fit <- cfa(model, data = df.yield)
```

    ## Warning in lav_model_vcov(lavmodel = lavmodel, lavsamplestats = lavsamplestats, : lavaan WARNING:
    ##     Could not compute standard errors! The information matrix could
    ##     not be inverted. This may be a symptom that the model is not
    ##     identified.

``` r
fit14 <- cfa(model, data = df.yield.14)
```

    ## Warning in lav_model_vcov(lavmodel = lavmodel, lavsamplestats = lavsamplestats, : lavaan WARNING:
    ##     Could not compute standard errors! The information matrix could
    ##     not be inverted. This may be a symptom that the model is not
    ##     identified.

``` r
fit15 <- cfa(model, data = df.yield.15)
```

    ## Warning in lav_model_vcov(lavmodel = lavmodel, lavsamplestats = lavsamplestats, : lavaan WARNING:
    ##     Could not compute standard errors! The information matrix could
    ##     not be inverted. This may be a symptom that the model is not
    ##     identified.

``` r
semPaths(fit, 'std', 
         edge.label.cex = 2,
         sizeMan = 13, sizeMan2 = 7,
         exoVar = FALSE, 
         layout = 'tree2')
```

![](Multiple_Regression_files/figure-markdown_github/Path%20Analysis-1.png)

``` r
semPaths(fit14, 'std',
         edge.label.cex = 2,
         sizeMan = 13, sizeMan2 = 7,
         exoVar = FALSE, 
         layout = 'tree2')
```

![](Multiple_Regression_files/figure-markdown_github/Path%20Analysis-2.png)

``` r
semPaths(fit15, 'std',
         edge.label.cex = 2,
         sizeMan = 13, sizeMan2 = 7,
         exoVar = FALSE, 
         layout = 'tree2')
```

![](Multiple_Regression_files/figure-markdown_github/Path%20Analysis-3.png)

Cross Validation
----------------

``` r
#Install and load all necessary packages
#install.packages("caret")
#install.packages("arm")
#install.packages("proxy")
library(caret)
```

    ## Loading required package: lattice

    ## Loading required package: ggplot2

``` r
library(arm)
```

    ## Loading required package: Matrix

    ## Loading required package: lme4

    ## 
    ## arm (Version 1.10-1, built: 2018-4-12)

    ## Working directory is /Users/rothmitc/Desktop/Git/SDS-Risk-Assessment_FINAL3/SDS-Risk-Assessment

``` r
library(proxy)
```

    ## 
    ## Attaching package: 'proxy'

    ## The following object is masked from 'package:Matrix':
    ## 
    ##     as.matrix

    ## The following objects are masked from 'package:stats':
    ## 
    ##     as.dist, dist

    ## The following object is masked from 'package:base':
    ## 
    ##     as.matrix

``` r
#Extract only the variables from the best model according to BIC above
summary(best.BIC.full)  #Yield, DW, DX
```

    ## 
    ## Call:
    ## lm(formula = Yield ~ R5.DW + R5.DX, data = df.yield)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.5384  -2.3318   0.4424   2.2014  11.0966 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 26.00298    2.62922   9.890 7.35e-15 ***
    ## R5.DW        1.95085    0.56284   3.466 0.000913 ***
    ## R5.DX       -0.35056    0.06769  -5.179 2.10e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.721 on 69 degrees of freedom
    ## Multiple R-squared:  0.5442, Adjusted R-squared:  0.531 
    ## F-statistic: 41.19 on 2 and 69 DF,  p-value: 1.693e-12

``` r
summary(best.BIC.DX)    #DX, Year, pre.ratio, preSCN.eggs
```

    ## 
    ## Call:
    ## lm(formula = R5.DX ~ Year + Pre.ratio + PreSCN.eggs, data = df.SDS.DX)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -14.023  -5.723  -1.534   3.803  26.571 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  2.8069519  2.5220653   1.113  0.26803    
    ## Year2015    -9.4603673  3.0845187  -3.067  0.00269 ** 
    ## Pre.ratio    2.5055165  0.5934764   4.222 4.84e-05 ***
    ## PreSCN.eggs  0.0025848  0.0006253   4.134 6.77e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.574 on 116 degrees of freedom
    ## Multiple R-squared:  0.3334, Adjusted R-squared:  0.3162 
    ## F-statistic: 19.34 on 3 and 116 DF,  p-value: 3.079e-10

``` r
summary(best.BIC.DW)    #DW, V3.root, preSCN.cysts
```

    ## 
    ## Call:
    ## lm(formula = R5.DW ~ V3.root + PreSCN.cysts, data = df.SDS.DX)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.0670 -0.9527 -0.1847  0.5452  3.8162 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   5.450856   0.287285  18.974  < 2e-16 ***
    ## V3.root      -0.029496   0.008835  -3.339  0.00113 ** 
    ## PreSCN.cysts -0.026222   0.009181  -2.856  0.00507 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.26 on 117 degrees of freedom
    ## Multiple R-squared:  0.2429, Adjusted R-squared:   0.23 
    ## F-statistic: 18.77 on 2 and 117 DF,  p-value: 8.518e-08

``` r
#Create data frames containing the required variables based on the best models above
model.data1 <- data.frame(df.yield$Yield, 
                          df.yield$R5.DW, df.yield$R5.DX)
model.data2 <- data.frame(df.SDS.DX$R5.DX, 
                          df.SDS.DX$Year, df.SDS.DX$Pre.ratio, df.SDS.DX$PreSCN.eggs)
model.data3 <- data.frame(df.SDS.DX$R5.DW, 
                          df.SDS.DX$V3.root, df.SDS.DX$PreSCN.cysts)
colnames(model.data1) <- c("Yield","R5.DW","R5.DX")
colnames(model.data2) <- c("R5.DX","Year", "Pre.Fv.qPCR", "Pre.SCN.eggs")
colnames(model.data3) <- c("R5.DW","V3.root.rot","Pre.SCN.cysts")

#Create the "testing" data containing only the risk factors (explanatory variables)
testing.data1 <- model.data1[, c("R5.DW","R5.DX")]
testing.data2 <- model.data2[, c("Year", "Pre.Fv.qPCR", "Pre.SCN.eggs")]
testing.data3 <- model.data3[, c("V3.root.rot","Pre.SCN.cysts")]

#Create a function to cross validate each model, make a prediction, and determine correlation between predicted and actual
#This particular function does 10 iterations of the CV, prediction, and correlation
#For a manuscript, I want to do 1000 iterations, and I ran this on an external server. To do this I changed the for statement to  be (i in 1:1000)
CV.func1 <- function(model.data, test.data){
  CV.df <- NULL
  for (i in 1:10){
    #Randomly select 5 data points from within the range of the testing data
    startSet <- sample(1:dim(test.data)[1], 5)
    #Create a data frame that is MISSING these random 5 data points
    samplePool <- test.data[-startSet,]
    #Select 75% of the data that has maximum distance from the randomly selected 5 data points
    start <- test.data[startSet,]
    newSamp <- maxDissim(start, samplePool, n = (0.75*length(test.data[,1])))
    #Make the furthest 75% of the data the training data, and the remaining 25% the testing data
    inTraining <- newSamp
    training <- model.data[ inTraining,]
    testing  <- model.data[-inTraining,]
    colnames(training) <- c("response","factor1","factor2")
    colnames(testing) <- c("response","factor1","factor2")
    #training$factor1 <- as.factor(training$factor1)
    #testing$factor1 <- as.factor(testing$factor1)
    #Make sure the CV will perform the cross validation 10 times, each with 10 repeats
    fitControl <- trainControl(method = "repeatedcv",
                               number = 10,
                               repeats = 10)
    #Train the model
    trained.fit <- train(response ~ ., data = training, 
                         method = "lm",
                         trControl = fitControl
                         #intercept = 
    )
    #Use the model to predict the variable of interest
    predicted.data <- predict(trained.fit, newdata = testing)
    #Create a data frame containing the predicted data, actual data, and the difference
    compare <- data.frame(predicted.data, testing[,1])
    compare$difference <- compare[,1] - compare[,2]
    #Run a correlation between predicted and actual, and export the results
    cor <- cor.test(compare[,1], compare[,2], method = c("pearson"))
    counter <- counter + 1
    row <- data.frame(mean(compare[,1]),
                      mean(compare[,2]),
                      mean(compare$difference), 
                      var(compare$difference), 
                      cor$estimate,
                      (cor$estimate * cor$estimate),
                      cor$p.value,
                      row.names = counter)
    colnames(row) <- c("Mean Predict", "Mean Actual", "Mean Dif", "Variance", "R", "R2", "P")
    CV.df <- rbind.data.frame(CV.df, row)
    print(CV.df)
  }
  return(CV.df)
}
CV.func2 <- function(model.data, test.data){
  CV.df <- NULL
  for (i in 1:10){
    #Randomly select 5 data points from within the range of the testing data
    startSet <- sample(1:dim(test.data)[1], 5)
    #Create a data frame that is MISSING these random 5 data points
    samplePool <- test.data[-startSet,]
    #Select 75% of the data that has maximum distance from the randomly selected 5 data points
    start <- test.data[startSet,]
    newSamp <- maxDissim(start, samplePool, n = (0.75*length(test.data[,1])))
    #Make the furthest 75% of the data the training data, and the remaining 25% the testing data
    inTraining <- newSamp
    training <- model.data[ inTraining,]
    testing  <- model.data[-inTraining,]
    colnames(training) <- c("response","factor1","factor2","factor3")
    colnames(testing) <- c("response","factor1","factor2","factor3")
    training$factor1 <- as.factor(training$factor1)
    testing$factor1 <- as.factor(testing$factor1)
    #Make sure the CV will perform the cross validation 10 times, each with 10 repeats
    fitControl <- trainControl(method = "repeatedcv",
                               number = 10,
                               repeats = 10)
    #Train the model
    trained.fit <- train(response ~ ., data = training, 
                         method = "lm",
                         trControl = fitControl
                         #intercept = 
    )
    #Use the model to predict the variable of interest
    predicted.data <- predict(trained.fit, newdata = testing)
    #Create a data frame containing the predicted data, actual data, and the difference
    compare <- data.frame(predicted.data, testing[,1])
    compare$difference <- compare[,1] - compare[,2]
    #Run a correlation between predicted and actual, and export the results
    cor <- cor.test(compare[,1], compare[,2], method = c("pearson"))
    counter <- counter + 1
    row <- data.frame(mean(compare[,1]),
                      mean(compare[,2]),
                      mean(compare$difference), 
                      var(compare$difference), 
                      cor$estimate,
                      (cor$estimate * cor$estimate),
                      cor$p.value,
                      row.names = counter)
    colnames(row) <- c("Mean Predict", "Mean Actual", "Mean Dif", "Variance", "R", "R2", "P")
    CV.df <- rbind.data.frame(CV.df, row)
    print(CV.df)
  }
  return(CV.df)
}

#Use func1 to run on yield and DW (Since these only have 2 explanatory variables)
counter <- 0
yield.model.CV <- CV.func1(model.data1, testing.data1)
```

    ##   Mean Predict Mean Actual   Mean Dif Variance         R        R2
    ## 1     29.39132     30.0144 -0.6230727    38.24 0.4134236 0.1709191
    ##            P
    ## 1 0.08812752
    ##   Mean Predict Mean Actual   Mean Dif Variance         R        R2
    ## 1     29.39132    30.01440 -0.6230727 38.24000 0.4134236 0.1709191
    ## 2     30.43006    31.28675 -0.8566916 41.14525 0.3528228 0.1244839
    ##            P
    ## 1 0.08812752
    ## 2 0.15097210
    ##   Mean Predict Mean Actual   Mean Dif Variance         R        R2
    ## 1     29.39132    30.01440 -0.6230727 38.24000 0.4134236 0.1709191
    ## 2     30.43006    31.28675 -0.8566916 41.14525 0.3528228 0.1244839
    ## 3     28.73987    30.51382 -1.7739516 25.00434 0.7734711 0.5982575
    ##              P
    ## 1 0.0881275155
    ## 2 0.1509720981
    ## 3 0.0001664179
    ##   Mean Predict Mean Actual   Mean Dif Variance         R        R2
    ## 1     29.39132    30.01440 -0.6230727 38.24000 0.4134236 0.1709191
    ## 2     30.43006    31.28675 -0.8566916 41.14525 0.3528228 0.1244839
    ## 3     28.73987    30.51382 -1.7739516 25.00434 0.7734711 0.5982575
    ## 4     29.85156    29.96774 -0.1161867 30.81677 0.7221112 0.5214447
    ##              P
    ## 1 0.0881275155
    ## 2 0.1509720981
    ## 3 0.0001664179
    ## 4 0.0007143244
    ##   Mean Predict Mean Actual   Mean Dif Variance         R        R2
    ## 1     29.39132    30.01440 -0.6230727 38.24000 0.4134236 0.1709191
    ## 2     30.43006    31.28675 -0.8566916 41.14525 0.3528228 0.1244839
    ## 3     28.73987    30.51382 -1.7739516 25.00434 0.7734711 0.5982575
    ## 4     29.85156    29.96774 -0.1161867 30.81677 0.7221112 0.5214447
    ## 5     29.04279    29.96312 -0.9203296 39.74507 0.4954275 0.2454484
    ##              P
    ## 1 0.0881275155
    ## 2 0.1509720981
    ## 3 0.0001664179
    ## 4 0.0007143244
    ## 5 0.0365587523
    ##   Mean Predict Mean Actual   Mean Dif Variance         R        R2
    ## 1     29.39132    30.01440 -0.6230727 38.24000 0.4134236 0.1709191
    ## 2     30.43006    31.28675 -0.8566916 41.14525 0.3528228 0.1244839
    ## 3     28.73987    30.51382 -1.7739516 25.00434 0.7734711 0.5982575
    ## 4     29.85156    29.96774 -0.1161867 30.81677 0.7221112 0.5214447
    ## 5     29.04279    29.96312 -0.9203296 39.74507 0.4954275 0.2454484
    ## 6     28.82575    31.03614 -2.2103919 23.49061 0.6384475 0.4076153
    ##              P
    ## 1 0.0881275155
    ## 2 0.1509720981
    ## 3 0.0001664179
    ## 4 0.0007143244
    ## 5 0.0365587523
    ## 6 0.0043506307
    ##   Mean Predict Mean Actual   Mean Dif Variance         R        R2
    ## 1     29.39132    30.01440 -0.6230727 38.24000 0.4134236 0.1709191
    ## 2     30.43006    31.28675 -0.8566916 41.14525 0.3528228 0.1244839
    ## 3     28.73987    30.51382 -1.7739516 25.00434 0.7734711 0.5982575
    ## 4     29.85156    29.96774 -0.1161867 30.81677 0.7221112 0.5214447
    ## 5     29.04279    29.96312 -0.9203296 39.74507 0.4954275 0.2454484
    ## 6     28.82575    31.03614 -2.2103919 23.49061 0.6384475 0.4076153
    ## 7     31.16278    32.30423 -1.1414457 23.47851 0.5527574 0.3055408
    ##              P
    ## 1 0.0881275155
    ## 2 0.1509720981
    ## 3 0.0001664179
    ## 4 0.0007143244
    ## 5 0.0365587523
    ## 6 0.0043506307
    ## 7 0.0173552472
    ##   Mean Predict Mean Actual   Mean Dif Variance         R        R2
    ## 1     29.39132    30.01440 -0.6230727 38.24000 0.4134236 0.1709191
    ## 2     30.43006    31.28675 -0.8566916 41.14525 0.3528228 0.1244839
    ## 3     28.73987    30.51382 -1.7739516 25.00434 0.7734711 0.5982575
    ## 4     29.85156    29.96774 -0.1161867 30.81677 0.7221112 0.5214447
    ## 5     29.04279    29.96312 -0.9203296 39.74507 0.4954275 0.2454484
    ## 6     28.82575    31.03614 -2.2103919 23.49061 0.6384475 0.4076153
    ## 7     31.16278    32.30423 -1.1414457 23.47851 0.5527574 0.3055408
    ## 8     29.05627    29.61083 -0.5545640 39.14215 0.7438982 0.5533845
    ##              P
    ## 1 0.0881275155
    ## 2 0.1509720981
    ## 3 0.0001664179
    ## 4 0.0007143244
    ## 5 0.0365587523
    ## 6 0.0043506307
    ## 7 0.0173552472
    ## 8 0.0004010762
    ##   Mean Predict Mean Actual   Mean Dif Variance         R        R2
    ## 1     29.39132    30.01440 -0.6230727 38.24000 0.4134236 0.1709191
    ## 2     30.43006    31.28675 -0.8566916 41.14525 0.3528228 0.1244839
    ## 3     28.73987    30.51382 -1.7739516 25.00434 0.7734711 0.5982575
    ## 4     29.85156    29.96774 -0.1161867 30.81677 0.7221112 0.5214447
    ## 5     29.04279    29.96312 -0.9203296 39.74507 0.4954275 0.2454484
    ## 6     28.82575    31.03614 -2.2103919 23.49061 0.6384475 0.4076153
    ## 7     31.16278    32.30423 -1.1414457 23.47851 0.5527574 0.3055408
    ## 8     29.05627    29.61083 -0.5545640 39.14215 0.7438982 0.5533845
    ## 9     30.07892    30.20095 -0.1220254 38.69342 0.5334100 0.2845263
    ##              P
    ## 1 0.0881275155
    ## 2 0.1509720981
    ## 3 0.0001664179
    ## 4 0.0007143244
    ## 5 0.0365587523
    ## 6 0.0043506307
    ## 7 0.0173552472
    ## 8 0.0004010762
    ## 9 0.0226271157
    ##    Mean Predict Mean Actual   Mean Dif Variance         R        R2
    ## 1      29.39132    30.01440 -0.6230727 38.24000 0.4134236 0.1709191
    ## 2      30.43006    31.28675 -0.8566916 41.14525 0.3528228 0.1244839
    ## 3      28.73987    30.51382 -1.7739516 25.00434 0.7734711 0.5982575
    ## 4      29.85156    29.96774 -0.1161867 30.81677 0.7221112 0.5214447
    ## 5      29.04279    29.96312 -0.9203296 39.74507 0.4954275 0.2454484
    ## 6      28.82575    31.03614 -2.2103919 23.49061 0.6384475 0.4076153
    ## 7      31.16278    32.30423 -1.1414457 23.47851 0.5527574 0.3055408
    ## 8      29.05627    29.61083 -0.5545640 39.14215 0.7438982 0.5533845
    ## 9      30.07892    30.20095 -0.1220254 38.69342 0.5334100 0.2845263
    ## 10     29.28242    30.77313 -1.4907128 36.06248 0.4523063 0.2045810
    ##               P
    ## 1  0.0881275155
    ## 2  0.1509720981
    ## 3  0.0001664179
    ## 4  0.0007143244
    ## 5  0.0365587523
    ## 6  0.0043506307
    ## 7  0.0173552472
    ## 8  0.0004010762
    ## 9  0.0226271157
    ## 10 0.0594825171

``` r
counter <- 0
R5.DW.model.CV <- CV.func1(model.data3, testing.data3)
```

    ##   Mean Predict Mean Actual   Mean Dif Variance        R        R2
    ## 1     3.858059    3.839167 0.01889236  2.04603 0.510005 0.2601051
    ##             P
    ## 1 0.003987635
    ##   Mean Predict Mean Actual    Mean Dif Variance         R        R2
    ## 1     3.858059    3.839167  0.01889236 2.046030 0.5100050 0.2601051
    ## 2     3.851434    4.016750 -0.16531649 1.014118 0.5923269 0.3508511
    ##              P
    ## 1 0.0039876346
    ## 2 0.0005636707
    ##   Mean Predict Mean Actual    Mean Dif Variance         R        R2
    ## 1     3.858059    3.839167  0.01889236 2.046030 0.5100050 0.2601051
    ## 2     3.851434    4.016750 -0.16531649 1.014118 0.5923269 0.3508511
    ## 3     3.774203    3.936333 -0.16213065 1.397900 0.5303255 0.2812451
    ##              P
    ## 1 0.0039876346
    ## 2 0.0005636707
    ## 3 0.0025736325
    ##   Mean Predict Mean Actual    Mean Dif Variance         R        R2
    ## 1     3.858059    3.839167  0.01889236 2.046030 0.5100050 0.2601051
    ## 2     3.851434    4.016750 -0.16531649 1.014118 0.5923269 0.3508511
    ## 3     3.774203    3.936333 -0.16213065 1.397900 0.5303255 0.2812451
    ## 4     3.551045    4.181000 -0.62995468 1.310252 0.4882689 0.2384065
    ##              P
    ## 1 0.0039876346
    ## 2 0.0005636707
    ## 3 0.0025736325
    ## 4 0.0061913917
    ##   Mean Predict Mean Actual    Mean Dif Variance         R        R2
    ## 1     3.858059    3.839167  0.01889236 2.046030 0.5100050 0.2601051
    ## 2     3.851434    4.016750 -0.16531649 1.014118 0.5923269 0.3508511
    ## 3     3.774203    3.936333 -0.16213065 1.397900 0.5303255 0.2812451
    ## 4     3.551045    4.181000 -0.62995468 1.310252 0.4882689 0.2384065
    ## 5     3.809641    4.254250 -0.44460895 1.803517 0.4664091 0.2175375
    ##              P
    ## 1 0.0039876346
    ## 2 0.0005636707
    ## 3 0.0025736325
    ## 4 0.0061913917
    ## 5 0.0093767558
    ##   Mean Predict Mean Actual    Mean Dif Variance         R        R2
    ## 1     3.858059    3.839167  0.01889236 2.046030 0.5100050 0.2601051
    ## 2     3.851434    4.016750 -0.16531649 1.014118 0.5923269 0.3508511
    ## 3     3.774203    3.936333 -0.16213065 1.397900 0.5303255 0.2812451
    ## 4     3.551045    4.181000 -0.62995468 1.310252 0.4882689 0.2384065
    ## 5     3.809641    4.254250 -0.44460895 1.803517 0.4664091 0.2175375
    ## 6     3.614409    4.481083 -0.86667477 1.763701 0.4575836 0.2093828
    ##              P
    ## 1 0.0039876346
    ## 2 0.0005636707
    ## 3 0.0025736325
    ## 4 0.0061913917
    ## 5 0.0093767558
    ## 6 0.0110066043
    ##   Mean Predict Mean Actual    Mean Dif Variance         R        R2
    ## 1     3.858059    3.839167  0.01889236 2.046030 0.5100050 0.2601051
    ## 2     3.851434    4.016750 -0.16531649 1.014118 0.5923269 0.3508511
    ## 3     3.774203    3.936333 -0.16213065 1.397900 0.5303255 0.2812451
    ## 4     3.551045    4.181000 -0.62995468 1.310252 0.4882689 0.2384065
    ## 5     3.809641    4.254250 -0.44460895 1.803517 0.4664091 0.2175375
    ## 6     3.614409    4.481083 -0.86667477 1.763701 0.4575836 0.2093828
    ## 7     3.746553    4.279250 -0.53269689 1.563883 0.3738453 0.1397603
    ##              P
    ## 1 0.0039876346
    ## 2 0.0005636707
    ## 3 0.0025736325
    ## 4 0.0061913917
    ## 5 0.0093767558
    ## 6 0.0110066043
    ## 7 0.0418417473
    ##   Mean Predict Mean Actual    Mean Dif Variance         R        R2
    ## 1     3.858059    3.839167  0.01889236 2.046030 0.5100050 0.2601051
    ## 2     3.851434    4.016750 -0.16531649 1.014118 0.5923269 0.3508511
    ## 3     3.774203    3.936333 -0.16213065 1.397900 0.5303255 0.2812451
    ## 4     3.551045    4.181000 -0.62995468 1.310252 0.4882689 0.2384065
    ## 5     3.809641    4.254250 -0.44460895 1.803517 0.4664091 0.2175375
    ## 6     3.614409    4.481083 -0.86667477 1.763701 0.4575836 0.2093828
    ## 7     3.746553    4.279250 -0.53269689 1.563883 0.3738453 0.1397603
    ## 8     3.729924    3.792333 -0.06240943 1.600585 0.4393665 0.1930430
    ##              P
    ## 1 0.0039876346
    ## 2 0.0005636707
    ## 3 0.0025736325
    ## 4 0.0061913917
    ## 5 0.0093767558
    ## 6 0.0110066043
    ## 7 0.0418417473
    ## 8 0.0151303867
    ##   Mean Predict Mean Actual    Mean Dif  Variance         R        R2
    ## 1     3.858059    3.839167  0.01889236 2.0460300 0.5100050 0.2601051
    ## 2     3.851434    4.016750 -0.16531649 1.0141178 0.5923269 0.3508511
    ## 3     3.774203    3.936333 -0.16213065 1.3978995 0.5303255 0.2812451
    ## 4     3.551045    4.181000 -0.62995468 1.3102516 0.4882689 0.2384065
    ## 5     3.809641    4.254250 -0.44460895 1.8035168 0.4664091 0.2175375
    ## 6     3.614409    4.481083 -0.86667477 1.7637010 0.4575836 0.2093828
    ## 7     3.746553    4.279250 -0.53269689 1.5638829 0.3738453 0.1397603
    ## 8     3.729924    3.792333 -0.06240943 1.6005847 0.4393665 0.1930430
    ## 9     4.090904    3.845250  0.24565442 0.9542143 0.6062700 0.3675633
    ##              P
    ## 1 0.0039876346
    ## 2 0.0005636707
    ## 3 0.0025736325
    ## 4 0.0061913917
    ## 5 0.0093767558
    ## 6 0.0110066043
    ## 7 0.0418417473
    ## 8 0.0151303867
    ## 9 0.0003835621
    ##    Mean Predict Mean Actual    Mean Dif  Variance         R        R2
    ## 1      3.858059    3.839167  0.01889236 2.0460300 0.5100050 0.2601051
    ## 2      3.851434    4.016750 -0.16531649 1.0141178 0.5923269 0.3508511
    ## 3      3.774203    3.936333 -0.16213065 1.3978995 0.5303255 0.2812451
    ## 4      3.551045    4.181000 -0.62995468 1.3102516 0.4882689 0.2384065
    ## 5      3.809641    4.254250 -0.44460895 1.8035168 0.4664091 0.2175375
    ## 6      3.614409    4.481083 -0.86667477 1.7637010 0.4575836 0.2093828
    ## 7      3.746553    4.279250 -0.53269689 1.5638829 0.3738453 0.1397603
    ## 8      3.729924    3.792333 -0.06240943 1.6005847 0.4393665 0.1930430
    ## 9      4.090904    3.845250  0.24565442 0.9542143 0.6062700 0.3675633
    ## 10     3.801617    4.034667 -0.23304933 1.7788437 0.4965121 0.2465243
    ##               P
    ## 1  0.0039876346
    ## 2  0.0005636707
    ## 3  0.0025736325
    ## 4  0.0061913917
    ## 5  0.0093767558
    ## 6  0.0110066043
    ## 7  0.0418417473
    ## 8  0.0151303867
    ## 9  0.0003835621
    ## 10 0.0052573236

``` r
#Use func2 to run on DX (Since this has 3 explanatory variables)
counter <- 0
R5.DX.model.CV <- CV.func2(model.data2, testing.data2)
```

    ##   Mean Predict Mean Actual Mean Dif Variance         R        R2
    ## 1     9.077303    7.578074 1.499229 65.79404 0.4763523 0.2269116
    ##             P
    ## 1 0.007788926
    ##   Mean Predict Mean Actual   Mean Dif Variance         R        R2
    ## 1     9.077303    7.578074 1.49922928 65.79404 0.4763523 0.2269116
    ## 2     8.380375    8.333407 0.04696736 68.47190 0.5329368 0.2840216
    ##             P
    ## 1 0.007788926
    ## 2 0.002428025
    ##   Mean Predict Mean Actual   Mean Dif Variance         R         R2
    ## 1     9.077303    7.578074 1.49922928 65.79404 0.4763523 0.22691155
    ## 2     8.380375    8.333407 0.04696736 68.47190 0.5329368 0.28402162
    ## 3     8.208876    6.540481 1.66839483 66.11387 0.2538705 0.06445024
    ##             P
    ## 1 0.007788926
    ## 2 0.002428025
    ## 3 0.175825071
    ##   Mean Predict Mean Actual    Mean Dif Variance         R         R2
    ## 1     9.077303    7.578074  1.49922928 65.79404 0.4763523 0.22691155
    ## 2     8.380375    8.333407  0.04696736 68.47190 0.5329368 0.28402162
    ## 3     8.208876    6.540481  1.66839483 66.11387 0.2538705 0.06445024
    ## 4     8.119086    9.463111 -1.34402519 86.44584 0.4166097 0.17356361
    ##             P
    ## 1 0.007788926
    ## 2 0.002428025
    ## 3 0.175825071
    ## 4 0.022012874
    ##   Mean Predict Mean Actual    Mean Dif Variance         R         R2
    ## 1     9.077303    7.578074  1.49922928 65.79404 0.4763523 0.22691155
    ## 2     8.380375    8.333407  0.04696736 68.47190 0.5329368 0.28402162
    ## 3     8.208876    6.540481  1.66839483 66.11387 0.2538705 0.06445024
    ## 4     8.119086    9.463111 -1.34402519 86.44584 0.4166097 0.17356361
    ## 5     9.464424    8.444889  1.01953553 67.58944 0.4608563 0.21238849
    ##             P
    ## 1 0.007788926
    ## 2 0.002428025
    ## 3 0.175825071
    ## 4 0.022012874
    ## 5 0.010376503
    ##   Mean Predict Mean Actual    Mean Dif Variance         R         R2
    ## 1     9.077303    7.578074  1.49922928 65.79404 0.4763523 0.22691155
    ## 2     8.380375    8.333407  0.04696736 68.47190 0.5329368 0.28402162
    ## 3     8.208876    6.540481  1.66839483 66.11387 0.2538705 0.06445024
    ## 4     8.119086    9.463111 -1.34402519 86.44584 0.4166097 0.17356361
    ## 5     9.464424    8.444889  1.01953553 67.58944 0.4608563 0.21238849
    ## 6     9.947478    8.046333  1.90114484 40.27638 0.6269506 0.39306710
    ##              P
    ## 1 0.0077889259
    ## 2 0.0024280249
    ## 3 0.1758250712
    ## 4 0.0220128739
    ## 5 0.0103765027
    ## 6 0.0002094457
    ##   Mean Predict Mean Actual    Mean Dif Variance         R         R2
    ## 1     9.077303    7.578074  1.49922928 65.79404 0.4763523 0.22691155
    ## 2     8.380375    8.333407  0.04696736 68.47190 0.5329368 0.28402162
    ## 3     8.208876    6.540481  1.66839483 66.11387 0.2538705 0.06445024
    ## 4     8.119086    9.463111 -1.34402519 86.44584 0.4166097 0.17356361
    ## 5     9.464424    8.444889  1.01953553 67.58944 0.4608563 0.21238849
    ## 6     9.947478    8.046333  1.90114484 40.27638 0.6269506 0.39306710
    ## 7     9.066726    8.258963  0.80776327 68.07627 0.4863714 0.23655718
    ##              P
    ## 1 0.0077889259
    ## 2 0.0024280249
    ## 3 0.1758250712
    ## 4 0.0220128739
    ## 5 0.0103765027
    ## 6 0.0002094457
    ## 7 0.0064253262
    ##   Mean Predict Mean Actual    Mean Dif Variance         R         R2
    ## 1     9.077303    7.578074  1.49922928 65.79404 0.4763523 0.22691155
    ## 2     8.380375    8.333407  0.04696736 68.47190 0.5329368 0.28402162
    ## 3     8.208876    6.540481  1.66839483 66.11387 0.2538705 0.06445024
    ## 4     8.119086    9.463111 -1.34402519 86.44584 0.4166097 0.17356361
    ## 5     9.464424    8.444889  1.01953553 67.58944 0.4608563 0.21238849
    ## 6     9.947478    8.046333  1.90114484 40.27638 0.6269506 0.39306710
    ## 7     9.066726    8.258963  0.80776327 68.07627 0.4863714 0.23655718
    ## 8     9.911789    8.897852  1.01393692 57.60797 0.5172116 0.26750781
    ##              P
    ## 1 0.0077889259
    ## 2 0.0024280249
    ## 3 0.1758250712
    ## 4 0.0220128739
    ## 5 0.0103765027
    ## 6 0.0002094457
    ## 7 0.0064253262
    ## 8 0.0034244896
    ##   Mean Predict Mean Actual    Mean Dif Variance         R         R2
    ## 1     9.077303    7.578074  1.49922928 65.79404 0.4763523 0.22691155
    ## 2     8.380375    8.333407  0.04696736 68.47190 0.5329368 0.28402162
    ## 3     8.208876    6.540481  1.66839483 66.11387 0.2538705 0.06445024
    ## 4     8.119086    9.463111 -1.34402519 86.44584 0.4166097 0.17356361
    ## 5     9.464424    8.444889  1.01953553 67.58944 0.4608563 0.21238849
    ## 6     9.947478    8.046333  1.90114484 40.27638 0.6269506 0.39306710
    ## 7     9.066726    8.258963  0.80776327 68.07627 0.4863714 0.23655718
    ## 8     9.911789    8.897852  1.01393692 57.60797 0.5172116 0.26750781
    ## 9    10.170786    8.213296  1.95749013 62.53101 0.4359880 0.19008552
    ##              P
    ## 1 0.0077889259
    ## 2 0.0024280249
    ## 3 0.1758250712
    ## 4 0.0220128739
    ## 5 0.0103765027
    ## 6 0.0002094457
    ## 7 0.0064253262
    ## 8 0.0034244896
    ## 9 0.0160212623
    ##    Mean Predict Mean Actual    Mean Dif Variance         R         R2
    ## 1      9.077303    7.578074  1.49922928 65.79404 0.4763523 0.22691155
    ## 2      8.380375    8.333407  0.04696736 68.47190 0.5329368 0.28402162
    ## 3      8.208876    6.540481  1.66839483 66.11387 0.2538705 0.06445024
    ## 4      8.119086    9.463111 -1.34402519 86.44584 0.4166097 0.17356361
    ## 5      9.464424    8.444889  1.01953553 67.58944 0.4608563 0.21238849
    ## 6      9.947478    8.046333  1.90114484 40.27638 0.6269506 0.39306710
    ## 7      9.066726    8.258963  0.80776327 68.07627 0.4863714 0.23655718
    ## 8      9.911789    8.897852  1.01393692 57.60797 0.5172116 0.26750781
    ## 9     10.170786    8.213296  1.95749013 62.53101 0.4359880 0.19008552
    ## 10     7.396559    7.855704 -0.45914501 50.50580 0.6168782 0.38053874
    ##               P
    ## 1  0.0077889259
    ## 2  0.0024280249
    ## 3  0.1758250712
    ## 4  0.0220128739
    ## 5  0.0103765027
    ## 6  0.0002094457
    ## 7  0.0064253262
    ## 8  0.0034244896
    ## 9  0.0160212623
    ## 10 0.0002827130

``` r
#Export the info to a table
#write.table(yield.model.CV, file = "yield.model.1000.CV.csv", sep = ",", col.names = T)
#write.table(R5.DX.model.CV, file = "R5.DX.model.1000.CV.csv", sep = ",", col.names = T)
#write.table(R5.DW.model.CV, file = "R5.DW.model.1000.CV.csv", sep = ",", col.names = T)

#This function will count the number of times the model predictions had a significant correlation to the actual data we observed
#It will return a % accuracy (% of the 1000 times the correlation was significant at P < 0.05)
#Reset sig and insig counters each time!
sig.insig <- function(P){
  for (i in P){
    if (i < 0.05){
     sig <- sig + 1
   }
   else if (i > 0.05){
     insig <- sig + 1
   }
  }
  return((sig/(sig + insig))*100)
}
sig <- 0
insig <- 0
yield.model.acc <- sig.insig(yield.model.CV$P)
sig <- 0
insig <- 0
R5.DW.model.acc <- sig.insig(R5.DW.model.CV$P)
sig <- 0
insig <- 0
R5.DX.model.acc <- sig.insig(R5.DX.model.CV$P)

#Check out the % accuracy
yield.model.acc
```

    ## [1] 46.66667

``` r
R5.DW.model.acc
```

    ## [1] 100

``` r
R5.DX.model.acc
```

    ## [1] 75
