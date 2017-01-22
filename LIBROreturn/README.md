
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **LIBROreturn** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet : LIBROreturn

Published in : 'Investing with cryptocurrencies - A LIquidity Bounded Risk-return Optimization
approach'

Description : 'Add Crypto currencies to Portugal stocks or DAX30 stocks to form portfolios using
Markowitz(1952) method.'

Keywords : crypto, CRIX, cryptocurrency, portfolio, variance, plot, time-series, returns

Author : Mingyang Li, Simon Trimborn

Submitted : Wed, January 18, 2016 by Mingyang Li

Datafile : processed_data.RData

```

![Picture1](Cum_return_DAX30_Cryptos.png)

![Picture2](Cum_return_PorStocks_Cryptos.png)


### R Code:
```r
### this is the code to plot the cumulative return of portfolios with or without
### crypto currencies.

# setwd('...')
rm(list = ls())
graphics.off()

libraries = c("xts", "plyr", "dplyr")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

load("processed_data.RData")

# write a function to conduct Markowitz portfolio formation
Markwtz = function(Asset, sigma = NULL, rf = NULL) {
    if (is.xts(Asset) == T) 
        time = index(Asset)
    mu_hat = colMeans(Asset, na.rm = T)
    Sig_hat = cov(Asset, use = "pairwise.complete.obs")
    nn = ncol(Asset)
    TT = nrow(Asset)
    ls = rep(1, times = nn)
    
    ## define intermediate variables
    AA = mu_hat %*% solve(Sig_hat, mu_hat)
    BB = mu_hat %*% solve(Sig_hat, ls)
    CC = ls %*% solve(Sig_hat, ls)
    
    ## calculate the global minimum variance value or market portfolio
    wei_Mkwtz_g = solve(Sig_hat, ls)/CC
    ret_Mkwtz_g = Asset %*% wei_Mkwtz_g
    ret_Mkwtz_g = xts(ret_Mkwtz_g, order.by = time)
    outlist = list(ret_Mkwtz_g = ret_Mkwtz_g, time = time, wei_Mkwtz_g = wei_Mkwtz_g)
    
    
    ## calculate weight and return given the risk constraints
    if (is.null(sigma) == F && is.null(rf)) {
        lam1 = sqrt((AA * CC - BB^2)/(CC * sigma^2 - 1))
        lam2 = (BB - lam1)/CC
        # ret_Mkwtz = lam1 * sigma^2 + lam2
        wei_Mkwtz = 1/lam1 * solve(Sig_hat, mu_hat) - lam2/lam1 * solve(Sig_hat, 
            ls)
        ret_Mkwtz = Asset %*% wei_Mkwtz
        ret_Mkwtz = xts(ret_Mkwtz, order.by = time)
        outlist$ret_Mkwtz = ret_Mkwtz
        outlist$wei_Mkwtz = c(1 - sum(wei_Mkwtz), wei_Mkwtz)
    }
    if (is.null(sigma) == F && is.null(rf) == F) {
        wei_Mkwtz = sigma * solve(Sig_hat, mu_hat)/sqrt(AA)
        ret_Mkwtz = Asset %*% wei_Mkwtz + rf * (1 - sum(wei_Mkwtz))
        ret_Mkwtz = xts(ret_Mkwtz, order.by = time)
        outlist$ret_Mkwtz = ret_Mkwtz
        outlist$wei_Mkwtz = c(1 - sum(wei_Mkwtz), wei_Mkwtz)
    }
    return(outlist)
}

# write a function to conduct portfolio formation with or without certain
# additional assets, here, this additional assets means crypto currencies.
add_asset = function(Asset, add, f, get, ...) {
    nc1 = NCOL(Asset)
    nc2 = NCOL(add)
    aa = cbind(Asset, add)
    aa = na.omit(aa)
    timeind = index(aa)
    bb1 = f(aa, ...)
    bb2 = f(aa[, 1:nc1], ...)
    bb = list(both = bb1, asset1 = bb2, time_index = timeind)
    return(bb)
}

# for Portugal Stocks
pocp_ret = add_asset(Asset = ret_pop, add = ret_cpp, f = function(x) Markwtz(x, 
    sigma = 0.002, rf = 0.9/360/100)$ret_Mkwtz)
time = pocp_ret$time_index
pocp_ret$time_index = NULL
sapply(pocp_ret, cumsum) %>% xts(order.by = time) %>% plot.zoo(plot.type = "single", 
    col = c("blue", "brown"), type = "l", lwd = 2, xlab = "date", ylab = "return", 
    main = "Cumulative Return of Portugal Stocks with/without Cryptos")
grid()

# for DAX30 stocks
dacp_ret = add_asset(Asset = ret_dap, add = ret_cpp, f = function(x) Markwtz(x, 
    sigma = 0.002, rf = 0.9/360/100)$ret_Mkwtz)
time = index(dacp_ret$both)
dacp_ret$time_index = NULL
sapply(dacp_ret, cumsum) %>% xts(order.by = time) %>% plot.zoo(plot.type = "single", 
    col = c("blue", "brown"), type = "l", lwd = 2, xlab = "date", ylab = "return", 
    main = "Cumulative Return of DAX30 Stocks with/without Cryptos")
grid()


```
