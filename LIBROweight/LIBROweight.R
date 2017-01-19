### this is the code to plot the weight of portfolios formed by Markowitz method
### with or without crypto currencies.

# please change your working directory 
# setwd('...')
rm(list = ls())
graphics.off()

libraries = c("xts")
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

# for Portugal stocks with/without Cryptos
pocp_wei = add_asset(Asset = ret_pop, add = ret_cpp, f = function(x) Markwtz(x, 
    sigma = 0.002, rf = 0.9/360/100)$wei_Mkwtz)
maxy = max(c(pocp_wei$both[-1], pocp_wei$asset1[-1]))
miny = min(c(pocp_wei$both[-1], pocp_wei$asset1[-1]))
plot(pocp_wei$both[-1], type = "p", col = "blue", pch = 20, panel.first = grid(), 
    xlab = "assets", ylab = "weights", main = "Weights of Portogal Stocks with/without Cryptos")
points(pocp_wei$asset1[-1], type = "p", col = "brown", pch = 20)
abline(v = ncol(ret_pop) + 0.5, lty = 2, col = "black", lwd = 2)

# for DAX30 stocks with/without Cryptos
dacp_wei = add_asset(Asset = ret_dap, add = ret_cpp, f = function(x) Markwtz(x, 
    sigma = 0.002, rf = 0.9/360/100)$wei_Mkwtz)
maxy = max(c(dacp_wei$both[-1], dacp_wei$asset1[-1]))
miny = min(c(dacp_wei$both[-1], dacp_wei$asset1[-1]))
plot(dacp_wei$both[-1], type = "p", col = "blue", pch = 20, ylim = c(miny, maxy), 
    panel.first = grid(), xlab = "assets", ylab = "weights", main = "Weights of DAX30 Stocks with/without Cryptos")
points(dacp_wei$asset1[-1], type = "p", col = "brown", pch = 20)
abline(v = ncol(ret_dap) + 0.5, lty = 2, col = "black", lwd = 2)

