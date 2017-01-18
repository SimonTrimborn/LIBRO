rm(list = ls(all = TRUE))
# please change your working directory 
# setwd('C:/...')

# install and load packages
libraries = c("xts", "readxl", "dplyr", "IDPmisc")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

load('CryptoData.RData')
spp = read_excel('S&P500comPrice.xlsx', skip = 1, 
    col_types = c('date', rep('numeric', 500)))
spv = read_excel('S&P500comVolume.xlsx', skip = 1, 
    col_types = c('date', rep('numeric', 500)))
dap = read_excel('DAX30comPrice.xlsx', skip = 1, 
    col_types = c('date', rep('numeric', 30)))
dav = read_excel('DAX30comVolume.xlsx', skip = 1, 
    col_types = c('date', rep('numeric', 30)))
pop = read_excel('PSIcomPrice.xlsx', skip = 1, 
    col_types = c('date', rep('numeric', 46)))
pov = read_excel('PSIcomVolume.xlsx', skip = 1, 
    col_types = c('date', rep('numeric', 46)))
pot = read_excel('PSIcomTurnover.xlsx', skip = 1, 
    col_types = c('date', rep('numeric', 46)))

xts_spp = xts(spp[, 2:ncol(spp)], order.by = spp[[1]])
xts_spv = xts(spv[, 2:ncol(spv)], order.by = spv[[1]])
xts_dap = xts(dap[, 2:ncol(dap)], order.by = dap[[1]])
xts_dav = xts(dav[, 2:ncol(dav)], order.by = dav[[1]])
xts_pop = xts(pop[, 2:ncol(pop)], order.by = pop[[1]])
xts_pov = xts(pov[, 2:ncol(pov)], order.by = pov[[1]])
xts_pot = xts(pot[, 2:ncol(pot)], order.by = pot[[1]])

ret_spp = diff(log(xts_spp))
ret_dap = diff(log(xts_dap))
ret_pop = diff(log(xts_pop))

## calculate turnover using daily data for USA and Germany
xts_spva = xts_spp * xts_spv
xts_dava = xts_dap * xts_dav

## calculate Amihud
amhd_sp = abs(ret_spp) / xts_spva
amhd_da = abs(ret_dap) / xts_dava
amhd_po = abs(ret_pop) / xts_pot

xts_cpp  = xts(coins_price, order.by = as.Date(row.names(coins_price)))
xts_cpv  = xts(coins_vol, order.by = as.Date(row.names(coins_vol)))
xts_cpva = xts_cpp * xts_cpv
ret_cpp  = diff(log(xts_cpp))
amhd_cp  = abs(ret_cpp) / xts_cpva

### get rid of assets that have too much NA
sum0NA = function(x) sum(is.na(x) | x==0)/length(x) 

# for SP500
aa = apply(ret_spp, 2, sum0NA) < 0.8
bb = apply(amhd_sp, 2, sum0NA) < 0.8
amhd_sp = amhd_sp[,(aa & bb)]
ret_spp = ret_spp[,(aa & bb)]

# for DAX30
aa = apply(ret_dap, 2, sum0NA) < 0.8
bb = apply(amhd_da, 2, sum0NA) < 0.8
amhd_da = amhd_da[,(aa & bb)]
ret_dap = ret_dap[,(aa & bb)]

# for PSI
aa = apply(ret_pop, 2, sum0NA) < 0.8
bb = apply(amhd_po, 2, sum0NA) < 0.8
amhd_po = amhd_po[,(aa & bb)]
ret_pop = ret_pop[,(aa & bb)]

# for cryptos
aa = apply(ret_cpp, 2, sum0NA) < 0.05
bb = apply(amhd_cp, 2, sum0NA) < 0.05
amhd_cp = amhd_cp[,(aa & bb)]
ret_cpp = ret_cpp[,(aa & bb)]

sd_ret = list()
sd_ret$cpp = apply(ret_cpp, 2, function(x) sd(arima(NaRV.omit(x), 
    order = c(1,0,1))$residuals))
sd_ret$spp = apply(ret_spp, 2, function(x) sd(arima(NaRV.omit(x), 
    order = c(1,0,1))$residuals))
sd_ret$dap = apply(ret_dap, 2, function(x) sd(arima(NaRV.omit(x), 
    order = c(1,0,1))$residuals))
sd_ret$pop = apply(ret_pop, 2, function(x) sd(arima(NaRV.omit(x), 
    order = c(1,0,1))$residuals))
LL = max(sapply(sd_ret, length))
sd_ret = lapply(sd_ret, function(x) {
    if (length(x)<LL) x = c(x, rep(NA, times = LL-length(x)))
    return(x) 
})
Reduce(cbind, sd_ret) %>% boxplot(names = c('Cryptos', 'S&P500', 'DAX30', 
    'Portugal Stocks'), main = 'Compare standard deviation of demeaned 
    Assets log returns', ylab = 'standard deviation')
