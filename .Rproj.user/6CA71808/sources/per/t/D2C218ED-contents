
# # ---- Get All boards for all markets and engines
# engines <- issEngines()
#
# allboards <- sapply(engines, function(x) {
#
#   engine = x
#   markets <- issMarkets(engine=engine)
#
#   sapply(markets, function(x) issBoards(engine=engine, market=x), simplify = F)
#
#   }, simplify = F)
#
#
# issGetFutures(sectypes = 'RI')
#
#
# bonds <- issGetBonds(board = 'TQOB')
#
#
# bond_yields <- left_join(bonds[[1]], bonds[[4]], by='SECID') %>% filter(is.na(SYSTIME))
#
# candles <- issCandles(engine = 'futures', market = 'forts', security = 'RIM9', interval = 'c')
#
# stocks <- issGetStocks(board ='TQBR' , tablenames = 'marketdata')[[1]]
