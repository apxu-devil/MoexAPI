# {
# library(rvest)
# library(dplyr)
# }

# NOTES
# https://iss.moex.com/iss/engines/stock - working time
# https://iss.moex.com/iss/engines/stock/markets/shares/boards/TQBR/securities/VTBR/trades?start=1000 - all trades
# https://iss.moex.com/iss/engines/stock/markets/shares/boards/TQBR/securities/VTBR/candles?from=2020-03-01&til=2020-04-01&interval=1&start=10000



# --- GET HISTORICAL CANDLES ---
#' OHLC data for a security.
#'
#' @param engine
#' @param market
#' @param board
#' @param security
#' @param from
#' @param til
#' @param interval
#'
#' @return
#' @export
#'
#' @examples
issCandles <- function(engine='stock', market='shares', board='TQBR', security='VTBR', from='2020-01-01', til='2037-12-31', interval=10){

  interval <- switch (interval,
                      '1m'  = 1,
                      '10m' = 10,
                      'h'   = 60,
                      'd'   = 24,
                      'w'   = 7
                      )
  if(is.null(interval)) stop('Wrong interval. Try: 1m, 10m, h, d, w')

  sec_path <- paste0('/iss/engines/', engine, '/markets/', market)

  if(board!='')
    sec_path <- paste0('/iss/engines/', engine, '/markets/', market, '/boards/', board)

  sec_path=paste0(sec_path, '/securities/', security)


  rows = 1
  page = 0
  all_candles <- data.frame()
  page_size <- 5000

  til  = format(as.Date( til), '%Y-%m-%d')
  from = format(as.Date(from), '%Y-%m-%d')

  suffix <- ifelse(grepl('\\?', sec_path), '&', '?')

  while (rows>0 | page==2) {

    iss_path <- paste0(sec_path, '/candles', suffix,
                       'start=', page_size*page,
                       '&from=', from,
                       '&til=', til,
                       '&interval=', interval)


    candles <- Read_ISS( iss_path, tablenames = 'candles' )[[1]]

    rows <- nrow(candles)

    if(rows>0){

      all_candles <- bind_rows(all_candles, candles)
      page_size <- rows
      page <- page + 1
    }

  }

  return(all_candles)

}

# EXAMPLE
# Load_All_Pages(path='/iss/engines/stock/markets/shares/boards/TQBR/securities/VTBR/trades', tablename='trades')


# history_iss <- Read_ISS( '/iss/engines/stock/markets/shares/boards/TQBR/securities/VTBR/candles?from=2020-03-01&til=2020-04-30&interval=60', tablenames = 'candles')[[1]]

# ISS Reference page







# --- GET SECURITIES LIST FOR A MARKET OR MARKET/BOARD ---
#' Loads all trades for a given security / board / market
#'
#' @param engine
#' @param market
#' @param board
#' @param security
#' @param raw
#'
#' @return
#' @export
#'
#' @examples
issTrades <- function(engine='stock', market='shares', board='EQBR', security='VTBR', raw=F){

  sec_path <- paste0('/iss/engines/', engine, '/markets/', market)

  if(board!='')
    sec_path <- paste0('/iss/engines/', engine, '/markets/', market, '/boards/', board)

  data <- Load_All_Pages(path=paste0(sec_path, '/securities'),tablename='trades' )

  return(data)

}


#' Load all data for a multipager
#'
#' @param path
#' @param tablename
#'
#' @return
#' @export
#'
#' @examples
Load_All_Pages <- function(path, tablename){

  rows = 1
  page = 0
  all_trades <- data.frame()
  page_size <- 5000

  suffix <- ifelse(grepl('\\?', path), '&', '?')

  while (rows>0 | page==2) {

    iss_path <- paste0(path, suffix, 'start=', page_size*page)

    trades <- Read_ISS( iss_path, tablenames = tablename )[[1]]

    rows <- nrow(trades)

    if(rows>0){

      all_trades <- bind_rows(all_trades, trades)
      page_size <- rows
      page <- page + 1
    }

  }
  return(all_trades)

}





#' List of reference pages
#'
#' @return
#' Function returns the referce page, structured in a table.
#' @export
#'
#' @examples
#'
issReference <- function(){


  iss_reference <- read_html('https://iss.moex.com/iss/reference/')

  iss_cmdss = iss_reference %>% xml_find_all('//dt/a') %>% html_text()

  iss_helps = iss_reference %>% xml_find_all('//dt/a') %>% html_attr('href')

  iss_descr = iss_reference %>% xml_find_all('//dd') %>% html_text(trim = T)

  iss_descr = gsub('[\n]|[\r]', ' ', iss_descr)

  data.frame(iss_cmdss, iss_descr, iss_helps)
}

#iss_cmdss[order(iss_cmdss)] %>% write.csv(file = 'iss_ref')

# Read_ISS(path='/iss/engines/futures/markets/options/securities?sectypes=ri', tablenames = 'securities')


#--- GET STOCKS  ---

#' Bonds data
#'
#' @param boards specify trading board (TQOB, EQOB, TQCB, etc.)
#' @param sectypes futures ticker or underlying
#' @param tablenames securities, marketdata, marketdata_yields, dataversion are avaliable
#'
#' @return
#' @export
#'
#' @examples
issGetStocks <- function(board='', sectypes='', tablenames=''){

  board_str=''

  if(board!='')
    board_str <- paste0('boards/', as.character(board),'/')

  data <- Read_ISS(path=paste0('/iss/engines/stock/markets/shares/',board_str,'securities?sectypes=',sectypes), tablenames = tablenames)
  data
}




#--- GET BONDS  ---

#' Bonds data
#'
#' @param boards specify trading board (TQOB, EQOB, TQCB, etc.)
#' @param sectypes futures ticker or underlying
#' @param tablenames securities, marketdata, marketdata_yields, dataversion are avaliable
#'
#' @return
#' @export
#'
#' @examples
issGetBonds <- function(board='', sectypes='', tablenames=''){

  board_str=''

  if(board!='')
    board_str <- paste0('boards/', as.character(board),'/')

  data <- Read_ISS(path=paste0('/iss/engines/stock/markets/bonds/',board_str,'securities?sectypes=',sectypes), tablenames = tablenames)
  data
}




#--- GET FUTURES ---

#' FORTS futures data
#'
#' @param sectypes futures ticker or underlying
#' @param tablenames
#'
#' @return
#' @export
#'
#' @examples
issGetFutures <- function(sectypes = '', tablenames=''){

  data <- Read_ISS(path=paste0('/iss/engines/futures/markets/forts/securities?sectypes=',sectypes), tablenames = tablenames)
  data
}


#--- GET OPTIONS ---

#' FORTS options data
#'
#' @param sectypes futures ticker or underlying
#' @param tablenames
#'
#' @return
#' @export
#'
#' @examples
issGetOptions <- function(sectypes = '', tablenames=''){

  data <- Read_ISS(path=paste0('/iss/engines/futures/markets/options/securities?sectypes=',sectypes), tablenames = tablenames)
  data
}




# ---- GET ENGINES LIST ----
#' Returns a list of data engines
#'
#' @param raw boolean
#'
#' @return
#' @export
#'
#' @examples
issEngines <- function( raw=F ){

  data <- Read_ISS(path='/iss/engines')

  if(raw)
    return(data)
  else
    return(data$engines$name)

}


# ---- GET MARKET LIST FOR AN ENGINE ----

#' Returns a list of the markets for a given engine
#'
#' @param engine
#' @param raw
#'
#' @return
#' @export
#'
#' @examples
issMarkets <- function(engine='stock', raw=F ){

  data <- Read_ISS(path=paste0('/iss/engines/', engine, '/markets') )

  if(raw)
    return(data)
  else
    return(data$markets$NAME)

}




# ---- GET BOARDS LIST FOR A MARKET ----

#' Get boards list for a market
#'
#' @param engine
#' @param market
#' @param raw
#'
#' @return
#' @export
#'
#' @examples
issBoards <- function(engine='stock', market='shares', raw=F ){

  data <- Read_ISS(path=paste0('/iss/engines/', engine, '/markets/', market, '/boards') )

  if(raw)
    return(data)
  else
    return(data$boards$boardid)

}


# --- GET SECURITIES LIST FOR A MARKET OR MARKET/BOARD ---
#' Get a security list fot a market or a board
#'
#' @param engine
#' @param market
#' @param board
#' @param raw
#'
#' @return
#' @export
#'
#' @examples
issSecurities <- function(engine='stock', market='shares', board='', raw=F){

  sec_path <- paste0('/iss/engines/', engine, '/markets/', market)

  # if(boardgroup!='')
  #   sec_path <- paste0('/iss/engines/', engine, '/markets/', market, '/boardgroups/', boardgroup)

  # If board is given, it overhelms boardgroups
  if(board!='')
    sec_path <- paste0('/iss/engines/', engine, '/markets/', market, '/boards/', board)

  data <- Read_ISS(path=paste0(sec_path, '/securities') )

  return(data)

}



#' Basic funtion - returns data from any iss adress
#'
#' @param path
#' @param tablenames
#'
#' @return
#' @export
#'
#' @examples
Read_ISS <- function(path='', tablenames=''){

  suffix <- ifelse(grepl('\\?', path),'&','?')

  iss_xml = read_xml(paste0('https://iss.moex.com', path, suffix, 'iss.only=', paste(tablenames, collapse = ',')) )


  # data id
  # xml_children(futdata)  %>% xml_attrs()

  # Vector of table names (id values)
  table_ids = xml_children(iss_xml)  %>% xml_attr(attr='id')


  iss_tables <- lapply( c(1:length(table_ids)), function(x){

    iss_document_data <- xml_nodes( iss_xml, xpath = paste0('//document/data[',x,']' ) )
    MakeTable(iss_document_data)

  } )

  names(iss_tables) = table_ids

  return(iss_tables)

}



#' Make tables from xml nodes <data>
#'
#' @param iss_document_data
#'
#' @return
#' @export
#'
#' @examples
MakeTable <- function(iss_document_data){

  # + ---  Converts variable to a given format --- +

  set_type <- function(x, type){

     # print(x)
     # print(type)
     # browser()
    # TODO: tryCatch; add undefined type
    x[x=='null' | x==''] <- NA

    if(type=='date')
      x[x=='0000-00-00'] <- NA

    result <- switch (type,
                       double   = as.double(x),
                       int32    = as.integer(x),
                       int64    = as.numeric(x),
                       string   = as.character(x),
                       date     = as.Date(x),
                       datetime = as.POSIXct(x)
    )

    if(is.null(result))
      result <- x

    return(result)
  }


  # Columns description
  col_descr <- xml_nodes( iss_document_data, xpath = './/metadata/columns/column')

  col_names <-  col_descr %>% xml_attr('name')
  col_types <-  col_descr %>% xml_attr('type')


  # Table values

  table_xml <-xml_nodes( iss_document_data, xpath = './/rows/row')


  table_df <- lapply( c(1:length(col_names)),
                      function(x) {

                        xml_attr(table_xml, col_names[x]) %>% set_type(., col_types[x])
                      }

  ) %>% as.data.frame( ., stringsAsFactors=FALSE )

  names(table_df) <- col_names

 return(table_df)
}



