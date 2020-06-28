#--- LOAD HISTORICAL YIELDS  ---

#' Historical bond yields
#'
#' @param board specify trading board (TQOB, EQOB, TQCB, etc.)
#' @param date
#'
#' @return
#' @export
#'
#' @examples
issHistoryYields <- function(board='', date='2020-06-01'){

  board_str=''

  if(board!='')
    board_str <- paste0('boards/', as.character(board),'/')

  date <- format(as.Date(date), '%Y-%m-%d')

  path <- paste0('/iss/history/engines/stock/markets/bonds/', board_str, 'yields?date=', date)

  #data <- Read_ISS(path=, tablenames = tablenames)
  data <- Load_All_Pages(path = path, tablename = 'history_yields')
  data
}


#--- LOAD HISTORICAL DATA  ---

#' Historical data for securities at the special date
#'
#' @param engine
#' @param market
#' @param board specify trading board (TQOB, EQOB, TQCB, etc.)
#' @param date
#'
#' @return
#' @export
#'
#' @examples
issHistorySecurities <- function(engine='stock', market='bonds', board='', date='2020-06-01'){


  board_str=''

  if(board!='')
    board_str <- paste0('/boards/', as.character(board),'/')

  date <- format(as.Date(date), '%Y-%m-%d')

  path <- paste0('/iss/history/engines/', engine, '/markets/', market, board_str, 'securities?date=', date)

  #data <- Read_ISS(path=, tablenames = tablenames)
  data <- Load_All_Pages(path = path, tablename = 'history')
  data
}

