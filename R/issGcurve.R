

#' G-Curve parametres
#'
#' @param date a date in the past
#' @param intraday if TRUE, the function returns all intraday coefficients
#' @param time the last known coefficients set for the given time
#'
#' @return
#' @export
#'
#' @examples
issGcurve <- function(date=NULL, intraday=F, time=''){

  if(intraday){

    sec_path = paste0('/iss/history/engines/stock/zcyc?date=', date, '&time=',time)

  } else {

    sec_path = paste0('/iss/engines/stock/zcyc?date=', date)
  }

  return(Read_ISS(path = sec_path))

}


