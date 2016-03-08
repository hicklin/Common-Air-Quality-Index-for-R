# The following function returnes the common air quality index (caqi) for traffic. First the sub-index (si) of each
# pollutant is calculated, then the maximum si is retured. NO2 and PM10 are mandatory while PM2.5 and CO are optional.

# Function inputs
# no2:          NO2 hourly value / maximum hourly value in µg/m3.
# pm10:         PM10 hourly value / daily value in µg/m3.
# pm10_hourly:  Indincates whether pm10 is an hourly average (TRUE) or a 24-hour average (FALSE). Default is TRUE.
# pm2.5:        PM2.5 hourly value / daily value in µg/m3.
# pm2.5_hourly: Indincates whether pm2.5 is an hourly average (TRUE) or a 24-hour average (FALSE). Default is TRUE.
# co:           CO 8 hours moving average / maximum 8 hours moving average in µg/m3.
# print_si:     Prints all the sub-indices. Default is FALSE.

caqi_t <- function(no2, pm10, pm10_hourly=TRUE, pm2.5=NULL, pm2.5_hourly=T, co=NULL, print_si=F){
  # Calculating the NO2 sub-index (si_no2)
  if(no2 < 100){
    si_no2 <- no2/2
  } else if (no2 > 200){
    si_no2 <- (no2/8)+50
  } else {
    si_no2 <- (no2/4) + 25
  }
  
  # Calculating the PM10 sub-index (si_pm10).
  # Different critera exist for PM10 at hourly and 24-h averages
  if(pm10_hourly == TRUE){
    # hourly average
    if(pm10 < 50){
      si_pm10 <- pm10
    } else if(pm10 > 90){
      si_pm10 <- ((5*pm10)/18) + 50
    } else {
      si_pm10 <- ((5*pm10)/8) + 18.75
    }
  } else {
    # 24-h average
    if(pm10 < 50){
      si_pm10 <- (5*pm10)/3
    } else if(pm10 > 90){
      si_pm10 <- (pm10/2) + 50
    } else {
      si_pm10 <- ((5*pm10)/4) + 12.5
    }
  }
  
  # Calculating the PM2.5 sub-index (si_pm2.5) if available.
  # Different critera exist for PM2.5 at hourly and 24-h averages
  if(!is.null(pm2.5)){
    
    if(pm2.5_hourly == TRUE){
      if(pm2.5 < 30){
        si_pm2.5 <- (5*pm2.5)/3
      } else if(pm2.5 > 55){
        si_pm2.5 <- ((5*pm2.5)/11) + 50
      } else {
        si_pm2.5 <- pm2.5 + 20
      }
    } else {
      if(pm2.5 < 30){
        si_pm2.5 <- (5*pm2.5)/2
      } else {
        si_pm2.5 <- ((5*pm2.5)/6) + 50
      }
    }
  } else {
    si_pm2.5 <- NULL
  }
  
  # Calculating the CO sub-index (si_co)
  # NOTE: CO must be inputted as an 8-hour moving average
  if(!is.null(co)){
    if(co < 5000){
      si_co <- co/200
    } else if (co > 10000){
      si_co <- (co/400) + 50
    } else {
      si_co <- (co/100) - 25
    } 
  } else {
    si_co <- NULL
  }
  
  if(print_si){
    print('Sub-indices')
    print('***********')
    print(paste0('NO2:   ', si_no2, collapse=NULL))
    print(paste0('PM10:  ', si_pm10, collapse=NULL))
    print(paste0('PM2.5: ', si_pm2.5, collapse=NULL))
    print(paste0('CO:    ', si_co, collapse=NULL))
  }
  
  return(max(si_no2, si_pm10, si_pm2.5, si_co))
}

