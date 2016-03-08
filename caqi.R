# This script defines functions for calculating the common air quality index (caqi) for traffic (caqi_t) and city
# background (caqi_b) as descibed in the Common Information to European Air report entitled "CAQI Air quality index
# Comparing" published on the 23rd of May 2012. The document can be found at 
# http://www.airqualitynow.eu/download/CITEAIR-Comparing_Urban_Air_Quality_across_Borders.pdf


# The following function returns the common air quality index (caqi) for traffic. First the sub-index (si) of each
# pollutant is calculated, then the maximum si is retured. NO2 and PM10 are mandatory while PM2.5 and CO are optional.

# Function inputs
# no2:          NO2 hourly value / maximum hourly value in ug/m3.
# pm10:         PM10 hourly value / daily value in ug/m3.
# pm10_hourly:  Indincates whether pm10 is an hourly average (TRUE) or a 24-hour average (FALSE). Default is TRUE.
# pm2.5:        PM2.5 hourly value / daily value in ug/m3.
# pm2.5_hourly: Indincates whether pm2.5 is an hourly average (TRUE) or a 24-hour average (FALSE). Default is TRUE.
# co:           CO 8 hours moving average / maximum 8 hours moving average in ug/m3.
# print_si:     Prints all the sub-indices. Default is FALSE.

caqi_t <- function(no2, pm10, pm10_hourly=TRUE, pm2.5=NULL, pm2.5_hourly=T, co=NULL, print_si=F){
  
  result <- c()
  # Checking whether the inputted arrays are of equal length
  len <- c(length(no2), length(pm10))
  if(!is.null(pm2.5)){
    len <- append(len, length(pm2.5))
  } 
  if(!is.null(co)){
    len <- append(len, length(co))
  }
  if(min(len) != max(len)){
    stop("Length of arrays are not equal.", call. = TRUE)
  }
  
  for(i in 1:length(no2)){
    
    # Calculating the NO2 sub-index (si_no2)
    no2i <- no2[i]
    if(!is.na(no2i)){
      if(no2i < 100){
        si_no2 <- no2i/2
      } else if (no2i > 200){
        si_no2 <- (no2i/8)+50
      } else {
        si_no2 <- (no2i/4) + 25
      }
    } else {
      result <- append(result, NA)
      next
    }
    
    # Calculating the PM10 sub-index (si_pm10).
    # Different critera exist for PM10 at hourly and 24-h averages
    pm10i <- pm10[i]
    if(!is.na(pm10i)){
      if(pm10_hourly == TRUE){
        # hourly average
        if(pm10i < 50){
          si_pm10 <- pm10i
        } else if(pm10i > 90){
          si_pm10 <- ((5*pm10i)/18) + 50
        } else {
          si_pm10 <- ((5*pm10i)/8) + 18.75
        }
      } else {
        # 24-h average
        if(pm10i < 50){
          si_pm10 <- (5*pm10i)/3
        } else if(pm10i > 90){
          si_pm10 <- (pm10i/2) + 50
        } else {
          si_pm10 <- ((5*pm10i)/4) + 12.5
        }
      }
    } else {
      result <- append(result, NA)
      next
    }
    
    # Calculating the PM2.5 sub-index (si_pm2.5) if available.
    # Different critera exist for PM2.5 at hourly and 24-h averages
    pm2.5i <- pm2.5[i]
    if((is.null(pm2.5i))||(is.na(pm2.5i))){
      si_pm2.5 <- NULL
    } else {
      if(pm2.5_hourly == TRUE){
        if(pm2.5i < 30){
          si_pm2.5 <- (5*pm2.5i)/3
        } else if(pm2.5i > 55){
          si_pm2.5 <- ((5*pm2.5i)/11) + 50
        } else {
          si_pm2.5 <- pm2.5i + 20
        }
      } else {
        if(pm2.5i < 30){
          si_pm2.5 <- (5*pm2.5i)/2
        } else {
          si_pm2.5 <- ((5*pm2.5i)/6) + 50
        }
      }
    }
    
    # Calculating the CO sub-index (si_co)
    # NOTE: CO must be inputted as an 8-hour moving average
    coi <- co[i]
    if((is.null(coi))||(is.na(coi))){
      si_co <- NULL
    } else {
      if(coi < 5000){
        si_co <- coi/200
      } else if (coi > 10000){
        si_co <- (coi/400) + 50
      } else {
        si_co <- (coi/100) - 25
      } 
    }
    
    if(print_si){
      print('Sub-indices')
      print('***********')
      print(paste0('NO2:   ', si_no2, collapse=NULL))
      print(paste0('PM10:  ', si_pm10, collapse=NULL))
      print(paste0('PM2.5: ', si_pm2.5, collapse=NULL))
      print(paste0('CO:    ', si_co, collapse=NULL))
    }
    
    result <- append(result, (max(si_no2, si_pm10, si_pm2.5, si_co)))
  }
  return(result)
}


# The following function returns the common air quality index (caqi) for city background. First the sub-index (si) of 
# each pollutant is calculated, then the maximum si is retured. NO2, PM10 and O3 are mandatory while PM2.5, CO and SO2
# are optional.

# Function inputs
# no2, o3, so2: NO2/O3/SO2 hourly value / maximum hourly value in ug/m3.
# pm10:         PM10 hourly value / daily value in ug/m3.
# pm10_hourly:  Indincates whether pm10 is an hourly average (TRUE) or a 24-hour average (FALSE). Default is TRUE.
# pm2.5:        PM2.5 hourly value / daily value in ug/m3.
# pm2.5_hourly: Indincates whether pm2.5 is an hourly average (TRUE) or a 24-hour average (FALSE). Default is TRUE.
# co:           CO 8 hours moving average / maximum 8 hours moving average in ug/m3.
# print_si:     Prints all the sub-indices. Default is FALSE.

caqi_b <- function(no2, pm10, pm10_hourly=TRUE, o3, pm2.5=NULL, pm2.5_hourly=T, co=NULL, so2=NULL, print_si=F){
  
  result <- c()
  # Checking whether the inputted arrays are of equal length
  len <- c(length(no2), length(pm10), length(o3))
  if(!is.null(pm2.5)){
    len <- append(len, length(pm2.5))
  } 
  if(!is.null(co)){
    len <- append(len, length(co))
  }
  if(!is.null(so2)){
    len <- append(len, length(so2))
  }
  if(min(len) != max(len)){
    stop("Length of arrays are not equal.", call. = TRUE)
  }
  
  for(i in 1:length(no2)){
    
    # Calculating the NO2 sub-index (si_no2)
    no2i <- no2[i]
    if(!is.na(no2i)){
      if(no2i < 100){
        si_no2 <- no2i/2
      } else if (no2i > 200){
        si_no2 <- (no2i/8)+50
      } else {
        si_no2 <- (no2i/4) + 25
      }
    } else {
      result <- append(result, NA)
      next
    }

    # Calculating the PM10 sub-index (si_pm10).
    # Different critera exist for PM10 at hourly and 24-h averages
    pm10i <- pm10[i]
    if(!is.na(pm10i)){
      if(pm10_hourly == TRUE){
        # hourly average
        if(pm10i < 50){
          si_pm10 <- pm10i
        } else if(pm10i > 90){
          si_pm10 <- ((5*pm10i)/18) + 50
        } else {
          si_pm10 <- ((5*pm10i)/8) + 18.75
        }
      } else {
        # 24-h average
        if(pm10i < 50){
          si_pm10 <- (5*pm10i)/3
        } else if(pm10i > 90){
          si_pm10 <- (pm10i/2) + 50
        } else {
          si_pm10 <- ((5*pm10i)/4) + 12.5
        }
      }
    } else {
      result <- append(result, NA)
      next
    }
    
    # Calculating the O3 sub-index (si_o3)
    o3i <- o3[i]
    if(!is.na(o3i)){
      si_o3 <- (5*o3i)/12
    } else {
      result <- append(result, NA)
      next
    }
    
    # Calculating the PM2.5 sub-index (si_pm2.5) if available.
    # Different critera exist for PM2.5 at hourly and 24-h averages
    pm2.5i <- pm2.5[i]
    if((is.null(pm2.5i))||(is.na(pm2.5i))){
      si_pm2.5 <- NULL
    } else {
      if(pm2.5_hourly == TRUE){
        if(pm2.5i < 30){
          si_pm2.5 <- (5*pm2.5i)/3
        } else if(pm2.5i > 55){
          si_pm2.5 <- ((5*pm2.5i)/11) + 50
        } else {
          si_pm2.5 <- pm2.5i + 20
        }
      } else {
        if(pm2.5i < 30){
          si_pm2.5 <- (5*pm2.5i)/2
        } else {
          si_pm2.5 <- ((5*pm2.5i)/6) + 50
        }
      }
    }
    
    # Calculating the CO sub-index (si_co)
    # NOTE: CO must be inputted as an 8-hour moving average
    coi <- co[i]
    if((is.null(coi))||(is.na(coi))){
      si_co <- NULL
    } else {
      if(coi < 5000){
        si_co <- coi/200
      } else if (coi > 10000){
        si_co <- (coi/400) + 50
      } else {
        si_co <- (coi/100) - 25
      } 
    }
    
    # Calculating the SO2 sub-index (si_so2)
    so2i <- so2[i]
    if((is.null(so2i))||(is.na(so2i))){
      si_so2 <- NULL
    } else {
      if(so2i < 100){
        si_so2 <- so2i/2
      } else if (so2i > 350){
        si_so2 <- (so2i/6) + (50/3)
      } else {
        si_so2 <- (so2i/10) + 40
      } 
    }
    
    if(print_si){
      print('Sub-indices')
      print('***********')
      print(paste0('NO2:   ', si_no2, collapse=NULL))
      print(paste0('PM10:  ', si_pm10, collapse=NULL))
      print(paste0('O3:    ', si_o3, collapse=NULL))
      print(paste0('PM2.5: ', si_pm2.5, collapse=NULL))
      print(paste0('CO:    ', si_co, collapse=NULL))
      print(paste0('SO2:   ', si_so2, collapse=NULL))
    }
    
    result <- append(result, (max(si_no2, si_pm10, si_o3, si_pm2.5, si_co, si_so2)))
  }
  return(result)
}