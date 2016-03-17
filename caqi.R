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

caqi_t <- setClass(
  # Set the name for the class
  "caqi_t",
  
  # Define the slots
  slots = c(
    datetime = "POSIXct",
    no2 = "numeric",
    pm10 = "numeric",
    pm10_hourly = "logical",
    pm2.5 = "numeric",
    pm2.5_hourly = "logical",
    co = "numeric",
    result_no2 = "numeric",
    result_pm10 = "numeric",
    result_pm2.5 = "numeric",
    result_co = "numeric",
    result = "numeric"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    pm10_hourly = TRUE,
    pm2.5_hourly = TRUE
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object){
    # Checking whether the inputted arrays are of equal length
    len <- c(length(object@no2), length(object@pm10))
    if(length(object@pm2.5) != 0){
      len <- append(len, length(object@pm2.5))
    } 
    if(length(object@co) != 0){
      len <- append(len, length(object@co))
    }
    if(min(len) != max(len)){
      # stop("Inputted values are not of equal length.", call. = TRUE)
      return("Inputted values are not of equal length.")
    }
    return(TRUE)
  }
)


setGeneric(name="getCAQI", 
  def=function(obj){
    standardGeneric("getCAQI")
  }
)

setMethod(f="getCAQI",
  signature="caqi_t",
  definition=function(obj){
    for(i in 1:length(obj@no2)){
      
      # Calculating the NO2 sub-index (si_no2)
      no2i <- obj@no2[i]
      if(!is.na(no2i)){
        if(no2i < 100){
          si_no2 <- no2i/2
        } else if (no2i > 200){
          si_no2 <- (no2i/8)+50
        } else {
          si_no2 <- (no2i/4) + 25
        }
      } else {
        si_no2 <- NA
      }


      # Calculating the PM10 sub-index (si_pm10).
      # Different critera exist for PM10 at hourly and 24-h averages
      pm10i <- obj@pm10[i]
      if(!is.na(pm10i)){
        if(obj@pm10_hourly == TRUE){
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
        si_pm10 <- NA
      }

      
      # Calculating the PM2.5 sub-index (si_pm2.5) if available.
      # Different critera exist for PM2.5 at hourly and 24-h averages
      pm2.5i <- obj@pm2.5[i]
      if((is.null(pm2.5i))||(is.na(pm2.5i))){
        si_pm2.5 <- NULL
      } else {
        if(obj@pm2.5_hourly == TRUE){
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
      coi <- obj@co[i]
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

      obj@result_no2 <- append(obj@result_no2, si_no2)
      obj@result_pm10 <- append(obj@result_pm10, si_pm10)
      obj@result_pm2.5 <- append(obj@result_pm2.5, si_pm2.5)
      obj@result_co <- append(obj@result_co, si_co)
      
      obj@result <- append(obj@result, (max(si_no2, si_pm10, si_pm2.5, si_co)))
    }
    obj
  }
)

# create a method to count number of values in each bin.
setGeneric(name="caqi_counts",
  def=function(obj, all=F){
    standardGeneric("caqi_counts")
  }
)

setMethod(f="caqi_counts",
  signature="caqi_t",
  definition=function(obj, all=F){
    c_counts <- function(x){
      Green  = sum(x <= 25, na.rm=T)
      Yellow = sum(x > 25 & x <= 50, na.rm=T)
      Orange = sum(x > 50 & x <= 75, na.rm=T)
      Red    = sum(x > 75 & x <= 100, na.rm=T)
      Purple = sum(x > 100, na.rm=T)
      return( data.frame(Green, Yellow, Orange, Red, Purple))
    }
    if (!all){
      c_counts(obj@result)
    } else {
      results <- list(obj@result_no2, obj@result_pm10, obj@result_pm2.5, obj@result_co, obj@result)
      counts <- sapply(results, c_counts)
      res <- data.frame(counts)
      colnames(res) <- list("NO2", "PM10", "PM2.5", "CO", "CAQI")
      t(res)
    }
  }
)

setGeneric(name="caqi_plot",
  def=function(obj, name=NULL, reporting=T, width=8, height=4){
    standardGeneric("caqi_plot")
  }
)

setMethod(f="caqi_plot",
  signature="caqi_t",
  definition=function(obj, name=NULL, reporting=T, width=8, height=4){
    val <- obj@result
    if(reporting){val[val > 100] <- 110}
    if(!is.null(name)){pdf(paste0(name),width=width,height=height)}
    print(
      qplot(obj@datetime, obj@result) + 
        geom_rect(ymin = -25, ymax = 25, xmin = -Inf, xmax = Inf, fill = "#92c27d") +
        geom_rect(ymin = 25, ymax = 50, xmin = -Inf, xmax = Inf, fill = '#fdd766') +
        geom_rect(ymin = 50, ymax = 75, xmin = -Inf, xmax = Inf, fill = '#f4b16b') +
        geom_rect(ymin = 75, ymax = 100, xmin = -Inf, xmax = Inf, fill = '#de6666') +
        geom_rect(ymin = 100, ymax = 150, xmin = -Inf, xmax = Inf, fill = '#c07b9f') +
        ylim(5, 120) +
        geom_point(shape=20) +
        theme(legend.position="none") +
        ylab("CAQI") +
        xlab("Time")
    )
    if(!is.null(name)){dev.off()}
    }
  )

# create a method to produce a summary
setGeneric(name="summary",
  def=function(obj){
    standardGeneric("summary")
  }
)

setMethod(f="summary",
  signature="caqi_t",
  definition=function(obj){
    results <- list(obj@result_no2, obj@result_pm10, obj@result_pm2.5, obj@result_co, obj@result)
    means <- sapply(results, mean, na.rm=T)
    counts <- caqi_counts(obj, all=T)
    res <- data.frame(means, counts)
    row.names(res) <- list("NO2", "PM10", "PM2.5", "CO", "CAQI")
    res
  }
)


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

caqi_b <- function(no2, pm10, pm10_hourly=TRUE, o3, pm2.5=NULL, pm2.5_hourly=T, co=NULL, so2=NULL){
  
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
    
    result <- append(result, (max(si_no2, si_pm10, si_o3, si_pm2.5, si_co, si_so2)))
  }
  result
}