# function which finds longest route and returns information on it
# input:
#  - vesselInfoDT   - data.table   - data table with numeric columns LON, LAT,
#                                    IS_PARKED and POSIXct column DATE_TIME
#
# output:
#  list which contains:
#   - routeHistory   - data.table  - data table with numerical columns
#                                    LON, LAT, and POSIXct column DATE_TIME
#   - routeDistance  - numeric     - longest route distance sailed
#   - routeDuration  - numeric     - longest route duration
#   - routeStartTime - POSIXct     - longest route start time
#   - routeEndTime   - POSIXct     - longest route end time
fCalculateLongestRoute <- function(vesselInfoDT) {
  if (nrow(vesselInfoDT) <= 0) return()
  
  # declare initial variables
  isParked <- logical()
  routesHistoryList <- list()
  currentRoute <- data.table("LAT" = numeric(),
                             "LON" = numeric(),
                             "DATE_TIME" = as.POSIXct(NULL))
  previousCoordinates <- NULL
  
  # order records by time
  vesselInfoDT <- vesselInfoDT[order(DATE_TIME)]
  
  # save each distinct route into routesHistoryList
  apply(vesselInfoDT, 1, function(routeInfo) {
    isParked <<- routeInfo[["IS_PARKED"]] %>% as.numeric()
    if(isParked && nrow(currentRoute) > 0) {
      # save the observation when vessel has just parked
      if(!as.numeric(previousCoordinates[["IS_PARKED"]])) {
        currentRoute <<- rbind(currentRoute,
                               data.table("LAT" = as.numeric(routeInfo[["LAT"]]),
                                          "LON" = as.numeric(routeInfo[["LON"]]),
                                          "DATE_TIME" = as.POSIXct(routeInfo[["DATE_TIME"]],
                                                                   format = "%Y-%m-%d %H:%M:%S")))
      }
      routesHistoryList <<- c(routesHistoryList, list(currentRoute))
      currentRoute <<- data.table("LAT" = numeric(),
                                  "LON" = numeric(),
                                  "DATE_TIME" = as.POSIXct(NULL))
    } else if (!isParked) {
      if(!is.null(previousCoordinates) && as.numeric(previousCoordinates[["IS_PARKED"]])) {
        # save the observation when vessel was parked but then went to a sail
        currentRoute <<- rbind(currentRoute,
                               data.table("LAT" = as.numeric(previousCoordinates[["LAT"]]),
                                          "LON" = as.numeric(previousCoordinates[["LON"]]),
                                          "DATE_TIME" = as.POSIXct(previousCoordinates[["DATE_TIME"]],
                                                                   format = "%Y-%m-%d %H:%M:%S")))
      }
      # save the observation when vessel is on a sail
      currentRoute <<- rbind(currentRoute,
                             data.table("LAT" = as.numeric(routeInfo[["LAT"]]),
                                        "LON" = as.numeric(routeInfo[["LON"]]),
                                        "DATE_TIME" = as.POSIXct(routeInfo[["DATE_TIME"]],
                                                                 format = "%Y-%m-%d %H:%M:%S")))
    }
    previousCoordinates <<- routeInfo
  })
  
  # remove routes with duration less than 30 seconds
  routesHistoryList <- lapply(routesHistoryList, function(routeDT) {
    routeDuration <- fCalculateRouteDuration(routeDT, "secs")
    if (routeDuration >= 30) {
      list(routeHistory = routeDT,
           routeDistance = fCalculateRouteDistance(routeDT[, c("LAT", "LON")]),
           routeDuration = round(
             fCalculateRouteDuration(routeDT[, c("DATE_TIME")], "secs"),
             2),
           routeStartTime = first(routeDT)$DATE_TIME,
           routeEndTime = last(routeDT)$DATE_TIME)
    }
  })
  
  # remove NULL values from routes list
  routesHistoryList <- routesHistoryList[!sapply(routesHistoryList,is.null)]
  
  answerRoute <- if (length(routesHistoryList) > 0) {
    routesDistanceVector <- sapply(routesHistoryList, function(routeInfo) {
      routeInfo[["routeDistance"]]
    })
    
    maxDistanceInd <- routesDistanceVector %in% max(routesDistanceVector) %>% which()
    # in case of several equal by distance routes accured, the least by duration
    # is to be choosed
    maxDistanceInd <- if (length(maxDistanceInd) > 1) {
      routesDurationVector <- sapply(routesHistoryList, function(routeInfo) {
        routeInfo[["routeDuration"]]
      })
      
      minDurationInd <- routesDurationVector %in% min(routesDurationVector) %>% which()
      # in case of several equal by duration routes accured, the least most recent
      # is to be choosen
      if (length(minDurationInd) > 0) {
        routesStartTimeList <- sapply(routesHistoryList, function(routeInfo) {
          routeInfo["routeStartTime"]
        })
        
        which.min(routesStartTimeList) %>% minDurationInd[.]
      } else {
        intersect(maxDistanceInd, minDurationInd)
      }
    } else {
      maxDistanceInd
    }
    
    routesHistoryList[[maxDistanceInd]]
  } else {
    list(routeHistory = NULL,
         routeDistance = -1,
         routeDuration = NULL,
         routeStartTime = NULL,
         routeEndTime = NULL)
  }
  
  return(answerRoute)
}

# function which calculates a route duration
# input:
#  - routeHistoryDT   - data.table   - data table with POSIXct columns DATE_TIME
#  - units            - character    - units values from ("auto", "secs", "mins",
#                                      "hours", "days", "weeks") 
# output:
#  duration of a route
fCalculateRouteDuration <- function(routeHistoryDT, units) {
  if (nrow(routeHistoryDT) > 0) {
    startTime <- first(routeHistoryDT)$DATE_TIME
    endTime <- last(routeHistoryDT)$DATE_TIME
    return(as.numeric(difftime(endTime, startTime), units= units))
  } else {
    return(0)
  }
}

# function which calculates the distance sailed in given route
# input:
#  - routeHistoryDT   - data.table   - data table with numeric columns
#                                      LON, LAT
#
# output:
#  distance which vessel sailed during given route
fCalculateRouteDistance <- function(routeHistoryDT) {
  distance <- 0
  previousCoordinates <- NULL
  apply(routeHistoryDT, 1, function(coordinates) {
    if (!is.null(previousCoordinates)) {
      distance <<- distance + fCalculateDistanceBetweenCoordinates(coordinates,
                                                                   previousCoordinates)
    }
    previousCoordinates <<- coordinates
  })
  return(distance)
}

# function which calculates the distance between two point with Haversine formula.
# input:
#  - previousCoordinates   - numeric    - numeric vector with two values
#                                         (latitude and longitude)
#  - currentCoordinates    - numeric    - -//-
#
# output:
#  distance between input coordinates
fCalculateDistanceBetweenCoordinates <- function(previousCoordinates, currentCoordinates) {
  return(distm(previousCoordinates, currentCoordinates, fun = distHaversine) %>% as.numeric())
}