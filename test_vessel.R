library(testthat)
library(data.table)
library(magrittr)
library(utils)
library(geosphere)
library(bit64)
library(leaflet)
library(lubridate)
source("functions.R")


test_that("Coordinates distance calculation", {
  
  distance <- fCalculateDistanceBetweenCoordinates(c(1, 2), c(3, 4))
  
  expect_that(distance, equals(314635.3253))
  expect_that(distance, is_a("numeric"))
  expect_that(length(distance), equals(1) )
  expect_true(distance  >= 0)
  
  expect_error(fCalculateDistanceBetweenCoordinates(1, c(3, 4)) )
})

test_that("Route distance calculation", {
  routeHistoryDT <- data.table(
    LON = c(1, 2),
    LAT = c(3, 4))
  distance <- fCalculateRouteDistance(routeHistoryDT)

  expect_that(distance, equals(157281.772))
  expect_that(distance, is_a("numeric"))
  expect_that(length(distance), equals(1) )
  expect_true(distance  >= 0)
  
  routeHistoryDT <- data.table()
  distance <- fCalculateRouteDistance(routeHistoryDT)
  expect_that(distance, equals(0) )
})

test_that("Route duration calculation", {
  routeHistoryDT <- data.table(
    DATE_TIME = as.POSIXct(c("2016-12-16 06:49:06",
                             "2016-12-16 06:51:07",
                             "2016-12-16 06:53:06",
                             "2016-12-16 06:55:06",
                             "2016-12-16 06:57:06",
                             "2016-12-16 06:59:06"),
                           format = "%Y-%m-%d %H:%M:%S"))
  duration <- fCalculateRouteDuration(routeHistoryDT, "secs")
  
  expect_that(duration, equals(600))
  expect_that(duration, is_a("numeric"))
  expect_that(length(duration), equals(1) )
  expect_true(duration  >= 0)
  
  
  routeHistoryDT <- data.table(DATE_TIME = as.character())
  routeHistoryDT[, DATE_TIME := as.POSIXct(DATE_TIME, format = "%Y-%m-%d %H:%M:%S")]
  duration <- fCalculateRouteDuration(routeHistoryDT, "secs")
  expect_that(duration, equals(0) )
})

test_that("Longest route calculation", {
  vesselInfoDT <- data.table(
    LON = c(55.66211, 55.66176, 55.66176, 55.65998, 55.66071, 55.66052),
    LAT = c(55.66211, 55.66176, 55.66176, 55.65998, 55.66071, 55.66052),
    IS_PARKED = c(1, 0, 0, 0, 0, 1),
    DATE_TIME = as.POSIXct(c("2016-12-16 06:49:06",
                             "2016-12-16 06:51:07",
                             "2016-12-16 06:53:06",
                             "2016-12-16 06:55:06",
                             "2016-12-16 06:57:06",
                             "2016-12-16 06:59:06"),
                           format = "%Y-%m-%d %H:%M:%S"))
  longestDistanceInfo <- fCalculateLongestRoute(vesselInfoDT)
  
  expect_true(389 < longestDistanceInfo$routeDistance && longestDistanceInfo$routeDistance < 390)
  expect_that(longestDistanceInfo$routeDistance, is_a("numeric"))
  expect_that(length(longestDistanceInfo$routeDistance), equals(1))
  
  expect_that(longestDistanceInfo$routeDuration, equals(600))
  expect_that(longestDistanceInfo$routeDuration, is_a("numeric"))
  expect_that(length(longestDistanceInfo$routeDuration), equals(1))
  
  expect_true(nrow(longestDistanceInfo$routeHistory) > 0)
  
  expect_that(longestDistanceInfo$routeStartTime, is_a("POSIXct"))
  expect_that(longestDistanceInfo$routeEndTime, is_a("POSIXct"))
  expect_true(as.numeric(difftime(longestDistanceInfo$routeEndTime, longestDistanceInfo$routeStartTime), units= "secs") > 0)
  
  expect_that(length(longestDistanceInfo$routeDistance), equals(1))
})