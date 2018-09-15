######################################################################
# Connect to AT API and retrieve bus locations
#
# Author: Stefan Schliebs
# Created: 2018-09-09 21:18:55 
######################################################################

library(httr)
library(jsonlite)
library(jqr)
library(shiny)
library(dplyr)


# Global constants --------------------------------------------------------

AT_VEHICLE_LOCATIONS <- "https://api.at.govt.nz/v2/public/realtime/vehiclelocations"
AT_BUS_ROUTES <- "https://api.at.govt.nz/v2/gtfs/routes"
AT_API_ENV_VAR <- "OCP_APIM_SUBSCRIPTION_KEY"


# API key -----------------------------------------------------------------

# Return API key stored in environment variable
get_api_key <- function() {
  Sys.getenv(AT_API_ENV_VAR)
}


# Bus locations -----------------------------------------------------------

unpack_jq <- function(s) {
  s %>% 
    as.character() %>% 
    paste(collapse = ",") %>% 
    paste("[", ., "]") %>% 
    fromJSON()
}

# Return dataframe of current snapshot of all bus positions
get_bus_locations <- function(key) {
  # fetch data from API
  response <- GET(AT_VEHICLE_LOCATIONS, add_headers(`Ocp-Apim-Subscription-Key` = key)) %>% 
    content(type = "text")
  
  # parse response into data.frame
  bind_cols(
    # unpack locations
    response %>% 
      jq(".response.entity[].vehicle.position") %>% 
      unpack_jq(),
    
    # unpack vehicle IDs
    response %>% 
      jq(".response.entity[].vehicle.vehicle") %>% 
      unpack_jq() %>% 
      rename(bus_id = id),
    
    # unpack trips
    response %>% 
      jq(".response.entity[].vehicle.trip") %>% 
      unpack_jq(),
    
    # unpack timestamp
    data_frame(
      timestamp = response %>% 
        jq(".response.entity[].vehicle.timestamp") %>% 
        unpack_jq() %>% 
        as.POSIXct(origin = "1970-01-01")
    )
      
  ) %>% tbl_df()
}


# Return data frame of all bus routes
get_bus_routes <- function(key) {
  # fetch data from API
  response <- GET(AT_BUS_ROUTES, add_headers(`Ocp-Apim-Subscription-Key` = key)) %>% 
    content(type = "text") 
  
  # parse response into data.frame
  response %>% 
    jq(".response[]") %>% 
    unpack_jq() %>% 
    tbl_df()
}
