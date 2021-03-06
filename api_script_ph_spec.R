token = "a6d62d995033130555b6721833abc6d7f0fe78716c0db714"

get_device <- function(device = NULL, brand = NULL, token){
  
  if(length(nchar(token)) == 0){
    stop("Insert a valid token from https://fonoapi.freshpixl.com/")
  }
  
  # Parameters
  library(httr)
  library(jsonlite)
  library(dplyr)
  url = "https://fonoapi.freshpixl.com/v1/getdevice"

  # Clean-UP
  device <- tolower(device)
  brand <- tolower(brand)
  
  # Conditional clause if brand or device (or both are chosen)
  if(length(nchar(brand)) == 0 & length(nchar(device)) != 0){
    
    param <- list(token = token,
                  brand = "",
                  device = device)
    
  } else if(length(nchar(brand)) != 0 & length(nchar(device)) == 0){
    
    param <- list(token = token,
                  brand = brand,
                  device = " ")
    
  } else if(length(nchar(brand)) != 0 & length(nchar(device)) != 0){
    
    param <- list(token = token,
                  brand = brand,
                  device = device)
  } else{
    
    stop("=== Insert a device/brand and the correct API Key from https://fonoapi.freshpixl.com/ ===")
    
  }
  
  # Transform the parameters into JSON format
  param <- toJSON(param)
  param <- gsub(pattern = '\\[', replacement = "", x = param)
  param <- gsub(pattern = '\\]', replacement = "", x = param)
  
  # API Call
  
  # fileName <- fromJSON(prettify(POST(url = url, add_headers(.headers = c("Content-Type"="application/json")), body = param)))
  fileName <- POST(url = url, add_headers(.headers = c("Content-Type"="application/json")), body = param)
  
  # Warning + Error Messages
  temp <- content(fileName)
  if(length(nchar(temp$message)) > 0){
    stop(temp$message)
  }
  
  # Data Structure
  fileName <- suppressMessages(fromJSON(prettify(fileName)))
  
  devices_list <<- fileName %>% 
    separate(internal, into = c("internal_memory", "ram_memory", "ram_memory2"), sep = ",", extra = "drop",  fill = "right") %>% 
    mutate(ram_memory = gsub("RAM","",coalesce(ram_memory2, ram_memory))) %>% 
    select(-ram_memory2) %>% 
    mutate(ram_memory = gsub("RAM DDR3","", ram_memory)) %>% 
    mutate(ram_memory = gsub("DDR3","", ram_memory)) 
    
}




