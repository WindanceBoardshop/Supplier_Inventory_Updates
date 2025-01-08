# Part 0: Libraries

# LIbraries
library(tidyverse)
library(httr2)
library(curl)
library(data.table)
library(jsonlite)
library(readxl)
library(rio)
library(sodium)
library(httr)

# Load environment variabels

LSPDAUTH <- list()
LSPDAUTH$LSPDApiName <- Sys.getenv("LSPDAUTH_LSPDAPINAME")
LSPDAUTH$LSPDClientID <- Sys.getenv("LSPDAUTH_LSPDCLIENTID")
LSPDAUTH$LSPDClientSecretKey <- Sys.getenv("LSPDAUTH_LSPDCLIENTSECRETKEY")
LSPDAUTH$LSPDRefreshToken <- Sys.getenv("LSPDAUTH_LSPDREFRESHTOKEN")
LSPDAUTH$AccessToken <- Sys.getenv("LSPDAUTH_LSPDACCESSTOKEN")



# Part 1: Lightspeed Authenitcation Flow. 

# Unused refresh tokens expire after 30 days

# These Variables should be saved in a file that is hidden in github. the vars should be pulled from the file when needed and updated in the file everytime a new set of tokens is created. 

# Use Refresh token to get access token

# get new access token

token_url <- "https://cloud.lightspeedapp.com/auth/oauth/token"

body <- list(
  refresh_token = LSPDAUTH$LSPDRefreshToken,
  client_id = LSPDAUTH$LSPDClientID,
  client_secret = LSPDAUTH$LSPDClientSecretKey,
  grant_type = 'refresh_token'
)

response <- request(token_url) %>% 
  req_body_json(body) %>% 
  req_method('POST') %>% 
  req_perform()

response_content <- resp_body_json(response)

#update access token in LSPDAUTH

LSPDAUTH$AccessToken<- response_content$access_token
LSPDAUTH$LSPDRefreshToken <- response_content$refresh_token

LSPDAUTH$ExpirationTime <- as.numeric(Sys.time())+response_content$expires_in


# Part 2: getting product data From Brands. 

# baks is 274

# BRand info from LSPD important brand information

LSPDinfo <- list()

LSPDinfo$AccountID <- 294509


# got data
LSPDinfo$EzzyVendorID <- 142
# need link
LSPDinfo$NorthVendorID <- 275
# need link
LSPDinfo$MysticVendorID <- 235
# got data
LSPDinfo$FoneVendorID <- 2
# will need to heavily format the available data
LSPDinfo$ManeraVendorID <- 356
#got link, wait
LSPDinfo$Slingshot <- 232
# got link, wait
LSPDinfo$RideEnigineVendorID <- 237


LSPDinfo$OneillVendorID <- 9

LSPDinfo$ChinookVendorID <- 221

LSPDinfo$WarehouseID <- 2


## Ezzy
# Get ezzy info from LSDP

url <- 'https://api.lightspeedapp.com/API/V3/Account/295409/Item.json?defaultVendorID=142'


EzzyItemsDF <- list()


# Loop until url is empty
while (url != "") {
  # Make the request to the API
  AccountResponse <- request(url) %>% #endpoint
    req_headers(Authorization = paste0('Bearer ', response_content$access_token)) %>% #access info
    req_perform() %>% #perfom the action 
    resp_body_json() #format the response
  
  
  
  EzzyItemsDF <- append(EzzyItemsDF, AccountResponse$Item)
  
  
  # Update url for the next iteration
  url <- AccountResponse[["@attributes"]][["next"]]
}


# change list to df
EzzyItemsDF <- bind_rows(lapply(EzzyItemsDF, as_tibble))

#remove unnecessary  info

EzzyItemsDF <- EzzyItemsDF %>% 
  select(itemID, manufacturerSku) %>% 
  filter(manufacturerSku != '') %>% 
  distinct(manufacturerSku, .keep_all = T) #checks for duplicates



# get and wrangle Ezzy Warehouse data

WarehouseEzzy <- read.csv('https://docs.google.com/spreadsheets/d/14RRJs7QEvblDK2OJL0vKOmh-tdGXrwAo/export?format=csv')

#remove all items that are not in LSPD

EzzyItemsDF$isinwarehouse <- EzzyItemsDF$manufacturerSku %in% WarehouseEzzy$SKU

# data wrangeling
EzzyItemsDF <- EzzyItemsDF %>% 
  filter(isinwarehouse==T) %>% # remove all rows that arent in LSPD
  select(manufacturerSku, itemID) %>%  # remove all cols that arent important 
  left_join(WarehouseEzzy[,c('Inventory','SKU')], by = c('manufacturerSku'='SKU')) %>% #merge ezzy warehouse data with LSPD data
  mutate(Inventory = .[[3]]) %>% #naming convensions
  filter(!is.na(Inventory)) %>% #remove NA values
  select(itemID, Inventory) %>% # Formatting
  mutate(itemID = as.character(itemID),  # change data types
         Inventory = as.character(Inventory)) #change data types




## Fone
# get FOne data from LSPD while loop

# Initialize url with the first value


url <- 'https://api.lightspeedapp.com/API/V3/Account/295409/Item.json?defaultVendorID=274'

FoneItemsDF <- list()

# Loop until url is empty
while (url != "") {
  # Make the request to the API
  AccountResponse <- request(url) %>% #endpoint
    req_headers(Authorization = paste0('Bearer ', response_content$access_token)) %>% #access info
    req_perform() %>% #perfom the action 
    resp_body_json() #format the response
  
  
  FoneItemsDF <- append(FoneItemsDF, AccountResponse$Item)
  
  
  # Update url for the next iteration
  url <- AccountResponse[["@attributes"]][["next"]]
}

#turn data in tibble

FoneItemsDF <- bind_rows(lapply(FoneItemsDF, as_tibble))

#remove unnecessary  info

FoneItemsDF <- FoneItemsDF %>% 
  select(itemID, ean) %>% 
  filter(ean != '') %>% 
  distinct(ean, .keep_all = T) #checks for duplicates


# get and wrangle fone data from warehouse

# Define the URL for the first sheet in .xlsx format

url <- "https://docs.google.com/spreadsheets/d/1XweqBFPC5K8Lnh9dpg4WxCWohx0bTBJZ2vJIgeM0ViE/export?format=xlsx&id=1XweqBFPC5K8Lnh9dpg4WxCWohx0bTBJZ2vJIgeM0ViE"

# Define a temporary file to save the downloaded Excel file
temp_file <- tempfile(fileext = ".xlsx")

# Download the file from the Google Sheets URL
download.file(url, temp_file, mode = "wb")

# Import the first sheet from the downloaded Excel file
WarehouseFone <- import(temp_file)

WarehouseFone <- WarehouseFone %>% 
  select(EAN, STOCK...9) %>% 
  mutate(EAN = as.character(EAN))



# merge Fone datasets

#check what is in LSPD and what is not

FoneItemsDF$isinwarehouse <- FoneItemsDF$ean %in% WarehouseFone$EAN


# data wrangleing

FoneItemsDF <- FoneItemsDF %>% 
  filter(isinwarehouse==T) %>% #remove all products not in LSPD
  select(ean, itemID) %>%  # keep relevant columns 
  left_join(WarehouseFone, by = c('ean' = 'EAN')) %>% #merge warehouse data into LSPD data
  mutate(Inventory = .[[3]]) %>% # change names
  filter(!is.na(Inventory)) %>% # remove all NA inventpry values
  select(itemID, Inventory) %>% # keep only relevant items
  mutate(itemID = as.character(itemID), #change data type
         Inventory = as.character(Inventory)) #change data type



# Merge all datasets to upload


# right now this includes Fone and Ezzy invntory

AllWarehouseinventory <- bind_rows(EzzyItemsDF, FoneItemsDF)



# Part 5: creating a new count, adding items to the count, reconciling the count. 

#  create new count

# this works 
url<- "https://api.lightspeedapp.com/API/V3/Account/295409/InventoryCount.json"

payload <- list(
  name = paste0(Sys.Date(), " Supplier Update Test"),
  shopID = "2"
)


response <- request(url) %>%
  req_headers(
    Authorization = paste0("Bearer ", LSPDAUTH$AccessToken),
    `Content-Type` = "application/json"
  ) %>%
  req_body_json(payload) %>%
  req_method("POST") %>%
  req_perform() %>% 
  resp_body_json()



# get id for count

# first I need to get the position of the count because i dont know the ID. 

url <- 'https://api.lightspeedapp.com/API/V3/Account/295409/InventoryCount.json?shopID=2&count=1'


AccountResponse <- request(url) %>% 
  req_headers(Authorization = paste0('Bearer ', LSPDAUTH$AccessToken)) %>% 
  req_perform()

resp_body_json(AccountResponse)


CountPosition <- as.numeric(AccountResponse[["cache"]][["json-5f34400b59"]][["@attributes"]][["count"]])


# now i have the position of the count in terms of the response
# now i need to response

url <- 'https://api.lightspeedapp.com/API/V3/Account/295409/InventoryCount.json?shopID=2'


AccountResponse <- request(url) %>% 
  req_headers(Authorization = paste0('Bearer ', LSPDAUTH$AccessToken)) %>% 
  req_perform()

resp_body_json(AccountResponse)


#now i need to get the ID of the count from the position it is in the count. 


InventoryCountID <- AccountResponse[["cache"]][["json-5f34400b59"]][["InventoryCount"]][[CountPosition]][["inventoryCountID"]]



# Notes: I want to update this so that when there are more than 100 items it can paginate to the last item of the last page.  



# USe this, it will put all the stock info in lspd inventory count. it takes a while but thats necessary so that it doesnt over load the system. 

# adding items to inventory count useing Allwarehouseinventory

# Define the API URL
url <- "https://api.lightspeedapp.com/API/V3/Account/295409/InventoryCountItem.json"

# Define the JSON payload

# post each item individually (pain in the butt) but it works!!!!!!!!!!!!

for(i in 1:nrow(AllWarehouseinventory)) {
  # check to see how much time is left on the access token, if there is 5 seconds or less get a new access token. the system will sleep for a minute to make sure 
  
  if(LSPDAUTH$ExpirationTime - as.numeric(Sys.time()) <= 5){
    
    Sys.sleep(60)
    
    print('Updating Token') 
    #get new access token
    
    token_url <- "https://cloud.lightspeedapp.com/auth/oauth/token"
    
    body <- list(
      refresh_token = LSPDAUTH$LSPDRefreshToken,
      client_id = LSPDAUTH$LSPDClientID,
      client_secret = LSPDAUTH$LSPDClientSecretKey,
      grant_type = 'refresh_token'
    )
    
    response <- request(token_url) %>% 
      req_body_json(body) %>% 
      req_method('POST') %>% 
      req_perform()
    
    response_content <- resp_body_json(response)
    
    #update access token in LSPDAUTH
    
    LSPDAUTH$AccessToken<- response_content$access_token
    LSPDAUTH$LSPDRefreshToken <- response_content$refresh_token
    
    LSPDAUTH$ExpirationTime <- as.numeric(Sys.time())+response_content$expires_in
    
    
  }
  
  # define information about the first item in the data frame. format as list. 
  payload <- list(
    qty = AllWarehouseinventory$Inventory[i],
    inventoryCountID = InventoryCountID,
    itemID = AllWarehouseinventory$itemID[i],
    employeeID = '35'
  )
  
  # Make the POST request for that item. 
  response <- request(url) %>%
    req_headers(
      Authorization = paste0("Bearer ", LSPDAUTH$AccessToken),
      `Content-Type` = "application/json"
    ) %>%
    req_body_json(payload) %>%
    req_method("POST") %>%
    req_perform()
  # check to see if the access token is still working
  # rinse and repeat 
print(i)
  ##############
  
  # The header value
  bucket_level <- response[["headers"]][["x-ls-api-bucket-level"]]
  
  # Split the string at the "/" to separate remaining and total
  parts <- strsplit(bucket_level, "/")[[1]]
  
  # Convert both parts to numeric
  remaining <- as.numeric(parts[1])  # Remaining bucket level
  total <- as.numeric(parts[2])      # Total bucket capacity
  
  # Print results
  cat("Remaining:", remaining, "\nTotal:", total, "\n")
  
  if(remaining >= 79){
    Sys.sleep(60)
  }
  
}




# now I need to reconcile the inventory count. 
# reconcile inventory count

# that was easy. 

url <- "https://api.lightspeedapp.com/API/V3/Account/295409/InventoryCountReconcile.json"

payload <- list(
  inventoryCountID = InventoryCountID
)

response <- request(url) %>%
  req_headers(
    Authorization = paste0("Bearer ", LSPDAUTH$AccessToken),
    `Content-Type` = "application/json"
  ) %>%
  req_body_json(payload) %>%
  req_method("POST") %>%
  req_perform()




