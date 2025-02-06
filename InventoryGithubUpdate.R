# Part 0: Libraries

# steps to do: 
# set up automated email
# add Email variables to github
# add Email vairables to yaml file
# add sys.getenv() for email variables int R script 
# get email to work 


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
library(openssl)
library(blastula) #need to add to yaml in github actions

# Load environment variabels
print('Load environment variabels')

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

print('Lightspeed Authenitcation Flow.')

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
print('update access token in LSPDAUTH')

LSPDAUTH$AccessToken<- response_content$access_token
LSPDAUTH$LSPDRefreshToken <- response_content$refresh_token

LSPDAUTH$ExpirationTime <- as.numeric(Sys.time())+response_content$expires_in


# Part 2: getting product data From Brands. 
print('Part 2: getting product data From Brands')

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
print('Ezzy info')

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
  select(systemSku, manufacturerSku) %>% 
  filter(manufacturerSku != '') %>% 
  distinct(manufacturerSku, .keep_all = T) #checks for duplicates



# get and wrangle Ezzy Warehouse data

WarehouseEzzy <- read.csv('https://docs.google.com/spreadsheets/d/14RRJs7QEvblDK2OJL0vKOmh-tdGXrwAo/export?format=csv')

#remove all items that are not in LSPD

EzzyItemsDF$isinwarehouse <- EzzyItemsDF$manufacturerSku %in% WarehouseEzzy$SKU

# data wrangeling
EzzyItemsDF <- EzzyItemsDF %>% 
  filter(isinwarehouse==T) %>% # remove all rows that arent in LSPD
  select(manufacturerSku, systemSku) %>%  # remove all cols that arent important 
  left_join(WarehouseEzzy[,c('Inventory','SKU')], by = c('manufacturerSku'='SKU')) %>% #merge ezzy warehouse data with LSPD data
  mutate(Inventory = .[[3]]) %>% #naming convensions
  filter(!is.na(Inventory)) %>% #remove NA values
  select(systemSku, Inventory) %>% # Formatting
  mutate(systemSku = as.character(systemSku),  # change data types
         Inventory = as.character(Inventory)) #change data types




## Fone
# get FOne data from LSPD while loop

# Initialize url with the first value
print('F-one info')

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
  select(systemSku, ean) %>% 
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
  select(ean, systemSku) %>%  # keep relevant columns 
  left_join(WarehouseFone, by = c('ean' = 'EAN')) %>% #merge warehouse data into LSPD data
  mutate(Inventory = .[[3]]) %>% # change names
  filter(!is.na(Inventory)) %>% # remove all NA inventpry values
  select(systemSku, Inventory) %>% # keep only relevant items
  mutate(systemSku = as.character(systemSku), #change data type
         Inventory = as.character(Inventory)) #change data type

# end of Fone Data
#########################################
# check token time left

if(LSPDAUTH$ExpirationTime - as.numeric(Sys.time()) <= 30){
  
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
  
  
}else(print(paste0('token has ',round(LSPDAUTH$ExpirationTime - as.numeric(Sys.time()),2), ' seconds left' )))


# end of check token time left
# end of cheeck bucket level
########################################

# Start of North and Mystic Data
# North Mystic from brand products

# download data from link



# download LSPD data from API. vendor = 275 and 235. 

WarehouseNorth <- read.csv('http://b2busa.northasg.com/export_stock_csv')


WarehouseNorth$ean <-  format(WarehouseNorth$ean, scientific = FALSE)

WarehouseNorth <- WarehouseNorth %>% 
  select(stock, ean)

# get info from LSPD 

# Initialize url with the first value
print('North and Mystic info')


# the IN filter allows to search for multiple vendors. should consolidate all of the vendor calls. 

url <- 'https://api.lightspeedapp.com/API/V3/Account/295409/Item.json?defaultVendorID=IN,[275,235]'


NorthItemsDF <- list()
url <- 'https://api.lightspeedapp.com/API/V3/Account/295409/Item.json?defaultVendorID=IN%2C%5B275%2C235%5D&sort=itemID&limit=100&after=WzMzNjU2XQ%3D%3D'

# Loop until url is empty
while (url != "") {
  # Make the request to the API
  AccountResponse <- request(url) %>% #endpoint
    req_headers(Authorization = paste0('Bearer ', response_content$access_token)) %>% #access info
    req_perform() %>% #perform the action 
    resp_body_json() #format the response
  
  
  NorthItemsDF <- append(NorthItemsDF, AccountResponse$Item)
  
  
  # Update url for the next iteration
  url <- AccountResponse[["@attributes"]][["next"]]
  Sys.sleep(3)
}

 
# big pipe to merge aadn format all North and mytic data

print('wrangle north and mystic data')

NorthItemsDF <- bind_rows(lapply(NorthItemsDF, as_tibble)) %>%
  select(systemSku, ean) %>%
  filter(!is.na(ean) & ean != "") %>%
  distinct(ean, .keep_all = TRUE) %>%
  mutate(ean = as.character(ean)) %>%
  left_join(WarehouseNorth %>%
              as_tibble() %>%
              mutate(ean = as.character(ean)), by = "ean") %>%
  filter(!is.na(stock)) %>%
  rename(Inventory = stock) %>%
  select(systemSku, Inventory) %>%
  mutate(across(everything(), as.character))  # Ensure all columns are character type



# end of North mystic Data things
#########################################

# start of slignshot and ride engine data

print('get slingshot and ride engine warehouse data')
#Eaiser way, no login requred

# Define a proper destination file (e.g., "myfolder.zip")
dest_file <- "slingshotfolder.zip"

# Download the Dropbox folder as a ZIP file
download.file(
  'https://www.dropbox.com/scl/fo/e2ggweadlxqyh106289cf/h?rlkey=q6o0odcceflql15f9olor55lx&st=6774mc3l&dl=1',
  destfile = dest_file,
  mode = "wb"
)
print('downloaded sling data')
unzip(dest_file, exdir = "slingshotextractedfolder")
print('unzipped sling data')
slingfilename <- (list.files(path = 'slingshotextractedfolder', full.names = T))
print('got sling file name')

slingshot <- read_excel(slingfilename)
print('read sling into r')
# this gets all the slingshot data
print('sling done')
# now get all ride engine data

# Define a proper destination file (e.g., "myfolder.zip")
dest_file <- "rideenginefolder.zip"

# Download the Dropbox folder as a ZIP file
download.file(
  'https://www.dropbox.com/scl/fo/04q39p4snxdue0n/h?rlkey=34f7wl3x85hyfgphjmv4tgl7x&st=umu5ko2z&dl=1',
  destfile = dest_file,
  mode = "wb"
)

unzip(dest_file, exdir = "rideengineextractedfolder")

slingfilename <- (list.files(path = 'rideengineextractedfolder', full.names = T))

rideEngine <- read_excel('rideEngine.xlsx')

# filter and wrangle slingshto data

slingshot <- slingshot %>% 
  select(`UPC Code`, `Location Available`) %>% 
  mutate( upc = `UPC Code`,
          Inventory = as.numeric(ifelse(`Location Available` == 'In Stock', 25, `Location Available`))) %>% 
  select(upc, Inventory) 

# filter and wrangle data ride engine and combine both

Warehouse7nation <- rideEngine %>% 
  select(`UPC Code`,`Location Available`) %>% 
  
  mutate(upc = `UPC Code`,
         Inventory = as.numeric(ifelse(`Location Available`== 'In Stock', 25, `Location Available`))) %>% 
  select(upc, Inventory) %>% 
  bind_rows(slingshot)

###############################

# download all ride engine and slingshot lightspeed product information 
#  232 = slingshot 
# 237 = ride engine
print('get slingshot oride engine lightspeed data')

url <- 'https://api.lightspeedapp.com/API/V3/Account/295409/Item.json?defaultVendorID=IN,[232,237]'

SevenNationItemsDF <- list()

# Loop until url is empty
while (url != "") {
  # Make the request to the API
  AccountResponse <- request(url) %>% #endpoint
    req_headers(Authorization = paste0('Bearer ', response_content$access_token)) %>% #access info
    req_perform() %>% #perform the action 
    resp_body_json() #format the response
  
  
  SevenNationItemsDF <- append(SevenNationItemsDF, AccountResponse$Item)
  
  
  # Update url for the next iteration
  url <- AccountResponse[["@attributes"]][["next"]]
  Sys.sleep(3)
}



###### 

SevenNationItemsDF2 <- bind_rows(lapply(SevenNationItemsDF, as_tibble)) %>%
  select(systemSku, upc) %>%
  filter(!is.na(upc) & upc != "") %>%
  distinct(upc, .keep_all = TRUE) %>%
  mutate(upc = as.character(upc)) %>%
  left_join(Warehouse7nation %>%
              as_tibble() %>%
              mutate(upc = as.character(upc)), by = "upc") %>%
  filter(!is.na(Inventory)) %>%
  select(systemSku, Inventory) %>%
  mutate(across(everything(), as.character))  # Ensure all columns are character type



# end of slingshot ride engine data things
##########################################

# Merge all datasets to upload
print('combine into one big dataset')

# right now this includes Fone and Ezzy invntory

AllWarehouseinventory <- bind_rows(EzzyItemsDF, FoneItemsDF, NorthItemsDF, SevenNationItemsDF2)

write_csv2(x = AllWarehouseinventory,
           file = 'Supplier_Inventory.csv',
           append = FALSE)



# Part 5: creating a new count, adding items to the count, reconciling the count. 
print('Part 5: creating a new count, adding items to the count, reconciling the count')
#  create new count

# this works 
url<- "https://api.lightspeedapp.com/API/V3/Account/295409/InventoryCount.json"

payload <- list(
  name = paste0(Sys.Date(), " Supplier Update"),
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

# this returns the counts with order created from most recent to least recent. ergo, the important one will always be number 1.

url <- 'https://api.lightspeedapp.com/API/V3/Account/295409/InventoryCount.json?shopID=2&sort=-inventoryCountID'
 
 AccountResponse <- request(url) %>% 
   req_headers(Authorization = paste0('Bearer ', LSPDAUTH$AccessToken)) %>% 
   req_perform()
 
 resp_body_json(AccountResponse)
 
 InventoryCountID <- AccountResponse[["cache"]][["json-5f34400b59"]][["InventoryCount"]][[1]][["inventoryCountID"]]
 
# Update github secrets with new tokens. 
###################################################################################

###################################################################################
# update values in github
###################################################################################

# start of Github Var updates

# Github information and variables. load these variables from the system environment

# keep this for github to run
GH <- list()
GH$REPO_OWNER <- Sys.getenv("GH_REPO_OWNER")
GH$REPO_NAME <- Sys.getenv("GH_REPO_NAME")
GH$PAT <- Sys.getenv("GH_PAT")


# Construct the URL dynamically
url <- paste0("https://api.github.com/repos/", 
              GH$REPO_OWNER, "/", 
              GH$REPO_NAME,
              "/actions/secrets/public-key")

# Make the API request to fetch the public key
response <- request(url) %>%
  req_auth_bearer_token(GH$PAT) %>%
  req_headers(Accept = "application/vnd.github+json") %>%
  req_perform()

# Parse the JSON response
public_key_data <- resp_body_json(response)

# Extract the public key and key ID
GH$PUBLIC_KEY <- public_key_data$key
GH$KEY_ID <- public_key_data$key_id

#####################################################################################
#got the keys now upload the LSPD tokens. 




# Encrypt the tokens values with Sodium

#######################################################

print('update LSPD tokens in github')
# First do the refresh token

# Step 1: Encrypt the secret value using LibSodium
# values can not be in lists or dfs. 

refresh_token <- charToRaw(LSPDAUTH$LSPDRefreshToken)
public_key <- base64_decode(GH$PUBLIC_KEY)

GH$LSPD_REFRESH_TOKEN <- simple_encrypt(refresh_token, public_key)

# Base64 encode the encrypted secret
GH$LSPD_REFRESH_TOKEN <- base64enc::base64encode(GH$LSPD_REFRESH_TOKEN)


###################################################

# Step 2: Prepare the payload
payload <- list(
  encrypted_value = GH$LSPD_REFRESH_TOKEN,
  key_id = GH$KEY_ID)

# Step 3: Make the API request to update the secret
secret_url <- paste0("https://api.github.com/repos/", GH$REPO_OWNER, "/", GH$REPO_NAME, "/actions/secrets/", 'LSPDAUTH_LSPDREFRESHTOKEN')

response_secret <- request(secret_url) %>%
  req_auth_bearer_token(GH$PAT) %>%
  req_method("PUT") %>%
  req_headers(
    Accept = "application/vnd.github+json") %>%
  req_body_json(payload) %>%
  req_perform()

# Step 5: Check the response
if (resp_status(response_secret) == 201) {
  print("Secret created successfully!")
} else if (resp_status(response_secret) == 204) {
  print("Secret updated successfully!")
} else {
  print("Failed to create/update secret: ", resp_body_string(response_secret))
}



# now do the Access Token. (this probably isnt important because we make a new one each time. )








