# I want to update this code so that there is only 1 lightspeed api call for products



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



# Part 0.1: Lightspeed Authenitcation Flow. 

# Unused refresh tokens expire after 30 days

# These Variables should be saved in a file that is hidden in github. 
# the vars should be pulled from the file when needed and updated in the file everytime a new set of tokens is created. 

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

################################################################################

# Part 1. get all product information from lightspeed
print('part 1.0 get lightspeed product data')

url <- 'https://api.lightspeedapp.com/API/V3/Account/295409/Item.json?'

allLSPD <- list()
page <- 0

# Loop until url is empty
while (url != "") {
  # Make the request to the API
  AccountResponse <- request(url) %>% #endpoint
    req_headers(Authorization = paste0('Bearer ', response_content$access_token)) %>% #access info
    req_perform() 
  
  
  # if the bucket get too full, this will empty it alot 
  print(AccountResponse[["headers"]][["x-ls-api-bucket-level"]])
  if(as.numeric(str_split_1(AccountResponse[["headers"]][["x-ls-api-bucket-level"]], '/'))[1] >= as.numeric(str_split_1(AccountResponse[["headers"]][["x-ls-api-bucket-level"]], '/'))[2]-10){
    Sys.sleep(20)} else {
      print('bucket is ok' )}
  
  AccountResponse <- AccountResponse %>% 
    resp_body_json()
  
  allLSPD <- append(allLSPD, AccountResponse$Item)
  
  
  # Update url for the next iteration
  url <- AccountResponse[["@attributes"]][["next"]]
  page <- page +1
  
  print(paste('pulling page ',page, 'from lightspeed. 100 items per page.'))
  
}  

# this makes the lightspeed data very tidy

allLSPD2 <- allLSPD %>% 
  tibble %>% 
  unnest_wider(1) %>% 
  unnest_wider(Prices) %>% 
  # unnest_longer(ItemPrice) %>% 
  unnest_wider(ItemPrice, names_sep = '') %>% 
  rename(Price = ItemPrice1,
         MSRP = ItemPrice2) %>% 
  unnest_wider(Price, names_sep = '_') %>% 
  unnest_wider(MSRP, names_sep = '_')
#pivot_wider(names_from = useType, values_from = amount)

# end get LSPD item information
################################################################################

# get vendor data from lightspeed

print(' Part 1.1 get vendor data from lightspeed')

url <- 'https://api.lightspeedapp.com/API/V3/Account/295409/Vendor.json?'


# Loop until url is empty
# Make the request to the API
AccountResponse <- request(url) %>% #endpoint
  req_headers(Authorization = paste0('Bearer ', response_content$access_token)) %>% #access info
  req_perform() %>% #perfom the action 
  resp_body_json() #format the response

vendorsLSPD <- AccountResponse[['Vendor']] %>% 
  tibble %>%
  unnest_wider(1)%>% 
  select(vendorID,
         name)

# end get LSPD information 

################################################################################
# create LSPD inventory count


# Part 1.1: creating a new count, adding items to the count, reconciling the count. 
print('Part 1.1: creating a new count, adding items to the count, reconciling the count')
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



# this returns the counts with order created from most recent to least recent. ergo, the important one will always be number 1.

url <- 'https://api.lightspeedapp.com/API/V3/Account/295409/InventoryCount.json?shopID=2&sort=-inventoryCountID'

AccountResponse <- request(url) %>% 
  req_headers(Authorization = paste0('Bearer ', LSPDAUTH$AccessToken)) %>% 
  req_perform()

resp_body_json(AccountResponse)

InventoryCountID <- AccountResponse[["cache"]][["json-5f34400b59"]][["InventoryCount"]][[1]][["inventoryCountID"]]

# Update github secrets with new tokens. 
##############################################################################

##############################################################################
# update values in github
##############################################################################

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

# end of github token update
# end of inventory count 
# end of lightspeed api pulls
################################################################################ 


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

################################################################################

# get ezzy warehouse data

# get and wrangle Ezzy Warehouse data

WarehouseEzzy <- read.csv('https://docs.google.com/spreadsheets/d/14RRJs7QEvblDK2OJL0vKOmh-tdGXrwAo/export?format=csv')

#remove all items that are not in LSPD

EzzyItemsDF <- allLSPD2 %>% 
  filter(defaultVendorID == 142)

EzzyItemsDF$isinwarehouse <- EzzyItemsDF$manufacturerSku %in% WarehouseEzzy$SKU

# export Ezzy items not in Lightspeed


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


# end Ezzy data
################################################################################
## Start Fone
# get FOne data from LSPD while loop

# Initialize url with the first value
print('F-one info')

#turn data in tibble

FoneItemsDF <- allLSPD2 %>% 
  filter(defaultVendorID == 274)

#remove unnecessary  info

FoneItemsDF <- FoneItemsDF %>% 
  select(systemSku, ean) %>% 
  filter(ean != '') %>% 
  distinct(ean, .keep_all = T) #checks for duplicates

################################################################################
# get and wrangle fone data from warehouse

# Define the URL for the first sheet in .xlsx format

url <- "https://docs.google.com/spreadsheets/d/1XweqBFPC5K8Lnh9dpg4WxCWohx0bTBJZ2vJIgeM0ViE/export?format=xlsx&id=1XweqBFPC5K8Lnh9dpg4WxCWohx0bTBJZ2vJIgeM0ViE"

# Define a temporary file to save the downloaded Excel file
temp_file <- tempfile(fileext = ".xlsx")

# Download the file from the Google Sheets URL
download.file(url, temp_file, mode = "wb")

# Import the first sheet from the downloaded Excel file\

WarehouseFoneNames <- excel_sheets(temp_file)
WarehouseFone <- list()

WarehouseFone$df0 <- read_excel(temp_file, sheet = 1, skip = 3)
WarehouseFone$df1 <- read_excel(temp_file, sheet = 2, skip = 3)
WarehouseFone$df2 <- read_excel(temp_file, sheet = 3, skip = 4)
WarehouseFone$df3 <- read_excel(temp_file, sheet = 4, skip = 3)
WarehouseFone$df4 <- read_excel(temp_file, sheet = 5, skip = 3)
WarehouseFone$df5 <- read_excel(temp_file, sheet = 6, skip = 3)
WarehouseFone$df6 <- read_excel(temp_file, sheet = 7, skip = 3)
WarehouseFone$df7 <- read_excel(temp_file, sheet = 8, skip = 2)
WarehouseFone$df8 <- read_excel(temp_file, sheet = 9, skip = 3)
WarehouseFone$df9 <- read_excel(temp_file, sheet = 10, skip = 2)
WarehouseFone$df10 <- read_excel(temp_file, sheet = 11, skip = 3)
WarehouseFone$df11 <- read_excel(temp_file, sheet = 12, skip = 3)
WarehouseFone$df12 <- read_excel(temp_file, sheet = 13, skip = 3)
WarehouseFone$df12 <- read_excel(temp_file, sheet = 14, skip = 3)


# now rename stock cols: 

# Find the first column that contains "stock"
# Function to rename the first matching "stock" column
rename_stock_column <- function(df) {
  stock_col_index <- which(str_detect(names(df), regex("stock", ignore_case = TRUE)))[1]
  
  if (!is.na(stock_col_index)) {
    names(df)[stock_col_index] <- "stock"
    
    df <- df %>%
      mutate(stock = as.character(stock))
  
  }
  
  return(df)
}

rename_ean_column <- function(df) {
  stock_col_index <- which(str_detect(names(df), regex("ean", ignore_case = TRUE)))[1]
  
  if (!is.na(stock_col_index)) {
    names(df)[stock_col_index] <- "ean"
    
    # Convert the column to character type
    df <- df %>%
      mutate(ean = as.character(ean))
  }
  
  return(df)
}

# now apply both functions to all dfs in the Fone warehouse

WarehouseFone <- lapply(WarehouseFone, rename_stock_column)

WarehouseFone <- lapply(WarehouseFone, rename_ean_column)

# now exclude dfs that dont have stock and ean cols. 

WarehouseFone <- Filter(function(df) "stock" %in% names(df), WarehouseFone)

WarehouseFone <- Filter(function(df) "ean" %in% names(df), WarehouseFone)




WarehouseAllFone <- bind_rows(lapply(WarehouseFone, function(df) {
  df %>% select(ean, stock)  # Select only EAN and stock columns
})) 

# now remove all rows that dont have a number in them

WarehouseAllFone <- WarehouseAllFone %>% 
  filter(str_detect(ean, "[0-9]")) %>% 
  filter(str_detect(stock, "[0-9]"))
  

################################################################################



# merge Fone datasets

#check what is in LSPD and what is not

FoneItemsDF$isinwarehouse <- FoneItemsDF$ean %in% WarehouseAllFone$ean

# export F-One items not in LSPD

# data wrangleing

FoneItemsDF <- FoneItemsDF %>% 
  filter(isinwarehouse==T) %>% #remove all products not in LSPD
  select(ean, systemSku) %>%  # keep relevant columns 
  left_join(WarehouseAllFone, by = 'ean') %>% #merge warehouse data into LSPD data
  mutate(Inventory = .[[3]]) %>% # change names
  filter(!is.na(Inventory)) %>% # remove all NA inventpry values
  select(systemSku, Inventory) %>% # keep only relevant items
  mutate(systemSku = as.character(systemSku), #change data type
         Inventory = as.character(Inventory)) #change data type

# end of Fone Data
################################################################################

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


# the IN filter allows to search for multiple vendors. should consolidate all of the vendor calls. 275 235 

NorthItemsDF <- allLSPD2 %>% 
  filter(defaultVendorID %in% c(275,235))

# big pipe to merge aadn format all North and mytic data

print('wrangle north and mystic data')

NorthItemsDF <- NorthItemsDF %>%
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

# now delete all the slingshot folders

unlink(dest_file, recursive = T)
unlink('slingshotextractedfolder', recursive = T)

print('deleted all slingshot data from directories after loading it into R')

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

ridefilename <- (list.files(path = 'rideengineextractedfolder', full.names = T))

rideEngine <- read_excel(ridefilename)

# delete ride engine files 

unlink(dest_file, recursive = T)
unlink('rideengineextractedfolder', recursive = T)

print('deleted all ride engine data from directories after loading it into R')

print('ride done')

# filter and wrangle slingshot data
print('wrangle sling')

slingshot <- slingshot %>% 
  select(`UPC Code`, `Location Available`) %>% 
  mutate( upc = `UPC Code`,
          Inventory = as.numeric(ifelse(`Location Available` == 'In Stock', 25, `Location Available`))) %>% 
  select(upc, Inventory) 

# filter and wrangle data ride engine and combine both
print('wrangle ride and merge with sling')

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
print('get slingshot and ride engine lightspeed data')

SevenNationItemsDF <- allLSPD2 %>% 
  filter(defaultVendorID %in% c(232, 237))

# wrangle seven nation data
SevenNationItemsDF2 <- SevenNationItemsDF %>%
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
################################################################################

# Merge all datasets to upload
print('combine into one big dataset')

# right now this includes Fone and Ezzy invntory

AllWarehouseinventory <- bind_rows(EzzyItemsDF, FoneItemsDF, NorthItemsDF, SevenNationItemsDF2)

# make all negative values  =zero (because LSPD doesnt like negatives) 

AllWarehouseinventory$Inventory <- ifelse(as.numeric(AllWarehouseinventory$Inventory) < 0, '0', AllWarehouseinventory$Inventory)



write_csv(x = AllWarehouseinventory,
          file = 'Supplier_Inventory.csv',
          append = FALSE)


