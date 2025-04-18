# the goal of this script is to produce a .csv file that compares slingshot online pricing to Windance pricing. 
# do this in a day. make it as messy as you want. limit scope. its like whatever. but you can do it. 

# Start libraries
library(tidyverse) # I love Hadley 
library(jsonlite) # for JSON data manipulation
library(httr2) # for scrapping and JSON data load
library(openssl) # for LSPD authenication
library(sodium) # for LSPD authentication flow 
# end   libraries 

# Start load LSPD data
# end   load LSPD data

# Start load env. vars
print(' start load env vars')
LSPDAUTH <- list()
LSPDAUTH$LSPDApiName <- Sys.getenv("LSPDAUTH_LSPDAPINAME")
LSPDAUTH$LSPDClientID <- Sys.getenv("LSPDAUTH_LSPDCLIENTID")
LSPDAUTH$LSPDClientSecretKey <- Sys.getenv("LSPDAUTH_LSPDCLIENTSECRETKEY")
LSPDAUTH$LSPDRefreshToken <- Sys.getenv("LSPDAUTH_LSPDREFRESHTOKEN")
LSPDAUTH$AccessToken <- Sys.getenv("LSPDAUTH_LSPDACCESSTOKEN")
print('end load env vars')
# end   load env. vars


# Start LSPD authenitcation Flow

# get new access token using refresh token

print(' start Lightspeed Authenitcation Flow.')

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
 print('end LSPD authentication flow')
# end   LSPD authenitcation Flow
 
 

# Start get LSPD data
 print('start get LSPD product data')
 
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
   keep(~ length(.) == 27) %>%
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
 
 print('end get LSPD product data')
 
# end  get LSPD data
 
 # start wrangle LSDP data
 print('start wrangle LSPD data')
 allLSPD3 <- allLSPD2 %>% 
   filter(defaultVendorID == 232 & manufacturerSku != '') %>% 
   select(description, upc, manufacturerSku, Price_amount, MSRP_amount)
 
 print('end wrangle LSPD data')
 # end wrangel LSPD data

# Start scrape Slingshot data
 
 print('start scrape slingshot data')
 
 # Base URL
 url <- "https://slingshotsports.com/products.json?limit=250&page="
 page <- 1
 all_products <- list()  # Initialize empty list
 
 repeat {
   # Construct the full URL
   fullurl <- paste0(url, page)
   
   # Perform the GET request and parse JSON
   response <- request(fullurl) %>% 
     req_method("GET") %>% 
     req_perform() %>% 
     resp_body_json()
   
   # Check if products exist
   if (length(response$products) == 0) {
     message("No more products found. Stopping download.")
     break
   }
   
   # Append to master list
   all_products <- append(all_products, response$products)
   
   # Print progress
   message("Fetched page ", page, " with ", length(response$products), " products.")
   
   # Increment page number
   page <- page + 1
 }
 
 # unnest data and format
 
 slingdata <- all_products %>% 
   tibble %>% 
   unnest_wider(1) %>% 
   unnest_longer(variants) %>% 
   unnest_wider(variants,names_sep = '_')
 
 print('end scrape slingshot data')
 
# end scrape Slingshot data

# Start select slingshot price and id data
 
 slingdata2 <- slingdata %>% 
   select(variants_sku, variants_price) %>% 
   mutate( slingshot_variants_sku = trimws(format(variants_sku, scientific = FALSE)),
           slingshot_variants_price = variants_price) %>% 
   select(slingshot_variants_sku,
          slingshot_variants_price)
# end

# Start merge datasets
 print(' start merge datasets')
 sling_LSPD_price_comparison <- allLSPD3 %>% 
   left_join(slingdata2, by = c('manufacturerSku' = 'slingshot_variants_sku')) %>% # merge datasets
   filter(!is.na(slingshot_variants_price)) %>%  # this gets rid of all the variants that are not on slingshots website. 
 mutate( price_difference = round(as.numeric(slingshot_variants_price) - as.numeric(Price_amount),2),
         update_price = price_difference < 0) %>% 
   filter(update_price == TRUE)
 
 
   
 print(' end   merge datasets')
# end  merge datasets

# Start write csv to repo
 
 print('write csv')
 write_csv(x = sling_LSPD_price_comparison,
           file = 'slingshot_price_comparison.csv',
           append = FALSE)
 
# end write csv to repo

 # start scrape ride engine data
 
 print('start scrape Ride Engine data')
 
 # Base URL
 url <- "https://rideengine.com/products.json?limit=250&page=" # check that this is actually the correct web address
 page <- 1
 all_products <- list()  # Initialize empty list
 
 repeat {
   # Construct the full URL
   fullurl <- paste0(url, page)
   
   # Perform the GET request and parse JSON
   response <- request(fullurl) %>% 
     req_method("GET") %>% 
     req_perform() %>% 
     resp_body_json()
   
   # Check if products exist
   if (length(response$products) == 0) {
     message("No more products found. Stopping download.")
     break
   }
   
   # Append to master list
   all_products <- append(all_products, response$products)
   
   # Print progress
   message("Fetched page ", page, " with ", length(response$products), " products.")
   
   # Increment page number
   page <- page + 1
 }
 
 # unnest data and format
 
 rideenginedata <- all_products %>% 
   tibble %>% 
   unnest_wider(1) %>% 
   unnest_longer(variants) %>% 
   unnest_wider(variants,names_sep = '_')
 
 print('end scrape ride engine data')
 
 # end scrape ride engine data
 
 # start ride engine data wrangle (price and Id) 

 rideeneginedata2 <- rideenginedata %>% 
   select(variants_sku, variants_price) %>% 
   mutate( rideengine_variants_sku = trimws(format(variants_sku, scientific = FALSE)),
           rideengine_variants_price = variants_price) %>% 
   select(rideengine_variants_sku,
          rideengine_variants_price)
 # end ride engine data wrangle
 
 # start LSPD subsetting
 print('start wrangle LSPD data for ride engine')
 
 allLSPD4 <- allLSPD2 %>% 
   filter(defaultVendorID == 237 & manufacturerSku != '') %>% 
   select(description, upc, manufacturerSku, Price_amount, MSRP_amount)
 
 
 # Start merge datasets
 print(' start merge datasets')
 rideengine_LSPD_price_comparison <- allLSPD4 %>% 
   left_join(rideeneginedata2, by = c('manufacturerSku' = 'rideengine_variants_sku')) %>% # merge datasets
   filter(!is.na(rideengine_variants_price)) %>%  # this gets rid of all the variants that are not on slingshots website. 
   mutate( price_difference = round(as.numeric(rideengine_variants_price) - as.numeric(Price_amount),2),
           update_price = price_difference < 0) %>% 
   filter(update_price == TRUE)
 
 
 
 print(' end   merge datasets')
 # end  merge datasets
 # end ride engine data wrangle
 