library(httr2)
library(openssl)
library(sodium)

# authentication Variables

LSPDAUTH <- list()
LSPDAUTH$LSPDApiName <- '	InventoryUpdateR'
LSPDAUTH$LSPDClientID <- '0abc655ba630e8de82f6d083987f7ce9a4e6d84d5e4846a7c048190d2fada820'
LSPDAUTH$LSPDClientSecretKey <- 'f259afa4db702a7aba731499d277a4049e65f95a53ad5cb1f4862fcbc81e4d97'
LSPDAUTH$LSPDRefreshToken <- ''
LSPDAUTH$AccessToken <- ''
LSPDAUTH$AUTHCODE <- ''

# granting access in browse

utils::browseURL(paste0('https://cloud.lightspeedapp.com/auth/oauth/authorize?response_type=code&client_id=', 
       LSPDAUTH$LSPDClientID,
       '&scope=employee:all'))

# update AUTH code
LSPDAUTH$AUTHCODE <- 'def5020036501d4305af0ac1c8d3af8e444012e56671fba4ecd4fd31c46778900072c484ae17c44ffa7fe86fe0652b87fbebdf6b8a854dc913c7b2193de8d92749052857e6f68e85333b090e1af420d83cdaed3bdda956327ababfc16b626a7ab53e02337f7352706e3edf633379a2367d1f4a0de305029da1770360187fcbd8aab7e1a239ae7cb5284f37db5de7843727a8a150ccdf8b2b33cfa09e2e6846d661df9b7790b47f5ae13a9e5e356d704f685bd053e5c300d665ebb6642fbadb62e088caa024342402cb62a87b403140b371d4f29ccaff99a3cbc79f99d71c30d70dc37f8f2981b2d57b65f5954f53d1a27ebabbe05122f9c2cc77a2b29eb8fe499cd5b1fe8322b1e83fc1cb1e1a899def40ae1a691b92e276b12d8814625ae10523ea243aba79d5489d8a7179ba5c92e1b31ec5bb0a7bc8053d6186152bb656096cc9b8d37005d9941120a99190ede068d1ce3f7710d5d289afbed3beb21d4db023f45510cf742da2bbaf80497a635902cbc25580fac35c26495f398722a8260fea2ffe7734b8c28611b8edc06de4766d5f30afcc'

# getting tokens

token_endpoint <- 'https://cloud.lightspeedapp.com/auth/oauth/token'

# prep body for api call

body <- list(client_id = LSPDAUTH$LSPDClientID,
             client_secret = LSPDAUTH$LSPDClientSecretKey,
             grant_type = "authorization_code",
             code = LSPDAUTH$AUTHCODE)

# api call for tokens

response <- request(token_endpoint) %>% 
  req_body_json(body) %>% 
  req_method("POST") %>% 
  req_perform()

# check response

print(resp_status(response))
response_content <- resp_body_json(response)

#update tokens values. 

LSPDAUTH$LSPDRefreshToken <- response_content$refresh_token
LSPDAUTH$AccessToken <- response_content$access_token

# end of LSPD token requests
###################################################################################
# update values in github
###################################################################################

# start of Github Var updates

# Github information and variables. load these variables from the system environment

# keep this for github to run
#GH <- list()
#GH$REPO_OWNER <- Sys.getenv("GH_REPO_OWNER")
#GH$REPO_NAME <- Sys.getenv("GH_REPO_NAME")
#GH$PAT <- Sys.getenv("GH_PAT")

# Use this to run in R
GH <- list()
GH$REPO_OWNER <- "WindanceBoardshop"
GH$REPO_NAME <- "Supplier_Inventory_Updates"
GH$PAT <- "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"


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
# convert to raw for sodium

# # # # # # # # #  #



# Step 2: Encrypt the secret value using LibSodium
refresh_token <- charToRaw(LSPDAUTH$LSPDRefreshToken)
public_key <- base64_decode(GH$PUBLIC_KEY)

GH$LSPD_REFRESH_TOKEN <- simple_encrypt(refresh_token, public_key)

# Base64 encode the encrypted secret
GH$LSPD_REFRESH_TOKEN <- base64enc::base64encode(GH$LSPD_REFRESH_TOKEN)


###################################################

# Step 3: Prepare the payload
payload <- list(
  encrypted_value = GH$LSPD_REFRESH_TOKEN,
  key_id = GH$KEY_ID)

# Step 4: Make the API request to update the secret
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








