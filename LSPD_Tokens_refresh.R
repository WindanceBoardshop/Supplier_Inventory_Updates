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
LSPDAUTH$AUTHCODE <- 'def502007b7411d057adc0d5dc435d04e2e7b7e0af91c075334e093a065d6e6c4be6180a1c6c11740640b01b2e060f90d6277f8b153d69b23a59a0f5a2b57f91b12aaadc9fcb7dff2a1a8ed70952a5068bd3e77ed23ad600ad1831f5112dae3aeb7eb56f782fa8fb0d509bff681b09c655e1b1c8dcd8d498153735faecb15bf38694aa88b0aadf1f77499a4b03e817078a577624dee72f11d82656951bbb12e75827ca1d0c6ef03eb419d2f29cccfc17ed91f2d0a5c8cea19ce63edf07cc3672922116fcb5be5cba4c1565ccc637c21b8c6eda5fd77eb0945b68fd6f3d672d78a187f65288110b5e2dd522924e257a73ac685fdbbd82600be56c53cfba041c1d18f8fd84031c85505a40be5a110118b61a8a629691b7db3d79f15b301cd363aaef833dc18b0875e1d21bfc7332bf225551c146b14ebb4fd7c06d37a9d82be1356bba4eafabdbaf377539a3fed16a8ee14db09e5684925a3a9a3797eb01aca166102d8a51c8d2d39681c28bf82f10cc93e71c2fd7b11eb76f506cc754df323ff5dc5c2fc4ae85fbf44554aa1cff01b86f37ad653b'

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








