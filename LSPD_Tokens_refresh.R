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
LSPDAUTH$AUTHCODE <- 'def50200f0bbcd94ffc67a7a8da67bb49715eff464f72f3df297d404fda7c878bdccea2e7f49018338a36848cd47e31b41206b55f4fddb6ac0a154d1dbfa96cf18d4705d2a0ad51d57533a504ca13260993805bce8a90f96a3bb812d6726246bab27fbfefcc16c2306d3890a42805c385dc37d2b0bf5ffee215688d5a72e401f1a4b8019b5f9cb29a6f97302e5af3feea5b85e61c9cb0c14e9a7c8c166c178658667dd1fa585b350b12a808cae111d1ae70eaa0e5a9b00dcdf1974e30e56c4d0c3de4643928193f0e23c71db3db5d545721b8f06f8817ffca0fc3e14906a754179a7b38468d24ca90ef0f1cc026696e34404aa49bd644a90f2f210a56aa79126a6febd816ebe7328314702b338c14c26cdca1e7bff2f19b9af42e3076194108996287406e6f3bf0287e5b8e9b7f81e5098308a13fa61edffdf985e37fc58b8719d41309e01f554a6e63db2083042f825cf5abf3b091fd1b3b38d057e676f3182edae212efd35e125d211cb6c59a2d90eaebcb067dcb001b83ab3adb00a922012faaa89c05b074045dde4a77f03ff8790bcdc7dbf'

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








