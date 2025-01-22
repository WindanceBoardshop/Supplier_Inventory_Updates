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
LSPDAUTH$AUTHCODE <- 'def50200ac226bd2e840a953086cc060c6a363ecd8cdc835fc5a53f4f00e259202549dcc1641de54464f6c40b1307fc6779d46fb5d19d2405beead6c5b40a913f27f92bf0e0d1569af2975a32fbfba9090e8caad63d3c7bdfd39fb411a1a121d13e70cf5a1f24047a705889fa589d4d362ce5c4b2ba9701182142ad4ff229b51c5497909eacc094d5910646548c23cc53c21f7a7701389de7fa635d36f33234d43ed0c2ca26bebe65c3d01c6e8a93e173fd1b6916d525e868656e1b0baede126c1b5fb9d7268839a829eceae50d226102786cdf3338524f9b6345da01799e08e148ee3e1ce1479ddf743f24c97b649881c2aca324270a0983dbac734687afccd11915f16c6609f274bdb666bf75146bfd3ff487b5ff183070bccf488ee1c3cf3f6b8a5545abe723e22c6f7d731649d6b372dd948446ed44565235e5ed6f756f276eb72f314fc26ebb9c8d0887398b295ed2275d31885d72dbe76f262df79b0bee043dbd16baf2dae558a6ee29f7800b41d68c2689213dc134125da5ab01222aafd0fa0861932254a3e336b5d2fedd77eab60f4e8'

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








