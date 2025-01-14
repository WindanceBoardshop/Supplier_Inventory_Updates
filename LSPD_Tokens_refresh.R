# authentication Variables

LSPDAUTH <- list()
LSPDAUTH$LSPDApiName <- '	InventoryUpdateR'
LSPDAUTH$LSPDClientID <- '0abc655ba630e8de82f6d083987f7ce9a4e6d84d5e4846a7c048190d2fada820'
LSPDAUTH$LSPDClientSecretKey <- 'f259afa4db702a7aba731499d277a4049e65f95a53ad5cb1f4862fcbc81e4d97'
LSPDAUTH$LSPDRefreshToken <- ''
LSPDAUTH$AccessToken <- ''
LSPDAUTH$AUTHCODE <- 'def502003b95253d40164991cfd50ad005545fdfbc46a64f53ab16e464796cfe55cfec325a4b8052bfbecdd77579b340cbf12d141eed8f95a36ce3b08410db5e98731b77ab127930cf8b7a2d81cea143f59b0319b2ebf068db187c877be1133df19af9d9d1e26f1f0fcbd3c9cae9858de18c1d6c7a562f7e7a5a799eb12d7eb4aa9d12cbfbbc273486e3386bbf372b9d23f0871aa6906af65a884ebb87f9a5ddebe046e098e78898d94a50e79b8ab3614624db10771827bd47203621ee1c9c35848fdc080801c568686b0b4560ea51af22dd555c523e7d5efa570a194a963a7d2901ff5ebaf059d3c1d1fb8983e5b214472f8a9de3f30b44408a48cb9395cc3e3b0daf7a5e99404a5e43867553a4a72bbc745fc12119bfb97c3f98f8eb213bebf26da2e8f590a863c97246dcfa8188cafc3a65f584c77ccc382c34665e133918005a45cd4b9b8ee02d77c9fc1297b63d32b4a6155ff040814ebf9be989d5f101fe8e254185c9b1467adb922a60e37bd335c9dded327e5030e6a449976824cc02dac654c4438a36a3fbceb820d1a931a28c40d2cd'

# granting access in browse

utils::browseURL(paste0('https://cloud.lightspeedapp.com/auth/oauth/authorize?response_type=code&client_id=', 
       LSPDAUTH$LSPDClientID,
       '&scope=employee:all'))

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
# GH$REPO_OWNER <- Sys.getenv("GH_REPO_OWNER")
# GH$REPO_NAME <- Sys.getenv("GH_REPO_NAME")
# GH$PAT <- Sys.getenv("GH_PAT")

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
secret_raw <- charToRaw(secret)
# decode public key
pub_key_gh_dec <- base64enc::base64decode(pub_key_gh)
# encrypt using the pub key
secret_raw_encr <- sodium::simple_encrypt(secret_raw, pub_key_gh_dec)
# base64 encode secret
secret_raw_encr <- base64enc::base64encode(secret_raw_encr)
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








