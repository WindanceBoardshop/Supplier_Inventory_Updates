# Send email with updated CSV file

library(blastula)
library(readr)

data <- read_csv2('Supplier_Inventory.csv')


# email



# setup to send email

# steps: need to load user name and password to github secrets. 
# and call them in the yaml file. 

msg <- compose_email(
  body = md(paste(
    'Test Good Morning, 
    
    Please Uplpoad the attached csv file to Lightspeed.
    
    Steps: 
    1. login into Lightspeed
    2. navigate to Inventory > Inventory Counts 
    3. filter for `Supplier Warehouses` in the search bar and search 
    4. select inventory Count: ',Sys.Date(), 'Supplier Update
    5. Drag and drop attached file into the `Import Items` area. 
    6. Map Columns: SystemSku = SystemID, and Inventory = Count
    7. Verify File
    8. Import
    9. Navigate to `Missed` 
    10. select all items
    11. Press `Zero all selected items`
    12. Navigate to `Reconcile` and click `Reconcile Inventory`
    
    
    For a step by step video guide please visit: 
    For Questions or issues contact: jasonelder@windance.com
    
    Thank you, 
    
    Jason `The Yay` Elder')
  )
)

# add warehouse data as attachment 


msg <- add_attachment(email = msg,
                      file = 'Supplier_Inventory.csv')

# credentials

email_creds <- creds_envvar(
  user = Sys.getenv('GMAIL_USER'),
  pass_envvar = 'GMAIL_PASSWORD',
  provider = 'gmail'
)

# send the email


smtp_send(
  email = msg,
  to = 'jasonelder@windance.com',
  cc = 'jasonelder@windance.com',
  from =  'windanceautomation@gmail.com',
  subject = paste('Windance Supplier Update: ',Sys.Date()),
  credentials = email_creds
)




