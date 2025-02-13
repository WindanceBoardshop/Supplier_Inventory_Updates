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
    'Good Morning, 
    
    Please Upload the attached csv file to Lightspeed.
    
    Steps: 
    1. login into Lightspeed
    2. navigate to **[Inventory > Inventory Counts](https://us.merchantos.com/?name=inventory_count.listings.inventory_counts&form_name=listing&searchstr=&shop_id=2&archived=off&__sort=last_modified&__sort_dir=DESC)**
    3. select inventory Count: ',Sys.Date(), 'Supplier Update
    4. Drag and drop attached file into the `Import Items` area. 
    5. Map Columns: SystemSku = SystemID, and Inventory = Count
    6. Verify File
    7. Import
    8. Navigate to `Missed` 
    9. select all items
    10. Press `Zero all selected items`
    11. Navigate to `Reconcile` and click `Reconcile Inventory`
    
    
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




