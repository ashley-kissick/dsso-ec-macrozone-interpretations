#Get the field-microenvironments:

install.packages("Microsoft365R")
library(Microsoft365R)
library(dplyr)

# Connect to your specific SharePoint site
site <- get_sharepoint_site(
  site_url = "https://bayergroup.sharepoint.com/sites/PreceonProduction/Shared%20Documents/Forms/AllItems.aspx?csf=1&web=1&e=wIW94D&ovuser=fcb2b37b%2D5da0%2D466b%2D9b83%2D0014b67a7c78%2Cashley%2Ekissick%40bayer%2Ecom&TeamsCID=6ceba3eb%2D7cf7%2D4d16%2Da2dc%2Da5392d5b609b&OR=Teams%2DHL&CT=1783522036286&clickparams=eyJBcHBOYW1lIjoiVGVhbXMtRGVza3RvcCIsIkFwcFZlcnNpb24iOiI1MC8yNjA0MzAxOTIxNiIsIkhhc0ZlZGVyYXRlZFVzZXIiOmZhbHNlfQ%3D%3D&CID=f64525a2%2D5058%2D2001%2D2217%2D304048bedfbc&cidOR=SPO&FolderCTID=0x012000130B928B9F63A748BB9DD57665C1DDAC&id=%2Fsites%2FPreceonProduction%2FShared%20Documents%2FGeneral%2FComp%20Sci%2FEHT%20Risk%20Model%2FEHT%5FSampling%5FDesign%5F2026%2Fshortstature%5Fhybrids%5Ffields%5F26%5Fec%5Fssurgo"
)

# Retrieve the default document library
drive <- site$get_drive()

# Alternatively, list all libraries if you have multiple
# site$list_drives()

# Connect to your specific folder path
folder_path <- "Shared Documents/ProjectData/2026_Reports"
folder <- drive$get_item(folder_path)

# List all files and folders inside that directory
files_df <- folder$list_items()
print(files_df)


# Download a file from the SharePoint folder to your local working directory
folder$download_file(
  src = "monthly_summary.csv",
  dest = "local_summary.csv"
)

# Upload a file from your computer into the SharePoint folder
folder$upload_file(
  src = "local_output.xlsx",
  dest = "final_output.xlsx"
)



