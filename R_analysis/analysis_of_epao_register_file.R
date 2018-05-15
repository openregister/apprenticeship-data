# Library Call
library(dplyr)


# Reg1 - reg_organisation = 'assessment-organisation'
organisations <- read.csv("epao-register_registerOrganisations.csv", stringsAsFactors = FALSE)  # read in file
organisations <- organisations[,-c(11,12)]  #removed two empty columns, erroneusly added from import. 
organisations <- select(organisations,EPA_organisation_identifier,UKPRN,EPA_organisation,Organisation_type,Website_link)  #reordering fields
organisations <- organisations %>% mutate(UPRN = NA)  #added empty 'UPRN' column
reg_organisation <- organisations ## Will need to normalise the 'org-type' column
reg_organisation <- reg_organisation %>% select(EPA_organisation_identifier, UKPRN, EPA_organisation, Org_type, UPRN)
reg_organisation$start_date <- NA
reg_organisation$end_date <- NA
reg_organisation <- reg_organisation %>% rename(assessment_organisation = EPA_organisation_identifier)
reg_organisation <- reg_organisation %>% rename(organisation_name = EPA_organisation)
reg_organisation <- reg_organisation %>% rename(ukprn = UKPRN)
reg_organisation <- reg_organisation %>% rename(organisation_type = Org_type)
reg_organisation <- reg_organisation %>% rename(uprn = UPRN)
reg_organisation <- reg_organisation %>% rename(address = uprn)

# Reorder columns
reg_organisation <-  reg_organisation %>% select(assessment_organisation,organisation_name, ukprn,  organisation_type, address, start_date, end_date)

#write csv
write.csv(reg_organisation, file = "assessment-organisation.csv")


#file for addressmatching
org_addresses <- data.frame(organisations$EPA_organisation_identifier,organisations[,5:10])  #created file for addressmatching
org_addresses$UPRN <- NA  #added column to fill with UPRNs
org_addresses <- org_addresses[,-7]  #removed 'UKPRN' field as not needed for addressmatching.

#write csv
write.csv(org_addresses, file = "org_addresses_for_matching.csv")


# Reg2 - reg_orgtype = assessment-organisation-type
organisations[3]  # Col.3 'Organisation type', appears to be a classification 
organisation_type_count <- organisations %>% group_by(Organisation_type) %>% summarise(n())  ## Count number of occurences
organisation_type_count$Index <- seq.int(nrow(organisation_type_count))  ## Add an index to table
organisation_type_ls <- unique(organisations$Organisation_type)  ##list of unique values
#use 'organisation_type_count' to create 'org_type' register:
reg_orgtype <- organisation_type_count
reg_orgtype <- reg_orgtype[,-2] #remove 'n()' column
reg_orgtype <- add_row(reg_orgtype,Organisation_type = 'Non-departmental public body', Index = '9')
#Re-order columns, and add start/end date columns
reg_orgtype <- reg_orgtype %>% select(Index, Organisation_type)
reg_orgtype$start_date <- NA
reg_orgtype$end_date <- NA
#Rename columns
reg_orgtype <- rename(reg_orgtype, assessment_organisation_type = Index)
reg_orgtype <- rename(reg_orgtype, organisation_type = Organisation_type)

#join reg_organistion with reg_type, keeping only the index column
reg_organisation <- reg_organisation %>% left_join(reg_orgtype, by = 'Organisation_type')
reg_organisation <- rename(reg_organisation, Org_type = Index)

#write csv
write.csv(reg_orgtype, file = "assessment-organisation-type.csv")


# Reg3 - reg_deliveryArea = assessment_organisation_delivery_area
#read csv:
lookups <- read.csv('epao-register_lookups.csv', header = TRUE)
#create a dataframe with  only the 'delivery area column:
reg_deliveryArea <- lookups[1]
reg_deliveryArea$Index <- seq.int(nrow(reg_deliveryArea)) #add an index to the table:
#[ ] Make sure to check against the t-reg for any values unaccounted for

#Re-order columns, and add start/end date columns
reg_deliveryArea <- reg_deliveryArea %>% select(Index, Delivery_area)
reg_deliveryArea$start_date <- NA
reg_deliveryArea$end_date <- NA

#rename coulumns
reg_deliveryArea <- rename(reg_deliveryArea, assessment_organisation_delivery_area = reg_deliveryArea)
reg_deliveryArea <- rename(reg_deliveryArea, delivery_area = Delivery_area)

xx <- reg_deliveryArea %>% select(Delivery_area,Index)  ##temp table to facilitate joining for transactional register

#write csv
write.csv(reg_deliveryArea, file = "assessment_organisation_delivery_area.csv")


#Reg4 - reg_standardCode = assessment_organisations_standard_code
#upload file
reg_standardCode <- read.csv('epao_register_standards_lookups.csv',header = TRUE,stringsAsFactors = FALSE)
#how many unique values under 'StandardCode':
length(unique(reg_standardCode$StandardCode)) ## 236
length(reg_standardCode$StandardCode) ## 237

# Notes:
# -entries are not sequential. sequence breaks as of 204, due to a "204a" at #205
# -there are two rows with a blank standard code
# -at the blanks, the number sequence jumps a few numbers.

#Decision - remove rows that do not have a standardcode
og_reg_standardCode <- reg_standardCode # a version with the rows intacted
reg_standardCode <- reg_standardCode[-c(229,230),]

#Filtered dataset for the columns that will remain, and add start/end_date columns
reg_standardCode <- reg_standardCode %>% select(StandardCode, StandardName, LastDateStarts)
reg_standardCode$start_date <- NA
reg_standardCode$end_date <- NA
#rename columns
reg_standardCode <- rename(reg_standardCode, assessment_organisation_standard_code = StandardCode)
reg_standardCode <- rename(reg_standardCode, standard_name = StandardName)
reg_standardCode <- rename(reg_standardCode, last_date_start = LastDateStarts)

#write csv
write.csv(reg_standardCode, file = "assessment_organisation_standard_code.csv")


#Reg5 - Transactional Register = assessment_organisation_standard_guide
og_standards <- read.csv("epao_register_standards.csv", stringsAsFactors = FALSE)  #readfile
og_standards <- og_standards[,-12]  #remove errorneuos 'X' column
reg_transactional <- og_standards  #made a copy,

#Joining 'Delivery Area' onto db, joining on 'Standard Code' from 'reg_standardCode
xx <- reg_deliveryArea %>% select(delivery_area,assessment_organisation_delivery_area)
xx <- rename(xx,"Delivery_area" = 'delivery_area')
epao_delivery_areas <- read.csv("epao_register_delivery_areas.csv", stringsAsFactors = FALSE, header = TRUE)
epao_delivery_areas <- epao_delivery_areas[-c(494:580),] #remove the empty rows at the bottom of sheet

reg_transactional <- reg_transactional %>% left_join(epao_delivery_areas, by = c("EPA_organisation_identifier","Standard_code"))
reg_transactional <- reg_transactional %>% left_join(xx,by = "Delivery_area")

#renaming fields, and adding register standard fields
reg_transactional$start_date <- NA
reg_transactional$end_date <- NA
reg_transactional <- rename(reg_transactional, assessment_organisation_delivery_area = Delivery_area)
reg_transactional <- rename(reg_transactional, assessment_organisations_standard_code = Standard_code)
reg_transactional <- rename(reg_transactional, assessment_organisation = EPA_organisation_identifier)
#Add an index to table, and renaming field
reg_transactional$index <- seq.int(nrow(reg_transactional))
reg_transactional <- rename(reg_transactional, assessment_organisation_apprenticeship = index)  #rename 'index' to 'assessment_organisation_standard_guide' 

# !! - before removing fields, resolve the 'NA' cells in 'assessment_organisation_delivery_area' field

#reorder and selecting fields
assessment_organisation_apprenticeship <- reg_transactional  ## make a copy
assessment_organisation_apprenticeship <- assessment_organisation_apprenticeship %>% select(assessment_organisation_apprenticeship,assessment_organisation,assessment_organisations_standard_code,assessment_organisation_delivery_area,start_date,end_date)
#rename register to 'assessment_organisation_apprenticeship'
assessment_organisation_apprenticeship <- rename(assessment_organisation_apprenticeship, "assessment_organisation_apprenticeship" = assessment_organisation_standard_guide)
#write to csv:
write.csv(assessment_organisation_apprenticeship, file = "assessment_organisation_apprenticeship.csv")


# Checking:
# [x] Reg1 - reg_organisation = 'assessment-organisation'
# [x] Reg2 - reg_orgtype = assessment-organisation-type
# [x] Reg3 - reg_deliveryArea = assessment_organisation_delivery_area
# [x] Reg4 - reg_standardCode = assessment_organisations_standard_code
# [x] Reg5 - Transactional Register = assessment_organisation_apprenticeship


# Errors found:
# # reg_standardCode
# - found duplicate entries for 'Laboratory Scientist'(44,221) and 'Welding'(94,95). Will delete the latter of each and update other records with the correct value
reg_standardCode$standard_name[duplicated(reg_standardCode$standard_name)]
# # reg_transactional
# - found record with incorrect index for standard code row:3197, update 221 to 44.
# - Incomplete join of 'delivery_area' column, some rows missing values
# - for some rows, 'delivery_area' has multiple entries. have to find a way to show this.

##reg_transactional$assessment_organisation_standard_code[3197] <- 221 ## Do not action! - appears to be an update to 221, Query!

# Error correction 
##attempting join again after removing the empyt rows.
xx <- reg_deliveryArea %>% select(delivery_area,assessment_organisation_delivery_area)
xx <- rename(xx,"Delivery_area" = 'delivery_area')
epao_delivery_areas <- read.csv("epao_register_delivery_areas.csv", stringsAsFactors = FALSE, header = TRUE)
epao_delivery_areas <- epao_delivery_areas[-c(494:580),] #remove the empty rows at the bottom of sheet

reg_transactional <- reg_transactional %>% left_join(epao_delivery_areas, by = c("EPA_organisation_identifier","Standard_code"))
reg_transactional <- reg_transactional %>% left_join(xx,by = "Delivery_area")


### 08/05/18
## Load new data for Apprenticeship Standards code.
# Load file:
ifa_apprenticeshipStandards <- read_excel('../lists/ApprenticeshipStandards.xlsx', col_types = 'text',skip = 1)
# subset the ifa data for name and reference:
og_ifa <- ifa_apprenticeshipStandards[,1:2]

# join onto reg_standard
Joined <- og_ifa %>% left_join(reg_standardCode, by = c('Name'='standard_name'))
###only 60 hits were acheived via this join, some obvious mataches were missed, need to improve, join critera.
# add column that removes whites spaces from both tables, and format text to be all lower case.
og_ifa$joining <- tolower(gsub(" ", "", og_ifa$Name, fixed = TRUE))
reg_standardCode$joining <- tolower(gsub(" ","", reg_standardCode$standard_name, fixed = TRUE))
# attempt join again
Joined <- og_ifa %>% left_join(reg_standardCode, by = 'joining')
# document the failed joins
failedJoins <- Joined$Name[is.na(Joined$standard_name)]
length(failedJoins) # 356 of 547 -- Questionable, considering reg_standardCode has only 235 records, points to duplication
Joined_dups <- table(Joined$Name)

# prove which standards are present in both lists, and which aren't(og_ifa, reg_standardCode).
Joined_success <- Joined %>% filter(is.na(Joined$assessment_organisation_standard_code) == FALSE) # a list of succesful joins, 191 records
Joined_fail <-  Joined %>% filter(is.na(Joined$assessment_organisation_standard_code) == TRUE) # a list of unsuccessful joins, 356 records

anti_join(reg_standardCode, og_ifa, by = 'joining')
