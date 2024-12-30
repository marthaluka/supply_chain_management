


# read data1 (master data) -- here, the location (dist, region etc) reflect the patients' home, not the clinics'
ibcm_bite_data <- read.csv("./data/ibcm_first_visit_patient_district_monthly_2018_2023-4.csv") %>%
  dplyr::mutate(
    Region_patient = REGION,
    District_patient = DISTRICT,
    Year_month = as.yearmon(paste(Year, Month, sep = "-"), "%Y-%m"),
    Year = year(Year_month)) %>%
  # clean up and format columns
    # rename columns, format date & drop NAs
  dplyr::mutate(
    Region_patient = REGION,
    District_patient = DISTRICT,
    Year_month = as.yearmon(paste(Year, Month, sep = "-"), "%Y-%m"),
    Year = year(Year_month),
    new_FACILITY = case_match(
      new_FACILITY,
      "Mkomaindo Hospital" ~ "Masasi Hospital",  # same hosp
      "Mangaka Hospital" ~ "Nanyumbu District Hospital",   # same hosp
      .default = new_FACILITY
    )) %>%
  drop_na()%>%
  dplyr::filter(!new_FACILITY %in% c("Tanzania Facility", "Rabies Test Facility", "Missing", "Mikumi Health Center"), # Mikumi Health Center inconsistent in reporting
                Year_month > as.yearmon("2018-5"),
                Year_month < as.yearmon("2024-1")
                ) %>%
  # drop unnecessary cols
  dplyr::select(-c(REGION, DISTRICT)) ->p

unique(ibcm_bite_data$new_FACILITY) %>% sort()

# PEP status dataset
facilities_PEP_status <- read.csv("./data/facilitiesPEPstatusOct2024.csv") %>%
  dplyr::select(name_facility, facility_type, District_facility, Region_facility, `facility_PEP_status`) %>%
  # dplyr::filter(!Region_facility == "Arusha",
  #               !District_facility == "Serengeti") %>%
  # revert to older districts (2012)
  dplyr::mutate(District_facility = dplyr::recode(District_facility,
                                                  'Mtwara Urban' = 'Mtwara',
                                                  'Mtwara Rural' = 'Mtwara',
                                                  'Masasi Township Authority' = 'Masasi',
                                                  'Lindi Urban' = 'Lindi',
                                                  'Lindi Rural' = 'Lindi',
                                                  'Morogoro Urban' = 'Morogoro',
                                                  #'Mvomero' = 'Morogoro', 
                                                  'Tarime District Council' = 'Tarime',
                                                  'Musoma Municipal' = 'Musoma'),
                facility_type = ifelse(name_facility == "Kitangari Dispensary", "Dispensary", facility_type),
                `facility_PEP_status` = ifelse(name_facility %in% c("Kitangari Dispensary", "Likombe Health Center", "Chiponda Health Center", "Coptic Health Center",
                                                                    "Peroza Health Center"), "No", facility_PEP_status)
                ) # Coptic and Peroza are private

unique(facilities_PEP_status$District_facility)
# find clinics missing in status dataset
status_clinics <- facilities_PEP_status$name_facility

ibcm_bite_data %>% 
  dplyr::filter(!new_FACILITY %in% status_clinics)
# 'St. Kizito Hospital' in Morogoro is missing -- removed extra white space 
# All good

# merge the two
combined_data <- ibcm_bite_data %>% 
  left_join(., facilities_PEP_status, by = c("new_FACILITY" = "name_facility")) %>% 
  # drop Arusha and serengeti clinics
  dplyr::filter(!District_facility == "Serengeti",
                !Region_facility == "Arusha") # lost 2,595 bite patients by dropping the two locations


    # This dataset has 27 districts and 6,973 patients


# Explore different filter criteria

  ## 1. filter out dispensaries

disp_out <- combined_data %>%
  dplyr::filter(!facility_type == "Dispensary")

# patients
print(paste("Dropping dispensaries drops", (sum(combined_data$n) - sum(disp_out$n)), "patients"))

# districts
ditricts_lost <- unique(combined_data$District_facility)[!unique(combined_data$District_facility) %in% unique(disp_out$District_facility)]

print(paste("This filter criteria loses", length(ditricts_lost), "districts" ))
  



  ## 2. filter out clinics that temporarily stock PEP

drop_tmp_PEP <- combined_data %>%
  dplyr::filter(#!new_FACILITY == "Peroza Health Center",  # Peroza is private
                facility_PEP_status == "Available"
                ) 

# patients
print(paste("Dropping temporary stocking of PEP drops", (sum(combined_data$n) - sum(drop_tmp_PEP$n)), "patients"))

# districts
ditricts_lost2 <- unique(combined_data$District_facility)[!unique(combined_data$District_facility) %in% unique(drop_tmp_PEP$District_facility)]

print(paste("This filter criteria loses", length(ditricts_lost2), "districts" ))
# all 20 districts still present


  ## 3. Drop dispensaries and those temporarirly stocking PEP


drop_both_dispAndtmpPEP <- combined_data %>%
  dplyr::filter(facility_PEP_status == "Available",
                !facility_type == "Dispensary")

# patients
print(paste("Dropping dispensanries and temporary stocking of PEP drops", (sum(combined_data$n) - sum(drop_both_dispAndtmpPEP$n)), "patients"))

# districts
ditricts_lost3 <- unique(combined_data$District_facility)[!unique(combined_data$District_facility) %in% unique(drop_both_dispAndtmpPEP$District_facility)]

print(paste("This filter criteria loses", length(ditricts_lost3), "districts" ))
# all 20 districts still present


unique(drop_both_dispAndtmpPEP$District_facility) %>% sort()
unique(combined_data$District_facility) %>% sort()



### Manuscript results section
# intro paragraph 
print(paste("Total bites in main data", sum(combined_data$n)))
print(paste("distinct districts (excluding Arusha region and Serengeti) in data:", length(unique(combined_data$District_facility))))



# number of dispensaries
  ## facilities
combined_data %>%
  dplyr::select(new_FACILITY, facility_type) %>%
  distinct()%>%
  dplyr::count(facility_type)

  ## patients
combined_data %>%
  group_by(facility_type) %>%
  dplyr::summarise(sum_pts = sum(n))

# Number PEP available
  ## facilities
combined_data %>%
  dplyr::filter(!facility_type == "Dispensary")  %>% # OTHER clinics temporarily stocking PEP
  dplyr::select(new_FACILITY, facility_PEP_status) %>%
  distinct()%>%
  dplyr::count(facility_PEP_status)

  ## patients
combined_data %>%
  dplyr::filter(!facility_type == "Dispensary") %>%
  group_by(facility_PEP_status) %>%
  dplyr::summarise(sum_pts = sum(n))


# Included in analysis
drop_both_dispAndtmpPEP

print(paste("Patients:", sum(drop_both_dispAndtmpPEP$n)))
print(paste("Clinics:", length(unique(drop_both_dispAndtmpPEP$new_FACILITY))))
print(paste("Districts:", length(unique(drop_both_dispAndtmpPEP$District_facility))))

