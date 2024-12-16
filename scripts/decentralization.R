


require(pacman)
pacman::p_load(reshape2)

# Data ##############
# Examine a district

# How many health centers?  # facilities per district
  # Distribution of patients across these clinics per district. What prop of pts attend central hosp? Pcentral
    # Explores theoretically how decentralizing PEP will change vial use


head(ibcm_bite_data)


# district names 
ibcm_districts_central_hosps <- data.frame(District_facility = unique(complete_data$dist)) %>%
  dplyr::mutate(new_FACILITY = case_when(
    District_facility == "Bunda" ~ "Bunda District Hospital",
    District_facility == "Butiama" ~ "Butiama District Hospital",
    District_facility == "Chake Chake" ~ "Chakechake District Hospital",
    District_facility == "Gairo" ~ "Uhuru Health Center",
    District_facility == "Kilombero" ~ "Kibaoni Health Center",
    District_facility == "Kilosa" ~ "Kilosa District Hospital",
    District_facility == "Kilwa" ~ "Kinyonga District Hospital",
    District_facility == "Lindi" ~ "Nyangao Hospital",
    District_facility == "Liwale" ~ "Liwale District Hospital",
    
    District_facility == "Malinyi" ~ "Lugala Hospital",
    District_facility == "Masasi" ~ "Masasi Hospital", # Note: Mkomaindo Hospital == Masasi Hospital, but data has both so this has been addressed in data cleaning
    District_facility == "Morogoro" ~ "Uhuru Health Center",  # Uhuru Health Center has more patients
    
    District_facility == "Mtwara" ~ "Ligula Regional Hospital",
    District_facility == "Musoma" ~ "Nyasho Health Center", # Nyasho Health Center has more patients
    District_facility == "Mvomero" ~ "Uhuru Health Center",
    District_facility == "Nachingwea" ~ "Nachingwea District Hospital",
    District_facility == "Nanyumbu" ~ "Nanyumbu District Hospital",
    District_facility == "Newala" ~ "Newala District Hospital",
    
    District_facility == "Rorya" ~ "Shirati Hospital",
    District_facility == "Ruangwa" ~ "Ruangwa District Hospital",
    District_facility == "Tandahimba" ~ "Tandahimba District Hospital",
    District_facility == "Tarime" ~ "Tarime District Hospital",
    District_facility == "Ulanga" ~ "Ulanga District Hospital",
    TRUE ~ NA ),
    is_central_hosp = "yes"
  )

# all facilities (despite stocking PEP or not)
all_facilities <- ibcm_bite_data %>%
  group_by(District_facility) %>%
  dplyr::filter(!District_facility == "NANA")%>%
  summarise(no_of_facilities = n_distinct(new_FACILITY)) %>%
  left_join(., ibcm_districts_shp, by = c("District_facility" = "dist_nm")) %>%
  dplyr::select(District_facility, no_of_facilities, Popultn) %>%
  st_drop_geometry()



# only facilities that stock PEP
# # stock PEP
# facilities_PEP_status <- read.csv("./data/clean/health_facilities_Revised30thMay.csv") %>%
#   dplyr::filter(!Region_facility == "Arusha",
#                 !District_facility == "Serengeti",
#                 `Is.PEP.available` == "Available") 


# clinics not in PEP status file
#unique(ibcm_bite_data$new_FACILITY[!ibcm_bite_data$new_FACILITY %in% facilities_PEP_status$name_facility])

# # bites
# ibcm_bite_data %>%
#   dplyr::filter(!new_FACILITY %in% facilities_PEP_status$name_facility) %>%
#   summarise(sum_bites = sum(n))


PEP_facilities <- ibcm_bite_data %>%
  group_by(District_facility) %>%
  summarise(no_of_facilities = length(unique(new_FACILITY)),
            facilities2020 = length(unique(new_FACILITY[Year <= 2019])),
            facilities2023 = length(unique(new_FACILITY[Year > 2019]))) %>%
  left_join(., ibcm_districts_shp, by = c("District_facility" = "dist_nm"))  %>%
  dplyr::select(District_facility, no_of_facilities, facilities2020, facilities2023, Popultn) %>%
  st_drop_geometry()


# decentralization
decentralization_data <- ibcm_bite_data %>%
  group_by(District_facility, new_FACILITY) %>%
  summarise(Total_Patients = sum(n), 
            Total_Patients2020 = sum(n[Year <= 2019]),
            Total_Patients2023 = sum(n[Year > 2019]), .groups = 'drop'
            ) %>%
  left_join(., ibcm_districts_central_hosps, by = c("District_facility", "new_FACILITY")) %>%
  group_by(District_facility) %>%
  dplyr::mutate(Total_Patients_Per_District = sum(Total_Patients),
                Total_Patientsdist2020 = sum(Total_Patients2020),
                Total_Patientsdist2023 = sum(Total_Patients2023)
                ) %>%
  ungroup() %>%
  dplyr::mutate(PCentral = ifelse(is_central_hosp == "yes", round(Total_Patients/Total_Patients_Per_District,2), "NA"),
                PCentral2020 = ifelse(is_central_hosp == "yes", round(Total_Patients2020/Total_Patientsdist2020,2), "NA"),
                PCentral2023 = ifelse(is_central_hosp == "yes", round(Total_Patients2023/Total_Patientsdist2023,2), "NA")
                )%>%
  drop_na()



data_decentralization <- PEP_facilities %>%
  left_join(., decentralization_data, by =c("District_facility")) %>%
  dplyr::select(-c(new_FACILITY, is_central_hosp, Total_Patients)) %>%
  dplyr::rename(
    District = District_facility,
    Facilities = no_of_facilities,
    "Total patients" = Total_Patients_Per_District,
    Population = Popultn
  )

data_decentralization_table <- flextable(data_decentralization)
data_decentralization_table

save_as_docx(data_decentralization_table, path = "./manuscript/decentralization_tmp.docx")

# Morogoro Regional Hospital - 63
    # Uhuru Health Center - 741

# Models ##############
source("./R/HelperFun.R")


# pt_data_to_vials in Helper. R

pt_data_to_annual_vials(no_of_months =13, regimen = IPC, mydata = complete_data, PCentral = 0.5, facilities = 5, 
                  district = 'Bunda', N = 100, PEP_admin = "ID", vial_size = 1, wastage = 0, pep_compliance = 1)


# Create a df to define different (de)centralization levels
create_decentral_df <- function() {
  # Define the columns
  decentralization <- c("low", "moderate", "high")
  facilities <- c(1, 4, 8)
  pCentral <- c(1, 0.7, 0.5)
  
  # Create the data frame
  decentral_df <- data.frame(decentralization, facilities, pCentral)
  
  return(decentral_df)
}

# Call the function to create the data frame
decentral_df <- create_decentral_df()

# View the data frame
print(decentral_df)



# Function to calculate vials per district with decentralization
vials_per_dist_decentralization <- function(decentral_df, no_of_months =13, schedule, mydata, district, N, PEP_admin, Vial_size, wastage, pep_compliance = 1) {
  results <- list() # Initialize an empty list to store results
  
  # Loop through each row of the decentral_df data frame
  for (i in 1:nrow(decentral_df)) {
    facilities <- decentral_df$facilities[i]
    pcentral_value <- decentral_df$pCentral[i]
    
    result <- (pt_data_to_annual_vials(no_of_months = no_of_months, regimen = schedule, mydata = mydata, PCentral = pcentral_value, 
                                facilities = facilities, district = district, N = N, PEP_admin = PEP_admin, 
                                vial_size = Vial_size, wastage = wastage, pep_compliance = pep_compliance))$total_vials
    
    # Store the result in the list
    results[[paste("PCentral", pcentral_value, sep = "_")]] <- result
  }
  
  combined_results <- do.call(cbind, results)
  
  return(combined_results)
}



# A low throughput district
  # IPC
Liwale_central_results_IPC <- vials_per_dist_decentralization(
  decentral_df = decentral_df, schedule = IPC, 
  mydata = complete_data, district = 'Liwale', 
  N = 1000, PEP_admin = "ID", Vial_size = 1, wastage = 0,
  pep_compliance = 1
  )

  # essen4
Liwale_central_results_essen4 <- vials_per_dist_decentralization(
  decentral_df = decentral_df, schedule = essen4, 
  mydata = complete_data, district = 'Liwale', 
  N = 1000, PEP_admin = "IM", Vial_size = 1, wastage = 0
  , pep_compliance = 1
)


# A high throughput district
  # IPC
Ulanga_central_results_IPC <- vials_per_dist_decentralization(
  decentral_df = decentral_df, schedule = IPC, 
  mydata = complete_data, district = 'Ulanga', 
  N = 1000, PEP_admin = "ID", Vial_size = 1, wastage = 0, pep_compliance = 1
)


  # essen4
Ulanga_central_results_essen4 <- vials_per_dist_decentralization(
  decentral_df = decentral_df, schedule = essen4, 
  mydata = complete_data, district = 'Ulanga', 
  N = 1000, PEP_admin = "IM", Vial_size = 1, wastage = 0, pep_compliance = 1
)


## Combine datasets
  # Create a combined dataset for Liwale district
Liwale_combined <- data.frame(
  Iteration = 1:nrow(Liwale_central_results_IPC), 
  PCentral_1_IPC = Liwale_central_results_IPC[,"PCentral_1"],
  PCentral_0.7_IPC = Liwale_central_results_IPC[,"PCentral_0.7"],
  PCentral_0.5_IPC = Liwale_central_results_IPC[,"PCentral_0.5"],
  PCentral_1_essen4 = Liwale_central_results_essen4[,"PCentral_1"],
  PCentral_0.7_essen4 = Liwale_central_results_essen4[,"PCentral_0.7"],
  PCentral_0.5_essen4 = Liwale_central_results_essen4[,"PCentral_0.5"]
)

  # Create a combined dataset for Ulanga district
Ulanga_combined <- data.frame(
  Iteration = 1:nrow(Ulanga_central_results_IPC),  
  PCentral_1_IPC = Ulanga_central_results_IPC[,"PCentral_1"],
  PCentral_0.7_IPC = Ulanga_central_results_IPC[,"PCentral_0.7"],
  PCentral_0.5_IPC = Ulanga_central_results_IPC[,"PCentral_0.5"],
  PCentral_1_essen4 = Ulanga_central_results_essen4[,"PCentral_1"],
  PCentral_0.7_essen4 = Ulanga_central_results_essen4[,"PCentral_0.7"],
  PCentral_0.5_essen4 = Ulanga_central_results_essen4[,"PCentral_0.5"]
)



## Convert each combined dataset into long format
  # Convert Liwale_combined to long format
Liwale_long <- Liwale_combined %>%
  pivot_longer(cols = starts_with("PCentral"), names_to = c("Label", "PCentral", "Regimen"), names_sep = "_", values_to = "Total_Vials") %>%
  dplyr::mutate(District = "Low throughput district")

  # Convert Ulanga_combined to long format
Ulanga_long <- Ulanga_combined %>%
  pivot_longer(cols = -Iteration, names_to = c("Label", "PCentral", "Regimen"), names_sep = "_", values_to = "Total_Vials") %>%
  dplyr::mutate(District = "High throughput district")


# Combine both long datasets
combined_long <- bind_rows(Liwale_long, Ulanga_long) %>%
  dplyr::mutate(Label = case_when(
    PCentral == "1" ~ "None",
    PCentral == "0.7" ~ "Moderate",
    PCentral == "0.5" ~ "High"
  ))




