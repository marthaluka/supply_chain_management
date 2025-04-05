

# 1. Explore seasonality #########

# data
## Per Year #####
# Step 1: Extract Year and Month
ibcm_bites_mon2 <- ibcm_bites_mon %>%
  dplyr::filter(Year_month > as.Date("2018-12-01"))%>%
  mutate(
    Year = year(Year_month),
    Month = month(Year_month, label = TRUE, abbr = TRUE)
  )

# Step 2: Get the peak bite month for each district and year
peak_months <- ibcm_bites_mon2 %>%
  group_by(District_facility, Year) %>%
  filter(total_n == max(total_n)) %>%
  slice(1) %>%  # in case of ties, take the first one
  ungroup()

# Step 3: Count frequency of each month
month_counts <- peak_months %>%
  count(Month)

# Step 4: Plot the histogram
ggplot(month_counts, aes(x = Month, y = n)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Peak Bite Months (per year per district)",
    x = "Month",
    y = "Number of Times Month Had Peak Bites"
  ) +
  theme_classic()


## Entire study period #####
# Step 2: Identify the peak month (overall) for each district
peak_months_overall <- ibcm_bites_mon2 %>%
  group_by(District_facility) %>%
  filter(total_n == max(total_n)) %>%
  slice(1) %>%  # Handle ties by selecting the first occurrence
  ungroup()

# Step 3: Count how often each month appears as the overall peak
overall_month_counts <- peak_months_overall %>%
  count(Month)

# Step 4: Plot the histogram
ggplot(overall_month_counts, aes(x = Month, y = n)) +
  geom_col(fill = "darkorange") +
  labs(
    title = "Peak Bite Month Per District (Overall)",
    x = "Month with Highest Reported Bites",
    y = "Number of Districts"
  ) +
  theme_classic()


# 2. Decentralization and increased demand #########

# 10 and 25% increase

# A low throughput district
# data as is
Liwale_central_results_IPC
# 10% increase in patients
Liwale_central_results_IPC2 <- vials_per_dist_decentralization(
  decentral_df = decentral_df, schedule = IPC, 
  mydata = complete_data, district = 'Liwale', 
  N = 1000, PEP_admin = "ID", Vial_size = 1, wastage = 0,
  pep_compliance = 1, multiplier = 1.1
)

# 25% increase in patients
Liwale_central_results_IPC3 <- vials_per_dist_decentralization(
  decentral_df = decentral_df, schedule = IPC, 
  mydata = complete_data, district = 'Liwale', 
  N = 1000, PEP_admin = "ID", Vial_size = 1, wastage = 0,
  pep_compliance = 1, multiplier = 1.25
)


# A high throughput district
# data as is
Ulanga_central_results_IPC

# 10% increase in patients
Ulanga_central_results_IPC2 <- vials_per_dist_decentralization(
  decentral_df = decentral_df, schedule = IPC, 
  mydata = complete_data, district = 'Ulanga', 
  N = 1000, PEP_admin = "ID", Vial_size = 1, wastage = 0, 
  pep_compliance = 1, multiplier = 1.1
)

# 25% increase in patients
Ulanga_central_results_IPC3 <- vials_per_dist_decentralization(
  decentral_df = decentral_df, schedule = IPC, 
  mydata = complete_data, district = 'Ulanga', 
  N = 1000, PEP_admin = "ID", Vial_size = 1, wastage = 0, 
  pep_compliance = 1, multiplier = 1.25
)



## Combine datasets
# Create a combined dataset for Liwale district
Liwale_combined2 <- data.frame(
  Iteration = 1:nrow(Liwale_central_results_IPC), 
  PCentral_1_IPC = Liwale_central_results_IPC[,"PCentral_1"],
  PCentral_0.7_IPC = Liwale_central_results_IPC[,"PCentral_0.7"],
  PCentral_0.5_IPC = Liwale_central_results_IPC[,"PCentral_0.5"],
  PCentral_1_IPC2 = Liwale_central_results_IPC2[,"PCentral_1"],
  PCentral_0.7_IPC2 = Liwale_central_results_IPC2[,"PCentral_0.7"],
  PCentral_0.5_IPC2 = Liwale_central_results_IPC2[,"PCentral_0.5"],
  PCentral_1_IPC3 = Liwale_central_results_IPC3[,"PCentral_1"],
  PCentral_0.7_IPC3 = Liwale_central_results_IPC3[,"PCentral_0.7"],
  PCentral_0.5_IPC3 = Liwale_central_results_IPC3[,"PCentral_0.5"]
)

# Create a combined dataset for Ulanga district
Ulanga_combined2 <- data.frame(
  Iteration = 1:nrow(Ulanga_central_results_IPC),  
  PCentral_1_IPC = Ulanga_central_results_IPC[,"PCentral_1"],
  PCentral_0.7_IPC = Ulanga_central_results_IPC[,"PCentral_0.7"],
  PCentral_0.5_IPC = Ulanga_central_results_IPC[,"PCentral_0.5"],
  PCentral_1_IPC2 = Ulanga_central_results_IPC2[,"PCentral_1"],
  PCentral_0.7_IPC2 = Ulanga_central_results_IPC2[,"PCentral_0.7"],
  PCentral_0.5_IPC2 = Ulanga_central_results_IPC2[,"PCentral_0.5"],
  PCentral_1_IPC3 = Ulanga_central_results_IPC3[,"PCentral_1"],
  PCentral_0.7_IPC3 = Ulanga_central_results_IPC3[,"PCentral_0.7"],
  PCentral_0.5_IPC3 = Ulanga_central_results_IPC3[,"PCentral_0.5"]
)



## Convert each combined dataset into long format
# Convert Liwale_combined to long format
Liwale_long2 <- Liwale_combined %>%
  pivot_longer(cols = starts_with("PCentral"), names_to = c("Label", "PCentral", "Regimen"), names_sep = "_", values_to = "Total_Vials") %>%
  dplyr::mutate(District = "Low throughput district")

# Convert Ulanga_combined to long format
Ulanga_long2 <- Ulanga_combined %>%
  pivot_longer(cols = -Iteration, names_to = c("Label", "PCentral", "Regimen"), names_sep = "_", values_to = "Total_Vials") %>%
  dplyr::mutate(District = "High throughput district")


# Combine both long datasets
combined_long2 <- bind_rows(Liwale_long2, Ulanga_long2) %>%
  dplyr::mutate(Label = case_when(
    PCentral == "1" ~ "None",
    PCentral == "0.7" ~ "Moderate",
    PCentral == "0.5" ~ "High"
  ),
  Regimen = case_when(
    Regimen == "IPC" ~ "Status quo",
    Regimen == "IPC2" ~ "10% increase",
    Regimen == "IPC3" ~ "25% increase"
  )
  )


# Plot
# Set factor levels to control order in the plot
combined_long2$Regimen <- factor(combined_long2$Regimen, levels = c("Status quo", "10% increase", "25% increase"))

# Plot
fig3d <- ggplot(combined_long2, aes(x = Label, y = Total_Vials, fill = Regimen)) +
  geom_boxplot() +
  facet_wrap(vars(fct_rev(District)), scales = "free_y", ncol = 2) +
  labs(
    x = "Decentralization",
    y = "Annual vials"
  ) +
  theme_classic() +
  scale_fill_manual(values = c(
    "Status quo" = "#1f77b4",
    "10% increase" = alpha("#1f77b4", 0.6),
    "25% increase" = alpha("#1f77b4", 0.3)
  )) +
  scale_x_discrete(limits = c("None", "Moderate", "High")) +
  theme(
    legend.position = "top",
    strip.background = element_blank(),
    legend.title = element_blank()
  ) +
  ylim(0, NA)


# some calcs for text

sq <- median(combined_long2 %>%
  dplyr::filter(Regimen == "Status quo" & District == "Low throughput district") %>%
  dplyr::pull("Total_Vials")); sq

ten_increase <- median(combined_long2 %>%
                         dplyr::filter(Regimen == "10% increase" & District == "Low throughput district") %>%
                         dplyr::pull("Total_Vials")); ten_increase

twentyfive_increase <- median(combined_long2 %>%
                         dplyr::filter(Regimen == "25% increase" & District == "Low throughput district") %>%
                         dplyr::pull("Total_Vials")); twentyfive_increase

ten_increase/sq
twentyfive_increase/ sq



sq2 <- median(combined_long2 %>%
               dplyr::filter(Regimen == "Status quo" & District == "High throughput district") %>%
               dplyr::pull("Total_Vials")); sq2

ten_increase2 <- median(combined_long2 %>%
                         dplyr::filter(Regimen == "10% increase" & District == "High throughput district") %>%
                         dplyr::pull("Total_Vials")); ten_increase2

twentyfive_increase2 <- median(combined_long2 %>%
                                dplyr::filter(Regimen == "25% increase" & District == "High throughput district") %>%
                                dplyr::pull("Total_Vials")); twentyfive_increase2


ten_increase2/sq2

twentyfive_increase2/ sq2


# 3. Restocking quartely -- as per IVD current practice ##########

monthly_to_quarterly <- function(mat) {
  num_months <- ncol(mat)
  num_quarters <- ceiling(num_months / 3)
  
  quarterly_mat <- matrix(0, nrow = nrow(mat), ncol = num_quarters)
  
  for (i in seq_len(num_quarters)) {
    cols <- ((i - 1) * 3 + 1):min(i * 3, num_months)
    quarterly_mat[, i] <- rowSums(mat[, cols, drop = FALSE])
  }
  
  return(quarterly_mat)
}
