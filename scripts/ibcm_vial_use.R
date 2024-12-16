


# load packages

require(pacman)
pacman::p_load(tidyverse, # data manipulation
               zoo, # dates
               lubridate, # more date manipulation
               flextable, # table presentation
               RColorBrewer, # colours
               ggridges, # ridges plot
               patchwork, # combine plots
               cowplot, # dynamic combine plots
               fitdistrplus,  # fit distributions of bite data
               sf, # spatial manipulation
               leaflet, # leaflet maps
               scales, # numbers aesthetics (commas)
               viridis # color palette
)





# Custom theme for Lancet-style plots
theme_lancet <- function() {
  theme_minimal() +
    theme(
      panel.grid = element_blank(), # Remove grid lines
      panel.border = element_blank(), # Remove panel border
      axis.line.x = element_line(linewidth = 0.5, color = "black"), # Add x-axis line
      axis.line.y = element_line(linewidth = 0.5, color = "black"), # Add y-axis line
      axis.ticks = element_line(color = "black", linewidth = 0.5),  # Add axis ticks
      axis.ticks.length = unit(2, "pt"), # Adjust tick length
      axis.text = element_text(size = 10), # Adjust axis text size
      axis.title = element_text(size = 12), # Adjust axis title size
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5), # Center and bold plot title
      legend.position = "right", # Place legend on the right
      legend.key = element_blank(), # Remove legend background
      legend.text = element_text(size = 10) # Adjust legend text size
    )
}


# data filtering script

source("./R/data_filtering.R")

# 1. Data at a glance ########

head(disp_out)
head(drop_tmp_PEP)
head(drop_both_dispAndtmpPEP)


ibcm_bite_data <- drop_both_dispAndtmpPEP


# when did districts start collecting data? (between June-Aug 2018)
dist_start <- read.csv("./data/clean/health_facilities_IBCM_corrected_ER.csv") %>%
  dplyr::mutate(name_facility = trimws(name_facility),
                date_added = as.yearmon((Date_added_facility), "%b-%y"),
                District_facility = dplyr::recode(District_facility,
                                                  'Mtwara Urban' = 'Mtwara',
                                                  'Mtwara Rural' = 'Mtwara',
                                                  'Masasi Township Authority' = 'Masasi',
                                                  'Lindi Urban' = 'Lindi',
                                                  'Lindi Rural' = 'Lindi',
                                                  'Tarime District Council' = 'Tarime',
                                                  'Morogoro Urban' = 'Morogoro',
                                                  'Musoma Municipal' = 'Musoma' )
                ) %>%
  dplyr::filter(!Region_facility == "Arusha") %>%
  # pick start date for a given district (date first clinic joined ibcm is start date for district)
  dplyr::select(District_facility, date_added, Region_facility) %>%
  group_by(District_facility) %>%
  slice_min(date_added) %>%
  ungroup() %>%
  distinct()



# Apply the function to filter ibcm_bites
ibcm_bites_mon <- ibcm_bite_data %>%
  group_by(District_facility, Year_month) %>%
  summarise(total_n = sum(n))  %>%
  ungroup()


#  Fill any missing bite patient values within the time scale with zero i.e., treat missing data == zero bites

# Join ibcm_bites_mon with earliest_dates
ibcm_bites_mon <- ibcm_bites_mon %>%
  # left_join(dist_start, by = c("District_facility"))  %>%
  dplyr::mutate(
    #date_added = as.Date(as.yearmon(date_added, "%b %Y")), 
    date_added = as.Date('01/06/2018', "%d/%m/%Y"),
    Year_month = as.Date(as.yearmon(Year_month, "%b %Y"))
  ) 


# Generate a complete sequence of months for each District_facility
latest_entry <- as.Date(max(ibcm_bite_data$Year_month))

complete_data<- ibcm_bites_mon %>%
  drop_na() %>%
  dplyr::mutate(dist = District_facility)%>%
  group_by(dist) %>%
  complete(District_facility, Year_month = seq(min(date_added), latest_entry, by = "month")) %>%
  replace_na(list(total_n = 0)) %>%
  ungroup() %>%
  dplyr::filter(!District_facility == "NANA")


# Calculate summary statistics per district
summary_stats <- complete_data %>%
  group_by(District_facility) %>%
  summarise(
    Zero_Months = sum(total_n == 0), # total months a district has been in ibcm register
    Total_Months = (as.yearmon(latest_entry) - as.yearmon(min(Year_month)))*12, # total months a district has been in ibcm register
    Prop_Months_Zero = round(Zero_Months/ Total_Months, 2),
    Range = paste0(min(total_n), ",", max(total_n)),
    Mean = round(mean(total_n),2),
    Median = median(total_n),
    Percentile_95 = paste(round(quantile(total_n, probs = c(0.025, 0.975)), 2), collapse = ", "),
    Surge_factor = round(quantile(total_n, probs = 0.975)/Mean, 2), # surge factor
    Total_reported = sum(total_n)) %>%
  dplyr::select(-c(Zero_Months, Total_Months)) 
# %>%
#   dplyr::filter(Mean > 1) # this drops Masasi and Newala (clinics who on average attend to less than 1 person a month)


## deaths :(
deaths_df <- read.csv("./data/clean/deaths_patient_district_monthly_2018-2023.csv") %>%
  dplyr::filter(!REGION == "Arusha",
                !DISTRICT == "Serengeti") %>%
  group_by(DISTRICT) %>%
  summarise(total_deaths = sum(n))

# Combine with summary table summary_stats

summary_stats <- summary_stats %>%
  left_join(., deaths_df, by = c("District_facility"="DISTRICT")) %>%
  dplyr::mutate(total_deaths = ifelse(is.na(total_deaths), 0, total_deaths))


# Create final row with desired values
final_row <- tibble(
  District_facility = "All/ Total",
  Prop_Months_Zero = round(sum(complete_data$total_n == 0)/ sum(complete_data$total_n),2),
  Range = paste0(min(complete_data$total_n), ",", max(complete_data$total_n)), 
  Mean = round(mean(complete_data$total_n),2), 
  Median = median(complete_data$total_n), 
  Percentile_95 = paste(round(quantile(complete_data$total_n, probs = c(0.025, 0.975)), 2), collapse = ", "), 
  Surge_factor = round(quantile(complete_data$total_n, probs = 0.975)/Mean, 2), # surge factor
  Total_reported = sum(complete_data$total_n),
  total_deaths = sum(summary_stats$total_deaths)
)

summary_stats2 <- bind_rows(summary_stats, final_row) %>%
  dplyr::select(
    District_facility, Mean, Median, Range, Percentile_95, Prop_Months_Zero,
    Surge_factor, Total_reported, total_deaths
  ) %>%
  dplyr::rename(
    District = District_facility,
    `% months with zero bites` = Prop_Months_Zero,
    `95% percentile` = Percentile_95,
    `Surge factor` = Surge_factor,
    `Total bites reported` = Total_reported,
    `Total deaths` = total_deaths
  )



# 2. Monthly bite distributions $############

# Calculate the 95% quantiles for each District
quantiles_data <- complete_data %>%
  group_by(District_facility) %>%
  summarise(Lower = quantile(total_n, probs = 0.025),
            Upper = quantile(total_n, probs = 0.975))

# Join the quantiles back to the complete_data
complete_data <- complete_data %>%
  left_join(quantiles_data, by = "District_facility")


# Plot with conditional coloring (free scale)
ggplot(complete_data, aes(x = total_n, fill = ifelse(total_n < Lower | total_n > Upper, "Outside 95%", "Inside 95%"))) +
  geom_histogram(binwidth = 1, color = "black") +
  facet_wrap(~ District_facility, scales = "free") +
  labs(title = "A",
       x = "Total Monthly Bites",
       y = "Frequency") +
  scale_fill_manual(values = c("Inside 95%" = "#1f77b4", "Outside 95%" = "red")) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  geom_vline(data = complete_data %>%
               group_by(District_facility) %>%
               summarize(Upper = max(Upper)), 
             aes(xintercept = Upper), 
             color = "red", linetype = "dashed")


# Plot with conditional coloring (fixed scaled)
ggplot(complete_data, aes(x = total_n, fill = ifelse(total_n < Lower | total_n > Upper, "Outside 95%", "Inside 95%"))) +
  geom_histogram(binwidth = 1, color = "black") +
  facet_wrap(~ District_facility, scales = "fixed") +
  labs(title = "B",
       x = "Monthly Bites",
       y = "Frequency") +
  scale_fill_manual(values = c("Inside 95%" = "#1f77b4", "Outside 95%" = "red")) +
  theme_minimal()+
  theme(legend.position = "top",
        legend.title = element_blank())+
  geom_vline(data = complete_data %>%
               group_by(District_facility) %>%
               summarize(Upper = max(Upper)), 
             aes(xintercept = Upper), 
             color = "red", linetype = "dashed") 


# 3. Seasonality ########
# Add a 'Month' and 'Year' columns
complete_data <- complete_data %>%
  dplyr::mutate(
    Month = month(Year_month, label = TRUE, abbr = TRUE),  # Extract month as an abbreviated label
    Year = factor(year(Year_month))  # Extract year and convert to factor
  )

# Plot - with different lines for each year
by_dist_plot <- complete_data %>% 
  # dplyr::mutate(District_facility = dplyr::recode(District_facility,
  #                                        "Tarime District Council" = "Tarime DC",
  #                                        "Masasi Township Authority" = "Masasi Township"))%>%
  ggplot(., aes(x = Month, y = total_n, group = Year, color = Year)) +
  geom_line() +
  facet_wrap(~ District_facility, scales = "free_y") +
  labs(#title = "A. Monthly Bites per District by Year",
    x = "Month",
    y = "Monthly Bites",
    color = "Year") +
  scale_x_discrete(breaks = month.abb[c(1, 4, 7, 10)], labels = month.abb[c(1, 4, 7, 10)]) + 
  theme_lancet() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank()) 


pdf("./manuscript/SupplementaryFig1.pdf", width = 8, height = 7)
by_dist_plot
dev.off()

# All districts

all_districts <- complete_data %>%
  group_by(Year_month) %>%
  summarise(Bites = sum(total_n)) %>%
  dplyr::mutate(
    Month = month(Year_month, label = TRUE, abbr = TRUE),  # Extract month as an abbreviated label
    Year = factor(year(Year_month))  # Extract year and convert to factor
  )


# # Plot 
# B <- ggplot(data = all_districts, aes(x = Month, y = Bites, group = Year, color = Year)) +
#   geom_line() +
#   labs(#title = "B. Monthly bites all districts by year",
#     x = "Month",
#     y = "Monthly Bites",
#     color = "Year") +
#   scale_x_discrete(breaks = month.abb[c(1, 4, 7, 10)], labels = month.abb[c(1, 4, 7, 10)]) +
#   theme_bw() +
#   theme(legend.position = "right",  # Remove legend
#         axis.text.x = element_text(angle = 45, hjust = 1))


# design <- "AAA
#            AAA
#            BB#"

# seasonality_plot <- A+B+
#   plot_layout(design = design)  +
#   plot_annotation(tag_levels = 'A')


# 
# pdf("./manuscript/SupplementaryFig1.pdf", width = 9, height = 10)
# seasonality_plot
# dev.off()


# 4. Spatial (map) #####

# read shapefile
#tz_shp <- st_read(dsn="./data/districts_2022_population_HDR/", layer="districts_2022_population_HDR")
tz_shp <- st_read(dsn="./data/new_shp_files_Ellie/", layer="tz_districts_2022") %>% 
  dplyr::mutate(District_facility = dist_nm)
  
  
  # shapefile of protected areas
  ##  projection file is missing, so specify - it’s UTM zone 37S.
tz_protect_areas <- read_sf("./data/ProtectedAreas/TZprotected_areas.shp")
st_crs(tz_protect_areas) <- "+proj=utm +zone=37 +south"

ggplot() +
  geom_sf(data = tz_protect_areas, color = "#FAFAFA", alpha= 1)


# rename for consistency
shapefile_bite_data <- complete_data %>%
  group_by(District_facility) %>%
  summarise(sum_bites = sum(total_n)) %>%
  ungroup() 


# extract districts in analysis
ibcm_districts <- unique(shapefile_bite_data$District_facility)

# check naming consistencies
ibcm_districts[!ibcm_districts %in% tz_shp$dist_nm] 

# Filter for selected districts
ibcm_districts_shp <- tz_shp %>% 
  dplyr::filter(dist_nm %in% ibcm_districts) %>% 
  left_join(., shapefile_bite_data, by = c( "District_facility")) %>% 
  dplyr::mutate(
    dist_label = case_when(
      dist_nm == "Liwale" ~ "Low throughput",
      dist_nm == "Ulanga" ~ "High throughput",
      .default = "Other IBCM district"
    ),
    bites_per_capita = (sum_bites/Popultn)*100000
    )

# high_low_distr <- tz_shp %>% 
#   dplyr::filter(dist_nm %in% c("Liwale", "Ulanga")) %>% 
#   left_join(., shapefile_bite_data, by = c( "District_facility")) %>% 
#   dplyr::mutate(bites_per_capita = (sum_bites/Popultn)*100000)


no_borders<- tz_shp %>% 
  dplyr::filter(!dist_nm %in% ibcm_districts)

my_palette <- colorRampPalette(c("#5553c7", "white", "#d6b7f5"))


plotA <- ggplot() +
  #geom_sf(data = tz_shp, fill = NA) +         # country shapefile
  geom_sf(data = no_borders, color = "grey", fill="#FAFAFA", alpha= 1) +  # background/ unwanted districts
  geom_sf(data = ibcm_districts_shp, aes(fill = dist_label)) +     # IBCM ditricts
  geom_sf(data = tz_protect_areas, color = "grey", fill = "grey", alpha= 1) + # protected areas
  #geom_sf(data = high_low_distr, aes(fill = sum_bites)) +
  #scale_fill_gradientn(colors = my_palette(100), name = "Total bites", labels = scales::comma) +
  scale_fill_manual(values = c("Low throughput" = "#5553c7", 
                                "High throughput" = "#d6b7f5",
                                "Other IBCM district" = "navajowhite",
                                "Protected area" = "grey90")
                    )+
  theme(plot.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        legend.position = 'top',
        legend.title = element_blank())


plotA


tz_bites_per_capita_map <- ggplot() +
  #geom_sf(data = tz_shp, fill = NA) +         # country shapefile
  geom_sf(data = no_borders, color = "grey", fill="#FAFAFA", alpha= 1) +  # background/ unwanted districts
  geom_sf(data = ibcm_districts_shp, aes(fill = bites_per_capita)) +     # IBCM ditricts
  geom_sf(data = tz_protect_areas, color = "grey", fill = "grey", alpha= 1) + # protected areas
  scale_fill_distiller(palette = "YlOrRd", name = "Bites per 100k", labels = scales::comma, direction = 1) +
  #geom_sf(data = high_low_distr, aes(fill = sum_bites)) +
  #scale_fill_gradientn(colors = my_palette(100), name = "Total bites", labels = scales::comma) +
  theme(plot.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        legend.position = 'top')

tz_bites_per_capita_map

# 4. Distributions  ##########
# counts per month

complete_data %>%
  group_by(Year_month)%>%
  summarise(bites = sum(total_n)) %>%
  ggplot(., aes(x = Year_month, y = bites)) +
  geom_col(, fill = "#1f77b4")+
  labs(x= NULL,
       y= "Bite patients")+
  theme_bw()

monthly_ave <- complete_data %>%
  group_by(Year_month) %>%
  summarize(monthly_ave = mean(total_n))


# Ensure Year_month is in Date format and rename District_facility
plotB <- complete_data %>%
  dplyr::mutate(Year_month = as.Date(Year_month),
                District_facility = dplyr::recode(District_facility,
                                                  "Liwale" = "Low throughput district",
                                                  "Ulanga" = "High throughput district")
                ) %>%
  # Filter data for the two districts of interest
  dplyr::filter(District_facility %in% c("Low throughput district", "High throughput district")) %>%
  ggplot(aes(x = Year_month, y = total_n, fill = District_facility, alpha = District_facility)) +
    geom_col(position = "identity") +  # Overlapping bars with specific transparency
    labs(x = NULL, y = "Bite patients") +
    theme_lancet() +
    scale_fill_manual(values = c("Low throughput district" = "#5553c7", "High throughput district" = "#d6b7f5")) +
    scale_alpha_manual(values = c("Low throughput district" = 1, "High throughput district" = 0.6)) +  
    scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
    theme(legend.position = 'none',
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.background = element_blank()) +
    # Add the monthly average as a geom_line
    geom_line(data = monthly_ave, aes(x = Year_month, y = monthly_ave), 
            color = "black", size = 0.8, linetype = "twodash", inherit.aes = FALSE)


# source helper functions
source("./R/HelperFun.R")

# hist of monthly bites overlayed with Neg binomial########
  ## Define modified function for two districts with support values as argument
plot_two_districts_fit <- function(data, N = 1000, district1 = "Liwale", district2 = "Ulanga", 
                                   label1 ="Low throughput district", 
                                   label2 ="High throughput district", support = 0:70, binwidth = 1) {
  # Filter data for each district
  district1_data <- data %>%
    dplyr::filter(District_facility == district1)
  
  district2_data <- data %>%
    dplyr::filter(District_facility == district2)
  
  # Fit negative binomial distribution for each district
  fit1 <- fitdist(district1_data$total_n, "nbinom")
  fit2 <- fitdist(district2_data$total_n, "nbinom")
  
  # Generate fitted values for each district based on support argument
  fitted_values1 <- dnbinom(support, mu = fit1$estimate["mu"], size = fit1$estimate["size"])
  fitted_values2 <- dnbinom(support, mu = fit2$estimate["mu"], size = fit2$estimate["size"])
  
  # Calculate means and confidence intervals using bootstrap for each district
  means1 <- replicate(N, mean(sample(district1_data$total_n, size = nrow(district1_data), replace = TRUE)))
  means2 <- replicate(N, mean(sample(district2_data$total_n, size = nrow(district2_data), replace = TRUE)))
  
  human_bites_CI1 <- quantile(means1, c(0.025, 0.975))
  human_bites_CI2 <- quantile(means2, c(0.025, 0.975))
  
  # Create data frames for fitted values for ggplot and include district labels
  fitted_data1 <- data.frame(x = support, fitted_values = fitted_values1, District = label1)
  fitted_data2 <- data.frame(x = support, fitted_values = fitted_values2, District = label2)
  
  # Combine fitted data
  combined_fitted_data <- rbind(fitted_data1, fitted_data2)
  
  # Combine district data and add district labels
  district1_data <- district1_data %>%
    dplyr::mutate(District = label1)
  
  district2_data <- district2_data %>%
    dplyr::mutate(District = label2)
  
  combined_district_data <- rbind(district1_data, district2_data)
  
  # Ensure the factor levels for District are set correctly
  combined_district_data$District <- factor(combined_district_data$District, levels = c(label1, label2))
  combined_fitted_data$District <- factor(combined_fitted_data$District, levels = c(label1, label2))
  
  # Plot using ggplot2 with specified binwidth and coloring points and lines by District
  p <- ggplot() +
    geom_point(data = combined_district_data, aes(x = total_n, y = ..density.., color = District), 
               stat = "bin", binwidth = binwidth, alpha = 0.7) +
    geom_line(data = combined_fitted_data, aes(x = x, y = fitted_values, color = District), size = 1.2) +
    scale_color_manual(values = c("Low throughput district" = "#5553c7", 
                                  "High throughput district" = "#d6b7f5")) +
    labs(x = "Monthly bites",
         y = "Density") +
    theme_lancet() +
    theme(
      strip.background = element_blank(),
      legend.position = "none"  
    )
  
  # Print the plot
  return(p)
}

# Use the modified function for both districts with a custom binwidth
plotC <- plot_two_districts_fit(data = complete_data, N = 1000, district1 = "Liwale", district2 = "Ulanga", 
                            label1 ="Low throughput district", 
                            label2 ="High throughput district", support = 0:70, binwidth = 2)
plotC


# b <- complete_data %>%
#   dplyr::mutate(District_facility = dplyr::recode(District_facility,
#                                                   "Liwale" = "Low throughput district",
#                                                   "Ulanga" = "High throughput district"),
#                 District_facility = factor(District_facility, 
#                                            levels = c("Low throughput district", "High throughput district"))) %>%
#   # Plot with updated x-axis labels and renamed districts
#   dplyr::filter(District_facility %in% c("Low throughput district", "High throughput district")) %>%
#   ggplot(aes(x = total_n, fill = District_facility)) +  # Correct fill aesthetic
#   geom_histogram(aes(y = ..count..), binwidth = 1, alpha = 0.7) + 
#   # Modify geom_density to show as a line and not shaded
#   geom_density(data = data.frame(combined_simulated_data), 
#                aes(x = simulated_value, y = ..density.. * 50), 
#                color = "red", fill = NA, size = 0.6) +  # Set `fill = NA` to avoid shading
#   facet_wrap(~ District_facility, scales = "free_y", ncol = 1) +
#   scale_y_continuous(name = "Count",
#                      sec.axis = sec_axis(~./50, name = "Density")) +  # Create secondary y-axis
#   labs(x = "Monthly Bites") +
#   scale_fill_manual(values = c("Low throughput district" = "#d6b7f5", 
#                                "High throughput district" = "#5553c7")) +
#   theme_bw() +
#   theme(
#     axis.title.y.right = element_text(color = "red"),  # Change secondary y-axis label to blue
#     legend.position = "none",
#     strip.background = element_blank()
#   )




# Export district size and mu
districts_in_df <- unique(ibcm_bites_mon$District_facility)
# Apply the extract_size_mu function to each district and combine results
size_and_mu <- purrr::map_dfr(districts_in_df, ~extract_size_mu(complete_data, .x)) %>%
  dplyr::select(district, size_estimate, mu_estimate) %>%
  dplyr::mutate(
    size_estimate = round(size_estimate, 2),
    mu_estimate = round(mu_estimate, 2)
  ) %>%
  dplyr::rename(
    "Negative binomial size" = size_estimate,
    "Negative binomial mu" = mu_estimate
  )



combined_table <- summary_stats2 %>%
  left_join(., size_and_mu, by = c("District" = "district"))

#### exported after running decentralization.R so as to incorporate decentralization numbers





# all
# all districts

# # Define modified function for monthly sum of all districts
# plot_monthly_sum_fit <- function(data, N = 1000, support = 0:70, binwidth = 1) {
#   # Summarize the total bites per month across all districts
#   monthly_sum_data <- data %>%
#     dplyr::group_by(Year_month) %>%
#     dplyr::summarise(sum_bites = sum(total_n, na.rm = TRUE)) %>%
#     dplyr::ungroup()
#   
#   # Fit negative binomial distribution for the monthly sums
#   fit <- fitdist(monthly_sum_data$sum_bites, "nbinom")
#   
#   # Generate fitted values for the monthly sums based on support argument
#   fitted_values <- dnbinom(support, mu = fit$estimate["mu"], size = fit$estimate["size"])
#   
#   # Calculate means and confidence intervals using bootstrap for monthly sums
#   means <- replicate(N, mean(sample(monthly_sum_data$sum_bites, size = nrow(monthly_sum_data), replace = TRUE)))
#   monthly_sum_CI <- quantile(means, c(0.025, 0.975))
#   
#   # Create data frame for fitted values for ggplot
#   fitted_data <- data.frame(x = support, fitted_values = fitted_values, Label = "Monthly Sum")
#   
#   # Plot using ggplot2 with custom bin size
#   p <- ggplot(monthly_sum_data, aes(x = sum_bites)) +
#     geom_histogram(aes(y = ..density..), binwidth = binwidth, fill = "grey40", alpha = 0.7, color = "grey40") +
#     geom_line(data = fitted_data, aes(x = x, y = fitted_values, color = Label), size = 1.2) +
#     scale_color_manual(values = c("Monthly Sum" = "#089669")) +
#     labs(x = "Monthly Sum of Bites",
#          y = "Density") +
#     theme_bw() +
#     theme(
#       plot.title = element_text(hjust = 0.5),  # Center the title
#       strip.background = element_blank(),
#       legend.position = "none"  # Remove legend
#     )
#   
#   # Print the plot
#   return(p)
# }
# 
# #  monthly sum of all districts
# c <- plot_monthly_sum_fit(data = complete_data, N = 1000, support = 0:250, binwidth = 10)
# c


# Surges
# 5. Can we define a surge factor?########
hist <- ggplot(data=summary_stats, aes(x = Surge_factor)) +
  geom_histogram(binwidth = 1, color = "white", fill="#1f77b4") +
  theme_lancet() +
  labs(
    x = "Surge factor",
    y = "Count")

scatter <- ggplot() +
  geom_point(data=summary_stats, aes(x = Mean, y = Surge_factor)) +
  geom_smooth(data=summary_stats, aes(x = Mean, y = Surge_factor), method = "loess", color ="#1f77b4")+
  theme_lancet() +
  labs(
    x = "Mean monthly bite patients",
    y = "Surge factor")

# Combine as an inset
plotD <- ggdraw() +
  draw_plot(scatter) + 
  draw_plot(hist, x = 0.45, y = 0.5, width = 0.5, height = 0.4)


# Convert ggplot objects to ggdraw
plotA <- ggdraw(plotA)
plotB <- ggdraw(plotB)
plotC <- ggdraw(plotC)


# Combine the plots
combined_plot <- (plotA+plotB) /(plotC+plotD) +
  plot_annotation(tag_levels = 'A')

# Display the combined plot
print(combined_plot)


pdf("./manuscript/Fig2.pdf", width = 9, height = 7)
combined_plot
dev.off()


# 6. Neighbouring districts #########
# 
# proximity <- c("Bunda", "Serengeti", "Tarime", "Tarime District Council", "Rorya", 
#                "Butiama",  "Musoma", "Musoma Municipal",  # Mara region
#                "Morogoro", "Morogoro Urban", "Kilosa","Ulanga", "Kilombero",          # Morogoro region
#                "Lindi Rural", "Lindi Urban", "Nachingwea", "Liwale", "Ruangwa", "Kilwa",      # Lindi region
#                "Mtwara Rural", "Mtwara Urban", "Masasi", "Masasi Township Authority", "Newala","Tandahimba"   # Mtwara region
# )
# 
# # Convert District_facility to a factor with levels based on proximity
# complete_data <- complete_data %>%
#   mutate(District_facility = factor(District_facility, levels = proximity))
# 
# # Plot with facets ordered by proximity
# count_bites <- ggplot(complete_data, aes(x = Year_month, y = total_n)) +
#   geom_line() +
#   facet_wrap(~ District_facility, scales = "free_y") + 
#   labs(title = "Monthly Bites per District",
#        x = "Date",
#        y = "Total Monthly Bites") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# # kernel densities
# 
# Uncount the data
# expanded_ibcm_bites_mon <- tidyr::uncount(ibcm_bites_mon, weights = total_n)
# expanded_ibcm_bites_mon <- expanded_ibcm_bites_mon %>%
#   mutate(District_facility = factor(District_facility, levels = proximity))
# 
# kernel_bites <- ggplot(expanded_ibcm_bites_mon, aes(x = Year_month, y= District_facility, fill = District_facility)) +
#   geom_density_ridges2(bandwidth=10,alpha = 0.5, scale = 2) +
#   theme_minimal()+
#   theme(legend.position = "none")+
#   labs(
#     title = "Kernel densities of bite pts per district",
#     x = "Date",
#     y = NULL
#   )
# 
# 
# out_plot <- count_bites/ kernel_bites
# 
# out_plot <- out_plot + plot_layout(heights = c(1, 2))
# 
# out_plot
# 
# 


# 7. Vial use (annual) #########

source("R/HelperFun.R")

# ibcm data


pt_data_to_annual_vials(no_of_months = 13, regimen = IPC, mydata = complete_data, PCentral = 1, facilities = 1, 
                        district = 'Bunda', N = 10, PEP_admin = "ID", vial_size = 1, wastage = 0, pep_compliance = 1)



loop_thru_districts <- function(no_of_months = 13, regimen = IPC, mydata = complete_data, PCentral = 1, facilities = 1, 
                                N = 100, PEP_admin = "ID", vial_size = 1, wastage = 0, pep_compliance = 1){
  districts <- unique(mydata$District_facility)
  num_districts <- length(districts)
  
  # Initialize output matrices
  total_vials_matrix <- matrix(nrow = num_districts, ncol = N)
  vials_per_pt_matrix <- matrix(nrow = num_districts, ncol = N)
  
  # Loop through each district
  for (i in 1:num_districts) {
    district_name <- districts[i]
    
    # Call pt_data_to_annual_vials function
    result <- pt_data_to_annual_vials(no_of_months = no_of_months, regimen = regimen, mydata = mydata, PCentral = PCentral, 
                                      facilities = facilities,  district = district_name, N = N, PEP_admin = PEP_admin, 
                                      vial_size = vial_size, wastage = wastage, pep_compliance = pep_compliance)
    
    # Store the results
    total_vials_matrix[i,] <- result$total_vials
    vials_per_pt_matrix[i,] <- result$vials_per_pt
  }
  
  # combine output
  # Each ROW is a DISTRICT
    # Each COLUMN is an ITERATION
  out <- list(total_vials_matrix,vials_per_pt_matrix)
  names(out) <- c('total_vials', 'vials_per_pt')
  return(out)
}


# Current regimen
vial_use_ibcm_essen4 <- loop_thru_districts(no_of_months = 13, mydata=complete_data, regimen=essen4, PCentral = 1, facilities = 1, 
                                            PEP_admin='IM', vial_size='1', N=1000, wastage=0, pep_compliance = 1) 

# WHO recommended
  # 0.5 vials (dropping as wont be in Gavi investment)
      # vial_use_ibcm_IPC_0.5ml <- loop_thru_districts(mydata=complete_data, regimen=IPC, PCentral = 1, facilities = 1,
      #                                                PEP_admin='ID', vial_size='0.5', N=100, wastage=0)
# 1 ml vials
vial_use_ibcm_IPC_1ml <- loop_thru_districts(no_of_months = 13, mydata=complete_data, regimen=IPC, PCentral = 1, facilities = 1,
                                             PEP_admin='ID', vial_size='1', N=1000, wastage=0, pep_compliance = 1)


# districts 
df_length <- length(unique(complete_data$District_facility))
baseline_df <- data.frame(rep(unique(complete_data$District_facility),2),c(rep('essen4', df_length),  rep('IPC', df_length)))
names(baseline_df) <- c('district', 'regimen')

# merge
total_vials_matrix <- rbind(vial_use_ibcm_essen4[[1]], vial_use_ibcm_IPC_1ml[[1]]) %>%
  bind_cols(., baseline_df)

vials_per_pt_matrix <- rbind(vial_use_ibcm_essen4[[2]], vial_use_ibcm_IPC_1ml[[2]]) %>%
  bind_cols(., baseline_df)



## Total vials
total_vials_long <- pivot_longer(total_vials_matrix, 
                                 cols = starts_with("..."), 
                                 names_to = "iter", 
                                 values_to = "vials") %>%
  dplyr::select(-c(iter))


# Calculate mean for each district under 'essen4' regimen -- directly correlates with bite pts
mean_essen4 <- total_vials_long %>%
  filter(regimen == "essen4") %>%
  group_by(district) %>%
  summarize(mean_vials = mean(vials, na.rm = TRUE)) %>%
  arrange(mean_vials)

# Create a factor with levels ordered by the mean vials
total_vials_long$district <- factor(total_vials_long$district,
                                    levels = mean_essen4$district)



## Vials per patient
vial_per_pt_long <- pivot_longer(vials_per_pt_matrix, 
                                 cols = starts_with("..."), 
                                 names_to = "iter", 
                                 values_to = "vials") %>%
  dplyr::select(-c(iter)) %>%
  dplyr::filter(vials>0)


# Create a factor with levels ordered by the same order as above
vial_per_pt_long$district <- factor(vial_per_pt_long$district,
                                    levels = mean_essen4$district)


total_vials_long %>%
  dplyr::filter(district == "Ulanga" & regimen == "IPC") %>%
  pull(vials) %>%
  mean()

a <- total_vials_long %>%
  dplyr::mutate(district = dplyr::recode(district,
                                         "Tarime District Council" = "Tarime DC",
                                         "Masasi Township Authority" = "Masasi Township")) %>%
  ggplot(aes(x = district, y = vials, fill = regimen)) +
  geom_boxplot() +
  theme_lancet() +
  labs(x = NULL,
       y = "Annual vials") +
  scale_fill_manual(values = c("IPC" = "#1f77b4", "essen4" = "#64c56e"), 
                    labels = c("IPC" = "ID", "essen4" = "IM")) +  # Custom labels 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.title = element_blank(),
        legend.position = 'top') +
  # Add dotted vertical lines after "Butiama" and "Kilombero" ( low, medium and high thruput)
  geom_vline(xintercept = which(levels(total_vials_long$district) == "Butiama") + 0.5, 
             linetype = "dashed", color = "black", size = 0.7) +
  geom_vline(xintercept = which(levels(total_vials_long$district) == "Kilombero") + 0.5, 
             linetype = "dashed", color = "black", size = 0.7)



b <- vial_per_pt_long %>%
  dplyr::mutate(district = dplyr::recode(district,
                           "Tarime District Council" = "Tarime DC",
                           "Masasi Township Authority" = "Masasi Township")) %>%
  ggplot(., aes(x = district, y = vials, fill = regimen)) +
  geom_boxplot(aes(color = regimen)) +
  theme_lancet() +
  labs(x = NULL,
       y = "Vials per patient") +
  ylim(0, 4.1) +
  # scale_fill_brewer(palette = "Accent", guide = "none") +
  scale_fill_manual(values = c("IPC" ="#1f77b4","essen4" = "#64c56e")) +
  scale_color_manual(values = c("IPC" ="black", "essen4" = "#64c56e")) + 
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  # Add dotted vertical lines after "Butiama" and "Kilombero" ( low, medium and high thruput)
  geom_vline(xintercept = which(levels(total_vials_long$district) == "Butiama") + 0.5, 
             linetype = "dashed", color = "black", size = 0.7) +
  geom_vline(xintercept = which(levels(total_vials_long$district) == "Kilombero") + 0.5, 
             linetype = "dashed", color = "black", size = 0.7)

a/b




source("./R/decentralization.R")

data_decentralization
combined_table

combined_table2 <- combined_table %>%
  left_join(., data_decentralization, by = "District")

# convert to flextable object
flextable_summary_stats <- flextable(combined_table2)

flextable_summary_stats

# Export the flextable object to an HTML file
save_as_docx(flextable_summary_stats, path = "./manuscript/Supp_Table1.docx")
write_csv(combined_table2, "./manuscript/Supp_Table1.csv")





combined_long %>%
  dplyr::filter(District == "High throughput district" & Regimen == "IPC")  %>%
  group_by(PCentral)%>%
  summarise(
    mean_vials = mean(Total_Vials, na.rm = TRUE),
    lower_ci = ceiling(quantile(Total_Vials, probs = 0.025, na.rm = TRUE)),
    upper_ci = ceiling(quantile(Total_Vials, probs = 0.975, na.rm = TRUE))
  )




# Plot box plot for both districts
d <- ggplot(combined_long, aes(x = Label, y = Total_Vials, fill = Regimen)) +
  geom_boxplot() +
  facet_wrap(vars(fct_rev(District)), scales = "free_y", ncol = 2) +
  labs(
    x = "Decentralization",
    y = "Annual vials"
  ) +
  theme_lancet() +
  scale_fill_manual(values = c("IPC" = "#1f77b4", "essen4" = "#64c56e")) +
  scale_x_discrete(limits = c("None", "Moderate", "High")) + 
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    legend.title = element_blank()
  ) +
  ylim(0, NA)


# combine
myplt <- a/b/d +
  plot_annotation(tag_levels = 'A')

pdf("./manuscript/Fig3.pdf", width = 7, height = 8)
myplt
dev.off()

# table
summ_table_vials <- total_vials_long %>% 
  group_by(district, regimen) %>%
  summarize(
    mean_vials = ceiling(mean(vials, na.rm = TRUE)), 
    median_vials = ceiling(median(vials, na.rm = TRUE)), 
    Range = paste0(min(vials), ",", max(vials)),
    lower_ci = ceiling(quantile(vials, probs = 0.025, na.rm = TRUE)),
    upper_ci = ceiling(quantile(vials, probs = 0.975, na.rm = TRUE))
  ) %>%
  pivot_wider(
    names_from = regimen, 
    values_from = c(mean_vials, median_vials, Range, lower_ci, upper_ci),
    names_sep = "_"
  ) %>%
  dplyr::select(
    district, 
    mean_vials_essen4, median_vials_essen4, Range_essen4, lower_ci_essen4, upper_ci_essen4,
    mean_vials_IPC, median_vials_IPC, Range_IPC, lower_ci_IPC, upper_ci_IPC
  )



sum(summ_table_vials$mean_vials_essen4)
sum(summ_table_vials$lower_ci_essen4)
sum(summ_table_vials$upper_ci_essen4)


sum(summ_table_vials$mean_vials_IPC)
sum(summ_table_vials$lower_ci_IPC)
sum(summ_table_vials$upper_ci_IPC)


# Create the initial flextable
ft <- flextable(summ_table_vials)

# Add custom headers - merge cells for headers
ft <- set_header_labels(ft, district = "District", mean_vials_essen4="Mean", median_vials_essen4="Median", Range_essen4="Range",
                        # mean_vials_IPC_0.5ml="Mean", median_vials_IPC_0.5ml ="Median", Range_IPC_0.5ml="Range", 
                        mean_vials_IPC="Mean", median_vials_IPC="Median", Range_IPC="Range")
# Format the table
ft <- add_header_row(
  ft, 
  values = c("", "", "", "essen4", "", "", "","", "IPC", "",""),
  colwidths = c(1, 1,1,1,1,1, 1, 1, 1, 1, 1)
)


ft <- theme_vanilla(ft)  
ft



# 7.2 Maps ###########
pacman::p_load(
  sf
)

# # only highlighting the IBCM districts used in the analysis
my_selection <- summ_table_vials$district
my_selection <- my_selection[my_selection != "All/ Total"]

# check
my_selection[!my_selection %in% tz_shp$dist_nm]

# Filter for selected districts
ibcm_districts_shp <- tz_shp %>% 
  dplyr::filter(dist_nm %in% my_selection) %>% 
  left_join(., summ_table_vials, by = c("dist_nm" =  "district"))


high_low_distr <- tz_shp %>% 
  dplyr::filter(dist_nm %in% c("Liwale", "Ulanga")) %>% 
  left_join(., summ_table_vials, by = c("dist_nm" =  "district"))


# # color palette
# pal1 <-
#   colorBin(
#     palette = "YlOrRd",
#     domain = ibcm_districts_shp$Popultn,
#     bins = 7,
#     pretty = TRUE,
#     na.color = "#808080"
#   )
# 
# # pop up message
# labels <- 
#   sprintf(
#     "<strong>%s</strong><br/>%s",
#     ibcm_districts_shp$dist_nm, scales::comma(ibcm_districts_shp$Popultn)) %>% 
#   lapply(htmltools::HTML)
# 
# # passing the shp df to leaflet
# map_1<- leaflet(ibcm_districts_shp) %>%
#   # adding tiles, without labels to minimize clutter
#   addProviderTiles("CartoDB.PositronNoLabels") %>%
#   # parameters for the polygons
#   addPolygons(
#     fillColor = ~pal1(ibcm_districts_shp$Popultn), 
#     weight = 1,
#     opacity = 1,
#     color = "white",
#     fillOpacity = 0.7,
#     highlight = highlightOptions(
#       weight = 2,
#       color = "#666",
#       fillOpacity = 0.8,
#       bringToFront = TRUE),
#     label = labels,
#     labelOptions = labelOptions(
#       style = list("font-weight" = "normal"),
#       textsize = "15px",
#       direction = "auto")) %>%
#   # legend
#   addLegend(pal = pal1,
#             values = ibcm_districts_shp$Popultn,
#             position = "bottomright",
#             title = "Population",
#             opacity = 0.8,
#             na.label = "No data")
# 
# # Print the map
# map_1



## ALL distrcits  (trouble shooting)

# # color palette
# pal2 <-
#   colorBin(
#     palette = "YlOrRd",
#     domain = tz_shp$Popultn,
#     bins = 5,
#     pretty = TRUE,
#     na.color = "#808080"
#   )
# 
# # pop up message
# labels2 <- 
#   sprintf(
#     "<strong>%s</strong><br/>%s",
#     tz_shp$dist_nm, scales::comma(tz_shp$Popultn)) %>% 
#   lapply(htmltools::HTML)
# 
# leaflet(tz_shp) %>%
#   # adding tiles, without labels to minimize clutter
#   addProviderTiles("CartoDB.PositronNoLabels") %>%
#   # parameters for the polygons
#   addPolygons(
#     fillColor = ~pal2(tz_shp$Popultn), 
#     weight = 1,
#     opacity = 1,
#     color = "white",
#     fillOpacity = 0.7,
#     highlight = highlightOptions(
#       weight = 2,
#       color = "#666",
#       fillOpacity = 0.8,
#       bringToFront = TRUE),
#     label = labels2,
#     labelOptions = labelOptions(
#       style = list("font-weight" = "normal"),
#       textsize = "15px",
#       direction = "auto")) %>%
#   # legend
#   addLegend(pal = pal2,
#             values = tz_shp$Popultn,
#             position = "bottomright",
#             title = "Population",
#             opacity = 0.8,
#             na.label = "No data")

# Calculate incidence ########
## bites summary
summary_stats

## population
population_df <- ibcm_districts_shp  %>%
  st_drop_geometry() %>%
  left_join(., summary_stats, by = c("dist_nm" = "District_facility")) %>%
  dplyr::select(dist_nm, `Total_reported`, `total_deaths`, Popultn) %>%
  dplyr::mutate(incidence_per_100k_per_year = (`Total_reported` / (Popultn*4.3))*100000) # data spans 4.3 years (Augu 2018- Dec 2023)


mean(population_df$incidence_per_100k_per_year)
min(population_df$incidence_per_100k_per_year)
max(population_df$incidence_per_100k_per_year)


# stocking PEP###########
facilities_df <- read.csv("./data/clean/health_facilities_Revised30thMay.csv")

facilities_df <- facilities_df %>%
  dplyr::filter(!Region_facility == "Arusha",
                !District_facility == "Serengeti")


sum(facilities_df$Is.PEP.available == 'No')
sum(facilities_df$Is.PEP.available == 'Available')

# by dist
tmp <- facilities_df %>%
  dplyr::filter(`Is.PEP.available` == "Available") %>%
  group_by(District_facility) %>%
  summarize(Count_No_PEP = n())




#### IBCM clinic and year
ibcm_clinic_year <- ibcm_bite_data %>%
  group_by(new_FACILITY, District_facility, Year) %>%
  summarise(bites = sum(n)) %>%
  pivot_wider(names_from = Year, values_from = bites, values_fill = 0)


write.csv(ibcm_clinic_year, "./manuscript/ibcm_clinic_year.csv")


