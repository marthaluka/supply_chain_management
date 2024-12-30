

# Data #######
ke_data <- read.csv("./data/ke_data/dog_bite_patientsMartha.csv") %>%
  # format date
  dplyr::mutate(Date = dmy(paste0("01-", periodname)),
                month_year = format(Date, "%b-%y"))  %>%
  dplyr::rename(
    county = orgunitlevel2,
    subcounty = orgunitlevel3,
    bites = `Dog.Bites`,
    clinic = organisationunitname
  )  %>%
  # select relevant columns
  dplyr::select(county, subcounty, month_year, Date, bites, clinic) %>%
  # more date formats for aesthetics
  dplyr::mutate(
    Year_month = as.Date(as.yearmon(Date, "%b %Y")),
    Month = month(Year_month, label = TRUE, abbr = TRUE),  # Extract month as an abbreviated label
    Year = factor(year(Year_month)),  # Extract year and convert to factor
    county = str_replace(county, " County$", ""), # shorten county names
    county = dplyr::recode(county,   "Muranga" = "Murang'a"), # proper name
    subcounty = str_replace(subcounty, " Sub County$", ""), # shorten subcounty names
    subcounty = dplyr::recode(subcounty,   "Muranga" = "Murang'a") # proper name
  ) 


#write.csv(ke_data, "./data/ke_data/ke_data_summarised.csv")

ke_data_county <- ke_data %>%
  # summarise per county per month
  group_by(county, month_year, Month, Year, Date) %>%
  summarise(Bites = sum(bites, na.rm = TRUE)) 


ke_data_subcounty <- ke_data %>%
  # summarise per county per month
  group_by(subcounty, county, month_year, Date) %>%
  summarise(Bites = sum(bites, na.rm = TRUE))
  
length(unique(ke_data$clinic))
length(unique(ke_data$county))
length(unique(ke_data_subcounty$subcounty))


# Seasonality ########
A <- ke_data_county %>% 
  ggplot(., aes(x = Month, y = Bites, group = Year, color = Year)) +
  geom_line() +
  facet_wrap(~ county, scales = "free_y") +
  labs(#title = "A. Monthly Bites per District by Year",
    x = "Month",
    y = "Monthly Bites",
    color = "Year") +
  scale_x_discrete(breaks = month.abb[c(1, 4, 7, 10)], labels = month.abb[c(1, 4, 7, 10)]) + 
  theme_lancet() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank()) 

A

# shapefile
  ## source: https://hub.arcgis.com/datasets/kuniversity::kenya-2019-census/explore?location=0.420180%2C38.252429%2C7.00 

ke_shp <- st_read(dsn="./data/ke_data/Kenya_2019_Population_Data_KNBS/", 
                  layer="Kenya_2019_Population_Data_KNBS") %>%
  dplyr::mutate(COUNTY = dplyr::recode(COUNTY,
                                       "Nairobi City" = "Nairobi",
                                       "Elgeyo-Marakwet" = "Elgeyo Marakwet")
  )

# subcounty shapefile
ke_shp2 <- st_read(dsn="./data/ke_data/ke_subcounty/", 
                   layer="ke_subcounty") %>%
  dplyr::mutate(
    county = str_to_title(county),
    subcounty = str_to_title(subcounty),
    subcounty = str_replace(subcounty, " Sub County$", ""), # shorten subcounty names
    county = dplyr::recode(county,   
                           "Muranga" = "Murang'a",
                           "Tharaka-Nithi" = "Tharaka Nithi",
                           "Elgeyo-Marakwet" = "Elgeyo Marakwet"
                           ), # proper names
    subcounty = dplyr::recode(subcounty,   "Muranga" = "Murang'a"),
    # remove extra white space
    county = str_trim(county),
    subcounty = str_trim(subcounty)
  )

unique(ke_shp2$county)

  # population (missing in shapefile)
subcounty_pop <- read.csv("./data/ke_data/ke_subcounty/ke_pop_subcounty_hdx.csv") %>%
  dplyr::select(1,2,3)
names(subcounty_pop) <- c("county", "subcounty", "pop")

subcounty_pop <- subcounty_pop %>%
  dplyr::mutate(
    county = str_to_title(county),
    subcounty = str_to_title(subcounty),
    pop = as.numeric(pop),  # Convert pop to numeric
    # remove extra white space
    county = str_trim(county),
    subcounty = str_trim(subcounty)
  ) %>%
  replace_na(list(pop = 0))  # Replace NA in pop with 0



ke_shp2 <- ke_shp2 %>%
  left_join(., subcounty_pop, by=c("county", "subcounty"))

unique(ke_shp2$subcounty) %>% sort()
ke_shp2 %>%
  filter(is.na(pop)) %>%
  nrow()


unique(ke_data_subcounty$subcounty)[!unique(ke_data_subcounty$subcounty) %in% ke_shp2$subcounty]
unique(ke_data_subcounty$subcounty)[!unique(ke_data_subcounty$subcounty) %in% subcounty_pop$subcounty]

# sum bites
ke_all <- ke_data_county %>%
  group_by(county) %>%
  summarise(sum_bites = sum(Bites))

# add bites to shapefile
ke_shp <- ke_shp %>%
  left_join(., ke_all, by = c("COUNTY" = "county")) %>%
  dplyr::mutate(bites_per_capita = (sum_bites/Total_Pop)*100000,
                incidence = bites_per_capita/3.25)



min(ke_shp$incidence)
max(ke_shp$incidence)


# facilities -- extract main facility
  ## county level
ke_data_main_facility_county <- ke_data %>%
  # Summarize per clinic
  group_by(county, clinic) %>%
  summarise(bites_in_top_clinic = sum(bites, na.rm = TRUE)) %>%
  dplyr::mutate(
    county = str_replace(county, " County$", ""), # shorten county names
    county = dplyr::recode(county,   "Muranga" = "Murang'a") # proper name
  ) %>%
  # Calculate the total bites and number of clinics per subcounty
  group_by(county) %>%
  dplyr::mutate(
    total_bites_county = sum(bites_in_top_clinic),
    num_clinics = n(),
    pCentral = bites_in_top_clinic/total_bites_county
  ) %>%
  # Extract the clinic with the highest bites per subcounty
  slice_max(bites_in_top_clinic, with_ties = FALSE)%>%
  ungroup()%>%
  dplyr::select(-clinic)

ke_data_main_facility_county

  ## subcounty level
ke_data_main_facility_subcounty <- ke_data %>%
  # Summarize per clinic
  group_by(county, subcounty, clinic) %>%
  summarise(bites_in_top_clinic = sum(bites, na.rm = TRUE)) %>%
  dplyr::mutate(
    county = str_replace(county, " County$", ""), # shorten county names
    county = dplyr::recode(county,   "Muranga" = "Murang'a"), # proper name
    subcounty = str_replace(subcounty, " Sub County$", "") # Shorten subcounty names
  ) %>%
  # Calculate the total bites and number of clinics per subcounty
  group_by(subcounty) %>%
  dplyr::mutate(
    total_bites_subcounty = sum(bites_in_top_clinic),
    num_clinics = n(),
    pCentral = bites_in_top_clinic/total_bites_subcounty
  ) %>%
  # Extract the clinic with the highest bites per subcounty
  slice_max(bites_in_top_clinic, with_ties = FALSE)%>%
  ungroup()%>%
  dplyr::select(-clinic)

ke_data_main_facility_subcounty



# incidence (export to table?)
  ## county
ke_inc <- ke_shp %>%
  left_join(., ke_data_main_facility_county, by=c("COUNTY"= "county")) %>%
  st_drop_geometry() %>%
  dplyr::select(COUNTY, Total_Pop, num_clinics, total_bites_county, bites_per_capita, incidence, pCentral)

min(ke_inc$incidence)
max(ke_inc$incidence)
mean(ke_inc$incidence)
  ## sub county





# Plot map
B <- ggplot() +
  geom_sf(data = ke_shp, aes(fill = bites_per_capita)) +     # IBCM districts
  scale_fill_distiller(palette = "YlOrRd", name = "Bites per 100k", labels = scales::comma, direction = 1) +
  theme(plot.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        legend.position = "top")


design <- "AAA
           AAA
           BBB
           BBB"


# Export
pdf("./figures/SupplementaryFig2.pdf", width = 10, height = 12)
A
dev.off()



# Summarise data 
tmp_summary <- ke_data_subcounty %>%
  group_by(subcounty) %>%
  summarise(
    ave_bites = mean(Bites),
    min_bites = min(Bites),
    max_bites = max(Bites),
    surgeF = max_bites/ave_bites
  )

# Use Makueni sub-county as high throughput & Garissa sub-county as low throughput

# Plot 1: Monthly bite patient presentations for Wajir and Nakuru
ke_monthly_ave <- ke_data_subcounty %>%
  #dplyr::mutate(month_year = as.Date(month_year)) %>%
  group_by(Date) %>%
  summarize(monthly_ave = mean(Bites)) %>%
  dplyr::filter(monthly_ave>0)
  

  
plotB <- ke_data_subcounty %>%
  dplyr::filter(subcounty %in% c("Makueni", "Garissa")) %>%
  dplyr::mutate(subcounty = dplyr::recode(subcounty,
                                          "Garissa" = "Low throughput sub-county",
                                          "Makueni" = "High throughput sub-county"),
                subcounty = factor(subcounty, 
                                   levels = c("High throughput sub-county", "Low throughput sub-county"))
                ) %>%
  ggplot(aes(x = Date, y = Bites, fill = subcounty, alpha = subcounty)) +
    geom_col(position = "identity") +  # Overlapping bars with specific transparency
    labs(x = NULL, y = "Bite patients") +
    theme_lancet() +
    scale_fill_manual(values = c("Low throughput sub-county" = "#5553c7", "High throughput sub-county" = "#d6b7f5")) +
    scale_alpha_manual(values = c("Low throughput sub-county" = 1, "High throughput sub-county" = 0.6)) +  
    scale_x_date(date_labels = "%b %Y", date_breaks = "4 months") +
    theme(legend.position = 'top',
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.background = element_blank()) +
  # Add the monthly average as a geom_line
  geom_line(data = ke_monthly_ave, aes(x = Date, y = monthly_ave), 
            color = "black", size = 0.8, linetype = "twodash", inherit.aes = FALSE) 

# Plot 2: Density distributions of Wajir and Nakuru separately

plot_two_districts_fit # fit and plot function from ibcm_vial_use.R

plot_data <- ke_data_subcounty %>%
  dplyr::rename(
    District_facility = subcounty,
    total_n = Bites
  )

plotC <- plot_two_districts_fit(data = plot_data, N = 1000, district1 = "Garissa", district2 = "Makueni",  
                       label1 ="Low throughput sub-county", 
                       label2 ="High throughput sub-county",support = 0:120, binwidth = 4) +
  scale_color_manual(values = c("Low throughput sub-county" = "#5553c7", 
                                "High throughput sub-county" = "#d6b7f5")) +
  theme(legend.position = "top",
        legend.title = element_blank())



# Plot 3: 

# Calculate surge factors
surge_factors <- ke_data_subcounty %>%
  group_by(subcounty) %>%
  summarise(mean_bites = mean(Bites),
            perc_97_5 = quantile(Bites, 0.975)) %>%
  mutate(surge_factor = perc_97_5 / mean_bites)%>%
  drop_na()

mean(surge_factors$surge_factor)
max(surge_factors$surge_factor)
min(surge_factors$surge_factor)

# Plot 4: Histogram of surge factors per subcounty
hist <- ggplot(surge_factors, aes(x = surge_factor)) +
  geom_histogram(binwidth = 0.5, color = "#1f77b4", fill = "#1f77b4") +
  theme_lancet() +
  labs(x = "Surge factor", y = "Count")

# Plot 5: Relationship between mean bites and surge factor
scatter <- ggplot(surge_factors, aes(x = mean_bites, y = surge_factor)) +
  geom_point() +
  geom_smooth(method = "loess", color = "#1f77b4") +
  theme_lancet() +
  labs(x = "Mean monthly bite patients", y = "Surge factor")

# Combine as an inset
plotD <- ggdraw() +
  draw_plot(scatter) + 
  draw_plot(hist, x = 0.4, y = 0.5, width = 0.55, height = 0.4)

# Convert ggplot objects to ggdraw
plotA <- ggdraw(B)
plotB <- ggdraw(plotB)
plotC <- ggdraw(plotC)


# Combine the plots
combined_plot <- (plotA+plotB)/(plotC+plotD) +
  plot_annotation(tag_levels = 'A')


pdf("./figures/SupplementaryFig3.pdf", width = 9.5, height = 8)
combined_plot
dev.off()

