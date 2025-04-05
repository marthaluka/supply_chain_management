

require(pacman)
pacman::p_load(brms,
               purrr)

source("./scripts/HelperFun.R")

# # 1. Extract sizes and mu from IBCM data ##########
# 
# extract_size_mu(mydata = complete_data,  district = 'Ulanga')
# 
# # Get the unique districts
# districts <- complete_data %>%
#   dplyr::distinct(District_facility) %>%
#   pull()
# 
# # Apply the extract_size_mu function to each district and combine results
# results <- purrr::map_dfr(districts, ~extract_size_mu(complete_data, .x))


# # 2.  Fit a Gamma GLM ######
# gamma_glm <- brm(
#   formula = size_estimate ~ mu_estimate,
#   data = results,
#   family = Gamma(link = "inverse"),
#   prior = c(
#     #set_prior("normal(0, 10)", class = "b"),
#     set_prior("cauchy(0.5, 2)", class = "shape")
#   ),
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   control = list(adapt_delta = 0.999)
# )
# 
# # Check model convergence (R-hat values should be close to 1.00)
# summary(gamma_glm) 
# pp_check(gamma_glm) # Use posterior predictive checks to see how well model fits the data.
# pairs(gamma_glm) # Generating pairs plot
# 
# 
# # Predict for a new mu_estimate value
# new_data <- data.frame(mu_estimate = c(1, 5, 10, 15, 20, 25, 
#                                        30, 35, 40, 45, 50, 55, 
#                                        60, 65, 70, 75, 80, 85, 90, 100))
# 
# # Draw posterior predictive samples
# posterior_predictive_samples <- posterior_predict(
#   object = gamma_glm,
#   newdata = new_data,
#   ndraws = 1000  # Number of samples you want to draw
# )
# 
# # View some of the predictive samples
# head(posterior_predictive_samples)


## Loop through possible mu values #######

# Define the loop function
loop_thru_possible_mus <- function(mu_values, regimen, PCentral, facilities, PEP_admin, no_of_months = 13, 
                                   vial_size, N, wastage, pep_compliance = 1) {
  # Initialize lists to store results
  total_vials_results <- list()
  vials_per_pt_results <- list()
  
  # Loop through each mu value
  for (mu in mu_values$mu_estimate) {
    # Run the mu_to_total_vials function with the current mu value
    result <- mu_to_total_vials(possible_mu = mu, regimen = regimen, PCentral = PCentral, facilities = facilities,
                                PEP_admin = PEP_admin, vial_size = vial_size,
                                no_of_months = no_of_months, N = N, wastage = wastage, pep_compliance = pep_compliance)
    
    # Store results with the mu value tagged
    total_vials_results[[as.character(mu)]] <- data.frame(mu_estimate = mu, total_vials = result$total_vials)
    vials_per_pt_results[[as.character(mu)]] <- data.frame(mu_estimate = mu, vials_per_pt = result$vials_per_pt)
  }
  
  # Convert lists to data frames
  total_vials_df <- do.call(rbind, total_vials_results)
  vials_per_pt_df <- do.call(rbind, vials_per_pt_results)
  
  # Return results as a list of data frames
  return(list(total_vials = total_vials_df, vials_per_pt = vials_per_pt_df))
}

# Possible mu (set my user depending on a regions throughput). These are mean bite patients per a given (theoretical) district
possible_mu_values <- data.frame(mu_estimate = c(1, 5, 10, 15, 20, 25, 
                                                 30, 35, 40, 45, 50, 55, 
                                                 60, 65, 70, 75, 80, 85, 90, 100, 150, 200))


# Call the function with the necessary parameters
## IPC
vials_hypothetical_IPC <- loop_thru_possible_mus(
  mu_values = possible_mu_values, 
  regimen = 'IPC', 
  PCentral = 1, 
  facilities = 1, 
  PEP_admin = 'ID', 
  vial_size = '1', 
  no_of_months = 13,
  N = 1000, 
  wastage = 0, 
  pep_compliance = 1)


## essen4
vials_hypothetical_essen4 <- loop_thru_possible_mus(
  mu_values = possible_mu_values,
  regimen = essen4,
  PCentral = 1,
  facilities = 1,
  PEP_admin = "IM",
  vial_size = "1",
  no_of_months = 13,
  N = 1000,
  wastage = 0, 
  pep_compliance = 1
)


# Process the results
  ## IPC
total_vials_hypothetical_IPC <- vials_hypothetical_IPC$total_vials %>%
  as_tibble() %>%
  dplyr::mutate(regimen = "IPC")
  
vials_per_pt_hypothetical_IPC <- vials_hypothetical_IPC$vials_per_pt %>%
  as_tibble() %>%
  dplyr::mutate(regimen = "IPC")

  ## combined
total_vials_hypothetical <- vials_hypothetical_essen4$total_vials %>%
  as_tibble() %>%
  dplyr::mutate(regimen = "essen4")  %>%
  rbind(., total_vials_hypothetical_IPC)

vials_per_pt_hypothetical <- vials_hypothetical_essen4$vials_per_pt %>%
  as_tibble() %>%
  dplyr::mutate(regimen = "essen4") %>%
  rbind(., vials_per_pt_hypothetical_IPC)


# Plot ###########




supp_a <- total_vials_hypothetical %>%
  dplyr::mutate(
    regimen = dplyr::recode(regimen,
                            "essen4" = "IM",
                            "IPC" = "ID")
  )%>%
  group_by(mu_estimate, regimen) %>%
  summarize(
    mean_vials = mean(total_vials),
    lower_bound = quantile(total_vials, 0.025),
    upper_bound = quantile(total_vials, 0.975)
  ) %>%
  # Create the plot with geom_ribbon
  ggplot(., aes(x = mu_estimate, y = mean_vials, fill = regimen)) +
    geom_line(aes(color = regimen)) +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound, fill = regimen), alpha = 0.7, color = NA) +
    theme_classic() +
    labs(x = "Mean monthly patients", y = "Annual vials") +
    ylim(0, NA) +
    scale_fill_manual(values = c("ID" = "#1f77b4", "IM" = "#64c56e")) +
  scale_color_manual(values = c("ID" = "black", "IM" = "black")) +
  theme(legend.position = "top",
        #axis.title.y = element_blank()
        )+
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL))


# Summarize the data to get mean, and lower and upper bounds for the confidence intervals
supp_c <- vials_per_pt_hypothetical %>%
  dplyr::mutate(
    regimen = dplyr::recode(regimen,
                            "essen4" = "IM",
                            "IPC" = "ID")) %>%
  group_by(mu_estimate, regimen) %>%
  summarize(
    mean_vials_per_pt = mean(vials_per_pt),
    lower_bound = quantile(vials_per_pt, 0.025),
    upper_bound = quantile(vials_per_pt, 0.975)
  ) %>%
  # Create the plot with geom_ribbon
  ggplot(., aes(x = mu_estimate, y = mean_vials_per_pt, fill = regimen)) +
    geom_line(aes(color = regimen)) +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound, fill = regimen), alpha = 0.7, color = NA) +
    theme_classic() +
    labs(x = "Mean monthly patients", y = "Vials per Patient") +
    ylim(0, NA) +
    scale_fill_manual(values = c("ID" = "#1f77b4", "IM" = "#64c56e")) +
    scale_color_manual(values = c("ID" = "black", "IM" = "#64c56e")) +
    theme(legend.position = "none") +
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL))


supp_a/supp_c


# Decentralization & a hypothetical monthly mean bites #######
  ## Note: This is for IPC only as decentralization has no impact on vials used under essen4

# Df defining different levels of decentralization
source("./scripts/decentralization.R")
decentral_df

no_decentralization <- loop_thru_possible_mus(
  mu_values = possible_mu_values, 
  regimen = 'IPC', 
  PCentral = 1, 
  facilities = 1, 
  PEP_admin = 'ID', 
  vial_size = '1', 
  no_of_months = 13,
  N = 1000, 
  wastage = 0)

mod_decentralization <- loop_thru_possible_mus(
  mu_values = possible_mu_values, 
  regimen = 'IPC', 
  PCentral = 0.7, 
  facilities = 4, 
  PEP_admin = 'ID', 
  vial_size = '1', 
  no_of_months = 13,
  N = 1000, 
  wastage = 0)


high_decentralization <- loop_thru_possible_mus(
  mu_values = possible_mu_values, 
  regimen = 'IPC', 
  PCentral = 0.5, 
  facilities = 8, 
  PEP_admin = 'ID', 
  vial_size = '1', 
  no_of_months = 13,
  N = 1000, 
  wastage = 0)

# essen4_no_decentralization <- loop_thru_possible_mus(
#   mu_values = possible_mu_values, 
#   regimen = essen4, 
#   PCentral = 1, 
#   facilities = 1, 
#   PEP_admin = 'IM', 
#   vial_size = '1', 
#   no_of_months = 13,
#   N = 1000, 
#   wastage = 0)

########

# Add a decentralization column to the data frames
add_decentralization_column <- function(data, level) {
  data$total_vials <- data$total_vials %>% mutate(decentralization = level)
  data$vials_per_pt <- data$vials_per_pt %>% mutate(decentralization = level)
  return(data)
}

# Add decentralization level to each data frame
no_decentralization <- add_decentralization_column(no_decentralization, "None")
mod_decentralization <- add_decentralization_column(mod_decentralization, "Moderate")
high_decentralization <- add_decentralization_column(high_decentralization, "High")
#essen4_no_decentralization <- add_decentralization_column(essen4_no_decentralization, "essen4")


# Function to summarize the data
summarize_data <- function(vials_data, pts_data) {
  vials_summary <- vials_data %>%
    group_by(mu_estimate, decentralization) %>%
    summarize(
      mean_vials = mean(total_vials),
      lower_bound = quantile(total_vials, 0.025),
      upper_bound = quantile(total_vials, 0.975),
      .groups = 'drop'
    )
  
  pts_summary <- pts_data %>%
    group_by(mu_estimate, decentralization) %>%
    summarize(
      mean_vials = mean(vials_per_pt),
      lower_bound = quantile(vials_per_pt, 0.025),
      upper_bound = quantile(vials_per_pt, 0.975),
      .groups = 'drop'
    )
  
  list(vials_summary = vials_summary, pts_summary = pts_summary)
}

# Summarize the data
no_decentralization_summary <- summarize_data(no_decentralization$total_vials, no_decentralization$vials_per_pt)
mod_decentralization_summary <- summarize_data(mod_decentralization$total_vials, mod_decentralization$vials_per_pt)
high_decentralization_summary <- summarize_data(high_decentralization$total_vials, high_decentralization$vials_per_pt)
#essen4_summary <- summarize_data(essen4_no_decentralization$total_vials, essen4_no_decentralization$vials_per_pt)

# Combine summaries for plotting
vials_summary <- bind_rows(
  no_decentralization_summary$vials_summary,
  mod_decentralization_summary$vials_summary,
  #essen4_summary$vials_summary,  # leave out essen4 - decentralization has ni impact on vials
  high_decentralization_summary$vials_summary
  
)

vials_per_pt_summary <- bind_rows(
  no_decentralization_summary$pts_summary,
  mod_decentralization_summary$pts_summary,
  #essen4_summary$pts_summary,   # leave out essen4 - decentralization has ni impact on vials per patient
  high_decentralization_summary$pts_summary
)


# datasets drom ibcm_vial_use.R (to label)
data1 <- mean_essen4 %>%
  left_join(., summary_stats[,c('District_facility', 'Mean')], by= c('district'= 'District_facility')) %>% 
  sample_n(5)


# Function to plot the summarized data with geom_ribbon

plot_summary_data <- function(summary_data, y_var, y_label) {
  ggplot() +
    geom_line(data = summary_data, aes(x = mu_estimate, y = !!sym(y_var), color = decentralization)) +
    geom_ribbon(data = summary_data, aes(x = mu_estimate, y = !!sym(y_var), ymin = lower_bound, ymax = upper_bound, 
                                         fill = decentralization), alpha = 0.5, color = NA) +
    scale_color_manual(values = c("High" = "#EE9A00", "Moderate" = "#8968CD", "None" = "#1f77b4")) +
    scale_fill_manual(values = c("High" = "#EE9A00", "Moderate" = "#8968CD", "None" = "#1f77b4")) +
    theme_classic() +
    labs(x = "Mean monthly patients", y = y_label) +
    ylim(0, NA) +
    theme(legend.position = "right") +
    guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL))
}


supp_b <- plot_summary_data(vials_summary, "mean_vials", "Annual vials") +
  theme(legend.position = "top",
        axis.title = element_blank())
supp_d <- plot_summary_data(vials_per_pt_summary, "mean_vials", "Vials per patient")+
  theme(legend.position = "none")+
  ylab("")



(supp_a+supp_b)/(supp_c+supp_d) +
  plot_annotation(tag_levels = 'A')

pdf("./figures/SupplementaryFig4.pdf", width = 8, height = 7)
(supp_a+supp_b)/(supp_c+supp_d) +
  plot_annotation(tag_levels = 'A')
dev.off()




