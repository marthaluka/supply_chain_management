



# Models ##############
source("./R/HelperFun.R")

# Define thresholds
restocking_vol <- 60
alert_threshold <- 20

mu_to_monthly_vials(regimen=IPC, PCentral=1, facilitie=1, possible_mu=20, no_of_months = 85,
                    PEP_admin='ID', vial_size='1', N=10, wastage=0, pep_compliance = 1, output = "district")$monthly_vials



# Stockouts and number of restocks
## No lag ######
calculate_stockouts_and_restocks_no_lag <- function(monthly_vax_needs, restocking_vol, alert_threshold) {
  n_rows <- nrow(monthly_vax_needs)
  n_cols <- ncol(monthly_vax_needs)
  stockouts <- integer(n_rows)  # Initialize vector to store stockout counts for each row
  restocks <- integer(n_rows)  # Initialize vector to store restock counts for each row
  
  for (i in 1:n_rows) {
    current_stock <- restocking_vol  # Start each year with a full stock
    
    for (j in 1:n_cols) {
      current_stock <- current_stock - monthly_vax_needs[i, j]  # Subtract month's usage from current stock
      
      # Check if we've run out of vaccines by the end of the month
      if (current_stock < 1) {
        stockouts[i] <- stockouts[i] + 1  # Record a stockout
      }
      
      # If stock hits Alert_threshold level or lower, restock by adding the Restocking_vol to current value for the next month. 
      # If current value is negative, it means some patients who missed their vaccines will receive once new vials arrive (therefore ok if negative!)
      if (current_stock <= alert_threshold) {
        restocks[i] <- restocks[i] + 1  # Record a restock
        current_stock <- current_stock + restocking_vol  # Allow missed patients to catch up with vaccine doses
      }
    }
  }
  
  return(list(stockouts = stockouts, restocks = restocks))
}

## With a  lag ######
# Assuming a lag of X months
calculate_stockouts_and_restocks_with_lag <- function(monthly_vax_needs, 
                                                      restocking_vol, alert_threshold, lag_in_months = 1) {
  n_rows <- nrow(monthly_vax_needs)
  n_cols <- ncol(monthly_vax_needs)
  stockouts <- integer(n_rows)  # Initialize vector to store stockout counts for each row
  restocks <- integer(n_rows)  # Initialize vector to store restock counts for each row
  
  for (i in 1:n_rows) {
    current_stock <- restocking_vol  # Start each year with a full stock
    restock_queue <- rep(0, n_cols + lag_in_months)  # Initialize restock queue to track restocking needs over the lag period
    restock_scheduled <- FALSE  # Flag to track if restocking is scheduled
    
    for (j in 1:ncol(monthly_vax_needs)) {
      current_stock <- current_stock - monthly_vax_needs[i, j]  # Subtract month's usage from current stock
      
      # Immediately check and record stockouts before any adjustments
      if (current_stock < 1) {
        stockouts[i] <- stockouts[i] + 1
      }
      
      # Apply restocking from the restock queue
      if (restock_queue[j] > 0) {
        current_stock <- current_stock + restock_queue[j]
        restock_queue[j] <- 0  # Clear the restock request
        restock_scheduled <- FALSE  # Reset the restock flag after restocking
      }
      
      # Check if we've hit or are below alert_threshold and schedule restocking if none is scheduled
      if (current_stock <= alert_threshold && !restock_scheduled) {
        restock_month <- j + lag_in_months  # Calculate the month when restocking will happen
        if (restock_month <= n_cols) {  # Ensure the restocking month is within the year
          restock_queue[restock_month] <- restocking_vol
          restocks[i] <- restocks[i] + 1  # Record a restock
          restock_scheduled <- TRUE  # Flag that restocking is scheduled
        }
      }
    }
    
    # Handle case where restocking is scheduled for the last month
    if (restock_queue[n_cols + 1] > 0) {
      current_stock <- current_stock + restock_queue[n_cols + 1]
    }
  }
  
  return(list(stockouts = stockouts, restocks = restocks))
}



## Wrapper function ######
calculate_stockouts_and_restocks <- function(monthly_vax_needs, restocking_vol, alert_threshold, lag_in_months = 0) {
  if (lag_in_months == 0) {
    return(calculate_stockouts_and_restocks_no_lag(monthly_vax_needs, restocking_vol, alert_threshold))
  } else {
    return(calculate_stockouts_and_restocks_with_lag(monthly_vax_needs, restocking_vol, alert_threshold, lag_in_months))
  }
}



## Loop Through Several restocking volumes and alert thresholds ######
  # Function to Loop Through Several restocking volumes and alert thresholds
calculate_stockouts_for_thresholds_and_values <- function(monthly_vax_needs, restocking_vols, 
                                                          alert_thresholds, lag_in_months = 0) {
  # Initialize a list to store results as rows of a matrix
  results_list <- list()
  
  # Loop over each combination of restocking_vol and alert_threshold
  for (Restocking_vol in restocking_vols) {
    for (value in alert_thresholds) {
      # Use the combined function to calculate stockouts and restocks for this combination
      stockouts_result <- calculate_stockouts_and_restocks(monthly_vax_needs, Restocking_vol, value, lag_in_months)
      
      # Extract stockouts and restocks_per_year from the result
      stockouts <- stockouts_result$stockouts
      restocks <- stockouts_result$restocks
      
      # Create a matrix for the current combination with restocking volume, alert_threshold, stockouts, and restocks
      current_matrix <- cbind(Stockouts = stockouts, Restocks = restocks, Restocking_vol = rep(Restocking_vol, length(stockouts)), alert_threshold = rep(value, length(stockouts)))
      
      # Append the current matrix to the results list
      results_list[[length(results_list) + 1]] <- current_matrix
    }
  }
  
  # Combine all matrices into a single matrix
  combined_matrix <- do.call(rbind, results_list)
  
  return(combined_matrix)
}


# Using hypothetical low and high throughput districts (instead of Liwale and Ulanga) ##########
# IPC
low_thru_vial_needs_IPC <- mu_to_monthly_vials(regimen=IPC, PCentral=1, facilitie=1, possible_mu=5, no_of_months = 85,
                                               PEP_admin='ID', vial_size='1', N=10, wastage=0, pep_compliance = 1, output = "district")$monthly_vials

high_thru_vial_needs_IPC <-  mu_to_monthly_vials(regimen=IPC, PCentral=1, facilitie=1, possible_mu=20, no_of_months = 85,
                                                 PEP_admin='ID', vial_size='1', N=1000, wastage=0, pep_compliance = 1, output = "district")$monthly_vials
# essen4
low_thru_vial_needs_essen4 <- mu_to_monthly_vials(regimen=essen4, PCentral=1, facilitie=1, possible_mu=5, no_of_months = 85,
                                                  PEP_admin='IM', vial_size='1', N=1000, wastage=0, pep_compliance = 1, output = "district")$monthly_vials

high_thru_vial_needs_essen4 <-  mu_to_monthly_vials(regimen=essen4, PCentral=1, facilitie=1, possible_mu=20, no_of_months = 85,
                                                    PEP_admin='IM', vial_size='1', N=1000, wastage=0, pep_compliance = 1, output = "district")$monthly_vials




# Calculate stockouts for all combinations of Restocking_vols and alert thresholds
# convert matrix to df
pre_visualize_processing <- function(dataset) {
  df <- dataset %>%
    as.data.frame() %>%
    group_by(Restocking_vol, alert_threshold) %>%
    summarise(mean_stockouts = mean(Stockouts), 
              mean_restocks = mean(Restocks))
  
  return(df)
}


# Define restocking values and alert_thresholds to iterate over
  restocking_vols <-seq(50, 700, 25)
  
  alert_thresholds <- seq(20, 400, 10)


# calc stockouts and restocks
run_the_selct_dists <- function(high_thruput_vaccine_needs = high_thru_vial_needs_IPC, 
                                low_thruput_vaccine_needs = low_thru_vial_needs_IPC, 
                                Restocking_vols = restocking_vols, 
                                crit_values= alert_thresholds) {
  
  high_thru_no_lag <- calculate_stockouts_for_thresholds_and_values(high_thruput_vaccine_needs, 
                                                                 Restocking_vols, crit_values, lag_in_months = 0)
  high_thru_no_lag <- pre_visualize_processing(high_thru_no_lag) %>%
    dplyr::mutate(df = "high_thru_no_lag")
  
  low_thru_no_lag <- calculate_stockouts_for_thresholds_and_values(low_thruput_vaccine_needs, 
                                                                 Restocking_vols, crit_values, lag_in_months = 0)
  low_thru_no_lag <- pre_visualize_processing(low_thru_no_lag) %>%
    dplyr::mutate(df = "low_thru_no_lag")
  
  high_thru_lag <- calculate_stockouts_for_thresholds_and_values(high_thruput_vaccine_needs, 
                                                              Restocking_vols, crit_values, lag_in_months = 1)
  high_thru_lag <- pre_visualize_processing(high_thru_lag) %>%
    dplyr::mutate(df = "high_thru_lag")
  
  low_thru_lag <- calculate_stockouts_for_thresholds_and_values(low_thruput_vaccine_needs, 
                                                              Restocking_vols, crit_values, lag_in_months = 1)
  low_thru_lag <- pre_visualize_processing(low_thru_lag) %>%
    dplyr::mutate(df = "low_thru_lag")
  
  combined_df <- rbind(high_thru_no_lag, low_thru_no_lag, high_thru_lag, low_thru_lag)
  
  return(combined_df)
}


# run function
combined_df_IPC <- run_the_selct_dists() %>%
  dplyr::mutate(district = dplyr::recode(df,
                                   'high_thru_no_lag' = 'High throughput',
                                   'high_thru_lag' = 'High throughput',
                                   'low_thru_lag' = 'Low throughput',
                                   'low_thru_no_lag' = 'Low throughput'),
                lag = dplyr::recode(df,
                                    'high_thru_no_lag' = 'No lag',
                                    'high_thru_lag' = 'One month lag',
                                    'low_thru_lag' = 'One month lag',
                                    'low_thru_no_lag' = 'No lag')
  )


combined_df_essen4 <- run_the_selct_dists(high_thruput_vaccine_needs = high_thru_vial_needs_essen4, 
                                          low_thruput_vaccine_needs = low_thru_vial_needs_essen4,
                                          Restocking_vols = restocking_vols, 
                                          crit_values= alert_thresholds) %>%
  dplyr::mutate(district = dplyr::recode(df,
                                         'high_thru_no_lag' = 'High throughput',
                                         'high_thru_lag' = 'High throughput',
                                         'low_thru_lag' = 'Low throughput',
                                         'low_thru_no_lag' = 'Low throughput'),
                lag = dplyr::recode(df,
                                    'high_thru_no_lag' = 'No lag',
                                    'high_thru_lag' = 'One month lag',
                                    'low_thru_lag' = 'One month lag',
                                    'low_thru_no_lag' = 'No lag')
  )

# Visualize
# Creating the heatmap
annotation_data <- data.frame(
  facet_variable = c('High throughput', 'High throughput'),  # Replace with your facet levels
  Restocking_vol = c(100, 150),  
  alert_threshold = c(50, 60),    
  mylabel = c('Minimum values', 'Minimum values')
)


plot_heatmap <- function(plot_data, col_to_plot, legend_title, label = "ID"){
  # Convert the character column name to a symbol
  col_to_plot <- sym(col_to_plot)
  
  plot_data <- plot_data %>%
    dplyr::filter(lag =="One month lag") %>%   # Only plot the 1 month lag (hash out to plot the no lag scenario)
    dplyr::mutate(
      lag = dplyr::recode(lag,
                  "One month lag" = label)) 
    ggplot() +
    geom_tile(data = plot_data, aes(x = Restocking_vol, y = alert_threshold, fill = !!(col_to_plot) / 7)) +
    facet_grid(rows = vars(lag), 
               cols = vars(district), scales = "free") +
    #geom_text(data = annotation_data, aes(x = Restocking_vol, y = alert_threshold, label = mylabel)) +
    scale_fill_gradient(low = "white", high = "red", name = legend_title) +
    labs(x = "Restocking volume",
         y = "Alert threshold",
         fill = legend_title) +
    theme_lancet() +
    theme(strip.background = element_blank(),
          #strip.text = element_blank(),
          legend.position = "top") 
}




stockouts_plot_IPC <- plot_heatmap(plot_data = combined_df_IPC, col_to_plot='mean_stockouts', legend_title="Annual \nstockouts: ") +
  theme(strip.text.y = element_blank(),
        axis.title.x = element_blank()) 

restocks_plot_IPC <- plot_heatmap(plot_data=combined_df_IPC, col_to_plot='mean_restocks', legend_title="Annual \nrestocks: ") +
  theme(axis.title = element_blank())

stockouts_plot_essen4 <- plot_heatmap(plot_data=combined_df_essen4, col_to_plot='mean_stockouts', legend_title="Stockouts") +
  theme(legend.position = "none",
        strip.text = element_blank())
restocks_plot_essen4 <- plot_heatmap(plot_data=combined_df_essen4, col_to_plot='mean_restocks', legend_title="Restocks", label="IM")+
  theme(legend.position = "none",
        strip.text.x = element_blank(),
        axis.title.y = element_blank())
 

stockouts_plot_IPC/stockouts_plot_essen4


myplot <- (stockouts_plot_IPC + restocks_plot_IPC) / (stockouts_plot_essen4+restocks_plot_essen4)+
  plot_annotation(tag_levels = 'A')

myplot

pdf("./manuscript/Fig4.pdf", width = 10, height = 5)
myplot
dev.off()


# Explore varying lags #######

## 1. Stockouts over different lags #######
## 2. Restocks over different lags #######

# Define the function
restocks_and_stockouts <- function(monthly_vax_needs, restocking_vol = 60, alert_threshold = 30) {
  # Create a list to store results
  results <- list()
  
  # Loop through lag_in_months values
  for (lag in 0:4) {
    result <- calculate_stockouts_and_restocks(monthly_vax_needs = monthly_vax_needs, 
                                               restocking_vol = restocking_vol, 
                                               alert_threshold = alert_threshold, lag_in_months = lag)
    results[[paste0(lag)]] <- result
  }
  
  # divide total stockouts by no of months to get a probability
  no_of_months <- ncol(monthly_vax_needs)
  
  # Function to calculate quantiles and mean for stockouts and restocks
  get_quantiles_and_mean <- function(result) {
    list(
      stockouts_quantiles = quantile((result$stockouts)/no_of_months, probs = c(0.025, 0.975)),
      stockouts_mean = mean((result$stockouts)/no_of_months),
      restocks_quantiles = quantile((result$restocks)/no_of_months, probs = c(0.025, 0.975)),
      restocks_mean = mean((result$restocks)/no_of_months)
    )
  }
  
  # Calculate quantiles and means for each lag result
  quantiles_and_means <- lapply(results, get_quantiles_and_mean)
  
  # Combine results into a data frame for easy viewing
  summary_df <- do.call(rbind, lapply(names(quantiles_and_means), function(name) {
    data.frame(
      lag = name,
      stockouts_2.5 = quantiles_and_means[[name]]$stockouts_quantiles[1],
      stockouts_mean = quantiles_and_means[[name]]$stockouts_mean,
      stockouts_97.5 = quantiles_and_means[[name]]$stockouts_quantiles[2],
      restocks_2.5 = quantiles_and_means[[name]]$restocks_quantiles[1],
      restocks_mean = quantiles_and_means[[name]]$restocks_mean,
      restocks_97.5 = quantiles_and_means[[name]]$restocks_quantiles[2]
    )
  }))
  
  row.names(summary_df) <- NULL
  return(summary_df)
}


# # Get stockouts and restocks
# Liwale_summary <- restocks_and_stockouts(monthly_vax_needs = Liwale_result,
#                                          restocking_vol = 60, alert_threshold = 30)
# 
# 
# Ulanga_summary <- restocks_and_stockouts(monthly_vax_needs = high_thruput_vaccine_needs,
#                                          restocking_vol = 250, alert_threshold = 60)


# Using hypothetical low and high throughput districts (instead of Liwale and Ulanga) ##########

# high_thru_vial_needs <- mu_to_monthly_vials(regimen=IPC, PCentral=1, facilities=1, possible_mu=20, no_of_months = 61,
  #  PEP_admin='ID', vial_size='1', N=10, wastage=0, output = "district")$monthly_vials

low_thru_vial_needs_IPC 
high_thru_vial_needs_IPC 

## calc restocks and stokouts
myplot

set.seed(123)
high_thru_IPC <- restocks_and_stockouts(monthly_vax_needs = high_thru_vial_needs_IPC,
                       restocking_vol = 200, alert_threshold = 110) %>%
  dplyr::mutate(regimen = "IPC")

low_thru_IPC <- restocks_and_stockouts(monthly_vax_needs = low_thru_vial_needs_IPC,
                       restocking_vol = 75, alert_threshold = 50) %>%
  dplyr::mutate(regimen = "IPC")

high_thru_essen4 <- restocks_and_stockouts(monthly_vax_needs = high_thru_vial_needs_essen4,
                                    restocking_vol = 200, alert_threshold = 110) %>%
  dplyr::mutate(regimen = "essen4")

low_thru_essen4 <- restocks_and_stockouts(monthly_vax_needs = low_thru_vial_needs_essen4,
                                   restocking_vol = 75, alert_threshold = 50) %>%
  dplyr::mutate(regimen = "essen4")



low_thru_plot_df <- low_thru_IPC %>%
  bind_rows(., low_thru_essen4)

high_thru_plot_df <- high_thru_IPC %>%
  bind_rows(., high_thru_essen4)



# Plot -- now moved to vial_use_by_models.R
plot_prob_function <- function(plot_data, subtitle){
  ggplot(data = plot_data) +
    geom_line(aes(x = lag, y = stockouts_mean, group = regimen, color=regimen)) +
    geom_ribbon(aes(x = lag, y = stockouts_mean, group = regimen, ymin = stockouts_2.5, ymax = stockouts_97.5, fill = regimen),
                alpha = 0.7) +
    labs(x = "Restocking lag in months",
         y = "Probability of a stockout",
         subtitle = subtitle) +
    scale_fill_manual(values = c("IPC" = "#1f77b4", "essen4" = "#64c56e"),
                      labels = c("IPC" = "ID", "essen4" = "IM")) +
    scale_color_manual(values = c("IPC" = "#1f77b4", "essen4" = "#64c56e"),
                       labels = c("IPC" = "ID", "essen4" = "IM")) +
    theme_lancet() +
    ylim(0,1) +
    guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL))
}



a <- plot_prob_function(plot_data = low_thru_plot_df, subtitle="Low throughput district") +
  theme(legend.position = "none")
b <- plot_prob_function(plot_data = high_thru_plot_df, subtitle="High throughput district")



(a+b)


pdf("./manuscript/Fig5.pdf", width = 8, height = 4)
(a+b) +
  plot_annotation(tag_levels = 'A')
dev.off()


# Table of appropriate stocking #######

# Define restocking values and alert_thresholds to iterate over
restock_vols <- seq(50, 1000, 25)

alert_thresh <- seq(10, 500, 10)




calc_appropriate_restock_numbers <- function(regimen=IPC, PCentral=1, facilities=1, district_mu=5, no_months = 13,
                                             PEP_admin='ID', vial_size='1', N=10, wastage=0, output = "district",
                                             restock_vols=restock_vols, alert_thresh=alert_thresh, lag_months = 1) {
  
  # Calculate vaccine needs
  vaccine_needs <- mu_to_monthly_vials(regimen=regimen, PCentral=PCentral, facilities=facilities, possible_mu=district_mu,
                                       no_of_months = no_months, PEP_admin=PEP_admin, vial_size=vial_size, N=N, wastage=wastage,
                                       output = output)$monthly_vials
  
  # Calculate stockouts for different thresholds and values
  stockouts <- calculate_stockouts_for_thresholds_and_values(monthly_vax_needs= vaccine_needs,
                                                             restocking_vols = restock_vols, alert_thresholds = alert_thresh,
                                                             lag_in_months = lag_months)
  
  # Summarize stockouts and restocks
  output <- as.data.frame(stockouts) %>%
    group_by(Restocking_vol, alert_threshold) %>%
    summarize(mean_stockouts = mean(Stockouts),
              mean_restocks = mean(Restocks), 
              .groups = 'drop') %>%
    dplyr::filter(mean_stockouts < 0.05,
                  Restocking_vol > (alert_threshold + (district_mu*4)))  # Keep combinations where restocking vol > alert threshold (proportinal to mu)
  
  # Select the row with the minimum Restocking_vol
  min_row <- output %>%
    filter(Restocking_vol == min(Restocking_vol)) %>%
    slice(1)  # In case there are multiple rows with the same min value, take the first one
  
  # Create result data frame with only one row
  result_df <- data.frame(
    district_mu = district_mu,
    min_restocking_vol = min_row$Restocking_vol,
    min_alert_threshold = min_row$alert_threshold,
    ave_restocks = min_row$mean_restocks
  )
  
  return(result_df)
}



possible_district_mu <- c(5, 10, 20, 30, 50, 75, 100)


# Define function to apply on each district_mu value
apply_function_ID <- function(possible_mu) {
  result <- tryCatch({
    calc_appropriate_restock_numbers(
      regimen = IPC, PCentral = 1, facilities = 1, district_mu = possible_mu, no_months = 85, 
      PEP_admin = 'ID', vial_size = '1', N = 1000, wastage = 0, output = "district",
      restock_vols = restock_vols, alert_thresh = alert_thresh, lag_months = 1)
  }, error = function(e) {
    message(paste("Error with district_mu =", possible_mu, ":", e$message))
    return(NULL)
  })
  
  return(result)
}

# Use lapply to iterate over the possible_district_mu values
output_list <- lapply(possible_district_mu, apply_function_ID)

# Combine results into a single data frame, removing NULL elements
output_table <- do.call(rbind, Filter(Negate(is.null), output_list)) %>%
  dplyr::mutate(ave_restocks = round(ave_restocks/7))

# Display the output
print(output_table)

# Export the flextable object to an word file
    # convert to flextable object
flextable_output_table <- flextable(output_table)
save_as_docx(flextable_output_table, path = "./manuscript/Table2.docx")


## table for essen 4 -- just to find values for fig5
 ## export as Supplementary Table 2


restock_vols2 = seq(30, 5000, 30)
alert_thresh2=seq(10, 3000, 30)


apply_function_IM <- function(possible_mu) {
  result <- tryCatch({
    calc_appropriate_restock_numbers(
      regimen = essen4, PCentral = 1, facilities = 1, district_mu = possible_mu, no_months = 61, 
      PEP_admin = 'IM', vial_size = '1', N = 1000, wastage = 0, output = "district",
      restock_vols = restock_vols2, alert_thresh = alert_thresh2, lag_months = 1)
  }, error = function(e) {
    message(paste("Error with district_mu =", possible_mu, ":", e$message))
    return(NULL)
  })
  
  return(result)
}

# Use lapply to iterate over the possible_district_mu values
output_list2 <- lapply(possible_district_mu, apply_function_IM)

# Combine results into a single data frame, removing NULL elements
output_table2 <- do.call(rbind, Filter(Negate(is.null), output_list2)) %>%
  dplyr::mutate(ave_restocks = round(ave_restocks/5))

# Display the output
print(output_table2)

# Export the flextable object to an word file
  # convert to flextable object
flextable_output_table2 <- flextable(output_table2)
save_as_docx(flextable_output_table2, path = "./manuscript/Supp_Table2.docx")




