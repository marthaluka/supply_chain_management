



######
# list regimen schedules
IPC <- c(0,3,7) 
essen4 <- c(0,3,7,14)
essen5 <- c(0,3,7,14,28)



# neg binomial distributions - start#######
# simulate bites from data
simulate_bites <- function(mydata, district,  N){
  # get recorded bites from district
  bites <- mydata %>%
    dplyr::filter(District_facility == district)  %>%
    pull(total_n)
  # ensure suffiecient data
  if (length(bites) < 2) {
    stop("Insufficient data to fit a negative binomial distribution")
  }
  
  # fit negative binomial
  fitted_nbinom <- fitdist(bites,"nbinom")
  # simulate based on distribution
  sample_nbinom <- rnbinom(N,size=fitted_nbinom$estimate["size"],mu=fitted_nbinom$estimate["mu"])
  
  return(sample_nbinom)
}


# Define days in each month (Dec to Dec next year)
days_in_month <- c(31, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)


# use sampling rather than division to avoid creating fractions of humans

decentralizePatients2 <- function(district_monthly_patients, Central, facilities = 5) {
  set.seed(123)  # Setting a seed for reproducibility
  
  num_months <- length(district_monthly_patients)  # Determine the actual number of months in district_monthly_patients
  
  out_list <- vector("list", num_months)
  
  for (m in 1:num_months) {
    total_patients <- district_monthly_patients[[m]]
    
    if (facilities == 1) {
      # All patients attend the main hospital
      main_hospital_patients <- unlist(lapply(total_patients, 
                                              function(x) rbinom(1, x, prob = 1)))
      # Ensure the output is in a matrix form (iterations x 1 facility)
      out_list[[m]] <- matrix(main_hospital_patients, ncol = 1)
    } else {
      if (facilities >= 2) {
        
        # Sample the number of patients attending the main hospital
        main_hospital_patients <- unlist(lapply(total_patients, 
                                                function(x) rbinom(1, x, prob = Central)))
        
        # Calculate the remaining patients after assigning to the main hospital
        remaining_patients <- total_patients - main_hospital_patients
        
        # Function to randomly distribute remaining patients among satellite facilities
        distributePatientsSatelitte <- function(num_patients, num_clinics) {
          if (num_patients > 0) {
            # Randomly assign each patient to a clinic
            assignments <- sample(1:num_clinics, num_patients, replace = TRUE)
            
            # Create a list to store the distribution
            distribution_list <- vector("numeric", num_clinics)
            
            # Count the number of patients in each clinic
            for (i in 1:num_clinics) {
              distribution_list[i] <- sum(assignments == i)
            }
            return(distribution_list)
          } else {
            return(rep(0, num_clinics))  # If no patients are remaining, return zeros
          }
        }
        
        # Number of satellite clinics
        num_clinics <- facilities - 1
        
        # Use lapply to distribute patients for each element in the list
        distribution_results <- lapply(remaining_patients, distributePatientsSatelitte, num_clinics = num_clinics)
        
        # Transpose the list of lists to get the desired format
        distribution_list <- do.call(rbind, distribution_results)
        
        # Add the new column to the first position
        distribution_list <- cbind(main_hospital_patients, distribution_list)
        
        colnames(distribution_list) <- NULL
        
        out_list[[m]] <- distribution_list
        
      } else {
        print("Please give a valid number of facilities (n > 0)")
      }
    }
  }
  
  return(out_list)
}





spread_patients_across_days <- function(first_visits, facilities, no_of_months, N) {
  
  days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)  # Base days per month
  days_in_month <- rep(days_in_month, length.out = no_of_months)  # Repeat for total months
  
  total_days <- sum(days_in_month)  # Total number of days across all months
  
  # Initialize a list to store matrices for each facility
  daily_matrices <- vector("list", facilities)
  
  # Handle the case where facilities == 1
  if (facilities == 1) {
    first_visits <- lapply(first_visits, function(matrix_or_vector) {
      if (is.vector(matrix_or_vector)) {
        matrix(matrix_or_vector, ncol = 1)
      } else {
        matrix_or_vector
      }
    })
  }
  
  for (f in seq_len(facilities)) {
    # Initialize a matrix for each facility
    facility_matrix <- matrix(0, nrow = N, ncol = total_days)
    
    for (m in seq_along(first_visits)) {
      month_days <- days_in_month[m]  # Days in the current month
      start_day <- sum(days_in_month[seq_len(m - 1)]) + 1  # Cumulative days before this month
      end_day <- start_day + month_days - 1  # Range of days for the current month
      
      # Ensure indices are finite and within valid bounds
      if (is.na(start_day) || is.na(end_day) || start_day > total_days || end_day > total_days) {
        next
      }
      
      # Get the number of patients for this facility and month
      monthly_data <- first_visits[[m]]
      
      for (i in seq_len(N)) {
        num_patients <- monthly_data[i, f]  # Patients for this facility and month
        
        # Randomly assign patients to days within the month
        if (num_patients > 0) {
          assigned_days <- sample(seq(start_day, end_day), num_patients, replace = TRUE)
          for (day in assigned_days) {
            facility_matrix[i, day] <- facility_matrix[i, day] + 1
          }
        }
      }
    }
    
    # Store the facility matrix
    daily_matrices[[f]] <- facility_matrix
  }
  
  return(daily_matrices)
}





pep_pts3_decentralized <- function(facility_matrices, regimen, pep_compliance = 1) {
  # Determine the total number of facilities
  num_facilities <- length(facility_matrices)
  # Initialize a list to store the aggregated daily matrices for each facility
  all_visits <- vector("list", num_facilities)
  
  for (f in seq_len(num_facilities)) {
    # Get the matrix for the current facility
    facility_matrix <- facility_matrices[[f]]
    # Number of iterations (rows) and days (columns) in the matrix
    iterations <- nrow(facility_matrix)
    total_days <- ncol(facility_matrix)
    
    # Initialize a new matrix to store aggregated visits per day
    aggregated_matrix <- matrix(0, nrow = iterations, ncol = total_days)
    
    # Iterate over each iteration (row)
    for (i in seq_len(iterations)) {
      # Extract the daily visits for the current iteration
      daily_visits <- facility_matrix[i, ]
      # Loop over each day in the calendar
      for (day in seq_along(daily_visits)) {
        # If there are patients on the current day
        if (daily_visits[day] > 0) {
          patients_remaining <- daily_visits[day]  # Start with patients visiting on the first day
          
          # Apply the regimen for subsequent doses
          for (dose in seq_along(regimen)) {
            dose_day <- day + regimen[dose]  # Day of the dose
            
            if (dose_day <= total_days) {
              # For the first dose, all patients come (compliance = 1)
              if (dose == 1) {
                patients_next_dose <- patients_remaining
              } else {
                # For subsequent doses, use pep_compliance
                patients_next_dose <- rbinom(1, patients_remaining, pep_compliance)
              }
              
              aggregated_matrix[i, dose_day] <- aggregated_matrix[i, dose_day] + patients_next_dose
              patients_remaining <- patients_next_dose  # Update the remaining patients for the next dose
            }
          }
        }
      }
    }
    # Store the aggregated matrix for the current facility
    all_visits[[f]] <- aggregated_matrix
  }
  
  # Combine all facility matrices into a list
  return(all_visits)
}




# Vials used (assuming availability is no issue and all seekers get PEP)
# Vials are either 1ml or 0.5 ml
# PEP_admin either 'IM' or 'ID'
calculate_vials_used <- function(patients_matrix, PEP_admin, Vial_size, wastage) {
  Vial_size <- as.numeric(Vial_size)
  
  # Validate arguments provided
  if (!(PEP_admin %in% c("ID", "IM")) | !(Vial_size %in% c(0.5, 1))) {
    stop("Invalid PEP admin or Vial size")
  }
  
  vial_used = 1-wastage
  
  if(PEP_admin == "ID" & Vial_size == 1){
    # ID with 1 ml vial
    vials_matrix <- ceiling(patients_matrix / (5*vial_used))  # up to 5 patients per vial in ID, with zero wastage
  } else if (PEP_admin == "ID" & Vial_size == 0.5){
    # ID with 0.5 ml vial
    vials_matrix <- ceiling(patients_matrix / (2.5*vial_used))  # up to 2.5 patients per vial in ID with 0.5 ml 
  } else {
    # IM (size doesn't matter)
    vials_matrix <- patients_matrix
  }
  return(vials_matrix)
}



# pt_data_to_vials_annual_district_sum
  # this wraps up the functions above
pt_data_to_annual_vials <- function(no_of_months = 13, regimen = essen4, mydata = complete_data, PCentral = 0.7, facilities = 5, 
                                     district = 'Bunda', N = 100, PEP_admin = "IM", vial_size = 1, wastage = 0, pep_compliance = 1){
  # Ensure regimen is an object
  if (is.character(regimen)) {
    regimen <- get(regimen, envir = .GlobalEnv)
  }
  
  # Simulate bites per district
  set.seed(123)
  bites_per_dist <- lapply(seq_len(no_of_months), function(x) simulate_bites(mydata=mydata, district=district,  N=N))
  bites_per_dist[[no_of_months]] <- rep(0,  N) # The extra month at the end has 0 new patients to allow for follow up visits of the last month
  
  # Spread out pts to different clinics in the district (first visit)
  set.seed(123)
  first_visits <- decentralizePatients2(district_monthly_patients = bites_per_dist, Central = PCentral, facilities = facilities)
  
  # assign dates of first visits
  set.seed(123)
  first_visits2 <- spread_patients_across_days(first_visits, facilities=facilities, no_of_months=no_of_months, N=N)
  
  # follow up visits -- to give total visits per facility
  set.seed(123)
  all_visits <- pep_pts3_decentralized(facility_matrices = first_visits2, regimen = regimen, pep_compliance = pep_compliance)
  
  # daily_vials_per_clinic
  daily_vials_per_clinic <- lapply(all_visits, 
                                   function(x) calculate_vials_used(x, PEP_admin = PEP_admin, 
                                                                    Vial_size = vial_size, wastage = wastage))
  
  
  # pick last 365 days of year ie drop the first 31 days (patient repeat doses not well represented)
  ## patients (first visits!!)
  monthly_new_pts_per_clinic <- first_visits #[-1] # Remove the first 1 month
  
  ## vials
  daily_vials_per_clinic2 <- daily_vials_per_clinic
  # daily_vials_per_clinic2 <- lapply(daily_vials_per_clinic, function(matrix) {
  #   matrix[, -c(1:31)]  # Remove the first 31 days
  # })
  
  # Use lapply to apply rowSums to each matrix in the list, getting annual patients & vials per clinic
  annual_pts_per_clinic <- lapply(seq_len(ncol(monthly_new_pts_per_clinic[[1]])), function(col_idx) {
    rowSums(sapply(monthly_new_pts_per_clinic, function(matrix) matrix[, col_idx]))
  })
  
  annual_vials_per_clinic <- lapply(daily_vials_per_clinic2, rowSums)
  
  # Combine the annual pts/ vials per clinic into one matrix
  annual_pts_per_clinic_matrix <- do.call(cbind, annual_pts_per_clinic)
  annual_vials_per_clinic_matrix <- do.call(cbind, annual_vials_per_clinic)
  
  # Calculate the row sums of the combined matrix to get total annual patients/ vials per district
  annual_dist_pts <- rowSums(annual_pts_per_clinic_matrix)
  annual_dist_vials <- rowSums(annual_vials_per_clinic_matrix)
  
  # vials
  vials_per_pt <- annual_dist_vials/annual_dist_pts
  # zero vials produce NaN's vials_per_pt -- get rid of that
  vials_per_pt <- replace_na(vials_per_pt, 0)
  
  # total vials in time horizon & vials per person
  out <- list(annual_dist_vials, vials_per_pt)
  names(out) <- c('total_vials', 'vials_per_pt')
  return(out) 
  
}


p<- pt_data_to_annual_vials(no_of_months = 13, regimen = IPC, mydata = complete_data, PCentral = 0.7, facilities = 5, 
                        district = 'Bunda', N = 10, PEP_admin = "ID", vial_size = 1, wastage = 0, pep_compliance = 1)
mean(p$vials_per_pt)



## Extract sizes and mu from IBCM data 
extract_size_mu <- function(mydata, district){
  # get recorded bites from district
  bites <- mydata %>%
    dplyr::filter(District_facility == district)  %>%
    pull(total_n)
  # fit negative binomial
  fitted_nbinom <- fitdist(bites,"nbinom")
  # extract values
  # & create a data frame row
  new_row <- data.frame(
    district = district, 
    mean_bites = mean(bites),
    median_bites = median(bites),
    size_estimate = fitted_nbinom$estimate["size"],
    size_std_error = fitted_nbinom$sd["size"],
    mu_estimate = fitted_nbinom$estimate["mu"],
    mu_std_error = fitted_nbinom$sd["mu"]
  )
  # Remove row names
  row.names(new_row) <- NULL
  # output
  return(new_row)
}


##  Calc patients per day given the mu and sampled size
simulate_bites_hypothetical <- function(possible_mu, N) {
  # Extract sizes and mu from IBCM data 
  ## Get the unique districts
  districts <- complete_data %>%
    dplyr::distinct(District_facility) %>%
    pull()
  
  ## Apply the extract_size_mu function to each district and combine results
  results <- purrr::map_dfr(districts, ~extract_size_mu(complete_data, .x))
  
  # Calculate the 2.5th and 97.5th percentiles
  lower_bound <- quantile(results$size_estimate, 0.025)
  upper_bound <- quantile(results$size_estimate, 0.975)
  
  # Sample from sizes between the lower and upper percentiles
  size_estimates <- runif(N, min = lower_bound, max = upper_bound)  
  
  # Function to simulate bites for a given size and possible_mu
  estimate_bites_for_given_size <- function(size) {
    rnbinom(1, size = size, mu = possible_mu)
  }
  
  # Use purrr::map_dbl to run the simulation across several sizes
  simulated_bites <- map_dbl(size_estimates, estimate_bites_for_given_size)
  
  return(simulated_bites)
}




mu_to_total_vials <- function(regimen=essen4, PCentral, facilities, possible_mu, no_of_months = 13,
                              PEP_admin='IM', vial_size='1', N=1000, wastage=0, pep_compliance = 1){
  
  # Ensure regimen is an object
  if (is.character(regimen)) {
    regimen <- get(regimen, envir = .GlobalEnv)
  }
  
  # Generate monthly patients list for each facility for X months
  set.seed(123)
  hypothetical_bites_per_dist <- lapply(seq_len(no_of_months), function(x) simulate_bites_hypothetical(possible_mu,  N))
  hypothetical_bites_per_dist[[no_of_months]] <- rep(0,  N) # The extra month at the end has 0 new patients to allow for follow up visits of the last month
  
  # Spread out pts to different clinics in the district (first visit)
  set.seed(123)
  first_visits <- decentralizePatients2(district_monthly_patients = hypothetical_bites_per_dist, Central = PCentral, facilities = facilities)
  
  # assign dates of first visits
  set.seed(123)
  first_visits2 <- spread_patients_across_days(first_visits, facilities=facilities, no_of_months=no_of_months, N=N)

  
  # follow up visits -- to give total visits per facility
  set.seed(123)
  all_visits <- pep_pts3_decentralized(facility_matrices = first_visits2, regimen = regimen, pep_compliance = pep_compliance)
  
  # daily_vials_per_clinic
  daily_vials_per_clinic <- lapply(all_visits, 
                                   function(x) calculate_vials_used(x, PEP_admin = PEP_admin, 
                                                                    Vial_size = vial_size, wastage = wastage))
  
  
  
  ## patients (first visits!!)
  monthly_new_pts_per_clinic <- first_visits #[-1] # Remove the first 1 month
  
  ## vials
  daily_vials_per_clinic2 <- daily_vials_per_clinic
  # daily_vials_per_clinic2 <- lapply(daily_vials_per_clinic, function(matrix) {
  #   matrix[, -c(1:31)]  # Remove the first 31 days
  # })
  
  # Use lapply to apply rowSums to each matrix in the list, getting annual patients & vials per clinic
  annual_pts_per_clinic <- lapply(seq_len(ncol(monthly_new_pts_per_clinic[[1]])), function(col_idx) {
    rowSums(sapply(monthly_new_pts_per_clinic, function(matrix) matrix[, col_idx]))
  })
  
  annual_vials_per_clinic <- lapply(daily_vials_per_clinic2, rowSums)
  
  # Combine the annual pts/ vials per clinic into one matrix
  annual_pts_per_clinic_matrix <- do.call(cbind, annual_pts_per_clinic)
  annual_vials_per_clinic_matrix <- do.call(cbind, annual_vials_per_clinic)
  
  
  # Calculate the row sums of the combined matrix to get total annual patients/ vials per district
  annual_dist_pts <- rowSums(annual_pts_per_clinic_matrix)
  annual_dist_vials <- rowSums(annual_vials_per_clinic_matrix)
  
  # vials
  vials_per_pt <- annual_dist_vials/annual_dist_pts
  # zero vials produce NaN's vials_per_pt -- get rid of that
  vials_per_pt <- replace_na(vials_per_pt, 0)
  
  # total vials in time horizon & vials per person
  out <- list(annual_dist_vials, vials_per_pt)
  names(out) <- c('total_vials', 'vials_per_pt')
  return(out) 
  
  
}



mu_to_monthly_vials<- function(regimen=essen4, PCentral, facilities, possible_mu, no_of_months = 45,
                               PEP_admin='IM', vial_size='1', N=1000, wastage=0, pep_compliance = 1, output = "district"){
  
  # Ensure regimen is an object
  if (is.character(regimen)) {
    regimen <- get(regimen, envir = .GlobalEnv)
  }
  
  # Generate monthly patients list for each facility for X months
  set.seed(123)
  hypothetical_bites_per_dist <- lapply(seq_len(no_of_months), function(x) simulate_bites_hypothetical(possible_mu,  N))
  hypothetical_bites_per_dist[[no_of_months]] <- rep(0,  N) # The extra month at the end has 0 new patients to allow for follow up visits of the last month
  
  # Spread out pts to different clinics in the district (first visit)
  set.seed(123)
  first_visits <- decentralizePatients2(district_monthly_patients = hypothetical_bites_per_dist, Central = PCentral, facilities = facilities)
  
  # assign dates of first visits
  set.seed(123)
  first_visits2 <- spread_patients_across_days(first_visits, facilities=facilities, no_of_months=no_of_months, N=N)
  
  
  # follow up visits -- to give total visits per facility
  set.seed(123)
  all_visits <- pep_pts3_decentralized(facility_matrices = first_visits2, regimen = regimen, pep_compliance = pep_compliance)
  
  # daily_vials_per_clinic
  daily_vials_per_clinic <- lapply(all_visits, 
                                   function(x) calculate_vials_used(x, PEP_admin = PEP_admin, 
                                                                    Vial_size = vial_size, wastage = wastage))
  
  
  ## Adjust based on specified months, repeating the pattern if necessary
  days_in_month <- rep(days_in_month, length.out = no_of_months)
  ## cum days
  cum_days <- c(0, cumsum(days_in_month))
  
  # Function to group columns by months and calculate row sums
  group_by_months <- function(daily_vials, cum_days) {
    num_cols <- ncol(daily_vials)  # Get the number of columns in the matrix
    
    monthly_sums <- sapply(seq_along(cum_days[-1]), function(i) {
      # Calculate start and end columns for this month
      start_col <- cum_days[i] + 1
      end_col <- cum_days[i + 1]
      
      # Ensure the indices are within the bounds of the matrix
      start_col <- max(1, min(start_col, num_cols))  # Start column must be between 1 and num_cols
      end_col <- max(1, min(end_col, num_cols))      # End column must be between 1 and num_cols
      
      # Handle cases where the range is invalid
      if (start_col > end_col) {
        return(rep(0, nrow(daily_vials)))  # Return zero vector for invalid ranges
      }
      
      # Compute the row sums for the valid range
      rowSums(daily_vials[, start_col:end_col, drop = FALSE])
    })
    
    return(monthly_sums)
  }
  
  
  
  # Apply the function to each clinic
  monthly_pts_per_clinic <- lapply(first_visits2, function(clinic_data) {
    group_by_months(clinic_data, cum_days)
  })
  monthly_vials_per_clinic <- lapply(daily_vials_per_clinic, function(clinic_data) {
    group_by_months(clinic_data, cum_days)
  })
  
  
  # sum up last month with first - the last month has no new patients but allows for follow up doses
  update_first_last_columns <- function(mat) {
    mat[, 1] <- mat[, 1] + mat[, ncol(mat)]  # Update first column
    mat <- mat[, -ncol(mat), drop = FALSE]   # Remove the last column
    return(mat)
  }
  
  # Apply the function to each matrix in the list
  monthly_pts_per_clinic <- lapply(monthly_pts_per_clinic, update_first_last_columns)
  monthly_vials_per_clinic <- lapply(monthly_vials_per_clinic, update_first_last_columns)
  
  # Reduce facility level data to district -- unless user specified to have output per facility
  reduce_clinic_data_to_dist <- function(matrix_list) {
    # Sum the matrices element-wise
    total_per_month <- Reduce(`+`, matrix_list)
    
    return(total_per_month)
  }
  
  # output
  if (output == "facility"){
    out_list <- list(monthly_pts_per_clinic,monthly_vials_per_clinic)
  } else {
    out1 <- reduce_clinic_data_to_dist(monthly_pts_per_clinic)
    out2 <- reduce_clinic_data_to_dist(monthly_vials_per_clinic)
    out_list <-  list(out1, out2)
  }
  
  names(out_list) <- c('monthly_pts', 'monthly_vials')
  return(out_list)
  
}



# 
mu_to_monthly_vials(regimen=IPC, PCentral=0.7, facilities=5, possible_mu=5, no_of_months = 15,
                  PEP_admin='ID', vial_size='1', N=10, pep_compliance = 1, wastage=0)
# 
# pt_data_to_annual_vials(no_of_months = 13, regimen = IPC, mydata = complete_data, PCentral = 1, facilities = 1, 
#                                     district = 'Bunda', N = 100, PEP_admin = "ID", vial_size = 1, wastage = 0, pep_compliance = 1)
