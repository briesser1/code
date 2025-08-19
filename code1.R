

# Load required libraries
library(dplyr)
library(tidyr)

# If you don't have tidyr, install it with: install.packages("tidyr")

# Function to apply Western Electric Rules
apply_control_rules <- function(values) {
  n <- length(values)
  if (n < 8) return(rep(FALSE, n))  # Need minimum points for meaningful analysis
  
  # Calculate control limits and zones
  centerline <- mean(values, na.rm = TRUE)
  sigma <- sd(values, na.rm = TRUE)
  
  ucl <- centerline + 3 * sigma
  lcl <- centerline - 3 * sigma
  
  # Zone boundaries
  zone_a_upper <- centerline + 2 * sigma
  zone_a_lower <- centerline - 2 * sigma
  zone_b_upper <- centerline + 1 * sigma
  zone_b_lower <- centerline - 1 * sigma
  
  # Initialize violation flags
  violations <- rep(FALSE, n)
  violation_types <- rep("", n)
  
  for (i in 1:n) {
    current_violations <- c()
    
    # Rule 1: Single point outside control limits
    if (values[i] > ucl || values[i] < lcl) {
      current_violations <- c(current_violations, "Rule1_Outlier")
    }
    
    # Rule 2: Two out of three consecutive points in Zone A
    if (i >= 3) {
      zone_a_count <- sum(
        (values[(i-2):i] > zone_a_upper) | (values[(i-2):i] < zone_a_lower),
        na.rm = TRUE
      )
      if (zone_a_count >= 2) {
        current_violations <- c(current_violations, "Rule2_2of3ZoneA")
      }
    }
    
    # Rule 3: Four out of five consecutive points in Zone B or beyond (same side)
    if (i >= 5) {
      above_zone_b <- sum(values[(i-4):i] > zone_b_upper, na.rm = TRUE)
      below_zone_b <- sum(values[(i-4):i] < zone_b_lower, na.rm = TRUE)
      if (above_zone_b >= 4 || below_zone_b >= 4) {
        current_violations <- c(current_violations, "Rule3_4of5ZoneB")
      }
    }
    
    # Rule 4: Eight consecutive points on same side of centerline
    if (i >= 8) {
      above_center <- sum(values[(i-7):i] > centerline, na.rm = TRUE)
      below_center <- sum(values[(i-7):i] < centerline, na.rm = TRUE)
      if (above_center == 8 || below_center == 8) {
        current_violations <- c(current_violations, "Rule4_8SameSide")
      }
    }
    
    # Rule 5: Six consecutive points increasing or decreasing
    if (i >= 6) {
      diffs <- diff(values[(i-5):i])
      if (all(diffs > 0, na.rm = TRUE) || all(diffs < 0, na.rm = TRUE)) {
        current_violations <- c(current_violations, "Rule5_6Trend")
      }
    }
    
    # Rule 6: Fourteen consecutive points alternating up and down
    if (i >= 14) {
      diffs <- diff(values[(i-13):i])
      alternating <- TRUE
      for (j in 2:length(diffs)) {
        if (sign(diffs[j]) == sign(diffs[j-1])) {
          alternating <- FALSE
          break
        }
      }
      if (alternating) {
        current_violations <- c(current_violations, "Rule6_14Alternating")
      }
    }
    
    # Rule 7: Fifteen consecutive points within 1 sigma (Zone C)
    if (i >= 15) {
      in_zone_c <- sum(
        (values[(i-14):i] > zone_b_lower) & (values[(i-14):i] < zone_b_upper),
        na.rm = TRUE
      )
      if (in_zone_c == 15) {
        current_violations <- c(current_violations, "Rule7_15NearCenter")
      }
    }
    
    # Rule 8: Eight consecutive points away from Zone C
    if (i >= 8) {
      away_from_zone_c <- sum(
        (values[(i-7):i] < zone_b_lower) | (values[(i-7):i] > zone_b_upper),
        na.rm = TRUE
      )
      if (away_from_zone_c == 8) {
        current_violations <- c(current_violations, "Rule8_8AwayCenter")
      }
    }
    
    if (length(current_violations) > 0) {
      violations[i] <- TRUE
      violation_types[i] <- paste(current_violations, collapse = ";")
    }
  }
  
  return(list(
    violations = violations,
    violation_types = violation_types,
    centerline = centerline,
    ucl = ucl,
    lcl = lcl,
    sigma = sigma
  ))
}

# Alternative main function that avoids complex unnesting
process_control_chart <- function(df) {
  # Create percentage column
  df$pct <- df$num / df$den
  
  # Sort by name, metric, and year to ensure proper time series order
  df <- df %>%
    arrange(name, metric, year)
  
  # Initialize result columns
  df$num_violations <- FALSE
  df$num_violation_types <- ""
  df$den_violations <- FALSE
  df$den_violation_types <- ""
  df$pct_violations <- FALSE
  df$pct_violation_types <- ""
  
  # Control limit columns
  df$num_centerline <- NA
  df$num_ucl <- NA
  df$num_lcl <- NA
  df$num_sigma <- NA
  df$den_centerline <- NA
  df$den_ucl <- NA
  df$den_lcl <- NA
  df$den_sigma <- NA
  df$pct_centerline <- NA
  df$pct_ucl <- NA
  df$pct_lcl <- NA
  df$pct_sigma <- NA
  
  # Get unique name/metric combinations
  combinations <- df %>% 
    distinct(name, metric) %>%
    arrange(name, metric)
  
  # Process each combination
  for (i in 1:nrow(combinations)) {
    current_name <- combinations$name[i]
    current_metric <- combinations$metric[i]
    
    # Filter data for current combination
    subset_indices <- which(df$name == current_name & df$metric == current_metric)
    subset_data <- df[subset_indices, ]
    
    # Apply control rules to num column
    if (any(!is.na(subset_data$num))) {
      num_results <- apply_control_rules(subset_data$num)
      df$num_violations[subset_indices] <- num_results$violations
      df$num_violation_types[subset_indices] <- num_results$violation_types
      df$num_centerline[subset_indices] <- num_results$centerline
      df$num_ucl[subset_indices] <- num_results$ucl
      df$num_lcl[subset_indices] <- num_results$lcl
      df$num_sigma[subset_indices] <- num_results$sigma
    }
    
    # Apply control rules to den column
    if (any(!is.na(subset_data$den))) {
      den_results <- apply_control_rules(subset_data$den)
      df$den_violations[subset_indices] <- den_results$violations
      df$den_violation_types[subset_indices] <- den_results$violation_types
      df$den_centerline[subset_indices] <- den_results$centerline
      df$den_ucl[subset_indices] <- den_results$ucl
      df$den_lcl[subset_indices] <- den_results$lcl
      df$den_sigma[subset_indices] <- den_results$sigma
    }
    
    # Apply control rules to pct column
    if (any(!is.na(subset_data$pct) & is.finite(subset_data$pct))) {
      pct_results <- apply_control_rules(subset_data$pct)
      df$pct_violations[subset_indices] <- pct_results$violations
      df$pct_violation_types[subset_indices] <- pct_results$violation_types
      df$pct_centerline[subset_indices] <- pct_results$centerline
      df$pct_ucl[subset_indices] <- pct_results$ucl
      df$pct_lcl[subset_indices] <- pct_results$lcl
      df$pct_sigma[subset_indices] <- pct_results$sigma
    }
  }
  df <- df %>% 
    mutate(date = paste0(year,"-",month,"-01")) %>% 
    mutate( date = as.Date(date)) %>% 
    select(-month, -year)
  return(df)
}

# Example usage:
 processed_df <- process_control_chart(df)

# To view violations for a specific name/metric combination:
# processed_df %>%
#   filter(name == "your_name", metric == "your_metric") %>%
#   select(year, num, num_violations, num_violation_types,
#          den, den_violations, den_violation_types,
#          pct, pct_violations, pct_violation_types)

# Function to display results in console (non-GUI friendly)
display_control_results <- function(processed_df) {
  cat("==============================================\n")
  cat("CONTROL CHART ANALYSIS RESULTS\n")
  cat("==============================================\n\n")
  
  # Overall Summary
  total_combinations <- processed_df %>% 
    distinct(name, metric) %>% 
    nrow()
  
  cat("Total Name/Metric Combinations Analyzed:", total_combinations, "\n")
  cat("Total Data Points:", nrow(processed_df), "\n\n")
  
  # Violation Summary by Name/Metric
  violation_summary <- processed_df %>%
    group_by(name, metric) %>%
    summarise(
      data_points = n(),
      num_violations = sum(num_violations, na.rm = TRUE),
      den_violations = sum(den_violations, na.rm = TRUE),
      pct_violations = sum(pct_violations, na.rm = TRUE),
      total_violations = num_violations + den_violations + pct_violations,
      .groups = 'drop'
    ) %>%
    arrange(desc(total_violations))
  
  cat("VIOLATION SUMMARY BY NAME/METRIC:\n")
  cat("=================================\n")
  print(violation_summary)
  cat("\n")
  
  # Rule-specific violation counts
  rule_summary <- processed_df %>%
    select(name, metric, num_violation_types, den_violation_types, pct_violation_types) %>%
    pivot_longer(cols = ends_with("_violation_types"), 
                 names_to = "column", values_to = "violation_types") %>%
    filter(violation_types != "") %>%
    separate_rows(violation_types, sep = ";") %>%
    count(violation_types, sort = TRUE)
  
  cat("VIOLATIONS BY RULE TYPE:\n")
  cat("========================\n")
  print(rule_summary)
  cat("\n")
  
  # Combinations with highest violation rates
  high_violation_rate <- violation_summary %>%
    mutate(violation_rate = total_violations / data_points) %>%
    filter(total_violations > 0) %>%
    arrange(desc(violation_rate)) %>%
    head(10)
  
  if (nrow(high_violation_rate) > 0) {
    cat("TOP 10 COMBINATIONS BY VIOLATION RATE:\n")
    cat("=====================================\n")
    print(high_violation_rate)
    cat("\n")
  }
  
  # Control limits summary
  control_limits_summary <- processed_df %>%
    group_by(name, metric) %>%
    slice(1) %>%
    select(name, metric, 
           num_centerline, num_ucl, num_lcl,
           den_centerline, den_ucl, den_lcl,
           pct_centerline, pct_ucl, pct_lcl) %>%
    ungroup()
  
  cat("CONTROL LIMITS (First 10 combinations):\n")
  cat("=======================================\n")
  print(head(control_limits_summary, 10))
  cat("\n")
}

# Function to write detailed results to files
write_control_results <- function(processed_df, output_dir = "control_chart_output") {
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # 1. Write full processed dataset to CSV
  full_output_file <- file.path(output_dir, paste0("control_chart_full_", timestamp, ".csv"))
  write.csv(processed_df, full_output_file, row.names = FALSE)
  cat("Full dataset written to:", full_output_file, "\n")
  
  # 2. Write violations-only dataset
  violations_only <- processed_df %>%
    filter(num_violations == TRUE | den_violations == TRUE | pct_violations == TRUE)
  
  if (nrow(violations_only) > 0) {
    violations_file <- file.path(output_dir, paste0("control_chart_violations_", timestamp, ".csv"))
    write.csv(violations_only, violations_file, row.names = FALSE)
    cat("Violations-only dataset written to:", violations_file, "\n")
  }
  
  # 3. Write summary report to text file
  summary_file <- file.path(output_dir, paste0("control_chart_summary_", timestamp, ".txt"))
  
  sink(summary_file)
  cat("CONTROL CHART ANALYSIS SUMMARY REPORT\n")
  cat("=====================================\n")
  cat("Generated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  # Violation summary
  violation_summary <- processed_df %>%
    group_by(name, metric) %>%
    summarise(
      data_points = n(),
      num_violations = sum(num_violations, na.rm = TRUE),
      den_violations = sum(den_violations, na.rm = TRUE),
      pct_violations = sum(pct_violations, na.rm = TRUE),
      total_violations = num_violations + den_violations + pct_violations,
      violation_rate = round(total_violations / data_points * 100, 2),
      .groups = 'drop'
    ) %>%
    arrange(desc(total_violations))
  
  cat("VIOLATION SUMMARY BY NAME/METRIC:\n")
  cat("---------------------------------\n")
  write.table(violation_summary, sep = "\t", row.names = FALSE, quote = FALSE)
  cat("\n\n")
  
  # Rule breakdown
  rule_summary <- processed_df %>%
    select(name, metric, num_violation_types, den_violation_types, pct_violation_types) %>%
    pivot_longer(cols = ends_with("_violation_types"), 
                 names_to = "column", values_to = "violation_types") %>%
    filter(violation_types != "") %>%
    separate_rows(violation_types, sep = ";") %>%
    count(violation_types, sort = TRUE)
  
  cat("VIOLATIONS BY RULE TYPE:\n")
  cat("------------------------\n")
  write.table(rule_summary, sep = "\t", row.names = FALSE, quote = FALSE)
  cat("\n\n")
  
  # Control limits for each combination
  cat("CONTROL LIMITS BY NAME/METRIC:\n")
  cat("------------------------------\n")
  control_limits <- processed_df %>%
    group_by(name, metric) %>%
    slice(1) %>%
    select(name, metric, 
           num_centerline, num_ucl, num_lcl,
           den_centerline, den_ucl, den_lcl,
           pct_centerline, pct_ucl, pct_lcl) %>%
    ungroup()
  
  write.table(control_limits, sep = "\t", row.names = FALSE, quote = FALSE)
  
  sink()
  cat("Summary report written to:", summary_file, "\n")
  
  # 4. Write detailed violation log
  if (nrow(violations_only) > 0) {
    violation_log_file <- file.path(output_dir, paste0("control_chart_violation_log_", timestamp, ".txt"))
    
    sink(violation_log_file)
    cat("DETAILED VIOLATION LOG\n")
    cat("======================\n")
    cat("Generated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
    
    for (i in 1:nrow(violations_only)) {
      row <- violations_only[i, ]
      cat("Violation", i, ":\n")
      cat("  Name:", row$name, "\n")
      cat("  Metric:", row$metric, "\n")
      cat("  Year:", row$year, "\n")
      cat("  Values: num =", row$num, ", den =", row$den, ", pct =", round(row$pct, 4), "\n")
      
      if (row$num_violations) cat("  NUM Violations:", row$num_violation_types, "\n")
      if (row$den_violations) cat("  DEN Violations:", row$den_violation_types, "\n")
      if (row$pct_violations) cat("  PCT Violations:", row$pct_violation_types, "\n")
      cat("\n")
    }
    
    sink()
    cat("Detailed violation log written to:", violation_log_file, "\n")
  }
  
  cat("\nAll output files saved to directory:", output_dir, "\n")
}

# Debug version of build_control_charts to see what's happening
build_control_charts <- function(processed_df, output_dir = "control_chart_output", 
                                 save_plots = TRUE, show_plots = FALSE) {
  
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(purrr)
  
  
  
  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Determine time variable - use date if available, otherwise year
  time_var <- if ("date" %in% colnames(processed_df)) "date" else "year"
  time_label <- if (time_var == "date") "Date" else "Year"
  
  cat("Using time variable:", time_var, "\n")
  
  # Prepare data for plotting - pivot longer for the three metrics
  chart_data <- processed_df %>%
    select(name, metric, all_of(time_var), 
           num, den, pct,
           num_violations, den_violations, pct_violations,
           num_violation_types, den_violation_types, pct_violation_types,
           num_centerline, num_ucl, num_lcl, num_sigma,
           den_centerline, den_ucl, den_lcl, den_sigma,
           pct_centerline, pct_ucl, pct_lcl, pct_sigma) %>%
    pivot_longer(cols = c(num, den, pct), 
                 names_to = "column_type", 
                 values_to = "value") %>%
    # Add corresponding violation data
    mutate(
      violations = case_when(
        column_type == "num" ~ num_violations,
        column_type == "den" ~ den_violations,
        column_type == "pct" ~ pct_violations,
        TRUE ~ FALSE
      ),
      violation_types = case_when(
        column_type == "num" ~ num_violation_types,
        column_type == "den" ~ den_violation_types,
        column_type == "pct" ~ pct_violation_types,
        TRUE ~ ""
      ),
      centerline = case_when(
        column_type == "num" ~ num_centerline,
        column_type == "den" ~ den_centerline,
        column_type == "pct" ~ pct_centerline,
        TRUE ~ NA_real_
      ),
      ucl = case_when(
        column_type == "num" ~ num_ucl,
        column_type == "den" ~ den_ucl,
        column_type == "pct" ~ pct_ucl,
        TRUE ~ NA_real_
      ),
      lcl = case_when(
        column_type == "num" ~ num_lcl,
        column_type == "den" ~ den_lcl,
        column_type == "pct" ~ pct_lcl,
        TRUE ~ NA_real_
      ),
      sigma = case_when(
        column_type == "num" ~ num_sigma,
        column_type == "den" ~ den_sigma,
        column_type == "pct" ~ pct_sigma,
        TRUE ~ NA_real_
      )
    ) %>%
    # Calculate zone boundaries
    mutate(
      zone_a_upper = centerline + 2 * sigma,
      zone_a_lower = centerline - 2 * sigma,
      zone_b_upper = centerline + 1 * sigma,
      zone_b_lower = centerline - 1 * sigma
    ) %>%
    # Clean up violation types for labeling
    mutate(
      violation_label = if_else(violations, 
                                paste0("R", stringr::str_extract(violation_types, "(?<=Rule)\\d+")),
                                "")
    ) %>%
    # Remove rows with missing values
    filter(!is.na(value), !is.na(centerline))
  
  cat("Chart data dimensions after processing:", nrow(chart_data), "x", ncol(chart_data), "\n")
  
  # Create charts for each name/metric/column combination
  chart_data %>%
    group_by(name, metric, column_type) %>%
    group_walk(~ {
      current_data <- .x
      current_name <- .y$name
      current_metric <- .y$metric
      current_column <- .y$column_type
      
      # DEBUG: Check current data
      cat("\n--- DEBUGGING CURRENT CHART ---\n")
      cat("Chart for:", current_name, "-", current_metric, "-", current_column, "\n")
      cat("Data points:", nrow(current_data), "\n")
      
      if (time_var == "date") {
        cat("Time values (first 10):", head(as.character(current_data[[time_var]]), 10), "\n")
        cat("Unique time values:", length(unique(current_data[[time_var]])), "\n")
      } else {
        cat("Time values (first 10):", head(current_data[[time_var]], 10), "\n")
        cat("Unique time values:", length(unique(current_data[[time_var]])), "\n")
      }
      
      # Skip if insufficient data
      if (nrow(current_data) < 3) {
        cat("Skipping - insufficient data\n")
        return()
      }
      
      # Create the plot
      p <- current_data %>%
        ggplot(aes(x = !!sym(time_var), y = value)) +
        
        # Add zone backgrounds
        geom_ribbon(aes(ymin = zone_b_lower, ymax = zone_b_upper), 
                    fill = "lightgray", alpha = 0.3) +
        geom_ribbon(aes(ymin = zone_a_lower, ymax = zone_a_upper), 
                    fill = "orange", alpha = 0.2) +
        
        # Add control limit lines
        geom_hline(aes(yintercept = centerline), color = "green", size = 1, linetype = "solid") +
        geom_hline(aes(yintercept = ucl), color = "red", size = 1, linetype = "dashed") +
        geom_hline(aes(yintercept = lcl), color = "red", size = 1, linetype = "dashed") +
        geom_hline(aes(yintercept = zone_a_upper), color = "orange", size = 0.5, linetype = "dotted") +
        geom_hline(aes(yintercept = zone_a_lower), color = "orange", size = 0.5, linetype = "dotted") +
        geom_hline(aes(yintercept = zone_b_upper), color = "gray", size = 0.5, linetype = "dotdash") +
        geom_hline(aes(yintercept = zone_b_lower), color = "gray", size = 0.5, linetype = "dotdash") +
        
        # Add data points and lines
        geom_line(color = "blue", size = 0.8) +
        geom_point(aes(color = violations, size = violations)) +
        scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red"), guide = "none") +
        scale_size_manual(values = c("FALSE" = 2, "TRUE" = 3), guide = "none") +
        
        # Add violation labels
        geom_text(data = filter(current_data, violations), 
                  aes(label = violation_label), 
                  vjust = -0.5, hjust = 0.5, color = "red", size = 3, fontface = "bold") +
        
        # Formatting
        labs(
          title = paste("Control Chart:", current_name, "-", current_metric, "-", toupper(current_column)),
          x = time_label,
          y = paste(toupper(current_column), "Value")
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank()
        )
      
      # Format x-axis based on time variable
      if (time_var == "date") {
        date_range <- range(current_data[[time_var]], na.rm = TRUE)
        date_span <- as.numeric(difftime(date_range[2], date_range[1], units = "days"))
        
        cat("Date range:", as.character(date_range[1]), "to", as.character(date_range[2]), "\n")
        cat("Date span (days):", date_span, "\n")
        
        if (date_span > 1095) {  # > 3 years
          p <- p + scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m")
          cat("Using 6-month breaks\n")
        } else if (date_span > 365) {  # > 1 year
          p <- p + scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m")
          cat("Using 3-month breaks\n")
        } else {  # <= 1 year
          p <- p + scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")
          cat("Using 1-month breaks\n")
        }
      }
      
      # Save or display the plot
      if (save_plots) {
        filename <- paste0("control_chart_", 
                           gsub("[^A-Za-z0-9]", "_", current_name), "_",
                           gsub("[^A-Za-z0-9]", "_", current_metric), "_",
                           current_column, "_", timestamp, ".png")
        filepath <- file.path(output_dir, filename)
        
        ggsave(filepath, plot = p, width = 12, height = 8, dpi = 100)
        cat("Chart saved:", filepath, "\n")
      }
      
      if (show_plots) {
        print(p)
        if (!save_plots) readline(prompt = "Press [Enter] for next chart...")
      }
    })
  
  cat("Chart creation complete!\n")
}

# Updated workflow function
run_control_analysis <- function(df, output_dir = "control_chart_output", 
                                 display_console = TRUE, create_charts = TRUE,
                                 save_charts = TRUE, show_charts = FALSE) {
  cat("Starting control chart analysis...\n")
  
  # Process the data
  processed_df <- process_control_chart(df)
  
  # Display results in console
  if (display_console) {
    display_control_results(processed_df)
  }
  
  # Write results to files
  write_control_results(processed_df, output_dir)
  
  # Create control charts using new ggplot function
  if (create_charts) {
    cat("Creating control charts with ggplot2...\n")
    build_control_charts(processed_df, output_dir, save_charts, show_charts)
  }
  
  cat("Analysis complete!\n")
  return(processed_df)
}

# Example usage:
results <- run_control_analysis(df)
# 
# Or to run without console display:
# results <- run_control_analysis(your_df, display_console = FALSE)
