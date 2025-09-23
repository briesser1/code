
# Quality Measures Anomaly Detection Script
# Detects big jumps in numerators, denominators, and percentages
# Designed for headless Linux server execution

# Load required libraries
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(tidyr)
library(knitr)
library(DT)
library(plotly)
library(htmlwidgets)

# Set up directories for output
output_dir <- "qa_output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

plots_dir <- file.path(output_dir, "plots")
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = TRUE)
}

#' Load and prepare quality measures data
#' @param file_path Path to the CSV file containing quality measures
#' @return Prepared dataframe with calculated percentages and date columns
load_quality_data <- function(file_path) {
  cat("Loading quality measures data...\n")
  
  # Load data
  data <- read_csv(file_path, show_col_types = FALSE)
  
  # Ensure required columns exist
  required_cols <- c("organization", "name", "id", "year", "month", "metric", "num", "den")
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Prepare data
  data_clean <- data %>%
    # Create date column
    mutate(
      date = as.Date(paste(year, month, "01", sep = "-")),
      # Calculate percentage (handle division by zero)
      percentage = ifelse(den == 0, NA, (num / den) * 100),
      # Create unique identifier for each org/site/metric combination
      group_id = paste(organization, name, metric, sep = "_")
    ) %>%
    # Remove rows with missing essential data
    filter(!is.na(num), !is.na(den), den >= 0, num >= 0) %>%
    # Sort by group and date
    arrange(group_id, date)
  
  cat(sprintf("Loaded %d records across %d unique org/site/metric combinations\n", 
              nrow(data_clean), length(unique(data_clean$group_id))))
  
  return(data_clean)
}

#' Generate descriptive statistics for the dataset
#' @param data Prepared quality measures dataframe
#' @return List of summary statistics and tables
describe_data <- function(data) {
  cat("Generating data description...\n")
  
  # Overall summary
  overall_summary <- list(
    total_records = nrow(data),
    date_range = range(data$date, na.rm = TRUE),
    organizations = length(unique(data$organization)),
    sites = length(unique(data$name)),
    metrics = length(unique(data$metric)),
    unique_combinations = length(unique(data$group_id))
  )
  
  # Summary by organization
  org_summary <- data %>%
    group_by(organization) %>%
    summarise(
      sites = n_distinct(name),
      metrics = n_distinct(metric),
      records = n(),
      date_range_start = min(date),
      date_range_end = max(date),
      .groups = 'drop'
    )
  
  # Summary by metric
  metric_summary <- data %>%
    group_by(metric) %>%
    summarise(
      organizations = n_distinct(organization),
      sites = n_distinct(name),
      records = n(),
      avg_num = round(mean(num, na.rm = TRUE), 2),
      avg_den = round(mean(den, na.rm = TRUE), 2),
      avg_percentage = round(mean(percentage, na.rm = TRUE), 2),
      .groups = 'drop'
    )
  
  # Data quality summary
  quality_summary <- data %>%
    group_by(group_id) %>%
    summarise(
      organization = first(organization),
      site = first(name),
      metric = first(metric),
      records = n(),
      missing_percentages = sum(is.na(percentage)),
      zero_denominators = sum(den == 0),
      date_gaps = max(date) - min(date) - (n() - 1) * 30,  # Approximate monthly gaps
      .groups = 'drop'
    ) %>%
    arrange(desc(missing_percentages))
  
  # Save summaries to files
  write_csv(org_summary, file.path(output_dir, "organization_summary.csv"))
  write_csv(metric_summary, file.path(output_dir, "metric_summary.csv"))
  write_csv(quality_summary, file.path(output_dir, "data_quality_summary.csv"))
  
  return(list(
    overall = overall_summary,
    organizations = org_summary,
    metrics = metric_summary,
    quality = quality_summary
  ))
}

#' Detect anomalies in time series data
#' @param values Numeric vector of values
#' @param dates Date vector corresponding to values
#' @param method Detection method ("zscore", "iqr", "percentage_change")
#' @param threshold Threshold for anomaly detection
#' @return Logical vector indicating anomalies
detect_anomalies <- function(values, dates, method = "percentage_change", threshold = 50) {
  if (length(values) < 3) return(rep(FALSE, length(values)))
  
  anomalies <- rep(FALSE, length(values))
  
  if (method == "zscore") {
    z_scores <- abs(scale(values)[,1])
    anomalies <- z_scores > threshold
    
  } else if (method == "iqr") {
    Q1 <- quantile(values, 0.25, na.rm = TRUE)
    Q3 <- quantile(values, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    anomalies <- values < (Q1 - threshold * IQR) | values > (Q3 + threshold * IQR)
    
  } else if (method == "percentage_change") {
    # Calculate percentage change from previous value
    pct_change <- c(0, diff(values) / lag(values, default = values[1]) * 100)[-1]
    anomalies[-1] <- abs(pct_change) > threshold
  }
  
  # Remove NA values
  anomalies[is.na(anomalies)] <- FALSE
  
  return(anomalies)
}

#' Analyze anomalies for each organization/site/metric combination
#' @param data Prepared quality measures dataframe
#' @param pct_change_threshold Threshold for percentage change detection (default 50%)
#' @return Dataframe of detected anomalies
analyze_anomalies <- function(data, pct_change_threshold = 50) {
  cat("Analyzing anomalies...\n")
  
  anomalies_list <- data %>%
    group_by(group_id) %>%
    group_split() %>%
    map(function(group_data) {
      if (nrow(group_data) < 3) return(NULL)
      
      # Detect anomalies in num, den, and percentage
      num_anomalies <- detect_anomalies(group_data$num, group_data$date, 
                                       "percentage_change", pct_change_threshold)
      den_anomalies <- detect_anomalies(group_data$den, group_data$date, 
                                       "percentage_change", pct_change_threshold)
      pct_anomalies <- detect_anomalies(group_data$percentage, group_data$date, 
                                       "percentage_change", pct_change_threshold)
      
      # Calculate percentage changes for context
      group_data$num_pct_change <- c(0, diff(group_data$num) / 
                                    lag(group_data$num, default = group_data$num[1]) * 100)[-1]
      group_data$den_pct_change <- c(0, diff(group_data$den) / 
                                    lag(group_data$den, default = group_data$den[1]) * 100)[-1]
      group_data$pct_pct_change <- c(0, diff(group_data$percentage) / 
                                    lag(group_data$percentage, default = group_data$percentage[1]) * 100)[-1]
      
      # Add anomaly flags
      group_data$num_anomaly <- num_anomalies
      group_data$den_anomaly <- den_anomalies
      group_data$pct_anomaly <- pct_anomalies
      group_data$any_anomaly <- num_anomalies | den_anomalies | pct_anomalies
      
      return(group_data)
    }) %>%
    bind_rows()
  
  # Filter to only anomalous records
  anomalies_only <- anomalies_list %>%
    filter(any_anomaly) %>%
    select(organization, name, metric, date, year, month, 
           num, den, percentage, 
           num_pct_change, den_pct_change, pct_pct_change,
           num_anomaly, den_anomaly, pct_anomaly) %>%
    arrange(organization, name, metric, date)
  
  # Save anomalies to file
  write_csv(anomalies_only, file.path(output_dir, "detected_anomalies.csv"))
  
  cat(sprintf("Detected %d anomalous records\n", nrow(anomalies_only)))
  
  return(list(
    all_data = anomalies_list,
    anomalies = anomalies_only
  ))
}

#' Create time series plots for each metric showing anomalies
#' @param data Data with anomaly flags
#' @param max_plots Maximum number of plots to create (to avoid overwhelming output)
create_anomaly_plots <- function(data, max_plots = 50) {
  cat("Creating anomaly visualization plots...\n")
  
  # Get unique combinations, prioritizing those with anomalies
  unique_combos <- data %>%
    group_by(group_id, organization, name, metric) %>%
    summarise(
      has_anomalies = any(any_anomaly, na.rm = TRUE),
      anomaly_count = sum(any_anomaly, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(desc(has_anomalies), desc(anomaly_count)) %>%
    head(max_plots)
  
  # Create plots for each combination
  for (i in 1:nrow(unique_combos)) {
    combo <- unique_combos[i, ]
    
    plot_data <- data %>%
      filter(group_id == combo$group_id) %>%
      arrange(date)
    
    if (nrow(plot_data) < 2) next
    
    # Create multi-panel plot
    p1 <- ggplot(plot_data, aes(x = date)) +
      geom_line(aes(y = num), color = "blue", alpha = 0.7) +
      geom_point(data = filter(plot_data, num_anomaly), 
                aes(y = num), color = "red", size = 2) +
      labs(title = paste("Numerator -", combo$organization, "-", combo$name), 
           subtitle = combo$metric, y = "Numerator", x = "Date") +
      theme_minimal()
    
    p2 <- ggplot(plot_data, aes(x = date)) +
      geom_line(aes(y = den), color = "green", alpha = 0.7) +
      geom_point(data = filter(plot_data, den_anomaly), 
                aes(y = den), color = "red", size = 2) +
      labs(y = "Denominator", x = "Date") +
      theme_minimal()
    
    p3 <- ggplot(plot_data, aes(x = date)) +
      geom_line(aes(y = percentage), color = "purple", alpha = 0.7) +
      geom_point(data = filter(plot_data, pct_anomaly), 
                aes(y = percentage), color = "red", size = 2) +
      labs(y = "Percentage", x = "Date") +
      theme_minimal()
    
    # Combine plots
    combined_plot <- cowplot::plot_grid(p1, p2, p3, ncol = 1, align = "v")
    
    # Save plot
    filename <- paste0("anomaly_plot_", gsub("[^A-Za-z0-9]", "_", combo$group_id), ".png")
    ggsave(file.path(plots_dir, filename), combined_plot, 
           width = 12, height = 10, dpi = 300, bg = "white")
  }
  
  cat(sprintf("Created %d anomaly plots in %s\n", nrow(unique_combos), plots_dir))
}

#' Create summary visualizations
#' @param anomalies_data List containing anomaly analysis results
#' @param description_data List containing data description results
create_summary_plots <- function(anomalies_data, description_data) {
  cat("Creating summary visualizations...\n")
  
  # Anomalies by organization
  org_anomalies <- anomalies_data$anomalies %>%
    group_by(organization) %>%
    summarise(
      anomaly_count = n(),
      sites_affected = n_distinct(name),
      metrics_affected = n_distinct(metric),
      .groups = 'drop'
    )
  
  p1 <- ggplot(org_anomalies, aes(x = reorder(organization, anomaly_count), y = anomaly_count)) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    coord_flip() +
    labs(title = "Anomalies by Organization", x = "Organization", y = "Number of Anomalies") +
    theme_minimal()
  
  ggsave(file.path(plots_dir, "anomalies_by_organization.png"), p1, 
         width = 10, height = 6, dpi = 300, bg = "white")
  
  # Anomalies by metric
  metric_anomalies <- anomalies_data$anomalies %>%
    group_by(metric) %>%
    summarise(
      anomaly_count = n(),
      organizations_affected = n_distinct(organization),
      .groups = 'drop'
    )
  
  p2 <- ggplot(metric_anomalies, aes(x = reorder(metric, anomaly_count), y = anomaly_count)) +
    geom_col(fill = "coral", alpha = 0.7) +
    coord_flip() +
    labs(title = "Anomalies by Metric", x = "Metric", y = "Number of Anomalies") +
    theme_minimal()
  
  ggsave(file.path(plots_dir, "anomalies_by_metric.png"), p2, 
         width = 10, height = 6, dpi = 300, bg = "white")
  
  # Anomalies over time
  time_anomalies <- anomalies_data$anomalies %>%
    group_by(date) %>%
    summarise(anomaly_count = n(), .groups = 'drop')
  
  p3 <- ggplot(time_anomalies, aes(x = date, y = anomaly_count)) +
    geom_line(color = "darkred", size = 1) +
    geom_point(color = "darkred", alpha = 0.7) +
    labs(title = "Anomalies Over Time", x = "Date", y = "Number of Anomalies") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(plots_dir, "anomalies_over_time.png"), p3, 
         width = 12, height = 6, dpi = 300, bg = "white")
  
  # Data completeness heatmap
  completeness_data <- description_data$quality %>%
    select(organization, site, metric, records, missing_percentages) %>%
    mutate(completeness = 1 - missing_percentages / records)
  
  if (nrow(completeness_data) > 0 && nrow(completeness_data) <= 100) {
    p4 <- ggplot(completeness_data, aes(x = metric, y = paste(organization, site, sep = " - "), 
                                       fill = completeness)) +
      geom_tile() +
      scale_fill_gradient2(low = "red", mid = "yellow", high = "green", 
                          midpoint = 0.5, name = "Completeness") +
      labs(title = "Data Completeness by Organization/Site and Metric", 
           x = "Metric", y = "Organization - Site") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(size = 8))
    
    ggsave(file.path(plots_dir, "data_completeness_heatmap.png"), p4, 
           width = 12, height = max(8, nrow(completeness_data) * 0.2), dpi = 300, bg = "white")
  }
  
  cat("Summary plots created successfully\n")
}

#' Generate HTML report
#' @param description_data Data description results
#' @param anomalies_data Anomaly analysis results
generate_html_report <- function(description_data, anomalies_data) {
  cat("Generating HTML report...\n")
  
  # Create HTML report content
  html_content <- sprintf('
<!DOCTYPE html>
<html>
<head>
    <title>Quality Measures Anomaly Detection Report</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        h1, h2, h3 { color: #333; }
        table { border-collapse: collapse; width: 100%%; margin: 20px 0; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #f2f2f2; }
        .summary-box { background: #f9f9f9; padding: 15px; border-left: 4px solid #007cba; margin: 20px 0; }
        .alert { background: #fff3cd; padding: 10px; border: 1px solid #ffc107; border-radius: 4px; margin: 10px 0; }
    </style>
</head>
<body>
    <h1>Quality Measures Anomaly Detection Report</h1>
    <p>Generated on: %s</p>
    
    <div class="summary-box">
        <h2>Executive Summary</h2>
        <ul>
            <li>Total Records: %s</li>
            <li>Organizations: %s</li>
            <li>Sites: %s</li>
            <li>Metrics: %s</li>
            <li>Date Range: %s to %s</li>
            <li>Anomalies Detected: %s</li>
        </ul>
    </div>
    
    <h2>Key Findings</h2>
    %s
    
    <h2>Data Quality Summary</h2>
    <p>The following table shows potential data quality issues by organization/site/metric combination:</p>
    %s
    
    <h2>Detected Anomalies</h2>
    <p>The following anomalies were detected using percentage change thresholds:</p>
    %s
    
    <h2>Visualizations</h2>
    <p>Detailed plots have been generated in the plots/ directory:</p>
    <ul>
        <li>anomalies_by_organization.png - Summary of anomalies by organization</li>
        <li>anomalies_by_metric.png - Summary of anomalies by metric type</li>
        <li>anomalies_over_time.png - Temporal distribution of anomalies</li>
        <li>Individual time series plots for each organization/site/metric combination</li>
    </ul>
</body>
</html>
    ',
    Sys.Date(),
    format(description_data$overall$total_records, big.mark = ","),
    description_data$overall$organizations,
    description_data$overall$sites,
    description_data$overall$metrics,
    description_data$overall$date_range[1],
    description_data$overall$date_range[2],
    format(nrow(anomalies_data$anomalies), big.mark = ","),
    if (nrow(anomalies_data$anomalies) > 0) {
      paste('<div class="alert">',
            sprintf('<strong>%s anomalies detected</strong> across %s organizations. ',
                   nrow(anomalies_data$anomalies),
                   length(unique(anomalies_data$anomalies$organization))),
            'Review the detailed tables and plots below for investigation.</div>')
    } else {
      '<div style="background: #d4edda; padding: 10px; border: 1px solid #c3e6cb; border-radius: 4px;">No significant anomalies detected in the data.</div>'
    },
    kable(head(description_data$quality %>% filter(missing_percentages > 0 | zero_denominators > 0), 20), 
          format = "html"),
    if (nrow(anomalies_data$anomalies) > 0) {
      kable(head(anomalies_data$anomalies, 50), format = "html")
    } else {
      "<p>No anomalies detected.</p>"
    }
  )
  
  writeLines(html_content, file.path(output_dir, "anomaly_report.html"))
  cat(sprintf("HTML report generated: %s\n", file.path(output_dir, "anomaly_report.html")))
}

#' Main function to run complete quality assurance analysis
#' @param file_path Path to the quality measures CSV file
#' @param pct_change_threshold Threshold for detecting percentage changes (default 50%)
run_quality_assurance <- function(file_path, pct_change_threshold = 50) {
  cat("=== Quality Measures Anomaly Detection Started ===\n")
  cat(sprintf("Threshold for percentage change detection: %s%%\n", pct_change_threshold))
  
  # Load and prepare data
  data <- load_quality_data(file_path)
  
  # Generate data description
  description <- describe_data(data)
  
  # Analyze anomalies
  anomalies <- analyze_anomalies(data, pct_change_threshold)
  
  # Create visualizations
  if (requireNamespace("cowplot", quietly = TRUE)) {
    create_anomaly_plots(anomalies$all_data)
  } else {
    cat("cowplot package not available, skipping detailed anomaly plots\n")
  }
  
  create_summary_plots(anomalies, description)
  
  # Generate report
  generate_html_report(description, anomalies)
  
  cat("=== Analysis Complete ===\n")
  cat(sprintf("Results saved in: %s\n", output_dir))
  cat("Files generated:\n")
  cat("- anomaly_report.html (main report)\n")
  cat("- detected_anomalies.csv (anomalous records)\n")
  cat("- organization_summary.csv, metric_summary.csv, data_quality_summary.csv\n")
  cat("- plots/ directory with visualizations\n")
  
  # Return summary for interactive use
  return(list(
    summary = description$overall,
    anomaly_count = nrow(anomalies$anomalies),
    output_directory = output_dir
  ))
}

# Example usage:
# Uncomment the following lines and modify the file path to run the analysis
# 
# # Install required packages if not already installed
# required_packages <- c("dplyr", "ggplot2", "readr", "lubridate", "tidyr", "knitr", "cowplot")
# new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
# if(length(new_packages)) install.packages(new_packages)
# 
# # Run the analysis
# results <- run_quality_assurance("your_quality_measures_data.csv", pct_change_threshold = 50)
# 
# # View summary
# print(results)

# For testing with sample data:
create_sample_data <- function(n_orgs = 3, n_sites = 5, n_metrics = 4, n_months = 24) {
  set.seed(123)
  
  organizations <- paste("Org", LETTERS[1:n_orgs], sep = "_")
  sites <- paste("Site", 1:n_sites, sep = "_")
  metrics <- paste("Metric", LETTERS[1:n_metrics], sep = "_")
  
  # Generate base data
  data <- expand.grid(
    organization = organizations,
    name = sites,
    metric = metrics,
    month_seq = 1:n_months
  ) %>%
    mutate(
      id = paste(organization, name, sep = "_"),
      year = 2022 + (month_seq - 1) %/% 12,
      month = ((month_seq - 1) %% 12) + 1,
      # Generate realistic denominators (stable with some variation)
      den = pmax(10, round(rnorm(n(), 100, 20))),
      # Generate numerators (correlated with denominator but with more variation)
      base_rate = runif(n(), 0.1, 0.8),
      num = rbinom(n(), den, base_rate)
    ) %>%
    select(-month_seq, -base_rate)
  
  # Inject some anomalies
  anomaly_indices <- sample(nrow(data), size = round(nrow(data) * 0.05))
  data$num[anomaly_indices] <- data$num[anomaly_indices] * sample(c(0.2, 3), length(anomaly_indices), replace = TRUE)
  
  return(data)
}

# Uncomment to create and test with sample data:
# sample_data <- create_sample_data()
# write_csv(sample_data, "sample_quality_data.csv")
# results <- run_quality_assurance("sample_quality_data.csv")
