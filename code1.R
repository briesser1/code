library(dplyr)
library(qcc)
library(lubridate)

# Assuming your data frame is called 'df'
# First, create the percent column and a proper time variable
df <- df %>%
  mutate(
    percent = (num / den) * 100,
    date = as.Date(paste(year, month, "01", sep = "-"))
  ) %>%
  arrange(id, measure, date)

# Get unique combinations of id and measure
combinations <- df %>%
  distinct(id, measure) %>%
  arrange(id, measure)

# Function to create control chart for a specific id-measure combination
create_control_chart <- function(location_id, measure_name, data) {
  
  # Filter data for this specific combination
  chart_data <- data %>%
    filter(id == location_id, measure == measure_name) %>%
    arrange(date)
  
  # Skip if insufficient data points (need at least 5-6 points)
  if (nrow(chart_data) < 5) {
    cat(paste("Skipping", location_id, "-", measure_name, 
              "- insufficient data points\n"))
    return(NULL)
  }
  
  # Create the control chart title
  chart_title <- paste("Control Chart:", location_id, "-", measure_name)
  
  # Create Individual-X control chart using percent values
  tryCatch({
    qcc_chart <- qcc(
      data = chart_data$percent,
      type = "xbar.one",
      title = chart_title,
      xlab = "Time Period",
      ylab = "Percent (%)",
      plot = TRUE
    )
    
    # Optional: Add time labels if you want dates on x-axis
    # Note: qcc uses sequential numbering, but you can customize
    
    return(qcc_chart)
    
  }, error = function(e) {
    cat(paste("Error creating chart for", location_id, "-", measure_name, 
              ":", e$message, "\n"))
    return(NULL)
  })
}

# Method 1: Create all charts in a loop
cat("Creating control charts for all combinations...\n")
for (i in 1:nrow(combinations)) {
  location <- combinations$id[i]
  measure <- combinations$measure[i]
  
  cat(paste("Processing:", location, "-", measure, "\n"))
  chart <- create_control_chart(location, measure, df)
  
  # Optional: Save each chart
  if (!is.null(chart)) {
    # Uncomment to save plots
    # ggsave(filename = paste0("chart_", location, "_", measure, ".png"), 
    #        width = 10, height = 6)
  }
}

# Method 2: Using purrr for functional programming approach (alternative)
library(purrr)

# Create a nested approach
charts_list <- combinations %>%
  pmap(function(id, measure) {
    create_control_chart(id, measure, df)
  })

# Name the list elements for easy reference
names(charts_list) <- paste(combinations$id, combinations$measure, sep = "_")

# Method 3: If you want to create charts with Moving Range as well
create_imr_charts <- function(location_id, measure_name, data) {
  
  chart_data <- data %>%
    filter(id == location_id, measure == measure_name) %>%
    arrange(date)
  
  if (nrow(chart_data) < 5) {
    cat(paste("Skipping", location_id, "-", measure_name, 
              "- insufficient data points\n"))
    return(NULL)
  }
  
  chart_title_i <- paste("Individual Chart:", location_id, "-", measure_name)
  chart_title_mr <- paste("Moving Range Chart:", location_id, "-", measure_name)
  
  # Individual Chart
  i_chart <- qcc(
    data = chart_data$percent,
    type = "xbar.one",
    title = chart_title_i,
    xlab = "Time Period",
    ylab = "Percent (%)"
  )
  
  # Moving Range Chart
  mr_chart <- qcc(
    data = chart_data$percent,
    type = "R",
    title = chart_title_mr,
    xlab = "Time Period",
    ylab = "Moving Range"
  )
  
  return(list(individual = i_chart, moving_range = mr_chart))
}

# Optional: Create both I and MR charts for all combinations
cat("\nCreating I-MR chart pairs...\n")
imr_charts <- combinations %>%
  pmap(function(id, measure) {
    create_imr_charts(id, measure, df)
  })

names(imr_charts) <- paste(combinations$id, combinations$measure, sep = "_")

# Summary of what was created
cat(paste("\nSummary: Created charts for", nrow(combinations), "combinations\n"))
print(combinations)
