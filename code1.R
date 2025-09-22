#test
# Value Set Authority API Functions in R
# Install required packages if not already installed
# install.packages(c("httr", "jsonlite", "dplyr"))

library(httr)
library(jsonlite)
library(dplyr)

# Main function to analyze value set type and grouping relationships
analyze_value_set <- function(value_set_id, base_url = "https://api.valuesetauthority.org", 
                             api_key = NULL, verbose = TRUE) {
  
  # Function to get value set details
  get_value_set_info <- function(vs_id) {
    endpoint <- paste0(base_url, "/valuesets/", vs_id)
    
    # Set up headers
    headers <- list("Accept" = "application/json")
    if (!is.null(api_key)) {
      headers[["Authorization"]] <- paste("Bearer", api_key)
    }
    
    tryCatch({
      response <- GET(endpoint, add_headers(.headers = headers))
      
      if (status_code(response) == 200) {
        content <- fromJSON(content(response, "text"), flatten = TRUE)
        return(content)
      } else {
        stop(paste("API request failed with status:", status_code(response)))
      }
    }, error = function(e) {
      stop(paste("Error fetching value set info:", e$message))
    })
  }
  
  # Function to check for grouping relationships
  check_grouping_membership <- function(vs_id) {
    endpoint <- paste0(base_url, "/valuesets/", vs_id, "/grouping-relationships")
    
    headers <- list("Accept" = "application/json")
    if (!is.null(api_key)) {
      headers[["Authorization"]] <- paste("Bearer", api_key)
    }
    
    tryCatch({
      response <- GET(endpoint, add_headers(.headers = headers))
      
      if (status_code(response) == 200) {
        content <- fromJSON(content(response, "text"), flatten = TRUE)
        return(content)
      } else if (status_code(response) == 404) {
        return(NULL)  # No grouping relationships found
      } else {
        warning(paste("Grouping relationship check failed with status:", status_code(response)))
        return(NULL)
      }
    }, error = function(e) {
      warning(paste("Error checking grouping relationships:", e$message))
      return(NULL)
    })
  }
  
  if (verbose) {
    cat("Analyzing value set:", value_set_id, "\n")
  }
  
  # Get basic value set information
  vs_info <- get_value_set_info(value_set_id)
  
  # Determine value set type
  # Note: Adjust these field names based on your actual API response structure
  vs_type <- NA
  if ("compose" %in% names(vs_info)) {
    if ("include" %in% names(vs_info$compose) && length(vs_info$compose$include) > 0) {
      # Check if it has explicit concept listings (extensional)
      has_concepts <- any(sapply(vs_info$compose$include, function(x) "concept" %in% names(x)))
      # Check if it has filter criteria (intensional/grouping)
      has_filters <- any(sapply(vs_info$compose$include, function(x) "filter" %in% names(x)))
      
      if (has_concepts && !has_filters) {
        vs_type <- "extensional"
      } else if (has_filters) {
        vs_type <- "grouping"
      } else {
        vs_type <- "mixed"
      }
    }
  } else if ("expansion" %in% names(vs_info)) {
    vs_type <- "extensional"
  } else if ("definition" %in% names(vs_info)) {
    vs_type <- "grouping"
  }
  
  # Initialize result
  result <- list(
    value_set_id = value_set_id,
    type = vs_type,
    belongs_to_grouping = FALSE,
    grouping_value_sets = character(0),
    details = vs_info
  )
  
  # If extensional, check for grouping membership
  if (vs_type == "extensional") {
    if (verbose) {
      cat("Value set is extensional. Checking for grouping relationships...\n")
    }
    
    grouping_info <- check_grouping_membership(value_set_id)
    
    if (!is.null(grouping_info) && length(grouping_info) > 0) {
      result$belongs_to_grouping <- TRUE
      
      # Extract grouping value set IDs (adjust based on API response structure)
      if ("parent_value_sets" %in% names(grouping_info)) {
        result$grouping_value_sets <- grouping_info$parent_value_sets
      } else if ("grouping_sets" %in% names(grouping_info)) {
        result$grouping_value_sets <- grouping_info$grouping_sets
      }
      
      if (verbose) {
        cat("Found", length(result$grouping_value_sets), "parent grouping value set(s)\n")
      }
    } else {
      if (verbose) {
        cat("No grouping relationships found\n")
      }
    }
  } else {
    if (verbose) {
      cat("Value set is", vs_type, "- skipping grouping relationship check\n")
    }
  }
  
  return(result)
}

# Batch analysis function for multiple value sets
analyze_multiple_value_sets <- function(value_set_ids, base_url = "https://api.valuesetauthority.org", 
                                       api_key = NULL, verbose = FALSE) {
  
  results <- list()
  
  cat("Analyzing", length(value_set_ids), "value sets...\n")
  
  for (i in seq_along(value_set_ids)) {
    vs_id <- value_set_ids[i]
    
    if (verbose || i %% 10 == 0) {
      cat("Processing", i, "of", length(value_set_ids), ":", vs_id, "\n")
    }
    
    tryCatch({
      result <- analyze_value_set(vs_id, base_url, api_key, verbose = FALSE)
      results[[vs_id]] <- result
    }, error = function(e) {
      cat("Error processing", vs_id, ":", e$message, "\n")
      results[[vs_id]] <- list(
        value_set_id = vs_id,
        type = "error",
        error = e$message
      )
    })
    
    # Add a small delay to be respectful to the API
    Sys.sleep(0.1)
  }
  
  return(results)
}

# Helper function to create a summary data frame from results
create_summary_df <- function(results) {
  
  summary_list <- lapply(results, function(result) {
    data.frame(
      value_set_id = result$value_set_id,
      type = result$type,
      belongs_to_grouping = result$belongs_to_grouping,
      num_grouping_sets = length(result$grouping_value_sets),
      grouping_sets = paste(result$grouping_value_sets, collapse = "; "),
      stringsAsFactors = FALSE
    )
  })
  
  summary_df <- do.call(rbind, summary_list)
  return(summary_df)
}

# Example usage:
# 
# # Single value set analysis
# result <- analyze_value_set("your-value-set-id", 
#                            base_url = "https://your-vsa-api.org",
#                            api_key = "your-api-key")
# 
# print(paste("Type:", result$type))
# print(paste("Belongs to grouping:", result$belongs_to_grouping))
# if (result$belongs_to_grouping) {
#   print(paste("Grouping value sets:", paste(result$grouping_value_sets, collapse = ", ")))
# }
#
# # Multiple value sets analysis
# vs_ids <- c("vs1", "vs2", "vs3")
# batch_results <- analyze_multiple_value_sets(vs_ids, 
#                                             base_url = "https://your-vsa-api.org",
#                                             api_key = "your-api-key")
#
# # Create summary table
# summary_table <- create_summary_df(batch_results)
# View(summary_table)
#
# # Filter for extensional value sets that belong to grouping sets
# extensional_in_groups <- summary_table %>%
#   filter(type == "extensional", belongs_to_grouping == TRUE)
# print(extensional_in_groups)
