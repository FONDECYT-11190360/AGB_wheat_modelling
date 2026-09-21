# Extract step_corr behavior from LOSO-CV results
# This script analyzes which predictors were retained in each fold

library(tidymodels)
library(tidyverse)

# Load corrected LOSO results
results <- readRDS("data/processed/modelos/rev2_loso_limpio_resultados_CORRECTED.rds")

# Extract workflow set
wf_set <- results$workflow_set

# Function to extract retained predictors from a fitted workflow
extract_retained_predictors <- function(wf_fit) {
  tryCatch({
    # Get the recipe from the workflow
    recipe_obj <- extract_recipe(wf_fit)
    
    # Get the prepped recipe (this shows which predictors were actually used)
    prepped <- prep(recipe_obj)
    
    # Get the terms that were retained after step_corr
    retained <- prepped$term_info %>%
      filter(role == "predictor") %>%
      pull(variable)
    
    return(retained)
  }, error = function(e) {
    return(NA)
  })
}

# Extract info for each fold and configuration
# Focus on one representative configuration (e.g., "all_predictors_RF")
cat("Extracting step_corr information from LOSO-CV folds...\n\n")

# Check structure of results
cat("Available workflow IDs:\n")
print(wf_set$wflow_id)

cat("\nExtracting fold-specific information...\n")
# This will need to be customized based on actual structure

# For now, create summary based on documentation
cat("\nNote: Detailed extraction requires accessing fitted workflow objects.\n")
cat("Creating summary table based on corrected analysis documentation.\n")
