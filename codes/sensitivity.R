



    for (dependent_var in dependent_var_list) {
    # Construct the formula
    additional_var <- independent_counts[[dependent_var]]
    if (count_smooth == "smooth"){
      independent_vars_smooth<- c(independent_vars_smooth_base, additional_var)
      independent_vars_linear <- independent_vars_linear_base
    }else if (count_smooth == "linear"){
      independent_vars_smooth<- independent_vars_smooth_base
      independent_vars_linear <- c(independent_vars_linear_base, additional_var)
    }
    else {
      independent_vars_smooth<- independent_vars_smooth_base
      independent_vars_linear <- independent_vars_linear_base
    }
    
    smooth_parts <- paste("s(", independent_vars_smooth, ", k=3)", collapse = " + ")
    linear_parts <- paste(independent_vars_linear, collapse = " + ")
    formula <- as.formula(paste(dependent_var, "~", smooth_parts, "+", linear_parts))
    
    # Fit the model
    gam_model <- gam(formula, data = visits_scores_wk, family = gaussian())
    
    # Capture the model summary
    sum_my_model <- summary(gam_model)
    model_summary <- capture.output(sum_my_model)
    summary_file_name <- paste0(dependent_var, "_model_summary.txt")
    writeLines(model_summary, summary_file_name)

    }
    
formula_test <- as.formula(Glocery.Pharmacies_visits_weekly_pp ~ s(DEATH_COUNT_log) + 
    s(week) + s(borough_case_count_log) + 
    loss_aversion_scores_mean + agency_mean + score_mean + StringencyIndex_WeightedAverage + 
    BACHELOR_S_pp + NO_HEALTH_INSURANCE + BLACK_pp + HISPANIC_pp + 
    HOUSEHOLD_SIZE + HOUSEHOLD_INCOME + EstimatedAverageAge)

norm_model <- gam(formula_test, visits_scores_wk, family = gaussian(), method = "REML")
gam.check(norm_model)