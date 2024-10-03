run_gam_models <- function(dependent_var_list, independent_vars_smooth_base, independent_vars_linear_base, visits_scores_wk, 
name_mapping_dep, sub_dir) {
    for (dependent_var in dependent_var_list) {
    # Construct the formula
    
    independent_vars_smooth <- independent_vars_smooth_base
    independent_vars_linear <- independent_vars_linear_base
    
    df_lag <- visits_scores_wk %>%
    arrange(MODZCTA, week) %>%
    group_by(MODZCTA) %>%
    mutate(log_y_lag1 = log(lag(!!sym(dependent_var), 1)), 
    log_y_lag2 = log(lag(!!sym(dependent_var), 2))) %>%
    ungroup() 

    smooth_parts <- paste("s(", independent_vars_smooth, ",k=10)",collapse = " + ")
    linear_parts <- paste(independent_vars_linear, collapse = " + ")
    
    formula <- as.formula(paste(dependent_var, "~", smooth_parts, "+", linear_parts, "+ log_y_lag1")) #, "+ y_lag1 + y_lag2"
    
    # Fit the model
    gam_model <- gam(formula, data = df_lag, family = gaussian(link="log"), method = "ML",na.action = na.exclude)
    #scat(link="log") #gaussian() #Gamma
    
    # Capture the model summary
    sum_my_model <- summary(gam_model)
    model_summary <- capture.output(sum_my_model)

    summary_file_name <- paste0("../results/", sub_dir, dependent_var, "_model_summary.txt")
    writeLines(model_summary, summary_file_name)
    ###save the model 

    check_file_name <- paste0("../results/", sub_dir, dependent_var, "_gam_check_plots.png")
    png(check_file_name, width = 16, height = 4, units = "in", res = 300)
    par(mfrow = c(1, 4),oma = c(0, 0, 2, 0))
    gam.check(gam_model)
    mtext(dependent_var, outer = TRUE, cex = 1.5)
    dev.off()

    file_name_model <- paste0("../results/", sub_dir, dependent_var, "_gam_model.RData") # nolint
    save(gam_model, file = file_name_model)
    ########## Residuals ############
    df_with_r <- df_lag %>%
    mutate( DV_r = residuals(gam_model))

    dw_results <- df_with_r %>%
    group_by(MODZCTA) %>%                     # Group by location
    arrange(week) %>%                         # Ensure residuals are ordered by week
    do(tidy(dwtest(as.formula(paste0('DV_r', " ~ 1")), data = .))) %>%  # Apply Durbin-Watson test
    ungroup() 

    residuals_file_name <- paste0("../results/", sub_dir, dependent_var, "_residuals_summary.csv")
    write.csv(dw_results, residuals_file_name)

    }
}   

plot_gam_models <- function(dependent_var_list, independent_vars_smooth_base, independent_vars_linear_base, visits_scores_wk, 
name_mapping_dep, sub_dir) {

    for (dependent_var in dependent_var_list) {
        # Construct the formula (this part seems to be incomplete, add as needed)
        # Read back the models
        file_name_model <- paste0("../results/", sub_dir, dependent_var, "_gam_model.RData")
        load(file_name_model)
        # Plotting
        df_lag <- visits_scores_wk %>%
            arrange(MODZCTA, week) %>%
            group_by(MODZCTA) %>%
            mutate(log_y_lag1 = log(lag(!!sym(dependent_var), 1)), 
            log_y_lag2 = log(lag(!!sym(dependent_var), 2))) %>%
            ungroup() 
        df_lag <- df_lag %>% na.omit()
        num_terms <- length(c(independent_vars_smooth, independent_vars_linear))+ 1
        file_name <- paste0("../results/", sub_dir, dependent_var, "_gam_model_plot.png")
        # opne the png device
        png(file_name, width = 800, height = num_terms * 400, res = 300)
        # set up the plotting layout, one more for title
        par(mfrow=c(num_terms+1, 1))
        # placeholder
        dependent_var_displace <- name_mapping_dep[dependent_var]
        sum_my_model <- summary(gam_model)
        title_text <- paste(dependent_var_displace, "\nR-squared:", round(sum_my_model$r.sq, 3))
    # Use the first "plot" as a title
        par(mar=c(0, 0, 4, 0))
        plot(1, type="n", axes=FALSE, ann=FALSE)  # Create an empty plot
        title(main = title_text, cex.main = 1)  # Add the title
        #### each term plot ####
        par(mar=c(4, 2, 1, 2))
        for(i in 1:num_terms) {
            plot(gam_model, shade = TRUE, shade.col = "gray", select = i, all.terms = TRUE, main = "")
            }
        dev.off()
    ##################################################
        # Predict and update dataframe
        predictions <- predict(gam_model, newdata = df_lag, type = "response")
        df_lag$predicted <- predictions
    
    # Plot true vs predicted
    # Remove rows with any NA values by lagging
    
        df <- df_lag[, c("week", "predicted", dependent_var, "MODZCTA")]
        df_long <- melt(df, id.vars = c("week", "MODZCTA"))
    
        color_mapping <- c(predicted = "red")
        color_mapping[dependent_var] <- "blue"
    
        p <- ggplot(df_long, aes(x = week, y = value, color = variable)) +
            geom_line() +
            facet_wrap(~MODZCTA, scales = "free_y") +
            theme_minimal() +
            labs(title = paste("True vs Predicted Values by MODZCTA for", dependent_var),
           x = "Week", y = "Value") +
      scale_color_manual(values = color_mapping)
    
    ggsave(paste0("../results/", sub_dir, dependent_var, "_true_vs_predicted.png"), plot = p, width = 10, height = 8)
    }
}   


dependence_terms <- function(dependent_var_list,independent_vars_linear_base, visits_scores_wk, sub_dir) {
    for (dependent_var in dependent_var_list) {
        # Read back the models
        file_name_model <- paste0("../results/", sub_dir, dependent_var, "_gam_model.RData")
        load(file_name_model)
        
        predictions <- predict(gam_model, type = "terms", se.fit = TRUE)

        results_list <- list()
        for (variable_name in independent_vars_linear_base) {
            print(variable_name)
            x_effect <- predictions$fit[, variable_name]
            x_se <- predictions$se.fit[, variable_name]
            df <- data.frame(
            x_original = visits_scores_wk[[variable_name]],  
            fit = x_effect,
            upper = x_effect + 2 * x_se,
            lower = x_effect - 2 * x_se
            )
            results_list[[variable_name]] <- df
            }
        # Plotting
        combined_results <- bind_rows(results_list, .id = "variable")

        file_name <- paste0("../results/", sub_dir, dependent_var, "_linear_term_dependence.csv")
        write.csv(combined_results, file_name)
    }
}