plot_gam_models <- function(dependent_var_list, independent_vars_smooth_base, independent_vars_linear_base, visits_scores_wk, 
name_mapping_dep, sub_dir) {
    for (dependent_var in dependent_var_list) {
    # Construct the formula
    
    independent_vars_smooth <- independent_vars_smooth_base
    independent_vars_linear <- independent_vars_linear_base
    
    df_lag <- visits_scores_wk %>%
    arrange(MODZCTA, week) %>%
    group_by(MODZCTA) %>%
    mutate(y_lag1 = log(lag(!!sym(dependent_var), 1)), 
    y_lag2 = log(lag(!!sym(dependent_var), 2))) %>%
    ungroup()


    smooth_parts <- paste("s(", independent_vars_smooth, ",k=13)",collapse = " + ")
    linear_parts <- paste(independent_vars_linear, collapse = " + ")
    
    formula <- as.formula(paste(dependent_var, "~", smooth_parts, "+", linear_parts, "+ y_lag1")) #, "+ y_lag1 + y_lag2"
    
    # Fit the model
    gam_model <- gam(formula, data = df_lag, family = gaussian(link='log'), method = "ML",na.action = na.exclude)
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

    file_name_model <- paste0("../results/", sub_dir, dependent_var, "_gam_model.RData")
    save(gam_model, file = file_name_model)
   
    ############# Prepare for plotting ##############
    num_terms <- length(c(independent_vars_smooth, independent_vars_linear))

    file_name <- paste0("../results/", sub_dir, dependent_var, "_gam_model_plot.png")
    # opne the png device
    png(file_name, width = 800, height = num_terms * 400, res = 300)
    # set up the plotting layout, one more for title
    par(mfrow=c(num_terms+1, 1))
    # placeholder
    dependent_var_displace <- name_mapping_dep[dependent_var]
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
    df_lag <- df_lag %>% na.omit()
    
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


plot_scores<-function(dependent_var_list,independent_vars_linear_base,sub_dir){
  for (dependent_var in dependent_var_list) {
    # Read the images, with a check to ensure each file is successfully read
    file_name <- paste0("../results/", sub_dir, dependent_var, "_gam_model.RData")
    load(file_name)
    save_name <- paste0("../results/", sub_dir, dependent_var, "_score_plot.png")
    png(save_name, width = 700, height = 1600, res = 300)
    ##### title plot ####
    par(mfrow=c(3, 1))
    par(mar=c(4, 2, 1, 1))
    for(i in 3:5) {
      term_name <- independent_vars_linear_base[i]
      plot(gam_model, select = i, all.terms = TRUE, main = "")
    }
    dev.off()
  }
}
#shade = TRUE, shade.col = "blue",


plot_zipcodes_for_multiple_vars <- function(data, selected_zipcodes, dependent_var_list,sub_dir) {
  plots <- list() # List to store each plot
  for (dependent_var in dependent_var_list) {
    # Read the images, with a check to ensure each file is successfully read
    file_name <- paste0("../results/", sub_dir, dependent_var, "_gam_model.RData")
    load(file_name)
  # Generate predictions only once for efficiency
    df_lag <- data %>%
    arrange(MODZCTA, week) %>%
    group_by(MODZCTA) %>%
    mutate(y_lag1 = lag( {{ dependent_var }}, 1), y_lag2 = lag({{ dependent_var }}, 2)) %>%
    ungroup()
    predictions <- predict(gam_model, newdata = df_lag, type = "response")
    data$predicted <- predictions
    # Define color mapping
    color_mapping <- c(predicted = "red")
    color_mapping[dependent_var] <- "blue"
    # Filter data for selected zipcodes
    data_selected <- data[data$MODZCTA %in% selected_zipcodes, ]
    # Prepare data for plotting
    df <- data_selected[, c("week", "predicted", dependent_var, "MODZCTA")]
    df_long <- melt(df, id.vars = c("week", "MODZCTA"), variable.name = "variable", value.name = "value")
    # Generate the plot for current dependent variable
    p <- ggplot(df_long, aes(x = week, y = value, color = variable)) +
      geom_line() +
      facet_wrap(~MODZCTA, scales = "free_y", nrow = 5) +
      theme_minimal() +
      labs(x = "Week", y = "Visits per 100 populations") +
      scale_color_manual(values = color_mapping) +
      theme(legend.position = "none",   
      text = element_text(size = 8),  # Base text size for all text in the plot
    axis.title = element_text(size = 8),  # Change axis titles font size
    axis.text = element_text(size = 6),  # Change axis text font size
      )
    
    plots[[dependent_var]] <- p
  }
  
  # Combine all plots into a single figure with 4 rows and 6 columns
  combined_plot <- marrangeGrob(plots, nrow = 1, ncol = 6)
  
  # Save the combined plot to a PDF file
  image_name <- paste0("../results/", sub_dir, "combined_true_vs_predicted.png")
  ggsave(image_name, combined_plot, width = 7, height = 5.5, device = 'png')
}
