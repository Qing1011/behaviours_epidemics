plot_gam_models <- function(dependent_var_list, independent_vars_smooth_base, independent_vars_linear_base, visits_scores_wk, 
name_mapping_dep) {
    for (dependent_var in dependent_var_list) {
    # Construct the formula
    
    independent_vars_smooth<- independent_vars_smooth_base
    independent_vars_linear <- independent_vars_linear_base
    
    #",k=12)",
    smooth_parts <- paste("s(", independent_vars_smooth, ",k=6)",collapse = " + ")
    linear_parts <- paste(independent_vars_linear, collapse = " + ")
    formula <- as.formula(paste(dependent_var, "~", smooth_parts, "+", linear_parts))

    # Fit the model
    gam_model <- gam(formula, data = visits_scores_wk, family = gaussian(),method = "REML")



    
    # Capture the model summary
    sum_my_model <- summary(gam_model)
    model_summary <- capture.output(sum_my_model)

    summary_file_name <- paste0("../results/", dependent_var, "_model_summary.txt")
    writeLines(model_summary, summary_file_name)
        ###save the model 
    # check_file_name <- paste0(dependent_var, "gam_check_plot_%03d.png")
    # png(check_file_name, width = 800, height = 400, res = 300)
    check_file_name <- paste0("../results/", dependent_var, "gam_check_plots.pdf")
    pdf(check_file_name, width = 5, height = 4)
    gam.check(gam_model)
    dev.off()

    file_name_model <- paste0("../results/", dependent_var, "_gam_model.RData")
    save(gam_model, file = file_name_model)
   
    # Prepare for plotting
    num_terms <- length(c(independent_vars_smooth, independent_vars_linear))
    file_name <- paste0("../results/", dependent_var, "_gam_model_plot.png")
    png(file_name, width = 800, height = num_terms * 400, res = 300)
    ##### title plot ####
    par(mfrow=c(num_terms+1, 1))
    dependent_var_displace <- name_mapping_dep[dependent_var]
    title_text <- paste(dependent_var_displace, "\nR-squared:", round(sum_my_model$r.sq, 3))
    # Use the first "plot" as a title
    par(mar=c(0, 0, 4, 0))
    plot(1, type="n", axes=FALSE, ann=FALSE)  # Create an empty plot
    title(main = title_text, cex.main = 1)  # Add the title
    #### each term plot ####
    par(mar=c(4, 2, 1, 2))
    for(i in 1:num_terms) {
      plot(gam_model, select = i, all.terms = TRUE, main = "")
    }
    dev.off()
        # Predict and update dataframe
    predictions <- predict(gam_model, newdata = visits_scores_wk, type = "response")
    visits_scores_wk$predicted <- predictions
    
    # Plot true vs predicted
    df <- visits_scores_wk[, c("week", "predicted", dependent_var, "zip_char")]
    df_long <- melt(df, id.vars = c("week", "zip_char"))
    
    color_mapping <- c(predicted = "red")
    color_mapping[dependent_var] <- "blue"
    
    p <- ggplot(df_long, aes(x = week, y = value, color = variable)) +
      geom_line() +
      facet_wrap(~zip_char, scales = "free_y") +
      theme_minimal() +
      labs(title = paste("True vs Predicted Values by Zipcode for", dependent_var),
           x = "Week", y = "Value") +
      scale_color_manual(values = color_mapping)
    
    ggsave(paste0("../results/", dependent_var, "_true_vs_predicted.png"), plot = p, width = 10, height = 8)
    }
}   


plot_scores<-function(dependent_var_list){
  for (dependent_var in dependent_var_list) {
    # Read the images, with a check to ensure each file is successfully read
    file_name <- paste0("../results/", dependent_var, "_gam_model.RData")
    load(file_name)
    save_name <- paste0("../results/", dependent_var, "_score_plot.png")
    png(save_name, width = 700, height = 1600, res = 300)
    ##### title plot ####
    par(mfrow=c(3, 1))
    par(mar=c(4, 2, 1, 1))
    for(i in 3:5) {
      plot(gam_model, select = i, all.terms = TRUE, main = "")
    }
    dev.off()
  }
}


plot_zipcodes_for_multiple_vars <- function(data, selected_zipcodes, dependent_var_list) {
  plots <- list() # List to store each plot
  for (dependent_var in dependent_var_list) {
    # Read the images, with a check to ensure each file is successfully read
    file_name <- paste0("../results/", dependent_var, "_gam_model.RData")
    load(file_name)
  # Generate predictions only once for efficiency
    predictions <- predict(gam_model, newdata = data, type = "response")
    data$predicted <- predictions
    # Define color mapping
    color_mapping <- c(predicted = "red")
    color_mapping[dependent_var] <- "blue"
    # Filter data for selected zipcodes
    data_selected <- data[data$zip_char %in% selected_zipcodes, ]
    # Prepare data for plotting
    df <- data_selected[, c("week", "predicted", dependent_var, "zip_char")]
    df_long <- melt(df, id.vars = c("week", "zip_char"), variable.name = "variable", value.name = "value")
    # Generate the plot for current dependent variable
    p <- ggplot(df_long, aes(x = week, y = value, color = variable)) +
      geom_line() +
      facet_wrap(~zip_char, scales = "free_y", nrow = 4) +
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
  ggsave("../results/combined_true_vs_predicted.png", combined_plot, width = 7, height = 4.5, device = 'png')
}
