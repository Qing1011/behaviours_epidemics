library(ggplot2)
library(mgcv)
library(reshape2)
library(magick)

visits_scores_wk <- read.csv(file.choose())
#visits_scores_wk$agency <- visits_scores_wk$rescale_avail + visits_scores_wk$rescale_avail


dependent_var_list <- c('Glocery.Pharmacies_visits_weekly', 'Retails_visits_weekly', 
                        'Arts.Entertainment_visits_weekly', 'Restaurants.Bars_visits_weekly',
                        'Educations_visits_weekly', 'Healthcares_visits_weekly',
                        'others_visits_weekly')
#,"BLACK","HISPANIC"

independent_vars_smooth = c("CASE_COUNT","DEATH_COUNT","week") 
# Variables to include with smoothing
independent_vars_linear = c("gain_loss_bias",
                            "agency", "score","HOUSEHOLD_SIZE", "HOUSEHOLD_INCOME", 
                            "EstimatedAverageAge",'StringencyIndex_WeightedAverage',
                            "BACHELOR_S", "NO_HEALTH_INSURANCE","POPULATION") 

independent_var_list = c("week","borough_case_count_log",
                         "regulated_loss_median",'regulated_agency_median', "regulated_scores_median",
                         'StringencyIndex_WeightedAverage',"BACHELOR_S_pp", "NO_HEALTH_INSURANCE","BLACK_pp",
                         "HISPANIC_pp","HOUSEHOLD_SIZE", "HOUSEHOLD_INCOME", 
                         "EstimatedAverageAge","no_vehciles_perhousehold",
                         'zip_char','DEATH_COUNT_log') 

## standarised
dependent_var_list <- c('Glocery.Pharmacies_visits_weekly_pp', 'Retails_visits_weekly_pp', 
                        'Arts.Entertainment_visits_weekly_pp', 'Restaurants.Bars_visits_weekly_pp',
                        'Educations_visits_weekly_pp', 'Healthcares_visits_weekly_pp')
#,"BLACK","HISPANIC"
# Variables to include with smoothing "CASE_COUNT_log", 
#  "BACHELOR_S_pp", "NO_HEALTH_INSURANCE","BLACK_pp","HISPANIC_pp""HOUSEHOLD_SIZE", "HOUSEHOLD_INCOME", 
# "EstimatedAverageAge", "DEATH_COUNT_log"
#independent_vars_smooth_base = c("week") 
independent_vars_smooth_base = c("week","borough_case_count_log",'DEATH_COUNT_log') 
independent_vars_linear_base = c("regulated_loss_median",'regulated_agency_median', "regulated_scores_median",
                            'StringencyIndex_WeightedAverage',"BACHELOR_S_pp", "NO_HEALTH_INSURANCE","BLACK_pp",
                            "HISPANIC_pp","HOUSEHOLD_SIZE", "HOUSEHOLD_INCOME", 
                            "EstimatedAverageAge","no_vehciles_perhousehold") 

independent_counts <- list(Glocery.Pharmacies_visits_weekly_pp = 'GROCERY_COUNT', 
                           Retails_visits_weekly_pp = 'RETAIL_COUNT', 
                           Arts.Entertainment_visits_weekly_pp = 'ENTERTAINMENT_COUNT', 
                           Restaurants.Bars_visits_weekly_pp = 'RESTAURANT_COUNT',
                           Educations_visits_weekly_pp = 'EDUCATION_COUNT', 
                           Healthcares_visits_weekly_pp = 'HEALTHCARE_COUNT')


plot_gam_models <- function(dependent_var_list, independent_vars_smooth_base, independent_vars_linear_base, visits_scores_wk, count_smooth) {
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
    
    smooth_parts <- paste("s(", independent_vars_smooth, ",k=11)", collapse = " + ")
    linear_parts <- paste(independent_vars_linear, collapse = " + ")
    formula <- as.formula(paste(dependent_var, "~", smooth_parts, "+", linear_parts))
    #"cb.case_borough + cb.death +",
    # Fit the model
    gam_model <- gam(formula, data = visits_scores_wk, family = gaussian(),method = "REML")
    gam.check(gam_model)
    
    # Capture the model summary
    sum_my_model <- summary(gam_model)
    model_summary <- capture.output(sum_my_model)
    summary_file_name <- paste0(dependent_var, "_model_summary.txt")
    writeLines(model_summary, summary_file_name)
    
    # Prepare for plotting
    num_terms <- length(c(independent_vars_smooth, independent_vars_linear))
    file_name <- paste0(dependent_var, "_gam_model_plot.png")
    png(file_name, width = 800, height = num_terms * 400, res = 300)
    
    par(mfrow=c(num_terms+1, 1))
    title_text <- paste(dependent_var, "\nR-squared:", round(sum_my_model$r.sq, 3))
    # Use the first "plot" as a title
    par(mar=c(0, 0, 4, 0))
    plot(1, type="n", axes=FALSE, ann=FALSE)  # Create an empty plot
    title(main = title_text, cex.main = 1)  # Add the title
    
    par(mar=c(5, 2, 0, 2))
    for(i in 1:num_terms) {
      plot(gam_model, select = i, all.terms = TRUE, main = paste("Term", i))
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
    
    ggsave(paste0(dependent_var, "_true_vs_predicted.png"), plot = p, width = 10, height = 8)
    }
}

plot_gam_models(dependent_var_list, independent_vars_smooth_base, independent_vars_linear_base, visits_scores_wk,count_smooth="NONE")

png_files <- paste0(dependent_var_list, "_gam_model_plot.png")
images <- list()

# Read the images, with a check to ensure each file is successfully read
for (file_name in png_files) {
  if (file.exists(file_name)) {
    images[[file_name]] <- image_read(file_name)
  } else {
    warning(paste("File not found:", file_name))
  }
}

# Ensure there are images to combine
if (length(images) > 0) {
  # Combine images horizontally
  combined_image <- image_append(image_join(images), stack = FALSE)
  
  # Save the combined image
  image_write(combined_image, "combined_image.png")
} else {
  warning("No images were read successfully.")
}





par(mfrow = c(1, length(dependent_var_list)))

for (var in dependent_var_list) {
  hist(visits_scores_wk[[var]], main = var, xlab = var)
}

# Reset to default plotting layout
par(mfrow = c(1, 1))

par(mfrow = c(1, length(independent_vars_smooth_base)))

for (var in independent_vars_smooth_base) {
  hist(visits_scores_wk[[var]], main = var, xlab = var)
}

# Reset to default plotting layout
par(mfrow = c(1, 1))


