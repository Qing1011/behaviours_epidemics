
#######################################
######### Graph-related functions #####
####################################### 

graph_anomalies_indiv <- function (dat_grap){
  des4 <- merge(dat_item, dat_grap, by= "ResponseId") %>% 
    group_by(Residence) %>% 
    count(anomaly) %>% 
    mutate(suma = sum(n), prop = n/suma) %>% 
    ggplot(aes(y = prop,
               x = anomaly,
               group = factor(anomaly),
               colour = factor(anomaly))) +
    geom_jitter(alpha = 0.9,
                size = 7,
                pch=19) +
    stat_summary(fun.data = mean_cl_normal, 
                 geom = "pointrange",
                 size = 1, color = "black") +
    theme_bw(base_size = 10) +
    theme(legend.position = "none")+
    ylab("Proportion of decisions") +
    xlab("Type of decision") +
    scale_colour_manual(values=palette_Dark2(length(table(dat_grap$anomaly))),
                        name = "Type of anomaly") +
    ylim(0,1)
  
  
  des5 <- merge(dat_item, dat_grap, by= "ResponseId") %>%
    filter(str_detect(response, 'ongruent')) %>%
    group_by(Residence) %>% 
    count(response) %>% 
    mutate(suma = sum(n), prop = n/suma) %>% 
    ggplot(aes(y = prop,
               x = response,
               group = factor(response),
               colour = factor(response))) +
    geom_jitter(alpha = 0.9,
                size = 7,
                pch = 19) +
    ylim(0,1)+
    stat_summary(fun.data = mean_cl_normal, 
                 geom = "pointrange",
                 size = 1, 
                 color = "black") +
    theme_bw(base_size = 10) +
    theme(legend.position = "none")+
    ylab("Proportion of total anomalies") +
    xlab("Type of anomaly") +
    #ylim(0,800)+
    scale_colour_manual(values=palette_Dark2(length(table(dat_grap$anomaly))),
                        name = "Type of anomaly")
  
  layout <- c(
    area(t = 1, l = 1, b = 6, r = 3),
    area(t = 1, l = 5, b = 8, r = 8))
  
  des6 <- des4 + des5 + 
    plot_layout(design = layout)
  
  return (des6)
  
}


graph_map <- function (data = NULL, var = "var", lowmin = NULL, upplim = NULL, pal = NULL, col = NULL, title = NULL) {
  mp1 <- ggplot(data) +
    geom_sf(aes_string(fill = var)) +
    coord_sf(datum = NA) +
    
    scale_fill_continuous(low="#ba0404", 
                          high="#080887", 
                          limits=c(lowmin,upplim), na.value = "white") +
    #scale_fill_material(col, na.value = "white")+
    bbc_theme() +
    theme(legend.position="bottom",
          legend.text = element_text(size = 15))+
     ggtitle(paste0(title)) + 
    guides(colour = guide_legend(override.aes = list(size = 5))) +  
    theme(legend.key.size = unit(1.5, "cm"))
    
  return(mp1)
}


graph_map_disc <- function (data = NULL, var,
                            lowmin = NULL, upplim = NULL, 
                            lwd = 1,
                            pal = NULL, 
                            title = NULL) {
  
  mp1 <- ggplot(data) +
    geom_sf(aes_string(fill = var), lwd = lwd, colour = "black") +
    coord_sf(datum = NA) +
    scale_fill_manual(values=pal,
                        name = "Type of anomaly",
                        na.translate = F) +
    bbc_theme() +
    theme(legend.position="bottom",
          legend.text = element_text(face = "bold", size = 9),
          axis.title=element_text(size=10,face="bold"))+
    #ggtitle(paste0(title)) + 
    labs(x = paste0(title)) + 
    guides(colour = guide_legend(override.aes = list(size = 4))) +  
    theme(legend.key.size = unit(.7, "cm"))
  
  return(mp1)
}

ineq_grap <- function (data, var1, var2, rangex1, rangex2, rangey1, rangey2, ylabel, xlabel, val, family = "gaussian") {
  
  p1 <-  data %>% 
    arrange({{var2}}) %>% 
    group_by(Code, Cntry_income) %>% 
    summarise(var1 = mean({{var1}}, na.rm =T), 
              n = n(),
              var2 = mean({{var2}}, na.rm =T)) %>% 
    ungroup() %>% 
    ggplot(aes(x =var1, 
               y =var2)) +
    
    geom_text(aes(label = Code), 
              check_overlap = TRUE, 
              vjust = 2.5, 
              size = 3.5,
              fontface = "bold",
              color = "black") +
    
    geom_point(aes(fill = Cntry_income),
              alpha=0.5, 
               size = 8, 
               shape=21) +
    
    geom_smooth(method = "gam",
                formula = y ~ s(x, bs = "cr"),
                #method.args = list(family = family),
                se = T,
                fullrange = T,
                color = "black", 
                size = 2,
                alpha = .15) +
    
    
    #scale_colour_manual(values=palette_Dark2(length(table(dat_item$Residence))),name = "Type of anomaly")+
    theme(legend.position="top") +
    theme(text = element_text(family="", size=15, face = "bold")) +  
    #geom_abline(intercept = 0, slope = 1, alpha = .90, linetype = "dashed") +
    ylab(ylabel) +
    xlab(xlabel) +
    coord_cartesian(xlim = c(rangex1, rangex2), ylim = c(rangey1, rangey2 )) +
    #ylim(rangey1, rangey2 ) +
    #xlim(rangex1, rangex2 ) +
    scale_fill_manual(values = pal, name = NULL) + 
    bbc_theme() + 
    scale_y_continuous(expand=expansion(mult=c(.15,.02)))
    
  return(p1)
}

ineq_indiv_grap <- function (data, 
                             var1, var2, 
                             rangex1, rangex2, 
                             rangey1, rangey2, 
                             ylabel, xlabel, val, 
                             family = "gaussian") {
  
  p1 <-  data %>% 
    ggplot(aes(x = var1, 
               y = var2,
               fill = Cntry_income)) +
    
    geom_smooth(method = "gam",
                # formula = "x ~ s(y, bs = "bs")",
                method.args = list(family = family),
                se = T,
                fullrange = T,
                color = "black", 
                size = 2,
                alpha = .9) +
    
   # geom_point(aes(fill = Cntry_income),
     geom_point(
               alpha=0.5, 
               size = 10, 
               shape=21 
               # stroke = 1.5, 
               # color="black"
    ) +
    
    #scale_colour_manual(values=palette_Dark2(length(table(dat_item$Residence))),name = "Type of anomaly")+
    theme(legend.position="top") +
    theme(text = element_text(family="", size=15, face = "bold")) +  
    #geom_abline(intercept = 0, slope = 1, alpha = .90, linetype = "dashed") +
    xlab(xlabel) +
    ylab(ylabel) +
    xlim(rangex1, rangex2) +
    ylim(rangey1, rangey2 ) +
    bbc_theme()
  
    # + scale_y_continuous(expand=expansion(mult=c(.25,0)))
    #scale_fill_manual(values = pal, name = NULL) + 
    #+ bbc_theme()
  
  return(p1)
}



bbc_theme <- function (axis_size = 12) {
  
  # Theme based on the themes provided by the bbplot package 
  # bbhttps://github.com/bbc/bbplot
  
  font <- "Verdana"
  ggplot2::theme(plot.title = ggplot2::element_text(family = font, size = 18, face = "bold", color = "#222222"), 
                 plot.subtitle = ggplot2::element_text(family = font, size = 16, margin = ggplot2::margin(9, 0, 9, 0)), 
                 plot.caption = ggplot2::element_blank(), 
                 legend.position = "top", 
                 legend.text.align = 0, 
                 legend.background = ggplot2::element_blank(), 
                 legend.title = ggplot2::element_blank(), 
                 legend.key = ggplot2::element_blank(), 
                 legend.text = ggplot2::element_text(family = font, size = 18, color = "#222222"), 
                 #axis.title = ggplot2::element_blank(), 
                 axis.text = ggplot2::element_text(family = font, size = axis_size, color = "#222222"), 
                 axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)), 
                 axis.ticks = ggplot2::element_blank(), 
                 axis.line = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank(), 
                 panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"), 
                 panel.grid.major.x = ggplot2::element_blank(), 
                 panel.background = ggplot2::element_blank(), 
                 strip.background = ggplot2::element_rect(fill = "white"), 
                 strip.text = ggplot2::element_text(size = 22, hjust = 0))
}




descript_score <- function (data = NULL, ylim2 = ylim2, lab = lab, pal = pal) {
  
  meta <- as.data.frame(data)
  meta$Code <- meta$studlab
  
  cnt <- dat_unique %>% 
    select(Code, Cntry_income) %>% 
    unique()
  
  meta <- merge(meta, cnt)
  
  dat_grap <- meta %>%
    
  select(studlab, Cntry_income, TE, lower, upper, sd) %>%
  mutate(sdlo = TE - sd,
         sdhigh = TE + sd) %>%
  drop_na() %>% 
          ggplot(aes(x = reorder(studlab, -TE), 
                     y = TE,
                     fill = Cntry_income,
                     colour = Cntry_income)) +
          
          
          geom_linerange(size = 1.1, aes(ymin =sdlo, ymax = sdhigh), color='grey') +
          geom_linerange(size = 1.1, aes(ymin = lower, ymax = upper)) +
          
          geom_point(size = 1.8) +
          
          labs(title = paste(lab)) +
          ylab(lab) +
          xlab("Country") +
          bbc_theme(axis_size = 6) +
          theme(legend.position = "none",
                plot.title = element_text(size=10, hjust = 0.5)) +
          coord_flip(ylim = c(0, ylim2))+
          #coord_flip()+
          #ylim(0,ylim2) +
          scale_color_manual(values=pal) 
  
}



descript_anom<- function (data = NULL, ylim2 = ylim2, lab = lab, pal = pal) {
  
  meta <- as.data.frame(data)
  meta$Code <- meta$studlab
  
  cnt <- dat_unique %>% 
    select(Code, Cntry_income) %>% 
    unique()
  
  meta <- merge(meta, cnt)
  
  dat_grap <- meta %>%
    
    select(studlab, event, n, Cntry_income, TE, lower, upper) %>%
    mutate(TE = plogis(TE)) %>% 
   
    drop_na() %>% 
    ggplot(aes(x = reorder(studlab, -TE), 
               y = TE,
               colour = Cntry_income)) +
    
    geom_point(size = 1.8) +
    
    geom_linerange(size = 1.1, aes(ymin = lower, ymax = upper)) +
    #geom_hline(aes(yintercept = mean(TE, na.rm = T), col = "red", size = .5)) +
    
    labs(title = paste(lab)) +
    ylab(lab) +
    xlab("Country") +
    bbc_theme(axis_size = 6) +
    theme(legend.position = "none",
          plot.title = element_text(size=10, hjust = 0.5)) +
    coord_flip()+
    ylim(0,ylim2)+
    scale_color_manual(values=pal,
                       name = "Type of anomaly",
                       na.translate = F) 
  
  return(dat_grap)
}


res_graph <- function (model, var = NULL, ymin, ymax, ylabs, xlabs) {
  
  ci <-  ggpredict(model,
                   terms = var,
                   type = "random",
                   interval = "prediction")
  ggplot(ci, aes(x = x,
                 y = predicted)) +
    geom_ribbon(mapping = aes(ymin = conf.low,
                              ymax = conf.high,
                              x = x),
                alpha=0.4,
                col = "black",
                fill = "#5ca4c4" ,
                inherit.aes = FALSE) +
    geom_line (lwd = 1.5) +
    labs(y = ylabs, x = xlabs) +
    ylim (ymin, ymax) +
    bbc_theme(axis_size = 8) +
    theme(axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"))
}

gam_graph <- function (model, var = NULL, ymin, ymax, ylabs, xlabs) {
 
  ci <-  conditional_smooths(model)[[var]]
  ggplot(ci, aes(x = effect1__ ,
                 y = estimate__)) +
    geom_ribbon(mapping = aes(ymin = lower__,
                              ymax = upper__,
                              x = effect1__),
                alpha=0.4,
                col = "black",
                fill = "#5ca4c4" ,
                inherit.aes = FALSE) +
    geom_line (lwd = 1.5) +
    labs(y = ylabs, x = xlabs) +
    ylim (ymin, ymax) +
    bbc_theme(axis_size = 8) +
    theme(axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"))
}



#######################################
######### Stats-related functions #####
#######################################

get_scores <- function (x) {
  
  ## This function will compute the total temporal discount for each participant based on their responses to each block question
  ##
  ## Input (x): A block of questions (5 previous questions to the one parsed to the function)
  ##
  ## Output: Vector of individual sum responses per block
  
  f <- as.numeric(gsub("block", "", x$block))[1]
  
  if (f == 1) {
    x %>% 
      mutate(score = case_when(
        amount_later == 750 & choice == 0 ~ 5,
        amount_later == 750 & choice == 1 ~ 4,
        amount_later == 600 & choice == 1 ~ 3,
        amount_later == 510 & choice == 0 ~ 2,
        amount_later == 505 & choice == 0 ~ 1,
        amount_later == 505 & choice == 1 ~ 0
      ))
    
  } else if (f == 2) {
    x %>% 
      mutate(score = case_when(
        amount_later == 750 & choice == 1 ~ 0,
        amount_later == 750 & choice == 0 ~ 1,
        amount_later == 600 & choice == 0 ~ 2,
        amount_later == 510 & choice == 1 ~ 3,
        amount_later == 505 & choice == 0 ~ 5,
        amount_later == 505 & choice == 1 ~ 4
      ))
  } else {
    x %>% 
      mutate(score = case_when(
        amount_later == 7500 & choice == 0 ~ 5,
        amount_later == 7500 & choice == 1 ~ 4,
        amount_later == 6000 & choice == 1 ~ 3,
        amount_later == 5100 & choice == 0 ~ 2,
        amount_later == 5050 & choice == 0 ~ 1,
        amount_later == 5050 & choice == 1 ~ 0
      ))
  }
}



get_latest_answer <- function (x, y){
  
  ## This function will obtain the latest answer given to the initial baseline questions. This function is necessary to control for the different possible options shown to each individual.
  ##
  ## Input: (x) A data frame including choices to questions; (y) a scalar identifying the next item following the item block. 
  ##
  ## Output: Vector of individual responses to the anomaly item
  
  x %>% 
    group_by(ResponseId) %>% 
    filter(as.numeric(gsub("Q", "", question)) < y) %>% 
    arrange(desc(as.numeric(gsub("Q", "", question)))) %>% 
    slice(1) %>% 
    ungroup() %>% 
    arrange(ResponseId) %>% 
    dplyr:::select(ResponseId, choice, amount_later, block) 
}

get_second_choices <- function (x, y) {
  
  ## This function will obtain the answer given to the anomalies questions. This function necessary to control for the different possible options shown to each individual
  ##
  ## Input: (x) A data frame including choices to questions; (y) the anomaly item identifier. 
  ##
  ## Output: Vector of individual responses to the anomaly item
  
  x %>% 
    arrange(ResponseId) %>%   
    arrange(desc(as.numeric(gsub("Q", "", question)))) %>%   
    filter(question == y) %>% 
    dplyr:::select(choice) %>% 
    unlist() %>% 
    as.vector()
}


fixer_anom <- function (d, type = NULL){
  
  ## This function will compute whether an anomaly was observed (and whether the initial answer was a sooner or later decision) and if the possible anomaly was congruent with theory or not.
  ##
  ## Input: (x) A data frame including the first choice (fc) and the type of anomaly to be estimated. 
  ##
  ## Output: dataframe including the individual id and the variables response (possible anomaly and initial decision) and anomaly (whether an anomaly or a consistent decision was observed).
  
  if (type == "presbias" || type == "absolmag" || type == "delay") {  
    
    d %>%
      mutate(response = case_when (
        fc == 0 & sc == 0 ~ "No anomaly sooner",
        fc == 1 & sc == 0 ~ "Anomaly later",
        fc == 0 & sc == 1 ~ "Anomaly sooner",
        fc == 1 & sc == 1 ~ "No anomaly later")) %>% 
      mutate (response = as.factor(response)) %>% 
      mutate(anomaly = case_when(
        response == "Anomaly later" ~ "Possible anomaly",
        response == "Anomaly sooner" ~ "Possible anomaly",
        response == "No anomaly later" ~ "Consistent",
        response == "No anomaly sooner" ~ "Consistent"
      )) %>% 
      mutate(anomaly = fct_relevel(anomaly, "Consistent", "Possible anomaly")) %>%
      mutate(response = fct_recode(response, 
                                   "Congruent" = "Anomaly sooner", 
                                   "Not congruent" = "Anomaly later")) %>% 
      mutate(type = rep(type, length(response))) %>% 
      dplyr:::select(ResponseId, response, anomaly, type) 
    
  } else if (type == "gainloss") {  
    
    d %>%
      mutate(response = case_when (
        fc == 0 & sc == 0 ~ "Anomaly sooner",
        fc == 1 & sc == 0 ~ "No anomaly later",
        fc == 0 & sc == 1 ~ "No anomaly sooner",
        fc == 1 & sc == 1 ~ "Anomaly later")) %>% 
      mutate (response = as.factor(response)) %>% 
      mutate(anomaly = case_when(
        response == "Anomaly later" ~ "Possible anomaly",
        response == "Anomaly sooner" ~ "Possible anomaly",
        response == "No anomaly later" ~ "Consistent",
        response == "No anomaly sooner" ~ "Consistent"
      )) %>% 
      mutate(anomaly = fct_relevel(anomaly, "Consistent", "Possible anomaly")) %>% 
      mutate(response = fct_recode(response, 
                                   "Congruent" = "Anomaly sooner", 
                                   "Not congruent" = "Anomaly later")) %>%
      mutate(type = rep(type, length(response))) %>% 
      dplyr:::select(ResponseId, response, anomaly, type) 
    
  }  else if (type == "subaddit") {  
    
    d %>%
      mutate(response = case_when (
        fc == 0 & sc == 0 ~ "No anomaly sooner",
        fc == 0 & sc == 1 ~ "Anomaly sooner",
        fc == 1 & sc == 1 ~ "Inconsistent",
        fc == 1 & sc == 0 ~ "Inconsistent",
        fc == 2 & sc == 0 ~ "Anomaly later",
        fc == 2 & sc == 1 ~ "No anomaly later",
      )) %>% 
      mutate (response = as.factor(response)) %>% 
      mutate(anomaly = case_when(
        response == "Anomaly later" ~ "Possible anomaly",
        response == "Anomaly sooner" ~ "Possible anomaly",
        response == "No anomaly later" ~ "Consistent",
        response == "No anomaly sooner" ~ "Consistent",
        response ==  "Inconsistent" ~ "Inconsistent"
      )) %>% 
      mutate(anomaly = fct_relevel(anomaly, "Consistent", "Possible anomaly", "Inconsistent")) %>% 
      mutate(response = fct_recode(response, 
                                   "Congruent" = "Anomaly sooner", 
                                   "Not congruent" = "Anomaly later")) %>%
      mutate(type = rep(type, length(response))) %>% 
      dplyr:::select(ResponseId, response, anomaly, type)  
    
  }
} 


ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}


#######################################################

fit_models_lmer_ns <- function(data){
  
  ## This function will estimate the main statistical models (i.e., random intercept for country) using glmer.
  ##
  ## Input: (x) A data frame including all predictors and main dependent variable.
  ##
  ## Output: List of results including results from all models.
  
  dv <- names(data)[1]
  formgam <- formula(paste(dv, "~ age.cwc + 
                          Gender + 
                          EducationCompleted + 
                          Employment +
                          indineq.cwc +
                          GDP.gmc  + 
                          GINI.gmc + 
                          
                          s(inflation.gmc, bs = 'cs', k = 4) + 
                          s(debt.cwc, bs = 'cs', k = 4) +
                          s(assets.cwc, bs = 'cs', k = 4)", ""))
  mg1 <- gamm4(formgam,
               random = ~(1|Residence), 
               data = data)
  
  return (list(
    mg1 = mg1
    
  ))
}


fit_models_glmer_ns <- function(data){
  
  ## This function will estimate the main statistical models (i.e., random intercept for country) using glmer.
  ##
  ## Input: (x) A data frame including all predictors and main dependent variable.
  ##
  ## Output: List of results including results from all models.
  
  dv <- names(data)[1]
  formgam <- formula(paste(dv, "~ age.cwc + 
                          Gender + 
                          EducationCompleted + 
                          Employment +
                          indineq.cwc +
                          GDP.gmc  + 
                          GINI.gmc + 
                          debt.cwc +
                          
                          s(inflation.gmc, bs = 'cs', k = 4) + 
                          s(assets.cwc, bs = 'cs', k = 4)", ""))
  mg1 <- gamm4(formgam,
               random = ~(1|Residence),
               family = "binomial",
               data = data)
  
  return (list(
    mg1 = mg1
    
  ))
}



metanal_anom_prop <- function (data = NULL, vars = NULL) {
  
  dat_test <- data %>% 
    group_by(Code) %>% 
    summarise(across({{vars}}, sum),
              n = n()) %>% 
    rename(total = 2) %>% 
    drop_na()
  
  meta <- metaprop(event = total, 
                   n = n, 
                   studlab=Code, 
                   sm="PLOGIT", 
                   data=dat_test, 
                   method="GLMM", 
                   method.tau="ML",
                   overall = T,
                   prediction = T)
} 


bam_estimation <- function (data = data,
                            bs = bs,
                            family = NULL) {
  
  dv <- names(data)[1]
  fm <- as.formula(paste(dv, "~ 
                  s(age.cwc,bs=bs)+
                  Gender+
                  EducationCompleted+
                  Employment+
                  s(indineq.cwc,bs=bs)+
                  
                  s(debt.cwc,bs=bs)+
                  s(assets.cwc,bs=bs)+
                  
                  s(GDP.gmc,bs=bs)+
                  s(GINI.gmc,bs=bs)+
                  s(inflation.gmc,bs=bs)+
                  s(Residence, bs = 're')", ""))
  
  m1 <- bam(fm,    
            data = data,
            family = family)
  
  return (list(
    m1 = m1))
} 



bam_estimation_comparison <- function (data = data,
                                       bs = bs,
                                       family = NULL) {
  
  dv <- names(data)[1]
  fm <- as.formula(paste(dv, "~s(Residence, bs = 're')", ""))
  m1 <- bam(fm,    
            data = data,
            family = family)
  
  fm2 <- as.formula(paste(dv, "~ 
                  s(age.cwc,bs=bs)+
                  Gender+
                  EducationCompleted+
                  Employment+
                  s(indineq.cwc,bs=bs)+
                  
                  s(debt.cwc,bs=bs)+
                  s(assets.cwc,bs=bs)+
                  
                  s(GDP.gmc,bs=bs)+
                  s(GINI.gmc,bs=bs)+
                  s(inflation.gmc,bs=bs)+
                  
                  s(Residence, bs = 're')", ""))
  
  m2 <- bam(fm2,    
            data = data,
            family = family)
  
  fm3 <- as.formula(paste(dv, "~ 
                  s(age.cwc,bs=bs)+
                  Gender+
                  EducationCompleted+
                  Employment+
                  s(indineq.cwc,bs=bs)+
                  
                  s(debt.cwc,bs=bs)+
                  s(assets.cwc,bs=bs)+
                  
                  s(GDP.gmc,bs=bs)+
                  s(GINI.gmc,bs=bs)+
                  s(inflation.gmc,bs=bs)+
                  
                  s(Residence, bs = 're') +
                  s(indineq.cwc, Residence, bs = 're') +
                  s(GINI.gmc, Residence, bs = 're') +
                  s(debt.cwc, Residence, bs = 're') +
                  s(assets.cwc, Residence, bs = 're') +
                  s(inflation.gmc, Residence, bs = 're')", ""))
  
  m3 <- bam(fm3,    
            data = data,
            family = family)
  
  return (list(
    m1 = m1,
    m2 = m2,
    m3 = m3))
} 


fit_models_brm_score_ns <- function (data, dist) {
  
  dv <- names(data)[1]
  
  prior2 <- c(prior(student_t(3, 0, 10), class = Intercept),
              prior(normal(0, 3), class = b),
              prior(exponential(1), class = sds)) 
  
  mg1 <- brm(paste(dv, "~ age.cwc + 
                          Gender + 
                          EducationCompleted + 
                          Employment +
                          indineq.cwc +
                          GDP.gmc  +
                          GINI.gmc + 
                          s(inflation.gmc, bs = 'cr', k = 4) + 
                          s(debt.cwc, bs = 'cr', k = 4) +
                          s(assets.cwc, bs = 'cr', k = 4) +
                          
                          
                         (1|Residence)", ""),  
             
             data = data,
             prior = prior2,
             sample_prior = TRUE,
             save_pars = save_pars(all = T),
             family = dist,
             #double!
             warmup = 1000, iter = 2000, 
             chains = 4,
             cores = 4,
             control = list(adapt_delta = 0.99,
                            max_treedepth = 15),
             backend = "cmdstanr")
  
  return (list(
    mg1 = mg1
  ))
}


fit_models_brm_score_ns_k9 <- function (data, dist) {
  
  dv <- names(data)[1]
  
  prior2 <- c(prior(student_t(3, 0, 10), class = Intercept),
              prior(normal(0, 3), class = b),
              prior(exponential(1), class = sds)) 
  
  mg1 <- brm(paste(dv, "~ age.cwc + 
                          Gender + 
                          EducationCompleted + 
                          Employment +
                          indineq.cwc +
                          GDP.gmc  +
                          GINI.gmc + 
                          s(inflation.gmc, bs = 'cr', k = 4) + 
                          s(debt.cwc, bs = 'cr', k = 4) +
                          s(assets.cwc, bs = 'cr', k = 4) +
                          
                          
                         (1|Residence)", ""),  
             
             data = data,
             prior = prior2,
             sample_prior = TRUE,
             save_pars = save_pars(all = T),
             family = dist,
             #double!
             warmup = 1000, iter = 2000, 
             chains = 4,
             cores = 4,
             control = list(adapt_delta = 0.99,
                            max_treedepth = 15),
             backend = "cmdstanr")
  
  return (list(
    mg1 = mg1
  ))
}


fit_models_brm_anomalies_ns <- function (data, dist) {
  
  dv <- names(data)[1]
  
  prior2 <- c(prior(student_t(1, 0, 10), class = Intercept),
              prior(normal(0, 3), class = b),
              prior(exponential(1), class = sds))  
  
  mg1 <- brm(paste(dv, "~ age.cwc + 
                          Gender + 
                          EducationCompleted + 
                          Employment +
                          indineq.cwc +
                          GDP.gmc  + 
                          GINI.gmc + 
                          debt.cwc +
                          
                          s(inflation.gmc, bs = 'cr', k = 4) + 
                          s(assets.cwc, bs = 'cr', k = 4) +
                          
                          
                         (1|Residence)", ""),  
             
             data = data,
             prior = prior2,
             sample_prior = TRUE,
             save_pars = save_pars(all = T),
             family = dist,
             #double!
             warmup = 500, iter = 1000, 
             chains = 4,
             cores = 4,
             control = list(adapt_delta = 0.99,
                            max_treedepth = 15),
             backend = "cmdstanr")
  
  return (list(
    mg1 = mg1
  ))
}
