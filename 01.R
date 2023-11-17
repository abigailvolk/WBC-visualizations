#### Load Packages ####
packages <-  c("tidyverse", "tidyquant", "gridExtra")
lapply(packages, library, character.only = T)


windowsFonts("Frutiger LT Std 55 Roman" = windowsFont("Frutiger LT Std 55 Roman"))
fontsize=20
nps_font <- "Frutiger LT Std 55 Roman" ###NPS fonts
nps_theme2 <- function(base_size = fontsize, base_family=nps_font) {
  theme_bw(base_size = base_size, base_family = nps_font) %+replace%
    theme(axis.text.x = element_text(family=nps_font, size = base_size * 0.8),
          complete = TRUE
    )}


#### Read in CSV ####
daily <- read_csv("Daily_streamflow.csv") %>% 
  select(c("date", "yr", "model", "yr_mo", "daily_cfs", "rcp"))



#### Function for Wrangling Daily, Monthly, and Annual ####
# Tidy Evaluation: https://dcl-prog.stanford.edu/tidy-eval-detailed.html 
#' Function for Wrangling Daily, Monthly, and Annual
#' summarizes mean, median, Q05, Q25, Q75, and Q95

wrangle_projections <- function(dataframe, summary_var, ...) {
  #' dataframe = the input dataframe to wrangle. Should be dailys
  #' summary_column = the column you are performing the summary on (tidy eval)
  #' ... = the variables to group_by (tidy eval)
  summary_var <- enquo(summary_var) # tidy eval  
  
  df <- dataframe %>%
  group_by(...) %>%
  dplyr::summarize_at(vars(!! summary_var),
                    list(mean=mean,
                         Q01=~quantile(., probs = 0.01),
                         Q05=~quantile(., probs = 0.05),
                         Q25=~quantile(., probs = 0.25),
                         median=median,
                         Q75=~quantile(., probs = 0.75),
                         Q95=~quantile(., probs = 0.95),
                         Q99=~quantile(., probs = 0.99)))
  split(df, df$rcp)
}

# test of function below
Daily_test <- wrangle_projections(daily, daily_cfs, rcp, date)
Monthly_test <- wrangle_projections(daily, daily_cfs, rcp, yr_mo)
Annual_test <- wrangle_projections(daily, daily_cfs, rcp, yr)


#### Testing wrangling options ####

daily_rcp <- daily %>%
  group_by(rcp, date) %>%
  dplyr::summarize_at(vars(daily_cfs), 
                      list(mean=mean, 
                           Q05=~quantile(., probs = 0.05),
                           Q25=~quantile(., probs = 0.25),
                           median=median, 
                           Q75=~quantile(., probs = 0.75),
                           Q95=~quantile(., probs = 0.95)))
daily_rcp_list <- split(daily_rcp, daily_rcp$rcp)
monthly_rcp <- daily %>%
  group_by(rcp, month(date), year(date)) %>%
  dplyr::summarize_at(vars(daily_cfs), 
                      list(mean=mean, 
                           Q05=~quantile(., probs = 0.05),
                           Q25=~quantile(., probs = 0.25),
                           median=median, 
                           Q75=~quantile(., probs = 0.75),
                           Q95=~quantile(., probs = 0.95))) %>% 
  mutate(date = make_date(`year(date)`, `month(date)`)) %>% 
  select(!c(`year(date)`))
monthly_rcp_list <- split(monthly_rcp, monthly_rcp$rcp)
annual_rcp <- daily %>%
  group_by(rcp, year(date)) %>%
  dplyr::summarize_at(vars(daily_cfs), 
                      list(mean=mean, 
                           Q05=~quantile(., probs = 0.05),
                           Q25=~quantile(., probs = 0.25),
                           median=median, 
                           Q75=~quantile(., probs = 0.75),
                           Q95=~quantile(., probs = 0.95))) %>% 
  rename(date = `year(date)`)
annual_rcp_list <- split(annual_rcp, annual_rcp$rcp)

#### function to grab the `...` inputs
test <- function(...){
  output <- list(...)
  if ("x" %in% output) {
    print(output)
  } 
}
test("y", "z")
test("x", "z")



#### New function customizable ####

graph_timeseries_quantile <- function(ts_list,
                                      time_step,
                                      hist_rcp_name = "Hist",
                                      rcp = "45",
                                      ylow = Q05,
                                      yhigh = Q95,
                                      xaxis = date,
                                      ysmooth = mean,
                                      titles = F,
                                      nps = F,
                                      proj_col = "red"
                                      ) {
  
  df_hist <- ts_list[[hist_rcp_name]]
  df_rcp <- ts_list[[rcp]]
  get_label <- function(x) {
    x <- rlang::as_label(x)
    switch(x,
           "Q01" = "1st percentile",
           "Q05" = "5th percentile",
           "Q25" = "25th percentile",
           "median" = "median",
           "mean" = "mean",
           "Q75" = "75th percentile",
           "Q95" = "95th percentile",
           "Q99" = "99th percentile",
           "unknown percentile"
    )
  }
  
  labels <- c(
    ylow = get_label(enquo(ylow)),
    yhigh = get_label(enquo(yhigh)),
    ysmooth_fut = paste("projected ensemble", get_label(enquo(ysmooth))),
    ysmooth = get_label(enquo(ysmooth))
  )
  
  pal_color <- c(
    ylow = "dodgerblue4",
    yhigh = "aquamarine",
    ysmooth = "black",
    ysmooth_fut = proj_col
  )
  
  pal_lty <- c(
    ylow = 2,
    yhigh = 3,
    ysmooth = 1,
    ysmooth_fut = 1
  )
  
  p <- df_hist %>% 
    ggplot(aes(x = {{ xaxis }}, y = {{ ysmooth }})) +
    geom_line(aes(y = {{ ylow }}, color = "ylow", lty = "ylow"), lwd = 1) +
    geom_line(aes(y = {{ yhigh }}, color = "yhigh", lty = "yhigh"), lwd = 1) +
    geom_ribbon(aes(x = {{ xaxis }}, ymin = {{ ylow }}, ymax = {{ yhigh }}),
                fill = "#E0EEEE", alpha = 0.5
    ) +
    geom_smooth(method = "loess", se = F, col = "gray") +
    geom_line(aes(color = "ysmooth", lty = "ysmooth"), lwd = 1) +
    
    
    geom_ribbon(data = df_rcp, 
                aes(x={{xaxis}}, ymin = {{ylow}}, ymax = {{yhigh}}), 
                fill = "#E0EEEE", alpha = 0.5) +
    geom_smooth(data = df_rcp, 
                aes(x={{xaxis}}, y = {{ysmooth}}), 
                method = "loess", se=F, col="gray") +
    geom_line(data = df_rcp, aes(x={{xaxis}}, y =  {{ysmooth}}, 
                                 color = "ysmooth_fut", 
                                 lty = "ysmooth_fut"), lwd = 1)+
    geom_line(data = df_rcp, 
              aes(y={{ylow}}, color="ylow", lty="ylow"), lwd=1) +
    geom_line(data = df_rcp, 
              aes(y={{yhigh}}, color="yhigh", lty="yhigh"), lwd=1) +
    theme_bw() +
    scale_color_manual(
      name = "Legend",
      limits = rev,
      labels = labels,
      values = pal_color) +
    scale_linetype_manual(
      name = "Legend",
      limits = rev,
      labels = labels,
      values = pal_lty) +
    labs(
      x = "Year", y = "Flow (cfs)",
    ) 
  if(nps == T & titles == T) {
      p <- p + 
        nps_theme2() + labs(
          title = paste("Historical and Projected RCP", rcp, time_step, "Streamflow") 
        )
  } else if(nps == T & titles == F) {
      p <- p + nps_theme2()
  } else if(nps == F & titles == T) {
      p <- p + 
        labs(
          title = paste("Historical and Projected RCP", rcp, time_step, "Streamflow") 
        )
  } else{
    p
  }
  return(p)
}

graph_timeseries_quantile(Annual_test, time_step = "Annual", xaxis = yr, nps = F, proj_col = "pink")
graph_timeseries_quantile(Annual_test, time_step = "Annual", rcp = "85", xaxis = yr)


timeseries_quantile_rcp_grid <- function(ts_list,
                                time_step,
                                hist_rcp_name = "Hist",
                                ylow = Q05,
                                yhigh = Q95,
                                xaxis = date,
                                ysmooth = mean,
                                titles = F,
                                nps = F,
                                rcp_45_col = "orange",
                                rcp_85_col = "red") {
  plot_1 <- graph_timeseries_quantile(ts_list,
                                      time_step,
                                      rcp = "45",
                                      hist_rcp_name,
                                      {{ylow}},
                                      {{yhigh}},
                                      {{xaxis}},
                                      {{ysmooth}},
                                      titles = titles,
                                      nps = nps,
                                      proj_col = rcp_45_col)
  plot_2 <- graph_timeseries_quantile(ts_list,
                                      time_step,
                                      rcp = "85",
                                      hist_rcp_name,
                                      {{ylow}},
                                      {{yhigh}},
                                      {{xaxis}},
                                      {{ysmooth}},
                                      titles = titles,
                                      nps = nps,
                                      proj_col = rcp_85_col)
  gridExtra::grid.arrange(plot_1, plot_2, ncol=2)
}
timeseries_quantile_rcp_grid(Annual_test, "Annual", xaxis = yr, nps = T)


#### Graph Prototype ####
annual_rcp_list$Hist %>% ggplot(aes(x=date, y = mean)) + 
  geom_line(aes(y=Q05, color="5th Percentile", lty="5th Percentile"), lwd=1) +
  geom_line(aes(y=Q95, color="95th Percentile", lty="95th Percentile"), lwd=1) +
  geom_ribbon(aes(x=date, ymin = Q05, ymax = Q95), fill = "#E0EEEE", alpha = 0.5) +
  geom_smooth(method = "loess", se=F, col="gray")+
  geom_line(aes(color = "Annual Mean Historical", lty = "Annual Mean Historical"), lwd=1) + 
  # RCP lines and ribbon
  geom_ribbon(data = annual_rcp_list$`45`, aes(x=date, ymin = Q05, ymax = Q95), fill = "#E0EEEE", alpha = 0.5) +
  geom_smooth(data = annual_rcp_list$`45`, aes(x=date, y = mean), method = "loess", se=F, col="gray") +
  geom_line(data = annual_rcp_list$`45`, aes(x=date, y = mean, color = "Projected Ensemble Annual Mean", lty = "Projected Ensemble Annual Mean"), lwd = 1)+
  geom_line(data = annual_rcp_list$`45`, aes(y=Q05, color="5th Percentile", lty="5th Percentile"), lwd=1) +
  geom_line(data = annual_rcp_list$`45`, aes(y=Q95, color="95th Percentile", lty="95th Percentile"), lwd=1) +
  theme_bw() +
  scale_color_manual(name = "Legend", 
                     values = c("5th Percentile" = "dodgerblue4", 
                                "95th Percentile" = "aquamarine",
                                "Projected Ensemble Annual Mean" = "red",
                                "Annual Mean Historical" = "black")) +
  scale_linetype_manual(name = "Legend",
                        values = c("5th Percentile" = 3,
                                   "95th Percentile" = 3,
                                   "Projected Ensemble Annual Mean" = 1,
                                   "Annual Mean Historical" = 1)) + 
  labs(x="Year", y="Annual Flow (cfs)", title=paste0("Annual Historical and RCP4.5 Projected Streamflow for WBC")) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=10),
        legend.title.align=0.5)





