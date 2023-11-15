#### Load Packages ####
packages <-  c("tidyverse", "tidyquant", "gridExtra")
lapply(packages, library, character.only = T)


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





#### Function Prototypes -- Not quite working how I want ####

# Below function works, but is not customizable for different quantiles.
graph_timeseries_quantile <- function(ts_list,
                                      rcp,
                                      ylow = Q05,
                                      yhigh = Q95,
                                      hist_rcp_name = "Hist",
                                      xaxis = date, 
                                      ysmooth = mean) {
  df_hist <- ts_list[[hist_rcp_name]]
  df_rcp <- ts_list[[rcp]]
  df_hist %>% ggplot(aes(x={{xaxis}}, y = {{ysmooth}})) +
    geom_line(aes(y={{ylow}}, color="5th Percentile", lty="5th Percentile"), lwd=1) +
    geom_line(aes(y={{yhigh}}, color="95th Percentile", lty="95th Percentile"), lwd=1) +
    geom_ribbon(aes(x={{xaxis}}, ymin = {{ylow}}, ymax = {{yhigh}}), fill = "#E0EEEE", alpha = 0.5) +
    geom_smooth(method = "loess", se=F, col="gray")+
    geom_line(aes(color = "Annual Mean Historical", lty = "Annual Mean Historical"), lwd=1) +
    # RCP lines and ribbon
    geom_ribbon(data = df_rcp, aes(x={{xaxis}}, ymin = {{ylow}}, ymax = {{yhigh}}), fill = "#E0EEEE", alpha = 0.5) +
    geom_smooth(data = df_rcp, aes(x={{xaxis}}, y = {{ysmooth}}), method = "loess", se=F, col="gray") +
    geom_line(data = df_rcp, aes(x={{xaxis}}, y = mean, color = "Projected Ensemble Annual Mean", lty = "Projected Ensemble Annual Mean"), lwd = 1)+
    geom_line(data = df_rcp, aes(y={{ylow}}, color="5th Percentile", lty="5th Percentile"), lwd=1) +
    geom_line(data = df_rcp, aes(y={{yhigh}}, color="95th Percentile", lty="95th Percentile"), lwd=1) +
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
}
graph_timeseries_quantile(annual_rcp_list, rcp = "45")
graph_timeseries_quantile(Annual_test, rcp = "45", xaxis = yr)




historic <- daily %>% filter(rcp == "Hist")
projections <- daily %>% filter(rcp == "45" | rcp == "85")
# read_csv reads in as a tibble; detects dates!
historic_y <- historic %>% group_by(year(historic$date)) %>% 
  dplyr::summarize(annual_cfs = mean(daily_cfs, na.rm = TRUE)) %>% 
  rename(yr = `year(historic$date)`)

#### Getting the Mean, Quantiles, and SD ####
# To get the mean by rcp, for example:
# projections_d <- projections %>% 
#   group_by(rcp, date) %>% 
#   summarize_if(is.numeric, c(mean))

# function to get the mean/stdev/quantiles from source below
custom_stat_fun <- function(x, na.rm = TRUE, ...) {
  # x     = numeric vector
  # na.rm = boolean, whether or not to remove NA's
  # ...   = additional args passed to quantile
  c(mean    = mean(x, na.rm = na.rm),
    stdev   = sd(x, na.rm = na.rm),
    quantile(x, na.rm = na.rm, ...)) 
}

options(digits = 4)
probs <- c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1) # probabilities used in quantiles
calc_y_proj <- projections %>%
  group_by(rcp) %>% # group by rcp
  tq_transmute(
    select = daily_cfs,
    mutate_fun = apply.yearly, # can do this for monthlys and yearlys
    FUN = custom_stat_fun,
    na.rm = TRUE,
    probs = probs
  )
calc_y_proj45 <- calc_y_proj %>% filter(rcp== 45)
calc_y_proj85 <- calc_y_proj %>% filter(rcp== 85)

# Merge the dataframes
# calc_y <- rename(historic, `50%` = daily_cfs) %>% 
#   bind_rows(calc_y_proj, historic)


#### Visualize time series with Quantiles ####
# write function to re-use ggplot

ggplot(historic) +
  geom_ribbon(data = calc_y_proj, aes(date, ymin = `25%`, ymax = `75%`), fill = "blue", alpha = 0.25) +
  geom_line(data = calc_y_proj, aes(date, `50%`), colour = "blue") +
  geom_line(aes(date, daily_cfs))

ggplot(data = madf2, aes(x=yr, y = value, color=model)) + geom_line()+#scale_color_viridis(discrete=TRUE)+#scale_fill_gradientn(colours=c("black","gray"))+#, alpha=westus_3_hs
  geom_line(data = ens_annual_85, aes(x=yr, y = total), col="red", size = 2)+#facet_wrap(~model)+ ylab("Stream flow (mm)") + xlab("Year")+
  geom_line(data = ens_annual_45, aes(x=yr, y = total), col="blue", size = 2)+
  geom_line(data = madf_sub, aes(x=yr, y=value), col = "black", size = 2)+
  #facet_wrap()
  xlab("Year")+ ylab("Annual flow (mm)")+
  ggtitle("Wet Beaver Creek annual flow")+
  #color=NAor lwd = 0.01
  nps_theme2()#;plot+
plot



