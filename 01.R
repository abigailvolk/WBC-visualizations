#### Load Packages ####
packages <-  c("tidyverse", "tidyquant", "gridExtra")
lapply(packages, library, character.only = T)


#### Read in CSV and split into historic and future ####
daily <- read_csv("Daily_streamflow.csv") %>% 
  select(c("date", "model", "daily_cfs", "rcp"))
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






#### Inspiration ####
# source : https://www.r-bloggers.com/2017/07/tidy-time-series-analysis-part-1/

#### Figure inspiration, written by David Thoma

##################ensemble data frames###########################
ens_daily <- compiled %>%
  group_by(rcp, date) %>%
  dplyr::summarize(modrunoff = mean(modrunoff, na.rm = TRUE),quick = mean(quick, na.rm = TRUE),
                   slow = mean(slow, na.rm = TRUE),total = mean(total, na.rm = TRUE))
ens_daily<-as.data.frame(ens_daily);head(ens_daily)

ens_monthly <- monthly_df %>%
  group_by(rcp, yr_mo) %>%
  dplyr::summarize(modrunoff = mean(modrunoff, na.rm = TRUE),quick = mean(quick, na.rm = TRUE),
                   slow = mean(slow, na.rm = TRUE),total = mean(total, na.rm = TRUE))
ens_monthly<-as.data.frame(ens_monthly);head(ens_monthly)

ens_annual <- annual_df %>%
  group_by(rcp, yr) %>%
  dplyr::summarize(modrunoff = mean(modrunoff, na.rm = TRUE),quick = mean(quick, na.rm = TRUE),
                   slow = mean(slow, na.rm = TRUE),total = mean(total, na.rm = TRUE))
ens_annual<-as.data.frame(ens_annual);head(ens_annual)

ens_annual_45<-subset(ens_annual, rcp == 45)
ens_annual_45$yr<-as.numeric(ens_annual_45$yr)#http://127.0.0.1:25719/graphics/plot_zoom_png?width=1920&height=1137
head(ens_annual_45); str(ens_annual_45)#http://127.0.0.1:25719/graphics/plot_zoom_png?width=1920&height=1137

ens_annual_85<-subset(ens_annual, rcp == 85)
ens_annual_85$yr<-as.numeric(ens_annual_85$yr)
head(ens_annual_85); str(ens_annual_85)#http://127.0.0.1:8693/graphics/plot_zoom_png?width=1920&height=1137
###########################################################################
#plotting
###########################################################################
head(annual_df);tail(annual_df)
madf<-melt(annual_df, id.vars=c("model", "yr", "rcp"), measure.vars=c("total"));head(madf)
madf_sub<-subset(madf, model == c("Historical","BNU-ESM_rcp45"));head(madf_sub)#for plotting just one future with historical
madf_sub<-subset(madf, model == c("Historical"));head(madf_sub)#for plotting just one future with historical
#remove historical from futures for plotting and color coding separately
madf2<-subset(madf, model !="Historical");unique(madf2$model)
hist_sub<-subset(madf, model =="Historical")#pull historical data for plotting separately if needed

plot<-ggplot(data = madf2, aes(x=yr, y = value, color=model)) + geom_line()+#scale_color_viridis(discrete=TRUE)+#scale_fill_gradientn(colours=c("black","gray"))+#, alpha=westus_3_hs
  geom_line(data = ens_annual_85, aes(x=yr, y = total), col="red", size = 2)+#facet_wrap(~model)+ ylab("Stream flow (mm)") + xlab("Year")+
  geom_line(data = ens_annual_45, aes(x=yr, y = total), col="blue", size = 2)+
  geom_line(data = madf_sub, aes(x=yr, y=value), col = "black", size = 2)+
  #facet_wrap()
  xlab("Year")+ ylab("Annual flow (mm)")+
  ggtitle("Wet Beaver Creek annual flow")+
  #color=NAor lwd = 0.01
  nps_theme2()#;plot+
plot

plot(ens_annual_45$yr, ens_annual_45$total, type = "l")


