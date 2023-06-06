library(ragg)
library(fpp3)
library(tidyverse)
library(scales)
library(extrafont)

loadfonts(device = "win", quiet = TRUE) 

# Load data
rheum <- read_csv("data/rheum.csv")

# Format and pivot
rheum$Date <- yearmonth(as.Date(rheum$Date, format = "%d/%m/%Y"))

rheum <- 
  rheum %>% 
  pivot_longer(cols = -Date, names_to = "Trust", values_to = "Referrals")

rheum$Referrals <- as.numeric(rheum$Referrals)

# Assumption 1: RFL between Aug21 - Dec-21
rheum2a <-
  rheum %>%
  filter(Trust == "RFL", Date >= yearmonth("2021-11-01"), Date <yearmonth("2022-03-01")) %>%
    mutate(Referrals = Referrals * 1.8)

# # Not march-22, especially high.
 rheum2b <-
   rheum %>%
   filter(Trust == "Whitt", Date == yearmonth("2022-03-01")) %>%
   mutate(Referrals = Referrals * 0.66)


# Update
#rheuma <- rows_update(rheum2a, rheum2b, unmatched = "ignore", by= c("Date", "Trust"))
rheum <- rows_update(rheum, rheum2a, unmatched = "ignore", by= c("Date", "Trust"))
rheum <- rows_update(rheum, rheum2b, unmatched = "ignore", by= c("Date", "Trust"))

# check
rheum %>%
  filter(Trust == "RFL", Date >= yearmonth("2021-09-01"), Date <yearmonth("2022-10-01"))

## Assumption 2: replace

## rheum3 <-
#  rheum %>%
#  filter(Trust == "Whitt", Date >= yearmonth("2021-09-01"), Date <yearmonth("2023-04-01")) %>%
#  mutate(Trust = "RFL")

## Update
#rheum <- rows_update(rheum, rheum3, unmatched = "ignore", by= c("Date", "Trust"))


# Make timeseries table
rheum <- as_tsibble(rheum, key = Trust, index = Date)

 # check for missing months
has_gaps(rheum)


# Visualise
rheum %>% autoplot(Referrals)

# Check autocorrelation
rheum %>%  ACF() %>%  autoplot()
rheum %>%  ACF(difference(Referrals)) %>%  autoplot()


mods_rheum <-
  rheum %>% 
  filter(Date >= yearmonth("2021 Jan")) %>% 
  #stretch_tsibble(.init = 10) %>% 
  model(
    mean = MEAN(Referrals),
    naive = NAIVE(Referrals),
    snaive = SNAIVE(Referrals ~ lag("year")),
    drift = RW(Referrals ~ drift()),
    ets = ETS(Referrals),
    ses = ETS(Referrals ~ error("A")+trend("N")+season("N")),
    holt_winter_a = ETS(Referrals ~ error("A")+trend("A")+season("A")),
    holt_winter_ad = ETS(Referrals ~ error("A")+trend("Ad")+season("A"))
    #holt_winter_m = ETS(Referrals ~ error("A")+trend("A")+season("M"))
    #arima = ARIMA(Referrals)
  )

rheum_forecast <-
  mods_rheum %>% 
  forecast(h="60 months") 

# a<- rheum_forecast %>% 
#   accuracy(rheum)  %>% 
#   select(.model, RMSE:MAPE)


rheum_forecast %>% 
  filter(Trust == "RFL") %>% 
  autoplot() +
  facet_wrap(~.model, ncol = 3)


rheum_hw <-
  rheum_forecast %>% 
  hilo(95) %>% 
  filter( .model == "holt_winter_ad") %>% 
  #filter(((Trust %in% c("RFL", "Whitt") & .model == "holt_winter_ad")) | ((!Trust %in% c("RFL", "Whitt")) & .model == "holt_winter_a")) %>% 
  mutate(portion = "Forecast") 


rheum_hw$lcl <- rheum_hw$`95%`$lower
rheum_hw$ucl <- rheum_hw$`95%`$upper


rheum_hw <-
  rheum_hw %>% 
  select(Trust, Referrals, .model, Date, .mean, lcl, ucl)

rheum_trust <-
ggplot(rheum, aes(x= as.Date(Date), col = Trust))+
  geom_line(aes(y=Referrals), linewidth=1)+
  geom_line(aes(y=.mean), data=rheum_hw, linewidth=1, alpha=0.6)+
  geom_smooth(aes(y=.mean), method="lm", data=rheum_hw, linewidth=1
              , se=FALSE, linetype="dashed", alpha=0.6)+
  geom_vline(xintercept = as.Date("01/04/2023", format = "%d/%m/%Y"), col = "red"
             , linewidth = 1, linetype="dashed")+
  #geom_ribbon(data=rheum_hw, aes(ymin = lcl, ymax=ucl, x= Date, fill = Trust)
  #            , alpha=0.5)+
  #scale_y_continuous(breaks = seq(0,1000,200))+
  scale_x_date("Date"
               , date_breaks = "2 month"
               , date_labels = "%b-%y"
               # , limits = c(as.Date("01/08/2020", format = "%d/%m/%Y"),
               #             as.Date("01/03/2028", format = "%d/%m/%Y"))
               ,expand = c(0,0)
               , date_minor_breaks =  "2 month"
               
  )+
  labs(title = "Rheumatology Referrals - All providers"
       , subtitle = "Forecast computed by 'Holt-Winters method', based on Apr-21 - Mar-23,
       \nRoyal Free is imputed from Nov-11 - Feb 12, and Whittington in March-22 due to outlier values"
       )+
  theme_minimal()+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle=90, size = 8, colour = "#595959"
                                   , hjust = 1, ),
        text = element_text(family="Mulish"), 
        plot.subtitle = element_text(face="italic", size = 9)
  )

rheum_trust




##### Cross-validation #####


mods2 <-
  rheum %>% 
  filter(Date >= yearmonth("2021 Apr")) %>% 
  stretch_tsibble(.init = 3) %>% 
  model(
    mean = MEAN(Referrals),
    naive = NAIVE(Referrals),
    snaive = SNAIVE(Referrals ~ lag("year")),
    drift = RW(Referrals ~ drift()),
    #ets = ETS(Referrals),
    #ses = ETS(Referrals ~ error("A")+trend("N")+season("N")),
    holt_winter_a = ETS(Referrals ~ error("A")+trend("A")+season("A")),
    holt_winter_m = ETS(Referrals ~ error("A")+trend("A")+season("M"))
    #arima = ARIMA(Referrals ~ pdq(0,1,1) + PDQ(0,1,1))
  )

accuracy(mods2)
