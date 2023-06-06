library(ragg)
library(fpp3)
library(tidyverse)
library(scales)
library(extrafont)

loadfonts(device = "win", quiet = TRUE) 

# Load data
pain <- read_csv("data/pain.csv")


# Format and pivot
pain$Date <- yearmonth(as.Date(pain$Date, format = "%d/%m/%Y"))

pain <- 
  pain %>% 
  pivot_longer(cols = -Date, names_to = "Trust", values_to = "Referrals")

pain$Referrals <- as.numeric(pain$Referrals)

# # Assumption 1: RFL between Aug21 - Dec-21
# pain2a <-
#   pain %>%
#   filter(Trust == "RFL", Date >= yearmonth("2021-11-01"), Date <yearmonth("2022-03-01")) %>%
#   mutate(Referrals = Referrals * 1.8)
# 
# # # Not march-22, especially high.
# pain2b <-
#   pain %>%
#   filter(Trust == "Whitt", Date == yearmonth("2022-03-01")) %>%
#   mutate(Referrals = Referrals * 0.66)
# 
# 
# # Update
# #paina <- rows_update(pain2a, pain2b, unmatched = "ignore", by= c("Date", "Trust"))
# pain <- rows_update(pain, pain2a, unmatched = "ignore", by= c("Date", "Trust"))
# pain <- rows_update(pain, pain2b, unmatched = "ignore", by= c("Date", "Trust"))
# 
# # check
# pain %>%
#   filter(Trust == "RFL", Date >= yearmonth("2021-09-01"), Date <yearmonth("2022-10-01"))

## Assumption 2: replace

## pain3 <-
#  pain %>%
#  filter(Trust == "Whitt", Date >= yearmonth("2021-09-01"), Date <yearmonth("2023-04-01")) %>%
#  mutate(Trust = "RFL")

## Update
#pain <- rows_update(pain, pain3, unmatched = "ignore", by= c("Date", "Trust"))


# Make timeseries table
pain <- as_tsibble(pain, key = Trust, index = Date)

# check for missing months
has_gaps(pain)


# Visualise
pain %>% autoplot(Referrals)

# Check autocorrelation
pain %>%  ACF() %>%  autoplot()
pain %>%  ACF(difference(Referrals)) %>%  autoplot()


mods_pain <-
  pain %>% 
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

pain_forecast <-
  mods_pain %>% 
  forecast(h="60 months") 

# a<- pain_forecast %>% 
#   accuracy(pain)  %>% 
#   select(.model, RMSE:MAPE)


pain_forecast %>% 
  filter(Trust == "RFL") %>% 
  autoplot() +
  facet_wrap(~.model, ncol = 3)


pain_hw <-
  pain_forecast %>% 
  hilo(95) %>% 
  filter( .model == "holt_winter_ad") %>% 
  #filter((Trust == "NMUH" & .model == "holt_winter_ad") | (Trust != "NMUH" & .model == "holt_winter_a")) %>% 
  mutate(portion = "Forecast") 


pain_hw$lcl <- pain_hw$`95%`$lower
pain_hw$ucl <- pain_hw$`95%`$upper


pain_hw <-
  pain_hw %>% 
  select(Trust, Referrals, .model, Date, .mean, lcl, ucl)

pain_trust <-
ggplot(pain, aes(x= as.Date(Date), col = Trust))+
  geom_line(aes(y=Referrals), linewidth=1)+
  geom_line(aes(y=.mean), data=pain_hw, linewidth=1, alpha=0.6)+
  geom_smooth(aes(y=.mean), method="lm", data=pain_hw, linewidth=1
              , se=FALSE, linetype="dashed", alpha=0.6)+
  geom_vline(xintercept = as.Date("01/04/2023", format = "%d/%m/%Y"), col = "red"
             , linewidth = 1, linetype="dashed")+
  #geom_ribbon(data=pain_hw, aes(ymin = lcl, ymax=ucl, x= Date, fill = Trust)
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
  labs(title = "Pain Management Referrals - All providers"
       , subtitle = "Forecast computed by 'Holt-Winters method', based on Apr-21 - Mar-23,
       \nRNOH is imputed in Mar-21 due to missing data"
  )+
  theme_minimal()+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle=90, size = 8, colour = "#595959"
                                   , hjust = 1, ),
        text = element_text(family="Mulish"), 
        plot.subtitle = element_text(face="italic", size = 9)
  )

pain_trust




##### Cross-validation #####


mods2 <-
  pain %>% 
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
