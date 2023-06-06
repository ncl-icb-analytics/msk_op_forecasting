library(ragg)
library(fpp3)
library(tidyverse)
library(scales)
library(extrafont)

loadfonts(device = "win", quiet = TRUE) 


theme_set(theme_minimal() +
            theme(legend.position = "bottom"
                  , text = element_text(family="Mulish"))
)



# Load data
TnO <- read_csv("data/tno.csv")
 
# Format and pivot
TnO$Date <- yearmonth(as.Date(TnO$Date, format = "%d/%m/%Y"))

TnO <- 
  TnO %>% 
  pivot_longer(cols = -Date, names_to = "Trust", values_to = "Referrals")




# Assumption 1: RFL between Aug21 - Dec-21
TnO2a <- 
  TnO %>% 
  filter(Trust == "RFL", Date >= yearmonth("2021-10-01"), Date <yearmonth("2023-02-01"), Date !=yearmonth("2022-10-01")) %>% 
  mutate(Referrals = Referrals * 2)

# Not October
TnO2b <- 
  TnO %>% 
  filter(Trust == "RFL", Date >= yearmonth("2021-12-01"), Date <yearmonth("2022-03-01")) %>% 
  mutate(Referrals = Referrals * 4)


# Update
TnOa <- rows_update(TnO2a, TnO2b, unmatched = "ignore", by= c("Date", "Trust"))
TnO <- rows_update(TnO, TnOa, unmatched = "ignore", by= c("Date", "Trust"))

# check
TnO %>% 
  filter(Trust == "RFL", Date >= yearmonth("2021-09-01"), Date <yearmonth("2022-10-01"))


## Assumption 2: replace 

## TnO3 <- 
#  TnO %>% 
#  filter(Trust == "Whitt", Date >= yearmonth("2021-09-01"), Date <yearmonth("2023-04-01")) %>% 
#  mutate(Trust = "RFL")

## Update
#TnO <- rows_update(TnO, TnO3, unmatched = "ignore", by= c("Date", "Trust"))


# Make timeseries table
TnO <- as_tsibble(TnO, key = Trust, index = Date)

# check for missing months
has_gaps(TnO)


# Visualise
TnO %>% autoplot(Referrals)

# seasonality
TnO %>% 
  gg_subseries(Referrals)


# Check autocorrelation
TnO %>%  ACF() %>%  autoplot()
TnO %>%  ACF(difference(Referrals)) %>%  autoplot()


# decompose the timeseries
dcmp <- TnO %>% 
  model(stl = STL(Referrals))

components(dcmp)

components(dcmp) %>%  autoplot()


# Visulaise effects of rolling average
TnO <- TnO %>% 
  mutate(
    `6-MA` = slider::slide_dbl(Referrals, mean,
                               .before = 6, .after = 0, .complete = TRUE)
  )




TnO %>% 
  autoplot(Referrals) +
  geom_line(aes(y = `6-MA`)) +
  labs(y = "Referrals") +
  guides(colour = guide_legend(title = "series"))


# Apply models candidates

mods1 <-
  TnO %>% 
  filter(Date >= yearmonth("2021 Apr")) %>% 
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
    #arima = ARIMA(Referrals)
  )

tno_forecast <-
  mods1 %>% 
  forecast(h="60 months") 

fc <- TnO %>% filter(Date >= yearmonth("2021 Apr"), )

accuracy(mods1, fc)


# a<- tno_forecast %>% 
#   accuracy(TnO)  %>% 
#   select(.model, RMSE:MAPE)


tno_forecast %>% 
  filter(Trust == "NMUH") %>% 
  autoplot() +
  facet_wrap(~.model, ncol = 3)


tno_hw <-
  tno_forecast %>% 
  hilo(95) %>% 
  filter(.model == "holt_winter_ad") %>% 
  mutate(portion = "Forecast") 


tno_hw$lcl <- tno_hw$`95%`$lower
tno_hw$ucl <- tno_hw$`95%`$upper


tno_hw <-
  tno_hw %>% 
  select(Trust, Referrals, .model, Date, .mean, lcl, ucl)

tno_trust <-
ggplot(TnO, aes(x= as.Date(Date), col = Trust))+
  geom_line(aes(y=Referrals), linewidth=1)+
  geom_line(aes(y=.mean), data=tno_hw, linewidth=1, alpha=0.6)+
  geom_smooth(aes(y=.mean), method="lm", data=tno_hw, linewidth=1
             , se=FALSE, linetype="dashed", alpha=0.6)+
  geom_vline(xintercept = as.Date("01/04/2023", format = "%d/%m/%Y"), col = "red"
             , linewidth = 1, linetype="dashed")+
  #geom_ribbon(data=tno_hw, aes(ymin = lcl, ymax=ucl, x= Date, fill = Trust)
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
  labs(title = "Orthopaedic Referrals - All providers"
       , subtitle = "Forecast computed by 'Holt-Winters method', based on Apr-21 - Mar-23, 
       \nRoyal Free is imputed from Oct-21 due to referral restrictions following capacity alert")+
  theme_minimal()+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle=90, size = 8, colour = "#595959"
                                   , hjust = 1, ),
        text = element_text(family="Mulish"), 
        plot.subtitle = element_text(face="italic", size = 9)
        )


tno_trust







##### Cross-validation #####


mods2 <-
  TnO %>% 
  filter(year(Date) >=2021) %>% 
  stretch_tsibble(.init = 6) %>% 
  model(
    mean = MEAN(Referrals),
    naive = NAIVE(Referrals),
    snaive = SNAIVE(Referrals ~ lag("year")),
    drift = RW(Referrals ~ drift()),
    #ets = ETS(Referrals),
    #ses = ETS(Referrals ~ error("A")+trend("N")+season("N")),
    #holt_winter = ETS(Referrals ~ error("A")+trend("A")+season("A"))
    arima = ARIMA(Referrals ~ pdq(0,1,1) + PDQ(0,1,1))
  )

accuracy(mods2)
