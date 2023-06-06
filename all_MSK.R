
# Combine and reindex as tsibble
all_MSK <-
  TnO %>% 
  as.data.frame() %>% 
  bind_rows(as.data.frame(rheum)) %>% 
  bind_rows(as.data.frame(pain)) %>% 
  group_by(Date, Trust) %>% 
  summarise(Referrals = sum(Referrals))

all_MSK <- as_tsibble(all_MSK, key = Trust, index = Date)

# check for missing months
has_gaps(all_MSK)


# Visualise
all_MSK %>% autoplot(Referrals)

# Check autocorrelation
all_MSK %>%  ACF() %>%  autoplot()
all_MSK %>%  ACF(difference(Referrals)) %>%  autoplot()


mods_all_MSK <-
  all_MSK %>% 
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

all_MSK_forecast <-
  mods_all_MSK %>% 
  forecast(h="60 months") 

# a<- all_MSK_forecast %>% 
#   accuracy(all_MSK)  %>% 
#   select(.model, RMSE:MAPE)


all_MSK_forecast %>% 
  filter(Trust == "RFL") %>% 
  autoplot() +
  facet_wrap(~.model, ncol = 3)


all_MSK_hw <-
  all_MSK_forecast %>% 
  hilo(95) %>% 
  filter( .model == "holt_winter_ad") %>% 
  #filter(((Trust %in% c("RFL", "Whitt") & .model == "holt_winter_ad")) | ((!Trust %in% c("RFL", "Whitt")) & .model == "holt_winter_a")) %>% 
  mutate(portion = "Forecast") 


all_MSK_hw$lcl <- all_MSK_hw$`95%`$lower
all_MSK_hw$ucl <- all_MSK_hw$`95%`$upper


all_MSK_hw <-
  all_MSK_hw %>% 
  select(Trust, Referrals, .model, Date, .mean, lcl, ucl)

all_MSK_trust <-
  ggplot(all_MSK, aes(x= as.Date(Date), col = Trust))+
  geom_line(aes(y=Referrals), linewidth=1)+
  geom_line(aes(y=.mean), data=all_MSK_hw, linewidth=1, alpha=0.6)+
  geom_smooth(aes(y=.mean), method="lm", data=all_MSK_hw, linewidth=1
              , se=FALSE, linetype="dashed", alpha=0.6)+
  geom_vline(xintercept = as.Date("01/04/2023", format = "%d/%m/%Y"), col = "red"
             , linewidth = 1, linetype="dashed")+
  #geom_ribbon(data=all_MSK_hw, aes(ymin = lcl, ymax=ucl, x= Date, fill = Trust)
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
  labs(title = "Orthopaedic, Rheumatology and Pain Management Referrals - by providers"
       , subtitle = "Forecast computed by 'Holt-Winters method', based on Apr-21 - Mar-23,
       \nRoyal Free is orthopaedic and rheumatology were inputed where articficially low through referall restriction,
       \nWhittingon rheumatology imputed in March-22 due to outlier values, RNOH missing data imputed Mar-21"
  )+
  theme_minimal()+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle=90, size = 8, colour = "#595959"
                                   , hjust = 1, ),
        text = element_text(family="Mulish"), 
        plot.subtitle = element_text(face="italic", size = 8, lineheight = 0.6)
  )

all_MSK_trust




##### Cross-validation #####


mods2 <-
  all_MSK %>% 
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
