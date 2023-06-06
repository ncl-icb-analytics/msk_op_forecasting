
# Combine and reindex as tsibble
NCL_MSK <-
  as.data.frame(c(TnO, Specialty = "tno")) %>% 
  bind_rows(as.data.frame(c(rheum, Specialty = "rheum"))) %>% 
  bind_rows(as.data.frame(c(pain, Specialty = "pain"))) %>% 
  group_by(Date, Specialty) %>% 
  summarise(Referrals = sum(Referrals))

NCL_MSK <- as_tsibble(NCL_MSK, key = Specialty, index = Date)

# check for missing months
has_gaps(NCL_MSK)


# Visualise
NCL_MSK %>% autoplot(Referrals)

# Check autocorrelation
NCL_MSK %>%  ACF() %>%  autoplot()
NCL_MSK %>%  ACF(difference(Referrals)) %>%  autoplot()


mods_NCL_MSK <-
  NCL_MSK %>% 
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

NCL_MSK_forecast <-
  mods_NCL_MSK %>% 
  forecast(h="60 months") 

# a<- NCL_MSK_forecast %>% 
#   accuracy(NCL_MSK)  %>% 
#   select(.model, RMSE:MAPE)

# 
# NCL_MSK_forecast %>% 
#   filter(Trust == "RFL") %>% 
#   autoplot() +
#   facet_wrap(~.model, ncol = 3)


NCL_MSK_hw <-
  NCL_MSK_forecast %>% 
  hilo(95) %>% 
  filter( .model == "holt_winter_ad") %>% 
  #filter(((Trust %in% c("RFL", "Whitt") & .model == "holt_winter_ad")) | ((!Trust %in% c("RFL", "Whitt")) & .model == "holt_winter_a")) %>% 
  mutate(portion = "Forecast") 


NCL_MSK_hw$lcl <- NCL_MSK_hw$`95%`$lower
NCL_MSK_hw$ucl <- NCL_MSK_hw$`95%`$upper


NCL_MSK_hw <-
  NCL_MSK_hw %>% 
  select(Specialty, Referrals, .model, Date, .mean, lcl, ucl)

NCL_MSK_trust <-
  ggplot(NCL_MSK, aes(x= as.Date(Date), col = Specialty))+
  geom_line(aes(y=Referrals), linewidth=1)+
  geom_line(aes(y=.mean), data=NCL_MSK_hw, linewidth=1, alpha=0.6)+
  geom_smooth(aes(y=.mean), method="lm", data=NCL_MSK_hw, linewidth=1
              , se=FALSE, linetype="dashed", alpha=0.6)+
  geom_vline(xintercept = as.Date("01/04/2023", format = "%d/%m/%Y"), col = "red"
             , linewidth = 1, linetype="dashed")+
  #geom_ribbon(data=NCL_MSK_hw, aes(ymin = lcl, ymax=ucl, x= Date, fill = Trust)
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
  labs(title = "Orthopaedic, Rheumatology and Pain Management Referrals - All NCL providers"
       , subtitle = "Forecast computed by 'Holt-Winters method', based on Apr-21 - Mar-23,
       \nRoyal Free is orthopaedic and rheumatology were inputed where articficiNCLy low through referNCL restriction,
       \nWhittingon rheumatology imputed in March-22 due to outlier values, RNOH missing data imputed Mar-21"
  )+
  theme_minimal()+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle=90, size = 8, colour = "#595959"
                                   , hjust = 1, ),
        text = element_text(family="Mulish"), 
        plot.subtitle = element_text(face="italic", size = 8, lineheight = 0.6)
  )


NCL_MSK_trust



############ Total ##################


# Combine and reindex as tsibble
NCL_MSK_all <-
  as.data.frame(TnO) %>% 
  bind_rows(as.data.frame(rheum)) %>% 
  bind_rows(as.data.frame(pain)) %>% 
  group_by(Date) %>% 
  summarise(Referrals = sum(Referrals))

NCL_MSK_all <- as_tsibble(NCL_MSK_all, index = Date)

# check for missing months
has_gaps(NCL_MSK_all)


# Visualise
NCL_MSK_all %>% autoplot(Referrals)

# Check autocorrelation
NCL_MSK_all %>%  ACF() %>%  autoplot()
NCL_MSK_all %>%  ACF(difference(Referrals)) %>%  autoplot()


mods_NCL_MSK_all <-
  NCL_MSK_all %>% 
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

NCL_MSK_all_forecast <-
  mods_NCL_MSK_all %>% 
  forecast(h="60 months") 

# a<- NCL_MSK_forecast %>% 
#   accuracy(NCL_MSK)  %>% 
#   select(.model, RMSE:MAPE)

# 
# NCL_MSK_forecast %>% 
#   filter(Trust == "RFL") %>% 
#   autoplot() +
#   facet_wrap(~.model, ncol = 3)


NCL_MSK_all_hw <-
  NCL_MSK_all_forecast %>% 
  hilo(95) %>% 
  filter( .model == "holt_winter_ad") %>% 
  #filter(((Trust %in% c("RFL", "Whitt") & .model == "holt_winter_ad")) | ((!Trust %in% c("RFL", "Whitt")) & .model == "holt_winter_a")) %>% 
  mutate(portion = "Forecast") 


NCL_MSK_all_hw$lcl <- NCL_MSK_all_hw$`95%`$lower
NCL_MSK_all_hw$ucl <- NCL_MSK_all_hw$`95%`$upper


NCL_MSK_all_hw <-
  NCL_MSK_all_hw %>% 
  select(Referrals, .model, Date, .mean, lcl, ucl)

NCL_MSK_all_trust <-
  ggplot(NCL_MSK_all, aes(x= as.Date(Date)))+
  geom_line(aes(y=Referrals), linewidth=1)+
  geom_line(aes(y=.mean), data=NCL_MSK_all_hw, linewidth=1, alpha=0.6)+
  geom_smooth(aes(y=.mean), method="lm", data=NCL_MSK_all_hw, linewidth=1
              , se=FALSE, linetype="dashed", alpha=0.6)+
  geom_vline(xintercept = as.Date("01/04/2023", format = "%d/%m/%Y"), col = "red"
             , linewidth = 1, linetype="dashed")+
  #geom_ribbon(data=NCL_MSK_hw, aes(ymin = lcl, ymax=ucl, x= Date, fill = Trust)
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
  labs(title = "Orthopaedic, Rheumatology and Pain Management Referrals combine - All NCL providers"
       , subtitle = "Forecast computed by 'Holt-Winters method', based on Apr-21 - Mar-23,
       \nRoyal Free is orthopaedic and rheumatology were inputed where articficiNCLy low through referNCL restriction,
       \nWhittingon rheumatology imputed in March-22 due to outlier values, RNOH missing data imputed Mar-21"
  )+
  theme_minimal()+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle=90, size = 8, colour = "#595959"
                                   , hjust = 1, ),
        text = element_text(family="Mulish"), 
        plot.subtitle = element_text(face="italic", size = 8, lineheight = 0.6)
  )


NCL_MSK_all_trust




##### Cross-validation #####


mods2 <-
  NCL_MSK %>% 
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
