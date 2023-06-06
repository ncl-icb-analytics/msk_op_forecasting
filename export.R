# Export

ggsave("output/tno_trust.png", plot = tno_trust)
ggsave("output/rheum_trust.png", plot = rheum_trust)
ggsave("output/pain_trust.png", plot = pain_trust)
ggsave("output/all_MSK_trust.png", plot = all_MSK_trust)
ggsave("output/NCL_spec.png", plot = NCL_MSK_trust)
ggsave("output/NCL_MSK_all.png", plot= NCL_MSK_all_trust)



write_csv(tno_hw, file = "output/tno_forecast.csv")
write_csv(rheum_hw, file = "output/rheum_forecast.csv")
write_csv(pain_hw, file = "output/pain_forecast.csv")
write_csv(all_MSK_hw, file = "output/all_MSK_forecast.csv")
write_csv(NCL_MSK_hw, file = "output/NCL_MSK_forecast.csv")
write_csv(NCL_MSK_all_hw, file = "output/NCL_MSK_all_forecast.csv")


library(openxlsx)

openxlsx::write.xlsx(list("tno" = tno_hw
                          , "rheum" = rheum_hw
                          , "pain" = pain_hw
                          , "All" = all_MSK_hw
                          , "NCL" = NCL_MSK_hw
                          , "Total_NCL" = NCL_MSK_all_hw)
                     , "output/MSK_OP_forecast.xlsx")



#trend change
library(broom)

tno_mods <-
  tno_hw %>% 
  group_by(Trust) %>% 
  select(Trust, .mean, Date) %>% 
  do(model = lm(.mean ~ Date, data = .))

tno_subs <- lapply(tno_mods[[2]], coef)

rheum_mods <-
  rheum_hw %>% 
  group_by(Trust) %>% 
  select(Trust, .mean, Date) %>% 
  do(model = lm(.mean ~ Date, data = .))

rheum_subs <- lapply(rheum_mods[[2]], coef)

pain_mods <-
  pain_hw %>% 
  group_by(Trust) %>% 
  select(Trust, .mean, Date) %>% 
  do(model = lm(.mean ~ Date, data = .))

pain_subs <- lapply(pain_mods[[2]], coef)

all_MSK_mods <-
  all_MSK_hw %>% 
  group_by(Trust) %>% 
  select(Trust, .mean, Date) %>% 
  do(model = lm(.mean ~ Date, data = .))

all_MSK_subs <- lapply(all_MSK_mods[[2]], coef)

NCL_mods <-
  NCL_MSK_hw %>% 
  group_by(Specialty) %>% 
  select(.mean, Date) %>% 
  do(model = lm(.mean ~ Date, data = .))

NCL_MSK_subs <- lapply(NCL_mods[[2]], coef)

NCL_mods_all <-  lm(.mean ~ Date, data = NCL_MSK_all_hw)


# Assemble into table
per_growth <-
  data.frame(
    Trust = c(tno_mods[[1]])
    , tno = sapply(tno_subs, "[[", 2)  
    , rheum = sapply(rheum_subs, "[[", 2)  
    , pain = sapply(pain_subs, "[[", 2)  
    , all = sapply(all_MSK_subs, "[[", 2)
  )

per_growth <-
  rbind(per_growth,
        data.frame(
          Trust = "All",
          tno = NCL_MSK_subs[[3]][[2]],
          rheum = NCL_MSK_subs[[2]][[2]],
          pain = NCL_MSK_subs[[1]][[2]],
          all = coef(NCL_mods_all)[2]
        ))

per_growth 
