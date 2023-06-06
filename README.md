# MSK Case for change - May-2023

This repository houses the code and assumptions used for rough projection of referrals
to secondary care outpatient specialities that are relevant to MSK patients.
It is not authoritative and is speculative because:

+ It's a model, and _'...all models are wrong, but some are useful.'_
+ The covid-19 pandemic and subsequent peaks of recovery activity do not represent stable timeseries, so manual imputation has been used for the purposes of damping this.
+ This is based on NCL resident referrals to providers in NCL, and also required de-duplication which is an imperfect system.

Several methods are evaluated here, but Holt-Winter's with damped trend was chosen due to small error, plausible trend and season.
