### continuous test of EpiModel
### source https://timchurches.github.io/blog/posts/2020-03-18-modelling-the-effects-of-public-health-interventions-on-covid-19-transmission-part-2/


library(tidyverse)
library(magrittr)
library(lubridate)
library(stringr)
library(tibble)
library(broom)
library(ggplot2)
library(gt)
library(knitr)
library(devtools)
library(DiagrammeR)
library(parallel)
library(foreach)
library(tictoc)
suppressMessages(library(EpiModel))
library(incidence)
library(earlyR)
theme(theme_minimal())

tic("Time to complete")

source_files <- c("_icm.mod.init.seiqhrf.R", "_icm.mod.status.seiqhrf.R", 
                  "_icm.mod.vital.seiqhrf.R", "_icm.control.seiqhrf.R", "_icm.utils.seiqhrf.R", 
                  "_icm.saveout.seiqhrf.R", "_icm.icm.seiqhrf.R")

src_path <- paste0("./_posts/2020-03-18-modelling-the-effects-of-public-health-", 
                   "interventions-on-covid-19-transmission-part-2/")

gist_url <- "https://gist.github.com/timchurches/92073d0ea75cfbd387f91f7c6e624bd7"

local_source <- FALSE

for (source_file in source_files) {
  if (local_source) {
    source(paste(src_path, source_file, sep = ""))
  } else {
    source_gist(gist_url, filename = source_file)
  }
}


# function to set-up and run the baseline simulations -----------
simulate <- function(# control.icm params
  type = "SEIQHRF", 
  nsteps = 366, 
  nsims = 8,
  ncores = 4,
  prog.rand = FALSE,
  rec.rand = FALSE,
  fat.rand = TRUE,
  quar.rand = FALSE,
  hosp.rand = FALSE,
  disch.rand = TRUE,
  infection.FUN = infection.seiqhrf.icm,
  recovery.FUN = progress.seiqhrf.icm,
  departures.FUN = departures.seiqhrf.icm,
  arrivals.FUN = arrivals.icm,
  get_prev.FUN = get_prev.seiqhrf.icm,
  # init.icm params
  s.num = 9997,
  e.num=0,
  i.num = 3,
  q.num=0,
  h.num=0,
  r.num = 0,
  f.num = 0,
  # param.icm params
  inf.prob.e = 0.02, 
  act.rate.e = 10,
  inf.prob.i = 0.05, 
  act.rate.i = 10,
  inf.prob.q = 0.02, 
  act.rate.q = 2.5,                    
  quar.rate = 1/30, 
  hosp.rate = 1/100,
  disch.rate = 1/15,
  prog.rate = 1/10,
  prog.dist.scale = 5,
  prog.dist.shape = 1.5,
  rec.rate = 1/20,
  rec.dist.scale = 35,
  rec.dist.shape = 1.5,
  fat.rate.base = 1/50,
  hosp.cap = 40,
  fat.rate.overcap = 1/25,
  fat.tcoeff = 0.5,
  vital = TRUE,
  a.rate = (10.5/365)/1000, 
  a.prop.e = 0.01,
  a.prop.i = 0.001,
  a.prop.q = 0.01,
  ds.rate = (7/365)/1000, 
  de.rate = (7/365)/1000, 
  di.rate = (7/365)/1000,
  dq.rate = (7/365)/1000,
  dh.rate = (20/365)/1000,
  dr.rate = (7/365)/1000,
  out="mean"
) {
  
  control <- control.icm(type = type, 
                         nsteps = nsteps, 
                         nsims = nsims,
                         ncores = ncores,
                         prog.rand = prog.rand,
                         rec.rand = rec.rand,
                         infection.FUN = infection.FUN,
                         recovery.FUN = recovery.FUN,
                         arrivals.FUN = arrivals.FUN,
                         departures.FUN = departures.FUN,
                         get_prev.FUN = get_prev.FUN)
  
  init <- init.icm(s.num = s.num,
                   e.num = e.num,
                   i.num = i.num,
                   q.num = q.num,
                   h.num = h.num,
                   r.num = r.num,
                   f.num = f.num)
  
  param <-  param.icm(inf.prob.e = inf.prob.e, 
                      act.rate.e = act.rate.e,
                      inf.prob.i = inf.prob.i, 
                      act.rate.i = act.rate.i,
                      inf.prob.q = inf.prob.q, 
                      act.rate.q = act.rate.q,                    
                      quar.rate = quar.rate,
                      hosp.rate = hosp.rate,
                      disch.rate = disch.rate,
                      prog.rate = prog.rate,
                      prog.dist.scale = prog.dist.scale,
                      prog.dist.shape = prog.dist.shape,
                      rec.rate = rec.rate,
                      rec.dist.scale = rec.dist.scale,
                      rec.dist.shape = rec.dist.shape,
                      fat.rate.base = fat.rate.base,
                      hosp.cap = hosp.cap,
                      fat.rate.overcap = fat.rate.overcap,
                      fat.tcoeff = fat.tcoeff,
                      vital = vital,
                      a.rate = a.rate, 
                      a.prop.e = a.prop.e,
                      a.prop.i = a.prop.i,
                      a.prop.q = a.prop.q,
                      ds.rate = ds.rate, 
                      de.rate = de.rate, 
                      di.rate = di.rate,
                      dq.rate = dq.rate,
                      dh.rate = dh.rate,
                      dr.rate = dr.rate)
  
  sim <- icm.seiqhrf(param, init, control)
  sim_df <- as.data.frame(sim, out=out)
  
  return(list(sim=sim, df=sim_df))
}


baseline_sim <- simulate(ncores = 4)


# define a function to extract timings and assemble a data
# frame
get_times <- function(simulate_results) {
  
  sim <- simulate_results$sim
  
  for (s in 1:sim$control$nsims) {
    if (s == 1) {
      times <- sim$times[[paste("sim", s, sep = "")]]
      times <- times %>% mutate(s = s)
    } else {
      times <- times %>% bind_rows(sim$times[[paste("sim", 
                                                    s, sep = "")]] %>% mutate(s = s))
    }
  }
  
  times <- times %>% mutate(infTime = ifelse(infTime < 0, -5, 
                                             infTime), expTime = ifelse(expTime < 0, -5, expTime)) %>% 
    mutate(incubation_period = infTime - expTime, illness_duration = recovTime - 
             expTime, illness_duration_hosp = dischTime - expTime, 
           hosp_los = dischTime - hospTime, quarantine_delay = quarTime - 
             infTime, survival_time = fatTime - infTime) %>% 
    select(s, incubation_period, quarantine_delay, illness_duration, 
           illness_duration_hosp, hosp_los, survival_time) %>% 
    pivot_longer(-s, names_to = "period_type", values_to = "duration") %>% 
    mutate(period_type = factor(period_type, levels = c("incubation_period", 
                                                        "quarantine_delay", "illness_duration", "illness_duration_hosp", 
                                                        "hosp_los", "survival_time"), labels = c("Incubation period", 
                                                                                                 "Delay entering isolation", "Illness duration", "Illness duration (hosp)", 
                                                                                                 "Hospital care required duration", "Survival time of case fatalities"), 
                                ordered = TRUE))
  return(times)
}

times <- get_times(baseline_sim)

times %>% filter(duration <= 30) %>% ggplot(aes(x = duration)) + 
  geom_bar() + facet_grid(period_type ~ ., scales = "free_y") + 
  labs(title = "Duration frequency distributions", subtitle = "Baseline simulation")


baseline_plot_df <- baseline_sim$df %>% # use only the prevalence columns
  select(time, s.num, e.num, i.num, q.num, h.num, r.num, f.num) %>% 
  # examine only the first 100 days since it is all over by
  # then using the default parameters
  filter(time <= 100) %>% pivot_longer(-c(time), names_to = "compartment", 
                                       values_to = "count")

# define a standard set of colours to represent compartments
compcols <- c(s.num = "yellow", e.num = "orange", i.num = "red", 
              q.num = "cyan", h.num = "magenta", r.num = "lightgreen", 
              f.num = "black")
complabels <- c(s.num = "Susceptible", e.num = "Infected/asymptomatic", 
                i.num = "Infected/infectious", q.num = "Self-isolated", h.num = "Requires hospitalisation", 
                r.num = "Recovered", f.num = "Case fatality")

baseline_plot_df %>% ggplot(aes(x = time, y = count, colour = compartment)) + 
  geom_line(size = 2, alpha = 0.7) + scale_colour_manual(values = compcols, 
                                                         labels = complabels) + theme_dark() + labs(title = "Baseline simulation", 
                                                                                                    x = "Days since beginning of epidemic", y = "Prevalence (persons)")


baseline_plot_df %>% filter(compartment %in% c("e.num", "i.num",
                                               "q.num", "h.num", "f.num")) %>% ggplot(aes(x = time, y = count,
                                                                                          colour = compartment)) + geom_line(size = 2, alpha = 0.7) +
  scale_colour_manual(values = compcols, labels = complabels) +
  theme_dark() + labs(title = "Baseline simulation", x = "Days since beginning of epidemic",
                      y = "Prevalence (persons)")







