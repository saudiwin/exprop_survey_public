
# required packages
require(dplyr)
require(tidyr)
require(stringr)
require(ggplot2)
require(betareg)
require(readr)
require(lubridate)
require(brms)
require(rstanarm)
require(forcats)
require(bayesplot)
require(inflection)
require(mirt)
require(texreg)
require(patchwork)

# auxiliary code

source("define_ord_betareg.R")

source("prepare_data_public.R")


# cmdstanr is needed for some models (i.e. splines) that take a long time to estimate
# for multi-threading support
# see brms documentation for more info
#options(brms.backend="cmdstanr")

# whether or not to run all models (will take a few hours), or use saved model objects from disk

run_model <- F

# look at some correlates of political connections

# let's make some factor scores for political connections
# need to recode these variables

all_types <- unique(unlist(str_split(unique(qual_data$pol_connect1_1),pattern=",")))
all_types <- all_types[!is.na(all_types)]

over_types <- lapply(all_types, function(a) {
  out_tib <- tibble(!!a :=rowSums(cbind(
          as.numeric(grepl(x=qual_data$pol_connect1_1,pattern=a)),
          as.numeric(grepl(x=qual_data$pol_connect1_2,pattern=a)),
          as.numeric(grepl(x=qual_data$pol_connect1_3,pattern=a)),
          as.numeric(grepl(x=qual_data$pol_connect1_4,pattern=a)))))
}) %>% bind_cols

names(over_types) <- c("cur_high_beau",
                       "for_low_beau",
                       "for_high_beau",
                       "for_mem_parl",
                       "cur_low_beau",
                       "cur_mem_parl")

# simple 1-D IRT model

pol_score <- mirt(over_types,
                  model=1,
                  itemtype="Rasch")

# put pol score back in the data frame

qual_data$pol_score <- as.numeric(fscores(pol_score))

# table of pol scores by performance

str_wrap_factor <- function(x, ...) {
  levels(x) <- str_wrap(levels(x), ...)
  x
}

qual_data <- mutate(qual_data,perf_1=factor(perf_1, levels=c("More than 20% loss",
                                                             "Between 10 and 20% loss",
                                                             "Between 10 and 5% loss",
                                                             "Between 5 and 0% loss",
                                                             "Broke even",
                                                             "Between 0 and 5% profit margin",
                                                             "Between 5 and 10% profit margin",
                                                             "Between 10 and 20% profit margin",
                                                             "More than 20% profit margin")))


qual_data %>% 
  filter(!is.na(perf_1)) %>% 
  ggplot(aes(y=pol_score,x=str_wrap_factor(perf_1,5))) +
  stat_summary(fun.data=mean_cl_boot) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size=7)) +
  ylab("Political Connections Score")

ggsave("pol_con_perf.png")

# let's do a heat map of pol connections 1-10 and pol efficiency 1-10

# make a quantitative version of firm performance

qual_data <- mutate(qual_data,perf_quant=as.numeric(as.character(factor(perf_1,
                                                                        labels=c(-30,
                                                                                 -15,
                                                                                 -7.5,
                                                                                 -2.5,
                                                                                 0,
                                                                                 2.5,
                                                                                 7.5,
                                                                                 15,
                                                                                 30)))))

qual_data %>% 
  mutate(pol_con_1=as.numeric(pol_con_1),
         pol_eff_1=as.numeric(pol_eff_1)) %>% 
  filter(!is.na(pol_con_1),!is.na(pol_eff_1)) %>% 
  group_by(pol_con_1,pol_eff_1) %>% 
  mutate(count_pol=n()) %>% 
  ggplot(aes(y=pol_con_1,x=pol_eff_1)) +
  geom_count() +
  theme(panel.background = element_blank()) +
  xlab("Political Connections Level") +
  ylab("Political Efficacy Level") +
  guides(size=guide_legend(title="N"))

ggsave("pol_con_eff.png")

# figure out categories of companies

qual_data <- mutate(qual_data,
                    pol_con_1=as.numeric(pol_con_1),
                    pol_eff_1=as.numeric(pol_eff_1),
                    firm_pol_cat=case_when(pol_con_1<6 & pol_eff_1<6~"Non-Connected/Hurt",
                                           pol_con_1>5 & pol_eff_1<6~"Connected/Hurt",
                                           pol_con_1<6 & pol_eff_1>5~"Non-Connected/Helped",
                                           pol_con_1>5 & pol_eff_1>5~"Connected/Helped"))

# see how performance breaks down by these categories

prop.table(table(qual_data$perf_1,qual_data$firm_pol_cat),margin=2)

qual_data %>% 
  group_by(perf_1,firm_pol_cat) %>% 
  count %>% 
  filter(!is.na(firm_pol_cat),!is.na(perf_1)) %>% 
  group_by(firm_pol_cat) %>% 
  mutate(n=n/sum(n)) %>% 
  ggplot(aes(x=perf_1,y=n)) +
  geom_col() +
  facet_wrap(~firm_pol_cat,ncol=1) +
  scale_y_continuous(labels=scales::percent,breaks=c(0,0.1,0.2)) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        axis.text.x = element_text(size=7,angle=90)) +
  xlab("")  + ylab("")

ggsave("con_perf.png")

# datasets:

# qual_data - original survey data
# combined_clean - all data excluding duplicates, fast responses
# combined_clean_clicks - data with information on number of clicks on the sliders (much more recent)

# fit basic OLS model

lm_fit <- lm(outcome_norm ~ connections + employees + years + own + sector + country,
                       data = combined_clean)

summary(lm_fit)

lm_fit <- lm(outcome_norm ~ gender*perf + employees + years + own,
             data = combined_clean)

summary(lm_fit)

# ordered beta regression

# scale variables 

# base treatment

combined_clean$combined_perf2 <- as.character(combined_clean$combined_perf) %>% 
    factor(levels=levels(combined_clean$combined_perf))


if(run_model) {
  base_con <- brm(outcome_norm ~ 0 + Intercept + connected + 
                    employees + years + own + country + sector + assets + combined_perf2,
                  family=ord_beta_reg,
                  data=combined_clean,
                  stanvars=stanvars,
                  prior=priors,
                  iter=1000,
                  chains=1,cores=1)
  
  saveRDS(base_con,"data/base.rds")
} else {
  base_con <- readRDS("data/base.rds")
}

# all treatments


if(run_model) {
  
  all_con <- brm(outcome_norm ~ 0 + Intercept + connections + employees + years + own + 
                   country + sector + assets + combined_perf2,
                  family=ord_beta_reg,
                  data=combined_clean,
                  stanvars=stanvars,
                  prior=priors,
                  iter=1000,
                  chains=1,cores=1)
  
  saveRDS(all_con,"data/all.rds")
} else {
  all_con <- readRDS("data/all.rds")
}

# connected X country

if(run_model) {
  country_con <- brm(outcome_norm ~ 0 + Intercept + connected*country_resp + employees + years + own + country + sector + assets,
                  family=ord_beta_reg,
                  data=combined_clean,
                  stanvars=stanvars,
                  prior=priors,
                  iter=1000,
                  chains=1,cores=1)
  
  saveRDS(country_con,"data/base_country.rds")
} else {
  country_con <- readRDS("data/base_country.rds")
}

# connected X pol_help 

if(run_model) {
  pol_int_con <- brm(outcome_norm ~ 0 + Intercept + connected*pol_eff_1 + employees + years + own + country + sector + assets,
                     family=ord_beta_reg,
                     data=combined_clean,
                     stanvars=stanvars,
                     prior=priors,
                     iter=1000,
                     chains=1,cores=1)
  
  saveRDS(pol_int_con,"data/pol_int_con.rds")
} else {
  pol_int_con <- readRDS("data/pol_int_con.rds")
}

# connected X connected

if(run_model) {
  pol_con_con <- brm(outcome_norm ~ 0 + Intercept + connected*pol_con_1 + employees + years + own + country + sector + assets,
                     family=ord_beta_reg,
                     data=combined_clean,
                     stanvars=stanvars,
                     prior=priors,
                     iter=1000,
                     chains=1,cores=1)
  
  saveRDS(pol_con_con,"data/pol_con_con.rds")
} else {
  pol_con_con <- readRDS("data/pol_con_con.rds")
}

# connected X respondent sector

if(run_model) {
  pol_con_sector <- brm(outcome_norm ~ 0 + Intercept + connected*sector_1 + employees + years + own + country + sector + assets,
                     family=ord_beta_reg,
                     data=combined_clean,
                     stanvars=stanvars,
                     prior=priors,
                     iter=1000,
                     chains=1,cores=1)
  
  saveRDS(pol_con_sector,"data/pol_con_sector.rds")
} else {
  pol_con_sector <- readRDS("data/pol_con_sector.rds")
}

# connected X profits

if(run_model) {
  con_profit_updown <- brm(bf(outcome_norm ~ 0 + Intercept + connected*total_perf,
                       decomp = "QR"),
                     family=ord_beta_reg,
                     data=combined_clean,
                     stanvars=stanvars,
                     prior=priors,
                     iter=1000,
                     chains=1,cores=1)
  
  saveRDS(con_profit_updown,"data/con_profit_updown.rds")
} else {
  con_profit_updown <- readRDS("data/con_profit_updown.rds")
}

if(run_model) {
  con_profit_lm <- brm(bf(outcome_norm ~ 0 + Intercept + connected*squared + connected*linear,
                              decomp = "QR"),
                           family=ord_beta_reg,
                           data=combined_clean,
                           stanvars=stanvars,
                           prior=priors,
                           iter=1000,
                           chains=1,cores=1)
  
  saveRDS(con_profit_lm,"data/con_profit_lm.rds")
} else {
  con_profit_lm <- readRDS("data/con_profit_lm.rds")
}

# profit as ordinal category

if(run_model) {
  con_profit_ord <- brm(bf(outcome_norm ~ 0 + Intercept + connected*mo(perf),
                          decomp = "QR"),
                       family=ord_beta_reg,
                       data=combined_clean,
                       stanvars=stanvars,threads=3,
                       prior=priors,
                       iter=1000,
                       chains=1,cores=1)
  
  saveRDS(con_profit_ord,"data/con_profit_ord.rds")
} else {
  con_profit_ord <- readRDS("data/con_profit_ord.rds")
}

# tfp

if(run_model) {
  tfp_full <- brm(bf(outcome_norm ~ 0 + Intercept + s(tfp,by=connected)),
                        family=ord_beta_reg,
                        data=tfp_data,
                        stanvars=stanvars,threads=3,
                        prior=priors,
                        iter=1000,
                        chains=1,cores=1)
  
  saveRDS(tfp_full,"data/tfp_full.rds")
} else {
  tfp_full <- readRDS("data/tfp_full.rds")
}

# tfp with polynomials

if(run_model) {
  # tfp_full_poly <- brm(bf(outcome_norm ~ 0 + Intercept + connected*tfp*poly(years,3),
  #                         decomp="QR"),
  #                 family=ord_beta_reg,
  #                 data=tfp_data,
  #                 stanvars=stanvars,threads=3,
  #                 prior=priors,
  #                 iter=1000,
  #                 chains=1,cores=1)
  
  #saveRDS(tfp_full_poly,"data/tfp_full_poly.rds")
} else {
  #tfp_full_poly <- readRDS("data/tfp_full_poly.rds")
}

# sales

if(run_model) {
  profit_full <- brm(bf(outcome_norm ~ 0 + Intercept + s(sales_series,by=connected)),
                  family=ord_beta_reg,
                  data=tfp_data,
                  stanvars=stanvars,threads=3,
                  prior=priors,
                  iter=1000,
                  chains=1,cores=1)
  
  saveRDS(profit_full,"data/profit_full.rds")
} else {
  profit_full <- readRDS("data/profit_full.rds")
}





# gender
if(run_model) {
  gender <- brm(outcome_norm ~ 0 + Intercept + gender + employees + years + own + Duration + position,
                family=ord_beta_reg,
                data=combined_clean,
                stanvars=stanvars,
                prior=priors,
                iter=1000,
                chains=1,cores=1)
  
  saveRDS(gender ,"data/gender1.rds")
} else {
  gender <- readRDS("data/gender1.rds")
}


# gender by country
# nothing to see here

if(run_model) {
  gender_country <- brm(outcome_norm ~ 0 + Intercept + gender*country_resp + employees + years + own + Duration + position,
                        family=ord_beta_reg,
                        data=combined_clean,
                        stanvars=stanvars,
                        prior=priors,
                        iter=1000,
                        chains=1,cores=1)
  
  saveRDS(gender_country ,"data/gender_country.rds")
} else {
  gender_country <- readRDS("data/gender_country.rds")
}


# gender by performance

if(run_model) {
  gender_combined_perf_conn <- brm(outcome_norm ~ 0 + Intercept + gender*mo(combined_perf) + employees + years + own + Duration + position,
                                   family=ord_beta_reg,
                                   data=combined_clean,
                                   stanvars=stanvars,threads=3,
                                   prior=priors,
                                   iter=1000,
                                   chains=1,cores=1)
  
  saveRDS(gender_combined_perf_conn  ,"data/gender_combined_perf_conn.rds")
} else {
  gender_combined_perf_conn  <- readRDS("data/gender_combined_perf_conn.rds")
}

# connections and expropriation threats

if(run_model) {
  con_exprop <- brm(bf(outcome_norm ~ 0 + Intercept + connected*sum_exprop,
                           decomp = "QR"),
                        family=ord_beta_reg,
                        data=combined_clean,
                        stanvars=stanvars,
                        prior=priors,
                        iter=1000,
                        chains=1,cores=1)
  
  saveRDS(con_exprop,"data/con_exprop.rds")
} else {
  con_exprop<- readRDS("data/con_exprop.rds")
}

# ordinal expropriation risk measure

if(run_model) {
  con_exprop_ord <- brm(bf(outcome_norm ~ 0 + Intercept + connected*man,
                       decomp = "QR"),
                    family=ord_beta_reg,
                    data=combined_clean,
                    stanvars=stanvars,
                    prior=priors,
                    iter=1000,
                    chains=1,cores=1)
  
  saveRDS(con_exprop_ord,"data/con_exprop_ord.rds")
} else {
  con_exprop_ord <- readRDS("data/con_exprop_ord.rds")
}

# do some coef plots

require(tidybayes)
require(ggthemes)

all_con_res <- all_con %>% 
  gather_draws(`b_connect.*`,regex=T) %>% 
  median_qi %>% 
  mutate(variable_rec = fct_recode(.variable,
                                                "Daughter of President" = "b_connectionsboard_daughter_pres",
                                                "Mid-Level Official\non Board" = "b_connectionsboard_med_crat",
                                                "Minister\non Board" = "b_connectionsboard_minister",
                                                "MP\non Board" = "b_connectionsboard_mp",
                                                "Son of President" = "b_connectionsboard_son_pres",
                                                "Army\nGeneral" = "b_connectionsown_gen_army",
                                                "MP\nOwner" = "b_connectionsown_mp",
                                                "PM Nephew\nOwner" = "b_connectionsown_pm_nephew",
                                                "PM Niece\nOwner" = "b_connectionsown_pm_niece",
                                                "Police\nOwner" = "b_connectionsown_police",
                                                "President Classmate\nOwner" = "b_connectionsown_pres_classmate",
                                                "President\nDaughter In-law" = "b_connectionsown_pres_daughter_inlaw",
                                                "Member of\nPresident Party" = "b_connectionsown_pres_party",
                                                "President\nSon In-law" = "b_connectionsown_pres_son_inlaw"
  ))

all_con_res %>% 
  ggplot(aes(y=.value,x=reorder(variable_rec,.value))) +
  geom_pointrange(aes(ymin=.lower,
                      ymax=.upper),fatten=2) +
  geom_text(aes(label=round(.value,digits=2)),vjust=-.75,size=3) +
  theme_tufte() +
  coord_flip() +
  geom_hline(yintercept=0,linetype=2) +
  labs(y="Logit Coefficient",x="",
       caption=stringr::str_wrap("Estimates derived from Bayesian logistic regression model. Point estimates are posterior medians and 
                                 the intervals are 5% to 95% quantiles of the empirical posterior distribution."))

ggsave("all_con.png")

# output to latex for inclusion in paper

# texreg(all_con,file="all_con.tex",booktabs = T,
#        omit.coef="ntercept",float.pos="H",
#        caption.above = T,threeparttable = T,use.packages = F,label="allcon",
#        dcolumn = T,caption="ATEs for All Political Connection Treatments",scalebox=.5,
#        longtable = T,
#        custom.coef.names=c("Intercept",
#                            "Daughter of President",
#                            "Mid-level Bureaucrat on Board",
#                            "Cabinet Minister on Board",
#                            "MP on Board",
#                            "Son of President on Board",
#                            "Owner is Army General",
#                            "Owner is an MP",
#                            "Owner is Nephew of PM",
#                            "Owner is Niece of PM",
#                            "Owner is Police Officer",
#                            "Owner is President's Classmate",
#                            "Owner is President's Daughter-in-law",
#                            "Owner belongs to President's Party",
#                            "Owner is President's Son in Law",
#                            "No. Employees",
#                            "Years Operation",
#                            "Private",
#                            "SOE",
#                            "Germany",
#                            "Saudi Arabia",
#                            "China",
#                            "Korea",
#                            "Egypt",
#                            "USA",
#                            "Japan",
#                            "Russia",
#                            "Ukraine",
#                            "Venezuela",
#                            "Construction",
#                            "Energy",
#                            "Financials",
#                            "Manufacturing",
#                            "Retail",
#                            "Telecom",
#                            "Assets",
#                            "Profit"))

base_con_reg <- base_con %>% 
  recover_types(combined_clean) %>% 
  gather_draws(`b_.*`,regex=T) %>% 
  median_qi %>% 
  mutate(var_rec=fct_recode(.variable,
                            "Very Profitable"="b_combined_perf2gain_much",
                            "Somewhat Profitable"="b_combined_perf2gain_some",
                            "Break Even"="b_combined_perf2break_even",
                            "Lose Some"="b_combined_perf2lose_some",
                            "Assets" = "b_assets",
                            "Connected" = "b_connectedtreatment",
                            "China" = "b_countryChina",
                            "Egypt" = "b_countryEgypt",
                            "Germany" = "b_countryGermany",
                            "Japan" = "b_countryJapan",
                            "Korea" = "b_countryKorea",
                            "Russia" = "b_countryRussia",
                            "Saudi" = "b_countrySaudi",
                            "Ukraine" = "b_countryUkraine",
                            "USA" = "b_countryUSA",
                            "Venezuela" = "b_countryVenezuela",
                            "Employees" = "b_employees",
                            "Intercept" = "b_Intercept",
                            "Private" = "b_ownprivate",
                            "SOE" = "b_ownsoe",
                            "Construction" = "b_sectorconstruction",
                            "Energy" = "b_sectorenergy",
                            "Financials" = "b_sectorfinancials",
                            "Manufacturing" = "b_sectormanufacturing",
                            "Retail" = "b_sectorretail",
                            "Telecom" = "b_sectortelecom",
                            "Years" = "b_years"
  ))

base_con_reg %>% 
  filter(var_rec!="Intercept") %>%
  ggplot(aes(y=.value,x=reorder(var_rec,.value))) +
  geom_pointrange(aes(ymin=.lower,
                      ymax=.upper)) +
  theme_tufte() +
  coord_flip() +
  geom_hline(yintercept=0,linetype=2) +
  labs(y="Logit Coefficient",x="",
       caption=stringr::str_wrap("Political connection treatments are collapsed to a single connected vs. un-connected binary treatment. Estimates derived from Bayesian logistic regression model. Point estimates are posterior medians and 
                                 the intervals are 5% to 95% quantiles of the empirical posterior distribution."))

ggsave("base_mod.png",scale=0.7)

# let's get conditional effects for subgroup analysis

country_con_int <- conditional_effects(country_con,"country_resp:connected")[[1]] %>% 
   mutate(effect1=fct_recode(effect1__,"Venezuela" = "Venezuela, Bolivarian Republic of..."),
          effect2=fct_recode(effect2__,
                             "Treatment" = "treatment",
                             "Control" = "control"
          ))

country_con_int %>% 
  ggplot(aes(y=estimate__,x=effect1)) +
  geom_pointrange(aes(ymin=lower__,
                      ymax=upper__,
                      colour=effect2,
                      shape=effect2),
                  position=position_dodge(width=.5)) +
  theme_tufte() +
  scale_colour_viridis_d() +
  labs(caption=stringr::str_wrap("Plot shows political connection treatment interacted with country intercepts. Estimates derived from Bayesian logistic regression model. Point estimates are posterior medians and 
                                 the intervals are 5% to 95% quantiles of the empirical posterior distribution."),
       x="",y="Predicted Investment Proportion") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
  guides(colour=guide_legend(title=""),
         shape=guide_legend(title="")) +
  theme(legend.position = "top")

ggsave("country_int.png",scale=0.7)

pol_int_res <- conditional_effects(pol_int_con,"connected:pol_eff_1",
                                                  int_conditions=list(pol_eff_1=seq(from=-1.5,to=1.5,length.out=100)))[[1]]
require(patchwork)


p1 <- pol_int_res %>% 
    mutate(connected=str_to_sentence(connected)) %>% 
  ggplot(aes(y=estimate__,x=as.numeric(as.character(pol_eff_1)))) +
  geom_ribbon(aes(ymin=lower__,ymax=upper__,fill=connected),alpha=0.5) +
  geom_line(aes(colour=connected)) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  ggtitle("Level of Connections") +
  theme_tufte() +
  guides(colour=guide_legend(title=""),
         fill=guide_legend(title="")) +
  labs(x="",y="Predicted Investment Proportion") +
  theme(legend.position = "top")

pol_con_res <- conditional_effects(pol_con_con,"connected:pol_con_1",
                                                  int_conditions=list(pol_con_1=seq(from=-1.3,to=1.6,length.out=100)))[[1]]


p2 <- pol_con_res %>% 
  mutate(connected=str_to_sentence(connected)) %>% 
  ggplot(aes(y=estimate__,x=as.numeric(as.character(pol_con_1)))) +
  geom_ribbon(aes(ymin=lower__,ymax=upper__,fill=connected),alpha=0.5) +
  geom_line(aes(colour=connected)) +
  ggtitle("Efficacy of Connections") +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  theme_tufte() +
  guides(colour=guide_legend(title=""),
         fill=guide_legend(title="")) +
  labs(x="",y="") +
  theme(legend.position = "top")

p1 + p2 + plot_layout(guides="collect") +
  plot_annotation(tag_levels = "A",
                  caption=stringr::str_wrap("Plot shows political connection treatment interacted with the respondent's political connections (A) and the efficacy of those connections (B). Estimates derived from Bayesian logistic regression model. Point estimates are posterior medians and 
                                 the intervals are 5% to 95% quantiles of the empirical posterior distribution.",
                                            width=75)) &
  theme(legend.position = "bottom")

ggsave("pol_con_int.png",scale=0.7)

# produce conditional effects for splines

prof_splines <- conditional_effects(tfp_full,c("tfp","tfp:connected"))

# make a plot for overall tfp

tfp_spline <- prof_splines$tfp %>% 
  ggplot(aes(y=estimate__,x=tfp)) +
  geom_ribbon(aes(ymin=lower__,ymax=upper__),fill="blue",alpha=0.5) +
  geom_line(linetype=2) +
  theme_tufte() +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
  labs(x="Investment TFP",y="Proportion Invested") +
  geom_hline(yintercept=0.5,linetype=3)

tfp_treat <- prof_splines$`tfp:connected` %>% 
  mutate(connected=stringr::str_to_sentence(connected)) %>% 
  ggplot(aes(y=estimate__,x=tfp)) +
  geom_ribbon(aes(ymin=lower__,ymax=upper__,
                  fill=connected),alpha=0.5) +
  geom_line(linetype=2,aes(group=connected)) +
  scale_fill_viridis_d() +
  theme_tufte() +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
  labs(x="Investment TFP",y="Proportion Invested") +
       # caption=stringr::str_wrap("Plot shows marginal effect of TFP on respondent choosing a profile (A) and TFP interacted with political connection treatment (B). Estimates derived from Bayesian logistic regression model. Point estimates are posterior medians and 
       #                           the intervals are 5% to 95% quantiles of the empirical posterior distribution.",
       #                           width=70)) +
  guides(fill=guide_legend(title="",label.position="bottom")) +
  theme(legend.position = "top") +
  geom_hline(yintercept=0.5,linetype=3)

tfp_treat

ggsave("spline_tfp.png",scale=0.7)

profit_splines <- conditional_effects(profit_full,c("sales_series:connected"))

profit_treat <- profit_splines$`sales_series:connected` %>% 
  mutate(connected=stringr::str_to_sentence(connected)) %>% 
  ggplot(aes(y=estimate__,x=sales_series)) +
  geom_ribbon(aes(ymin=lower__,ymax=upper__,
                  fill=connected),alpha=0.5) +
  geom_line(linetype=2,aes(group=connected)) +
  scale_fill_viridis_d() +
  theme_tufte() +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
  scale_x_continuous(labels=scales::dollar_format(accuracy=1)) +
  labs(x="Investment Profit",y="") +
       # caption=stringr::str_wrap("Plot shows the marginal effect of investment profit on a respondent choosing a profile interacted with the political connection treatment. Estimates derived from Bayesian logistic regression model. Point estimates are posterior medians and 
       #                           the intervals are 5% to 95% quantiles of the empirical posterior distribution.",
       #                           width=70)) +
  guides(fill=guide_legend(title="",label.position="bottom")) +
  geom_hline(yintercept=0.5,linetype=3)

profit_treat

ggsave("profit_spline.png")

tfp_treat + profit_treat + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A",
                  caption=stringr::str_wrap("Plot shows the marginal effect of investment profile TFP (A) and investment profile profit (B) on a respondent choosing a profile. Estimates are shown conditional on a connected (treatment) profile versus a un-connected (control) profile. Estimates derived from Bayesian logistic regression model with a spline. Point estimates are posterior medians and 
                                 the intervals are 5% to 95% quantiles of the empirical posterior distribution.",
                                            width=90)) &
  theme(legend.position = "bottom")

ggsave("tfp_profit.png")

# connected X sector plot
# better to plot differences in conditional investment shares

con_sector <- conditional_effects(pol_con_sector)

bind_rows(con_sector$sector_1,
          con_sector$`connected:sector_1`) %>% 
   mutate(sector_1=fct_recode(sector_1,
                              "Restaurants" = "ACCOMMODATION AND FOOD SERVICE ACTIVITIES",
                              "Other" = "ACTIVITIES OF EXTRATERRITORIAL ORGANISATIONS AND BODIES",
                              "Domestic" = "ACTIVITIES OF HOUSEHOLDS AS EMPLOYERS; UNDIFFERENTIATED GOODS- AND SERVICES-PRODUCING ACTIVITIES OF HOUSEHOLDS FOR OWN USE",
                              "Admin" = "ADMINISTRATIVE AND SUPPORT SERVICE ACTIVITIES",
                              "Agriculture" = "AGRICULTURE, FORESTRY AND FISHING",
                              "Arts" = "ARTS, ENTERTAINMENT AND RECREATION",
                              "Construction" = "CONSTRUCTION",
                              "Education" = "EDUCATION",
                              "Utilities" = "ELECTRICITY, GAS, STEAM AND AIR CONDITIONING SUPPLY",
                              "Finance" = "FINANCIAL AND INSURANCE ACTIVITIES",
                              "Health" = "HUMAN HEALTH AND SOCIAL WORK ACTIVITIES",
                              "ICT" = "INFORMATION AND COMMUNICATION",
                              "Manufacturing" = "MANUFACTURING",
                              "Mining" = "MINING AND QUARRYING",
                              "Other Service" = "OTHER SERVICE ACTIVITIES",
                              "Scientific" = "PROFESSIONAL, SCIENTIFIC AND TECHNICAL ACTIVITIES",
                              "Public Administration" = "PUBLIC ADMINISTRATION AND DEFENCE; COMPULSORY SOCIAL SECURITY",
                              "Real Estate" = "REAL ESTATE ACTIVITIES",
                              "Transportation" = "TRANSPORTATION AND STORAGE",
                              "Water" = "WATER SUPPLY; SEWERAGE, WASTE MANAGEMENT AND REMEDIATION ACTIVITIES",
                              "Commerce" = "WHOLESALE AND RETAIL TRADE; REPAIR OF MOTOR VEHICLES AND MOTORCYCLES")) %>% 
  mutate(connected=str_to_sentence(connected)) %>% 
  ggplot(aes(y=estimate__,x=reorder(sector_1,estimate__))) +
  geom_pointrange(aes(ymin=lower__,ymax=upper__,colour=connected),
                  position=position_dodge(width=0.5),
                  alpha=0.5) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  theme_tufte() +
  guides(colour=guide_legend(title=""),
         fill=guide_legend(title="")) +
  labs(x="",y="Amount Invested",
       caption=stringr::str_wrap("Plot shows the marginal effect of the respondent's company's sector on the share invested in a profile. Estimates are shown conditional on a connected (treatment) profile versus a un-connected (control) profile. Estimates derived from Bayesian logistic regression model. Point estimates are posterior medians and 
                                 the intervals are 5% to 95% quantiles of the empirical posterior distribution.",
                                      width=90)) +
  scale_y_continuous(labels=scales::dollar_format(accuracy=1,scale = 100)) +
  coord_flip() +
  theme(legend.position = "top")

ggsave("sector_diff.png")

# plot of expropriation/informal means and connections

whatis <- conditional_effects(con_exprop)

whatis$`sum_exprop:connected` %>% 
  mutate(treatment=recode(`effect2__`,treatment="Treatment",
                          control="Control")) %>% 
  ggplot(aes(y=`estimate__`,x=sum_exprop )) +
  geom_ribbon(aes(ymin=`lower__`,
                  ymax=`upper__`,
                  fill=treatment),alpha=0.5) +
  geom_line(aes(linetype=treatment)) +
  theme_tufte() +
  scale_fill_viridis_d() +
  theme(legend.position = "top") +
  xlab("Number of Times Informal Enforcement Used") +
  scale_y_continuous(labels=scales::percent) +
  ylab("Share of Money Invested") + 
  labs(caption=stringr::str_wrap("Plot shows the marginal effect of the respondent's company's use of informal enforcement on the share invested in a profile. Estimates are shown conditional on a connected (treatment) profile versus a un-connected (control) profile. Estimates derived from Bayesian logistic regression model. Point estimates are posterior medians and 
                                 the intervals are 5% to 95% quantiles of the empirical posterior distribution.",
                                 width=90)) +
  guides(fill=guide_legend(title=""),
         linetype=guide_legend(title=""))

ggsave("exprop_treatment.png")  
