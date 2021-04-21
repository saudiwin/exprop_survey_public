# script that munges and prepares data

# change path to reflect most recent data from Qualtrics

qual_data <- readRDS("data/qual_data.rds")

# let's test for difference between exp 1 and exp 2

clicks_any <- !is.na(qual_data$clicks1) | !is.na(qual_data$clicks2) | !is.na(qual_data$clicks3) | !is.na(qual_data$clicks4)
qual_data <- qual_data %>% 
  mutate_at(vars(matches("click")), function(c,clicks_any) {
    c <- ifelse(clicks_any,coalesce(as.numeric(c),0),c)
  },clicks_any) %>% 
  mutate(clicks_comb = as.numeric(clicks1) + as.numeric(clicks2) + 
           as.numeric(clicks3) + as.numeric(clicks4))

t.test(formula=clicks_comb~short_long,data=filter(qual_data,EndDate>ymd_hms("2020-01-22 12:00:00"),clicks_comb<50))
kruskal.test(formula=clicks_comb~short_long,data=filter(qual_data,EndDate>ymd_hms("2020-01-22 12:00:00")))
wilcox.test(formula=clicks_comb~short_long,data=filter(qual_data,EndDate>ymd_hms("2020-01-22 12:00:00")),paired=F)
summary(glm(clicks_comb~mobile+short_long+position,data=filter(qual_data,EndDate>ymd_hms("2020-01-22 12:00:00"),clicks_comb<50),
            family="poisson"))

summary(glm(clicks_comb~mobile+short_long*position,data=filter(qual_data,EndDate>ymd_hms("2020-01-22 12:00:00"),clicks_comb<50),
            family="poisson"))

summary(lm(`Duration (in seconds)`~mobile+short_long*position,data=filter(qual_data,EndDate>ymd_hms("2020-01-22 12:00:00"),clicks_comb<50)))

summary(lm(`Duration (in seconds)`~mobile+short_long*position,data=filter(qual_data,`Duration (in seconds)`<10000)))

summary(glm(clicks_comb~mobile+short_long*position,data=filter(qual_data,`Duration (in seconds)`<10000),
            family="poisson"))

qual_data %>% 
  ggplot(aes(x=clicks_comb)) +
  geom_histogram() +
  facet_wrap(~short_long,ncol=1) +
  scale_x_log10() +
  theme_minimal()

# need to run a model predicting outcome with treatments
# using betareg
# transform outcome & recode data from survey

recode_currency <- function(col) {
  
  col <- sapply(col, function(w) {
    
    if(is.na(w)) {
      return(NA)
    }
    
    if(grepl(x=w,pattern="M")) {
      as.numeric(str_remove_all(w,"[M,'k$-]"))*1000000
    } else if(grepl(x=w,pattern="k")) {
      as.numeric(str_remove_all(w,"[M,'k$-]"))*100000
    } else {
      as.numeric(str_remove_all(w,"[M,'k$-]"))
    }
  })
  
  col
  
}

# need to create some new variables

qual_data <- mutate(qual_data,tfpA=coalesce(tfpA2,tfpA),
                    tfpB=coalesce(tfpB2,tfpB),
                    exprop_risk=ordered(exprop_risk,
                                        levels=c("Very low",
                                                 "Low",
                                                 "Moderate",
                                                 "High",
                                                 "Very high")),
                    man_belief_2=ordered(man_belief_2,
                                         levels=c("Strongly disagree",
                                                  "Somewhat disagree",
                                                  "Neither agree nor disagree",
                                                  "Somewhat agree",
                                                  "Strongly agree")))

# need to create expropriation measure summing across variables

exprop_vars <- select(qual_data,matches("measure"),ResponseId) %>% 
  gather(key="var",value="value",-ResponseId) %>% 
  group_by(ResponseId) %>% 
  summarize(sum_exprop=sum(grepl(x=value,pattern="Contacted other groups|Settlement outside")))

qual_data <- left_join(qual_data,exprop_vars,by="ResponseId")

saveRDS(qual_data,"data/qual_data_vn_uk_eg.rds")

exp1 <- select(qual_data,outcomeA="outcome1",
               outcomeC="outcome2",
               outcomeE="outcome3",
               outcomeG="outcome4",
               matches("[a-z][A|C|E|G]",ignore.case=F),clicksA="clicks1",clicksC="clicks2",clicksE="clicks3",
               clicksG="clicks4",
               position,short_long,ResponseId,Duration,country_resp="country",
               ceo,sector_1,pol_con_1,pol_eff_1,experience,educ,sum_exprop,exprop_risk,
               income_access,man_belief_2) %>% 
  mutate_at(vars(matches("assets|profit")),
            recode_currency) %>% 
  mutate(experiment=1) %>% 
  select(-RecipientEmail,
         -DistributionChannel)


exp2 <- select(qual_data,outcomeB="outcome1",
               outcomeD="outcome2",
               outcomeF="outcome3",
               outcomeH="outcome4",
               matches("[a-z][B|D|F|H]",ignore.case=F),clicksB="clicks1",clicksD="clicks2",
               clicksF="clicks3",clicksH="clicks4",
               position,short_long,ResponseId,Duration,country_resp="country",
               ceo,sector_1,pol_con_1,pol_eff_1,experience,educ,sum_exprop,
               exprop_risk,income_access,man_belief_2) %>% 
  mutate_at(vars(matches("assets|profit")),
            recode_currency) %>% 
  mutate(experiment=2) %>% 
  select(-StartDate,
         -EndDate,
         -RecordedDate,
         -RecipientFirstName)


exp1_treat <- select(exp1,-matches("outcome")) %>% gather(key = treatment,value=value,-experiment,-Duration,
                                                          -position,-ResponseId,-short_long,-country_resp,-sum_exprop,
                                                          -ceo,-sector_1,-pol_con_1,-pol_eff_1,-experience,-educ,-exprop_risk,
                                                          -income_access,
                                                          -man_belief_2) %>% 
  mutate(profile=str_extract(treatment,"[A-Z]"),
         treatment=str_remove(treatment,"[A-Z]"))

exp1_out <- select(exp1,matches("outcome"),ResponseId) %>% gather(key = treatment,value=outcome,-ResponseId) %>% 
  mutate(profile=str_extract(treatment,"[A-Z]"),
         outcome=str_remove(outcome,"[A-Z]")) %>% 
  select(-treatment)


exp1_treat <- left_join(exp1_treat,exp1_out,by=c("ResponseId","profile"))

exp2_treat <- select(exp2,-matches("outcome")) %>% gather(key = treatment,value=value,-experiment,-position,-Duration,
                                                          -ResponseId,-short_long,-country_resp,-sum_exprop,
                                                          -ceo,-sector_1,-pol_con_1,-pol_eff_1,-experience,-educ,-exprop_risk,
                                                          -income_access,-man_belief_2) %>% 
  mutate(profile=str_extract(treatment,"[A-Z]"),
         treatment=str_remove(treatment,"[A-Z]"))

exp2_out <- select(exp2,matches("outcome"),ResponseId) %>% gather(key = treatment,value=outcome,-ResponseId) %>% 
  mutate(profile=str_extract(treatment,"[A-Z]"),
         outcome=str_remove(outcome,"[A-Z]")) %>% 
  select(-treatment)

exp2_treat <- left_join(exp2_treat,exp2_out,by=c("ResponseId","profile"))

names(exp1_treat) <- str_replace(names(exp1_treat),"(?<=[a-z2])[CEG]",
                                 "A")

names(exp2_treat) <- str_replace(names(exp2_treat),"(?<=[a-z]2)[DFH]",
                                 "B")

combined <- bind_rows(exp1_treat,
                      exp2_treat)

combined <- combined %>% 
  mutate(outcome=as.numeric(outcome),
         outcome=ifelse(experiment==1,100-outcome,outcome),
         outcome_norm=(outcome - min(outcome,na.rm = T))/(max(outcome,na.rm=T) - 
                                                            min(outcome,na.rm = T)),
         outcome_prop=(outcome_norm* (sum(!is.na(outcome_norm))-1) + 0.5)/sum(!is.na(outcome_norm)))

# make it wide

combined <- spread(combined,key = "treatment",value="value")

# replace missing click values with mean of respondent

combined <- mutate(combined, clicks=as.numeric(clicks))

combined <- mutate(combined,
                   connections=recode(connections,
                                      `El propietario es ex oficial de policía`="own_police",
                                      `El propietario es miembro del partido político del presidente`="own_pres_party",
                                      `El propietario es sobrino del primer ministro`="own_pm_nephew",
                                      `El propietario es un ex miembro del parlamento`="own_mp",
                                      `El propietario es un general retirado de las Fuerzas Armadas`="own_gen_army",
                                      `El propietario no tiene interés en la política`="control",
                                      `Un funcionario público de rango intermedio está en la Junta Directiva de la empresa`="board_med_crat",
                                      `Un hijo del presidente forma parte de la Junta Directiva de la empresa`="board_son_pres",
                                      `Un miembro del parlamento está en la Junta Directiva de la empresa`="board_mp",
                                      `Un ministro está en la Junta Directiva de la empresa`="board_minister",
                                      `El propietario es ex compañero de clase del presidente`="own_pres_classmate",
                                      `Una hija del presidente forma parte de la Junta Directiva de la empresa`="board_daughter_pres",
                                      `El propietario está casado con un hijo del presidente`="own_pres_son_inlaw",
                                      `La propietaria es sobrina del primer ministro`="own_pm_niece",
                                      `La propietaria está casado con una hija del presidente`="own_pres_daughter_inlaw",
                                      `Владелец - бывший генерал`="own_gen_army",
                                      `Владелец - бывший офицер полиции`="own_police",
                                      `Владелец - бывший член парламента`="own_mp",
                                      `Владелец - зять президента`="own_pres_son_inlaw",
                                      `Владелец - одноклассник президента`="own_pres_classmate",
                                      `Владелец - племянник премьер министра`="own_pm_nephew",
                                      `Владелец - племянница премьер министра`="own_pm_niece",
                                      `Владелец - член политической партии президента`="own_pres_party",
                                      `Владелица - невестка президента`="own_pres_daughter_inlaw",
                                      `Дочь президента входит в совет директоров компании`="board_daughter_pres",
                                      `Министр входит в совет директоров компании`="board_minister",
                                      `Сын президента входит в совет директоров компании`="board_son_pres",
                                      `У владелеца нет интереса к политике`="control",
                                      `Чиновник среднего уровня входит в совет директоров компании`="board_med_crat",
                                      `Член парламента входит в совет директоров компании`="board_mp",
                                      `المالك  جنرال سابق في الجيش`="own_gen_army",
                                      `المالك  زميل دراسة سابق للرئيس`="own_pres_classmate",
                                      `المالك  ضابط سابق في الشرطة`="own_police",
                                      `المالك عضو سابق في البرلمان`="own_mp",
                                      `المالك عضو في الحزب السياسي الذي ينتمي له الرئيس`="own_pres_party",
                                      `المالك غير مهتم بالسياسية`="control",
                                      `المالك متزوج من ابنة الرئيس`="own_pres_son_inlaw",
                                      `المالك هو إبن أخ رئيس الوزراء`="own_pm_nephew",
                                      `المالكة هي بنت أخ رئيس الوزراء`="own_pm_niece",
                                      `يتضمن مجلس إدارة الشركة إبن الرئيس`="board_son_pres",
                                      `المالكة متزوجة من ابن الرئيس`="own_pres_daughter_inlaw",
                                      `يتضمن مجلس إدارة الشركة إبنة الرئيس`="board_daughter_pres",
                                      `يتضمن مجلس إدارة الشركة بيروقراطي متوسط المستوى`="board_med_crat",
                                      `يتضمن مجلس إدارة الشركة رئيس الوزارة`="board_minister",
                                      `يتضمن مجلس إدارة الشركة عضو من البرلمان`="board_mp",
                                      `Mid-level bureaucrat is on board of company`="board_med_crat",
                                      `Owner has no interest in politics`="control",
                                      `Owner is a niece of the prime minister`="own_pm_niece",
                                      `Owner is member of the President's political party`="own_pres_party",
                                      `President's daughter is on the board of the company`="board_daughter_pres",
                                      `Owner is former general in the military`="own_gen_army",
                                      `President's son is on the board of the company`="board_son_pres",
                                      `Owner is former member of parliament`="own_mp",
                                      `Owner is a nephew of the prime minister`="own_pm_nephew",
                                      `Owner is former officer in the police`="own_police",
                                      `Owner is former classmate of the President`="own_pres_classmate",
                                      `Owner is married to the President's son`="own_pres_daughter_inlaw",
                                      `Head of ministry is on board of company`="board_minister",
                                      `Member of parliament is on board of company`="board_mp",
                                      `Owner is married to the President's daughter`="own_pres_son_inlaw"),
                   sector=forcats::fct_collapse(sector,retail=c("Retail","بيع التجزئة",
                                                                "Ритейл","Ventas al menudeo"),
                                                agriculture=c("Agriculture","Сельское хозяйство",
                                                              "Agricultura","الزراعة"),
                                                manufacturing=c("Manufacturing","Производство",
                                                                "Manufactura","التصنيع"),
                                                construction=c("Construction","Строительство",
                                                               "Construcción","الإنشاءات"),
                                                energy=c("Energy Sector","Энергетика",
                                                         "Sector energía","قطاع الطاقة"),
                                                telecom=c("Telecommunications","Telecomunicaciones",
                                                          "Телекоммуникации","الإتصالات"),
                                                financials=c("Financial Services","خدمات التمويل",
                                                             "Финансовые услуги","Servicios financieros")),
                   connections=forcats::fct_relevel(factor(connections),"control"),
                   connected=ifelse(connections=="control","control","treatment"),
                   assets=as.numeric(assets),
                   conn_collapse=fct_collapse(connections,
                                              bureaucracy=c("board_med_crat","board_minister"),
                                              security=c("own_gen_army",
                                                         "own_police"),
                                              relationship=c("own_pm_nephew",
                                                             "own_pres_son_inlaw",
                                                             "own_pm_niece",
                                                             "own_pres_daughter_inlaw",
                                                             "board_daughter_pres",
                                                             "board_son_pres",
                                                             "own_pres_classmate"),
                                              parliament=c("own_mp",
                                                           "board_mp"),
                                              control="control"),
                   conn_own_type=fct_collapse(connections,board=c("board_minister",
                                                                  "board_daughter_pres",
                                                                  "board_son_pres",
                                                                  "board_mp",
                                                                  "board_med_crat"),
                                              own=c("own_police",
                                                    "own_pres_daughter_inlaw",
                                                    "own_pres_party",
                                                    "own_gen_army",
                                                    "own_mp",
                                                    "own_pres_classmate",
                                                    "own_pm_nephew",
                                                    "own_pres_son_inlaw",
                                                    "own_pm_niece"),
                                              control="control"),
                   gender=fct_collapse(connections,
                                       female=c("own_pres_daughter_inlaw",
                                                "board_daughter_pres",
                                                "own_pm_niece"),
                                       male=c("own_pm_nephew",
                                              "own_pres_son_inlaw",
                                              "board_son_pres"),
                                       neutral=c("own_police",
                                                 "own_gen_army",
                                                 "own_pres_classmate",
                                                 "own_pres_party",
                                                 "own_mp",
                                                 "board_med_crat",
                                                 "board_mp",
                                                 "board_minister",
                                                 "board_minister"),
                                       control="control"),
                   gender=fct_relevel(gender,"control"),
                   perf=recode(perf,`Покрывает расходы`="break_even",
                               `Теряет немного денег`="lose_some",
                               `Теряет много денег`="lose_much",
                               `Algo rentable`="gain_some",
                               `Немного прибыльная`="gain_some",
                               `Очень прибыльная`="gain_much",
                               `Perder mucho dinero`="lose_much",
                               `Cubrir los gastos`="break_even",
                               `Muy rentable`="gain_much",
                               `Perder algo de dinero`="lose_some",
                               `خسارة الكثير من المال`="lose_much",
                               `خسارة بعض المال`="lose_some",
                               `مربحة إلى حد ما`="gain_some",
                               `التعادل (لا يوجد ربح أو خسارة)`="break_even",
                               `مربح للغاية`="gain_much",
                               `Losing A Lot of Money`="lose_much",
                               `Break Even`="break_even",
                               `Somewhat Profitable`="lose_some",
                               `Losing Some Money`="lose_some",
                               `Very Profitable`="gain_much"),
                   country=fct_collapse(country,Germany=c("Alemania","Germany","Германия",
                                                          "ألمانيا"),
                                        Egypt=c("Egypt","مصر"),
                                        Ukraine=c("Украина","Ukraine"),
                                        Saudi=c("Arabia Saudita","Saudi Arabia","Саудовская Аравия",
                                                "المملكة العربية السعودية"),
                                        Brazil=c("Brasil","Brazil","Бразилия",
                                                 "البرازيل"),
                                        China=c("China","Китай",
                                                "الصين"),
                                        Korea=c("Corea del Sur","Южная Корея","South Korea",
                                                "كوريا الجنوبية"),
                                        USA=c("United States","Estados Unidos","США",
                                              "الولايات المتحدة الأمريكية"),
                                        Japan=c("Japan","Япония","Japón",
                                                "اليابان"),
                                        Russia=c("Rusia","Россия",
                                                 "روسيا"),
                                        Venezuela=c("Venezuela","Venezuela, Bolivarian Republic of...")),
                   country=fct_relevel(country,"Brazil"),
                   own=fct_collapse(own,foreign=c("100% ملكية أجنبية",
                                                  "100% иностранное предприятие",
                                                  "100% Foreign-owned",
                                                  "100% empresa extranjera"),
                                    soe=c("Domestic - Public",
                                          "محلي - عام",
                                          "Nacional - Pública",
                                          "отечественные государственные фирмы"),
                                    private=c("Domestic - Private",
                                              "محلي - خاص",
                                              "Nacional - Privada",
                                              "отечественные частные фирмы")),
                   educ=ordered(educ,levels=c("High School or lower",
                                              "University (2-year)",
                                              "University (4-year)",
                                              "PhD")),
                   employees=as.numeric(employees),
                   years=as.numeric(years))

# need to figure out what to do with profit/revenue/etc

loop_tfp <- lapply(unique(combined$ResponseId), function(r) {
  this_data <- filter(combined,ResponseId==r)

  tfp <- str_split(this_data$tfp,"\\|")

  check_lengths <- sapply(tfp,length)
  
  #print(paste0("now on ", this_data$ResponseId[1]))

  if(any(check_lengths>1) && all(this_data$short_long=="Long",na.rm=T)) {
    # convert back to observed profit/sales
    # iterate over experiment
    over_exp <- lapply(1:nrow(this_data), function(i) {
      this_exp <- slice(this_data,i)
      
      if(is.na(this_exp$tfp)) {
        return(tibble(lm_intercept=NA,
                      linear=NA,
                      squared=NA,
                      ResponseId=this_exp$ResponseId,
                      experiment=this_exp$experiment,
                      profile=this_exp$profile)) 
      }
      
      sales <- sqrt(this_exp$assets)*sqrt(this_exp$employees)*as.numeric(tfp[[i]])
      profit <- sales - sqrt(this_exp$assets)*sqrt(this_exp$employees)
      all_years <- 1:length(tfp[[i]])
      # get coefficients back
      lm_fit <- lm(profit~all_years + I(all_years^2))
      
      type <- check_curve(all_years,profit)$ctype
      
      direction <- if_else((profit[max(all_years)]-profit[min(all_years)])>0,
                           "up",
                           "down")

      # store lm_intercept/slope/squared term
      tibble(lm_intercept=lm_fit$coefficients[1],
             linear=lm_fit$coefficients[2],
             squared=lm_fit$coefficients[3],
             type_curve=type,
             tfp_mean=mean(as.numeric(tfp[[i]])),
             tfp_series=list(as.numeric(tfp[[i]])),
             perf_series=list(profit),
             sales_series=list(sales),
             total_perf=direction,
             profitable=profit[max(all_years)]>0,
             end_profit=profit[max(all_years)],
             ResponseId=this_exp$ResponseId,
             experiment=this_exp$experiment,
             profile=this_exp$profile)
    }) %>% bind_rows


  } else {
    # did not have dynamic TFP, return static results
    
    over_exp <- tibble(lm_intercept=NA,
                       linear=NA,
                       squared=NA,
                       tfp_mean=NA,
                       tfp_series=NA,
                       perf_series=NA,
                       ResponseId=this_data$ResponseId,
                       experiment=this_data$experiment,
                       profile=this_data$profile)
    
  }

  over_exp

}) %>% bind_rows

# join back to original data

combined <- left_join(combined,loop_tfp,by=c("ResponseId","experiment","profile")) %>% 
  mutate(combined_perf=case_when(type_curve %in% c("convex_concave",
                                                   "concave_convex")~"break_even",
                                 type_curve=="concave" & total_perf=="down"~"lose_much",
                                 type_curve=="convex" & total_perf == "down"~"lose_some",
                                 type_curve=="concave" & total_perf=="up"~"gain_some",
                                 type_curve=="convex" & total_perf=="up"~"gain_much",
                                 TRUE~perf),
         perf=ordered(perf,levels=c("lose_much","lose_some","break_even",
                                    "gain_some","gain_much")),
         profit_combined=coalesce(end_profit,as.numeric(profit)),
         perf=ordered(perf,levels=c("lose_much","lose_some","break_even","gain_some","gain_much")),
         combined_perf=ordered(combined_perf,levels=c("lose_much","lose_some","break_even","gain_some","gain_much")))

# scale variables

combined <- mutate_at(combined,c("experience","profit","profit_combined","employees","years","assets",
            "pol_con_1","pol_eff_1"),~scale(as.numeric(.)))



# remove 50s, 51s and 63s (possible missing values)

combined_clean <- group_by(combined,ResponseId) %>% 
  mutate(all50=(!(all(outcome==50) || all(outcome==63) || all(outcome==51) || all(outcome==52) || all(outcome==37)))) %>% 
  filter(all50,!is.na(outcome))

# only ones where we measured clicks

combined_clean_clicks <- filter(combined_clean,!is.na(clicks))


# need to add in tfp at each level

tfp_data <- parallel::mclapply(1:nrow(combined_clean), function(i) {
  
  
  if(is.null(combined_clean$tfp_series[[i]])) {
    tfp <- NA_real_
    sales <- NA_real_
    profit <- NA_real_
  } else {
    tfp <- combined_clean$tfp_series[[i]]
    profit <- combined_clean$perf_series[[i]]
    sales <- combined_clean$sales_series[[i]]
  }
  
  tibble(tfp=tfp,
         sales_series=sales,
         profit_series=profit,
         ResponseId=combined_clean$ResponseId[i],
         experiment=combined_clean$experiment[i],
         profile=combined_clean$profile[i]) %>% 
    mutate(years=1:n())
},mc.cores=3) %>% bind_rows


tfp_data <- left_join(tfp_data,select(combined_clean,outcome_norm,connected,connections,
                                      country_resp,country,ResponseId,experiment,profile),
                      by=c("ResponseId","experiment","profile"))
