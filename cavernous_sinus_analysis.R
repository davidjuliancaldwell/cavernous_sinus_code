#########################################
# load libraries
library(car)
library(tidyr)
library(plyr)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggsci)
library(gridExtra)
library(cowplot)
library(here)
library(sjPlot)
library(coin)
library(emmeans)
library(readxl)
library(gtsummary)
library(labelled)
library(webshot)
library(flextable)
library(officer)
library(emmeans)
library(janitor)
library(aod)
library(MASS)
library(brant)
library(nnet)
library(purrr)
library(broom)
library(lmerTest)
library(GGally)
library(nlme)
library(glmmTMB)
library(AER)
library(DHARMa)
library(elrm)
library(stats)
library(ordinal)

setwd("~/code/cavernous_sinus_code")
rootDir = here()
dataDir = '/Users/davidcaldwell/OneDrive - UCSF/Research/UW_research/cav_sinus_total/cav_sinus_2022'
saveFig = FALSE
include_na_table = FALSE
doOrdinal = FALSE
doMixed = TRUE
fontSize = 10

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8.5, height = 11),
  type = "continuous",
  page_margins = page_mar()
)

### read data
#data_file <- read_excel(file.path(dataDir,"Cavernous sinus cases, data collection sheet.xlsx"))
data_file <- read_excel(file.path(dataDir,"cav_file_for_analysis.xlsx"))


column_names = colnames(data_file)
# print column names to console for easy copy and paste for selection to build code 
cat(column_names,sep="', '")


### clean up data
data_file <- mutate_all(data_file,.funs=str_to_lower)

if (include_na_table){
data_file<-data_file %>% mutate(  across(everything() & where(is.character), ~replace_na(.x, "Missing")))
} else {
  data_file<-data_file %>% mutate(  across(everything(), ~na_if(., "na")))
}

#data_file$Surgery_DateTime <- excel_numeric_to_date(as.numeric(as.character(data_file$Surgery_DateTime)), date_system = "modern")    


data_file <- data_file %>% mutate(`Age at the time of surgery`=as.numeric(`Age at the time of surgery`),
                                  `Tumor size (T, cm)` =as.numeric(as.character(`Tumor size (T, cm)`)),
                                  `Tumor size (AP, cm)` =as.numeric(as.character(`Tumor size (AP, cm)`)),
                                  `Tumor size (RC, cm)` =as.numeric(as.character(`Tumor size (RC, cm)`)),
                                  `Tumor volume (ABC/2, cm3)` = as.numeric(as.character(`Tumor volume (ABC/2, cm3)`)),
                                  `EBL (mL)` = as.numeric(as.character(`EBL (mL)`)),
                                  `Length of Surgery (Anesthesia time in Min)` = as.numeric(as.character(`Length of Surgery (Anesthesia time in Min)`)),
                                  `Length of follow-up (months)` = as.numeric(as.character(`Length of follow-up (months)`)),
                                  `Length of ICU Stay (Days)` = as.numeric(as.character(`Length of ICU Stay (Days)`)),
                                  `Overall length of stay (days)` = as.numeric(as.character(`Overall length of stay (days)`))
#                                  `Surgery_DateTime`=as.Date(`Surgery_DateTime`,origin = "1899-12-30")
                                  )

data_file <- data_file %>% mutate(across(c(`Tumor only in cavernous sinus (Y=1, 2 = N, NA = NA)`,
                                           `Tumor epicenter (1 = cavernous sinus, 2 = MF 3 = PF 4 = ITF 5 = sella 6 = sphenoid sinus 7 = orbital apex`),
                                  factor))



data_file <- data_file %>% mutate(minor_comp = case_when(((`UTI (Y=1, N=2)`=="1")|(`Pneumonia (y=1 N=2)`=="1")|(`DVT (Y=1 N=2)`=="1")|(`PE (Y=1, N=2)`=="1")|(`MI (Y=1 N=2)`=="1")|(`Sinus thrombosis (Y=1, N=2)`=="1")|(`CSF Leak (Y=1 N=2)`=="1")|(`Need for OR for wound revision (y=1, N=2)`=="1")|(`Need for permanent CSF diversion (Y=1, N=2)`=="1")|(`Immediate PO Frontalis Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)`=="3")|(`PO 6 weeks-3 months Frontalis Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)`=="3"))~1,
                                                         TRUE~0),
                                  major_comp = case_when(((`Symptomatic PO hematoma (Y=1 N=2)`=="1")|(`Need for OR for hematoma evacuation (Y=1 N=2)`=="1")| (`Stroke (y=1, N=2)`=="1"))~1,
                                                         TRUE~0))


if (saveFig){
  
  if(include_na_table){
    ### do everything
    data_summary_total <- data_file %>% tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                                     all_categorical() ~ "{n} / {N} ({p}%)"),
                                                    digits = all_continuous() ~ 1,
                                                    missing = "no") 
    #missing_text = "(Missing)")  
    
  data_summary_total %>%
    as_gt() %>%             # convert to gt table
    gt::gtsave(             # save table as image
      filename = "total_summary.png"
    )
  
  data_summary_total %>%
    as_gt() %>%             # convert to gt table
    gt::gtsave(             # save table as Html
      filename = "total_summary.html"
    )
  
  data_summary_total_flex <- data_summary_total %>% as_flex_table() 
  data_summary_total_flex <- fontsize(data_summary_total_flex,size=fontSize)
  
  save_as_docx(data_summary_total_flex,path="total_summary.docx",pr_section = sect_properties)
  }
  else{
    ### do everything
    data_summary_total <- data_file %>% tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                                     all_categorical() ~ "{n} / {N} ({p}%)"),
                                                    digits = all_continuous() ~ 1,
                                                    missing = "no") 
    #missing_text = "(Missing)")  
    
    data_summary_total %>%
      as_gt() %>%             # convert to gt table
      gt::gtsave(             # save table as image
        filename = "total_summary_no_na.png"
      )
    
    data_summary_total %>%
      as_gt() %>%             # convert to gt table
      gt::gtsave(             # save table as Html
        filename = "total_summary_no_na.html"
      )
    
    data_summary_total_flex <- data_summary_total %>% as_flex_table() 
    data_summary_total_flex <- fontsize(data_summary_total_flex,size=fontSize)
    
    save_as_docx(data_summary_total_flex,path="total_summary_no_na.docx",pr_section = sect_properties)
    
  }
  }



### Model fitting time 

# now go back and replace Missing with NaN for model fitting
# linear mixed model to account for missing data


data_file_stats<-data_file %>% mutate(  across(everything() & where(is.character), ~na_if(., "Missing")))
data_file_stats<-data_file %>% mutate(  across(everything()& where(is.character), ~na_if(., "na")))
data_file_stats<-data_file %>% mutate(  across(everything()& where(is.character), ~na_if(., "NA")))
data_file_stats<-data_file %>% mutate(  across(everything()& where(is.numeric), ~na_if(., NA)))


data_file_stats <- data_file_stats %>% rename(age = `Age at the time of surgery`,
                                              path = `Pathology (Meningioma = 1, Chordoma = 2, Chondrosarcoma = 3, Schwannoma = 4, Adenoma = 5 hemangioma = 6 Metastasis/cancer = 7  Other = 8)`,
                                              t_size=`Tumor size (T, cm)`,
                                              ap_size =`Tumor size (AP, cm)`,
                                              rc_size = `Tumor size (RC, cm)`,
                                              volume = `Tumor volume (ABC/2, cm3)`,
                                              cav_only = `Tumor only in cavernous sinus (Y=1, 2 = N, NA = NA)`,
                                              epi = `Tumor epicenter (1 = cavernous sinus, 2 = MF 3 = PF 4 = ITF 5 = sella 6 = sphenoid sinus 7 = orbital apex`,
                                              surg_approach = `Surgical approach (Fronto-temporal approach =1, Subtemporal approach = 2 Posterior petrosal = 3  transnasal/transmaxillary = 4 Other = 5  , 1+2 = 6)`,
                                              resect = `Tumor resection based on PO MRI (1 = GTR, 2 NTR = No residual on MRI, but tumor known to be left, 3 = Subtotal resection (residual < 10 %) 4 = Partial resection (Residual  > 10 %) 5 = Biopsy`,
                                              skull_osteo = `Skull base osteotomy (1 = None 2 = Posterolateral orbitotomy 3 = Full orbitotomy 4 = Zygomatic osteotomy 5 = Orbito-zygomatic osteotomy 6 = retrolabyrinthine 7 = tranlabyrinthine 8= petrous apex 9= 2,4 10 = 2,4,8 11 = 4,8 12 = 4,6,8 13 = 6,8`,
                                              optic = `Optic canal decompression and anterior clinoidectomy Y =1, 2=N)`,
                                              wall_cav = `Wall of the cavernous sinus approached ( 1= Lateral wall, 2 = Superior wall 3 = Posterior wall 4 = Anterior wall 5 = Medial wall 6 =1,2 7 =2,4, 8 =4,5 9 =1,3 10 = 2,3`,
                                              lat = `Lateral wall (Y=1, N=2)`,
                                              med = `Medial wall (Y=1, N=2)`,
                                              sup = `Superior wall (Y=1, N=2)`,
                                              post = `Posterior wall (Y=1, N=2)`,
                                              ant = `Anterior wall (Y=1, N=2)`,
                                              grade = `Tumor grade (WHO) (NA = NA,`,
                                              post_treat = `Post-operative aditionnal treatment (None=1, Repeat surgery = 2, SRS =3 FSRT = 4 Proton therapy =5 Chemo/immunotherapy = 6 Other = 7, 3,4 = 8,  5,6 = 9 4, 6 = 10`,
                                              prev_rad = `Previous radiation therapy ((Y=1, 2 = N)`,
                                              prev_surg = `Previous surgery (Y=1, 2 = N)`,
                                              po_1_cn_3 = `PO 1 year (or last follow up) CN 3 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)`,
                                              po_6_3_months_cn_3 = `PO 6 weeks-3 months CN 3 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)`,
                                              imm_cn_3 = `Immediate PO CN 3 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)`,
                                              po_1_cn_4 = `PO 1 year (or last follow up) CN 4 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)`,
                                              po_6_3_months_cn_4 = `PO 6 weeks-3 months CN 4 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)`,
                                              imm_cn_4 = `Immediate PO CN 4 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)`,
                                              po_1_cn_5 = `PO 1 year (or last follow up) CN 5 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)`,
                                              po_6_3_months_cn_5 = `PO 6 weeks-3 months CN 5 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)`,
                                              imm_cn_5 = `Immediate PO CN 5 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)`,
                                              po_1_cn_6 = `PO 1 year (or last follow up) CN 6 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)`,
                                              po_6_3_months_cn_6 = `PO 6 weeks-3 months CN 6 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)`,
                                              imm_cn_6 = `Immediate PO CN 6 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)`
                                              )


# recode no's to yes's

data_file_stats <- data_file_stats %>% mutate(prev_rad = as.factor(recode(prev_rad,"1"=2,"2"=1)),
                                              prev_surg = as.factor(recode(prev_surg,"1"=2,"2"=1)),
                                              lat = as.factor(recode(lat,"1"=2,"2"=1)),
                                              sup = as.factor(recode(sup,"1"=2,"2"=1)),
                                              post = as.factor(recode(post,"1"=2,"2"=1))
                                                              )


if (doOrdinal){
data_file_stats <- data_file_stats %>% mutate(resect_condense = as.factor(recode(resect,"1" = 1,"2" = 0,"3"=0,"4"=0)),
                                              post_treat_condense = as.factor(recode(post_treat,"1" = 0,"2" = 1,"3"=1,"4"=1,"5"=1,"6"=1,"7"=1,"8"=1,"9"=1,"10"=1)),
                                              surg_approach_condense = as.factor(recode(surg_approach,"1"="1","2"="2","3"="3","4"="4","5"="4","6"="4")),
                                              epi_condense = as.factor(recode(epi,"1"="1","2"="2","3"="3","4"="4","5"="4","6"="4","7"="4")),
                                              path_condense = as.factor(recode(path,"1"="1","2"="2","3"="3","4"="4","5"="4","6"="4","7"="4","8"="4")),
                                              po_1_cn_3 = as.factor(recode(as.numeric(po_1_cn_3),"1" = 2, "2"=3,"3"=1)),
                                              po_6_3_months_cn_3 = as.factor(recode(as.numeric(po_6_3_months_cn_3),"1" = 2, "2"=3,"3"=1)),
                                              imm_cn_3 = as.factor(recode(as.numeric(imm_cn_3),"1" = 2, "2"=3,"3"=1)),
                                              po_6_3_months_cn_4 = as.factor(recode(as.numeric(po_6_3_months_cn_4),"1" = 2, "2"=3,"3"=1)),
                                              po_1_cn_4 = as.factor(recode(as.numeric(po_1_cn_4),"1" = 2, "2"=3,"3"=1)),
                                              imm_cn_4 = as.factor(recode(as.numeric(imm_cn_4),"1" = 2, "2"=3,"3"=1)),
                                              po_6_3_months_cn_5 = as.factor(recode(as.numeric(po_6_3_months_cn_5),"1" = 2, "2"=3,"3"=1)),
                                              po_1_cn_5 = as.factor(recode(as.numeric(po_1_cn_5),"1" = 2, "2"=3,"3"=1)),
                                              imm_cn_5 = as.factor(recode(as.numeric(imm_cn_5),"1" = 2, "2"=3,"3"=1)),
                                              po_6_3_months_cn_6 = as.factor(recode(as.numeric(po_6_3_months_cn_6),"1" = 2, "2"=3,"3"=1)),
                                              po_1_cn_6 = as.factor(recode(as.numeric(po_1_cn_6),"1" = 2, "2"=3,"3"=1)),
                                              imm_cn_6 = as.factor(recode(as.numeric(imm_cn_6),"1" = 2, "2"=3,"3"=1))
                                              )

} else{
  data_file_stats <- data_file_stats %>% mutate(resect_condense = as.factor(recode(resect,"1" = 1,"2" = 0,"3"=0,"4"=0)),
                                                post_treat_condense = as.factor(recode(post_treat,"1" = 0,"2" = 1,"3"=1,"4"=1,"5"=1,"6"=1,"7"=1,"8"=1,"9"=1,"10"=1)),
                                                surg_approach_condense = as.factor(recode(surg_approach,"1"="1","2"="2","3"="3","4"="4","5"="4","6"="4")),
                                                epi_condense = as.factor(recode(epi,"1"="1","2"="2","3"="3","4"="4","5"="4","6"="4","7"="4")),
                                                path_condense = as.factor(recode(path,"1"="1","2"="2","3"="3","4"="4","5"="4","6"="4","7"="4","8"="4")),
                                                po_1_cn_3 = as.factor(recode(as.numeric(po_1_cn_3),"1" = 2, "2"=2,"3"=1)),
                                                po_6_3_months_cn_3 = as.factor(recode(as.numeric(po_6_3_months_cn_3),"1" = 2, "2"=2,"3"=1)),
                                                imm_cn_3 = as.factor(recode(as.numeric(imm_cn_3),"1" = 2, "2"=2,"3"=1)),
                                                po_1_cn_4 = as.factor(recode(as.numeric(po_1_cn_4),"1" = 2, "2"=2,"3"=1)),
                                                po_6_3_months_cn_4 = as.factor(recode(as.numeric(po_6_3_months_cn_4),"1" = 2, "2"=2,"3"=1)),
                                                imm_cn_4 = as.factor(recode(as.numeric(imm_cn_4),"1" = 2, "2"=2,"3"=1)),
                                                po_1_cn_5 = as.factor(recode(as.numeric(po_1_cn_5),"1" = 2, "2"=2,"3"=1)),
                                                po_6_3_months_cn_5 = as.factor(recode(as.numeric(po_6_3_months_cn_5),"1" = 2, "2"=2,"3"=1)),
                                                imm_cn_5 = as.factor(recode(as.numeric(imm_cn_5),"1" = 2, "2"=2,"3"=1)),
                                                po_1_cn_6 = as.factor(recode(as.numeric(po_1_cn_6),"1" = 2, "2"=2,"3"=1)),
                                                po_6_3_months_cn_6 = as.factor(recode(as.numeric(po_6_3_months_cn_6),"1" = 2, "2"=2,"3"=1)),
                                                imm_cn_6 = as.factor(recode(as.numeric(imm_cn_6),"1" = 2, "2"=2,"3"=1))
  )
  
}
dependent_vars = c("resect_condense","post_treat_condense","imm_cn_3","po_1_cn_3","po_6_3_months_cn_3","imm_cn_4","po_1_cn_4","po_6_3_months_cn_4","imm_cn_5","po_1_cn_5","po_6_3_months_cn_5","imm_cn_6","po_1_cn_6","po_6_3_months_cn_6")
independent_vars = c("surg_approach","surg_approach_condense","epi","epi_condense","path","path_condense","prev_rad","prev_surg","age","lat","sup","post")
independent_vars = c("surg_approach_condense","epi_condense","path_condense","prev_rad","prev_surg","lat","sup","post","age")


dependent_vars = set_names(dependent_vars)
independent_vars = set_names(independent_vars)

ggpairs(data_file_stats[,c("surg_approach_condense","epi_condense","path_condense","age","lat","sup","post","resect_condense","post_treat_condense")])


ggpairs(data_file_stats[,c("surg_approach_condense","epi_condense","path_condense","age","lat","sup","post","po_1_cn_3","po_1_cn_4","po_1_cn_5","po_1_cn_6")])


#https://aosmith.rbind.io/2018/08/20/automating-exploratory-plots/

jitter_fun = function(x,y){
  ggplot(data_file_stats,aes(x=.data[[y]],y=.data[[x]]) ) + 
    geom_jitter(height=0.2,width=0.2)
}

all_plots2 = map(dependent_vars,function(dependent_vars){
  map(independent_vars,function(independent_vars){
    jitter_fun(x=dependent_vars,y=independent_vars)
  })
})

plotnames = imap(all_plots2,~paste0("cav_sinus_",.y,"_",names(.x),".png")) %>% flatten()

if (saveFig){
walk2(plotnames, flatten(all_plots2), ~ggsave(filename = .x, plot = .y, 
                                             height = 7, width = 7))
}

plot3 <- ggplot(data_file_stats,aes(x=age,y=po_1_cn_6,color=path)) + geom_jitter(height=0.2,width=0.2)
plot3

formula1 <- list(); model1 <- list(); p1nonadjust <- list()
formula1_subsets <- list(); formula1_totals <- list();totals1 <- list(); subsets1<-list();data_frame1<- list();resexact_1<-list()
for (i in 1:length(independent_vars)) {
  formula1[[i]] = paste0("resect_condense", " ~ ", independent_vars[[i]])
  model1[[i]] = glm(formula1[[i]],data=data_file_stats,family="binomial") 
  anova_temp <- Anova(model1[[i]],type="II", test="Wald");
  p1nonadjust[[i]] <- round(anova_temp[[3]],3)
  

  print(summary(model1[[i]]))
  print(anova_temp)
  #print(wald.test(b=coef(model1[[i]])),Sigma = vcov(model1[[i]]),Terms= )
  #wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
  
  formula1_subsets[[i]] = paste0("~resect_condense + ", independent_vars[[i]])
  subsets1[[i]] = xtabs(formula1_subsets[[i]],data=data_file_stats)
  totals1[[i]]  = subsets1[[i]][1,]+subsets1[[i]][2,]
  data_frame1[[i]] = data.frame(indepvar = levels(as.data.frame(subsets1[[i]])[,2]),depvar=subsets1[[i]][2,],n=totals1[[i]])
  if (i<length(independent_vars)){
  resexact_1[[i]] = elrm(depvar/n ~ as.factor(indepvar), interest = ~as.factor(indepvar), iter=5000000, 
                    burnIn=5000, data=data_frame1[[i]], r=2)
  }
  else {
    resexact_1[[i]] = elrm(depvar/n ~ as.integer(indepvar), interest = ~as.integer(indepvar), iter=5000000, 
                           burnIn=5000, data=data_frame1[[i]], r=2)
    }
  summary(resexact_1[[i]])

  
}


p1adjust <- p.adjust(p1nonadjust,"BH")
p1total <- cbind(p1nonadjust,p1adjust)


fit.logit1 = glm(resect_condense ~ surg_approach + prev_rad + prev_surg + epi + age + lat + med + sup + post + ant + path,data=data_file_stats,family="binomial")
fit.logit1 = glm(resect_condense ~ surg_approach + prev_rad + prev_surg + epi + age + path,data=data_file_stats,family="binomial")

fit.logit1 = glm(resect_condense ~ surg_approach + prev_rad + prev_surg + age + path,data=data_file_stats,family="binomial")

fit.logit1 = glm(resect_condense ~ surg_approach_condense + prev_rad + prev_surg + age + path_condense + epi_condense+lat+post+sup,data=data_file_stats,family="binomial")

fit.logit1 = glm(resect_condense ~ prev_rad + prev_surg + age + path_condense + epi_condense+lat+post+sup,data=data_file_stats,family="binomial")

#http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/
# Extract model results
fit.logit1.data <- augment(fit.logit1) %>% 
  mutate(index = 1:n()) 

ggplot(fit.logit1.data, aes(index, .std.resid)) + 
  geom_point(aes(color = resect_condense), alpha = .5) +
  theme_bw()

fit.logit1.data %>% 
  filter(abs(.std.resid) > 3)

car::vif(fit.logit1)

formula2 <- list(); model2 <- list(); p2nonadjust <- list()
formula2_subsets <- list(); formula2_totals <- list();totals2 <- list(); subsets2<-list();data_frame2<- list();resexact_2<-list()

for (i in 1:length(independent_vars)) {
  formula2[[i]] = paste0("post_treat_condense", " ~ ", independent_vars[[i]])
  model2[[i]] = glm(formula2[[i]],data=data_file_stats,family="binomial") 
  anova_temp <- Anova(model2[[i]],type="II", test="Wald");
  p2nonadjust[[i]] <- round(anova_temp[[3]],3)
  
  
  print(summary(model2[[i]]))
  print(anova_temp)
  #print(wald.test(b=coef(model1[[i]]),Sigma = vcov(model1[[i]]),Terms= 1:3))
  #wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
  
  
  formula2_subsets[[i]] = paste0("~post_treat_condense + ", independent_vars[[i]])
  subsets2[[i]] = xtabs(formula2_subsets[[i]],data=data_file_stats)
  totals2[[i]]  = subsets2[[i]][1,]+subsets2[[i]][2,]
  data_frame2[[i]] = data.frame(indepvar = levels(as.data.frame(subsets2[[i]])[,2]),depvar=subsets2[[i]][2,],n=totals2[[i]])
  if (i<length(independent_vars)){
    resexact_2[[i]] = elrm(depvar/n ~ as.factor(indepvar), interest = ~as.factor(indepvar), iter=5000000, 
                           burnIn=5000, data=data_frame2[[i]], r=2)
  }
  else {
    resexact_2[[i]] = elrm(depvar/n ~ as.integer(indepvar), interest = ~as.integer(indepvar), iter=5000000, 
                           burnIn=5000, data=data_frame2[[i]], r=2)
  }
  summary(resexact_2[[i]])
  
}
p2adjust <- p.adjust(p2nonadjust,"BH")
p2total <- cbind(p2nonadjust,p2adjust)

fit.logit2 = glm(post_treat_condense ~ surg_approach + prev_rad + prev_surg + epi + age + lat + med + sup + post + ant + path,data=data_file_stats,family="binomial")
fit.logit2 = glm(post_treat_condense ~ surg_approach + prev_rad + prev_surg + epi + age + path,data=data_file_stats,family="binomial")
fit.logit2 = glm(post_treat_condense ~ surg_approach + prev_rad + prev_surg  + age + path,data=data_file_stats,family="binomial")

fit.logit2 = glm(post_treat_condense ~ surg_approach_condense + prev_rad + prev_surg  + age + path_condense + epi_condense+lat+sup+post,data=data_file_stats,family="binomial")

#http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/
# Extract model results
fit.logit2.data <- augment(fit.logit2) %>% 
  mutate(index = 1:n()) 

ggplot(fit.logit2.data, aes(index, .std.resid)) + 
  geom_point(aes(color = post_treat_condense), alpha = .5) +
  theme_bw()

fit.logit2.data %>% 
  filter(abs(.std.resid) > 3)

car::vif(fit.logit2)


#fit.logit = glm(resect_condense ~ surg_approach ,data=data_file_stats,family="binomial")
confint(fit.logit1)
wald.test(b = coef(fit.logit1), Sigma = vcov(fit.logit1), Terms = 2:6)
exp(coef(fit.logit1))

confint(fit.logit2)
wald.test(b = coef(fit.logit2), Sigma = vcov(fit.logit2), Terms = 2:6)
exp(coef(fit.logit2))

fit.logit3 = glm(major_comp ~ surg_approach_condense + prev_rad + prev_surg  + age + path_condense + epi_condense+lat+sup+post,data=data_file_stats,family="binomial")
   
formula4 <- list(); model4 <- list(); p4nonadjust <- list()
formula4_subsets <- list(); formula4_totals <- list();totals4 <- list(); subsets4<-list();data_frame4<- list();resexact_4<-list()

for (i in 1:length(independent_vars)) {
  formula4[[i]] = paste0("minor_comp", " ~ ", independent_vars[[i]])
  model4[[i]] = glm(formula4[[i]],data=data_file_stats,family="binomial") 
  anova_temp <- Anova(model4[[i]],type="II", test="Wald");
  p4nonadjust[[i]] <- round(anova_temp[[3]],3)
  
  
  print(summary(model4[[i]]))
  print(anova_temp)
  #print(wald.test(b=coef(model1[[i]])),Sigma = vcov(model1[[i]]),Terms= )
  #wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
  
  formula4_subsets[[i]] = paste0("~minor_comp + ", independent_vars[[i]])
  subsets4[[i]] = xtabs(formula4_subsets[[i]],data=data_file_stats)
  totals4[[i]]  = subsets4[[i]][1,]+subsets4[[i]][2,]
  data_frame4[[i]] = data.frame(indepvar = levels(as.data.frame(subsets4[[i]])[,2]),depvar=subsets4[[i]][2,],n=totals4[[i]])
  if (i<length(independent_vars)){
    resexact_4[[i]] = elrm(depvar/n ~ as.factor(indepvar), interest = ~as.factor(indepvar), iter=5000000, 
                           burnIn=5000, data=data_frame4[[i]], r=2)
  }
  else {
    resexact_4[[i]] = elrm(depvar/n ~ as.integer(indepvar), interest = ~as.integer(indepvar), iter=5000000, 
                           burnIn=5000, data=data_frame4[[i]], r=2)
  }
  summary(resexact_4[[i]])
  
}
p4adjust <- p.adjust(p4nonadjust,"BH")
p4total <- cbind(p4nonadjust,p4adjust)

fit.logit4 = glm(minor_comp ~ surg_approach_condense + prev_rad + prev_surg  + age + path_condense + epi_condense+lat+sup+post,data=data_file_stats,family="binomial")

M1 <- logLik(fit.ordinal_cn_3)
M2 <- logLik(fit.multinom_cn_3)
(G <- -2*(M1[1] - M2[1]))
# degree of freedoM
pchisq(G,3,lower.tail = FALSE)

# using epi in additional to surg_approach says rank deficient

formula5 <- list(); model5 <- list(); p5nonadjust <- list()
formula5_subsets <- list(); formula5_totals <- list();totals5 <- list(); subsets5<-list();data_frame5<- list();resexact_5<-list()

for (i in 1:length(independent_vars)) {
  formula5[[i]] = paste0("po_1_cn_3", " ~ ", independent_vars[[i]])
  model5[[i]] = glm(formula5[[i]],data=data_file_stats,family="binomial") 
  anova_temp <- Anova(model5[[i]],type="II", test="Wald");
  p5nonadjust[[i]] <- round(anova_temp[[3]],3)
  
  
  print(summary(model5[[i]]))
  print(anova_temp)
  #print(wald.test(b=coef(model1[[i]])),Sigma = vcov(model1[[i]]),Terms= )
  #wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
  
  formula5_subsets[[i]] = paste0("~po_1_cn_3 + ", independent_vars[[i]])
  subsets5[[i]] = xtabs(formula5_subsets[[i]],data=data_file_stats)
  totals5[[i]]  = subsets5[[i]][1,]+subsets5[[i]][2,]
  data_frame5[[i]] = data.frame(indepvar = levels(as.data.frame(subsets5[[i]])[,2]),depvar=subsets5[[i]][2,],n=totals5[[i]])
  if (i<length(independent_vars)){
    resexact_5[[i]] = elrm(depvar/n ~ as.factor(indepvar), interest = ~as.factor(indepvar), iter=5000000, 
                           burnIn=5000, data=data_frame5[[i]], r=2)
  }
  else {
    resexact_5[[i]] = elrm(depvar/n ~ as.integer(indepvar), interest = ~as.integer(indepvar), iter=5000000, 
                           burnIn=5000, data=data_frame5[[i]], r=2)
  }
  summary(resexact_5[[i]])
  
}
p5adjust <- p.adjust(p5nonadjust,"BH")
p5total <- cbind(p5nonadjust,p5adjust)

formula6 <- list(); model6 <- list(); p6nonadjust <- list()
formula6_subsets <- list(); formula6_totals <- list();totals6 <- list(); subsets6<-list();data_frame6<- list();resexact_6<-list()

for (i in 1:length(independent_vars)) {
  formula6[[i]] = paste0("po_1_cn_4", " ~ ", independent_vars[[i]])
  model6[[i]] = glm(formula6[[i]],data=data_file_stats,family="binomial") 
  anova_temp <- Anova(model6[[i]],type="II", test="Wald");
  p6nonadjust[[i]] <- round(anova_temp[[3]],3)
  
  
  print(summary(model6[[i]]))
  print(anova_temp)
  #print(wald.test(b=coef(model1[[i]])),Sigma = vcov(model1[[i]]),Terms= )
  #wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
  
  formula6_subsets[[i]] = paste0("~po_1_cn_4 + ", independent_vars[[i]])
  subsets6[[i]] = xtabs(formula6_subsets[[i]],data=data_file_stats)
  totals6[[i]]  = subsets6[[i]][1,]+subsets6[[i]][2,]
  data_frame6[[i]] = data.frame(indepvar = levels(as.data.frame(subsets6[[i]])[,2]),depvar=subsets6[[i]][2,],n=totals6[[i]])
  if (i<length(independent_vars)){
    resexact_6[[i]] = elrm(depvar/n ~ as.factor(indepvar), interest = ~as.factor(indepvar), iter=5000000, 
                           burnIn=5000, data=data_frame6[[i]], r=2)
  }
  else {
    resexact_6[[i]] = elrm(depvar/n ~ as.integer(indepvar), interest = ~as.integer(indepvar), iter=5000000, 
                           burnIn=5000, data=data_frame6[[i]], r=2)
  }
  summary(resexact_6[[i]])
  
}
p6adjust <- p.adjust(p6nonadjust,"BH")
p6total <- cbind(p6nonadjust,p6adjust)

formula7 <- list(); model7 <- list(); p7nonadjust <- list()
formula7_subsets <- list(); formula7_totals <- list();totals7 <- list(); subsets7<-list();data_frame7<- list();resexact_7<-list()

for (i in 1:length(independent_vars)) {
  formula7[[i]] = paste0("po_1_cn_5", " ~ ", independent_vars[[i]])
  model7[[i]] = glm(formula7[[i]],data=data_file_stats,family="binomial") 
  anova_temp <- Anova(model7[[i]],type="II", test="Wald");
  p7nonadjust[[i]] <- round(anova_temp[[3]],3)
  
  
  print(summary(model7[[i]]))
  print(anova_temp)
  #print(wald.test(b=coef(model1[[i]])),Sigma = vcov(model1[[i]]),Terms= )
  #wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
  
  formula7_subsets[[i]] = paste0("~po_1_cn_5 + ", independent_vars[[i]])
  subsets7[[i]] = xtabs(formula7_subsets[[i]],data=data_file_stats)
  totals7[[i]]  = subsets7[[i]][1,]+subsets7[[i]][2,]
  data_frame7[[i]] = data.frame(indepvar = levels(as.data.frame(subsets7[[i]])[,2]),depvar=subsets7[[i]][2,],n=totals7[[i]])
  if (i<length(independent_vars)){
    resexact_7[[i]] = elrm(depvar/n ~ as.factor(indepvar), interest = ~as.factor(indepvar), iter=5000000, 
                           burnIn=5000, data=data_frame7[[i]], r=2)
  }
  else {
    resexact_7[[i]] = elrm(depvar/n ~ as.integer(indepvar), interest = ~as.integer(indepvar), iter=5000000, 
                           burnIn=5000, data=data_frame7[[i]], r=2)
  }
  summary(resexact_7[[i]])
  
}
p7adjust <- p.adjust(p7nonadjust,"BH")
p7total <- cbind(p7nonadjust,p7adjust)

formula8 <- list(); model8 <- list(); p8nonadjust <- list()
formula8_subsets <- list(); formula8_totals <- list();totals8 <- list(); subsets8<-list();data_frame8<- list();resexact_8<-list()

for (i in 1:length(independent_vars)) {
  formula8[[i]] = paste0("po_1_cn_6", " ~ ", independent_vars[[i]])
  model8[[i]] = glm(formula8[[i]],data=data_file_stats,family="binomial") 
  anova_temp <- Anova(model8[[i]],type="II", test="Wald");
  p8nonadjust[[i]] <- round(anova_temp[[3]],3)
  
  
  print(summary(model8[[i]]))
  print(anova_temp)
  #print(wald.test(b=coef(model1[[i]])),Sigma = vcov(model1[[i]]),Terms= )
  #wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
  
  formula8_subsets[[i]] = paste0("~po_1_cn_6 + ", independent_vars[[i]])
  subsets8[[i]] = xtabs(formula8_subsets[[i]],data=data_file_stats)
  totals8[[i]]  = subsets8[[i]][1,]+subsets8[[i]][2,]
  data_frame8[[i]] = data.frame(indepvar = levels(as.data.frame(subsets8[[i]])[,2]),depvar=subsets8[[i]][2,],n=totals8[[i]])
  if (i<length(independent_vars)){
    resexact_8[[i]] = elrm(depvar/n ~ as.factor(indepvar), interest = ~as.factor(indepvar), iter=5000000, 
                           burnIn=5000, data=data_frame8[[i]], r=2)
  }
  else {
    resexact_8[[i]] = elrm(depvar/n ~ as.integer(indepvar), interest = ~as.integer(indepvar), iter=5000000, 
                           burnIn=5000, data=data_frame8[[i]], r=2)
  }
  summary(resexact_8[[i]])
  
}
p8adjust <- p.adjust(p8nonadjust,"BH")
p8total <- cbind(p8nonadjust,p8adjust)

interest_var = resexact_1
for (i in 1:(length(independent_vars))) {
  print(independent_vars[[i]])
  print((round(exp(interest_var[[i]]$coeffs),3)))
  print((round(exp(interest_var[[i]]$coeffs.ci),3)))
  print((round(interest_var[[i]]$p.values,3)))
  
}

# 
# 
# formula9 <- list(); model9 <- list(); p9nonadjust <- list()
# for (i in 1:length(independent_vars)) {
#   formula9[[i]] = paste0("resect_condense", " ~ ", independent_vars[[i]])
#   model9[[i]] = glm(formula9[[i]],data=data_file_stats,family="binomial") 
#   anova_temp <- Anova(model9[[i]],type="II", test="Wald");
#   p9nonadjust[[i]] <- round(anova_temp[[3]],3)
#   
#   
#   print(summary(model9[[i]]))
#   print(anova_temp)
#   #print(wald.test(b=coef(model1[[i]])),Sigma = vcov(model1[[i]]),Terms= )
#   #wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
#   
# }
# p9adjust <- p.adjust(p9nonadjust,"BH")
# p9total <- cbind(p9nonadjust,p9adjust)
# 
# formula10 <- list(); model10 <- list(); p10nonadjust <- list()
# for (i in 1:length(independent_vars)) {
#   formula10[[i]] = paste0("resect_condense", " ~ ", independent_vars[[i]])
#   model10[[i]] = glm(formula10[[i]],data=data_file_stats,family="binomial") 
#   anova_temp <- Anova(model10[[i]],type="II", test="Wald");
#   p10nonadjust[[i]] <- round(anova_temp[[3]],3)
#   
#   
#   print(summary(model10[[i]]))
#   print(anova_temp)
#   #print(wald.test(b=coef(model1[[i]])),Sigma = vcov(model1[[i]]),Terms= )
#   #wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
#   
# }
# p10adjust <- p.adjust(p10nonadjust,"BH")
# p10total <- cbind(p10nonadjust,p10adjust)
# 
# formula11 <- list(); model11 <- list(); p11nonadjust <- list()
# for (i in 1:length(independent_vars)) {
#   formula11[[i]] = paste0("resect_condense", " ~ ", independent_vars[[i]])
#   model11[[i]] = glm(formula11[[i]],data=data_file_stats,family="binomial") 
#   anova_temp <- Anova(model11[[i]],type="II", test="Wald");
#   p11nonadjust[[i]] <- round(anova_temp[[3]],3)
#   
#   
#   print(summary(model11[[i]]))
#   print(anova_temp)
#   #print(wald.test(b=coef(model1[[i]])),Sigma = vcov(model1[[i]]),Terms= )
#   #wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
#   
# }
# p11adjust <- p.adjust(p11nonadjust,"BH")
# p11total <- cbind(p11nonadjust,p11adjust)


if (doOrdinal){
  
  
  fit.ordinal_cn_3 = polr(imm_cn_3~surg_approach + prev_rad + prev_surg + epi + age + lat + med + sup + post + ant + path,data=data_file_stats)
  
  fit.ordinal_cn_3 = polr(po_1_cn_3~surg_approach,data=data_file_stats)
  fit.multinom_cn_3 = multinom(imm_cn_3~surg_approach,data=data_file_stats)
  brant(fit.ordinal_cn_3)
  
  
#fit.ordinal_cn_3_imm = polr(imm_cn_3~surg_approach_condense+age+prev_rad+prev_surg+path_condense+epi_condense,data=data_file_stats)
fit.ordinal_cn_3_imm = polr(imm_cn_3~surg_approach_condense+age+prev_rad+prev_surg+lat+sup+post,data=data_file_stats)

#fit.ordinal_cn_3 = polr(po_1_cn_3~surg_approach_condense+age+prev_rad+prev_surg+path_condense+epi_condense,data=data_file_stats)

fit.ordinal_cn_3 = polr(po_1_cn_3~surg_approach_condense+age+prev_rad+prev_surg+lat+sup+post,data=data_file_stats)


fit.ordinal_cn_4_imm = polr(imm_cn_4~surg_approach_condense+age+prev_rad+prev_surg+path_condense+epi_condense+lat+sup+post,data=data_file_stats)
fit.ordinal_cn_4 = polr(po_1_cn_4~surg_approach_condense+age+prev_rad+prev_surg+path_condense+epi_condense+lat+sup+post,data=data_file_stats)

fit.ordinal_cn_5_imm = polr(imm_cn_5~surg_approach_condense+age+prev_rad+prev_surg+path_condense+epi_condense+lat+sup+post,data=data_file_stats)
fit.ordinal_cn_5 = polr(po_1_cn_5~surg_approach_condense+age+prev_rad+prev_surg+path_condense+epi_condense+lat+sup+post,data=data_file_stats)

fit.ordinal_cn_6_imm = polr(imm_cn_6~surg_approach_condense+age+prev_rad+prev_surg+path_condense+epi_condense+lat+sup+post,data=data_file_stats)
fit.ordinal_cn_6 = polr(po_1_cn_6~surg_approach_condense+age+prev_rad+prev_surg+path_condense+epi_condense+lat+sup+post,data=data_file_stats)
} else{
 # fit.log_cn_3_imm = glm(imm_cn_3~surg_approach_condense+epi_condense+path_condense + age+prev_rad+prev_surg+lat+sup+post,data=data_file_stats,family="binomial")
  fit.log_cn_3_imm = glm(imm_cn_3~surg_approach_condense + age+prev_rad+prev_surg+lat+sup+post,data=data_file_stats,family="binomial")
  
  #fit.ordinal_cn_3 = glm(po_1_cn_3~surg_approach_condense+age+prev_rad+prev_surg+path_condense+epi_condense,data=data_file_stats)
  
#  fit.log_cn_3 = glm(po_1_cn_3~surg_approach_condense+epi_condense+path_condense + +age+prev_rad+prev_surg+lat+sup+post,data=data_file_stats,family="binomial")
  fit.log_cn_3_imm = glm(imm_cn_3~surg_approach_condense + age+prev_rad+prev_surg+lat+sup+post,data=data_file_stats,family="binomial")
  
  
  fit.log_cn_4_imm = glm(imm_cn_4~surg_approach_condense+age+prev_rad+prev_surg+lat+sup+post,data=data_file_stats,family="binomial")
  fit.log_cn_4 = glm(po_1_cn_4~ age+prev_rad+prev_surg+lat+sup+post,data=data_file_stats,family="binomial")
  
  fit.log_cn_5_imm = glm(imm_cn_5~surg_approach_condense+age+prev_rad+prev_surg+path_condense+epi_condense+lat+sup+post,data=data_file_stats,family="binomial")
  fit.log_cn_5 = glm(po_1_cn_5~surg_approach_condense+age+prev_rad+prev_surg+path_condense+epi_condense+lat+sup+post,data=data_file_stats,family="binomial")
  
  fit.log_cn_6_imm = glm(imm_cn_6~surg_approach_condense+age+prev_rad+prev_surg+path_condense+epi_condense+lat+sup+post,data=data_file_stats,family="binomial")
  fit.log_cn_6 = glm(po_1_cn_6~age+prev_rad+prev_surg+lat+sup+post,data=data_file_stats,family="binomial")
  
  
}
  if (doMixed){
  data_file_stats_long3<-data_file_stats%>%pivot_longer(cols = c("imm_cn_3","po_6_3_months_cn_3","po_1_cn_3"),names_to="names_time_cn3",values_to="cn_3")
  data_file_stats_long4<-data_file_stats%>%pivot_longer(cols = c("imm_cn_4","po_6_3_months_cn_4","po_1_cn_4"),names_to="names_time_cn4",values_to="cn_4")
  data_file_stats_long5<-data_file_stats%>%pivot_longer(cols = c("imm_cn_5","po_6_3_months_cn_5","po_1_cn_5"),names_to="names_time_cn5",values_to="cn_5")
  data_file_stats_long6<-data_file_stats%>%pivot_longer(cols = c("imm_cn_6","po_6_3_months_cn_6","po_1_cn_6"),names_to="names_time_cn6",values_to="cn_6")
  
  data_file_stats_long3 <- data_file_stats_long3 %>% mutate(time_point = as.factor(case_when(grepl("imm", names_time_cn3, ignore.case = TRUE)~'time1',
                                                                              grepl("po_6_3_months", names_time_cn3, ignore.case = TRUE)~'time2',
                                                                              grepl("po_1", names_time_cn3, ignore.case = TRUE)~'time3'
                                                                              )))
  
  data_file_stats_long4 <- data_file_stats_long4 %>% mutate(time_point = as.factor(case_when(grepl("imm", names_time_cn4, ignore.case = TRUE)~'time1',
                                                                                 grepl("po_6_3_months", names_time_cn4, ignore.case = TRUE)~'time2',
                                                                                 grepl("po_1", names_time_cn4, ignore.case = TRUE)~'time3'
  )))
  
  data_file_stats_long5 <- data_file_stats_long5 %>% mutate(time_point = as.factor(case_when(grepl("imm", names_time_cn5, ignore.case = TRUE)~'time1',
                                                                                 grepl("po_6_3_months", names_time_cn5, ignore.case = TRUE)~'time2',
                                                                                 grepl("po_1", names_time_cn5, ignore.case = TRUE)~'time3'
  )))
  
  data_file_stats_long6 <- data_file_stats_long6 %>% mutate(time_point = as.factor(case_when(grepl("imm", names_time_cn6, ignore.case = TRUE)~'time1',
                                                                                 grepl("po_6_3_months", names_time_cn6, ignore.case = TRUE)~'time2',
                                                                                 grepl("po_1", names_time_cn6, ignore.case = TRUE)~'time3'
  )))
  
  fit.cn_time_3_ord = clmm(cn_3 ~ time_point + (1|id), data=data_file_stats_long3)
  fit.cn_time_4_ord = clmm(cn_4 ~ time_point + (1|id), data=data_file_stats_long4)
  fit.cn_time_5_ord = clmm(cn_5 ~ time_point + (1|id), data=data_file_stats_long5)
  fit.cn_time_6_ord = clmm(cn_6 ~ time_point + (1|id), data=data_file_stats_long6)
  
  emm_model_3_ord = emmeans(fit.cn_time_3_ord, "time_point")
  pairs(emm_model_3_ord, reverse = TRUE)
  
  emm_model_4_ord = emmeans(fit.cn_time_4_ord, "time_point")
  pairs(emm_model_4_ord, reverse = TRUE)
  
  emm_model_5_ord = emmeans(fit.cn_time_5_ord, "time_point")
  pairs(emm_model_5_ord, reverse = TRUE)
  
  emm_model_6_ord = emmeans(fit.cn_time_6_ord, "time_point")
  pairs(emm_model_6_ord, reverse = TRUE)
  
  # set contrast options for unordered and ordered variables 
  #options(contrasts = rep ("contr.treatment", 2)) - this was previously selected 
  
  fit.cn_time_3 = glmmTMB(cn_3 ~ time_point + (1|id), data=data_file_stats_long3,family=binomial)
  fit.cn_time_4 = glmmTMB(cn_4 ~ time_point + (1|id), data=data_file_stats_long4,family=binomial)
  fit.cn_time_5 = glmmTMB(cn_5 ~ time_point + (1|id), data=data_file_stats_long5,family=binomial)
  fit.cn_time_6 = glmmTMB(cn_6 ~ time_point + (1|id), data=data_file_stats_long6,family=binomial)
  
  fit.cn_time_3 = glmmTMB(cn_3 ~ time_point + (1|id) + ar1(time_point + 0|id), data=data_file_stats_long3,family=binomial)
#  fit.cn_time_4 = glmmTMB(cn_4 ~ time_point + (1|id) + ar1(time_point + 0|id), data=data_file_stats_long4,family=binomial)
 # fit.cn_time_5 = glmmTMB(cn_5 ~ time_point + (1|id) + ar1(time_point + 0|id), data=data_file_stats_long5,family=binomial)
#  fit.cn_time_6 = glmmTMB(cn_6 ~ time_point + (1|id) + ar1(time_point + 0|id), data=data_file_stats_long6,family=binomial)
  
  fit.cn_time_3 = glmer(cn_3 ~ time_point + (time_point|id), data=data_file_stats_long3,family=binomial)
  emm_model_3 = emmeans(fit.cn_time_3, "time_point")
  pairs(emm_model_3, reverse = TRUE)
  
  
  simulationOutput <- simulateResiduals(fittedModel = fit.cn_time_3)
  plot(simulationOutput, asFactor = T)
 # plotResiduals(simulationOutput,data_file_stats_long3$time_point, quantreg = T)
  
  

  options(contrasts = c("contr.sum","contr.poly"))
  contr.sum(3)
  
  p <- ggplot(data = subset(data_file_stats_long3,!is.na(cn_3)), aes(x = time_point, y = cn_3, group = id,col=lat))
  p+geom_jitter(height=0.2,width=0.2)
  
  fit.cn_3_mixed = glmer(cn_3 ~ surg_approach_condense+prev_rad+prev_surg+lat+sup+post + (1|id), data=data_file_stats_long3,family=binomial,nAGQ=10)
  fit.cn_3_mixed = glmer(cn_3 ~ time_point*lat + age+prev_rad+prev_surg+(1|id), data=data_file_stats_long3,family=binomial,nAGQ=10)
  
  fit.cn_3_mixedlat = glmmPQL(cn_3 ~ time_point*lat,random = list(id = ~1),correlation = corAR1(), data=data_file_stats_long3,family=binomial)
  fit.cn_3_mixedlat = glmmTMB(cn_3 ~ time_point*lat + (1|id) + ar1(time_point + 0|id), data=data_file_stats_long3,family=binomial)
  pairs(emmeans(fit.cn_3_mixedlat, "time_point", by = "lat"))
  #dispersiontest(fit.cn_3_mixedlat)
  
  
  fit.cn_3_mixedlat = glmer(cn_3 ~ time_point*lat  + (1|id), data=data_file_stats_long3,family=binomial,nAGQ=10)
  
  fit.cn_3_lat = glmer(cn_3 ~ time_point + lat  + (1|id), data=data_file_stats_long3,family=binomial,nAGQ=10)
  anova(fit.cn_3_lat,fit.cn_3_mixedlat)
  Anova(fit.cn_3_mixedlat,type="III")
  
  fit.cn_3_mixedsup = glmer(cn_3 ~ time_point*sup  + (1|id), data=data_file_stats_long3,family=binomial,nAGQ=10)
  fit.cn_3_sup = glmer(cn_3 ~ time_point + sup  + (1|id), data=data_file_stats_long3,family=binomial,nAGQ=10)
  anova(fit.cn_3_sup,fit.cn_3_mixedsup)
  Anova(fit.cn_3_mixedsup,type="III")
  
  
  fit.cn_3_mixedpost = glmer(cn_3 ~ time_point*post  + (1|id), data=data_file_stats_long3,family=binomial,nAGQ=10)
  fit.cn_3_post = glmer(cn_3 ~ time_point + post  + (1|id), data=data_file_stats_long3,family=binomial,nAGQ=10)
  anova(fit.cn_3_post,fit.cn_3_mixedpost)
  Anova(fit.cn_3_mixedpost,type="III")
  
  
  fit.cn_4_mixedlat = glmer(cn_4 ~ time_point*lat  + (1|id), data=data_file_stats_long4,family=binomial,nAGQ=10)
  fit.cn_4_lat = glmer(cn_4 ~ time_point + lat  + (1|id), data=data_file_stats_long4,family=binomial,nAGQ=10)
  anova(fit.cn_4_lat,fit.cn_4_mixedlat)
  Anova(fit.cn_4_mixedlat,type="III")
  
  
  fit.cn_4_mixedsup = glmer(cn_4 ~ time_point*sup  + (1|id), data=data_file_stats_long4,family=binomial,nAGQ=10)
  fit.cn_4_sup = glmer(cn_4 ~ time_point + sup  + (1|id), data=data_file_stats_long4,family=binomial,nAGQ=10)
  anova(fit.cn_4_lat,fit.cn_4_mixedsup)
  Anova(fit.cn_4_mixedsup,type="III")
  
  
  fit.cn_4_mixedpost = glmer(cn_4 ~ time_point*post  + (1|id), data=data_file_stats_long4,family=binomial,nAGQ=10)
  fit.cn_4_post = glmer(cn_4 ~ time_point + post  + (1|id), data=data_file_stats_long4,family=binomial,nAGQ=10)
  anova(fit.cn_4_post,fit.cn_4_mixedpost)
  Anova(fit.cn_4_mixedpost,type="III")
  
  p <- ggplot(data = subset(data_file_stats_long5,!is.na(cn_5)), aes(x = time_point, y = cn_5, group = id,col=lat))
  p+geom_jitter(height=0.2,width=0.2)
  
  
  fit.cn_5_mixedlat_PQL = glmmPQL(cn_5 ~ time_point*lat,random = list(id = ~1),correlation = corAR1(), data=data_file_stats_long5,family=binomial)
  fit.cn_5_mixedlat_PQL = glmmPQL(cn_5 ~ time_point*lat,random = list(id = ~1), data=data_file_stats_long5,family=binomial)
  
  
  fit.cn_5_mixedlat_TMB = glmmTMB(cn_5 ~ time_point*lat  + (1|id), data=data_file_stats_long5,family=binomial)
  fit.cn_5_mixedlat_TMB = glmmTMB(cn_5 ~ time_point*lat  + (1|id) + ar1(time_point + 0|id), data=data_file_stats_long5,family=binomial)
  
  fit.cn_5_mixedlat = glmer(cn_5 ~ time_point*lat  + (1|id), data=data_file_stats_long5,family=binomial,nAGQ=10)
  fit.cn_5_lat = glmer(cn_5 ~ time_point + lat  + (1|id), data=data_file_stats_long5,family=binomial,nAGQ=10)
  anova(fit.cn_5_lat,fit.cn_5_mixedlat)
  Anova(fit.cn_5_mixedlat,type="III")
  
  fit.cn_5_mixedsup_PQL = glmmPQL(cn_5 ~ time_point*sup,random = list(id = ~1),correlation = corAR1(), data=data_file_stats_long5,family=binomial)
  fit.cn_5_mixedsup_TMB = glmmTMB(cn_5 ~ time_point*sup  + (1|id), data=data_file_stats_long5,family=binomial)
  
  fit.cn_5_mixedsup = glmer(cn_5 ~ time_point*sup  + (1|id), data=data_file_stats_long5,family=binomial,nAGQ=10)
  fit.cn_5_sup = glmer(cn_5 ~ time_point + sup  + (1|id), data=data_file_stats_long5,family=binomial,nAGQ=10)
  anova(fit.cn_5_sup,fit.cn_5_mixedsup)
  Anova(fit.cn_5_mixedsup,type="III")
  
  fit.cn_5_mixedpost = glmmPQL(cn_5 ~ time_point*sup,random = list(id = ~1),correlation = corAR1(), data=data_file_stats_long5,family=binomial)
  
  
  fit.cn_5_mixedpost = glmer(cn_5 ~ time_point*post  + (1|id), data=data_file_stats_long5,family=binomial,nAGQ=10)
  fit.cn_5_post = glmer(cn_5 ~ time_point + post  + (1|id), data=data_file_stats_long5,family=binomial,nAGQ=10)
  
  fit.cn_5_mixedpost = glmmTMB(cn_5 ~ time_point*post  + (1|id), data=data_file_stats_long5,family=binomial)
  fit.cn_5_post = glmmTMB(cn_5 ~ time_point + post  + (1|id), data=data_file_stats_long5,family=binomial)
  
  
  anova(fit.cn_5_post,fit.cn_5_mixedpost)
  Anova(fit.cn_5_mixedpost,type="III")
  
  
  fit.cn_6_mixedlat = glmer(cn_6 ~ time_point*lat  + (1|id), data=data_file_stats_long6,family=binomial,nAGQ=10)
  fit.cn_6_lat = glmer(cn_6 ~ time_point + lat  + (1|id), data=data_file_stats_long6,family=binomial,nAGQ=10)
  anova(fit.cn_6_lat,fit.cn_6_mixedlat)
  Anova(fit.cn_6_mixedlat,type="III")
  
  
  fit.cn_6_mixedsup = glmer(cn_6 ~ time_point*sup  + (1|id), data=data_file_stats_long6,family=binomial,nAGQ=10)
  fit.cn_6_sup = glmer(cn_6 ~ time_point + sup  + (1|id), data=data_file_stats_long6,family=binomial,nAGQ=10)
  anova(fit.cn_6_sup,fit.cn_6_mixedsup)
  Anova(fit.cn_6_mixedsup,type="III")
  
  
  fit.cn_6_mixedpost = glmer(cn_6 ~ time_point*post  + (1|id), data=data_file_stats_long6,family=binomial,nAGQ=10)
  fit.cn_6_post = glmer(cn_6 ~ time_point + lat  + (1|id), data=data_file_stats_long6,family=binomial,nAGQ=10)
  anova(fit.cn_6_post,fit.cn_6_mixedpost)
  Anova(fit.cn_6_mixedpost,type="III")
  
  
  plot_model(fit.cn_3_mixedlat)
  
  
}

if (saveFig){
  save.image(file = paste0(rootDir,"/cav_sinus_data.RData"))
}