#########################################
# load libraries
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

setwd("~/SharedCode/cavernous_sinus_code")
rootDir = here()
dataDir = 'C:/Users/david/OneDrive - UW/Cavernous sinus project'
saveFig = TRUE
fontSize = 10

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8.5, height = 11),
  type = "continuous",
  page_margins = page_mar()
)

### read data
data_file <- read_excel(file.path(dataDir,"Cavernous sinus cases, data collection sheet.xlsx"))

column_names = colnames(data_file)
# print column names to console for easy copy and paste for selection to build code 
cat(column_names,sep="', '")


### clean up data
data_file <- mutate_all(data_file,.funs=str_to_lower)

data_file<-data_file %>% mutate(  across(everything(), ~replace_na(.x, "Missing")))

data_file <- data_file %>% mutate(`Age at the time of surgery`=as.numeric(`Age at the time of surgery`),
                                  `Tumor size (T, cm)` =as.numeric(as.character(`Tumor size (T, cm)`)),
                                  `Tumor size (AP, cm)` =as.numeric(as.character(`Tumor size (AP, cm)`)),
                                  `Tumor size (RC, cm)` =as.numeric(as.character(`Tumor size (RC, cm)`)),
                                  `Tumor volume (ABC/2, cm3)` = as.numeric(as.character(`Tumor volume (ABC/2, cm3)`))
                                  )

### add labels
# var_label(data_file$...) <- ""

### make missing values explicit so that percentages are correct 



### summarize tumor characteristics 
select_string_tumor = c('Pathology','Tumor grade (WHO)','Tumor only in cavernous sinus (Y=1, 2 = N)','Tumor epicenter in cavernous sinus (Y=1, 2 = N)',
                  'Tumor epicenter in middle fossa (Y=1, 2 = N)','Tumor epicenter in posterior fossa (Y=1, 2 = N)','Tumor epicenter in the infratemporal fossa (Y=1, 2 = N)',
                  'Tumor epicenter in the sella (Y=1, 2 = N)','Tumor epicenter in the sphenoid sinus (Y=1, 2 = N)',
                  'Tumor epicenter in the orbital apex/SOF (Y=1, 2 = N)' ,'Tumor extension in the middle fossa (Y=1, 2 = N)',
                  'Tumor extension in the posterior fossa (Y=1, 2 = N)', 'Tumor extension in the infratemporal fossa (Y=1, 2 = N)',
                  'Tumor extend in the sella (Y=1, 2 = N)','Tumor extension in the sphenoid sinus (Y=1, 2 = N)','Tumor extension in the orbital apex/SOF')

data_summary_tumor <- data_file %>% select(all_of(select_string_tumor)) %>% tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                            all_categorical() ~ "{n} / {N} ({p}%)"),
                                           digits = all_continuous() ~ 2,
                                           missing = "no") 
                                           #missing_text = "(Missing)") 
#%>% add_p(pvalue_fun = ~style_pvalue(.x, digits = 2))

if (saveFig){
data_summary_tumor %>%
as_gt() %>%             # convert to gt table
  gt::gtsave(             # save table as image
    filename = "tumor_characteristics.png"
  )

data_summary_tumor_flex <- data_summary_tumor %>% as_flex_table() 

save_as_docx(data_summary_tumor_flex,path="tumor_characteristics.docx")

data_summary_tumor_flex <- fontsize(data_summary_tumor_flex,size=fontSize)
save_as_docx(data_summary_tumor_flex,path="tumor_characteristics.docx",pr_section = sect_properties)

}
### summarize tumor dimensions

select_string_size = c('Tumor size (AP, cm)','Tumor size (T, cm)','Tumor size (RC, cm)','Tumor volume (ABC/2, cm3)' )

data_summary_size <- data_file %>% select(all_of(select_string_size)) %>% tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                                                                         all_categorical() ~ "{n} / {N} ({p}%)"),
                                                                                        digits = all_continuous() ~ 2,
                                                                                        missing = "no") 
                                                                                        #missing_text = "(Missing)") 
#%>% add_p(pvalue_fun = ~style_pvalue(.x, digits = 2))
#%>% add_p(pvalue_fun = ~style_pvalue(.x, digits = 2))
if (saveFig){
data_summary_size %>%
  as_gt() %>%             # convert to gt table
  gt::gtsave(             # save table as image
    filename = "tumor_size.png"
  )
  
  
  data_summary_size_flex <- data_summary_size %>% as_flex_table() 
  
  data_summary_size_flex <- fontsize(data_summary_size_flex,size=fontSize)
  save_as_docx(data_summary_size_flex,path="tumor_size.docx",pr_section = sect_properties)
  
}
### summarize past and future treatment 

select_string_prev = c('Previous surgery (Y=1, 2 = N)','What was previous surgery','Previous radiation therapy ((Y=1, 2 = N)',
                       'What was the previous radiation (None=1, Repeat surgery = 2, SRS =3 FSRT = 4 Proton therapy =5 Chemo/immunotherapy = 6 Other = 7',
                       'Post-operative aditionnal treatment (None=1, Repeat surgery = 2, SRS =3 FSRT = 4 Proton therapy =5 Chemo/immunotherapy = 6 Other = 7', 'Reason for aditionnal treatment (1 = tumor recurrence 2 = tumor progression 3 = Tumor grade/histology 4= Tumor residual 5= other', 'Tumor recurrence at follow up (Y=1 N=2)', 'Tumor progression during follow up (Y=1 N= 2)')
data_summary_prev <- data_file %>% select(all_of(select_string_prev)) %>% tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                                                                       all_categorical() ~ "{n} / {N} ({p}%)"),
                                                                                      digits = all_continuous() ~ 2,
                                                                                      missing = "no") 
                                                                                      #missing_text = "(Missing)") 
#%>% add_p(pvalue_fun = ~style_pvalue(.x, digits = 2))
if (saveFig){
data_summary_prev %>%
  as_gt() %>%             # convert to gt table
  gt::gtsave(             # save table as image
    filename = "past_future_treatment.png"
  )

  
  data_summary_prev_flex <- data_summary_prev %>% as_flex_table() 
  
  data_summary_prev_flex <- fontsize(data_summary_prev_flex,size=6)
  save_as_docx(data_summary_prev_flex,path="past_future_treatment.docx",pr_section = sect_properties)
  
}
### summarize operative approach, EBL, and anesthesia time

select_string_surg = c('Surgical approach (Fronto-temporal approach =1, Subtemporal approach = 2 Posterior petrosal = 3  transnasal/transmaxillary = 4 Other = 5', 'Skull base osteotomy (1 = None 2 = Posterolateral orbitotomy 3 = Full orbitotomy 4 = Zygomatic osteotomy 5 = Orbito-zygomatic osteotomy 6 = retrolabyrinthine 7 = tranlabyrinthine 8: petrous apex', 'Optic canal decompression and anterior clinoidectomy Y =1, 2=N)', 'Wall of the cavernous sinus approached ( 1= Lateral wall, 2 = Superior wall 3 = Posterior wall 4 = Anterior wall 5 = Medial wall', 'Cervical ICA exposure (Y=1, N=2)', 'Bypass (y=1, N= 2)', 'Pre-op Angiogram (Y=1, N=2)', 'Pre-op Embo (Y=1, N=2)', 'Length of Surgery (Anesthesia time in Min)', 'EBL (mL)', 'Tumor resection based on PO MRI (1 = GTR, 2 NTR = No residual on MRI, but tumor known to be left, 3 = Subtotal resection (residual < 10 %) 4 = Partial resection (Residual  > 10 %) 5 = Biopsy')
data_summary_surg <- data_file %>% select(all_of(select_string_prev)) %>% tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                                                                       all_categorical() ~ "{n} / {N} ({p}%)"),
                                                                                      digits = all_continuous() ~ 2,
                                                                                      missing = "no") 
                                                                                      #missing_text = "(Missing)") 
#%>% add_p(pvalue_fun = ~style_pvalue(.x, digits = 2))
if (saveFig){
data_summary_surg %>%
  as_gt() %>%             # convert to gt table
  gt::gtsave(             # save table as image
    filename = "surgery_info.png"
  )
  
  
  data_summary_surg_flex <- data_summary_surg %>% as_flex_table() 
  

  data_summary_surg_flex <- fontsize(data_summary_surg_flex,size=6)
  save_as_docx(data_summary_surg_flex,path="surgery_info.docx",pr_section = sect_properties)
  
}

### hospitalization characteristics 
select_string_hosp = c('UTI (Y=1, N=2)', 'Pneumonia (y=1 N=2)', 'DVT (Y=1 N=2)', 'PE (Y=1, N=2)', 'MI (Y=1 N=2)', 'Sinus thrombosis (Y=1, N=2)', 'CSF Leak (Y=1 N=2)', 'Symptomatic PO hematoma (Y=1 N=2)', 'Need for OR for hematoma evacuation (Y=1 N=2)', 'Need for OR for wound revision (y=1, N=2)', 'Need for permanent CSF diversion (Y=1, N=2)', 'Stroke (y=1, N=2)', 'Other morbidity( 2=NONE)', 'Death (Y=1, N=2)', 'Length of ICU Stay (Days)', 'Overall length of stay (days)', 'Dispo (Home:1, SNF/Rehab: 2)', '30 days Readmission  (Y=1, N= 2)', 'Reason for readmission')
data_summary_hosp <- data_file %>% select(all_of(select_string_hosp)) %>% tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                                                                   all_categorical() ~ "{n} / {N} ({p}%)"),
                                                                                  digits = all_continuous() ~ 2,
                                                                                  missing = "no") 
                                                                                  #missing_text = "(Missing)") 
if (saveFig){
data_summary_hosp %>%
  as_gt() %>%             # convert to gt table
  gt::gtsave(             # save table as image
    filename = "hosp_summary.png"
  )
  
  
  data_summary_hosp_flex <- data_summary_hosp %>% as_flex_table() 
  
  data_summary_hosp_flex <- fontsize(data_summary_hosp_flex,size=fontSize)
  save_as_docx(data_summary_hosp_flex,path="hosp_summary.docx",pr_section = sect_properties)

  
}

### cranial nerve long term 
select_string_CN = c('Pre-op CN 3 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 3 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 3 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Post-op 6 weeks-3 months  CN 3 Function (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 6 weeks-3 months CN 3 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'PO 1 year (or last follow up) CN 3 function  (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 1 year (or last follow up) CN 3 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Pre-op CN 4 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 4 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 4 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Post-op 6 weeks-3 months  CN 4 Function (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 6 weeks-3 months CN 4 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'PO 1 year (or last follow up) CN 4 function  (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 1 year (or last follow up) CN 4 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Pre-op CN 5 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 5 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 5 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Post-op 6 weeks-3 months  CN 5 Function (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 6 weeks-3 months CN 5 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'PO 1 year (or last follow up) CN 5 function  (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 1 year (or last follow up) CN 5 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Pre-op CN 6 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 6 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 6 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Post-op 6 weeks-3 months  CN 6 Function (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 6 weeks-3 months CN 6 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'PO 1 year (or last follow up) CN 6 function  (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 1 year (or last follow up) CN 6 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)')

data_summary_CN <- data_file %>% select(all_of(select_string_CN)) %>% tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                                                                       all_categorical() ~ "{n} / {N} ({p}%)"),
                                                                                      digits = all_continuous() ~ 2,
                                                                                      missing = "no") 
                                                                                      #missing_text = "(Missing)") 
if (saveFig){
data_summary_CN %>%
  as_gt() %>%             # convert to gt table
  gt::gtsave(             # save table as image
    filename = "cranial_nerve_summary.png"
  )
  
  
  data_summary_CN_flex <- data_summary_CN %>% as_flex_table() 
  data_summary_CN_flex <- fontsize(data_summary_CN_flex,size=fontSize)
  save_as_docx(data_summary_CN_flex,path="cranial_nerve_summary.docx",pr_section = sect_properties)
}
### do demographics statistics
select_string_demo <- c('Sex (M=1, F=2)','Age at the time of surgery','Race (Cauc = 1, Black = 2, Other =3)')
data_summary_demo <- data_file %>% select(all_of(select_string_demo)) %>% tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                                                     all_categorical() ~ "{n} / {N} ({p}%)"),
                                                                    digits = all_continuous() ~ 2,
                                                                    missing = "no") 
                                                                    #missing_text = "(Missing)")  

if (saveFig){
  data_summary_demo %>%
    as_gt() %>%             # convert to gt table
    gt::gtsave(             # save table as image
      filename = "demographics_summary.png"
    )
  
  
  data_summary_demo_flex <- data_summary_demo %>% as_flex_table() 
  data_summary_demo_flex <- fontsize(data_summary_demo_flex,size=fontSize)

  save_as_docx(data_summary_demo_flex,path="demographic_summary.docx",pr_section = sect_properties)
}
#gender <- data_file %>% group_by(`Sex (M=1, F=2)`) %>% tally()
#gender <- gender %>% mutate(per = round(prop.table(n)*100,1))

#age <- data_file %>% summarize(median(`Age at the time of surgery`))

#race <- data_file %>% group_by(`Race (Cauc = 1, Black = 2, Other =3)`) %>% tally()
#race <- race %>% mutate(per = round(prop.table(n)*100,1))

earliest_date <- min(data_file$`Surgery_DateTime`,na.rm = TRUE)
latest_date <- max(data_file$`Surgery_DateTime`,na.rm = TRUE)

select_string_CN_full = c('Pre-op CN 1 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 1 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 1 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Post-op 6 weeks-3 months  CN I Function (1= Normal, 2 = Partial deficit 3 = Complete deficit)', 
                     'PO 6 weeks-3 months CN 1 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)', 
                     'PO 1 year (or last follow up) CN 1 function  (1= Normal, 2 = Partial deficit 3 = Complete deficit)', 
                     'PO 1 year (or last follow up) CN 1 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)', 
                     'Pre-op CN 2 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)', 
                     'Immediate PO CN 2 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 2 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Post-op 6 weeks-3 months  CN 2 Function (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 6 weeks-3 months CN 2 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'PO 1 year (or last follow up) CN 2 function  (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 1 year (or last follow up) CN 2 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Pre-op CN 3 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 3 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 3 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Post-op 6 weeks-3 months  CN 3 Function (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 6 weeks-3 months CN 3 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'PO 1 year (or last follow up) CN 3 function  (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 1 year (or last follow up) CN 3 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Pre-op CN 4 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 4 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 4 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Post-op 6 weeks-3 months  CN 4 Function (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 6 weeks-3 months CN 4 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'PO 1 year (or last follow up) CN 4 function  (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 1 year (or last follow up) CN 4 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Pre-op CN 5 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 5 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 5 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Post-op 6 weeks-3 months  CN 5 Function (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 6 weeks-3 months CN 5 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'PO 1 year (or last follow up) CN 5 function  (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 1 year (or last follow up) CN 5 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Pre-op CN 6 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 6 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 6 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Post-op 6 weeks-3 months  CN 6 Function (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 6 weeks-3 months CN 6 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'PO 1 year (or last follow up) CN 6 function  (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 1 year (or last follow up) CN 6 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Pre-op CN 7 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 7 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 7 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Post-op 6 weeks-3 months  CN 7 Function (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 6 weeks-3 months CN 7 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'PO 1 year (or last follow up) CN 7 function  (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 1 year (or last follow up) CN 7 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Pre-op Frontalis Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO Frontalis Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO Frontalis Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Post-op 6 weeks-3 months Frontalis Function (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 6 weeks-3 months Frontalis Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'PO 1 year (or last follow up) Frontalis Function  (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 1 year (or last follow up) Frontalis Function  compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Pre-op CN 8 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 8 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 8 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Post-op 6 weeks-3 months  CN 8 Function (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 6 weeks-3 months CN 8 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'PO 1 year (or last follow up) CN 8 function  (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 1 year (or last follow up) CN 8 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Pre-op CN 9 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 9 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)', 
                     'Immediate PO CN 9 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Post-op 6 weeks-3 months  CN 9 Function (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 6 weeks-3 months CN 9 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'PO 1 year (or last follow up) CN 9 function  (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 1 year (or last follow up) CN 9 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Pre-op CN 10 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 10 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 10 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Post-op 6 weeks-3 months  CN 10 Function (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 6 weeks-3 months CN 10 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'PO 1 year (or last follow up) CN 10 function  (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 1 year (or last follow up) CN 10 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Pre-op CN 11 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 11 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 11 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Post-op 6 weeks-3 months  CN 11 Function (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 6 weeks-3 months CN 11 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'PO 1 year (or last follow up) CN 11 function  (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 1 year (or last follow up) CN 11 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Pre-op CN 12 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 12 Function (1 = Normal 2 = Partial deficit 3 = Complete deficit)',
                     'Immediate PO CN 12 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'Post-op 6 weeks-3 months  CN 12 Function (1= Normal, 2 = Partial deficit 3 = Complete deficit)',
                     'PO 6 weeks-3 months CN 12 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)',
                     'PO 1 year (or last follow up) CN 12 function  (1= Normal, 2 = Partial deficit 3 = Complete deficit)', 
                     'PO 1 year (or last follow up) CN 12 Function compare to baseline (1 = No change 2 = Improved 3 = deteriorate)')
