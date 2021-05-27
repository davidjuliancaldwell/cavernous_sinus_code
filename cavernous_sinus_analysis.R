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
library(lmerTest)
library(emmeans)
library(janitor)

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
#data_file <- read_excel(file.path(dataDir,"Cavernous sinus cases, data collection sheet.xlsx"))
data_file <- read_excel(file.path(dataDir,"cav_file_for_analysis.xlsx"))


column_names = colnames(data_file)
# print column names to console for easy copy and paste for selection to build code 
cat(column_names,sep="', '")


### clean up data
data_file <- mutate_all(data_file,.funs=str_to_lower)

data_file<-data_file %>% mutate(  across(everything(), ~replace_na(.x, "Missing")))

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


### do everything
data_summary_total <- data_file %>% tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                                                                       all_categorical() ~ "{n} / {N} ({p}%)"),
                                                                                      digits = all_continuous() ~ 1,
                                                                                      missing = "no") 
#missing_text = "(Missing)")  

if (saveFig){
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






### Model fitting time 

# now go back and replace Missing with NaN for model fitting
# linear mixed model to account for missing data


data_file_stats<-data_file %>% mutate(  across(everything(), ~na_if(., "Missing")))

data_file_stats <- data_file_stats %>% rename(t_size=`Tumor size (T, cm)`,
                                              ap_size =`Tumor size (AP, cm)`,
                                              rc_size = `Tumor size (RC, cm)`,
                                              volume = `Tumor volume (ABC/2, cm3)`,
                                              id = `Case Number`,
                                              cav_only = `Tumor only in cavernous sinus (Y=1, 2 = N, NA = NA)`,
                                              epi = `Tumor epicenter (1 = cavernous sinus, 2 = MF 3 = PF 4 = ITF 5 = sella 6 = sphenoid sinus 7 = orbital apex`,
                                              surg_approach = `Surgical approach (Fronto-temporal approach =1, Subtemporal approach = 2 Posterior petrosal = 3  transnasal/transmaxillary = 4 Other = 5`,
                                              resect = `Tumor resection based on PO MRI (1 = GTR, 2 NTR = No residual on MRI, but tumor known to be left, 3 = Subtotal resection (residual < 10 %) 4 = Partial resection (Residual  > 10 %) 5 = Biopsy`,
                                              skull_osteo = `Skull base osteotomy (1 = None 2 = Posterolateral orbitotomy 3 = Full orbitotomy 4 = Zygomatic osteotomy 5 = Orbito-zygomatic osteotomy 6 = retrolabyrinthine 7 = tranlabyrinthine 8: petrous apex`,
                                              optic = `Optic canal decompression and anterior clinoidectomy Y =1, 2=N)`,
                                              wall_cav = `Wall of the cavernous sinus approached ( 1= Lateral wall, 2 = Superior wall 3 = Posterior wall 4 = Anterior wall 5 = Medial wall`
                                              )

fit.logit = multinom(resect ~ surg_approach + skull_osteo + optic + wall_cav,data=data_file_stats)

fit.logit = mlogit(`Tumor resection based on PO MRI (1 = GTR, 2 NTR = No residual on MRI, but tumor known to be left, 3 = Subtotal resection (residual < 10 %) 4 = Partial resection (Residual  > 10 %) 5 = Biopsy` ~ `Surgical approach (Fronto-temporal approach =1, Subtemporal approach = 2 Posterior petrosal = 3  transnasal/transmaxillary = 4 Other = 5`+ `Skull base osteotomy (1 = None 2 = Posterolateral orbitotomy 3 = Full orbitotomy 4 = Zygomatic osteotomy 5 = Orbito-zygomatic osteotomy 6 = retrolabyrinthine 7 = tranlabyrinthine 8: petrous apex` + `Optic canal decompression and anterior clinoidectomy Y =1, 2=N)` + `Wall of the cavernous sinus approached ( 1= Lateral wall, 2 = Superior wall 3 = Posterior wall 4 = Anterior wall 5 = Medial wall` + (1|`Case Number`),data=data_file_stats)



fit.logit = mclogit(`Tumor resection based on PO MRI (1 = GTR, 2 NTR = No residual on MRI, but tumor known to be left, 3 = Subtotal resection (residual < 10 %) 4 = Partial resection (Residual  > 10 %) 5 = Biopsy` ~ `Surgical approach (Fronto-temporal approach =1, Subtemporal approach = 2 Posterior petrosal = 3  transnasal/transmaxillary = 4 Other = 5`+ `Skull base osteotomy (1 = None 2 = Posterolateral orbitotomy 3 = Full orbitotomy 4 = Zygomatic osteotomy 5 = Orbito-zygomatic osteotomy 6 = retrolabyrinthine 7 = tranlabyrinthine 8: petrous apex` + `Optic canal decompression and anterior clinoidectomy Y =1, 2=N)` + `Wall of the cavernous sinus approached ( 1= Lateral wall, 2 = Superior wall 3 = Posterior wall 4 = Anterior wall 5 = Medial wall` + (1|`Case Number`),data=data_file_stats)
emmeans(fit.logit, list(pairwise ~ overallBlockType), adjust = "tukey")

emm_s.t <- emmeans(fit.logit, pairwise ~ overallBlockType| mapStimLevel)
emm_s.t <- emmeans(fit.logit, pairwise ~ mapStimLevel | overallBlockType)
summary(glht(fit.logit,linfct=mcp(overallBlockType="Tukey")))

emm_pairwise <- emmeans(fit.logit,~overallBlockType*pre_post,adjust="Tukey")
contrast(emm_pairwise,interaction="pairwise")
eff_size(emm_pairwise,sigma=sigma(fit.logit),edf=df.residual(fit.logit))
marginal_means_plot <- emmip(fit.logit,overallBlockType~pre_post)
marginal_means_plot <- marginal_means_plot + aes(x = factor(pre_post, level=c('pre','post'))) + labs(x = expression(paste("Pre versus Post Conditioning")),y=expression(paste("Linear Prediction log(",mu,"V)")),color="Experimental Condition",title = paste0("Estimated Marginal Means by Conditioning and Status")) +
  scale_color_brewer(palette="Dark2") +
  scale_fill_brewer(palette="Dark2") 