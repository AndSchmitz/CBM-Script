#Init-----
#Clear workspace
rm(list=ls())
graphics.off()
options(stringsAsFactors = FALSE,warnPartialMatchDollar=T)


#Define work dir
WorkDir <- "C:\\work\\Thuenen Cloud sync\\Canopy budget models R script\\Version 2019-03-25"


#Make the CBM-function available
source(file = file.path(WorkDir,"CalculateCBMs.R"))


#Read annual deposition rates in kg/(ha*a)
AnnualDepositionRates <- read.table(file.path(WorkDir,"DemoData.csv"),header=T,sep=";",stringsAsFactors = F)


#Run CBM script--------
CBM_Results <- CalculateCBMs(
  AnnualDepositionRates = AnnualDepositionRates,
  #Default parameter values
  TracerSubstance = "Na",
  WA_DD_rel_WA_OF = 1,
  Uptake_efficiency_H_vs_NH4 = 6,
  Uptake_efficiency_NH4_vs_NO3 = 6,
  ApplyWetOnlyCorrection = "yes",
  WeakAcidGapFilling = "CB_WithCorrection"
)


#Extract CBM results and save to CSV-----
CBM_Results_kg_ha_a <- CBM_Results$CBM_Results_kg_ha_a
CBM_Results_keq_ha_a <- CBM_Results$CBM_Results_keq_ha_a
write.table(x=CBM_Results_kg_ha_a,file=file.path(WorkDir,"Canopy_budget_in_kg.csv"),sep=";",row.names = F)
write.table(x=CBM_Results_keq_ha_a,file=file.path(WorkDir,"Canopy_budget_in_keq.csv"),sep=";",row.names = F)



#Optional code to plot results:
#Plot------
# library(ggplot2)
# library(dplyr)
# library(tidyr)
# PlotDat <- CBM_Results_kg_ha_a %>%
#   gather(key=BoundaryType,value=y,N_TD_LowerBoundary,N_TD_UpperBoundary) %>%
#   arrange(code_plot) %>%
#   mutate(code_plot = as.factor(code_plot))
# 
# ylabel <-  expression("Total nitrogen deposition rate"~(kg~N~ha^{-1}~a^{-1}))
# 
# ggplot(data=PlotDat,mapping = aes(x=survey_year,y=y,group=survey_year)) +
#   geom_line() +
#   geom_point() +
#   facet_wrap( ~ code_plot) +
#   ylim(0,max(PlotDat$y)) +
#   ylab(ylabel) +
#   xlab("Time")
# 
# ggsave(filename = file.path(WorkDir,"Total_N_Deposition_vs_Time.png"),width=25,height = 15,units = "cm")



