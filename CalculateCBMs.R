#2019-03-23
#
#Andreas Schmitz, Bernd Ahrends, Henning Andreae
#Please report any bugs and suggestions to andreas.schmitz@thuenen.de
#
#This script implements three common canopy budget models to estimate interception deposition, total deposition and
#canopy exchange for several substances based on annual deposition rates in the open field and under canopy.
#
#U94:  Ulrich, B. (1994): Nutrient and Acid-Base Budget of Central European Forest Ecosystems. In: Godbold, D. & A. Hüttermann:
#Effects of Acid Rain on Forest Processes. Wiley-Liss. New York. p. 1-50.
# 
#D95:  Draaijers, G. P. J. & J. W. Erisman (1995): A canopy budget model to assess atmospheric deposition from throughfall measurements.
#Water, Air, and Soil Pollution, 85, 2253-2258.
# 
#V01:  De Vries, W., Reinds, G.J., van der Salm, C., Draaijers, G.P.J., Bleeker, A., Erisman, J.W., Auée, J., Gundersen, P., Kristensen,
#H.L., van Dobben, H., de Zwart, D., Derome, J., Voogd, J.H.C., Vel, E.M., 2001. Intensive Monitoring of Forest Ecosystems in
#Europe. Technical Report 2001. EC, UNECE, Forest Intensive Monitoring Coordinating Institute (FIMCI), Brussels, Geneva.


CalculateCBMs <- function(
  #Default values according to corresponding publications
  AnnualDepositionRates = NA,
  TracerSubstance = "Na",
  WA_DD_rel_WA_OF = 1,
  Uptake_efficiency_H_vs_NH4 = 6,
  Uptake_efficiency_NH4_vs_NO3 = 6,
  ApplyWetOnlyCorrection = "yes",
  WeakAcidGapFilling = "CB_WithCorrection"
) {

#Sanity checks of function arguments-------
  
#AnnualDepositionRates in kg/(ha*a)
if ( is.atomic(AnnualDepositionRates) ) stop("Parameter AnnualDepositionRates must be a data frame.")
ValueColsRequired <- c("h","n_nh4","n_no3","mg","ca","k","na","cl","s_so4","WeakAcids_MA","n_org")
ValueColsRequiredUpper <- c("H","N_NH4","N_NO3","Mg","Ca","K","Na","Cl","S_SO4","WeakAcids_MA","N_Org")
IDCols <- c("code_country","code_plot","survey_year","SamplingType")
InputColsRequired <- c(IDCols,ValueColsRequired)
ColsUnused <- colnames(AnnualDepositionRates)[!(colnames(AnnualDepositionRates) %in% InputColsRequired)]
if ( length(ColsUnused) > 0 ) stop(paste("Parameter AnnualDepositionRates contains the following unused columns:",paste(ColsUnused,collapse = ",")))
ColsMissing <- InputColsRequired[!(InputColsRequired %in% colnames(AnnualDepositionRates))]
if ( length(ColsMissing) > 0 ) stop(paste("Parameter AnnualDepositionRates: The following columns are missing:",paste(ColsMissing,collapse = ",")))
if ( nrow(AnnualDepositionRates) < 2 ) stop("Parameter AnnualDepositionRates must have at least two rows.")
if ( any(is.na(AnnualDepositionRates[,IDCols])) ) stop("One of the following columns contains missing values: code_country,code_plot,survey_year,SamplingType.")
ValidSamplingTypes <- c("UC","OF")
if ( !all(AnnualDepositionRates$SamplingType %in% ValidSamplingTypes) ) stop("Parameter AnnualDepositionRates: Column SamplingType must only contain UC (indicating throughfall (+ stemflow)) and OF (indicating open field deposition).")
CountryPlotYears <- unique(AnnualDepositionRates[,c("code_country","code_plot","survey_year")])
for ( iPY in 1:nrow(CountryPlotYears) ) {
  CurrentCountry <- CountryPlotYears$code_country[iPY]
  CurrentPlot <- CountryPlotYears$code_plot[iPY]
  CurrentYear <- CountryPlotYears$survey_year[iPY]
  CurrentCPY <- paste(CurrentCountry,CurrentPlot,CurrentYear,sep="-")
  idx <- which(  (AnnualDepositionRates$code_country == CurrentCountry) & (AnnualDepositionRates$code_plot == CurrentPlot) & (AnnualDepositionRates$survey_year == CurrentYear) )
  if ( length(idx) != 2 ) stop(paste("Parameter AnnualDepositionRates: Two rows must be provided per country-plot-year (UC and OF deposition rates) but country-plot-year",CurrentCPY,"has",length(idx),"rows."))
  CurrentSamplingTypes <- AnnualDepositionRates$SamplingType[idx]
  if ( length(unique(CurrentSamplingTypes)) != 2 ) stop(paste("Parameter AnnualDepositionRates: Make sure both OF and UC rows are available for country-plot-year",CurrentCPY))
  tmp <- AnnualDepositionRates[idx,]
  tmp <- tmp[tmp$SamplingType == "UC",]
  if ( any(tmp[,ValueColsRequired] < 0,na.rm = T) ) stop(paste("Parameter AnnualDepositionRates: UC deposition rates contain negative values for country-plot-year",CurrentCPY))
  tmp <- AnnualDepositionRates[idx,]
  tmp <- tmp[tmp$SamplingType == "OF",]
  if ( any(tmp[,ValueColsRequired] < 0,na.rm = T) ) stop(paste("Parameter AnnualDepositionRates: OF deposition rates contain negative values for country-plot-year",CurrentCPY))  
}
#Accept input data frame
DB <- AnnualDepositionRates

#Tracer substance
ValidTracerSubstances <- c("Na","S_SO4","Cl")
if ( !(TracerSubstance %in% ValidTracerSubstances) ) stop(paste("Parameter TracerSubstance must be one of",paste(ValidTracerSubstances,collapse = ","),sep=" "))

#WA_DD_rel_WA_OF
#Canopy leaching of weak acids. D95 and V01 assume
#WA_DD_rel_WA_OF = 1, ie.. WA_DD = WA_OF, i.e.
#WA_CL <- WA_UC - WA_OF - WA_DD
#Standard value is 1.
if ( is.na(WA_DD_rel_WA_OF) | !is.numeric(WA_DD_rel_WA_OF) | (WA_DD_rel_WA_OF < 0) ) {
  stop("Parameter WA_DD_rel_WA_OF must be greater or equal to zero.")
}

#Uptake_efficiency_H_vs_NH4
#Canopy uptake efficiency of NH4 vs canopy uptake efficicency of H+ for D95 and V01.
#Standard value is 6. 
if ( is.na(Uptake_efficiency_H_vs_NH4) | !is.numeric(Uptake_efficiency_H_vs_NH4) | (Uptake_efficiency_H_vs_NH4 < 0) ) {
  stop("Parameter Uptake_efficiency_H_vs_NH4 must be greater or equal to zero.")
}

#Uptake_efficiency_NH4_vs_NO3
#Canopy uptake efficiency of NO3 vs canopy uptake efficicency of NH4 for V01.
#Standard value is 6.
if ( is.na(Uptake_efficiency_NH4_vs_NO3) | !is.numeric(Uptake_efficiency_NH4_vs_NO3) | (Uptake_efficiency_NH4_vs_NO3 < 0) ) {
  stop("Parameter Uptake_efficiency_NH4_vs_NO3 must be greater or equal to zero.")
}

#ApplyWetOnlyCorrection
if ( !(ApplyWetOnlyCorrection %in% c("yes","no")) ) {
  stop("Parameter ApplyWetOnlyCorrection must be either yes or no.")
}

#WeakAcidGapFilling
WeakAcidGapFillingValidValues <- c("CB_WithCorrection","CB","none")
if ( !(WeakAcidGapFilling %in% WeakAcidGapFillingValidValues) ) {
  stop("Parameter WeakAcidGapFilling must be one of: CB_WithCorrection, CB, none")
}



#Beginning of calculations--------------------------------------

#Naming conventions
#UC: Under canopy = throughfall + stemflow
#OF: Open-field
#CU: Canopy uptake
#CL: Canopy leaching
#TD: Total deposition
#DD_g: Gaseous dry deposition
#DD_p: Particulate dry deposition


#_Define CheckRoutine -----
#The largest source of errors are calculations resulting in negative flux rates (substractions)
#and non-finite values (division by zero). To catch these issues, the following check routine is
#used at multiple places in the script. In contrast, NA values simply propagate through 
#calculations, which is the desired behaviour.
CheckRoutine <- function(x) {
  for ( c in colnames(x) ) {
    if ( any( (!is.na(x[,c])) & (!is.finite(x[,c])) ) ) stop(paste("Non-finite values found (division by zero?) in column",c))
    if ( any(x[,c] < 0,na.rm=T) ) stop(paste("Negative values found in column",c))
  }
}


#_Definition of molar masses and valences to convert from keq to kg--------
MassValenceInfo <- data.frame(
  Substance = c("H","N_NH4","K","Ca","Mg","Na","Cl","S_SO4","N_NO3","N_tot","Mn","N_Org"),
  MolarMass = c(1.008,14.007,39.098,40.078,24.305,22.99,35,32,14.007,14.007,55,14.007),
  Valence = c(1,1,1,2,2,1,1,2,1,1,2,1)
)
MassValenceInfo$ConversionFactor <- MassValenceInfo$MolarMass / MassValenceInfo$Valence  


#_Apply wet-only correction if desired----
#Values are "species specific annual mean ratios of wet-only and bulk fluxes"
#Thus, the correction for precipitation amount should not be applied additional to
#the species-specific correction because it is already implicitly included in these
#factors.
#Gauger, Thomas, Hans-Dieter Haenel, Claus Rösemann, et al. 2008
#National Implementation of the UNECE Convention on Long-Range Transboundary Air Pollution (Effects)
#Part 1: Deposition Loads: Methods, Modelling and Mapping Results, Trends.
#Bundesforschungsanstalt für Landwirtschaft Institut für Agrarökologie.
if ( ApplyWetOnlyCorrection == "yes" ) {
  WO_k <- 0.62
  WO_ca <- 0.63
  WO_mg <- 0.76
  WO_na <- 0.81
  WO_s_so4 <- 0.82
  WO_cl <- 0.85
  WO_n_no3 <- 0.9
  WO_n_nh4 <- 0.95
  # WO_ph <- 0.97
  WO_h = 1.34
  # WO_quantity_dem <- 1.03
  idx_OF <- which(DB$SamplingType == "OF")
  DB$k[idx_OF] <- DB$k[idx_OF] * WO_k
  DB$ca[idx_OF] <- DB$ca[idx_OF] * WO_ca
  DB$mg[idx_OF] <- DB$mg[idx_OF] * WO_mg
  DB$na[idx_OF] <- DB$na[idx_OF] * WO_na
  DB$s_so4[idx_OF] <- DB$s_so4[idx_OF] * WO_s_so4
  DB$cl[idx_OF] <- DB$cl[idx_OF] * WO_cl
  DB$n_no3[idx_OF] <- DB$n_no3[idx_OF] * WO_n_no3
  DB$n_nh4[idx_OF] <- DB$n_nh4[idx_OF] * WO_n_nh4
  DB$h[idx_OF] <- DB$h[idx_OF] * WO_h
}


#_Conversion to keq-------
#Convert all values from kg/ha/a to keq/ha/a:
#Deposition flow (in kg) / molar mass (in g/mol) = Deposition flow in kmol
#Deposition flow in kmol / valence = Deposition flow in kilo-equivalents (keq)
for ( cn in colnames(DB)  ) {
  
  #Do not convert H+ and weak acids, they come already in keq/ha/a
  if ( cn %in% c("h","WeakAcids_MA") ) {
    colnames(DB)[colnames(DB) == cn] <- paste(cn,"_keq_ha",sep="")
    next
  }
  
  idxC <- which(tolower(MassValenceInfo$Substance) == cn)
  if ( length(idxC) == 0 ) {
    if ( cn %in% c("code_country","code_plot","SamplingType","survey_year") ) next
    print(paste("No factor found to convert substance",cn,"from kg to keq."))
    next
  }
  factor <- MassValenceInfo$ConversionFactor[idxC]
  DB[,cn] <- DB[,cn] / factor
  #And rename the corresponding column
  colnames(DB)[colnames(DB) == cn] <- paste(cn,"_keq_ha",sep="")
}


#_Reshaping and further filtering -----
#Select only required columns from input
ValueCols <- paste(ValueColsRequired,"_keq_ha",sep="")
ColsReq <- c("code_country","code_plot","survey_year","SamplingType",ValueCols)
if ( !all(ColsReq %in% colnames(DB)) ) stop("Some mandatory columns are not present in the input dataset.")
DB <- DB[,ColsReq]
#Cut "_keq_ha" from column names for easier notation
colnames(DB) <- gsub(pattern="_keq_ha",replacement = "", x=colnames(DB))
#Some other renaming
for ( cn in colnames(DB) ) {
  idx <- which ( tolower(ValueColsRequiredUpper) == cn )
  if ( length(idx) == 0 ) next
  colnames(DB)[colnames(DB) == cn] <- ValueColsRequiredUpper[idx]
}
#Convert from two rows per plot-year (one for open-field and one for under canopy) to one row with many cols.
#To identify OF vs. UC measurements add "_UC" and "_OC" to the respective column name.
UC <- DB[DB$SamplingType == "UC",]
OF <- DB[DB$SamplingType == "OF",]
ValueCols <- gsub(pattern="_keq_ha",replacement = "", x=ValueColsRequiredUpper)
colnames(UC)[colnames(UC) %in% ValueCols] <- paste(colnames(UC)[colnames(UC) %in% ValueCols],"_UC",sep="")
colnames(OF)[colnames(OF) %in% ValueCols] <- paste(colnames(OF)[colnames(OF) %in% ValueCols],"_OF",sep="")
#Delete SamplingType column
UC <- UC[,colnames(UC) != "SamplingType"]
OF <- OF[,colnames(OF) != "SamplingType"]
#Merge UC and OF tables by plot and year column
DB <- merge(UC,OF,by=c("code_country","code_plot","survey_year"),all=T)
CheckRoutine(DB)


#Actual CBM calculations------

#_U94-----

#__Calculate DDF----
Tracer_UC <- DB[,colnames(DB) == paste(TracerSubstance,"UC",sep="_")]
Tracer_OF <- DB[,colnames(DB) == paste(TracerSubstance,"OF",sep="_")]
DB$DDF <- (Tracer_UC - Tracer_OF) / Tracer_OF
idx_Tracer_OF_zero <- which(Tracer_OF == 0)
DB$DDF[idx_Tracer_OF_zero] <- NA
DB$DDF[which(DB$DDF < 0)] <- 0

#__DDp - Dry particulate deposition-----
DB$Na_DD_p <- DB$DDF * DB$Na_OF
DB$K_DD_p <- DB$DDF * DB$K_OF
DB$Mg_DD_p <- DB$DDF * DB$Mg_OF
DB$Ca_DD_p <- DB$DDF * DB$Ca_OF
# DB$Mn_DD_p <- DB$DDF * DB$Mn_OF
DB$N_NH4_DD_p <- DB$DDF * DB$N_NH4_OF
DB$S_SO4_DD_p <- DB$DDF * DB$S_SO4_OF
DB$Cl_DD_p <- DB$DDF * DB$Cl_OF
DB$N_NO3_DD_p <- DB$DDF * DB$N_NO3_OF
DB$H_DD_p <- DB$DDF * DB$H_OF #Not listed in U94
CheckRoutine(DB)

#__TD - Total deposition for particulate-only substances (no gas)-----
DB$Na_TD <- DB$Na_OF + DB$Na_DD_p
DB$K_TD <- DB$K_OF + DB$K_DD_p
DB$Mg_TD <- DB$Mg_OF + DB$Mg_DD_p
DB$Ca_TD <- DB$Ca_OF + DB$Ca_DD_p
# DB$Mn_TD <- DB$Mn_OF + DB$Mn_DD_p
CheckRoutine(DB)

#__DDg - Gaseous deposition-----
DB$N_NH3_DD_g <- DB$N_NH4_UC - DB$N_NH4_OF - DB$N_NH4_DD_p
DB$N_NH3_DD_g[which(DB$N_NH3_DD_g < 0)] <- 0
DB$N_NO3_DD_g <- DB$N_NO3_UC - DB$N_NO3_OF - DB$N_NO3_DD_p 
DB$N_NO3_DD_g[which(DB$N_NO3_DD_g < 0)] <- 0
DB$Cl_DD_g <- DB$Cl_UC - DB$Cl_OF - DB$Cl_DD_p
DB$Cl_DD_g[which(DB$Cl_DD_g < 0)] <- 0 
DB$S_SO4_DD_g <- DB$S_SO4_UC - DB$S_SO4_OF - DB$S_SO4_DD_p
DB$S_SO4_DD_g[which(DB$S_SO4_DD_g < 0)] <- 0
DB$H_DD_g <- DB$S_SO4_DD_g + DB$Cl_DD_g + DB$N_NO3_DD_g - DB$N_NH3_DD_g
DB$H_DD_g[which(DB$H_DD_g < 0)] <- 0
CheckRoutine(DB)

#__TD for N - Total deposition for N compounds-----
DB$N_NH4_TD_U94 <- DB$N_NH4_OF + DB$N_NH4_DD_p + DB$N_NH3_DD_g
DB$N_NO3_TD_U94 <- DB$N_NO3_OF + DB$N_NO3_DD_p + DB$N_NO3_DD_g
DB$N_TD_U94 <- DB$N_NH4_TD_U94 + DB$N_NO3_TD_U94
CheckRoutine(DB)
#For S and Cl:
#TD = UC because CU = 0 and CL = 0 or
#TD = OF+DDp+DDg in case of gaseous deposition


#_Weak Acids for D95 and V01----
#Variants D95 and V01 both require estimates of WA_CL

#Weak acids according to the measured alkalinity approach
#provided as input column
DB$WA_UC_MA <- DB$WeakAcids_MA_UC
DB$WA_UC_MA[which(DB$WA_UC_MA < 0)] <- 0
DB$WA_OF_MA <- DB$WeakAcids_MA_OF
DB$WA_OF_MA[which(DB$WA_OF_MA < 0)] <- 0
#Delete columns from DB to avoid having duplicated columns
DB <- DB[,!(colnames(DB) %in% c("WeakAcids_MA_OF","WeakAcids_MA_UC"))]

#Weak acids according to the charge balance method.
#Optionally used for gap-filling of WA_MA values
#De Vries et al 2001
#Annex 4 - A4.16:
#Cat = Ca, Mg, Na, K, H, NH4
#An = SO4, NO3, Cl
DB$CatIon_UC <- DB$Ca_UC + DB$Mg_UC + DB$Na_UC + DB$K_UC + DB$H_UC + DB$N_NH4_UC
DB$Anion_UC <- DB$S_SO4_UC + DB$N_NO3_UC + DB$Cl_UC
DB$WA_UC_CB <- DB$CatIon_UC - DB$Anion_UC
DB$WA_UC_CB[which(DB$WA_UC_CB < 0)] <- 0
DB$CatIon_OF <- DB$Ca_OF + DB$Mg_OF + DB$Na_OF + DB$K_OF + DB$H_OF + DB$N_NH4_OF
DB$Anion_OF <- DB$Cl_OF + DB$N_NO3_OF + DB$S_SO4_OF
DB$WA_OF_CB <- DB$CatIon_OF - DB$Anion_OF
DB$WA_OF_CB[which(DB$WA_OF_CB < 0)] <- 0
CheckRoutine(DB)

#Apply gap-filling settings
#Use MA-approach values as default
DB$WA_UC <- DB$WA_UC_MA
DB$WA_OF <- DB$WA_OF_MA
#Indices where weak acids according to the MA approach are originally missig
#(Replace both UC and OF if either of them is missing to avoid a mixture of MA and CB variant within one year)
idx_WAMA_miss <- which( is.na(DB$WA_UC) | is.na(DB$WA_OF) )
if ( (WeakAcidGapFilling == "CB") | (WeakAcidGapFilling == "CB_WithCorrection") ) {
  if ( length(idx_WAMA_miss) > 0 ) {
    DB$WA_UC[idx_WAMA_miss] <- DB$WA_UC_CB[idx_WAMA_miss]
    DB$WA_OF[idx_WAMA_miss] <- DB$WA_OF_CB[idx_WAMA_miss]
  }
}

#Canopy leaching of weak acids. D95 and V01 assume
#WA_DD_rel_WA_OF = 1, ie.. WA_DD = WA_OF, i.e.
#WA_CL <- WA_UC - WA_OF - WA_DD
DB$WA_DD <- WA_DD_rel_WA_OF * DB$WA_OF
DB$WA_CL <- DB$WA_UC - DB$WA_OF - DB$WA_DD
DB$WA_CL[which(DB$WA_CL < 0)] <- 0
CheckRoutine(DB)


#_D95-----
#Canopy budget according to Draijers and Erisman 1995
#Estimation of canopy leaching of base cations
#__CL of BC-----
DB$K_CL <- DB$K_UC - DB$K_OF - DB$K_DD_p
DB$K_CL[which(DB$K_CL < 0)] <- 0
DB$Ca_CL <- DB$Ca_UC - DB$Ca_OF - DB$Ca_DD_p
DB$Ca_CL[which(DB$Ca_CL < 0)] <- 0
DB$Mg_CL <- DB$Mg_UC - DB$Mg_OF - DB$Mg_DD_p
DB$Mg_CL[which(DB$Mg_CL < 0)] <- 0
#Excretion factor
DB$EF_D95 <- DB$WA_CL / (DB$K_CL + DB$Ca_CL + DB$Mg_CL)
idx_zero <- which( (DB$K_CL + DB$Ca_CL + DB$Mg_CL) == 0 )
DB$EF_D95[idx_zero] <- 1
DB$EF_D95[which(DB$EF_D95 < 0)] <- 0
DB$EF_D95[which(DB$EF_D95 > 1)] <- 1
CheckRoutine(DB)

#__CU of NH4 and H----
DB$BC_CL_D95 <- (DB$K_CL + DB$Ca_CL + DB$Mg_CL) * (1-DB$EF_D95)
#Apply Weak acid correction depending on user settings
if ( WeakAcidGapFilling == "CB_WithCorrection" ) {
  if ( length(idx_WAMA_miss) > 0 ) {
    #Values at idx_WAMA_miss are based on the CB-approach for WA-calculation
    #if parameter WeakAcidGapFilling is not set to "none". If it is set to
    #"CB_WithCorrection", then apply the correction to reduce the bias of CB
    #approach compared to the MA approach. The empirical correction function
    #applied has been established based on data from Germany 2000-2015
    DB$BC_CL_D95[idx_WAMA_miss] <- DB$BC_CL_D95[idx_WAMA_miss] * 0.745 + 0.265
  }
}
#
DB$H_CU_D95 <- DB$BC_CL_D95 / (1 + (1/(Uptake_efficiency_H_vs_NH4*( ((DB$H_UC / DB$N_NH4_UC) + (DB$H_OF/DB$N_NH4_OF)) / 2 ) ) ) )
#Note that the D95 formular for DB$H_CU_D95 is equivalent(!) to the H_CU (Hce) formular 
#in de Vries 2001 5.13 page 79 - besides the aspect that de Vries 2001 use througfall (UC)
#while D95 use the average of throughfall and open field for the ratio between H and NH4.
DB$N_NH4_CU_D95 <- DB$BC_CL_D95 - DB$H_CU_D95
DB$N_NH4_CU_D95[which(DB$N_NH4_CU_D95 < 0)] <- DB$N_NH4_CU_D95
DB$N_NH4_TD_D95 <- DB$N_NH4_UC + DB$N_NH4_CU_D95

#__Total deposition-----
DB$H_TD_D95 <- DB$H_UC + DB$H_CU_D95
DB$N_NO3_TD_D95 <- DB$N_NO3_UC
DB$N_TD_D95 <- DB$N_NH4_TD_D95 + DB$N_NO3_TD_D95
DB$S_SO4_TD_D95 <- DB$S_SO4_UC
CheckRoutine(DB)


#_V01-----

#__CL of BC-------
DB$S_SO4_TD_V01 <- DB$S_SO4_UC
#There are two ways to calculate canopy leaching of BCs:
#1. Sum up existing CL rates per BC element
#2. Substract BC_TD from BC_UC
#In option 1, the CL rates can be checked element-wise
#for negative values and set to zero in case.
#In option 2, negative CL rates for one substance
#could be compensated by positive CL rates for another
#substance, thus negative rates could be overlooked.
#Thus, prefer option 1.
DB$BC_CL_V01 <- DB$K_CL + DB$Ca_CL + DB$Mg_CL
DB$BC_CL_V01[which(DB$BC_CL_V01 < 0)] <- 0
CheckRoutine(DB)


#__CU of NH4-------

#The quantity N_NH4_H_CU_V01 is mathematically the same value as the quantity BC_CL_D95.
#Hover, depending on user-defined parameters, a correction of weak acid calculations has been applied to BC_CL_D95 already.
#Thus, take value from BC_CL_D95  here.
DB$N_NH4_H_CU_V01 <- DB$BC_CL_D95 
#DB$N_NH4_H_CU_V01 <- DB$BC_CL_V01 - DB$WA_CL #original relation
DB$N_NH4_H_CU_V01[which(DB$N_NH4_H_CU_V01 < 0)] <- 0
DB$H_CU_V01 <- (Uptake_efficiency_H_vs_NH4*DB$H_UC / (DB$N_NH4_UC + Uptake_efficiency_H_vs_NH4*DB$H_UC)) * DB$N_NH4_H_CU_V01
DB$N_NH4_CU_V01 <- DB$N_NH4_H_CU_V01 - DB$H_CU_V01
DB$N_NH4_CU_V01[which(DB$N_NH4_CU_V01 < 0)] <- 0
DB$N_NH4_TD_V01 <- DB$N_NH4_UC + DB$N_NH4_CU_V01

CheckRoutine(DB)

#__CU of NO3------
DB$N_CU_V01 <- DB$N_NH4_CU_V01 * (Uptake_efficiency_NH4_vs_NO3 * DB$N_NH4_UC + DB$N_NO3_UC) / (Uptake_efficiency_NH4_vs_NO3 * DB$N_NH4_UC)
DB$N_CU_V01[which(DB$N_CU_V01 < 0)] <- 0
DB$N_NO3_CU_V01 <- DB$N_CU_V01 - DB$N_NH4_CU_V01
DB$N_NO3_CU_V01[which(DB$N_NO3_CU_V01 < 0)] <- 0

#__Total deposition-----
DB$N_NO3_TD_V01 <- DB$N_NO3_UC + DB$N_NO3_CU_V01
DB$N_TD_V01 <- DB$N_NH4_TD_V01 + DB$N_NO3_TD_V01
DB$H_TD_V01 <- DB$H_UC + DB$H_CU_V01

CheckRoutine(DB)

#N_TD upper and lower boundaries-------
DB$N_TD_U94V01_Max <- apply(X=DB[,c("N_TD_U94","N_TD_V01")],MARGIN=1,FUN=function(x) ifelse(all(is.na(x)),NA,max(x,na.rm=T)))
DB$N_TD_LowerBoundary <- DB$N_NH4_UC + DB$N_NO3_UC + DB$N_Org_OF
DB$N_TD_UpperBoundary <- DB$N_TD_U94V01_Max + DB$N_Org_OF
DB <- DB[,colnames(DB) != "N_TD_U94V01_Max"]


#Acid deposition ----
DB$H_TD_U83ClSO2 <- DB$H_OF + DB$H_DD_p + DB$S_SO4_DD_g + DB$Cl_DD_g
DB$H_TD_U83SO2 <- DB$H_OF + DB$H_DD_p + DB$S_SO4_DD_g
DB$AC_TD_U83_keq <- DB$H_TD_U83SO2 + DB$N_NH4_UC
DB$AC_TD_U94 <- DB$H_TD_U83ClSO2 + DB$N_NH4_UC + DB$N_NO3_DD_g - DB$N_NH3_DD_g
DB$AC_TD_U94[which(DB$AC_TD_U94 < 0)] <- 0
DB$AC_TD_D95 <- DB$H_UC + DB$N_NH4_UC + DB$H_CU_D95 + DB$N_NH4_CU_D95
DB$H_CU_U83 <- DB$H_TD_U83SO2 - DB$H_UC
DB$H_CU_U83[which(DB$H_CU_U83 < 0)] <- 0
DB$H_CU_U83Cl <- DB$H_TD_U83ClSO2 - DB$H_UC
DB$H_CU_U83Cl[which(DB$H_CU_U83Cl < 0)] <- 0
DB$H_CU_U94 <- DB$AC_TD_U94 - DB$H_UC
DB$H_CU_U94[which(DB$H_CU_U94 < 0)] <- 0

CheckRoutine(DB)

#Finish and export data-----
#Export of deposition rates in keq/(ha*a).
DB_keq <- DB
#If there is just one plot, then the following apply()
#command does not work. Temporarily add a dummy row.
DummyAdded <- F
if ( nrow(DB_keq) == 1 ) {
  DummyAdded <- T
  DB_keq[2,] <- DB_keq[1,]
}
DB_keq <- as.data.frame(apply(X=DB_keq,MARGIN=2,FUN=round,3))
if ( DummyAdded ) DB_keq <- DB_keq[1,]


#Export of deposition rates in kg/(ha*a).
#There are three types of columns:
#1. Columns that can be converted to kg/(ha*a)
#2. Columns that cannot be converted to kg/(ha*a) in a meaningful way, like base cation of weak acid flows.
#These remain in keq/(ha*a).
#3. Columns without units (ratios, plot, year).

#1. Define a data frame with all quantities scheduled for conversion and the corresponding conversion factor
#from keq to kg.
MassValenceInfo$ConversionFactor <- MassValenceInfo$MolarMass / MassValenceInfo$Valence
keqToKgDF <- MassValenceInfo
TranslationFun <- function(DF,suffix) {
  DF$Substance <- paste(DF$Substance,suffix,sep="")
  return(DF)
}
keqToKgDF <- rbind(keqToKgDF,TranslationFun(MassValenceInfo,"_OF"))
keqToKgDF <- rbind(keqToKgDF,TranslationFun(MassValenceInfo,"_UC"))
keqToKgDF <- rbind(keqToKgDF,TranslationFun(MassValenceInfo,"_DD_p"))
keqToKgDF <- rbind(keqToKgDF,TranslationFun(MassValenceInfo,"_DD_g"))
keqToKgDF <- rbind(keqToKgDF,TranslationFun(MassValenceInfo,"_TD"))
keqToKgDF <- rbind(keqToKgDF,TranslationFun(MassValenceInfo,"_CU"))
keqToKgDF <- rbind(keqToKgDF,TranslationFun(MassValenceInfo,"_CL"))
#
#Handle special cases
#Other nitrogen flows
NSpecies <- data.frame(Substance=c("N_NH3_DD_g","N_CU","N_TD","N_TD_LowerBoundary","N_TD_UpperBoundary"),
                       MolarMass=NA,Valence=NA,ConversionFactor=MassValenceInfo$ConversionFactor[MassValenceInfo$Substance == "N_NH4"])
keqToKgDF <- rbind(keqToKgDF,NSpecies)
#Add different CBM versions for all substances
tmp <- keqToKgDF
keqToKgDF <- rbind(keqToKgDF,TranslationFun(tmp,"_U94"))
keqToKgDF <- rbind(keqToKgDF,TranslationFun(tmp,"_D95"))
keqToKgDF <- rbind(keqToKgDF,TranslationFun(tmp,"_V01"))
keqToKgDF <- rbind(keqToKgDF,TranslationFun(tmp,"_TI"))
keqToKgDF$Substance <- as.character(keqToKgDF$Substance)
  
#2. Define columns to remain in keq
ColsKeepKeq <- c("BC_TD_V01","BC_UC_V01","BC_CL","BC_CL_D95","BC_CL_V01","CatIon_UC","Anion_UC","WA_UC","CatIon_OF","Anion_OF","WA_OF",
                 "WA_CL","WA_DD","H_TD_U83ClSO2","H_TD_U83SO2","AC_TD_U83_keq","AC_TD_U94","AC_TD_D95",
                 "H_CU_U83","H_CU_U83Cl","H_CU_U94","N_NH4_H_CU_V01","CatIon_UC_TI","Anion_UC_TI","WA_UC_TI","WA_OF_TI","CatIon_OF_TI","Anion_OF_TI","WA_CL_TI","BC_TD_TI",
                 "BC_UC_TI","BC_CL_TI","N_NH4_H_CU_TI","WA_UC_MA","WA_OF_MA","WA_UC_CB","WA_OF_CB")

#3. Define dimensionless columns
ColsKeep <- c("code_country","code_plot","survey_year","DDF","EF","EF_D95")

#Check for unhandled columns
UnhandledCols <- colnames(DB)[ !(colnames(DB) %in% c(keqToKgDF$Substance,ColsKeepKeq,ColsKeep)) ]
if ( length(UnhandledCols) > 0 ) stop(paste("Export of deposition in rates in kg/(ha*a) failed: No conversion rules defined for the following columns:",paste(UnhandledCols,collapse = ",")))

#Perform conversion
DB_kg <- DB
#Explicitly add "_keq" to those columns that stay in keq.
colnames(DB_kg)[colnames(DB_kg) %in% ColsKeepKeq] <- paste(colnames(DB_kg)[colnames(DB_kg) %in% ColsKeepKeq],"_keq",sep="")
#Convert to kg
for ( c in colnames(DB_kg) ) {
  idx_keqToKgDF <- which(keqToKgDF$Substance == c)
  if ( length(idx_keqToKgDF) == 0 ) next
  if ( length(idx_keqToKgDF) > 1 ) stop("Conversion from keq to kg failed. More than one conversion rule found.")
  DB_kg[,c] <- DB_kg[,c] * keqToKgDF$ConversionFactor[idx_keqToKgDF]
}
#If there is just one plot, then the following apply()
#command does not work. Temporarily add a dummy row.
DummyAdded <- F
if ( nrow(DB_kg) == 1 ) {
  DummyAdded <- T
  DB_kg[2,] <- DB_kg[1,]
}
DB_kg <- as.data.frame(apply(X=DB_kg,MARGIN=2,FUN=round,3))
if ( DummyAdded ) DB_kg <- DB_kg[1,]

#Done-----
# print("Finished.")

return(list(
  CBM_Results_kg_ha_a = DB_kg,
  CBM_Results_keq_ha_a = DB_keq
))

} #End of overall CBM-function


