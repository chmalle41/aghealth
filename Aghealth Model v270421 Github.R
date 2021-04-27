library(plyr)
library(numbers)
library(mc2d)


wd <- "C:/Users/cm1479/Google Drive/L drive files/Africa Assessment/Agriculture/Github"

setwd(wd)



##Define modules for agricultural systems and health impacts

herd_ruminants <- function(AF_dairy = 0, NCOWS = 0, FNUM = 0,
                           Ckg_dairy = 0, AFkg_dairy = 0, AMkg_dairy = 0, MFSkg_dairy = 0, MMSkg_dairy = 0, 
                           DR1_dairy = 0, DR1m_dairy = 0, DR2_dairy = 0, FR_dairy = 0, FRRF_dairy = 0, RRF_dairy =0,
                           AFC_dairy = 0, MFR_dairy = 0, 
                           Ckg_beef = 0, AFkg_beef = 0, AMkg_beef = 0, MFSkg_beef = 0, MMSkg_beef = 0, 
                           DR1_beef = 0, DR1m_beef = 0, DR2_beef = 0, FR_beef = 0, FRRF_beef = 0, RRF_beef =0,
                           AFC_beef = 0, MFR_beef = 0,
                           FATTDAY = 0, LWSTARTF = 0, LWENDF = 0, LWSTARTM = 0, LWENDM = 0){
  
  ##Dairy Herd
  ##Female
  AFin_dairy <- AF_dairy * RRF_dairy/100
  AFx_dairy <- AF_dairy * DR2_dairy/100
  AFexit_dairy <- AF_dairy * (RRF_dairy/100) - AFx_dairy
  
  CFin_dairy <- AF_dairy * ((1 - (DR2_dairy / 100)) * (FR_dairy / 100) + (RRF_dairy / 100)) * 0.5 * (1 - (DR1_dairy / 100))
  CMin_dairy <- AF_dairy * ((1 - (DR2_dairy / 100)) * (FR_dairy / 100) + (RRF_dairy / 100)) * 0.5 * (1 - (DR1m_dairy / 100))
  RFin_dairy <- ((AF_dairy * (RRF_dairy / 100)) / FRRF_dairy) / (1 - (DR2_dairy / 100))^(AFC_dairy)
  RFexit_dairy <- ((AF_dairy * (RRF_dairy / 100)) / FRRF_dairy) - AFin_dairy
  RFx_dairy <- RFin_dairy - (AFin_dairy + RFexit_dairy)
  RF_dairy <- (RFin_dairy + AFin_dairy) / 2 * AFC_dairy
  MFin_dairy <- CFin_dairy - RFin_dairy
  
  ASF_dairy <- AFC_dairy * (MFSkg_dairy - Ckg_dairy) / (AFkg_dairy - Ckg_dairy)
  
  MFtexit_dairy <- MFin_dairy * (1 - (DR2_dairy / 100))^(ASF_dairy)
  MFtx_dairy <- MFin_dairy - MFtexit_dairy
  MFt_dairy <- (MFin_dairy + MFtexit_dairy) / 2 * (AFC_dairy * (MFSkg_dairy - Ckg_dairy) / (AFkg_dairy - Ckg_dairy))
  MFtd_dairy <- MFt_dairy
  
  ##Male
  AM_dairy <- AF_dairy * MFR_dairy
  AMx_dairy <- AM_dairy * (DR2_dairy / 100)
  AMexit_dairy <- AM_dairy / AFC_dairy - AMx_dairy
  AMin_dairy <- AM_dairy/AFC_dairy
  RMin_dairy <-AMin_dairy / (1 - (DR2_dairy / 100))^(AFC_dairy)
  RMx_dairy <- RMin_dairy - AMin_dairy
  RM_dairy <- (RMin_dairy + AMin_dairy) / 2 * AFC_dairy
  MMin_dairy <- CMin_dairy - RMin_dairy
  
  ASM_dairy <- AFC_dairy * (MMSkg_dairy - Ckg_dairy) / (AMkg_dairy - Ckg_dairy)
  
  MMtexit_dairy <- MMin_dairy * (1 - (DR2_dairy / 100))^(ASM_dairy)
  MMtx_dairy <- MMin_dairy - MMtexit_dairy
  MMt_dairy <- (MMin_dairy + MMtexit_dairy) / 2 * (AFC_dairy * (MMSkg_dairy - Ckg_dairy) / (AMkg_dairy - Ckg_dairy))
  MMtd_dairy <- MMt_dairy
  DCATTLE_dairy <- AF_dairy + RF_dairy + MFt_dairy + AM_dairy + RM_dairy + MMt_dairy
  AFD_dairy <- AF_dairy
  
  
  
  if(NCOWS - DCATTLE_dairy > 0){
  
  ##Beef Herd
  BCATTLE <- NCOWS - DCATTLE_dairy
  ##Female
  AF_beef <- (AFD_dairy / DCATTLE_dairy) * BCATTLE
  AFin_beef <- AF_beef * RRF_beef/100
  AFx_beef <- AF_beef * DR2_beef/100
  AFexit_beef <- AF_beef * (RRF_beef/100) - AFx_beef
  
  CFin_beef <- AF_beef * ((1 - (DR2_beef / 100)) * (FR_beef / 100) + (RRF_beef / 100)) * 0.5 * (1 - (DR1_beef / 100))
  CMin_beef <- AF_beef * ((1 - (DR2_beef / 100)) * (FR_beef / 100) + (RRF_beef / 100)) * 0.5 * (1 - (DR1m_beef / 100))
  RFin_beef <- ((AF_beef * (RRF_beef / 100)) / FRRF_beef) / (1 - (DR2_beef / 100))^(AFC_beef)
  RFexit_beef <- ((AF_beef * (RRF_beef / 100)) / FRRF_beef) - AFin_beef
  RFx_beef <- RFin_beef - (AFin_beef + RFexit_beef)
  RF_beef <- (RFin_beef + AFin_beef) / 2 * AFC_beef
  MFin_beef <- CFin_beef - RFin_beef
  
  ASF_beef <- AFC_beef * (MFSkg_beef - Ckg_beef) / (AFkg_beef - Ckg_beef)
  
  MFtexit_beef <- MFin_beef * (1 - (DR2_beef / 100))^(ASF_beef)
  MFtx_beef <- MFin_beef - MFtexit_beef
  MFt_beef <- (MFin_beef + MFtexit_beef) / 2 * (AFC_beef * (MFSkg_beef - Ckg_beef) / (AFkg_beef - Ckg_beef))
  MFtb_beef <- MFt_beef
  
  ##Male
  AM_beef <- AF_beef * MFR_beef
  AMx_beef <- AM_beef * (DR2_beef / 100)
  AMexit_beef <- AM_beef / AFC_beef - AMx_beef
  AMin_beef <- AM_beef/AFC_beef
  RMin_beef <-AMin_beef / (1 - (DR2_beef / 100))^(AFC_beef)
  RMx_beef <- RMin_beef - AMin_beef
  RM_beef <- (RMin_beef + AMin_beef) / 2 * AFC_beef
  MMin_beef <- CMin_beef - RMin_beef
  
  ASM_beef <- AFC_beef * (MMSkg_beef - Ckg_beef) / (AMkg_beef - Ckg_beef)
  
  MMtexit_beef <- MMin_beef * (1 - (DR2_beef / 100))^(ASM_beef)
  MMtx_beef <- MMin_beef - MMtexit_beef
  MMt_beef <- (MMin_beef + MMtexit_beef) / 2 * (AFC_beef * (MMSkg_beef - Ckg_beef) / (AMkg_beef - Ckg_beef))
  MMtb_beef <- MMt_beef
  DCATTLE_beef <- AF_beef + RF_beef + MFt_beef + AM_beef + RM_beef + MMt_beef
  AFD_beef <- AF_beef
  
  
  }else{
    
    ##Beef Herd
    BCATTLE <- 0
    ##Female
    AF_beef <- 0
    AFin_beef <- 0
    AFx_beef <- 0
    AFexit_beef <- 0
    
    CFin_beef <- 0
    CMin_beef <- 0
    RFin_beef <- 0
    RFexit_beef <- 0
    RFx_beef <- 0
    RF_beef <- 0
    MFin_beef <- 0
    
    ASF_beef <- 0
    
    MFtexit_beef <- 0
    MFtx_beef <- 0
    MFt_beef <- 0
    MFtb_beef <- 0
    
    ##Male
    AM_beef <- 0
    AMx_beef <- 0
    AMexit_beef <- 0
    AMin_beef <- 0
    RMin_beef <-0
    RMx_beef <- 0
    RM_beef <- 0
    MMin_beef <- 0
    
    ASM_beef <- 0
    
    MMtexit_beef <- 0
    MMtx_beef <- 0
    MMt_beef <- 0
    MMtb_beef <- 0
    DCATTLE_beef <- 0
    AFD_beef <- 0

}
  
  
  ##Feedlot
  M_HERD = MFtd_dairy + MMtd_dairy + MFtb_beef + MFtb_beef
  DMFfrac = MFtd_dairy / M_HERD
  BMFfrac = MFtb_beef / M_HERD
  DMMfrac = MMtd_dairy / M_HERD
  BMMfrac = MMtb_beef / M_HERD
  
  MFf_dairy = FNUM * DMFfrac
  MFf_beef = FNUM * BMFfrac
  MMf_dairy = FNUM * DMMfrac
  MMf_beef = FNUM * BMMfrac
  
  MFfexit_dairy = MFtexit_dairy * (MFf_dairy / MFt_dairy)
  AFF_dairy = (LWSTARTF - Ckg_dairy) / (AFkg_dairy - Ckg_dairy) * AFC_dairy
  ASFF_dairy = AFF_dairy + FATTDAY / 365
  MF_dairy = MFt_dairy - ifelse(is.na(MFf_dairy),0,MFf_dairy)
  MFexit_dairy = MFtexit_dairy * (MF_dairy / MFt_dairy)
  
  MMfexit_dairy = MMtexit_dairy * (MMf_dairy / MMt_dairy)
  AFM_dairy = (LWSTARTM - Ckg_dairy) / (AMkg_dairy - Ckg_dairy) * AFC_dairy
  ASFM_dairy = AFM_dairy + FATTDAY / 365
  MM_dairy = MMt_dairy - ifelse(is.na(MMf_dairy),0,MMf_dairy)
  MMexit_dairy = MMtexit_dairy * (MM_dairy / MMt_dairy)
  
  MFfexit_beef = MFtexit_beef * (MFf_beef / MFt_beef)
  AFF_beef = (LWSTARTF - Ckg_beef) / (AFkg_beef - Ckg_beef) * AFC_beef
  ASFF_beef = AFF_beef + FATTDAY / 365
  MF_beef = MFt_beef - ifelse(is.na(MFf_beef),0,MFf_beef)
  MFexit_beef = MFtexit_beef * (MF_beef / MFt_beef)
  
  MMfexit_beef = MMtexit_beef * (MMf_beef / MMt_beef)
  AFM_beef = (LWSTARTM - Ckg_beef) / (AMkg_beef - Ckg_beef) * AFC_beef
  ASFM_beef = AFM_beef + FATTDAY / 365
  MM_beef = MMt_beef - ifelse(is.na(MMf_beef),0,MMf_beef)
  MMexit_beef = MMtexit_beef * (MM_beef / MMt_beef)
  
  
  #Weight and Growth Rates
  RFkg_dairy = (AFkg_dairy - Ckg_dairy) / 2 + Ckg_dairy
  RMkg_dairy = (AMkg_dairy - Ckg_dairy) / 2 + Ckg_dairy
  MFkg_dairy = (MFSkg_dairy - Ckg_dairy) / 2 + Ckg_dairy
  MMkg_dairy = (MMSkg_dairy - Ckg_dairy) / 2 + Ckg_dairy
  MFfkg_dairy = (((LWSTARTF - Ckg_dairy) / 2 + Ckg_dairy) * AFF_dairy + ((LWENDF - LWSTARTF) / 2 + LWSTARTF) * (FATTDAY / 365)) / ASFF_dairy
  MMfkg_dairy = (((LWSTARTM - Ckg_dairy) / 2 + Ckg_dairy) * AFM_dairy + ((LWENDM - LWSTARTM) / 2 + LWSTARTM) * (FATTDAY / 365)) / ASFM_dairy
  DWGF_dairy = (AFkg_dairy - Ckg_dairy) / (365 * AFC_dairy)
  DWGM_dairy = (AMkg_dairy - Ckg_dairy) / (365 * AFC_dairy)
  DWGFF_dairy = (DWGF_dairy * AFF_dairy + ((LWENDF - LWSTARTF) / FATTDAY) * (FATTDAY / 365)) / ASFF_dairy
  DWGFM_dairy = (DWGM_dairy * AFM_dairy + ((LWENDM - LWSTARTM) / FATTDAY) * (FATTDAY / 365)) / ASFM_dairy
  
  RFkg_beef = (AFkg_beef - Ckg_beef) / 2 + Ckg_beef
  RMkg_beef = (AMkg_beef - Ckg_beef) / 2 + Ckg_beef
  MFkg_beef = (MFSkg_beef - Ckg_beef) / 2 + Ckg_beef
  MMkg_beef = (MMSkg_beef - Ckg_beef) / 2 + Ckg_beef
  MFfkg_beef = (((LWSTARTF - Ckg_beef) / 2 + Ckg_beef) * AFF_beef + ((LWENDF - LWSTARTF) / 2 + LWSTARTF) * (FATTDAY / 365)) / ASFF_beef
  MMfkg_beef = (((LWSTARTM - Ckg_beef) / 2 + Ckg_beef) * AFM_beef + ((LWENDM - LWSTARTM) / 2 + LWSTARTM) * (FATTDAY / 365)) / ASFM_beef
  DWGF_beef = (AFkg_beef - Ckg_beef) / (365 * AFC_beef)
  DWGM_beef = (AMkg_beef - Ckg_beef) / (365 * AFC_beef)
  DWGFF_beef = (DWGF_beef * AFF_beef + ((LWENDF - LWSTARTF) / FATTDAY) * (FATTDAY / 365)) / ASFF_beef
  DWGFM_beef = (DWGM_beef * AFM_beef + ((LWENDM - LWSTARTM) / FATTDAY) * (FATTDAY / 365)) / ASFM_beef
  
  
  my_list <- list("AF_dairy" = AF_dairy, "AM_dairy" = AM_dairy,
                  "RF_dairy" = RF_dairy, "RM_dairy" = RM_dairy,
                  "MF_dairy" = MF_dairy, "MM_dairy" = MM_dairy,
                  "MFf_dairy" = MFf_dairy, "MMf_dairy" = MMf_dairy,
                  
                  "AF_beef" = AF_beef, "AM_beef" = AM_beef,
                  "RF_beef" = RF_beef, "RM_beef" = RM_beef,
                  "MF_beef" = MF_beef, "MM_beef" = MM_beef,
                  "MFf_beef" = MFf_beef, "MMf_beef" = MMf_beef,
                  
                  "RFkg_dairy" = RFkg_dairy, "RMkg_dairy" = RMkg_dairy,
                  "MFkg_dairy" = MFkg_dairy, "MMkg_dairy" = MMkg_dairy,
                  "MFfkg_dairy" = MFfkg_dairy, "MMfkg_dairy" = MMfkg_dairy,
                  "DWGF_dairy" = DWGF_dairy, "DWGM_dairy" = DWGM_dairy,
                  "DWGFF_dairy" = DWGFF_dairy, "DWGFM_dairy" = DWGFM_dairy,
                  
                  "RFkg_beef" = RFkg_beef, "RMkg_beef" = RMkg_beef,
                  "MFkg_beef" = MFkg_beef, "MMkg_beef" = MMkg_beef,
                  "MFfkg_beef" = MFfkg_beef, "MMfkg_beef" = MMfkg_beef,
                  "DWGF_beef" = DWGF_beef, "DWGM_beef" = DWGM_beef,
                  "DWGFF_beef" = DWGFF_beef, "DWGFM_beef" = DWGFM_beef
                  
  )
  return(my_list) 
  
}


herd_sheepgoat <- function(NANIMAL = 0, AF_dairy = 0, AF_nondairy = 0,
                           Ckg = 0, AFkg = 0, AMkg = 0, MFSkg = 0, MMSkg = 0, 
                           DR1 = 0, DR2 = 0, FR = 0, FRRF = 0, RRF = 0, 
                           AFC = 0, DSR = 0, MFR = 0, LINT = 0, LITSIZE = 0){
  
##Dairy Herd - Female  
  
  AF = AF_dairy
  AFin = AF * (RRF / 100)
  AFx = AF * (DR2 / 100)
  AFexit = AF * (RRF / 100) - AFx
  Cin = AF * ((1 - (DR2 / 100)) * (((365 * FR) / LINT) / 100) * LITSIZE + (RRF / 100))
  RFin = ((AF * (RRF / 100)) / FRRF) / ((1 - (DR1 / 100)) * (1 - (DR2 / 100))^(AFC - 1))
  RFexit = ((AF * (RRF / 100)) / FRRF) - AFin
  RFx = RFin - (AFin + RFexit)
  RF1 = RFin * (1 - (DR1 / 100))
  RFA = (RFin + RF1) / 2
  RFB = ((RF1 + AFin) / 2) * (AFC - 1)
  RF = ((RFin + RF1) / 2) + (((RF1 + AFin) / 2) * (AFC - 1))
  MFin = Cin / 2 - RFin
  
  ASF = AFC * (MFSkg - Ckg) / (AFkg - Ckg)
  
  
  MFexit = MFin * (1 - (DR1 / 100))^ASF
  MFx = MFin - MFexit
  MF = (MFin + MFexit) / 2 * ASF
  
  
##Dairy Herd  - Male  
  
  AM = AF * MFR
  AMx = AM * (DR2 / 100)
  AMexit = AM / (3 * AFC) - AMx
  AMin = AM / (3 * AFC)
  RMin = AMin / ((1 - (DR1 / 100)) * (1 - (DR2 / 100))^(AFC - 1))
  RM1 = RMin * (1 - (DR1 / 100))
  RMA = (RMin + RM1) / 2
  RMB = ((RM1 + AMin) / 2) * (AFC - 1)
  RMx = RMin - AMin
  RM = ((RMin + RM1) / 2) + ((RM1 + AMin) / 2) * (AFC - 1)
  MMin = Cin / 2 - RMin
  
  ASM = AFC * (MMSkg - Ckg) / (AMkg - Ckg)
  
  MMexit = MMin * (1 - (DR1 / 100))^ASM
  MMx = MMin - MMexit
  MM = (MMin + MMexit) / 2 * ASM
  
  DANIMAL = AF + RF + MF + AM + RM + MM
  AFD = AF
  
  
##Non-dairy herd
  
  BANIMAL = NANIMAL - DANIMAL
  AF_nd = AF_nondairy
  
##Female section  
  
  AFin_nd = AF_nd * (RRF / 100)
  AFx_nd = AF_nd * (DR2 / 100)
  AFexit_nd = AF_nd * (RRF / 100) - AFx_nd
  
  Cin_nd = AF_nd * ((1 - (DR2 / 100)) * (((365 * FR) / LINT) / 100) * LITSIZE + (RRF / 100)) 
  RFin_nd = ((AF_nd * (RRF / 100)) / FRRF) / ((1 - (DR1 / 100)) * (1 - (DR2 / 100))^(AFC - 1))
  
  RFexit_nd = ((AF_nd * (RRF / 100)) / FRRF) - AFin_nd
  RFx_nd = RFin_nd - (AFin_nd + RFexit_nd)
  RF1_nd = RFin_nd * (1 - (DR1 / 100))
  RFA_nd = (RFin_nd + RF1_nd) / 2
  RFB_nd = ((RF1_nd + AFin_nd) / 2) * (AFC - 1)
  RF_nd = ((RFin_nd + RF1_nd) / 2) + (((RF1_nd + AFin_nd) / 2) * (AFC - 1))
  MFin_nd = Cin_nd / 2 - RFin_nd
  
  ASF_nd = AFC * (MFSkg - Ckg) / (AFkg - Ckg)
  
  
  MFexit_nd = MFin_nd * (1 - (DR1 / 100))^ASF_nd
  MFx_nd = MFin_nd - MFexit_nd
  MF_nd = (MFin_nd + MFexit_nd) / 2 * ASF_nd
  
  
##Male section 
  
  AM_nd = AF_nd * MFR
  AMx_nd = AM_nd * (DR2 / 100)
  AMexit_nd = AM_nd / (3 * AFC) - AMx_nd
  AMin_nd = AM_nd / (3 * AFC)
  RMin_nd = AMin_nd / ((1 - (DR1 / 100)) * (1 - (DR2 / 100))^(AFC - 1))
  RM1_nd = RMin_nd * (1 - (DR1 / 100))
  RMA_nd = (RMin_nd + RM1_nd) / 2
  RMB_nd = ((RM1_nd + AMin_nd) / 2) * (AFC - 1)
  RMx_nd = RMin_nd - AMin_nd
  RM_nd = ((RMin_nd + RM1_nd) / 2) + ((RM1_nd + AMin_nd) / 2) * (AFC - 1)
  MMin_nd = Cin_nd / 2 - RMin_nd
  
  ASM_nd = AFC * (MMSkg - Ckg) / (AMkg - Ckg)
  
  MMexit_nd = MMin_nd * (1 - (DR1 / 100))^ASM_nd
  MMx_nd = MMin_nd - MMexit_nd
  MM_nd = (MMin_nd + MMexit_nd) / 2 * ASM_nd
  
  
##Average weights and growth rates
  
  
  RFkg = (AFkg + Ckg) / 2
  RF1kg = Ckg + ((AFkg - Ckg) / AFC)
  RFAkg = (Ckg + RF1kg) / 2
  RFBkg = (RF1kg + AFkg) / 2
  RMkg = (AMkg + Ckg) / 2
  RM1kg = Ckg + ((AMkg - Ckg) / AFC)
  RMAkg = (Ckg + RM1kg) / 2
  RMBkg = (RM1kg + AMkg) / 2
  MFkg = (MFSkg - Ckg) / 2 + Ckg
  MMkg = (MMSkg - Ckg) / 2 + Ckg
  
  DWGF = (AFkg - Ckg) / (365 * AFC)
  DWGM = (AMkg - Ckg) / (365 * AFC)
  
  
  
  my_list <- list("AF" = AF, "AM" = AM,
                  "RF" = RF, "RM" = RM,
                  "MF" = MF, "MM" = MM,
                  "RFA" = RFA, "RMA" = RMA,
                  "RFB" = RFB, "RMB" = RMB,
                  
                  "AF_nd" = AF_nd, "AM_nd" = AM_nd,
                  "RF_nd" = RF_nd, "RM_nd" = RM_nd,
                  "MF_nd" = MF_nd, "MM_nd" = MM_nd,
                  "RFA_nd" = RFA_nd, "RMA_nd" = RMA_nd,
                  "RFB_nd" = RFB_nd, "RMB_nd" = RMB_nd,
                  
                  "DWGF" = DWGF, "DWGM" = DWGM,
                  
                  "RFkg" = RFkg, "RF1kg" = RF1kg,
                  "RFAkg" = RFAkg, "RFBkg" = RFBkg,
                  "RMkg" = RFkg, "RM1kg" = RF1kg,
                  "RMAkg" = RFkg, "RMBkg" = RF1kg,
                  "MFkg" = RFkg, "MMkg" = RF1kg)
  
  return(my_list)
  
}



herd_pigs <- function(NPIGS = 0, AF = 0, 
                           Ckg = 0, Wkg = 0, AFkg = 0, AMkg = 0, M2Skg = 0, 
                           DR1 = 0, DRR2A = 0, DRR2B = 0, DRF2 = 0, FR = 0, FRRF = 0.95, RRF = 0, RRM = 0,
                           WA = 0, MFR = 0, DWG2 = 0, LITSIZE = 0){
  
##Female  
  
  AFin = AF * (RRF / 100)
  AFx = AF * (DRR2B / 100)
  AFexit = AF * (RRF / 100) - AFx
  Cin = AF * ((1 - (DRR2B / 100)) * FR * LITSIZE + (RRF / 100) * LITSIZE) * (1 - (DR1 / 100))
  
  DWGF = AFkg / ((AFkg + AMkg) / 2) * DWG2
  
  AFCF = (AFkg - Wkg) / (365 * DWGF) + (WA / 365)
  
  RFin = ((AF * (RRF / 100)) / FRRF) / (1 - (DRR2A / 100))^AFCF
  RFexit = ((AF * (RRF / 100)) / FRRF) - AFin
  RFx = RFin - (AFin + RFexit)
  RF = (RFin + AFin) / 2 * ((AFkg - Wkg) / (365 * DWGF) + (WA / 365))
  MFin = Cin / 2 - RFin
  
#Male
  
  AM = AF * MFR
  AMx = AM * (DRR2B / 100)
  
  DWGM = AMkg / ((AFkg + AMkg) / 2) * DWG2
  
  AFCM = (AMkg - Wkg) / (365 * DWGM) + (WA / 365)
 
  AMexit = AM * RRM / 100 - AMx
  AMin = AM * RRM / 100
  RMin = AMin / (1 - (DRR2A / 100))^AFCM
  RMx = RMin - AMin
  RM = (RMin + AMin) / 2 * ((AMkg - Wkg) / (365 * DWGM) + (WA / 365))
  MMin = Cin / 2 - RMin
  
##Fattening
  
  M2in = MFin + MMin
  
  A2S = (M2Skg - Wkg) / (365 * DWG2)
  
  M2exit = M2in * (1 - (DRF2 / 100))^A2S
  M2x = M2in - M2exit
  M2 = (M2in + M2exit) / 2 * ((M2Skg - Wkg) / (365 * DWG2))
  
##Average weights
  
  RFkg = (AFkg - Wkg) / 2 + Wkg
  RMkg = (AMkg - Wkg) / 2 + Wkg
  M2kg = (M2Skg - Wkg) /2 + Wkg
  
  
  my_list <- list("AF" = AF, "AM" = AM,
                  "RF" = RF, "RM" = RM,
                  "M2" = M2, 
                  
                  "DWGF" = DWGF, "DWGM" = DWGM, 
                  
                  "RFkg" = RFkg, "RMkg" = RMkg,
                  "M2kg" = M2kg,
                  
                  "AFCF" = AFCF, "AFCM" = AFCM,
                  "A2S" = A2S)
  
  return(my_list)
  
  
  
}


herd_chickens_backyard <- function(AFC = 0, NCHK = 0, AF = 0,
                                    Ckg = 0, AF2kg = 0, AM2kg = 0, M2Skg = 0, 
                                    DR1 = 0, FRRF = 0.95, DR2 = 0,
                                    MFR = 0, EGGSyear = 0,  EGGwght = 0, HATCH = 0, 
                                    AFS = 0, CYCLE = 0, CLTSIZE = 0){
  
  ##Reproductive female
  
  RRF = 365 / (AFS - AFC)
  
  AFin = AF * RRF
  AFx = AF * (DR2 / 100)
  AFexit = AF * RRF - AFx
  
  EGGSrepro = CYCLE * CLTSIZE
  
  if(EGGSrepro > EGGSyear){EGGSrepro = EGGSyear}
  
  EGGconsAF = EGGSyear - EGGSrepro
  
  
  Cin = (AF * (1 - (DR2 / 100)) * EGGSrepro) * HATCH
  RFin = ((AF * RRF) / FRRF) / (1 - (DR1 / 100))
  RFexit = ((AF * RRF) / FRRF) - AFin
  RFx = RFin - (AFin + RFexit)
  RF = (RFin + AFin) / 2 * (AFC / 365)
  MF1in = Cin / 2 - RFin
  
  
  ##Reproductive Males
  
  AM = AF * MFR
  
  RRM = RRF
  
  AMx = AM * (DR2 / 100)
  AMexit = AM * RRM - AMx
  AMin = AM * RRM
  RMin = AMin / (1 - (DR1 / 100))
  RMx = RMin - AMin
  RM = (RMin + AMin) / 2 * (AFC / 365)
  MMin = Cin / 2 - RMin
  
  
  ##Male fattening
  
  
  MMexit = MMin * (1 - (DR1 / 100))
  MMx = MMin - MMexit
  MM = ((MMin + MMexit) / 2) * (AFC / 365)
  
  
  ##Female fattening and egg production 
  
  MF1x = MF1in * (DR1 / 100)
  MF1exit = (MF1in - MF1x) * (1 - FRRF)
  MF2in = (MF1in - MF1x) * FRRF
  MF1 = ((MF1in + MF2in) / 2) * (AFC / 365)
 
  MF2exit = MF2in * (1 - (DR2 / 100))^((AFS - AFC) / 365)
  MF2x = MF2in - MF2exit
  MF2 = ((MF2in + MF2exit) / 2) * ((AFS - AFC) / 365)
  
  EGGconsMF = EGGSyear
  
  ##average characteristics
  AF1kg = M2Skg * (AF2kg / ((AF2kg + AM2kg) / 2))
  AM1kg = M2Skg * (AM2kg / ((AF2kg + AM2kg) / 2))
  MF1Skg = AF1kg
  MF2Skg = AF2kg
  MMSkg = M2Skg * (AM2kg / ((AF2kg + AM2kg) / 2))
  RFkg = (AF1kg - Ckg) / 2 + Ckg
  RMkg = (AM1kg - Ckg) / 2 + Ckg
  AFkg = (AF2kg - AF1kg) / 2 + AF1kg
  AMkg = (AM2kg - AM1kg) /2 + AM1kg
  MF1kg = RFkg
  MF2kg = AFkg
  MMkg = (MMSkg - Ckg) / 2 + Ckg
 
  DWGF1 = (AF1kg - Ckg) / AFC
  DWGF2 = (AF2kg - AF1kg) / (AFS - AFC)
  DWGM1 = (AM1kg - Ckg) / AFC
  DWGM2 = (AM2kg - AM1kg) / (AFS - AFC)
  
  
  my_list <- list("AF" = AF, "AM" = AM,
                  "RF" = RF, "RM" = RM,
                  "MF1" = MF1, "MF2" = MF2,
                  "MM" = MM,
                  
                  "DWGF1" = DWGF1, "DWGF2" = DWGF2, 
                  "DWGM1" = DWGM1, "DWGM2" = DWGM2,
                  
                  "AF1kg" = AF1kg, "AM1kg" = AM1kg,
                  "AFkg" = AFkg, "AMkg" = AMkg,
                  "RFkg" = RFkg, "RMkg" = RMkg,
                  "MF1kg" = MF1kg, "MF2kg" = MF2kg,
                  "MMkg" = MMkg, 
                  "MMSkg" = MMSkg, "EGGconsAF" = EGGconsAF)
  
  return(my_list)
}

herd_chickens_layers <- function(AFC = 0, NCHK = 0, AF = 0, 
                                   Ckg = 0, AF1kg = 0, AF2kg = 0, 
                                   DR1 = 0, FRRF = 0.95, DRL2 = 0, DRM = 0,
                                   MFR = 0, EGGSyear = 0, EGGwght = 0, HATCH = 0, 
                                   LAY1weeks = 0, LAY2weeks = 30, MOLTweeks = 6){
  
  ##Laytime
  
  if(MOLTweeks == 0){LAYtime = LAY1weeks / 52}else{LAYtime = (LAY1weeks + LAY2weeks + MOLTweeks) / 52}
  
  
  # Reproductive female section
  AFin = AF / LAYtime
  AFx = AF * ((52 * DRL2 / LAY1weeks) / 100)
  AFexit = AF / LAYtime - AFx
  Cin = AF * (1 - (DRL2 / 100)) * EGGSyear * HATCH
  RFin = ((AF / LAYtime) / FRRF) / (1 - (DR1 / 100))
  RFexit = ((AF / LAYtime) / FRRF) - AFin
  RFx = RFin - (AFin + RFexit)
  RF = (RFin + AFin) / 2 * (AFC / 365)
  MF1in = Cin / 2 - RFin
 
  # Male reproduction section
  AM = AF * MFR
  AMx = AM * ((52 * DRL2 / LAY1weeks) / 100)
  AMexit = AM / LAYtime - AMx
  AMin = AM / LAYtime
  RMin = AMin / (1 - (DR1 / 100))
  RMx = RMin - AMin
  RM = (RMin + AMin) / 2 * (AFC / 365)
  MMin = Cin / 2 - RMin

  # Laying section
  # Growing period
  MF2in = MF1in * (1 - (DR1 / 100))
  MF1x = MF1in - MF2in
  MF1 = ((MF1in + MF2in) / 2) * (AFC / 365)
  
  #Laying period
  
  MF2exit = MF2in * (1 - (DRL2 / 100))
  MF2x = MF2in - MF2exit
  MF2 = ((MF2in + MF2exit) / 2) * (LAY1weeks / 52)
  
  if(MOLTweeks == 0){
    MF4exit = MF2exit
    MF3 = 0
    MF4 = 0}else{
                  MF3exit = MF2exit * (1 - (DRM / 100))
                  MF3x = MF2exit - MF3exit
                  MF3 = ((MF2exit + MF3exit) / 2) * (MOLTweeks / 52)
                  MF4exit = MF3exit * (1 - (DRL2 / 100))
                  MF4x = MF3exit - MF4exit
                  MF4 = ((MF3exit + MF4exit) / 2) * (LAY2weeks / 52)}


  # Male meat production section

MMexit = MMin * (1 - (DR1 / 100))
MMx = MMin - MMexit
MM = ((MMin + MMexit) / 2) * (AFC / 52)

 # Average weight and growth rates
MF1kg = AF1kg ##Values swapped from GLEAM documentation in Section 2.4.1 as AF1kg is input variable
MF2kg = AF2kg ##Values swapped from GLEAM documentation in Section 2.4.1 as AF1kg is input variable
AM1kg = 1.3 * MF1kg
AM2kg = 1.3 * MF2kg
MM1kg = 1.3 * MF1kg
MF11kg = (MF1kg - Ckg) / 2 + Ckg
RFkg = MF11kg
MF22kg = (MF2kg - MF1kg) / 2 + MF1kg
AFkg = MF22kg
AMkg = (AM2kg - AM1kg) / 2 + AM1kg
RMkg = (AM1kg - Ckg) / 2 + Ckg
MMkg = (MM1kg - Ckg) / 2 + Ckg
DWGF1 = (MF1kg - Ckg) / (365 * AFC)
DWGF2 = (MF2kg - MF1kg) / (7 * LAY1weeks)
DWGF3 = 0
DWGF4 = 0
DWGM1 = (AM1kg - Ckg) / (365 * AFC)
DWGM2 = (AM2kg - AM1kg) / (365 * (LAY1weeks / 52))
  

my_list <- list("AF" = AF, "AM" = AM,
                "RF" = RF, "RM" = RM,
                "MF1" = MF1, "MF2" = MF2,
                "MF3" = MF3, "MF4" = MF4,
                "MM" = MM,
                
                "DWGF1" = DWGF1, "DWGF2" = DWGF2, 
                "DWGM1" = DWGM1, "DWGM2" = DWGM2,
                
                "AF1kg" = AF1kg, "AM1kg" = AM1kg,
                "AF2kg" = AF2kg, "AM2kg" = AM2kg,
                "AFkg" = AFkg, "AMkg" = AMkg,
                "RFkg" = RFkg, "RMkg" = RMkg,
                "MF1kg" = MF1kg, "MF2kg" = MF2kg,
                "MF11kg" = MF11kg, "MF22kg" = MF22kg,
                "MMkg" = MMkg)

return(my_list)



}

herd_chickens_broilers <- function(AFC = 0, NCHK = 0, AF = 0, 
                                   Ckg = 0, AF1kg = 0, AF2kg = 0, M2Skg = 0, 
                                   DR1 = 0, FRRF = 0.95, DRB2 = 0, DRL2 = 0,
                                   MFR = 0, EGGSyear = 0, EGGwght = 0, HATCH = 0, 
                                   A2S = 0, BIDLE = 14, LAYweeks = 0){
  
  # Reproductive female section
  AFin = AF / (LAYweeks / 52)
  AFx = AF * (((52 * DRL2 / LAYweeks)) / 100)
  AFexit = AF * FRRF - AFx ##RRF replaced with FRRF compared to GLEAM documentation as RRF not listed as input variable for broilers
  Cin = AF * (1 - (DRL2 / 100)) * EGGSyear * HATCH
  RFin = ((AF / (LAYweeks / 52)) / FRRF) / (1 - (DR1 / 100))
  RFexit = ((AF / (LAYweeks / 52)) / FRRF) - AFin
  RFx = RFin - (AFin + RFexit)
  RF = ((RFin + AFin) / 2) * (AFC / 365)
  MFin = Cin / 2 - RFin
  
  # Male reproduction section
  AM = AF * MFR
  AMx = AM * ((52 * DRL2 / LAYweeks) / 100)
  AMexit = AM / (LAYweeks / 52) - AMx
  AMin = AM / (LAYweeks / 52)
  RMin = AMin / (1 - (DR1 / 100))
  RMx = RMin - AMin
  RM = ((RMin + AMin) / 2) * (AFC / 365)
  MMin = Cin / 2 - RMin
 
  # Broilers section
  M2in = MFin + MMin
  M2exit = M2in * (1 - (DRB2 / 100))
  M2x = M2in - M2exit
  M2 = ((M2in + M2exit) / 2) * (A2S + (BIDLE / 365))
  
  # Average weight and growth rates
  AFkg = (AF2kg + AF1kg) / 2
  RFkg = (AF1kg - Ckg) / 2 + Ckg
  AM1kg = 1.3 * AF1kg
  AM2kg = 1.3 * AF2kg
  AMkg = 1.3 * AFkg
  RMkg = (AM1kg - Ckg) / 2 + Ckg
  M2kg = (M2Skg - Ckg) / 2 + Ckg
  
  DWGF0 = (AF1kg - Ckg) / (365 * AFC)
  DWGM0 = (AM1kg - Ckg) / (365 * AFC)
 
  DWG2B = (M2Skg - Ckg) / (365 * A2S)
  
  
  
  my_list <- list("AF" = AF, "AM" = AM,
                  "RF" = RF, "RM" = RM,
                  "M2" = M2,
                  
                  "DWGF0" = DWGF0, "DWGM0" = DWGM0, 
                  "DWG2B" = DWG2B,
                  
                  "AM1kg" = AM1kg, "AM2kg" = AM2kg, 
                  "RFkg" = RFkg, "RMkg" = RMkg,
                  "AFkg" = AFkg, "AMkg" = AMkg,
                  "M2kg" = M2kg)
  
  return(my_list)
  
  
  
  
}



##FEED DIGESTIBILITY
##Calculate digestibility of feed for each animal category for dairy and beef
##Output should be FEED DI and FEED GE for each animal category
feed_categories <- c("GRASS", "GRASSH", "GRASSH2", "GRASSLEGF", "GRASSLEGH", 
                     "ALFALFAH", "GRAINSIL", "MAIZESIL", "RSTRAW", "WSTRAW", 
                     "BSTRAW", "ZSTOVER", "MSTOVER", "SSTOVER", "TOPS", 
                     "LEAVES", "FDDRBEET", "GRAINS", "CORN", "MLSOY",
                     "MLRAPE", "MLCTTN", "PKEXP", "MZGLTM", "MZGLTF",
                     "BPULP", "MOLASSES", "GRNBYDRY", "GRNBYWET", "CONC")

feed_digestibility <- function(pct = 0, di = 0, ge = 0, n_cont = 0){ 
  
  feed_di <- sum(((pct/100) * di), na.rm = TRUE) #percentage average digestibility of the feed
  feed_ge <- sum(((pct/100) * ge), na.rm = TRUE) #average gross energy content of ration MJ kgDM-1
  feed_ncont <- sum(((pct/100) * n_cont), na.rm = TRUE)/1000 #kgN kgDM-1
  
  
  my_list <- list("di" = feed_di, "ge" = feed_ge, "ncont" = feed_ncont)
  return(my_list)
}



feed_digestibility_monogastrics <- function(pct = 0, di = 0, ge = 0, me= 0, n_cont = 0){ 
  
  feed_di <- sum(((pct/100) * di), na.rm = TRUE) #percentage average digestibility of the feed
  feed_ge <- sum(((pct/100) * ge), na.rm = TRUE) #average gross energy content of ration MJ kgDM-1
  feed_me <- sum(((pct/100) * me), na.rm = TRUE) #average metabolizable energy content of ration, MJ·kgDM-1
  feed_ncont <- sum(((pct/100) * n_cont), na.rm = TRUE)/1000 #kgN kgDM-1
  
  
  my_list <- list("di" = feed_di, "ge" = feed_ge, "me" = feed_me, "ncont" = feed_ncont)
  return(my_list)
}





##Energy Intake
energy_intake <- function(cohort = "AF", live_weight = 0, Cmain = 0, Cact = 0,
                          DWG = 0, Akg = 0, Mfkg = 0, DWGFX = 0, LWEND = 0, Cgro = 0,
                          milk_prod = 0, milk_fat_pct = 0, hour_work = 0, FR = 0, AFC = 0, 
                          ev_fibre = 0, prod_fibre =0, diet_di = 0, diet_ge = 0){
  
  ##NE maintenance (MJ head-1 day-1)  
  
  if(cohort == "RF" | cohort == "RM"  | cohort == "MF" | cohort == "MM" | cohort == "MFf" | cohort == "MMf"){
    
    NE_main = 0.975 * Cmain * live_weight^0.75
    
  } else {
    
    NE_main = Cmain * live_weight^0.75
  }
  
  ##NE activity (MJ head-1 day-1)  
  
  NE_act <- NE_main * Cact
  
  
  ##NE grow (MJ head-1 day-1)    
  if(cohort == "AF" | cohort == "AM"){
    NE_grow = 0
  } else if(cohort == "RF" | cohort == "MF" | cohort == "RM" | cohort == "MM"){
    NE_grow = 22.02 * ((live_weight /(Cgro*Akg))^0.75)* DWG^1.097
  } else if(cohort == "MFf" | cohort == "MMf"){
    NE_grow = 22.02 * ((Mfkg /(Cgro*LWEND))^0.75)* DWGFX^1.097
  }  
  
  ##NE milk (MJ head-1 day-1)    
  if(cohort == "AF"){
    NE_milk = milk_prod * (1.47+0.4*milk_fat_pct) ##Milk prod units kg milk cow-1 day-1
  } else {
    NE_milk = 0 
  }  
  
  ##NE work (MJ head-1 day-1)  
  NE_work <- 0.1 * NE_main * hour_work
  
  ##NE fibre (MJ head-1 day-1)  
  NE_fibre <- ev_fibre * prod_fibre    
  
  ##NE pregnancy (MJ head-1 day-1)  
  if(cohort == "AF"){
    NE_preg = NE_main * 0.1 * FR/100
  } else if(cohort == "RF"){
    NE_preg = NE_main * 0.1 / (AFC/2)
  } else{
    NE_preg = 0
  }
  
  
  ##REM (ratio of net energy in the feed intake for maintenance to digestible energy)
  rem <-  1.123 - (4.092E-3 * diet_di) + (1.126E-5 * diet_di^2) - (25.4 / diet_di)
  
  
  ##REG (Ratio of net energy available in the feed intake for growth to digestible energy consumed)
  reg <-  1.164 - (5.16E-3 * diet_di) + (1.3086E-5 * diet_di^2) - (37.4 / diet_di)
  
  
  ##Total Gross Energy (MJ head-1 day-1)
  
  GE = (((NE_main + NE_act + NE_milk + NE_work + NE_preg) / rem) + ((NE_grow + NE_fibre) / reg)) / (diet_di / 100) 
  
  ##DMI (dry matter intake (kg DM head-1 day-1) 
  
  DMI = GE / diet_ge
  
  my_list <- list("DMI" = DMI, "NE_grow" = NE_grow)
  
  return(my_list)
  
}




energy_intake_small_ruminants<- function(cohort = "AF", live_weight = 0, Cmain = 0, Cact = 0,
                                          DWG = 0, Ckg = 0, milk_prod = 0, ev_milk = 4.6, ev_fibre = 24, prod_fibre =0,
                                          LITSIZE = 0, FR = 0, LINT = 0,  diet_di = 0, diet_ge = 0){
  
  ##NE maintenance (MJ head-1 day-1)  
  
  NE_main = Cmain * live_weight^0.75
  
  ##NE activity (MJ head-1 day-1)  
  
  NE_act <- live_weight * Cact
  
  
  ##NE grow (MJ head-1 day-1)    
  if(cohort == "AF" | cohort == "AM"){
    NE_grow = 0
  } else if(cohort == "RF" | cohort == "RFA" | cohort == "RFB" | cohort == "MF"){
    NE_grow = DWG * (2.1 + 0.45 * Ckg)+0.5*0.45*(DWG^2)
  } else if(cohort == "RM" | cohort == "RMA" | cohort == "RMB" | cohort == "MM"){
    NE_grow = DWG * (2.5 + 0.35 * Ckg)+0.5*0.35*(DWG^2)
  }  
  
  ##NE milk (MJ head-1 day-1)    
  if(cohort == "AF"){
    NE_milk = milk_prod * ev_milk ##Milk prod units kg milk cow-1 day-1; net energy to produce 1 kg milk (MJ kg mik-1), assume 4.6
  } else {
    NE_milk = 0 
  }  
  
  ##NE work (MJ head-1 day-1)  
  NE_work <- 0
  
  ##NE fibre (MJ head-1 day-1)
  if(cohort == "AF" | cohort == "AM"| cohort == "MF" |cohort == "MM" ){
  NE_fibre <- ev_fibre * prod_fibre 
  } else{
    NE_fibre = 0
  }
  
  
  ##NE pregnancy (MJ head-1 day-1)  
  if(cohort == "AF"){
    NE_preg = NE_main * (0.077 * (2 - LITSIZE) + 0.126 * (LITSIZE - 1)) * (365 * FR / LINT/100)
  } else if(cohort == "RFB"){
    NE_preg = NE_main * 0.077
  } else{
    NE_preg = 0
  }
  
  
  ##REM (ratio of net energy in the feed intake for maintenance to digestible energy)
  rem <-  1.123 - (4.092E-3 * diet_di) + (1.126E-5 * diet_di^2) - (25.4 / diet_di)
  
  
  ##REG (Ratio of net energy available in the feed intake for growth to digestible energy consumed)
  reg <-  1.164 - (5.16E-3 * diet_di) + (1.3086E-5 * diet_di^2) - (37.4 / diet_di)
  
  
  ##Total Gross Energy (MJ head-1 day-1)
  
  GE = (((NE_main + NE_act + NE_milk + NE_work + NE_preg) / rem) + ((NE_grow + NE_fibre) / reg)) / (diet_di / 100) 
  
  ##DMI (dry matter intake (kg DM head-1 day-1) 
  
  DMI = GE / diet_ge
  
  my_list <- list("DMI" = DMI, "NE_grow" = NE_grow)
  
  return(my_list)
  
}



energy_intake_pigs<- function(cohort = "AF", live_weight = 0, Cmain = 0.444, Cact = 0, Ckg = 0, AFkg = 0, Wkg = 0,
                              Cgest = 0.148, LITSIZE = 0, AFCF = 0, DR1 = 0, Clact = 20.59, 
                              Cwloss = 0.38, Cconv = 0.67,  diet_me = 0,
                              DWG = 0, PTissue = 0, Prot = 0.23, CMEprot = 54,
                              Fat = 0.9, CMEfat = 52.3, idle = 0, lact = 0, gest = 0){
  
  ##ME maintenance (MJ head-1 day-1)  
  live_weight_gestation = live_weight + (LITSIZE * Ckg + 0.15 * AFkg) / 2
  live_weight_lactation = live_weight + (0.15 * AFkg) / 2
  
  ME_main = Cmain * (live_weight^0.75) * Cact 
  
  ME_main_gestation = Cmain * (live_weight_gestation^0.75) * Cact
  ME_main_lactation = Cmain * (live_weight_lactation^0.75) * Cact
  
  
  ##NE Gestation (MJ head-1 day-1)
  if(cohort == "AF"){
    Cadj = 1
    ME_preg = Cgest * LITSIZE * Cadj
  } else if(cohort == "RF"){
    Cadj = 1 / AFCF
    ME_preg = Cgest * LITSIZE * Cadj
  } else{
    ME_preg = 0
  }
  
  ##ME Lactation
  if(cohort == "AF"){
    Cadj = 1
    ME_lact = LITSIZE *( (1 - 0.5 * (DR1 / 100)) * (Clact * (Wkg - Ckg) / lact) - (Cwloss / Cconv)) * Cadj ##Taken out  * 1000. In Gleam Clact is in units MJ g liveweight-1 but assume this is a mistake and should be MJ kg liveweight-1
  } else if(cohort == "RF"){
    Cadj = 1 / AFCF
    ME_lact = LITSIZE *( (1 - 0.5 * (DR1 / 100)) * (Clact * (Wkg - Ckg) / lact) - (Cwloss / Cconv)) * Cadj ##Taken out  * 1000. In Gleam Clact is in units MJ g liveweight-1 but assume this is a mistake and should be MJ kg liveweight-1
  } else{
    ME_lact = 0
  }
  
  ##ME grow (MJ head-1 day-1)    
  if(cohort == "AF" | cohort == "AM"){
    ME_prot = 0
    ME_fat  = 0
  } else {
    ME_prot = DWG * PTissue * Prot * CMEprot
    ME_fat  = DWG * (1 - PTissue) * Fat * CMEfat
  }  
  
  
  
  ##ME total (MJ head-1 day-1)    
  if(cohort == "AF"){
    ME_tot = (gest * (ME_main_gestation + ME_preg) + lact * (ME_main_lactation + ME_lact) + idle * (ME_main))/sum(gest,lact,idle)##Divided by total number of days to get MJ head-1 day-1 (not in GLEAM equations)
  } else if(cohort == "AM") {
    ME_tot = ME_main
  } else if(cohort == "RF") {
    ME_tot = ((gest * ME_preg) + (lact * ME_lact) + 365 * AFCF * (ME_main + ME_prot + ME_fat))/sum(gest,lact,365*AFCF)##Divided by total number of days to get MJ head-1 day-1 (not in GLEAM equations)
  }else if(cohort == "RM" | cohort == "M2"){
    ME_tot = ME_main + ME_prot + ME_fat
    
  }
  
  
  
  ##DMI (dry matter intake (kg DM head-1 day-1) 
  
  DMI = ME_tot / diet_me
  
  my_list <- list("DMI" = DMI)
  
  return(my_list)
  
}


energy_intake_chickens<- function(cohort = "AF", mgmt = "backyard", temp = 0, live_weight = 0, 
                                  feathering_score = 1, Cact = 0, DWG = 0, Cgro = 0, EGG = 0, Cegg = 0, diet_me = 0){
  
  ##ME maintenance (MJ head-1 day-1)  
   LCT = 24.54 - 5.65 * feathering_score
  
  if(mgmt == "backyard"){
    if(cohort == "AF" | cohort == "AM" |cohort == "MF2"){
    TEMPreg = 0.693 - 0.0099 * temp} else {
      if(temp < LCT){
    TEMPreg = 0.386 + 0.03 * (LCT - temp)}else{
      TEMPreg = 0.386 + 0.0037 * (temp - LCT)  
    }}}else if(mgmt == "layers"){
      if(cohort == "AF" | cohort == "AM" | cohort == "MF2" | cohort == "MF3" | cohort == "MF4"){
        TEMPreg = 0.693 - 0.0099 * temp}else {
          TEMPreg = 0.390
        }}else if(mgmt == "broilers"){
          if(cohort == "AF" | cohort == "AM"){
            TEMPreg = 0.806 - 0.026 * temp + 0.0005 * (temp^2)} else if(cohort == "RF" | cohort == "RM"){
              TEMPreg = 0.727 - 0.00786 * temp} else{
                TEMPreg = 1.287 - 0.065 * temp + 0.0013 * (temp^2)
          }}
 
  
  
  ME_main = (live_weight^0.75) * TEMPreg * Cact
  
  
  ##ME grow
  ME_grow = DWG * 1000 * Cgro
  
  
  ##ME Egg
  if(mgmt == "backyard"){
    if(cohort == "AF" | cohort == "MF2"){
      ME_egg = 0.001 * EGG * Cegg} else {
        ME_egg = 0
        }}else if(mgmt == "layers"){
            if(cohort == "AF" | cohort == "AM" | cohort == "MF2" | cohort == "MF3" | cohort == "MF4"){
              ME_egg = 0.001 * EGG * Cegg} else {
                ME_egg = 0
              }}else if(mgmt == "broilers"){
                if(cohort == "AF"){
                  ME_egg = 0.001 * EGG * Cegg} else {
                    ME_egg = 0}}
  
  
  ME_tot = ME_main + ME_grow + ME_egg
  
  ##DMI (dry matter intake (kg DM head-1 day-1) 
  
  DMI = ME_tot / diet_me
  
  my_list <- list("DMI" = DMI)
  
  return(my_list)
  
}









##Enteric methane emissions
enteric_methane <- function(species = "cattle", cohort = "AF", number_cows = 0, diet_di = 0, diet_ge = 0, dmi = 0){
  
  if(species == "cattle"){
  
  
  if(cohort == "MFf" | cohort == "MMf"){
    Ym = 3
  } else {
    Ym <- 9.75 - 0.05* diet_di
  }
  
  }
   
  
  if(species == "sheep" | species == "goat"){
    
    
    if(cohort == "AF" | cohort == "AM"){
      Ym <- 9.75 - 0.05* diet_di
    } else {
      Ym <- 7.75 - 0.05* diet_di
    }
    
  }
  
  
  if(species == "pig"){
    
    
    if(cohort == "AF" | cohort == "AM"){
      Ym <- 1.01
    } else {
      Ym <- 0.39
    }
    
  }
    
  if(species == "chicken"){
    
    Ym = 0
    
  }
  
  
  enteric_methane_emis <- number_cows * 365 * diet_ge * dmi * (Ym/100)/55.65 ##Kg CH4 y-1
  
  
  
  return(enteric_methane_emis)
  
  
  
  
}


##FEED REQUIREMENTS
gross_dm <- function(cohort = "AF", pct = 0, fue = 0, mfa = 0,  dmi = 0, number_cows = 0){
  
  ##dry matter intake of different feed categories
  feed_dmi <- dmi * pct/100 ## kg DM head-1 day-1
  total_feed_dmi = feed_dmi * number_cows * 365
  
  gross_dm = total_feed_dmi/(fue * mfa)
  
  return(gross_dm)
}


##MANURE
manure_management_systems <- c("pasture", "daily_spread", "solid_storage", "dry_lot", "liquid_slurry", "liquid_slurry_crust", 
                               "anaerobic_lagoon", "pitstorage_less1month", "pitstorage_greater1month", "anaerobic_digester", "composting", "burned_fuel")


manure_methane <- function(species = "cattle", cohort = "AF", number_cows = 0, dmi = 0, diet_di = 0, diet_ge = 0, diet_me = 0, Bo = 0, pct = 0, mcf = 0){
  
  ##VS (daily volatile solid excreted by animal (kg VS head -1 day -1
 
  if(species == "cattle" | species == "sheep" | species == "goat"){
  
   vs_daily <- dmi * (1.04 - diet_di/100) * 0.92
  
  }
   
  if(species == "pig"){
    
    vs_daily <- dmi * (1.02 - diet_di/100) * 0.80
    
  }
  
  if(species == "chicken"){
    
    vs_daily <- dmi * (1.00 - diet_me/diet_ge) * 0.70
    
  }
  
  ##Overall MCF
  mcf_overall <- sum((pct/100)*(mcf/100), na.rm = TRUE)
  
  ##methane emissions manure  (kg CH4 y-1)
  manure_methane_emis <- number_cows * ((365*vs_daily)*(Bo*0.67*mcf_overall))
  
  return(manure_methane_emis)
}


##Nitrogen Excretion (from GLEAM Section 4.4) and N2O emissions

nitrogen_excretion <- function(species = "cattle", cohort = "AF", number_cows = 0, dmi = 0, diet_ncont = 0, 
                               milk_prod = 0, milk_protein = 0, Ckg = 0, net_energy_required = 0,
                               LITSIZE = 0, FR = 0, Wkg = 0, DWG = 0, AFCF = 0, NLW = 0.028, NEGG = 0.0185, EGG = 0){
  
  
  if(species == "cattle" | species == "sheep" | species == "goat"){
  
  if(cohort == "AF"){
    n_retention = ((milk_prod * (milk_protein/100))/6.38) + ((Ckg/365) * (268 - (7.03 * net_energy_required/DWG))* 0.001 / 6.25)
  } else if(cohort == "AM"){
    n_retention = 0
  } else {
    n_retention = (DWG * (268 - (7.03 * net_energy_required/DWG))* 0.001 / 6.25)
  } 
  
  }  
  
  if(species == "pig"){
    
    if(cohort == "AF"){
      n_retention = ((0.025 * LITSIZE * FR * (Wkg - Ckg) / 0.98) + (0.025 * LITSIZE * FR * Ckg)) / 365
    } else if(cohort == "AM"){
      n_retention = 0
    } else if(cohort == "RF"){
      n_retention = 0.025 * DWG * (1/AFCF) * (((0.025 * LITSIZE * FR * (Wkg - Ckg) / 0.98) + (0.025 * LITSIZE * FR * Ckg)) / 365)
    } else{
      n_retention = 0.025 * DWG
    }
    
  }  
  
  
  if(species == "chicken"){
    
    if(cohort == "AF" | cohort == "MF2" | cohort == "MF4"){
      n_retention = NLW * DWG + NEGG * 0.001 * EGG
    } else if(cohort == "MF3"){
      n_retention = 0
    } else {
      n_retention = NLW * DWG
    } 
    
  }   
  
    
  n_excretion = 365 * (dmi * diet_ncont) - n_retention ##kg N head-1 year-1
  
  n_excretion_total = number_cows * n_excretion ##kg N year-1
  
  return(n_excretion_total)

}




##NH3, N2O, NO losses at point of excretion - EMEP/EEA Tier 2 methods 3B Manure Management Page 25 (Step 1-Step 11) + GLEAM module for N2O adapted
manure_excretion_storage_emis <- function(cohort = "AF", number_cows = 0, n_excretion_total = 0, prop_graze = 0, prop_yard = 0, prop_house = 0, prop_TAN = 0, prop_slurry = 0,
                          ef_house_slurry = 0, ef_house_solid = 0, ef_yard = 0, straw = 0, N_straw = 0, frac_imm = 0,
                          prop_store_slurry = 0, prop_biogas = 0, prop_burnfuel = 0, prop_store_solid = 0, frac_min = 0.1, 
                          ef_storage_slurry_nh3 = 0, ef_storage_slurry_no = 0, ef_storage_slurry_n2 = 0,
                          ef_storage_solid_nh3 = 0, ef_storage_solid_no = 0, ef_storage_solid_n2 = 0,
                          pct_mms_solid = 0, pct_mms_slurry = 0, direct_ef_solid = 0, direct_ef_slurry = 0,
                          ef_vol = 0.01,
                          frac_leach_solid = 0, frac_leach_slurry = 0, ef_leach = 0.0075){
  
  
  N_graze <- n_excretion_total * prop_graze #Kg N y-1
  N_yard <- n_excretion_total * prop_yard #Kg N y-1
  N_house <- n_excretion_total * prop_house #Kg N y-1
  
  TAN_graze <- N_graze * prop_TAN #kg NH3-N y-1
  TAN_yard <- N_yard * prop_TAN #kg NH3-N y-1
  TAN_house <- N_house * prop_TAN #kg NH3-N y-1
  
  N_house_slurry <- N_house * prop_slurry / (1-prop_graze) #Kg N y-1 ##Adjust to calculate percentage of manure that is not deposited to pasture during grazing is handled as slurry
  N_house_solid <- N_house - N_house_slurry #Kg N y-1
  TAN_house_slurry <- TAN_house * prop_slurry / (1-prop_graze) #Kg N y-1 ##Adjust to calculate percentage of manure that is not deposited to pasture during grazing is handled as slurry
  TAN_house_solid <- TAN_house - TAN_house_slurry #Kg N y-1
  
  NH3_emis_house_slurry <- TAN_house_slurry * ef_house_slurry #kg NH3-N emitted y-1
  NH3_emis_house_solid <- TAN_house_solid * ef_house_solid #kg NH3-N emitted y-1
  NH3_emis_yard <- TAN_yard * ef_yard #kg NH3-N emitted y-1
  
  
  ##Subtract N immobilised in bedding from solid waste in housing
  bedding = number_cows * prop_house * straw ##straw kg anmial-1 year-1 (for full year, then adjusted based on proportion of time spent in housing)
  N_bedding = number_cows * prop_house * N_straw ##straw kg anmial-1 year-1 (for full year, then adjusted based on proportion of time spent in housing)

  N_house_solid_2 <- N_house_solid + N_bedding - NH3_emis_house_solid
  TAN_house_solid_2 <- TAN_house_solid - (NH3_emis_house_solid + (N_bedding * frac_imm))
  
  ##Calculate the amounts of total-N and TAN stored before application to land
  TAN_stored_slurry = ((TAN_house_slurry - NH3_emis_house_slurry) + (TAN_yard - NH3_emis_yard)) * prop_store_slurry #kg NH3 y-1
  N_stored_slurry = ((N_house_slurry - NH3_emis_house_slurry) + (N_yard - NH3_emis_yard)) * prop_store_slurry #kg NH3 y-1
  
  TAN_biogas_slurry = ((TAN_house_slurry - NH3_emis_house_slurry) + (TAN_yard - NH3_emis_yard)) * prop_biogas #kg NH3 y-1
  N_biogas_slurry = ((N_house_slurry - NH3_emis_house_slurry) + (N_yard - NH3_emis_yard)) * prop_biogas #kg NH3 y-1
  
  TAN_direct_slurry = ((TAN_house_slurry - NH3_emis_house_slurry) + (TAN_yard - NH3_emis_yard)) * (1-(prop_biogas+prop_store_slurry)) #kg NH3 y-1
  N_direct_slurry = ((N_house_slurry - NH3_emis_house_slurry) + (N_yard - NH3_emis_yard)) * (1-(prop_biogas+prop_store_slurry)) #kg NH3 y-1
  
  
  
  
  TAN_stored_solid = TAN_house_solid_2 * prop_store_solid
  N_stored_solid   = N_house_solid_2 * prop_store_solid
  
  TAN_biogas_solid = TAN_house_solid_2 * prop_biogas
  N_biogas_solid   = N_house_solid_2 * prop_biogas
  
  TAN_burnfuel_solid = TAN_house_solid_2 * prop_burnfuel
  N_burnfuel_solid   = N_house_solid_2 * prop_burnfuel
  
  TAN_direct_solid = TAN_house_solid_2 * (1-(prop_biogas+prop_store_solid+prop_burnfuel))
  N_direct_solid   = N_house_solid_2 * (1-(prop_biogas+prop_store_solid+prop_burnfuel))
  
  
  ##Calculate fraction of organic N mineralised and converted to TAN
  TAN_stored_slurry_2 <- TAN_stored_slurry + ((N_stored_slurry - TAN_stored_slurry)*frac_min) ###kg NH3 y-1
  
  NH3_storage_slurry <- TAN_stored_slurry_2 * ef_storage_slurry_nh3
  NO_storage_slurry <- TAN_stored_slurry_2 * ef_storage_slurry_no
  N2_storage_slurry <- TAN_stored_slurry_2 * ef_storage_slurry_n2
  
  
  pct_mms_slurry_2 <- pct_mms_slurry / sum(pct_mms_slurry)
  n2o_direct_ef_slurry = sum((pct_mms_slurry_2)*(direct_ef_slurry), na.rm = TRUE)    
  n2o_direct_slurry = N_stored_slurry * n2o_direct_ef_slurry ##kg n2o-n y-1  
  
  n2o_vol_slurry = (NH3_storage_slurry + NO_storage_slurry + NH3_emis_house_slurry) * ef_vol    
  
  TAN_leach_slurry = sum((pct_mms_slurry_2 * TAN_stored_slurry_2 * frac_leach_slurry), na.rm = TRUE)
  N_leach_slurry = sum((pct_mms_slurry_2 * N_stored_slurry * frac_leach_slurry), na.rm = TRUE)
  
  n2o_leach_slurry = N_leach_slurry * ef_leach
  
  
  
  
  NH3_storage_solid <- TAN_stored_solid * ef_storage_solid_nh3
  NO_storage_solid <- TAN_stored_solid * ef_storage_solid_no
  N2_storage_solid <- TAN_stored_solid * ef_storage_solid_n2
 
  pct_mms_solid_2 <- pct_mms_solid / sum(pct_mms_solid)
  n2o_direct_ef_solid = sum(((pct_mms_solid_2)*(direct_ef_solid)), na.rm = TRUE)    
  n2o_direct_solid = N_stored_solid * n2o_direct_ef_solid ##kg n2o-n y-1  
  
  n2o_vol_solid = (NH3_storage_solid + NO_storage_solid + NH3_emis_house_solid + NH3_emis_yard) * ef_vol    
  
  TAN_leach_solid = sum((pct_mms_solid_2 * TAN_stored_solid * frac_leach_solid), na.rm = TRUE)
  N_leach_solid = sum((pct_mms_solid_2 *N_stored_solid * frac_leach_solid), na.rm = TRUE)
  
  n2o_leach_solid = N_leach_solid * ef_leach
  
  
  
  
  ##Calculate N-manure applied to fields
  TAN_applic_slurry <- TAN_direct_slurry + TAN_stored_slurry_2 - NH3_storage_slurry - NO_storage_slurry - N2_storage_slurry - n2o_direct_slurry - TAN_leach_slurry  ##Add in digestate N applied to fields from anaerobic digesters
  N_applic_slurry <- N_direct_slurry + N_stored_slurry - NH3_storage_slurry - NO_storage_slurry - N2_storage_slurry - n2o_direct_slurry - N_leach_slurry ##Add in digestate N applied to fields from anaerobic digesters
  
  TAN_applic_solid <- TAN_direct_solid + TAN_stored_solid - NH3_storage_solid - NO_storage_solid - N2_storage_solid - n2o_direct_solid - TAN_leach_solid 
  N_applic_solid <- N_direct_solid + N_stored_solid - NH3_storage_solid - NO_storage_solid - N2_storage_solid - n2o_direct_solid - N_leach_solid 
  
  
  
  summary <- list("NH3_emis_house_slurry" = NH3_emis_house_slurry, "NH3_emis_house_solid" = NH3_emis_house_solid, "NH3_emis_yard" = NH3_emis_yard,
                  "TAN_biogas_slurry" = TAN_biogas_slurry, "TAN_biogas_solid" = TAN_biogas_solid, 
                  "N_biogas_slurry" = TAN_biogas_slurry, "N_biogas_solid" = TAN_biogas_solid,
                  
                  "TAN_burnfuel_solid" = TAN_biogas_slurry, "N_burnfuel_solid" = N_burnfuel_solid,
                  
                  "NH3_storage_slurry" = NH3_storage_slurry, "NH3_storage_solid" = NH3_storage_solid, 
                  "NO_storage_slurry" = NO_storage_slurry, "NO_storage_solid" = NO_storage_solid,
                  "N2_storage_slurry" = NH3_storage_slurry, "N2_storage_solid" = NH3_storage_solid,
                  "n2o_direct_slurry" = n2o_direct_slurry, "n2o_direct_solid" = n2o_direct_solid,
                  "n2o_vol_slurry" = n2o_vol_slurry, "n2o_vol_solid" = n2o_vol_solid,
                  "n2o_leach_slurry" = n2o_leach_slurry, "n2o_leach_solid" = n2o_leach_solid,
                  
                  "TAN_leach_slurry" = TAN_leach_slurry, "TAN_leach_solid" = TAN_leach_solid,
                  "N_leach_slurry" = N_leach_slurry, "N_leach_solid" = N_leach_solid,
                  
                  "TAN_applic_slurry" = TAN_applic_slurry, "TAN_applic_solid" = TAN_applic_solid,
                  "N_applic_slurry" = N_applic_slurry, "N_applic_solid" = N_applic_solid,
                  
                  "TAN_graze" = TAN_graze, "N_graze" = N_graze)
  
  
  return(summary)  
  
}




##Domestic crop production
domestic_crop_production <- function(domestic_consumption = 0, loss = 0,  processing = 0, nonfood = 0, 
                                 stock_var = 0, export = 0, import = 0, feed = 0, seed = 0){
  
  domestic_crop_production = sum(c(feed, seed, domestic_consumption, loss, processing, nonfood, stock_var, export), na.rm = TRUE) - import ##tonnes 

  return(domestic_crop_production)
  
}



manure_N_application <- function(manure_N_available_slurry = 0,  manure_TAN_available_slurry = 0, manure_N_available_solid = 0,  manure_TAN_available_solid = 0,  manure_N_available_graze = 0, manure_TAN_available_graze = 0, fraction_cropland_slurry = 0, fraction_cropland_solid = 0, fraction_cropland_graze = 0, cropland_ha = 0, grassland_ha = 0){
  
  manure_N_crops_slurry <- manure_N_available_slurry * fraction_cropland_slurry
  manure_N_grasslands_slurry <- manure_N_available_slurry * (1-fraction_cropland_slurry)
  
  manure_TAN_crops_slurry <- manure_TAN_available_slurry * fraction_cropland_slurry
  manure_TAN_grasslands_slurry <- manure_TAN_available_slurry * (1-fraction_cropland_slurry)
  
  manure_N_crops_solid <- manure_N_available_solid * fraction_cropland_solid
  manure_N_grasslands_solid <- manure_N_available_solid * (1-fraction_cropland_solid)
  
  manure_TAN_crops_solid <- manure_TAN_available_solid * fraction_cropland_solid
  manure_TAN_grasslands_solid <- manure_TAN_available_solid * (1-fraction_cropland_solid)
  
  manure_N_crops_graze <- manure_N_available_graze * fraction_cropland_graze
  manure_N_grasslands_graze <- manure_N_available_graze * (1-fraction_cropland_graze)
  
  manure_TAN_crops_graze <- manure_TAN_available_graze * fraction_cropland_graze
  manure_TAN_grasslands_graze <- manure_TAN_available_graze * (1-fraction_cropland_graze)
  
  
  manure_N_application_crops_slurry <- manure_N_crops_slurry / cropland_ha
  manure_N_application_crops_solid <- manure_N_crops_solid / cropland_ha
  manure_N_application_crops_graze <- manure_N_crops_graze / cropland_ha
  manure_TAN_application_crops_slurry <- manure_TAN_crops_slurry / cropland_ha
  manure_TAN_application_crops_solid <- manure_TAN_crops_solid / cropland_ha
  manure_TAN_application_crops_graze <- manure_TAN_crops_graze / cropland_ha
  
  manure_N_application_grasslands_slurry <- manure_N_grasslands_slurry / grassland_ha
  manure_N_application_grasslands_solid <- manure_N_grasslands_solid / grassland_ha
  manure_N_application_grasslands_graze <- manure_N_grasslands_graze / grassland_ha
  manure_TAN_application_grasslands_slurry <- manure_TAN_grasslands_slurry / grassland_ha
  manure_TAN_application_grasslands_solid <- manure_TAN_grasslands_solid / grassland_ha
  manure_TAN_application_grasslands_graze <- manure_TAN_grasslands_graze / grassland_ha
  
  manure_N_application_crops <- manure_N_application_crops_slurry + manure_N_application_crops_solid + manure_N_application_crops_graze
  manure_N_application_grasslands <- manure_N_application_grasslands_slurry + manure_N_application_grasslands_solid + manure_N_application_grasslands_graze
  
  manure_TAN_application_crops <- manure_TAN_application_crops_slurry + manure_TAN_application_crops_solid + manure_TAN_application_crops_graze
  manure_TAN_application_grasslands <- manure_TAN_application_grasslands_slurry + manure_TAN_application_grasslands_solid + manure_TAN_application_grasslands_graze
  
  my_list <- list("manure_N_application_crops_slurry" = manure_N_application_crops_slurry, 
                  "manure_N_application_crops_solid" = manure_N_application_crops_solid,
                  "manure_N_application_crops_graze" = manure_N_application_crops_graze,
                  "manure_N_application_crops" = manure_N_application_crops,
                  
                  "manure_TAN_application_crops_slurry" = manure_TAN_application_crops_slurry, 
                  "manure_TAN_application_crops_solid" = manure_TAN_application_crops_solid,
                  "manure_TAN_application_crops_graze" = manure_TAN_application_crops_graze,
                  "manure_TAN_application_crops" = manure_TAN_application_crops,
                  
                  
                  "manure_N_application_grasslands_slurry" = manure_N_application_grasslands_slurry, 
                  "manure_N_application_grasslands_solid" = manure_N_application_grasslands_solid,
                  "manure_N_application_grasslands_graze" = manure_N_application_grasslands_graze,
                  "manure_N_application_grasslands" = manure_N_application_grasslands,
                  
                  "manure_TAN_application_grasslands_slurry" = manure_TAN_application_grasslands_slurry, 
                  "manure_TAN_application_grasslands_solid" = manure_TAN_application_grasslands_solid,
                  "manure_TAN_application_grasslands_graze" = manure_TAN_application_grasslands_graze,
                  "manure_TAN_application_grasslands" = manure_TAN_application_grasslands)
  
  return(my_list) 
  
  
}





##Yield calculation kg crop ha-1 From Billen et al. 2014 and others
yield_feedcrop <- function(pctN_in_crop = 0, ymax = 0,  synthetic_fertiliser_N = 0, manure_N = 0, fixed_N = 0, deposition_N = 0, residue_N = 0, domestic_production = 0){
  
  N_inputs <-  synthetic_fertiliser_N + manure_N + fixed_N + deposition_N + residue_N
  
  N_yield <- ymax * (N_inputs / (N_inputs + ymax))
  
  yield <- N_yield / (pctN_in_crop/100) ##kg ha-1
  
  land_area <- domestic_production / yield #hectares
  
  
  N_surplus <- N_inputs - N_yield
  
  
 my_list <- list("yield" = yield, "land_area" = land_area, "N_surplus" = N_surplus)
  
  return(my_list)  
}



yield <- function(pctN_in_crop = 0, synthetic_fertiliser_N = 0, manure_N = 0, fixed_N = 0, deposition_N = 0, residue_N = 0, domestic_production = 0, land_area = 0){
  
  yield = domestic_production / land_area #kg ha-1
  
  N_yield = yield * pctN_in_crop #kg N ha-1
  
  N_inputs <-  synthetic_fertiliser_N + manure_N + fixed_N + deposition_N + residue_N

  N_surplus <- N_inputs - N_yield
  
  NUE = N_yield / N_inputs # fraction
  
  ymax <- (N_yield * N_inputs) * (N_inputs - N_yield)
  
  my_list <- list("yield" = yield, "land_area" = land_area, "N_surplus" = N_surplus, "NUE" = NUE, "ymax" = ymax)
  
  return(my_list)  
}








##Crop Residue Burning##Not needed, incorporated into below
crop_residue_burning <- function(production = 0, residue_crop_ratio = 0, dry_matter_fraction = 0, feed = 0, 
                                 fraction_oxidised = 0, ef = c(0,0,0,0,0,0,0,0,0,0)){
  
  residue_mass <- production * residue_crop_ratio * dry_matter_fraction #tonnes
  fraction_burned_in_field <- ((residue_mass - feed) / residue_mass)
  total_residue_burned <- residue_mass * fraction_burned_in_field * fraction_oxidised
  
  emissions = total_residue_burned * ef ##emissions in units kg as EF kg/tonne residue burned
  names(emissions) <- c("CO", "NOx", "SO2", "NMVOC", "NH3", "PM10", "PM2.5", "BC", "OC", "CH4")
  
  return(emissions)
  
  ##emission returns (and EF should be specified) in order CO, NOx, SO2, NMVOC, NH3, PM10, PM2.5, BC, OC, CH4
}




##NH3/NO synthetic fertiliser emissions
##EMEP/EEA 2019 Tier 2 method (Table 3.2 EF)
emis_manure_fertiliser_applied <- function(application_rate = 0, crop_area = 0, fertiliser_pct = 0, ef = 0){
  emissions <- application_rate * crop_area * fertiliser_pct/100 * ef ##kg NH3/NO emitted per year. EF units kg NH3/NO kg N-1 applied
  return(emissions)
}


##indirect N2O fertilisers emissions
##IPCC 2006 Tier 1 Table 11.3
indirect_n2o_leach_fertiliser <- function(application_rate = 0, crop_area = 0, fertiliser_pct = 0, frac = 0, ef = 0){ #frac refers to fraction leached and fraction volitised
  emissions <- application_rate * crop_area * fertiliser_pct/100 * frac * ef ##kg NH3 emitted per year. EF units kg N2O kg N-1 applied
  return(emissions)
}


##indirect N2O fertilisers emissions
##IPCC 2006 Tier 1 Table 11.3
indirect_n2o_vol_fertiliser <- function(NH3_N_emitted = 0, NO_N_emitted = 0, ef = 0){ #frac refers to fraction leached and fraction volitised
  emissions <- (NH3_N_emitted + NO_N_emitted) * ef ##EF units kg N2O kg N-1 emitted as NH3 and NO
  return(emissions)
}


##NH3 manure application emissions EMEP/EEA 2019 
emis_manure_application <- function(application_rate = 0, crop_area = 0, ef = 0){
  emissions <- application_rate * crop_area * ef ##kg NH3/NO emitted per year. EF units kg NH3-N kg TAN-1 applied
  return(emissions)
}






crop_residue<- function(production = 0, residue_crop_ratio = 0, dry_matter_fraction = 0, feed = 0, slope_ag = 0, intercept_ag =0, residue_ratio_agbg = 0, fuel_pct = 0, frac_burned = 0, 
                        fraction_oxidised = 0, ef = c(0,0,0,0,0,0,0,0,0,0), ncont_ag = 0, ncont_bg = 0, crop_area = 0){
  
  
  production_dm = production * dry_matter_fraction##tonnes
  residue_ag <- (production_dm * slope_ag) + intercept_ag##tonnes
  residue_bg <- residue_ag * residue_ratio_agbg ##Table 11.2 and Equation 11.6 provide methods and values IPCC 2006 Chapter 11
  
  residue_fuel <- (residue_ag * fuel_pct/100)##tonnes
  
  residue_remain_in_fields <- residue_ag - feed - residue_fuel##tonnes
  
  total_residue_burned <- residue_ag * frac_burned##fraction of total above ground residue
  
  
  residue_field_ag <- residue_remain_in_fields - total_residue_burned##tonnes
  residue_field_bg <- residue_bg##tonnes
  
  residue_field_ag_N <- residue_field_ag * ncont_ag##tonnes N
  residue_field_bg_N <- residue_field_bg * ncont_bg##tonnes N
  

  residue_N_input <- residue_field_ag_N + residue_field_bg_N##tonnes N
  residue_N_input_ha <- (residue_N_input * 1000) / crop_area ##kg N ha-1
  
  
  
  
  emissions = total_residue_burned * ef ##emissions in units kg as EF kg/tonne residue burned
  names(emissions) <- c("CO", "NOx", "SO2", "NMVOC", "NH3", "PM10", "PM2.5", "BC", "OC", "CH4")
  
  
  my_list <- list("emissions_burning" = emissions, "residue_N_input" = residue_N_input, "residue_N_input_ha" = residue_N_input_ha, "residue_fuel" = residue_fuel, "total_residue_burned" = total_residue_burned, "residue_ag" = residue_ag, "residue_bg" = residue_bg)
  
  return(my_list)  
  
  ##emission returns (and EF should be specified) in order CO, NOx, SO2, NMVOC, NH3, PM10, PM2.5, BC, OC, CH4
}




##methane rice
ch4_rice <- function(land_area = 0, irrigated_prop = 0, baseline_ef = 0, wrsf = 0, pre_sf = 0, cfoa =0 , roa = 0, cultivation_period = 0, number_gs = 0){
  
  sf_o <- (1 + sum(roa*cfoa, na.rm = TRUE))^0.59
  
  ef_adj_irr <- baseline_ef * wrsf[1] * pre_sf * sf_o ##wrsf, first value should be irrigated wrsf
  ef_adj_nonirr <- baseline_ef * wrsf[2] * pre_sf * sf_o ##wrsf, first value should be rainfed wrsf
  
  area_irrigated <- land_area * irrigated_prop * number_gs
  area_nonirrigated <- land_area * (1 - irrigated_prop) * number_gs
  
  ch4_irrigated <- ef_adj_irr * cultivation_period * area_irrigated ##kg ch4 y-1
  ch4_nonirrigated <- ef_adj_nonirr * cultivation_period * area_nonirrigated ##kg ch4 y-1
  
  ch4_emissions_rice <- sum(ch4_irrigated, ch4_nonirrigated)
  
  my_list <- list("ch4_emissions_rice" = ch4_emissions_rice)
  
  return(my_list)  
  
}



machinery_energy <- function(land_area = 0, fuel_consumption_per_hectare = 0, ef = c(0,0,0,0,0,0,0,0,0,0,0)){
  
  energy_consumption <- land_area * fuel_consumption_per_hectare ##GJ diesel consumption
  
  emissions = energy_consumption * ef ##emissions in units kg as EF kg/GJ fuel consumed
  names(emissions) <- c("CO2", "CO", "NOx", "SO2", "NMVOC", "NH3", "PM10", "PM2.5", "BC", "OC", "CH4")
  
  my_list <- list("energy_consumption" = energy_consumption,
                  "emissions" = emissions)
  
  return(my_list) 
  
}


##Based on Koons et al. 2012
fertiliser_energy <- function(application_rate = 0, crop_area = 0, fertiliser_pct = 0,
                              conversion_N_tonnes = 0,
                              
                              #fertiliser_type = "Urea",  
                              #fertiliser_production = 0, 
                              nh3_req = 0, 
                              nitric_acid_req = 0,
                              nitric_acid_nh3_req = 0,
                              urea_req = 0, 
                              urea_nh3_req = 0,
                              
                              ammonium_nitrate_req = 0,
                              ammonium_nitrate_nh3_req = 0,
                              ammonium_nitrate_nitric_acid_req = 0,
                              
                              #dolomite_req = 0,
                              #sulphuric_acid_req = 0,
                              #phosphoric_acid_req = 0,
                              #muriate_of_potash_req = 0,
                              #inert_req = 0,
                              map_req = 0,
                              map_nh3_req = 0,
                              dap_req = 0,
                              dap_nh3_req = 0, 
                              
                              #triple_super_phosphate_req = 0,
                              ammonia_energy_input = 0,
                              ammonia_fuel_gas_prop = 0,
                              ammonia_fuel_oil_prop = 0,
                              ammonia_fuel_coal_prop = 0,
                              
                              #potash_rock_energy_req = 0,
                              #phosphoric_acid_energy_req = 0,
                              #sulphuric_acid_energy_req = 0,
                              #muriate_of_potash_energy_req = 0,
                              #sulphate_of_potash_energy_req = 0,
                              #inert_energy_req = 0,
                              
                              nitric_acid_n2o_ef = 0,
                              
                              ef_gas = c(0,0,0,0,0,0,0,0,0,0,0),
                              ef_oil = c(0,0,0,0,0,0,0,0,0,0,0),
                              ef_coal = c(0,0,0,0,0,0,0,0,0,0,0)){
  
  
  fertiliser_production_N <- application_rate /1000 * crop_area * fertiliser_pct ##Tonnes N of different fertilisers
  fertiliser_production <- fertiliser_production_N * conversion_N_tonnes ##Tonnes fertiliser
  
  
  nitric_acid_requirements = (fertiliser_production) * (nitric_acid_req + 
                                                          ((ammonium_nitrate_req / 1000) * ammonium_nitrate_nitric_acid_req)) ##kg nitric acid y-1
  
  ##Calculate ammonia requirements
  ammonia_requirements = sum(((fertiliser_production) * (nh3_req + 
                                                      ((ammonium_nitrate_req / 1000) * ammonium_nitrate_nh3_req) +  
                                                      ((urea_req / 1000) * urea_nh3_req)+
                                                      ((map_req / 1000) * map_nh3_req) +
                                                      ((dap_req / 1000) * dap_nh3_req)+
                                                      (nitric_acid_requirements*(nitric_acid_nh3_req/1000)))),na.rm = TRUE)  ##kg nh3 y-1 ##Need to add NH3 needed to make nitric acid
  
  
  ammonia_energy_requirements = ammonia_requirements / 1000 * ammonia_energy_input ##GJ energy
  
  ammonia_gas_consumption = ammonia_energy_requirements * ammonia_fuel_gas_prop ##GJ natural gas
  ammonia_oil_consumption = ammonia_energy_requirements * ammonia_fuel_oil_prop ##GJ natural gas
  ammonia_coal_consumption = ammonia_energy_requirements * ammonia_fuel_coal_prop ##GJ natural gas
  
  ammonia_gas_emissions = ammonia_gas_consumption * ef_gas /1000 ##kg emissions
  names(ammonia_gas_emissions) <- c("CO2", "CO", "NOx", "SO2", "NMVOC", "NH3", "PM10", "PM2.5", "BC", "OC", "CH4")
  
  ammonia_oil_emissions = ammonia_oil_consumption * ef_coal/1000 ##kg emissions
  names(ammonia_oil_emissions) <- c("CO2", "CO", "NOx", "SO2", "NMVOC", "NH3", "PM10", "PM2.5", "BC", "OC", "CH4")
  
  ammonia_coal_emissions = ammonia_coal_consumption * ef_coal/1000 ##kg emissions
  names(ammonia_coal_emissions) <- c("CO2", "CO", "NOx", "SO2", "NMVOC", "NH3", "PM10", "PM2.5", "BC", "OC", "CH4")
  
  
  ##Nitric acid
  n2o_nitric_acid <- sum(nitric_acid_requirements,na.rm = TRUE) / 1000 * nitric_acid_n2o_ef ##kg N2O y-1
  
  
  my_list <- list("ammonia_gas_consumption" = ammonia_gas_consumption,
                  "ammonia_oil_consumption" = ammonia_oil_consumption,
                  "ammonia_coal_consumption" = ammonia_coal_consumption,
                  "ammonia_gas_emissions" = ammonia_gas_emissions,
                  "ammonia_oil_emissions" = ammonia_oil_emissions,
                  "ammonia_coal_emissions" = ammonia_coal_emissions,
                  "nitric_acid_n2o_emissions" = n2o_nitric_acid)
  
  return(my_list) 
  
}





##Grassland landarea


grassland_area <- function(grassland_feed = 0, grassland_yield = 0){
  
 
  land_area <- grassland_feed / grassland_yield #hectares
  
  return(land_area)  
}




diet_risks <- function(GDP_per_capita = 0, GINI = 0, cv_requirement = 0, malnutrition_calories = 0, calories_consumed = 0,
                       fruit_mean = 0, fruit_tmrel = 250, fruit_elasticity = 0,
                       vegetable_mean = 0, vegetable_tmrel = 360, vegetable_elasticity = 0,
                       legume_mean = 0, legume_tmrel = 60, legume_elasticity = 0,
                       grain_mean = 0, grain_tmrel = 125, grain_elasticity = 0,
                       milk_mean = 0, milk_tmrel = 435, milk_elasticity = 0,
                       meat_mean = 0, meat_tmrel = 435, meat_elasticity = 0,
                       alpha_max = 0, alpha_min = 0){ 
  
  ##Malnutrition
  ##Coefficient of variation energy
  cv_energy = 0.243+(0.0392*GINI)+(-0.0328*log(GDP_per_capita))
  cv_requirement = cv_requirement
  cv <- sqrt((cv_energy^2)+(cv_requirement^2))
  skewness <- ((cv^2)+3)*cv
  
  lognormal_sigma <- (log((cv^2)+1))^0.5
  lognormal_mu <- log(calories_consumed)-(lognormal_sigma^2)/2
  
  z_cutoff <- (log(malnutrition_calories)-lognormal_mu)/lognormal_sigma
  
  prop_malnourished <- dnorm(z_cutoff)
  ##prop_malnourished <- plnorm(1800, meanlog = lognormal_mu, sdlog = lognormal_sigma)  ##Gives same results as above
  
  ##Child stunting - Method taken from Lloyd et al. 2011 
  
  alpha <- log(GDP_per_capita / GINI)
  
  development_score <- if(alpha <= 10){-(alpha-alpha_max)/(alpha_max-alpha_min)}else{0}
  
  
  ##Moderate_stunting
  prop_mod_stunting <- 0.025 + (0.35 * prop_malnourished) + (development_score * 0.26) + (-0.43 * prop_malnourished * development_score) ##>2SD below mean weight-for-height
  prop_severe_stunting <- -0.052 + (0.18 * prop_malnourished) + (development_score * 0.34) + (-0.43 * prop_malnourished * development_score) ##>3SD below mean weight-for-height
  
  
  
  ##Overweight
  prop_overweight <- 0.02462 * calories_consumed - 29.67965
  prop_obese <- 0.01 * calories_consumed - 14.98936
  
  
  ##Diets
  sd_logdist <- sqrt(qgamma(GINI, 0.5, 1))*2
  
  ##Low in fruits
  fruit_prop_belowtmrel <- plnorm(GDP_per_capita + (GDP_per_capita * (((fruit_tmrel / fruit_mean -1))/fruit_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  fruit_prop_belowtmrel_0.9 <- plnorm(GDP_per_capita + (GDP_per_capita * (((fruit_tmrel*0.9 / fruit_mean -1))/fruit_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  fruit_prop_belowtmrel_0.8 <- plnorm(GDP_per_capita + (GDP_per_capita * (((fruit_tmrel*0.8 / fruit_mean -1))/fruit_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  fruit_prop_belowtmrel_0.7 <- plnorm(GDP_per_capita + (GDP_per_capita * (((fruit_tmrel*0.7 / fruit_mean -1))/fruit_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  fruit_prop_belowtmrel_0.6 <- plnorm(GDP_per_capita + (GDP_per_capita * (((fruit_tmrel*0.6 / fruit_mean -1))/fruit_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  fruit_prop_belowtmrel_0.5 <- plnorm(GDP_per_capita + (GDP_per_capita * (((fruit_tmrel*0.5 / fruit_mean -1))/fruit_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  fruit_prop_belowtmrel_0.4 <- plnorm(GDP_per_capita + (GDP_per_capita * (((fruit_tmrel*0.4 / fruit_mean -1))/fruit_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  fruit_prop_belowtmrel_0.3 <- plnorm(GDP_per_capita + (GDP_per_capita * (((fruit_tmrel*0.3 / fruit_mean -1))/fruit_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  fruit_prop_belowtmrel_0.2 <- plnorm(GDP_per_capita + (GDP_per_capita * (((fruit_tmrel*0.2 / fruit_mean -1))/fruit_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  fruit_prop_belowtmrel_0.1 <- plnorm(GDP_per_capita + (GDP_per_capita * (((fruit_tmrel*0.1 / fruit_mean -1))/fruit_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  fruit_prop_belowtmrel_0 <- plnorm(GDP_per_capita + (GDP_per_capita * (((fruit_tmrel*1E-50 / fruit_mean -1))/fruit_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  
  ##Low in vegetables
  vegetable_prop_belowtmrel <- plnorm(GDP_per_capita + (GDP_per_capita * (((vegetable_tmrel / vegetable_mean -1))/vegetable_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  vegetable_prop_belowtmrel_0.9 <- plnorm(GDP_per_capita + (GDP_per_capita * (((vegetable_tmrel*0.9 / vegetable_mean -1))/vegetable_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  vegetable_prop_belowtmrel_0.8 <- plnorm(GDP_per_capita + (GDP_per_capita * (((vegetable_tmrel*0.8 / vegetable_mean -1))/vegetable_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  vegetable_prop_belowtmrel_0.7 <- plnorm(GDP_per_capita + (GDP_per_capita * (((vegetable_tmrel*0.7 / vegetable_mean -1))/vegetable_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  vegetable_prop_belowtmrel_0.6 <- plnorm(GDP_per_capita + (GDP_per_capita * (((vegetable_tmrel*0.6 / vegetable_mean -1))/vegetable_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  vegetable_prop_belowtmrel_0.5 <- plnorm(GDP_per_capita + (GDP_per_capita * (((vegetable_tmrel*0.5 / vegetable_mean -1))/vegetable_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  vegetable_prop_belowtmrel_0.4 <- plnorm(GDP_per_capita + (GDP_per_capita * (((vegetable_tmrel*0.4 / vegetable_mean -1))/vegetable_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  vegetable_prop_belowtmrel_0.3 <- plnorm(GDP_per_capita + (GDP_per_capita * (((vegetable_tmrel*0.3 / vegetable_mean -1))/vegetable_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  vegetable_prop_belowtmrel_0.2 <- plnorm(GDP_per_capita + (GDP_per_capita * (((vegetable_tmrel*0.2 / vegetable_mean -1))/vegetable_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  vegetable_prop_belowtmrel_0.1 <- plnorm(GDP_per_capita + (GDP_per_capita * (((vegetable_tmrel*0.1 / vegetable_mean -1))/vegetable_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  vegetable_prop_belowtmrel_0 <- plnorm(GDP_per_capita + (GDP_per_capita * (((vegetable_tmrel*1E-50 / vegetable_mean -1))/vegetable_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  
  ##Low in legumes
  legume_prop_belowtmrel <- plnorm(GDP_per_capita + (GDP_per_capita * (((legume_tmrel / legume_mean -1))/legume_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  legume_prop_belowtmrel_0.9 <- plnorm(GDP_per_capita + (GDP_per_capita * (((legume_tmrel*0.9 / legume_mean -1))/legume_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  legume_prop_belowtmrel_0.8 <- plnorm(GDP_per_capita + (GDP_per_capita * (((legume_tmrel*0.8 / legume_mean -1))/legume_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  legume_prop_belowtmrel_0.7 <- plnorm(GDP_per_capita + (GDP_per_capita * (((legume_tmrel*0.7 / legume_mean -1))/legume_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  legume_prop_belowtmrel_0.6 <- plnorm(GDP_per_capita + (GDP_per_capita * (((legume_tmrel*0.6 / legume_mean -1))/legume_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  legume_prop_belowtmrel_0.5 <- plnorm(GDP_per_capita + (GDP_per_capita * (((legume_tmrel*0.5 / legume_mean -1))/legume_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  legume_prop_belowtmrel_0.4 <- plnorm(GDP_per_capita + (GDP_per_capita * (((legume_tmrel*0.4 / legume_mean -1))/legume_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  legume_prop_belowtmrel_0.3 <- plnorm(GDP_per_capita + (GDP_per_capita * (((legume_tmrel*0.3 / legume_mean -1))/legume_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  legume_prop_belowtmrel_0.2 <- plnorm(GDP_per_capita + (GDP_per_capita * (((legume_tmrel*0.2 / legume_mean -1))/legume_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  legume_prop_belowtmrel_0.1 <- plnorm(GDP_per_capita + (GDP_per_capita * (((legume_tmrel*0.1 / legume_mean -1))/legume_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  legume_prop_belowtmrel_0 <- plnorm(GDP_per_capita + (GDP_per_capita * (((legume_tmrel*1E-50 / legume_mean -1))/legume_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  
  
  ##Low in grains
  grain_prop_belowtmrel <- plnorm(GDP_per_capita + (GDP_per_capita * (((grain_tmrel / grain_mean -1))/grain_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  grain_prop_belowtmrel_0.9 <- plnorm(GDP_per_capita + (GDP_per_capita * (((grain_tmrel*0.9 / grain_mean -1))/grain_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  grain_prop_belowtmrel_0.8 <- plnorm(GDP_per_capita + (GDP_per_capita * (((grain_tmrel*0.8 / grain_mean -1))/grain_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  grain_prop_belowtmrel_0.7 <- plnorm(GDP_per_capita + (GDP_per_capita * (((grain_tmrel*0.7 / grain_mean -1))/grain_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  grain_prop_belowtmrel_0.6 <- plnorm(GDP_per_capita + (GDP_per_capita * (((grain_tmrel*0.6 / grain_mean -1))/grain_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  grain_prop_belowtmrel_0.5 <- plnorm(GDP_per_capita + (GDP_per_capita * (((grain_tmrel*0.5 / grain_mean -1))/grain_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  grain_prop_belowtmrel_0.4 <- plnorm(GDP_per_capita + (GDP_per_capita * (((grain_tmrel*0.4 / grain_mean -1))/grain_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  grain_prop_belowtmrel_0.3 <- plnorm(GDP_per_capita + (GDP_per_capita * (((grain_tmrel*0.3 / grain_mean -1))/grain_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  grain_prop_belowtmrel_0.2 <- plnorm(GDP_per_capita + (GDP_per_capita * (((grain_tmrel*0.2 / grain_mean -1))/grain_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  grain_prop_belowtmrel_0.1 <- plnorm(GDP_per_capita + (GDP_per_capita * (((grain_tmrel*0.1 / grain_mean -1))/grain_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  grain_prop_belowtmrel_0 <- plnorm(GDP_per_capita + (GDP_per_capita * (((grain_tmrel*1E-50 / grain_mean -1))/grain_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  
  ##Low in milks
  milk_prop_belowtmrel <- plnorm(GDP_per_capita + (GDP_per_capita * (((milk_tmrel / milk_mean -1))/milk_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  milk_prop_belowtmrel_0.9 <- plnorm(GDP_per_capita + (GDP_per_capita * (((milk_tmrel*0.9 / milk_mean -1))/milk_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  milk_prop_belowtmrel_0.8 <- plnorm(GDP_per_capita + (GDP_per_capita * (((milk_tmrel*0.8 / milk_mean -1))/milk_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  milk_prop_belowtmrel_0.7 <- plnorm(GDP_per_capita + (GDP_per_capita * (((milk_tmrel*0.7 / milk_mean -1))/milk_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  milk_prop_belowtmrel_0.6 <- plnorm(GDP_per_capita + (GDP_per_capita * (((milk_tmrel*0.6 / milk_mean -1))/milk_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  milk_prop_belowtmrel_0.5 <- plnorm(GDP_per_capita + (GDP_per_capita * (((milk_tmrel*0.5 / milk_mean -1))/milk_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  milk_prop_belowtmrel_0.4 <- plnorm(GDP_per_capita + (GDP_per_capita * (((milk_tmrel*0.4 / milk_mean -1))/milk_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  milk_prop_belowtmrel_0.3 <- plnorm(GDP_per_capita + (GDP_per_capita * (((milk_tmrel*0.3 / milk_mean -1))/milk_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  milk_prop_belowtmrel_0.2 <- plnorm(GDP_per_capita + (GDP_per_capita * (((milk_tmrel*0.2 / milk_mean -1))/milk_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  milk_prop_belowtmrel_0.1 <- plnorm(GDP_per_capita + (GDP_per_capita * (((milk_tmrel*0.1 / milk_mean -1))/milk_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  milk_prop_belowtmrel_0 <- plnorm(GDP_per_capita + (GDP_per_capita * (((milk_tmrel*1E-50 / milk_mean -1))/milk_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  
  ##Low in meats
  meat_prop_belowtmrel <- plnorm(GDP_per_capita + (GDP_per_capita * (((meat_tmrel / meat_mean -1))/meat_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  meat_prop_below50g <- plnorm(GDP_per_capita + (GDP_per_capita * (((50 / meat_mean -1))/meat_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  meat_prop_below100g <- plnorm(GDP_per_capita + (GDP_per_capita * (((100 / meat_mean -1))/meat_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  meat_prop_below150g <- plnorm(GDP_per_capita + (GDP_per_capita * (((150 / meat_mean -1))/meat_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  meat_prop_below200g <- plnorm(GDP_per_capita + (GDP_per_capita * (((200 / meat_mean -1))/meat_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  meat_prop_below250g <- plnorm(GDP_per_capita + (GDP_per_capita * (((250 / meat_mean -1))/meat_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  meat_prop_below300g <- plnorm(GDP_per_capita + (GDP_per_capita * (((300 / meat_mean -1))/meat_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  meat_prop_below350g <- plnorm(GDP_per_capita + (GDP_per_capita * (((350 / meat_mean -1))/meat_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  meat_prop_below400g <- plnorm(GDP_per_capita + (GDP_per_capita * (((400 / meat_mean -1))/meat_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  meat_prop_below500g <- plnorm(GDP_per_capita + (GDP_per_capita * (((500 / meat_mean -1))/meat_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  meat_prop_below600g <- plnorm(GDP_per_capita + (GDP_per_capita * (((600 / meat_mean -1))/meat_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  meat_prop_below700g <- plnorm(GDP_per_capita + (GDP_per_capita * (((700 / meat_mean -1))/meat_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  meat_prop_below800g <- plnorm(GDP_per_capita + (GDP_per_capita * (((800 / meat_mean -1))/meat_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  meat_prop_below900g <- plnorm(GDP_per_capita + (GDP_per_capita * (((900 / meat_mean -1))/meat_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  meat_prop_below1000g <- plnorm(GDP_per_capita + (GDP_per_capita * (((1000 / meat_mean -1))/meat_elasticity)), log(GDP_per_capita), sqrt(sd_logdist))
  
  
  my_list <- list("development_score" = development_score,
                  "prop_malnourised" = prop_malnourished,
                  "prop_overweight" = prop_overweight,
                  "prop_obese" = prop_obese, 
                  "prop_mod_stunting" = prop_mod_stunting,
                  "prop_severe_stunting" = prop_severe_stunting,
                  
                  "fruit_prop_belowtmrel" = fruit_prop_belowtmrel,
                  "fruit_prop_belowtmrel_0.9" = fruit_prop_belowtmrel_0.9,
                  "fruit_prop_belowtmrel_0.8" = fruit_prop_belowtmrel_0.8,
                  "fruit_prop_belowtmrel_0.7" = fruit_prop_belowtmrel_0.7,
                  "fruit_prop_belowtmrel_0.6" = fruit_prop_belowtmrel_0.6,
                  "fruit_prop_belowtmrel_0.5" = fruit_prop_belowtmrel_0.5,
                  "fruit_prop_belowtmrel_0.4" = fruit_prop_belowtmrel_0.4,
                  "fruit_prop_belowtmrel_0.3" = fruit_prop_belowtmrel_0.3,
                  "fruit_prop_belowtmrel_0.2" = fruit_prop_belowtmrel_0.2,
                  "fruit_prop_belowtmrel_0.1" = fruit_prop_belowtmrel_0.1,
                  "fruit_prop_belowtmrel_0" = fruit_prop_belowtmrel_0,
                  
                  "vegetable_prop_belowtmrel" = vegetable_prop_belowtmrel,
                  "vegetable_prop_belowtmrel_0.9" = vegetable_prop_belowtmrel_0.9,
                  "vegetable_prop_belowtmrel_0.8" = vegetable_prop_belowtmrel_0.8,
                  "vegetable_prop_belowtmrel_0.7" = vegetable_prop_belowtmrel_0.7,
                  "vegetable_prop_belowtmrel_0.6" = vegetable_prop_belowtmrel_0.6,
                  "vegetable_prop_belowtmrel_0.5" = vegetable_prop_belowtmrel_0.5,
                  "vegetable_prop_belowtmrel_0.4" = vegetable_prop_belowtmrel_0.4,
                  "vegetable_prop_belowtmrel_0.3" = vegetable_prop_belowtmrel_0.3,
                  "vegetable_prop_belowtmrel_0.2" = vegetable_prop_belowtmrel_0.2,
                  "vegetable_prop_belowtmrel_0.1" = vegetable_prop_belowtmrel_0.1,
                  "vegetable_prop_belowtmrel_0" = vegetable_prop_belowtmrel_0,
                  
                  "legume_prop_belowtmrel" = legume_prop_belowtmrel,
                  "legume_prop_belowtmrel_0.9" = legume_prop_belowtmrel_0.9,
                  "legume_prop_belowtmrel_0.8" = legume_prop_belowtmrel_0.8,
                  "legume_prop_belowtmrel_0.7" = legume_prop_belowtmrel_0.7,
                  "legume_prop_belowtmrel_0.6" = legume_prop_belowtmrel_0.6,
                  "legume_prop_belowtmrel_0.5" = legume_prop_belowtmrel_0.5,
                  "legume_prop_belowtmrel_0.4" = legume_prop_belowtmrel_0.4,
                  "legume_prop_belowtmrel_0.3" = legume_prop_belowtmrel_0.3,
                  "legume_prop_belowtmrel_0.2" = legume_prop_belowtmrel_0.2,
                  "legume_prop_belowtmrel_0.1" = legume_prop_belowtmrel_0.1,
                  "legume_prop_belowtmrel_0" = legume_prop_belowtmrel_0,
                  
                  "grain_prop_belowtmrel" = grain_prop_belowtmrel,
                  "grain_prop_belowtmrel_0.9" = grain_prop_belowtmrel_0.9,
                  "grain_prop_belowtmrel_0.8" = grain_prop_belowtmrel_0.8,
                  "grain_prop_belowtmrel_0.7" = grain_prop_belowtmrel_0.7,
                  "grain_prop_belowtmrel_0.6" = grain_prop_belowtmrel_0.6,
                  "grain_prop_belowtmrel_0.5" = grain_prop_belowtmrel_0.5,
                  "grain_prop_belowtmrel_0.4" = grain_prop_belowtmrel_0.4,
                  "grain_prop_belowtmrel_0.3" = grain_prop_belowtmrel_0.3,
                  "grain_prop_belowtmrel_0.2" = grain_prop_belowtmrel_0.2,
                  "grain_prop_belowtmrel_0.1" = grain_prop_belowtmrel_0.1,
                  "grain_prop_belowtmrel_0" = grain_prop_belowtmrel_0,
                  
                  "milk_prop_belowtmrel" = milk_prop_belowtmrel,
                  "milk_prop_belowtmrel_0.9" = milk_prop_belowtmrel_0.9,
                  "milk_prop_belowtmrel_0.8" = milk_prop_belowtmrel_0.8,
                  "milk_prop_belowtmrel_0.7" = milk_prop_belowtmrel_0.7,
                  "milk_prop_belowtmrel_0.6" = milk_prop_belowtmrel_0.6,
                  "milk_prop_belowtmrel_0.5" = milk_prop_belowtmrel_0.5,
                  "milk_prop_belowtmrel_0.4" = milk_prop_belowtmrel_0.4,
                  "milk_prop_belowtmrel_0.3" = milk_prop_belowtmrel_0.3,
                  "milk_prop_belowtmrel_0.2" = milk_prop_belowtmrel_0.2,
                  "milk_prop_belowtmrel_0.1" = milk_prop_belowtmrel_0.1,
                  "milk_prop_belowtmrel_0" = milk_prop_belowtmrel_0,
                  
                  "meat_prop_belowtmrel" = meat_prop_belowtmrel,
                  "meat_prop_below50g" = meat_prop_below50g,
                  "meat_prop_below100g" = meat_prop_below100g,
                  "meat_prop_below150g" = meat_prop_below150g,
                  "meat_prop_below200g" = meat_prop_below200g,
                  "meat_prop_below250g" = meat_prop_below250g,
                  "meat_prop_below300g" = meat_prop_below300g,
                  "meat_prop_below350g" = meat_prop_below350g,
                  "meat_prop_below400g" = meat_prop_below400g,
                  "meat_prop_below500g" = meat_prop_below500g,
                  "meat_prop_below600g" = meat_prop_below600g,
                  "meat_prop_below700g" = meat_prop_below700g,
                  "meat_prop_below800g" = meat_prop_below800g,
                  "meat_prop_below900g" = meat_prop_below900g,
                  "meat_prop_below1000g" = meat_prop_below1000g
  )
  
  return(my_list)
}

diet_hia <- function(pop = 0, RR = 0, RR_95min = 0, RR_95max = 0, bmort = 0, n_it = 1000){ 
  
  ##calculate SD for RR based on 95% CI being +-1.96 SD
  RR_sd <- (RR_95max - RR_95min)/ (1.96*2)
  
  if(RR != 0){
  
  
  RR_dist = mcstoc(rnorm,type="U", mean = RR, sd = RR_sd, nsu = 1000)
  
  deaths = bmort * (((RR_dist-1)/RR_dist)) * pop
  
  }else{
    
    deaths = rep(0, n_it)
    
  }
  
  my_list <- list("deaths_mean" = mean(deaths, na.rm = TRUE),
                  "deaths_median" = as.numeric(quantile(deaths[1:n_it], 0.5, na.rm = TRUE)),
                  "deaths_2.5" = as.numeric(quantile(deaths[1:n_it], 0.025, na.rm = TRUE)),
                  "deaths_97.5" = as.numeric(quantile(deaths[1:n_it], 0.975, na.rm = TRUE)))
  return(my_list)
}

































##Set up data frame with variables for cattle, sheep_goat, pigs, chickens
species_lists <- data.frame(species = numeric(4))
species_lists$species <- c("cattle", "sheep", "pig", "chicken")
species_lists$groups <- c("Bovine Meat", "Mutton & Goat Meat", "Pigmeat", "Poultry Meat")
species_lists$primary_animals_1 <- c("Meat, cattle", "Meat, sheep", "Meat, pig", "Meat, Poultry")
species_lists$primary_animals_2 <- c(NA, "Meat, goat", NA, NA)
species_lists$live_animals <- c("Cattle", "Sheep and Goats", "Pigs", "Poultry Birds")



feed_categories_ruminants <- c("GRASS", "GRASSH", "GRASSLEGF", "GRASSLEGH", "ALFALFAH", 
                               "GRAINSIL", "MAIZESIL", 
                               "WSTRAW", "ZSTOVER", "MSTOVER", "SSTOVER", "RSTRAW", "BSTRAW", "TOPS", 
                               "FDDRBEET", "CORN", "GRAINS", 
                               "MLSOY", "MLRAPE", "MLCTTN", "BPULP", "PKEXP", "MOLASSES", 
                               "MZGLTM", "MZGLTF", "GRNBYDRY", "GRNBYWET", "CONC", "LEAVES", "GRASSH2")




feed_categories_monogastrics <- c("SWILL", "GRASSF", 
                                  "PULSES", "PSTRAW", "CASSAVA", "WHEAT", "MAIZE", "BARLEY", "MILLET", "RICE", 
                                  "SORGHUM", "SOY", "TOPS", "LEAVES", "BNFRUIT", "BNSTEM", "MLSOY", "MLCTTN", "MLOILSDS", "GRNBYDRY", 
                                  "PULSES", "CASSAVA", "WHEAT", "MAIZE", "BARLEY", "MILLET", "RICE", "SORGHUM", "SOY", "RAPESEED",
                                  "SOYOIL", "MLSOY", "MLCTTN", "MLRAPE", "PKEXP", "MLOILSDS", "FISHMEAL", "MOLASSES", "GRNBYDRY", "GRNBYWET", 
                                  "SYNTHETIC", "LIMESTONE")



manure_management_systems <- c("pasture", "daily_spread", "solid_storage", "dry_lot", 
                               "liquid_slurry", "liquid_slurry_crust", 
                               "anaerobic_lagoon", 
                               "pitstorage_less1month", "pitstorage_greater1month", "pitstorage_greater1month_chickens", 
                               "anaerobic_digester", "composting", 
                               "burned_fuel", "poultry_with_litter")





crop_categories <- c("Cereals", "Fruits", "Nuts", "Oilcrops", "Pulses", "Roots", "Stimulants", "Sugar", "Vegetables", "Vegoils")
Cereals <- c("Barley and products", "Cereals, Other", "Maize and products" , "Millet and products", "Oats", "Rice and products", "Rye and products", "Sorghum and products", "Wheat and products")
Fruits <- c("Apples and products", "Bananas", "Citrus, Other", "Dates", "Fruits, Other", "Grapefruit and products",  "Grapes and products (excl wine)", "Lemons, Limes and products", "Oranges, Mandarines", "Pineapples and products",         "Plantains")
Nuts <- c("Nuts and products")
Oilcrops <- c("Coconuts - Incl Copra", "Groundnuts (Shelled Eq)", "Oilcrops", "Oilcrops, Other", "Olives (including preserved)", "Rape and Mustard Oil", "Rape and Mustardseed", "Sesame seed", "Soyabeans")
Pulses <- c("Beans",  "Peas", "Pulses")
Roots <- c("Cassava and products", "Potatoes and products", "Roots, Other", "Sweet potatoes", "Yams")
Stimulants <- c("Cocoa Beans and products", "Coffee and products", "Tea (including mate)")
Sugar <- c("Sugar cane", "Sugar beet")
Vegetables <- c("Onions", "Tomatoes and products", "Vegetables, Other")
Vegoils <- c("Coconut Oil", "Cottonseed Oil", "Groundnut Oil", "Maize Germ Oil", "Oilcrops Oil, Other", "Olive Oil", "Palm Oil", "Palmkernel Oil", "Sesameseed Oil", "Soyabean Oil", "Sunflowerseed Oil")


country_codes <- read.csv(".\\Inputs\\country codes.csv")

country_codes$region <- as.character(country_codes$region)
country_codes$region[is.na(country_codes$region)] <- "NA"
##Remove Tokelau
country_codes <- country_codes[-c(176),] 
##Remove Andorra
country_codes <- country_codes[-c(4),] 




##Develop input data
start_year <- 2018
end_year <- 2018  

setwd("./Inputs")

file.names <- list.files()
x <- as.data.frame(file.names)

y <- as.character(x[grepl("inputs_meat", x[["file.names"]]), ])


z <- as.character((lapply(strsplit(y, "\\_"), "[", 3)))
z <- as.character((lapply(strsplit(z, "\\."), "[", 1)))
country_list <- z


































for(countries in seq(from=1, to=(length(country_list)), by=1)) {

  country = country_list[countries]
  code <- as.numeric(country_codes[ which(country_codes$country == country), ]$FAOSTAT)
  region <- as.character(country_codes[ which(country_codes$country == country), ]$region)
  

  for(year in seq(from= start_year, to= end_year, by=1)) {
    

inputs_meat <- read.csv(paste(".\\inputs_meat_", country, ".csv", sep = ""))
inputs_herd <- read.csv(paste(".\\inputs_herd_", country, ".csv", sep = ""))
inputs_feed_cattle <- read.csv(paste(".\\inputs_feed_cattle_", country, ".csv", sep = ""))
inputs_manure_cattle<- read.csv(paste(".\\inputs_manure_cattle_", country, ".csv", sep = ""))
inputs_feed_sheep<- read.csv(paste(".\\inputs_feed_sheep_", country, ".csv", sep = ""))
inputs_manure_sheep<- read.csv(paste(".\\inputs_manure_sheep_", country, ".csv", sep = ""))
inputs_feed_pig<- read.csv(paste(".\\inputs_feed_pig_", country, ".csv", sep = ""))
inputs_manure_pig<- read.csv(paste(".\\inputs_manure_pig_", country, ".csv", sep = ""))
inputs_feed_chicken <- read.csv(paste(".\\inputs_feed_chicken_", country, ".csv", sep = ""))
inputs_manure_chicken <- read.csv(paste(".\\inputs_manure_chicken_", country, ".csv", sep = ""))

inputs_feedcrop <- read.csv(paste(".\\inputs_feedcrop_", country, ".csv", sep = ""))
inputs_crop <- read.csv(paste(".\\inputs_crop_", country, ".csv", sep = ""))
inputs_othercrop <- read.csv(paste(".\\inputs_othercrop_", country, ".csv", sep = ""))

feed_monogastrics <- read.csv(paste(".\\feed_monogastrics_", country, ".csv", sep = ""))
feed_ruminants <- read.csv(paste(".\\feed_ruminants_", country, ".csv", sep = ""))

inputs_health <- read.csv(paste(".\\inputs_health_", country, ".csv", sep = ""))
inputs_risk <- read.csv(paste(".\\inputs_risk_", country, ".csv", sep = ""))


manure <- read.csv(paste(".\\manure_", country, ".csv", sep = ""))






for(count in seq(from=1, to=length(species_lists$species), by=1)) {

##Calculations

#LIVE ANIMALS
##Cattle Meat
  
kcal <-  (as.numeric(inputs_meat[ which(inputs_meat$variable == "Total Food supply"), ][paste("y_", year, sep = "")])) * (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Fraction Food supply consumed ", species_lists$groups[count], sep = "")), ][paste("y_", year, sep = "")]))+
                     (as.numeric(inputs_meat[ which(inputs_meat$variable == "Total Food supply"), ][paste("y_", year, sep = "")])) * (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Fraction Food supply wasted ", species_lists$groups[count], sep = "")), ][paste("y_", year, sep = "")]))

kcal_consumed <- (as.numeric(inputs_meat[ which(inputs_meat$variable == "Total Food supply"), ][paste("y_", year, sep = "")])) * (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Fraction Food supply consumed ", species_lists$groups[count], sep = "")), ][paste("y_", year, sep = "")]))/
                         (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Population", sep = "")), ][paste("y_", year, sep = "")]))/365 ##kcal/person/day consumed (i.e. not wasted)

g_consumed <- kcal_consumed / (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Kcal per tonne ", species_lists$groups[count], sep = "")), ][paste("y_", year, sep = "")]))*1000000 #g/person/day


kcal_wasted <- (as.numeric(inputs_meat[ which(inputs_meat$variable == "Total Food supply"), ][paste("y_", year, sep = "")])) * (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Fraction Food supply wasted ", species_lists$groups[count], sep = "")), ][paste("y_", year, sep = "")]))/
  (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Population", sep = "")), ][paste("y_", year, sep = "")]))/365 ##kcal/person/day wasted

g_wasted <- kcal_wasted / (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Kcal per tonne ", species_lists$groups[count], sep = "")), ][paste("y_", year, sep = "")]))*1000000 #g/person/day




domestic_consumption <- kcal / (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Kcal per tonne ", species_lists$groups[count], sep = "")), ][paste("y_", year, sep = "")]))

domestic_production <- domestic_consumption + 
                                    (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Loss ", species_lists$groups[count], sep = "")), ][paste("y_", year, sep = "")])) +
                                    (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Stock var ", species_lists$groups[count], sep = "")), ][paste("y_", year, sep = "")])) +
                                    (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Export ", species_lists$groups[count], sep = "")), ][paste("y_", year, sep = "")])) -
                                    (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Import ", species_lists$groups[count], sep = "")), ][paste("y_", year, sep = "")]))

if(species_lists$species[count] == "cattle"){
  
  domestic_production <- domestic_production * as.numeric(inputs_meat[ which(inputs_meat$variable == paste("bovine meat production cattle", sep = "")), ][paste("y_", year, sep = "")])
  
}
  
##Convert to number of head

live_animals <- domestic_production / (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Carcass weight ", species_lists$groups[count], sep = "")), ][paste("y_", year, sep = "")])) / 
                                              (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Offtake rate ", species_lists$groups[count], sep = "")), ][paste("y_", year, sep = "")])) ##Head

##Dairy

kcal_dairy <- (as.numeric(inputs_meat[ which(inputs_meat$variable == "Total Food supply"), ][paste("y_", year, sep = "")])) * (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Fraction food supply dairy consumed", sep = "")), ][paste("y_", year, sep = "")]))+
                    (as.numeric(inputs_meat[ which(inputs_meat$variable == "Total Food supply"), ][paste("y_", year, sep = "")])) * (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Fraction food supply dairy wasted", sep = "")), ][paste("y_", year, sep = "")]))


kcal_consumed_dairy <- (as.numeric(inputs_meat[ which(inputs_meat$variable == "Total Food supply"), ][paste("y_", year, sep = "")])) * (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Fraction food supply dairy consumed", sep = "")), ][paste("y_", year, sep = "")]))/
  (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Population", sep = "")), ][paste("y_", year, sep = "")]))/365 ##kcal/person/day consumed (i.e. not wasted)

g_consumed_dairy <- kcal_consumed_dairy / (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Kcal per tonne dairy", sep = "")), ][paste("y_", year, sep = "")]))*1000000 #g/person/day


kcal_wasted_dairy <- (as.numeric(inputs_meat[ which(inputs_meat$variable == "Total Food supply"), ][paste("y_", year, sep = "")])) * (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Fraction food supply dairy wasted", sep = "")), ][paste("y_", year, sep = "")]))/
  (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Population", sep = "")), ][paste("y_", year, sep = "")]))/365 ##kcal/person/day wasted

g_wasted_dairy <- kcal_wasted_dairy / (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Kcal per tonne dairy", sep = "")), ][paste("y_", year, sep = "")]))*1000000 #g/person/day





domestic_consumption_dairy <- kcal_dairy / (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Kcal per tonne dairy", sep = "")), ][paste("y_", year, sep = "")]))

domestic_production_dairy <- domestic_consumption_dairy + 
  (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Feed dairy", sep = "")), ][paste("y_", year, sep = "")])) +
  (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Loss dairy", sep = "")), ][paste("y_", year, sep = "")])) +
  (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Stock var dairy", sep = "")), ][paste("y_", year, sep = "")])) +
  (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Export dairy", sep = "")), ][paste("y_", year, sep = "")])) -
  (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Import dairy", sep = "")), ][paste("y_", year, sep = "")]))

dairy_live_animals <- 0

if(species_lists$species[count] == "cattle"){

dairy_live_animals <- ((as.numeric(inputs_meat[ which(inputs_meat$variable == "Milk Production" & inputs_meat$species == "cattle" & inputs_meat$units == "fraction"), ][paste("y_", year, sep = "")])) * domestic_production_dairy) / 
                                (as.numeric(inputs_meat[ which(inputs_meat$variable == "Milk Production" & inputs_meat$species == "cattle" & inputs_meat$units == "tonnes per animal"), ][paste("y_", year, sep = "")]))

}else if(species_lists$species[count] == "sheep"){
  
  dairy_live_animals <- ((1-as.numeric(inputs_meat[ which(inputs_meat$variable == "Milk Production" & inputs_meat$species == "cattle" & inputs_meat$units == "fraction"), ][paste("y_", year, sep = "")])) * domestic_production_dairy) / 
    (as.numeric(inputs_meat[ which(inputs_meat$variable == "Milk Production Other" & inputs_meat$species == "sheep" & inputs_meat$units == "tonnes per animal"), ][paste("y_", year, sep = "")]))
  
  
}

if(is.nan(dairy_live_animals)){
  
  dairy_live_animals <- 0
  
}

if(species_lists$species[count] == "chicken"){
##Eggs
kcal_eggs <- (as.numeric(inputs_meat[ which(inputs_meat$variable == "Total Food supply"), ][paste("y_", year, sep = "")])) * (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Fraction food supply eggs", sep = "")), ][paste("y_", year, sep = "")]))

kcal_consumed_eggs <- (as.numeric(inputs_meat[ which(inputs_meat$variable == "Total Food supply"), ][paste("y_", year, sep = "")])) * (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Fraction food supply eggs", sep = "")), ][paste("y_", year, sep = "")]))/
  (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Population", sep = "")), ][paste("y_", year, sep = "")]))/365 ##kcal/person/day consumed (i.e. not wasted)

g_consumed_eggs <- kcal_consumed_eggs / (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Kcal per tonne eggs", sep = "")), ][paste("y_", year, sep = "")]))*1000000 #g/person/day

kcal_wasted_eggs <- NA
g_wasted_eggs <- NA


domestic_consumption_eggs <- kcal_eggs / (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Kcal per tonne eggs", sep = "")), ][paste("y_", year, sep = "")]))

domestic_production_eggs <- domestic_consumption_eggs + 
  (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Feed eggs", sep = "")), ][paste("y_", year, sep = "")])) +
  (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Loss eggs", sep = "")), ][paste("y_", year, sep = "")])) +
  (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Stock var eggs", sep = "")), ][paste("y_", year, sep = "")])) +
  (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Export eggs", sep = "")), ][paste("y_", year, sep = "")])) -
  (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Import eggs", sep = "")), ][paste("y_", year, sep = "")]))

eggs_live_animals <- domestic_production_eggs / (as.numeric(inputs_meat[ which(inputs_meat$variable == "Egg production" & inputs_meat$units == "tonnes per animal"), ][paste("y_", year, sep = "")]))

eggs_prod_per_animal <- (as.numeric(inputs_meat[ which(inputs_meat$variable == "Egg production" & inputs_meat$units == "Number of eggs per year"), ][paste("y_", year, sep = "")])) / eggs_live_animals

}


cohort_cattle <- c("AF", "RF", "AM", "RM", "MF", "MM", "MFf", "MMf")
cohort_sheep <- c("AF", "RF", "AM", "RM", "MF", "MM")
cohort_sheep_2 <- c("AF", "RFA", "RFB", "AM", "RMA", "RMB", "MF", "MM")
cohort_sheep_3 <- c("AF", "RF",  "RF",  "AM", "RM",  "RM",  "MF", "MM")
cohort_pig <- c("AF", "RF", "AM", "RM", "M2")
cohort_chicken_backyard <- c("AF", "AM", "RF", "RM", "MF1", "MF2", "MM")
cohort_chicken_layers <- c("AF", "AM", "RF", "RM", "MF1", "MF2", "MF3", "MF4", "MM")
cohort_chicken_broilers <- c("AF", "AM", "RF", "RM", "M2")

mgmt_cattle <- c("mixed", "grassland")
animal_cattle <- c("beef", "dairy")

mgmt_sheep <- c("mixed", "grassland")
animal_sheep <- c("non-dairy", "dairy")

mgmt_pig <- c("backyard", "intermediate", "industrial")
animal_pig <- c("all")

mgmt_chicken <- c("backyard", "layers", "broilers")
animal_chicken <- c("all")



##Herd
if(species_lists$species[count] == "cattle"){



##HERD MODULE - 

##Define AF (Adult Females), AM (Adult Males), RF (Replacement Females), RM (Replacement Males), 
##MF (Meat Females), MM (Meat Males), MFf (Meat Females Feedlot), MMf (Meat Male feedots)
##for Mixed and Grassland groups
AF_dairy_mixed <- dairy_live_animals * (as.numeric(inputs_herd[ which(inputs_herd$variable == "mgmt proportion AF dairy" & inputs_herd$mgmt == "mixed" & inputs_herd$species == "cattle"), ][paste("y_", year, sep = "")]))
AF_dairy_grassland <- dairy_live_animals * (as.numeric(inputs_herd[ which(inputs_herd$variable == "mgmt proportion AF dairy" & inputs_herd$mgmt == "grassland" & inputs_herd$species == "cattle"), ][paste("y_", year, sep = "")]))

total_mixed <- live_animals * (as.numeric(inputs_herd[ which(inputs_herd$variable == "mgmt proportion all" & inputs_herd$mgmt == "mixed" & inputs_herd$species == "cattle"), ][paste("y_", year, sep = "")]))
total_grassland <- live_animals * (as.numeric(inputs_herd[ which(inputs_herd$variable == "mgmt proportion all" & inputs_herd$mgmt == "grassland" & inputs_herd$species == "cattle"), ][paste("y_", year, sep = "")]))

total_feedlot_mixed <- total_mixed *  (as.numeric(inputs_herd[ which(inputs_herd$variable == "FNUM fraction" & inputs_herd$mgmt == "mixed" & inputs_herd$species == "cattle"), ][paste("y_", year, sep = "")]))
total_feedlot_grassland <- total_mixed *  (as.numeric(inputs_herd[ which(inputs_herd$variable == "FNUM fraction" & inputs_herd$mgmt == "grassland" & inputs_herd$species == "cattle"), ][paste("y_", year, sep = "")]))


##Herd Module
for(i in seq(from=1, to=length(mgmt_cattle), by=1)) {

  assign(paste("herd_", mgmt_cattle[i], sep = ""), herd_ruminants(AF_dairy = get(paste("AF_dairy_", mgmt_cattle[i], sep = "")), 
                                                                  NCOWS = get(paste("total_", mgmt_cattle[i], sep = "")), 
                                                                  FNUM = get(paste("total_feedlot_", mgmt_cattle[i], sep = "")),
                                                           Ckg_dairy = (as.numeric(inputs_herd[ which(inputs_herd$variable == "Ckg" & inputs_herd$animal == "dairy" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])), 
                                                           AFkg_dairy = (as.numeric(inputs_herd[ which(inputs_herd$variable == "AFkg" & inputs_herd$animal == "dairy" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])), 
                                                           AMkg_dairy = (as.numeric(inputs_herd[ which(inputs_herd$variable == "AMkg" & inputs_herd$animal == "dairy" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])), 
                                                           MFSkg_dairy = (as.numeric(inputs_herd[ which(inputs_herd$variable == "MFSkg" & inputs_herd$animal == "dairy" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])),
                                                           MMSkg_dairy = (as.numeric(inputs_herd[ which(inputs_herd$variable == "MMSkg" & inputs_herd$animal == "dairy" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])), 
                                                           DR1_dairy = (as.numeric(inputs_herd[ which(inputs_herd$variable == "DR1" & inputs_herd$animal == "dairy" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])), 
                                                           DR1m_dairy = (as.numeric(inputs_herd[ which(inputs_herd$variable == "DR1m" & inputs_herd$animal == "dairy" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])), 
                                                           DR2_dairy = (as.numeric(inputs_herd[ which(inputs_herd$variable == "DR2" & inputs_herd$animal == "dairy" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])), 
                                                           FR_dairy = (as.numeric(inputs_herd[ which(inputs_herd$variable == "FR" & inputs_herd$animal == "dairy" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])), 
                                                           FRRF_dairy = (as.numeric(inputs_herd[ which(inputs_herd$variable == "FRRF" & inputs_herd$animal == "dairy" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])), 
                                                           RRF_dairy = (as.numeric(inputs_herd[ which(inputs_herd$variable == "RRF" & inputs_herd$animal == "dairy" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])),
                                                           AFC_dairy = (as.numeric(inputs_herd[ which(inputs_herd$variable == "AFC" & inputs_herd$animal == "dairy" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])), 
                                                           MFR_dairy = (as.numeric(inputs_herd[ which(inputs_herd$variable == "MFR" & inputs_herd$animal == "dairy" & inputs_herd$mgmt == mgmt_cattle[i] & inputs_herd$species == "cattle"), ][paste("y_", year, sep = "")])), 
                                                           Ckg_beef = (as.numeric(inputs_herd[ which(inputs_herd$variable == "Ckg" & inputs_herd$animal == "beef" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])), 
                                                           AFkg_beef = (as.numeric(inputs_herd[ which(inputs_herd$variable == "AFkg" & inputs_herd$animal == "beef" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])), 
                                                           AMkg_beef = (as.numeric(inputs_herd[ which(inputs_herd$variable == "AMkg" & inputs_herd$animal == "beef" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])), 
                                                           MFSkg_beef = (as.numeric(inputs_herd[ which(inputs_herd$variable == "MFSkg" & inputs_herd$animal == "beef" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])),
                                                           MMSkg_beef = (as.numeric(inputs_herd[ which(inputs_herd$variable == "MMSkg" & inputs_herd$animal == "beef" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])), 
                                                           DR1_beef = (as.numeric(inputs_herd[ which(inputs_herd$variable == "DR1" & inputs_herd$animal == "beef" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])), 
                                                           DR1m_beef = (as.numeric(inputs_herd[ which(inputs_herd$variable == "DR1m" & inputs_herd$animal == "beef" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])), 
                                                           DR2_beef = (as.numeric(inputs_herd[ which(inputs_herd$variable == "DR2" & inputs_herd$animal == "beef" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])), 
                                                           FR_beef = (as.numeric(inputs_herd[ which(inputs_herd$variable == "FR" & inputs_herd$animal == "beef" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])), 
                                                           FRRF_beef = (as.numeric(inputs_herd[ which(inputs_herd$variable == "FRRF" & inputs_herd$animal == "beef" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])), 
                                                           RRF_beef = (as.numeric(inputs_herd[ which(inputs_herd$variable == "RRF" & inputs_herd$animal == "beef" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])),
                                                           AFC_beef = (as.numeric(inputs_herd[ which(inputs_herd$variable == "AFC" & inputs_herd$animal == "beef" & inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i]), ][paste("y_", year, sep = "")])), 
                                                           MFR_beef = (as.numeric(inputs_herd[ which(inputs_herd$variable == "MFR" & inputs_herd$animal == "beef" & inputs_herd$mgmt == mgmt_cattle[i] & inputs_herd$species == "cattle"), ][paste("y_", year, sep = "")])), 
                                                           FATTDAY = (as.numeric(inputs_herd[ which(inputs_herd$variable == "FATTDAY" & inputs_herd$animal == "beef" & inputs_herd$species == "cattle"), ][paste("y_", year, sep = "")])),  
                                                           LWSTARTF = (as.numeric(inputs_herd[ which(inputs_herd$variable == "LWSTARTF" & inputs_herd$animal == "beef" & inputs_herd$species == "cattle"), ][paste("y_", year, sep = "")])),  
                                                           LWENDF = (as.numeric(inputs_herd[ which(inputs_herd$variable == "LWENDF" & inputs_herd$animal == "beef" & inputs_herd$species == "cattle"), ][paste("y_", year, sep = "")])),   
                                                           LWSTARTM = (as.numeric(inputs_herd[ which(inputs_herd$variable == "LWSTARTM" & inputs_herd$animal == "beef" & inputs_herd$species == "cattle"), ][paste("y_", year, sep = "")])),  
                                                           LWENDM = (as.numeric(inputs_herd[ which(inputs_herd$variable == "LWENDM" & inputs_herd$animal == "beef" & inputs_herd$species == "cattle"), ][paste("y_", year, sep = "")]))))  
}  



}else if(species_lists$species[count] == "sheep"){



total_mixed <- live_animals * (as.numeric(inputs_herd[ which(inputs_herd$variable == "mgmt proportion all" & inputs_herd$mgmt == "mixed" & inputs_herd$species == "sheep"), ][paste("y_", year, sep = "")]))
total_grassland <- live_animals * (as.numeric(inputs_herd[ which(inputs_herd$variable == "mgmt proportion all" & inputs_herd$mgmt == "grassland"  & inputs_herd$species == "sheep"), ][paste("y_", year, sep = "")]))


DSR_value <- dairy_live_animals / live_animals


AF_dairy_mixed <- total_mixed * DSR_value * (as.numeric(inputs_herd[ which(inputs_herd$variable == "mgmt proportion AF" & inputs_herd$mgmt == "mixed" & inputs_herd$species == "sheep"), ][paste("y_", year, sep = "")]))
AF_nondairy_mixed <- total_mixed * (1-DSR_value) * (as.numeric(inputs_herd[ which(inputs_herd$variable == "mgmt proportion AF" & inputs_herd$mgmt == "mixed" & inputs_herd$species == "sheep"), ][paste("y_", year, sep = "")]))

AF_dairy_grassland <- total_grassland * DSR_value * (as.numeric(inputs_herd[ which(inputs_herd$variable == "mgmt proportion AF" & inputs_herd$mgmt == "grassland" & inputs_herd$species == "sheep"), ][paste("y_", year, sep = "")]))
AF_nondairy_grassland <- total_grassland * (1-DSR_value) * (as.numeric(inputs_herd[ which(inputs_herd$variable == "mgmt proportion AF" & inputs_herd$mgmt == "grassland" & inputs_herd$species == "sheep"), ][paste("y_", year, sep = "")]))


for(i in seq(from=1, to=length(mgmt_sheep), by=1)) {

  assign(paste("herd_", mgmt_sheep[i], sep = ""), herd_sheepgoat(NANIMAL = get(paste("total_", mgmt_sheep[i], sep = "")),
                                                           AF_dairy = get(paste("AF_dairy_", mgmt_sheep[i], sep = "")),
                                                           AF_nondairy = get(paste("AF_nondairy_", mgmt_sheep[i], sep = "")),
                                                           Ckg = (as.numeric(inputs_herd[ which(inputs_herd$variable == "Ckg" & inputs_herd$species == "sheep"), ][paste("y_", year, sep = "")])),  
                                                           AFkg = (as.numeric(inputs_herd[ which(inputs_herd$variable == "AFkg" & inputs_herd$species == "sheep"), ][paste("y_", year, sep = "")])),   
                                                           AMkg = (as.numeric(inputs_herd[ which(inputs_herd$variable == "AMkg" & inputs_herd$species == "sheep"), ][paste("y_", year, sep = "")])),   
                                                           MFSkg = (as.numeric(inputs_herd[ which(inputs_herd$variable == "MFSkg" & inputs_herd$species == "sheep"), ][paste("y_", year, sep = "")])),   
                                                           MMSkg = (as.numeric(inputs_herd[ which(inputs_herd$variable == "MMSkg" & inputs_herd$species == "sheep"), ][paste("y_", year, sep = "")])),   
                                                           DR1 = (as.numeric(inputs_herd[ which(inputs_herd$variable == "DR1" & inputs_herd$species == "sheep"), ][paste("y_", year, sep = "")])),  
                                                           DR2 = (as.numeric(inputs_herd[ which(inputs_herd$variable == "DR2" & inputs_herd$species == "sheep"), ][paste("y_", year, sep = "")])),  
                                                           FR = (as.numeric(inputs_herd[ which(inputs_herd$variable == "FR" & inputs_herd$species == "sheep"), ][paste("y_", year, sep = "")])),   
                                                           FRRF = (as.numeric(inputs_herd[ which(inputs_herd$variable == "FRRF" & inputs_herd$species == "sheep"), ][paste("y_", year, sep = "")])),   
                                                           RRF = (as.numeric(inputs_herd[ which(inputs_herd$variable == "RRF" & inputs_herd$species == "sheep"), ][paste("y_", year, sep = "")])),  
                                                           AFC = (as.numeric(inputs_herd[ which(inputs_herd$variable == "AFC" & inputs_herd$species == "sheep"), ][paste("y_", year, sep = "")])),   
                                                           DSR = DSR_value,   
                                                           MFR = (as.numeric(inputs_herd[ which(inputs_herd$variable == "MFR" & inputs_herd$species == "sheep"), ][paste("y_", year, sep = "")])),   
                                                           LINT = (as.numeric(inputs_herd[ which(inputs_herd$variable == "LINT" & inputs_herd$species == "sheep"), ][paste("y_", year, sep = "")])),   
                                                           LITSIZE = (as.numeric(inputs_herd[ which(inputs_herd$variable == "LITSIZE" & inputs_herd$species == "sheep"), ][paste("y_", year, sep = "")]))))


}  
  


}else if(species_lists$species[count] == "pig"){







total_backyard <- live_animals * (as.numeric(inputs_herd[ which(inputs_herd$variable == "mgmt proportion all" & inputs_herd$mgmt == "backyard" & inputs_herd$species == "pig"), ][paste("y_", year, sep = "")]))
total_intermediate <- live_animals * (as.numeric(inputs_herd[ which(inputs_herd$variable == "mgmt proportion all" & inputs_herd$mgmt == "intermediate"  & inputs_herd$species == "pig"), ][paste("y_", year, sep = "")]))
total_industrial <- live_animals * (as.numeric(inputs_herd[ which(inputs_herd$variable == "mgmt proportion all" & inputs_herd$mgmt == "industrial"  & inputs_herd$species == "pig"), ][paste("y_", year, sep = "")]))

AF_backyard <- total_backyard * (as.numeric(inputs_herd[ which(inputs_herd$variable == "mgmt proportion AF" & inputs_herd$mgmt == "backyard" & inputs_herd$species == "pig"), ][paste("y_", year, sep = "")]))
AF_intermediate <- total_intermediate * (as.numeric(inputs_herd[ which(inputs_herd$variable == "mgmt proportion AF" & inputs_herd$mgmt == "intermediate" & inputs_herd$species == "pig"), ][paste("y_", year, sep = "")]))
AF_industrial <- total_industrial * (as.numeric(inputs_herd[ which(inputs_herd$variable == "mgmt proportion AF" & inputs_herd$mgmt == "industrial" & inputs_herd$species == "pig"), ][paste("y_", year, sep = "")]))



for(i in seq(from=1, to=length(mgmt_pig), by=1)) {
  
  assign(paste("herd_", mgmt_pig[i], sep = ""), herd_pigs(NPIGS = get(paste("total_", mgmt_pig[i], sep = "")), 
                                                      AF = get(paste("AF_", mgmt_pig[i], sep = "")),    
                                                      Ckg = (as.numeric(inputs_herd[ which(inputs_herd$variable == "Ckg" & inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i]), ][paste("y_", year, sep = "")])), 
                                                      Wkg = (as.numeric(inputs_herd[ which(inputs_herd$variable == "Wkg" & inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i]), ][paste("y_", year, sep = "")])), 
                                                      AFkg = (as.numeric(inputs_herd[ which(inputs_herd$variable == "AFkg" & inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i]), ][paste("y_", year, sep = "")])), 
                                                      AMkg = (as.numeric(inputs_herd[ which(inputs_herd$variable == "AMkg" & inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i]), ][paste("y_", year, sep = "")])), 
                                                      M2Skg = (as.numeric(inputs_herd[ which(inputs_herd$variable == "M2Skg" & inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i]), ][paste("y_", year, sep = "")])), 
                                                      DR1 = (as.numeric(inputs_herd[ which(inputs_herd$variable == "DR1" & inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i]), ][paste("y_", year, sep = "")])), 
                                                      DRR2A = (as.numeric(inputs_herd[ which(inputs_herd$variable == "DRR2A" & inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i]), ][paste("y_", year, sep = "")])), 
                                                      DRR2B = (as.numeric(inputs_herd[ which(inputs_herd$variable == "DRR2B" & inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i]), ][paste("y_", year, sep = "")])), 
                                                      DRF2 = (as.numeric(inputs_herd[ which(inputs_herd$variable == "DRF2" & inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i]), ][paste("y_", year, sep = "")])), 
                                                      FR = (as.numeric(inputs_herd[ which(inputs_herd$variable == "FR" & inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i]), ][paste("y_", year, sep = "")])), 
                                                      FRRF = (as.numeric(inputs_herd[ which(inputs_herd$variable == "FRRF" & inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i]), ][paste("y_", year, sep = "")])),
                                                      RRF = (as.numeric(inputs_herd[ which(inputs_herd$variable == "RRF" & inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i]), ][paste("y_", year, sep = "")])), 
                                                      RRM = (as.numeric(inputs_herd[ which(inputs_herd$variable == "RRM" & inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i]), ][paste("y_", year, sep = "")])),
                                                      WA = (as.numeric(inputs_herd[ which(inputs_herd$variable == "WA" & inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i]), ][paste("y_", year, sep = "")])), 
                                                      MFR = (as.numeric(inputs_herd[ which(inputs_herd$variable == "MFR" & inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i]), ][paste("y_", year, sep = "")])), 
                                                      DWG2 = (as.numeric(inputs_herd[ which(inputs_herd$variable == "DWG2" & inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i]), ][paste("y_", year, sep = "")])), 
                                                      LITSIZE = (as.numeric(inputs_herd[ which(inputs_herd$variable == "LITSIZE" & inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i]), ][paste("y_", year, sep = "")]))))
}




}else if(species_lists$species[count] == "chicken"){



total_backyard <- live_animals * (as.numeric(inputs_herd[ which(inputs_herd$variable == "mgmt proportion all" & inputs_herd$mgmt == "backyard" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")]))
total_layers <- live_animals * (as.numeric(inputs_herd[ which(inputs_herd$variable == "mgmt proportion all" & inputs_herd$mgmt == "layers"  & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")]))
total_broilers <- live_animals * (as.numeric(inputs_herd[ which(inputs_herd$variable == "mgmt proportion all" & inputs_herd$mgmt == "broilers"  & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")]))

AF_backyard <- total_backyard * (as.numeric(inputs_herd[ which(inputs_herd$variable == "mgmt proportion AF" & inputs_herd$mgmt == "backyard" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")]))
AF_layers <- total_layers * (as.numeric(inputs_herd[ which(inputs_herd$variable == "mgmt proportion AF" & inputs_herd$mgmt == "layers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")]))
AF_broilers <- total_broilers * (as.numeric(inputs_herd[ which(inputs_herd$variable == "mgmt proportion AF" & inputs_herd$mgmt == "broilers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")]))


herd_backyard <- herd_chickens_backyard(AFC = (as.numeric(inputs_herd[ which(inputs_herd$variable == "AFC" & inputs_herd$mgmt == "backyard" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        NCHK = total_backyard, 
                                        AF = AF_backyard,
                                        Ckg = (as.numeric(inputs_herd[ which(inputs_herd$variable == "Ckg" & inputs_herd$mgmt == "backyard" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        AF2kg = (as.numeric(inputs_herd[ which(inputs_herd$variable == "AF2kg" & inputs_herd$mgmt == "backyard" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        AM2kg = (as.numeric(inputs_herd[ which(inputs_herd$variable == "AM2kg" & inputs_herd$mgmt == "backyard" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        M2Skg = (as.numeric(inputs_herd[ which(inputs_herd$variable == "M2Skg" & inputs_herd$mgmt == "backyard" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        DR1 = (as.numeric(inputs_herd[ which(inputs_herd$variable == "DR1" & inputs_herd$mgmt == "backyard" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        FRRF = (as.numeric(inputs_herd[ which(inputs_herd$variable == "FRRF" & inputs_herd$mgmt == "backyard" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        DR2 = (as.numeric(inputs_herd[ which(inputs_herd$variable == "DR2" & inputs_herd$mgmt == "backyard" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])),
                                        MFR = (as.numeric(inputs_herd[ which(inputs_herd$variable == "MFR" & inputs_herd$mgmt == "backyard" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        EGGSyear = (as.numeric(inputs_herd[ which(inputs_herd$variable == "EGGSyear" & inputs_herd$mgmt == "backyard" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])),  
                                        EGGwght = (as.numeric(inputs_herd[ which(inputs_herd$variable == "Eggwght" & inputs_herd$mgmt == "backyard" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        HATCH = (as.numeric(inputs_herd[ which(inputs_herd$variable == "HATCH" & inputs_herd$mgmt == "backyard" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        AFS = (as.numeric(inputs_herd[ which(inputs_herd$variable == "AFS" & inputs_herd$mgmt == "backyard" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        CYCLE = (as.numeric(inputs_herd[ which(inputs_herd$variable == "CYCLE" & inputs_herd$mgmt == "backyard" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])),
                                        CLTSIZE = (as.numeric(inputs_herd[ which(inputs_herd$variable == "CLTSIZE" & inputs_herd$mgmt == "backyard" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])))

herd_layers <- herd_chickens_layers(AFC = (as.numeric(inputs_herd[ which(inputs_herd$variable == "AFC" & inputs_herd$mgmt == "layers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                    NCHK = total_layers, 
                                    AF = AF_layers,
                                    Ckg = (as.numeric(inputs_herd[ which(inputs_herd$variable == "Ckg" & inputs_herd$mgmt == "layers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                    AF1kg = (as.numeric(inputs_herd[ which(inputs_herd$variable == "AF1kg" & inputs_herd$mgmt == "layers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                    AF2kg = (as.numeric(inputs_herd[ which(inputs_herd$variable == "AF2kg" & inputs_herd$mgmt == "layers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                    DR1 = (as.numeric(inputs_herd[ which(inputs_herd$variable == "DR1" & inputs_herd$mgmt == "layers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                    FRRF = (as.numeric(inputs_herd[ which(inputs_herd$variable == "FRRF" & inputs_herd$mgmt == "layers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                    DRL2 = (as.numeric(inputs_herd[ which(inputs_herd$variable == "DRL2" & inputs_herd$mgmt == "layers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                    DRM = (as.numeric(inputs_herd[ which(inputs_herd$variable == "DRM" & inputs_herd$mgmt == "layers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])),
                                    MFR = (as.numeric(inputs_herd[ which(inputs_herd$variable == "MFR" & inputs_herd$mgmt == "layers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                    EGGSyear = (as.numeric(inputs_herd[ which(inputs_herd$variable == "EGGSyear" & inputs_herd$mgmt == "layers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                    EGGwght = (as.numeric(inputs_herd[ which(inputs_herd$variable == "EGGwght" & inputs_herd$mgmt == "layers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                    HATCH = (as.numeric(inputs_herd[ which(inputs_herd$variable == "HATCH" & inputs_herd$mgmt == "layers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                    LAY1weeks = (as.numeric(inputs_herd[ which(inputs_herd$variable == "LAY1weeks" & inputs_herd$mgmt == "layers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                    LAY2weeks = (as.numeric(inputs_herd[ which(inputs_herd$variable == "LAY2weeks" & inputs_herd$mgmt == "layers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                    MOLTweeks = (as.numeric(inputs_herd[ which(inputs_herd$variable == "MOLTweeks" & inputs_herd$mgmt == "layers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])))

herd_broilers <- herd_chickens_broilers(AFC = (as.numeric(inputs_herd[ which(inputs_herd$variable == "AFC" & inputs_herd$mgmt == "broilers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        NCHK = total_broilers,
                                        AF = AF_broilers,
                                        Ckg = (as.numeric(inputs_herd[ which(inputs_herd$variable == "Ckg" & inputs_herd$mgmt == "broilers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        AF1kg = (as.numeric(inputs_herd[ which(inputs_herd$variable == "AF1kg" & inputs_herd$mgmt == "broilers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        AF2kg = (as.numeric(inputs_herd[ which(inputs_herd$variable == "AF2kg" & inputs_herd$mgmt == "broilers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        M2Skg = (as.numeric(inputs_herd[ which(inputs_herd$variable == "M2Skg" & inputs_herd$mgmt == "broilers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        DR1 = (as.numeric(inputs_herd[ which(inputs_herd$variable == "DR1" & inputs_herd$mgmt == "broilers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        FRRF = (as.numeric(inputs_herd[ which(inputs_herd$variable == "FRRF" & inputs_herd$mgmt == "broilers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        DRB2 = (as.numeric(inputs_herd[ which(inputs_herd$variable == "DRB2" & inputs_herd$mgmt == "broilers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        DRL2 = (as.numeric(inputs_herd[ which(inputs_herd$variable == "DRL2" & inputs_herd$mgmt == "broilers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])),
                                        MFR = (as.numeric(inputs_herd[ which(inputs_herd$variable == "MFR" & inputs_herd$mgmt == "broilers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        EGGSyear = (as.numeric(inputs_herd[ which(inputs_herd$variable == "EGGSyear" & inputs_herd$mgmt == "broilers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        EGGwght = (as.numeric(inputs_herd[ which(inputs_herd$variable == "EGGwght" & inputs_herd$mgmt == "broilers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        HATCH = (as.numeric(inputs_herd[ which(inputs_herd$variable == "HATCH" & inputs_herd$mgmt == "broilers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        A2S = (as.numeric(inputs_herd[ which(inputs_herd$variable == "A2S" & inputs_herd$mgmt == "broilers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        BIDLE = (as.numeric(inputs_herd[ which(inputs_herd$variable == "BIDLE" & inputs_herd$mgmt == "broilers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])), 
                                        LAYweeks = (as.numeric(inputs_herd[ which(inputs_herd$variable == "LAYweeks" & inputs_herd$mgmt == "broilers" & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])))

}



##Feed
if(species_lists$species[count] == "cattle"){


##feed digestibility
for(i in seq(from=1, to=length(mgmt_cattle), by=1)) {
  for(j in seq(from=1, to=length(animal_cattle), by=1)) {
    for(k in seq(from=1, to=length(cohort_cattle), by=1)) {

      assign(paste("feed_di", cohort_cattle[k], animal_cattle[j], mgmt_cattle[i], sep = "_"), feed_digestibility(
                                                                                            pct = as.numeric(inputs_feed_cattle[inputs_feed_cattle$mgmt == mgmt_cattle[i] & inputs_feed_cattle$animal == animal_cattle[j] & inputs_feed_cattle$cohort == cohort_cattle[k], ][paste("y_", year, sep = "")][[1]]), 
                                                                                            di = feed_ruminants["di"][[1]], 
                                                                                            ge = feed_ruminants["ge"][[1]], 
                                                                                            n_cont = feed_ruminants["n_cont"][[1]]))
    }
  }
}

}else if(species_lists$species[count] == "sheep"){
  
  for(i in seq(from=1, to=length(mgmt_sheep), by=1)) {
    for(j in seq(from=1, to=length(animal_sheep), by=1)) {
      for(k in seq(from=1, to=length(cohort_sheep), by=1)) {
        
        assign(paste("feed_di", cohort_sheep[k], animal_sheep[j], mgmt_sheep[i], sep = "_"), feed_digestibility(pct = as.numeric(inputs_feed_sheep[inputs_feed_sheep$mgmt == mgmt_sheep[i] & inputs_feed_sheep$animal == animal_sheep[j] & inputs_feed_sheep$cohort == cohort_sheep[k], ][paste("y_", year, sep = "")][[1]]), 
                                                                                              di = feed_ruminants["di"][[1]], 
                                                                                              ge = feed_ruminants["ge"][[1]], 
                                                                                              n_cont = feed_ruminants["n_cont"][[1]]))
      }
    }
  }
  
  
  
  
  
}else if(species_lists$species[count] == "pig"){
  
  
  for(i in seq(from=1, to=length(mgmt_pig), by=1)) {
        
        assign(paste("feed_di", mgmt_pig[i], sep = "_"), feed_digestibility_monogastrics(pct = as.numeric(inputs_feed_pig[inputs_feed_pig$mgmt == mgmt_pig[i], ][paste("y_", year, sep = "")][[1]]), 
                                                                                              di = feed_monogastrics["di"][[1]], 
                                                                                              ge = feed_monogastrics["ge"][[1]], 
                                                                                              me = feed_monogastrics["me_pig"][[1]], 
                                                                                              n_cont = feed_monogastrics["n_cont"][[1]]))
      }
  
  
  
  
  
  
}else if(species_lists$species[count] == "chicken"){
  
  
  for(i in seq(from=1, to=length(mgmt_chicken), by=1)) {
    
    assign(paste("feed_di", mgmt_chicken[i], sep = "_"), feed_digestibility_monogastrics(pct = as.numeric(inputs_feed_chicken[inputs_feed_chicken$mgmt == mgmt_chicken[i], ][paste("y_", year, sep = "")][[1]]), 
                                                                                     di = feed_monogastrics["di"][[1]], 
                                                                                     ge = feed_monogastrics["ge"][[1]], 
                                                                                     me = feed_monogastrics["me_chicken"][[1]], 
                                                                                     n_cont = feed_monogastrics["n_cont"][[1]]))
  }
  
  
  
}



##Energy intake
if(species_lists$species[count] == "cattle"){





##energy intake
for(i in seq(from=1, to=length(mgmt_cattle), by=1)) {
  for(j in seq(from=1, to=length(animal_cattle), by=1)) {
    for(k in seq(from=1, to=length(cohort_cattle), by=1)) {
    
      assign(paste("energy_intake", cohort_cattle[k], animal_cattle[j], mgmt_cattle[i], sep = "_"), 
           energy_intake(cohort = cohort_cattle[k], 
                         live_weight = if(cohort_cattle[k] == "AF" | cohort_cattle[k] == "AM"){
                           as.numeric(inputs_herd[ which(inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i] & inputs_herd$animal == animal_cattle[j] & inputs_herd$variable == paste(cohort_cattle[k], "kg", sep = "")), ][paste("y_", year, sep = "")])
                              }else if(cohort_cattle[k] == "RF"){as.numeric(get(paste("herd", mgmt_cattle[i], sep = "_"))[paste("RFkg_", animal_cattle[j], sep = "")])
                                }else if(cohort_cattle[k] == "RM"){as.numeric(get(paste("herd", mgmt_cattle[i], sep = "_"))[paste("RMkg_", animal_cattle[j], sep = "")])
                                  }else if(cohort_cattle[k] == "MF"){as.numeric(get(paste("herd", mgmt_cattle[i], sep = "_"))[paste("MFkg_", animal_cattle[j], sep = "")])
                                    }else if(cohort_cattle[k] == "MM"){as.numeric(get(paste("herd", mgmt_cattle[i], sep = "_"))[paste("MMkg_", animal_cattle[j], sep = "")])
                                      }else if(cohort_cattle[k] == "MFf"){as.numeric(get(paste("herd", mgmt_cattle[i], sep = "_"))[paste("MFfkg_", animal_cattle[j], sep = "")])
                                       } else if(cohort_cattle[k] == "MMf"){as.numeric(get(paste("herd", mgmt_cattle[i], sep = "_"))[paste("MMfkg_", animal_cattle[j], sep = "")])
                                       }, 
                         Cmain = as.numeric(inputs_herd[ which(inputs_herd$species == "cattle" & inputs_herd$cohort == cohort_cattle[k] & inputs_herd$variable == "Cmain"), ][paste("y_", year, sep = "")]), 
                         Cact = as.numeric(inputs_herd[ which(inputs_herd$species == "cattle" & inputs_herd$variable == "Cact Pasture"), ][paste("y_", year, sep = "")]) * if(cohort_cattle[k] == "MFf" | cohort_cattle[k] == "MMf"){
                           as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == "feedlot", ][paste("y_", year, sep = "")][[1]][1])/100
                         }else{
                           as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == animal_cattle[j], ][paste("y_", year, sep = "")][[1]][1])/100
                         }, ##assume pasture
                         DWG = if(cohort_cattle[k] == "AF" | cohort_cattle[k] == "RF" | cohort_cattle[k] == "MF" | cohort_cattle[k] == "MFf"){as.numeric(get(paste("herd", mgmt_cattle[i], sep = "_"))[paste("DWGF_", animal_cattle[j], sep = "")])}else{
                                        as.numeric(get(paste("herd", mgmt_cattle[i], sep = "_"))[paste("DWGM_", animal_cattle[j], sep = "")])}, 
                         Akg = if(cohort_cattle[k] == "AF" | cohort_cattle[k] == "RF" | cohort_cattle[k] == "MF" | cohort_cattle[k] == "MFf"){
                           as.numeric(inputs_herd[ which(inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i] & inputs_herd$animal == animal_cattle[j] & inputs_herd$variable == "AFkg"), ][paste("y_", year, sep = "")])
                         }else{as.numeric(inputs_herd[ which(inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i] & inputs_herd$animal == animal_cattle[j] & inputs_herd$variable == "AMkg"), ][paste("y_", year, sep = "")])}, 
                         
                         Mfkg = if(cohort_cattle[k] == "AF" | cohort_cattle[k] == "RF" | cohort_cattle[k] == "MF" | cohort_cattle[k] == "MFf"){
                           as.numeric(get(paste("herd", mgmt_cattle[i], sep = "_"))[paste("MFfkg_", animal_cattle[j], sep = "")])}else{
                             as.numeric(get(paste("herd", mgmt_cattle[i], sep = "_"))[paste("MMfkg_", animal_cattle[j], sep = "")])},
                         
                         DWGFX = if(cohort_cattle[k] == "AF" | cohort_cattle[k] == "RF" | cohort_cattle[k] == "MF" | cohort_cattle[k] == "MFf"){as.numeric(get(paste("herd", mgmt_cattle[i], sep = "_"))[paste("DWGFF_", animal_cattle[j], sep = "")])}else{
                                        as.numeric(get(paste("herd", mgmt_cattle[i], sep = "_"))[paste("DWGFM_", animal_cattle[j], sep = "")])}, 
                         
                         LWEND = if(cohort_cattle[k] == "AF" | cohort_cattle[k] == "RF" | cohort_cattle[k] == "MF" | cohort_cattle[k] == "MFf"){
                           as.numeric(inputs_herd[ which(inputs_herd$species == "cattle" & inputs_herd$variable == "LWENDF"), ][paste("y_", year, sep = "")])}else{
                             as.numeric(inputs_herd[ which(inputs_herd$species == "cattle" & inputs_herd$variable == "LWENDM"), ][paste("y_", year, sep = "")])}, 
                         
                         Cgro = if(cohort_cattle[k] == "AF" | cohort_cattle[k] == "AM"){
                           0}else{as.numeric(inputs_herd[ which(inputs_herd$species == "cattle" & inputs_herd$cohort == cohort_cattle[k] & inputs_herd$variable == "Cgro"), ][paste("y_", year, sep = "")])},
                         milk_prod = if(animal_cattle[j] == "beef"){0}else{(as.numeric(inputs_meat[ which(inputs_meat$variable == "Milk Production" & inputs_meat$units == "tonnes per animal"), ][paste("y_", year, sep = "")][[1]])*1000)/365}, 
                         milk_fat_pct = as.numeric(inputs_herd[ which(inputs_herd$species == "cattle" & inputs_herd$variable == "Milk fat content"), ][paste("y_", year, sep = "")]), 
                         hour_work = 0, 
                         FR = as.numeric(inputs_herd[ which(inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i] & inputs_herd$animal == animal_cattle[j] & inputs_herd$variable == "FR"), ][paste("y_", year, sep = "")]), 
                         AFC = as.numeric(inputs_herd[ which(inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i] & inputs_herd$animal == animal_cattle[j] & inputs_herd$variable == "AFC"), ][paste("y_", year, sep = "")]), 
                         ev_fibre = 0, 
                         prod_fibre =0, 
                         diet_di = as.numeric(get(paste("feed_di", cohort_cattle[k], animal_cattle[j], mgmt_cattle[i], sep = "_"))$di),
                         diet_ge = as.numeric(get(paste("feed_di", cohort_cattle[k], animal_cattle[j], mgmt_cattle[i], sep = "_"))$ge)))
 
    }}}



}else if(species_lists$species[count] == "sheep"){
  
  
  for(i in seq(from=1, to=length(mgmt_sheep), by=1)) {
      for(k in seq(from=1, to=length(cohort_sheep_2), by=1)) {
        
  
        assign(paste("energy_intake", cohort_sheep_2[k], "non-dairy", mgmt_sheep[i], sep = "_"), 
               energy_intake_small_ruminants(cohort = cohort_sheep_2[k], 
                                live_weight = if(cohort_sheep_2[k] == "AF" | cohort_sheep_2[k] == "AM"){
                                  as.numeric(inputs_herd[ which(inputs_herd$species == "sheep" & inputs_herd$variable == paste(cohort_sheep_2[k], "kg", sep = "")), ][paste("y_", year, sep = "")])
                                }else if(cohort_sheep_2[k] == "RFA"){as.numeric(get(paste("herd", mgmt_sheep[i], sep = "_"))["RFAkg"])
                                }else if(cohort_sheep_2[k] == "RFB"){as.numeric(get(paste("herd", mgmt_sheep[i], sep = "_"))["RFBkg"])
                                }else if(cohort_sheep_2[k] == "RMA"){as.numeric(get(paste("herd", mgmt_sheep[i], sep = "_"))["RMAkg"])
                                }else if(cohort_sheep_2[k] == "RMB"){as.numeric(get(paste("herd", mgmt_sheep[i], sep = "_"))["RMBkg"])
                                }else if(cohort_sheep_2[k] == "MF"){as.numeric(get(paste("herd", mgmt_sheep[i], sep = "_"))["MFkg"])
                                }else if(cohort_sheep_2[k] == "MM"){as.numeric(get(paste("herd", mgmt_sheep[i], sep = "_"))["MMkg"])
                                }, 
                                
                                Cmain = as.numeric(inputs_herd[ which(inputs_herd$species == "sheep" & inputs_herd$cohort == cohort_sheep_2[k] & inputs_herd$variable == "Cmain"), ][paste("y_", year, sep = "")]), 
                                Cact = as.numeric(inputs_herd[ which(inputs_herd$species == "sheep" & inputs_herd$variable == "Flat Pasture"), ][paste("y_", year, sep = "")]), ##assume pasture
                                DWG = if(cohort_sheep_2[k] == "AF" | cohort_cattle[k] == "RFA" | cohort_cattle[k] == "RFB" | cohort_cattle[k] == "MF"){as.numeric(get(paste("herd", mgmt_sheep[i], sep = "_"))["DWGF"])}else{
                                  as.numeric(get(paste("herd", mgmt_sheep[i], sep = "_"))["DWGM"])}, 
                                Ckg = as.numeric(inputs_herd[ which(inputs_herd$species == "sheep" & inputs_herd$variable == "Ckg"), ][paste("y_", year, sep = "")]), 
                                
                                milk_prod = 0, 
                                ev_milk = as.numeric(inputs_herd[ which(inputs_herd$species == "sheep" & inputs_herd$variable == "Net energy milk"), ][paste("y_", year, sep = "")]), 
                                ev_fibre = as.numeric(inputs_herd[ which(inputs_herd$species == "sheep" & inputs_herd$variable == "Net energy fibre"), ][paste("y_", year, sep = "")]), 
                                prod_fibre =0,
                                LITSIZE = as.numeric(inputs_herd[ which(inputs_herd$species == "sheep" & inputs_herd$variable == "LITSIZE"), ][paste("y_", year, sep = "")]), 
                                FR = as.numeric(inputs_herd[ which(inputs_herd$species == "sheep" & inputs_herd$variable == "FR"), ][paste("y_", year, sep = "")]), 
                                LINT = as.numeric(inputs_herd[ which(inputs_herd$species == "sheep" & inputs_herd$variable == "LINT"), ][paste("y_", year, sep = "")]),  
                                
                                diet_di = if(cohort_sheep_2[k] == "AF" | cohort_sheep_2[k] == "AM" | cohort_sheep_2[k] == "MF" | cohort_sheep_2[k] == "MM"){
                                              as.numeric(get(paste("feed_di", cohort_sheep_2[k], "non-dairy", mgmt_sheep[i], sep = "_"))$di) }else if(cohort_sheep_2[k] == "RFA" | cohort_sheep_2[k] == "RFB"){
                                                as.numeric(get(paste("feed_di_RF_non-dairy", mgmt_sheep[i], sep = "_"))$di)}else if(cohort_sheep_2[k] == "RMA" | cohort_sheep_2[k] == "RMB"){
                                                  as.numeric(get(paste("feed_di_RM_non-dairy", mgmt_sheep[i], sep = "_"))$di)},
                                diet_ge = if(cohort_sheep_2[k] == "AF" | cohort_sheep_2[k] == "AM" | cohort_sheep_2[k] == "MF" | cohort_sheep_2[k] == "MM"){
                                  as.numeric(get(paste("feed_di", cohort_sheep_2[k], "non-dairy", mgmt_sheep[i], sep = "_"))$ge) }else if(cohort_sheep_2[k] == "RFA" | cohort_sheep_2[k] == "RFB"){
                                    as.numeric(get(paste("feed_di_RF_non-dairy", mgmt_sheep[i], sep = "_"))$ge)}else if(cohort_sheep_2[k] == "RMA" | cohort_sheep_2[k] == "RMB"){
                                      as.numeric(get(paste("feed_di_RM_non-dairy", mgmt_sheep[i], sep = "_"))$ge)}))
  
       
        assign(paste("energy_intake", cohort_sheep_2[k], "dairy", mgmt_sheep[i], sep = "_"), 
               energy_intake_small_ruminants(cohort = cohort_sheep_2[k], 
                                             live_weight = if(cohort_sheep_2[k] == "AF" | cohort_sheep_2[k] == "AM"){
                                               as.numeric(inputs_herd[ which(inputs_herd$species == "sheep" & inputs_herd$variable == paste(cohort_sheep[k], "kg", sep = "")), ][paste("y_", year, sep = "")])
                                             }else if(cohort_sheep_2[k] == "RFA"){as.numeric(get(paste("herd", mgmt_sheep[i], sep = "_"))["RFAkg"])
                                             }else if(cohort_sheep_2[k] == "RFB"){as.numeric(get(paste("herd", mgmt_sheep[i], sep = "_"))["RFBkg"])
                                             }else if(cohort_sheep_2[k] == "RMA"){as.numeric(get(paste("herd", mgmt_sheep[i], sep = "_"))["RMAkg"])
                                             }else if(cohort_sheep_2[k] == "RMB"){as.numeric(get(paste("herd", mgmt_sheep[i], sep = "_"))["RMBkg"])
                                             }else if(cohort_sheep_2[k] == "MF"){as.numeric(get(paste("herd", mgmt_sheep[i], sep = "_"))["MFkg"])
                                             }else if(cohort_sheep_2[k] == "MM"){as.numeric(get(paste("herd", mgmt_sheep[i], sep = "_"))["MMkg"])
                                             }, 
                                             
                                             Cmain = as.numeric(inputs_herd[ which(inputs_herd$species == "sheep" & inputs_herd$cohort == cohort_sheep_2[k] & inputs_herd$variable == "Cmain"), ][paste("y_", year, sep = "")]), 
                                             Cact = as.numeric(inputs_herd[ which(inputs_herd$species == "sheep" & inputs_herd$variable == "Flat Pasture"), ][paste("y_", year, sep = "")]), ##assume pasture
                                             DWG = if(cohort_sheep_2[k] == "AF" | cohort_cattle[k] == "RFA" | cohort_cattle[k] == "RFB" | cohort_cattle[k] == "MF"){as.numeric(get(paste("herd", mgmt_sheep[i], sep = "_"))["DWGF"])}else{
                                               as.numeric(get(paste("herd", mgmt_sheep[i], sep = "_"))["DWGM"])}, 
                                             Ckg = as.numeric(inputs_herd[ which(inputs_herd$species == "sheep" & inputs_herd$variable == "Ckg"), ][paste("y_", year, sep = "")]), 
                                             
                                             milk_prod = (as.numeric(inputs_meat[ which(inputs_meat$variable == "Milk Production Other" & inputs_meat$units == "tonnes per animal"), ][paste("y_", year, sep = "")][[1]])*1000)/365, 
                                             ev_milk = as.numeric(inputs_herd[ which(inputs_herd$species == "sheep" & inputs_herd$variable == "Net energy milk"), ][paste("y_", year, sep = "")]), 
                                             ev_fibre = as.numeric(inputs_herd[ which(inputs_herd$species == "sheep" & inputs_herd$variable == "Net energy fibre"), ][paste("y_", year, sep = "")]), 
                                             prod_fibre =0,
                                             LITSIZE = as.numeric(inputs_herd[ which(inputs_herd$species == "sheep" & inputs_herd$variable == "LITSIZE"), ][paste("y_", year, sep = "")]), 
                                             FR = as.numeric(inputs_herd[ which(inputs_herd$species == "sheep" & inputs_herd$variable == "FR"), ][paste("y_", year, sep = "")]), 
                                             LINT = as.numeric(inputs_herd[ which(inputs_herd$species == "sheep" & inputs_herd$variable == "LINT"), ][paste("y_", year, sep = "")]),  
                                             
                                             diet_di = if(cohort_sheep_2[k] == "AF" | cohort_sheep_2[k] == "AM" | cohort_sheep_2[k] == "MF" | cohort_sheep_2[k] == "MM"){
                                               as.numeric(get(paste("feed_di", cohort_sheep_2[k], "dairy", mgmt_sheep[i], sep = "_"))$di) }else if(cohort_sheep_2[k] == "RFA" | cohort_sheep_2[k] == "RFB"){
                                                 as.numeric(get(paste("feed_di_RF_dairy", mgmt_sheep[i], sep = "_"))$di)}else if(cohort_sheep_2[k] == "RMA" | cohort_sheep_2[k] == "RMB"){
                                                   as.numeric(get(paste("feed_di_RM_dairy", mgmt_sheep[i], sep = "_"))$di)},
                                             diet_ge = if(cohort_sheep_2[k] == "AF" | cohort_sheep_2[k] == "AM" | cohort_sheep_2[k] == "MF" | cohort_sheep_2[k] == "MM"){
                                               as.numeric(get(paste("feed_di", cohort_sheep_2[k], "dairy", mgmt_sheep[i], sep = "_"))$ge) }else if(cohort_sheep_2[k] == "RFA" | cohort_sheep_2[k] == "RFB"){
                                                 as.numeric(get(paste("feed_di_RF_dairy", mgmt_sheep[i], sep = "_"))$ge)}else if(cohort_sheep_2[k] == "RMA" | cohort_sheep_2[k] == "RMB"){
                                                   as.numeric(get(paste("feed_di_RM_dairy", mgmt_sheep[i], sep = "_"))$ge)}))
      }
    }
  
  
  
  
}else if(species_lists$species[count] == "pig"){
  
  
  
  for(i in seq(from=1, to=length(mgmt_pig), by=1)) {
    for(k in seq(from=1, to=length(cohort_pig), by=1)) {
      
  
      
      assign(paste("energy_intake", cohort_pig[k], mgmt_pig[i], sep = "_"),    
  energy_intake_pigs(cohort = cohort_pig[k], 
                     live_weight = if(cohort_pig[k] == "AF" | cohort_pig[k] == "AM"){
                       as.numeric(inputs_herd[ which(inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i] & inputs_herd$variable == paste(cohort_pig[k], "kg", sep = "")), ][paste("y_", year, sep = "")])
                     }else if(cohort_pig[k] == "RF"){as.numeric(get(paste("herd", mgmt_pig[i], sep = "_"))["RFkg"])
                     }else if(cohort_pig[k] == "RM"){as.numeric(get(paste("herd", mgmt_pig[i], sep = "_"))["RMkg"])
                     }else if(cohort_pig[k] == "M2"){as.numeric(get(paste("herd", mgmt_pig[i], sep = "_"))["M2kg"])
                     },  
                     Cmain = as.numeric(inputs_herd[ which(inputs_herd$species == "pig" & inputs_herd$variable == "Cmain"), ][paste("y_", year, sep = "")]), 
                     Cact = as.numeric(inputs_herd[ which(inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i] & inputs_herd$variable == "Cact"), ][paste("y_", year, sep = "")]), 
                     Ckg = as.numeric(inputs_herd[ which(inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i] & inputs_herd$variable == "Ckg"), ][paste("y_", year, sep = "")]), 
                     Wkg = as.numeric(inputs_herd[ which(inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i] & inputs_herd$variable == "Wkg"), ][paste("y_", year, sep = "")]), 
                     AFkg = as.numeric(inputs_herd[ which(inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i] & inputs_herd$variable == "AFkg"), ][paste("y_", year, sep = "")]),
                     Cgest = as.numeric(inputs_herd[ which(inputs_herd$species == "pig" & inputs_herd$variable == "Cgest"), ][paste("y_", year, sep = "")]), 
                     
                     LITSIZE = as.numeric(inputs_herd[ which(inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i] & inputs_herd$variable == "LITSIZE"), ][paste("y_", year, sep = "")]),
                     AFCF = as.numeric(get(paste("herd", mgmt_pig[i], sep = "_"))["AFCF"]), 
                     DR1 = as.numeric(inputs_herd[ which(inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i] & inputs_herd$variable == "DR1"), ][paste("y_", year, sep = "")]), 
                     
                     Clact = as.numeric(inputs_herd[ which(inputs_herd$species == "pig" & inputs_herd$variable == "Clact"), ][paste("y_", year, sep = "")]),
                     Cwloss = as.numeric(inputs_herd[ which(inputs_herd$species == "pig" & inputs_herd$variable == "Cwloss"), ][paste("y_", year, sep = "")]), 
                     Cconv = as.numeric(inputs_herd[ which(inputs_herd$species == "pig" & inputs_herd$variable == "Cconv"), ][paste("y_", year, sep = "")]),  
                     
                     diet_me = as.numeric(get(paste("feed_di", mgmt_pig[i], sep = "_"))$me), 
                     DWG = if(cohort_pig[k] == "AF" | cohort_pig[k] == "AM"){0
                     }else if(cohort_pig[k] == "RF"){as.numeric(get(paste("herd", mgmt_pig[i], sep = "_"))["DWGF"])
                     }else if(cohort_pig[k] == "RM"){as.numeric(get(paste("herd", mgmt_pig[i], sep = "_"))["DWGM"])
                     }else if(cohort_pig[k] == "M2"){as.numeric(inputs_herd[ which(inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i] & inputs_herd$variable == "DWG2"), ][paste("y_", year, sep = "")])}, 
                     
                     PTissue = as.numeric(inputs_herd[ which(inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i] & inputs_herd$variable == "PTissue"), ][paste("y_", year, sep = "")]), 
                     Prot = as.numeric(inputs_herd[ which(inputs_herd$species == "pig" & inputs_herd$variable == "Prot"), ][paste("y_", year, sep = "")]), 
                     CMEprot = as.numeric(inputs_herd[ which(inputs_herd$species == "pig" & inputs_herd$variable == "Cmeprot"), ][paste("y_", year, sep = "")]),
                     Fat = as.numeric(inputs_herd[ which(inputs_herd$species == "pig" & inputs_herd$variable == "FAT"), ][paste("y_", year, sep = "")]), 
                     CMEfat = as.numeric(inputs_herd[ which(inputs_herd$species == "pig" & inputs_herd$variable == "Cmefat"), ][paste("y_", year, sep = "")]), 
                     idle = as.numeric(inputs_herd[ which(inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i] & inputs_herd$variable == "Idle"), ][paste("y_", year, sep = "")]), 
                     lact = as.numeric(inputs_herd[ which(inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i] & inputs_herd$variable == "Lact"), ][paste("y_", year, sep = "")]), 
                     gest = as.numeric(inputs_herd[ which(inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i] & inputs_herd$variable == "Gest"), ][paste("y_", year, sep = "")])))
  
  
  
    }
  }
  
  
  
  
  
  
}else if(species_lists$species[count] == "chicken"){
  
  
  for(i in seq(from=1, to=length(mgmt_chicken), by=1)) {
    for(k in seq(from=1, to=length(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))), by=1)) {
      
      assign(paste("energy_intake", get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k], mgmt_chicken[i], sep = "_"),
  energy_intake_chickens(cohort = get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k], 
                         mgmt = mgmt_chicken[i], 
                         temp = as.numeric(inputs_herd[ which(inputs_herd$species == "chicken" & inputs_herd$mgmt == mgmt_chicken[i] & inputs_herd$variable == "temp"), ][paste("y_", year, sep = "")]), 
                         live_weight = if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "AF" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "AM" | 
                                          get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "RF" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "RM" | 
                                          get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MF1" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MF2" 
                                          | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MM" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "M2"){
                           as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))[paste(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k], "kg", sep = "")])
                         }else if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MF3" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MF4" ){
                           as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))["MF22kg"])
                         } else if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MF1" & mgmt_chicken[i] == "backyard" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MF2" & mgmt_chicken[i] == "backyard"){
                           as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))[paste(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k], "kg", sep = "")])
                         }else if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MF1" & mgmt_chicken[i] == "layers"){
                           as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))["MF11kg"])
                         }else if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MF2" & mgmt_chicken[i] == "layers"){
                           as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))["MF22kg"])
                         }else {0}, 
                         
                         feathering_score = as.numeric(inputs_herd[ which(inputs_herd$species == "chicken" & inputs_herd$variable == "Feathering score"), ][paste("y_", year, sep = "")]), 
                         Cact = as.numeric(inputs_herd[ which(inputs_herd$species == "chicken" & inputs_herd$mgmt == mgmt_chicken[i] & inputs_herd$variable == "Cact"), ][paste("y_", year, sep = "")]), 
                         DWG = if(mgmt_chicken[i] == "backyard"){
                               if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MF1" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "RF"){
                           as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))["DWGF1"])
                               }else if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MF2" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "AF"){
                                 as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))["DWGF2"])
                               }else if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "RM" ){
                                 as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))["DWGM1"])
                               }else if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MM" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "AM"){ ##Check whether correct DWG assigned to each chicken cohort
                                 as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))["DWGM1"])
                               }
                           } else if(mgmt_chicken[i] == "layers"){
                             if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MF1" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "RF"){
                               as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))["DWGF1"])
                             }else if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MF2" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MF3" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MF4" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "AF"){
                               as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))["DWGF2"])
                             }else if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "RM" ){
                               as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))["DWGM1"])
                             }else if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MM" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "AM"){ ##Check whether correct DWG assigned to each chicken cohort
                               as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))["DWGM1"])
                             }
                           } else if(mgmt_chicken[i] == "broilers"){
                             if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "RF"){
                               as.numeric(herd_layers["DWGF1"])
                             }else if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "AF"){
                               as.numeric(herd_layers["DWGF2"])
                             }else if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "RM" ){
                               as.numeric(herd_layers["DWGM1"])
                             }else if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "M2" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "AM"){ ##Check whether correct DWG assigned to each chicken cohort
                               as.numeric(herd_layers["DWGM2"])
                             }
                           }, 
                         
                                       
                         Cgro = as.numeric(inputs_herd[ which(inputs_herd$species == "chicken" & inputs_herd$mgmt == mgmt_chicken[i] & inputs_herd$cohort == get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] & inputs_herd$variable == "Cgro"), ][paste("y_", year, sep = "")]), 
                         
                         diet_me = as.numeric(get(paste("feed_di", mgmt_chicken[i], sep = "_"))$me),
                         
                         EGG = (as.numeric(inputs_herd[ which(inputs_herd$variable == "EGGSyear" & inputs_herd$mgmt == mgmt_chicken[i] & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")]))/365 * 
                                    (as.numeric(inputs_herd[ which(inputs_herd$variable == "EGGSwght" & inputs_herd$mgmt == mgmt_chicken[i] & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])),
                         
                         Cegg = as.numeric(inputs_herd[ which(inputs_herd$species == "chicken" & inputs_herd$variable == "Cegg"), ][paste("y_", year, sep = "")])))
  
  
  
  
    }
  }
  
  
  
  
}




##enteric emissions
if(species_lists$species[count] == "cattle"){


for(i in seq(from=1, to=length(mgmt_cattle), by=1)) {
  for(j in seq(from=1, to=length(animal_cattle), by=1)) {
    for(k in seq(from=1, to=length(cohort_cattle), by=1)){
      
      assign(paste("enteric_methane", cohort_cattle[k], animal_cattle[j], mgmt_cattle[i], sep = "_"), 
             enteric_methane(species = species_lists$species[count],
                             cohort = cohort_cattle[k], 
                             number_cows = as.numeric(get(paste("herd", mgmt_cattle[i], sep = "_"))[paste(cohort_cattle[k], animal_cattle[j], sep = "_")]), 
                             diet_di = as.numeric(get(paste("feed_di", cohort_cattle[k], animal_cattle[j], mgmt_cattle[i], sep = "_"))$di), 
                             diet_ge = as.numeric(get(paste("feed_di", cohort_cattle[k], animal_cattle[j], mgmt_cattle[i], sep = "_"))$ge), 
                             dmi = as.numeric(get(paste("energy_intake", cohort_cattle[k], animal_cattle[j], mgmt_cattle[i], sep = "_")))[1]))
    }
  }
}

}else if(species_lists$species[count] == "sheep"){
  
  for(i in seq(from=1, to=length(mgmt_sheep), by=1)) {
    for(j in seq(from=1, to=length(animal_sheep), by=1)) {
      for(k in seq(from=1, to=length(cohort_sheep_2), by=1)){
        
        if(animal_sheep[j] == "non-dairy"){value = "_nd"}else{value = ""}
        
        assign(paste("enteric_methane", cohort_sheep_2[k], animal_sheep[j], mgmt_sheep[i], sep = "_"), 
               enteric_methane(species = species_lists$species[count],
                               cohort = cohort_sheep_2[k], 
                               number_cows = as.numeric(get(paste("herd", mgmt_sheep[i], sep = "_"))[paste(cohort_sheep_2[k], value, sep = "")]), 
                               diet_di = as.numeric(get(paste("feed_di", cohort_sheep_3[k], animal_sheep[j], mgmt_sheep[i], sep = "_"))$di), 
                               diet_ge = as.numeric(get(paste("feed_di", cohort_sheep_3[k], animal_sheep[j], mgmt_sheep[i], sep = "_"))$ge), 
                               dmi = as.numeric(get(paste("energy_intake", cohort_sheep_2[k], animal_sheep[j], mgmt_sheep[i], sep = "_")))[1]))
      }
    }
  }
  
  
  
  
}else if(species_lists$species[count] == "pig"){
  
  for(i in seq(from=1, to=length(mgmt_pig), by=1)) {
    for(j in seq(from=1, to=length(animal_pig), by=1)) {
      for(k in seq(from=1, to=length(cohort_pig), by=1)){
        
        assign(paste("enteric_methane", cohort_pig[k], animal_pig[j], mgmt_pig[i], sep = "_"), 
               enteric_methane(species = species_lists$species[count],
                               cohort = cohort_pig[k], 
                               number_cows = as.numeric(get(paste("herd", mgmt_pig[i], sep = "_"))[paste(cohort_pig[k], sep = "")]), 
                               diet_di = as.numeric(get(paste("feed_di", mgmt_pig[i], sep = "_"))$di), 
                               diet_ge = as.numeric(get(paste("feed_di", mgmt_pig[i], sep = "_"))$ge), 
                               dmi = as.numeric(get(paste("energy_intake", cohort_pig[k], mgmt_pig[i], sep = "_")))[1]))
      }
    }
  } 


}




##Feed requirements
if(species_lists$species[count] == "cattle"){

##feed requirements
##feed categories included in feed_categories variable
for(i in seq(from=1, to=length(mgmt_cattle), by=1)) {
  for(j in seq(from=1, to=length(animal_cattle), by=1)) {
    for(k in seq(from=1, to=length(cohort_cattle), by=1)){
      
      assign(paste("feed_requirements", cohort_cattle[k], animal_cattle[j], mgmt_cattle[i], sep = "_"), 
             gross_dm(cohort = cohort_cattle[k], 
                      pct = as.numeric(inputs_feed_cattle[inputs_feed_cattle$mgmt == mgmt_cattle[i] & inputs_feed_cattle$animal == animal_cattle[j] & inputs_feed_cattle$cohort == cohort_cattle[k], ][paste("y_", year, sep = "")][[1]]),
                      fue = feed_ruminants["fue"][[1]],
                      mfa = feed_ruminants["mfa"][[1]],
                      number_cows = as.numeric(get(paste("herd", mgmt_cattle[i], sep = "_"))[paste(cohort_cattle[k], animal_cattle[j], sep = "_")]), 
                      dmi = as.numeric(get(paste("energy_intake", cohort_cattle[k], animal_cattle[j], mgmt_cattle[i], sep = "_")))[1]))
    }
  }
}


}else if(species_lists$species[count] == "sheep"){
  
  for(i in seq(from=1, to=length(mgmt_sheep), by=1)) {
    for(j in seq(from=1, to=length(animal_sheep), by=1)) {
      for(k in seq(from=1, to=length(cohort_sheep_2), by=1)){
        
        
        if(animal_sheep[j] == "non-dairy"){value = "_nd"}else{value = ""}
        
        assign(paste("feed_requirements", cohort_sheep_2[k], animal_sheep[j], mgmt_sheep[i], sep = "_"), 
               gross_dm(cohort = cohort_sheep[k], 
                        pct = as.numeric(inputs_feed_sheep[inputs_feed_sheep$mgmt == mgmt_sheep[i] & inputs_feed_sheep$animal == animal_sheep[j] & inputs_feed_sheep$cohort == cohort_sheep_3[k], ][paste("y_", year, sep = "")][[1]]),
                        fue = feed_ruminants["fue"][[1]],
                        mfa = feed_ruminants["mfa"][[1]],
                        number_cows = as.numeric(get(paste("herd", mgmt_sheep[i], sep = "_"))[paste(cohort_sheep_2[k], value, sep = "")]),
                        dmi = as.numeric(get(paste("energy_intake", cohort_sheep_2[k], animal_sheep[j], mgmt_sheep[i], sep = "_")))[1]))
      }
    }
  }
  
  
}else if(species_lists$species[count] == "pig"){
  
  for(i in seq(from=1, to=length(mgmt_pig), by=1)) {
    for(j in seq(from=1, to=length(animal_pig), by=1)) {
      for(k in seq(from=1, to=length(cohort_pig), by=1)){
        
        assign(paste("feed_requirements", cohort_pig[k], animal_pig[j], mgmt_pig[i], sep = "_"), 
               gross_dm(cohort = cohort_pig[k], 
                        pct = as.numeric(inputs_feed_pig[inputs_feed_pig$mgmt == mgmt_pig[i], ][paste("y_", year, sep = "")][[1]]),
                        fue = feed_monogastrics["fue"][[1]],
                        mfa = feed_monogastrics["mfa"][[1]],
                        number_cows = as.numeric(get(paste("herd", mgmt_pig[i], sep = "_"))[paste(cohort_pig[k], sep = "_")]), 
                        dmi = as.numeric(get(paste("energy_intake", cohort_pig[k], mgmt_pig[i], sep = "_")))[1]))
      }
    }
  }
  
  
}else if(species_lists$species[count] == "chicken"){

  for(i in seq(from=1, to=length(mgmt_chicken), by=1)) {
    for(j in seq(from=1, to=length(animal_chicken), by=1)) {
      for(k in seq(from=1, to=length(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))), by=1)){
        
        assign(paste("feed_requirements", get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k], animal_chicken[j], mgmt_chicken[i], sep = "_"), 
               gross_dm(cohort = cohort_chicken[k], 
                        pct = as.numeric(inputs_feed_chicken[inputs_feed_chicken$mgmt == mgmt_chicken[i], ][paste("y_", year, sep = "")][[1]]),
                        fue = feed_monogastrics["fue"][[1]],
                        mfa = feed_monogastrics["mfa"][[1]],
                        number_cows = as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))[paste(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k], sep = "_")]), 
                        dmi = as.numeric(get(paste("energy_intake", get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k], mgmt_chicken[i], sep = "_")))[1]))
      }
    }
  }
}





##Manure emissions

  
if(species_lists$species[count] == "cattle"){
  
  
for(i in seq(from=1, to=length(mgmt_cattle), by=1)) {
  for(j in seq(from=1, to=length(animal_cattle), by=1)) {
    for(k in seq(from=1, to=length(cohort_cattle), by=1)){
      
      assign(paste("manure_methane", cohort_cattle[k], animal_cattle[j], mgmt_cattle[i], sep = "_"), 
             manure_methane(cohort = cohort_cattle[k], 
                      Bo = as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Bo" & inputs_manure_cattle$animal == animal_cattle[j], ][paste("y_", year, sep = "")][[1]]),
                      pct = if(cohort_cattle[k] == "MFf" | cohort_cattle[k] == "MMf"){
                                as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == "feedlot", ][paste("y_", year, sep = "")][[1]])
                              }else{
                                as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == animal_cattle[j], ][paste("y_", year, sep = "")][[1]])
                              },
                      mcf = if(manure$temp[1] < 14){manure["mcf_less14"][[1]]}else if(manure$temp[1] >= 14 & manure$temp[1] <= 26){manure["mcf_14_26"][[1]]}else{manure["mcf_above26"][[1]]},
                      diet_di = as.numeric(get(paste("feed_di", cohort_cattle[k], animal_cattle[j], mgmt_cattle[i], sep = "_"))$di),
                      number_cows = as.numeric(get(paste("herd", mgmt_cattle[i], sep = "_"))[paste(cohort_cattle[k], animal_cattle[j], sep = "_")]), 
                      dmi = as.numeric(get(paste("energy_intake", cohort_cattle[k], animal_cattle[j], mgmt_cattle[i], sep = "_")))[1]))
    }
  }
}


  
}
  
  
if(species_lists$species[count] == "sheep"){
  
  
  for(i in seq(from=1, to=length(mgmt_sheep), by=1)) {
    for(j in seq(from=1, to=length(animal_sheep), by=1)) {
      for(k in seq(from=1, to=length(cohort_sheep_2), by=1)){
        
        if(animal_sheep[j] == "non-dairy"){value = "_nd"}else{value = ""}
        
        assign(paste("manure_methane", cohort_sheep_2[k], animal_sheep[j], mgmt_sheep[i], sep = "_"), 
               manure_methane(cohort = cohort_sheep_2[k], 
                              Bo = as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "Bo", ][paste("y_", year, sep = "")][[1]]),
                              pct = as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "Manure mgmt system" & inputs_manure_sheep$mgmt == mgmt_sheep[i], ][paste("y_", year, sep = "")][[1]]),
                              mcf = if(manure$temp[1] < 14){manure["mcf_less14"][[1]]}else if(manure$temp[1] >= 14 & manure$temp[1] <= 26){manure["mcf_14_26"][[1]]}else{manure["mcf_above26"][[1]]},
                              diet_di = as.numeric(get(paste("feed_di", cohort_sheep_3[k], animal_sheep[j], mgmt_sheep[i], sep = "_"))$di),
                              number_cows = as.numeric(get(paste("herd", mgmt_sheep[i], sep = "_"))[paste(cohort_sheep_2[k], value, sep = "")]), 
                              dmi = as.numeric(get(paste("energy_intake", cohort_sheep_2[k], animal_sheep[j], mgmt_sheep[i], sep = "_")))[1]))
      }
    }
  }
  
  
  
}


if(species_lists$species[count] == "pig"){
  
  
  for(i in seq(from=1, to=length(mgmt_pig), by=1)) {
    for(j in seq(from=1, to=length(animal_pig), by=1)) {
      for(k in seq(from=1, to=length(cohort_pig), by=1)){
        
        assign(paste("manure_methane", cohort_pig[k], animal_pig[j], mgmt_pig[i], sep = "_"), 
               manure_methane(cohort = cohort_pig[k], 
                              Bo = as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "Bo", ][paste("y_", year, sep = "")][[1]]),
                              pct = as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "Manure mgmt system" & inputs_manure_pig$mgmt == mgmt_pig[i], ][paste("y_", year, sep = "")][[1]]),
                              mcf = if(manure$temp[1] < 14){manure["mcf_less14"][[1]]}else if(manure$temp[1] >= 14 & manure$temp[1] <= 26){manure["mcf_14_26"][[1]]}else{manure["mcf_above26"][[1]]},
                              diet_di = as.numeric(get(paste("feed_di", mgmt_pig[i], sep = "_"))$di),
                              number_cows = as.numeric(get(paste("herd", mgmt_pig[i], sep = "_"))[paste(cohort_pig[k], sep = "_")]), 
                              dmi = as.numeric(get(paste("energy_intake", cohort_pig[k], mgmt_pig[i], sep = "_")))[1]))
      }
    }
  }
  
  
  
}



if(species_lists$species[count] == "chicken"){
  
  
  for(i in seq(from=1, to=length(mgmt_chicken), by=1)) {
    for(j in seq(from=1, to=length(animal_chicken), by=1)) {
      for(k in seq(from=1, to=length(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))), by=1)){
        
        assign(paste("manure_methane",get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k], animal_chicken[j], mgmt_chicken[i], sep = "_"), 
               manure_methane(cohort = get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k], 
                              Bo = as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "Bo" & inputs_manure_chicken$mgmt == mgmt_chicken[j], ][paste("y_", year, sep = "")][[1]]),
                              pct = as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "Manure mgmt system" & inputs_manure_chicken$mgmt == mgmt_chicken[i], ][paste("y_", year, sep = "")][[1]]),
                              mcf = if(manure$temp[1] < 14){manure["mcf_less14"][[1]]}else if(manure$temp[1] >= 14 & manure$temp[1] <= 26){manure["mcf_14_26"][[1]]}else{manure["mcf_above26"][[1]]},
                              diet_di = 0,
                              diet_me = as.numeric(get(paste("feed_di", mgmt_chicken[i], sep = "_"))$me),
                              diet_ge = as.numeric(get(paste("feed_di", mgmt_chicken[i], sep = "_"))$ge),
                              number_cows = as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))[paste(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k], sep = "_")]), 
                              dmi = as.numeric(get(paste("energy_intake", get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k], mgmt_chicken[i], sep = "_")))[1]))
      }
    }
  }
  
  
  
}








##Manure module - N excretion (kg N y-1 (all animals in mgmt and cohort))

if(species_lists$species[count] == "cattle"){


for(i in seq(from=1, to=length(mgmt_cattle), by=1)) {
  for(j in seq(from=1, to=length(animal_cattle), by=1)) {
    for(k in seq(from=1, to=length(cohort_cattle), by=1)){
      
      assign(paste("nitrogen_excretion", cohort_cattle[k], animal_cattle[j], mgmt_cattle[i], sep = "_"), 
             nitrogen_excretion(species = "cattle",
                                cohort = cohort_cattle[k], 
                                number_cows = as.numeric(get(paste("herd", mgmt_cattle[i], sep = "_"))[paste(cohort_cattle[k], animal_cattle[j], sep = "_")]), 
                                dmi = as.numeric(get(paste("energy_intake", cohort_cattle[k], animal_cattle[j], mgmt_cattle[i], sep = "_")))[1], 
                                diet_ncont = as.numeric(get(paste("feed_di", cohort_cattle[k], animal_cattle[j], mgmt_cattle[i], sep = "_"))$ncont), 
                                milk_prod = as.numeric(inputs_meat[inputs_meat$species == "cattle" & inputs_meat$variable == "Milk Production" & inputs_meat$units == "tonnes per animal", ][paste("y_", year, sep = "")][[1]])*1000/ 365,
                                milk_protein = as.numeric(inputs_herd[inputs_herd$species == "cattle" & inputs_herd$variable == "Milk protein content", ][paste("y_", year, sep = "")][[1]]), ##from GLEAM
                                Ckg = as.numeric(inputs_herd[inputs_herd$species == "cattle" & inputs_herd$mgmt == mgmt_cattle[i] & inputs_herd$animal == animal_cattle[j] & inputs_herd$variable == "Ckg", ][paste("y_", year, sep = "")][[1]]), 
                                DWG = as.numeric(get(paste("herd", mgmt_cattle[i], sep = "_"))[paste("DWG", if(cohort_cattle[k] == "AF" |  cohort_cattle[k] == "RF" |  cohort_cattle[k] == "MF" |  cohort_cattle[k] == "MFf"){"F"}else{"M"}, "_",  animal_cattle[j], sep = "")]), 
                                net_energy_required = if(cohort_cattle[k] == "AF"){as.numeric(get(paste("energy_intake_RF", animal_cattle[j], mgmt_cattle[i], sep = "_")))[2]}else{as.numeric(get(paste("energy_intake", cohort_cattle[k], animal_cattle[j], mgmt_cattle[i], sep = "_")))[2]}))
     }
  }
}


}else if(species_lists$species[count] == "sheep"){
  
  
  for(i in seq(from=1, to=length(mgmt_sheep), by=1)) {
    for(j in seq(from=1, to=length(animal_sheep), by=1)) {
      for(k in seq(from=1, to=length(cohort_sheep_2), by=1)){
        
        if(animal_sheep[j] == "non-dairy"){value = "_nd"}else{value = ""}
        
        assign(paste("nitrogen_excretion", cohort_sheep_2[k], animal_sheep[j], mgmt_sheep[i], sep = "_"), 
               nitrogen_excretion(species = "sheep",
                                  cohort = cohort_sheep_2[k], 
                                  number_cows = as.numeric(get(paste("herd", mgmt_sheep[i], sep = "_"))[paste(cohort_sheep_2[k], value, sep = "")]), 
                                  dmi = as.numeric(get(paste("energy_intake", cohort_sheep_2[k], animal_sheep[j], mgmt_sheep[i], sep = "_")))[1], 
                                  diet_ncont = as.numeric(get(paste("feed_di", cohort_sheep_3[k], animal_sheep[j], mgmt_sheep[i], sep = "_"))$ncont), 
                                  milk_prod = as.numeric(inputs_meat[inputs_meat$species == "sheep" & inputs_meat$variable == "Milk Production Other" & inputs_meat$units == "tonnes per animal", ][paste("y_", year, sep = "")][[1]])*1000/ 365,
                                  milk_protein = as.numeric(inputs_herd[inputs_herd$species == "cattle" & inputs_herd$variable == "Milk protein content", ][paste("y_", year, sep = "")][[1]]), ##from GLEAM
                                  Ckg = as.numeric(inputs_herd[inputs_herd$species == "sheep" & inputs_herd$variable == "Ckg", ][paste("y_", year, sep = "")][[1]]), 
                                  DWG = as.numeric(get(paste("herd", mgmt_sheep[i], sep = "_"))[paste("DWG", if(cohort_sheep_2[k] == "AF" |  cohort_sheep_2[k] == "RFA"  |  cohort_sheep_2[k] == "RFB" |  cohort_sheep_2[k] == "MF"){"F"}else{"M"}, sep = "")]), 
                                  net_energy_required = if(cohort_sheep_2[k] == "AF"){as.numeric(get(paste("energy_intake_RFA", animal_sheep[j], mgmt_sheep[i], sep = "_")))[2]}else{as.numeric(get(paste("energy_intake", cohort_sheep_2[k], animal_sheep[j], mgmt_sheep[i], sep = "_")))[2]}))
      }
    }
  }
  
}else if(species_lists$species[count] == "pig"){

  
  for(i in seq(from=1, to=length(mgmt_pig), by=1)) {
    for(j in seq(from=1, to=length(animal_pig), by=1)) {
      for(k in seq(from=1, to=length(cohort_pig), by=1)){
        
        assign(paste("nitrogen_excretion", cohort_pig[k], animal_pig[j], mgmt_pig[i], sep = "_"), 
               nitrogen_excretion(species = "pig",
                                  cohort = cohort_pig[k], 
                                  number_cows = as.numeric(get(paste("herd", mgmt_pig[i], sep = "_"))[paste(cohort_pig[k], sep = "_")]), 
                                  dmi = as.numeric(get(paste("energy_intake", cohort_pig[k], mgmt_pig[i], sep = "_")))[1], 
                                  diet_ncont = as.numeric(get(paste("feed_di", mgmt_pig[i], sep = "_"))$ncont), 
                                  LITSIZE = as.numeric(inputs_herd[inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i] & inputs_herd$variable == "LITSIZE", ][paste("y_", year, sep = "")][[1]]), 
                                  FR = as.numeric(inputs_herd[inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i] & inputs_herd$variable == "FR", ][paste("y_", year, sep = "")][[1]]), 
                                  Wkg = as.numeric(inputs_herd[inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i] & inputs_herd$variable == "Wkg", ][paste("y_", year, sep = "")][[1]]), 
                                  AFCF = as.numeric(inputs_herd[inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i] & inputs_herd$variable == "AFCF", ][paste("y_", year, sep = "")][[1]]), 
                                  Ckg = as.numeric(inputs_herd[inputs_herd$species == "pig" & inputs_herd$mgmt == mgmt_pig[i]  & inputs_herd$variable == "Ckg", ][paste("y_", year, sep = "")][[1]]), 
                                  DWG = as.numeric(get(paste("herd", mgmt_pig[i], sep = "_"))[paste("DWG", if(cohort_pig[k] == "AF" |  cohort_pig[k] == "RF" |  cohort_pig[k] == "MF" |  cohort_pig[k] == "MFf"){"F"}else{"M"}, sep = "")])))
      }
    }
  }
  
}else if(species_lists$species[count] == "chicken"){
  
  
  for(i in seq(from=1, to=length(mgmt_chicken), by=1)) {
    for(j in seq(from=1, to=length(animal_chicken), by=1)) {
      for(k in seq(from=1, to=length(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))), by=1)){
        
        assign(paste("nitrogen_excretion", get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k], animal_chicken[j], mgmt_chicken[i], sep = "_"), 
               nitrogen_excretion(species = "chicken",
                                  cohort = get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k], 
                                  number_cows = as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))[paste(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k], sep = "_")]), 
                                  dmi = as.numeric(get(paste("energy_intake", get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k], mgmt_chicken[i], sep = "_")))[1], 
                                  diet_ncont = as.numeric(get(paste("feed_di", mgmt_chicken[i], sep = "_"))$ncont), 
                                  NLW = as.numeric(inputs_herd[inputs_herd$species == "chicken" & inputs_herd$variable == "NLW", ][paste("y_", year, sep = "")][[1]]), 
                                  NEGG = as.numeric(inputs_herd[inputs_herd$species == "chicken" & inputs_herd$variable == "NEGG", ][paste("y_", year, sep = "")][[1]]),
                                  EGG = (as.numeric(inputs_herd[ which(inputs_herd$variable == "EGGSyear" & inputs_herd$mgmt == mgmt_chicken[i] & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")]))/365 * 
                                    (as.numeric(inputs_herd[ which(inputs_herd$variable == "EGGSwght" & inputs_herd$mgmt == mgmt_chicken[i] & inputs_herd$species == "chicken"), ][paste("y_", year, sep = "")])),
                                  DWG = if(mgmt_chicken[i] == "backyard"){
                                    if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MF1" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "RF"){
                                      as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))["DWGF1"])
                                    }else if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MF2" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "AF"){
                                      as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))["DWGF2"])
                                    }else if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "RM" ){
                                      as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))["DWGM1"])
                                    }else if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MM" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "AM"){ ##Check whether correct DWG assigned to each chicken cohort
                                      as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))["DWGM1"])
                                    }
                                  } else if(mgmt_chicken[i] == "layers"){
                                    if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MF1" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "RF"){
                                      as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))["DWGF1"])
                                    }else if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MF2" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MF3" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MF4" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "AF"){
                                      as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))["DWGF2"])
                                    }else if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "RM" ){
                                      as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))["DWGM1"])
                                    }else if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "MM" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "AM"){ ##Check whether correct DWG assigned to each chicken cohort
                                      as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))["DWGM1"])
                                    }
                                  } else if(mgmt_chicken[i] == "broilers"){
                                    if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "RF"){
                                      as.numeric(herd_layers["DWGF1"])
                                    }else if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "AF"){
                                      as.numeric(herd_layers["DWGF2"])
                                    }else if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "RM" ){
                                      as.numeric(herd_layers["DWGM1"])
                                    }else if(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "M2" | get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k] == "AM"){ ##Check whether correct DWG assigned to each chicken cohort
                                      as.numeric(herd_layers["DWGM2"])
                                    }
                                  }))
      }
    }
  }

}







##NH3, NO and N2O (kg N emitted per year (need to be converted to kg pollutant)) emissions from manure after excretion

if(species_lists$species[count] == "cattle"){

for(i in seq(from=1, to=length(mgmt_cattle), by=1)) {
  for(j in seq(from=1, to=length(animal_cattle), by=1)) {
    for(k in seq(from=1, to=length(cohort_cattle), by=1)){

      
     p_graze <- if(cohort_cattle[k] == "MFf" | cohort_cattle[k] == "MMf"){
        as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == "feedlot" & inputs_manure_cattle$manure == "pasture", ][paste("y_", year, sep = "")][[1]])/100}else{
          as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == animal_cattle[j] & inputs_manure_cattle$manure == "pasture", ][paste("y_", year, sep = "")][[1]])/100}
      
     time_on_yard <- as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "prop_yard" & inputs_manure_cattle$animal == animal_cattle[i], ][paste("y_", year, sep = "")][[1]])
     
     p_house <- (1- p_graze) * (1 - time_on_yard)
     
      p_yard <- 1 - p_graze - p_house
      
      
      assign(paste("manure_emissions_available", cohort_cattle[k], animal_cattle[j], mgmt_cattle[i], sep = "_"),
        manure_excretion_storage_emis(cohort = cohort_cattle[k], 
                                number_cows = as.numeric(get(paste("herd", mgmt_cattle[i], sep = "_"))[paste(cohort_cattle[k], animal_cattle[j], sep = "_")]), 
                                n_excretion_total = as.numeric(get(paste("nitrogen_excretion", cohort_cattle[k], animal_cattle[j], mgmt_cattle[i], sep = "_")))[1], 
                                      
                                prop_graze = p_graze,
                                      
                                prop_yard = p_yard, ##Tier 2 EMEP/EEA dairy cattle 0.25, other cattle 0.1
                                      
                                prop_house = p_house, ##Tier 2 EMEP/EEA dairy/other cattle 0.5, 
                                      
                                prop_TAN = as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "prop_TAN" & inputs_manure_cattle$animal == animal_cattle[i], ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP EEA default Table 3.9 
                                      
                                prop_slurry = if(cohort_cattle[k] == "MFf" | cohort_cattle[k] == "MMf"){
                                        sum(as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == "feedlot" & inputs_manure_cattle$manure == "liquid_slurry" |
                                                                          inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == "feedlot" & inputs_manure_cattle$manure == "liquid_slurry_crust" |
                                                                          inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == "feedlot" & inputs_manure_cattle$manure == "anaerobic_lagoon", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE)}else{
                                        sum(as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == animal_cattle[j] & inputs_manure_cattle$manure == "liquid_slurry" |
                                                                              inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == animal_cattle[j] & inputs_manure_cattle$manure == "liquid_slurry_crust" |
                                                                              inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == animal_cattle[j] & inputs_manure_cattle$manure == "anaerobic_lagoon", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE)},
                                  
                                ef_house_slurry = as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "ef_house_slurry" & inputs_manure_cattle$species == "cattle", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                ef_house_solid  = as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "ef_house_solid" & inputs_manure_cattle$species == "cattle", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                ef_yard = as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "ef_yard" & inputs_manure_cattle$species == "cattle" & inputs_manure_cattle$animal == animal_cattle[j], ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                      
                                straw = as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "straw" & inputs_manure_cattle$species == "cattle" & inputs_manure_cattle$animal == animal_cattle[j], ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.7 adjusted to full year equivalent 
                                N_straw = as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "N_straw" & inputs_manure_cattle$species == "cattle" & inputs_manure_cattle$animal == animal_cattle[j], ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.7 adjusted to full year equivalent
                                frac_imm = as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "frac_imm" & inputs_manure_cattle$species == "cattle", ][paste("y_", year, sep = "")][[1]]),
                                      
                                prop_store_slurry = 1-if(cohort_cattle[k] == "MFf" | cohort_cattle[k] == "MMf"){
                                  sum(as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == "feedlot" & inputs_manure_cattle$manure == "anaerobic_digester", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE) }else{
                                    sum(as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == animal_cattle[j] & inputs_manure_cattle$manure == "anaerobic_digester", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE)},
                                         
                            
                              prop_biogas = if(cohort_cattle[k] == "MFf" | cohort_cattle[k] == "MMf"){
                                                  sum(as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == "feedlot" & inputs_manure_cattle$manure == "anaerobic_digester", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE) }else{
                                                  sum(as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == animal_cattle[j] & inputs_manure_cattle$manure == "anaerobic_digester", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE)},
                                                    
                                
                              prop_store_solid = 1-if(cohort_cattle[k] == "MFf" | cohort_cattle[k] == "MMf"){
                                sum(as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == "feedlot" & inputs_manure_cattle$manure == "anaerobic_digester", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE) }else{
                                  sum(as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == animal_cattle[j] & inputs_manure_cattle$manure == "anaerobic_digester", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE)}-if(cohort_cattle[k] == "MFf" | cohort_cattle[k] == "MMf"){
                                    sum(as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == "feedlot" & inputs_manure_cattle$manure == "burned_fuel", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE) }else{
                                      sum(as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == animal_cattle[j] & inputs_manure_cattle$manure == "burned_fuel", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE)},
                              prop_burnfuel = if(cohort_cattle[k] == "MFf" | cohort_cattle[k] == "MMf"){
                                                  sum(as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == "feedlot" & inputs_manure_cattle$manure == "burned_fuel", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE) }else{
                                                  sum(as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == animal_cattle[j] & inputs_manure_cattle$manure == "burned_fuel", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE)},
                              
                                
                              frac_min = as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "frac_min" & inputs_manure_cattle$species == "cattle", ][paste("y_", year, sep = "")][[1]]), 
                                      
                                ef_storage_slurry_nh3 =  as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "ef_storage_slurry_nh3" & inputs_manure_cattle$species == "cattle", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                ef_storage_slurry_no = as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "ef_storage_slurry_no" & inputs_manure_cattle$species == "cattle", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.10 
                                ef_storage_slurry_n2 = as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "ef_storage_slurry_n2" & inputs_manure_cattle$species == "cattle", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                ef_storage_solid_nh3 = as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "ef_storage_solid_nh3" & inputs_manure_cattle$species == "cattle", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                ef_storage_solid_no = as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "ef_storage_solid_no" & inputs_manure_cattle$species == "cattle", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9 
                                ef_storage_solid_n2 = as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "ef_storage_solid_n2" & inputs_manure_cattle$species == "cattle", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9 
                                
                              pct_mms_solid = if(cohort_cattle[k] == "MFf" | cohort_cattle[k] == "MMf"){
                                as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == "feedlot" & inputs_manure_cattle$manure == "solid_storage" |
                                                                      inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == "feedlot" & inputs_manure_cattle$manure == "dry_lot" |
                                                                      inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == "feedlot" & inputs_manure_cattle$manure == "pitstorage_less1month" |
                                                                      inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == "feedlot" & inputs_manure_cattle$manure == "pitstorage_greater1month" |
                                                                      inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == "feedlot" & inputs_manure_cattle$manure == "pitstorage_greater1month_chickens" |
                                                                      inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == "feedlot" & inputs_manure_cattle$manure == "composting" |
                                                                      inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == "feedlot" & inputs_manure_cattle$manure == "poultry_with_litter", ][paste("y_", year, sep = "")][[1]]) }else{
                                                                        as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == animal_cattle[j] & inputs_manure_cattle$manure == "solid_storage" |
                                                                                                              inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == animal_cattle[j] & inputs_manure_cattle$manure == "dry_lot" |
                                                                                                              inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == animal_cattle[j] & inputs_manure_cattle$manure == "pitstorage_less1month" |
                                                                                                              inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == animal_cattle[j] & inputs_manure_cattle$manure == "pitstorage_greater1month" |
                                                                                                              inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == animal_cattle[j] & inputs_manure_cattle$manure == "pitstorage_greater1month_chickens" |
                                                                                                              inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == animal_cattle[j] & inputs_manure_cattle$manure == "composting" |
                                                                                                              inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == animal_cattle[j] & inputs_manure_cattle$manure == "poultry_with_litter", ][paste("y_", year, sep = "")][[1]])},
                              pct_mms_slurry = if(cohort_cattle[k] == "MFf" | cohort_cattle[k] == "MMf"){
                                as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == "feedlot" & inputs_manure_cattle$manure == "liquid_slurry" |
                                                                      inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == "feedlot" & inputs_manure_cattle$manure == "liquid_slurry_crust" |
                                                                      inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == "feedlot" & inputs_manure_cattle$manure == "anaerobic_lagoon", ][paste("y_", year, sep = "")][[1]]) }else{
                                                                        as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == animal_cattle[j] & inputs_manure_cattle$manure == "liquid_slurry" |
                                                                                                              inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == animal_cattle[j] & inputs_manure_cattle$manure == "liquid_slurry_crust" |
                                                                                                              inputs_manure_cattle$variable == "Manure mgmt system" & inputs_manure_cattle$mgmt == mgmt_cattle[i] & inputs_manure_cattle$animal == animal_cattle[j] & inputs_manure_cattle$manure == "anaerobic_lagoon", ][paste("y_", year, sep = "")][[1]])},
                              
                              
                              
                              direct_ef_solid = manure[ which(manure$manure_management_systems == "solid_storage" | manure$manure_management_systems == "dry_lot"  | manure$manure_management_systems == "pitstorage_less1month"  | manure$manure_management_systems == "pitstorage_greater1month"  | manure$manure_management_systems == "pitstorage_greater1month_chickens" | manure$manure_management_systems == "composting" | manure$manure_management_systems == "poultry_with_litter" ), ]["direct_ef_n2o_ruminants"][[1]], 
                              direct_ef_slurry = manure[ which(manure$manure_management_systems == "liquid_slurry" | manure$manure_management_systems == "liquid_slurry_crust"  | manure$manure_management_systems == "anaerobic_lagoon"), ]["direct_ef_n2o_ruminants"][[1]],
                                ef_vol = as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "ef_vol" & inputs_manure_cattle$species == "cattle", ][paste("y_", year, sep = "")][[1]]),
                              frac_leach_solid = manure[ which(manure$manure_management_systems == "solid_storage" | manure$manure_management_systems == "dry_lot"  | manure$manure_management_systems == "pitstorage_less1month"  | manure$manure_management_systems == "pitstorage_greater1month"  | manure$manure_management_systems == "pitstorage_greater1month_chickens" | manure$manure_management_systems == "composting" | manure$manure_management_systems == "poultry_with_litter" ), ]["frac_leach"][[1]]/100, 
                              frac_leach_slurry = manure[ which(manure$manure_management_systems == "liquid_slurry" | manure$manure_management_systems == "liquid_slurry_crust"  | manure$manure_management_systems == "anaerobic_lagoon"), ]["frac_leach"][[1]]/100, 
                                ef_leach = as.numeric(inputs_manure_cattle[inputs_manure_cattle$variable == "ef_leach" & inputs_manure_cattle$species == "cattle", ][paste("y_", year, sep = "")][[1]])))

    }
  }
}

}else if(species_lists$species[count] == "sheep"){
  
  for(i in seq(from=1, to=length(mgmt_sheep), by=1)) {
    for(j in seq(from=1, to=length(animal_sheep), by=1)) {
      for(k in seq(from=1, to=length(cohort_sheep_2), by=1)){
        
        if(animal_sheep[j] == "non-dairy"){value = "_nd"}else{value = ""}
        
        assign(paste("manure_emissions_available", cohort_sheep_2[k], animal_sheep[j], mgmt_sheep[i], sep = "_"),
               manure_excretion_storage_emis(cohort = cohort_sheep_2[k], 
                                             number_cows = as.numeric(get(paste("herd", mgmt_sheep[i], sep = "_"))[paste(cohort_sheep_2[k], value, sep = "")]), 
                                             n_excretion_total = as.numeric(get(paste("nitrogen_excretion", cohort_sheep_2[k], animal_sheep[j], mgmt_sheep[i], sep = "_")))[1], 
                                             
                                             prop_graze = as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "Manure mgmt system" & inputs_manure_sheep$mgmt == mgmt_sheep[i] & inputs_manure_sheep$manure == "pasture", ][paste("y_", year, sep = "")][[1]])/100,
                                             
                                             prop_yard = as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "prop_yard" & inputs_manure_sheep$animal == animal_sheep[i], ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA dairy sheep 0.25, other sheep 0.1
                                             
                                             prop_house = 1- as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "Manure mgmt system" & inputs_manure_sheep$mgmt == mgmt_sheep[i] & inputs_manure_sheep$manure == "pasture", ][paste("y_", year, sep = "")][[1]])/100 - 
                                               as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "prop_yard" & inputs_manure_sheep$animal == animal_sheep[i], ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA dairy/other sheep 0.5, 
                                             
                                             prop_TAN = as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "prop_TAN", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP EEA default Table 3.9 
                                             
                                             prop_slurry = sum(as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "Manure mgmt system" & inputs_manure_sheep$mgmt == mgmt_sheep[i] & inputs_manure_sheep$manure == "liquid_slurry" |
                                                                                     inputs_manure_sheep$variable == "Manure mgmt system" & inputs_manure_sheep$mgmt == mgmt_sheep[i] & inputs_manure_sheep$manure == "liquid_slurry_crust" |
                                                                                     inputs_manure_sheep$variable == "Manure mgmt system" & inputs_manure_sheep$mgmt == mgmt_sheep[i] & inputs_manure_sheep$manure == "anaerobic_lagoon", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE),
                                             
                                             ef_house_slurry = as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "ef_house_slurry" & inputs_manure_sheep$species == "sheep", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                             ef_house_solid  = as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "ef_house_solid" & inputs_manure_sheep$species == "sheep", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                             ef_yard = as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "ef_yard" & inputs_manure_sheep$species == "sheep", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                             
                                             straw = as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "straw" & inputs_manure_sheep$species == "sheep", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.7 adjusted to full year equivalent 
                                             N_straw = as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "N_straw" & inputs_manure_sheep$species == "sheep", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.7 adjusted to full year equivalent
                                             frac_imm = as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "frac_imm" & inputs_manure_sheep$species == "sheep", ][paste("y_", year, sep = "")][[1]]),
                                             
                                             prop_store_slurry = 1-sum(as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "Manure mgmt system" & inputs_manure_sheep$mgmt == mgmt_sheep[i] & inputs_manure_sheep$manure == "anaerobic_digester", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE),
                                             
                                             prop_biogas = sum(as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "Manure mgmt system" & inputs_manure_sheep$mgmt == mgmt_sheep[i] & inputs_manure_sheep$manure == "anaerobic_digester", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE),
                                             
                                             
                                             prop_store_solid = 1-sum(as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "Manure mgmt system" & inputs_manure_sheep$mgmt == mgmt_sheep[i] & inputs_manure_sheep$manure == "anaerobic_digester", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE)-sum(as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "Manure mgmt system" & inputs_manure_sheep$mgmt == mgmt_sheep[i] & inputs_manure_sheep$manure == "burned_fuel", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE),
                                             
                                             prop_burnfuel = sum(as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "Manure mgmt system" & inputs_manure_sheep$mgmt == mgmt_sheep[i] & inputs_manure_sheep$manure == "burned_fuel", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE),
                                             
                                             
                                             frac_min = as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "frac_min" & inputs_manure_sheep$species == "sheep", ][paste("y_", year, sep = "")][[1]]), 
                                             
                                             ef_storage_slurry_nh3 =  as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "ef_storage_slurry_nh3" & inputs_manure_sheep$species == "sheep", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                             ef_storage_slurry_no = as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "ef_storage_slurry_no" & inputs_manure_sheep$species == "sheep", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.10 
                                             ef_storage_slurry_n2 = as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "ef_storage_slurry_n2" & inputs_manure_sheep$species == "sheep", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                             ef_storage_solid_nh3 = as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "ef_storage_solid_nh3" & inputs_manure_sheep$species == "sheep", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                             ef_storage_solid_no = as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "ef_storage_solid_no" & inputs_manure_sheep$species == "sheep", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9 
                                             ef_storage_solid_n2 = as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "ef_storage_solid_n2" & inputs_manure_sheep$species == "sheep", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9 
                                             
                                             pct_mms_solid = as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "Manure mgmt system" & inputs_manure_sheep$mgmt == mgmt_sheep[i] & inputs_manure_sheep$manure == "solid_storage" |
                                                                                 inputs_manure_sheep$variable == "Manure mgmt system" & inputs_manure_sheep$mgmt == mgmt_sheep[i] & inputs_manure_sheep$manure == "dry_lot" |
                                                                                 inputs_manure_sheep$variable == "Manure mgmt system" & inputs_manure_sheep$mgmt == mgmt_sheep[i] & inputs_manure_sheep$manure == "pitstorage_less1month" |
                                                                                 inputs_manure_sheep$variable == "Manure mgmt system" & inputs_manure_sheep$mgmt == mgmt_sheep[i] & inputs_manure_sheep$manure == "pitstorage_greater1month" |
                                                                                 inputs_manure_sheep$variable == "Manure mgmt system" & inputs_manure_sheep$mgmt == mgmt_sheep[i] & inputs_manure_sheep$manure == "pitstorage_greater1month_chickens" |
                                                                                 inputs_manure_sheep$variable == "Manure mgmt system" & inputs_manure_sheep$mgmt == mgmt_sheep[i] & inputs_manure_sheep$manure == "composting" |
                                                                                 inputs_manure_sheep$variable == "Manure mgmt system" & inputs_manure_sheep$mgmt == mgmt_sheep[i] & inputs_manure_sheep$manure == "poultry_with_litter", ][paste("y_", year, sep = "")][[1]]),
                                             
                                             pct_mms_slurry = as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "Manure mgmt system" & inputs_manure_sheep$mgmt == mgmt_sheep[i] & inputs_manure_sheep$manure == "liquid_slurry" |
                                                                                 inputs_manure_sheep$variable == "Manure mgmt system" & inputs_manure_sheep$mgmt == mgmt_sheep[i] & inputs_manure_sheep$manure == "liquid_slurry_crust" |
                                                                                 inputs_manure_sheep$variable == "Manure mgmt system" & inputs_manure_sheep$mgmt == mgmt_sheep[i] & inputs_manure_sheep$manure == "anaerobic_lagoon", ][paste("y_", year, sep = "")][[1]]),
                                             
                                             direct_ef_solid = manure[ which(manure$manure_management_systems == "solid_storage" | manure$manure_management_systems == "dry_lot"  | manure$manure_management_systems == "pitstorage_less1month"  | manure$manure_management_systems == "pitstorage_greater1month"  | manure$manure_management_systems == "pitstorage_greater1month_chickens" | manure$manure_management_systems == "composting" | manure$manure_management_systems == "poultry_with_litter" ), ]["direct_ef_n2o_ruminants"][[1]], 
                                             direct_ef_slurry = manure[ which(manure$manure_management_systems == "liquid_slurry" | manure$manure_management_systems == "liquid_slurry_crust"  | manure$manure_management_systems == "anaerobic_lagoon"), ]["direct_ef_n2o_ruminants"][[1]],
                                             ef_vol = as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "ef_vol" & inputs_manure_sheep$species == "sheep", ][paste("y_", year, sep = "")][[1]]),
                                             frac_leach_solid = manure[ which(manure$manure_management_systems == "solid_storage" | manure$manure_management_systems == "dry_lot"  | manure$manure_management_systems == "pitstorage_less1month"  | manure$manure_management_systems == "pitstorage_greater1month"  | manure$manure_management_systems == "pitstorage_greater1month_chickens" | manure$manure_management_systems == "composting" | manure$manure_management_systems == "poultry_with_litter" ), ]["frac_leach"][[1]]/100, 
                                             frac_leach_slurry = manure[ which(manure$manure_management_systems == "liquid_slurry" | manure$manure_management_systems == "liquid_slurry_crust"  | manure$manure_management_systems == "anaerobic_lagoon"), ]["frac_leach"][[1]]/100, 
                                             ef_leach = as.numeric(inputs_manure_sheep[inputs_manure_sheep$variable == "ef_leach" & inputs_manure_sheep$species == "sheep", ][paste("y_", year, sep = "")][[1]])))
        
      }
    }
  }



}else if(species_lists$species[count] == "pig"){
  
  for(i in seq(from=1, to=length(mgmt_pig), by=1)) {
    for(j in seq(from=1, to=length(animal_pig), by=1)) {
      for(k in seq(from=1, to=length(cohort_pig), by=1)){
        
        
        assign(paste("manure_emissions_available", cohort_pig[k], animal_pig[j], mgmt_pig[i], sep = "_"),
               manure_excretion_storage_emis(cohort = cohort_pig[k], 
                                             number_cows = as.numeric(get(paste("herd", mgmt_pig[i], sep = "_"))[paste(cohort_pig[k], sep = "_")]), 
                                             n_excretion_total = as.numeric(get(paste("nitrogen_excretion", cohort_pig[k], animal_pig[j], mgmt_pig[i], sep = "_")))[1], 
                                             
                                             prop_graze = as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "Manure mgmt system" & inputs_manure_pig$mgmt == mgmt_pig[i] & inputs_manure_pig$manure == "pasture", ][paste("y_", year, sep = "")][[1]])/100,
                                             
                                             prop_yard = as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "prop_yard" & inputs_manure_pig$mgmt == mgmt_pig[i], ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA dairy pig 0.25, other pig 0.1
                                             
                                             prop_house = 1- as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "Manure mgmt system" & inputs_manure_pig$mgmt == mgmt_pig[i] & inputs_manure_pig$manure == "pasture", ][paste("y_", year, sep = "")][[1]])/100 - 
                                               as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "prop_yard" & inputs_manure_pig$mgmt == mgmt_pig[i], ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA dairy/other pig 0.5, 
                                             
                                             prop_TAN = as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "prop_TAN", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP EEA default Table 3.9 
                                             
                                             prop_slurry = sum(as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "Manure mgmt system" & inputs_manure_pig$mgmt == mgmt_pig[i] & inputs_manure_pig$manure == "liquid_slurry" |
                                                                                                inputs_manure_pig$variable == "Manure mgmt system" & inputs_manure_pig$mgmt == mgmt_pig[i] & inputs_manure_pig$manure == "liquid_slurry_crust" |
                                                                                                inputs_manure_pig$variable == "Manure mgmt system" & inputs_manure_pig$mgmt == mgmt_pig[i] & inputs_manure_pig$manure == "anaerobic_lagoon", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE),
                                             
                                             ef_house_slurry = as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "ef_house_slurry" & inputs_manure_pig$species == "pig", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                             ef_house_solid  = as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "ef_house_solid" & inputs_manure_pig$species == "pig", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                             ef_yard = as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "ef_yard" & inputs_manure_pig$species == "pig", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                             
                                             straw = as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "straw" & inputs_manure_pig$species == "pig", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.7 adjusted to full year equivalent 
                                             N_straw = as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "N_straw" & inputs_manure_pig$species == "pig", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.7 adjusted to full year equivalent
                                             frac_imm = as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "frac_imm" & inputs_manure_pig$species == "pig", ][paste("y_", year, sep = "")][[1]]),
                                             
                                             prop_store_slurry = 1-sum(as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "Manure mgmt system" & inputs_manure_pig$mgmt == mgmt_pig[i] & inputs_manure_pig$manure == "anaerobic_digester", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE),
                                             
                                             prop_biogas = sum(as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "Manure mgmt system" & inputs_manure_pig$mgmt == mgmt_pig[i] & inputs_manure_pig$manure == "anaerobic_digester", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE),
                                             
                                             
                                             prop_store_solid = 1-sum(as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "Manure mgmt system" & inputs_manure_pig$mgmt == mgmt_pig[i] & inputs_manure_pig$manure == "anaerobic_digester", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE)-sum(as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "Manure mgmt system" & inputs_manure_pig$mgmt == mgmt_pig[i] & inputs_manure_pig$manure == "burned_fuel", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE),
                                             
                                             prop_burnfuel = sum(as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "Manure mgmt system" & inputs_manure_pig$mgmt == mgmt_pig[i] & inputs_manure_pig$manure == "burned_fuel", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE),
                                             
                                             
                                             frac_min = as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "frac_min" & inputs_manure_pig$species == "pig", ][paste("y_", year, sep = "")][[1]]), 
                                             
                                             ef_storage_slurry_nh3 =  as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "ef_storage_slurry_nh3" & inputs_manure_pig$species == "pig", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                             ef_storage_slurry_no = as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "ef_storage_slurry_no" & inputs_manure_pig$species == "pig", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.10 
                                             ef_storage_slurry_n2 = as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "ef_storage_slurry_n2" & inputs_manure_pig$species == "pig", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                             ef_storage_solid_nh3 = as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "ef_storage_solid_nh3" & inputs_manure_pig$species == "pig", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                             ef_storage_solid_no = as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "ef_storage_solid_no" & inputs_manure_pig$species == "pig", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9 
                                             ef_storage_solid_n2 = as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "ef_storage_solid_n2" & inputs_manure_pig$species == "pig", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9 
                                             
                                             pct_mms_solid = as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "Manure mgmt system" & inputs_manure_pig$mgmt == mgmt_pig[i] & inputs_manure_pig$manure == "solid_storage" |
                                                                                              inputs_manure_pig$variable == "Manure mgmt system" & inputs_manure_pig$mgmt == mgmt_pig[i] & inputs_manure_pig$manure == "dry_lot" |
                                                                                              inputs_manure_pig$variable == "Manure mgmt system" & inputs_manure_pig$mgmt == mgmt_pig[i] & inputs_manure_pig$manure == "pitstorage_less1month" |
                                                                                              inputs_manure_pig$variable == "Manure mgmt system" & inputs_manure_pig$mgmt == mgmt_pig[i] & inputs_manure_pig$manure == "pitstorage_greater1month" |
                                                                                              inputs_manure_pig$variable == "Manure mgmt system" & inputs_manure_pig$mgmt == mgmt_pig[i] & inputs_manure_pig$manure == "pitstorage_greater1month_chickens" |
                                                                                              inputs_manure_pig$variable == "Manure mgmt system" & inputs_manure_pig$mgmt == mgmt_pig[i] & inputs_manure_pig$manure == "composting" |
                                                                                              inputs_manure_pig$variable == "Manure mgmt system" & inputs_manure_pig$mgmt == mgmt_pig[i] & inputs_manure_pig$manure == "poultry_with_litter", ][paste("y_", year, sep = "")][[1]]),
                                             
                                             pct_mms_slurry = as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "Manure mgmt system" & inputs_manure_pig$mgmt == mgmt_pig[i] & inputs_manure_pig$manure == "liquid_slurry" |
                                                                                               inputs_manure_pig$variable == "Manure mgmt system" & inputs_manure_pig$mgmt == mgmt_pig[i] & inputs_manure_pig$manure == "liquid_slurry_crust" |
                                                                                               inputs_manure_pig$variable == "Manure mgmt system" & inputs_manure_pig$mgmt == mgmt_pig[i] & inputs_manure_pig$manure == "anaerobic_lagoon", ][paste("y_", year, sep = "")][[1]]),
                                             
                                             direct_ef_solid = manure[ which(manure$manure_management_systems == "solid_storage" | manure$manure_management_systems == "dry_lot"  | manure$manure_management_systems == "pitstorage_less1month"  | manure$manure_management_systems == "pitstorage_greater1month"  | manure$manure_management_systems == "pitstorage_greater1month_chickens" | manure$manure_management_systems == "composting" | manure$manure_management_systems == "poultry_with_litter" ), ]["direct_ef_n2o_monogastrics"][[1]], 
                                             direct_ef_slurry = manure[ which(manure$manure_management_systems == "liquid_slurry" | manure$manure_management_systems == "liquid_slurry_crust"  | manure$manure_management_systems == "anaerobic_lagoon"), ]["direct_ef_n2o_monogastrics"][[1]],
                                             ef_vol = as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "ef_vol" & inputs_manure_pig$species == "pig", ][paste("y_", year, sep = "")][[1]]),
                                             frac_leach_solid = manure[ which(manure$manure_management_systems == "solid_storage" | manure$manure_management_systems == "dry_lot"  | manure$manure_management_systems == "pitstorage_less1month"  | manure$manure_management_systems == "pitstorage_greater1month"  | manure$manure_management_systems == "pitstorage_greater1month_chickens" | manure$manure_management_systems == "composting" | manure$manure_management_systems == "poultry_with_litter" ), ]["frac_leach"][[1]]/100, 
                                             frac_leach_slurry = manure[ which(manure$manure_management_systems == "liquid_slurry" | manure$manure_management_systems == "liquid_slurry_crust"  | manure$manure_management_systems == "anaerobic_lagoon"), ]["frac_leach"][[1]]/100, 
                                             ef_leach = as.numeric(inputs_manure_pig[inputs_manure_pig$variable == "ef_leach" & inputs_manure_pig$species == "pig", ][paste("y_", year, sep = "")][[1]])))
        
      }
    }
  }
  
  
  
}else if(species_lists$species[count] == "chicken"){
  
  for(i in seq(from=1, to=length(mgmt_chicken), by=1)) {
    for(j in seq(from=1, to=length(animal_chicken), by=1)) {
      for(k in seq(from=1, to=length(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))), by=1)){
        
        
        assign(paste("manure_emissions_available",  get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k], animal_chicken[j], mgmt_chicken[i], sep = "_"),
               manure_excretion_storage_emis(cohort =  get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k], 
                                             number_cows = as.numeric(get(paste("herd", mgmt_chicken[i], sep = "_"))[paste(get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k], sep = "_")]),
                                             n_excretion_total = as.numeric(get(paste("nitrogen_excretion", get(paste("cohort_chicken_", mgmt_chicken[i], sep = ""))[k], animal_chicken[j], mgmt_chicken[i], sep = "_")))[1], 
                                             
                                             prop_graze = as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "Manure mgmt system" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$manure == "pasture", ][paste("y_", year, sep = "")][[1]])/100,
                                             
                                             prop_yard = as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "prop_yard" & inputs_manure_chicken$mgmt == mgmt_chicken[i], ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA dairy chicken 0.25, other chicken 0.1
                                             
                                             prop_house = 1- as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "Manure mgmt system" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$manure == "pasture", ][paste("y_", year, sep = "")][[1]])/100 - 
                                               as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "prop_yard" & inputs_manure_chicken$mgmt == mgmt_chicken[i], ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA dairy/other chicken 0.5, 
                                             
                                             prop_TAN = as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "prop_TAN", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP EEA default Table 3.9 
                                             
                                             prop_slurry = sum(as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "Manure mgmt system" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$manure == "liquid_slurry" |
                                                                                              inputs_manure_chicken$variable == "Manure mgmt system" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$manure == "liquid_slurry_crust" |
                                                                                              inputs_manure_chicken$variable == "Manure mgmt system" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$manure == "anaerobic_lagoon", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE),
                                             
                                             ef_house_slurry = as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "ef_house_slurry" & inputs_manure_chicken$species == "chicken", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                             ef_house_solid  = as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "ef_house_solid" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$species == "chicken", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                             ef_yard = as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "ef_yard" & inputs_manure_chicken$species == "chicken", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                             
                                             straw = as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "straw" & inputs_manure_chicken$species == "chicken", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.7 adjusted to full year equivalent 
                                             N_straw = as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "N_straw" & inputs_manure_chicken$species == "chicken", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.7 adjusted to full year equivalent
                                             frac_imm = as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "frac_imm" & inputs_manure_chicken$species == "chicken", ][paste("y_", year, sep = "")][[1]]),
                                             
                                             prop_store_slurry = 1-sum(as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "Manure mgmt system" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$manure == "anaerobic_digester", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE),
                                             
                                             prop_biogas = sum(as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "Manure mgmt system" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$manure == "anaerobic_digester", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE),
                                             
                                             
                                             prop_store_solid = 1-sum(as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "Manure mgmt system" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$manure == "anaerobic_digester", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE)-sum(as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "Manure mgmt system" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$manure == "burned_fuel", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE),
                                             
                                             prop_burnfuel = sum(as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "Manure mgmt system" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$manure == "burned_fuel", ][paste("y_", year, sep = "")][[1]])/100, na.rm = TRUE),
                                             
                                             
                                             frac_min = as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "frac_min" & inputs_manure_chicken$species == "chicken", ][paste("y_", year, sep = "")][[1]]), 
                                             
                                             ef_storage_slurry_nh3 =  as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "ef_storage_slurry_nh3" & inputs_manure_chicken$species == "chicken", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                             ef_storage_slurry_no = as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "ef_storage_slurry_no" & inputs_manure_chicken$species == "chicken", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.10 
                                             ef_storage_slurry_n2 = as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "ef_storage_slurry_n2" & inputs_manure_chicken$species == "chicken", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                             ef_storage_solid_nh3 = as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "ef_storage_solid_nh3" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$species == "chicken", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9
                                             ef_storage_solid_no = as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "ef_storage_solid_no" & inputs_manure_chicken$species == "chicken", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9 
                                             ef_storage_solid_n2 = as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "ef_storage_solid_n2" & inputs_manure_chicken$species == "chicken", ][paste("y_", year, sep = "")][[1]]), ##Tier 2 EMEP/EEA Table 3.9 
                                             
                                             pct_mms_solid = as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "Manure mgmt system" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$manure == "pasture" | ##pasture included for chicken as is included in GLEAM and assumed to be similar to feedlot
                                                                                                inputs_manure_chicken$variable == "Manure mgmt system" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$manure == "solid_storage" |
                                                                                            inputs_manure_chicken$variable == "Manure mgmt system" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$manure == "dry_lot" |
                                                                                            inputs_manure_chicken$variable == "Manure mgmt system" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$manure == "pitstorage_less1month" |
                                                                                            inputs_manure_chicken$variable == "Manure mgmt system" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$manure == "pitstorage_greater1month" |
                                                                                            inputs_manure_chicken$variable == "Manure mgmt system" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$manure == "pitstorage_greater1month_chickens" |
                                                                                            inputs_manure_chicken$variable == "Manure mgmt system" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$manure == "composting" |
                                                                                            inputs_manure_chicken$variable == "Manure mgmt system" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$manure == "poultry_with_litter", ][paste("y_", year, sep = "")][[1]]),
                                             
                                             pct_mms_slurry = as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "Manure mgmt system" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$manure == "liquid_slurry" |
                                                                                             inputs_manure_chicken$variable == "Manure mgmt system" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$manure == "liquid_slurry_crust" |
                                                                                             inputs_manure_chicken$variable == "Manure mgmt system" & inputs_manure_chicken$mgmt == mgmt_chicken[i] & inputs_manure_chicken$manure == "anaerobic_lagoon", ][paste("y_", year, sep = "")][[1]]),
                                             
                                             direct_ef_solid = manure[ which(manure$manure_management_systems == "pasture" | manure$manure_management_systems == "solid_storage" | manure$manure_management_systems == "dry_lot"  | manure$manure_management_systems == "pitstorage_less1month"  | manure$manure_management_systems == "pitstorage_greater1month"  | manure$manure_management_systems == "pitstorage_greater1month_chickens" | manure$manure_management_systems == "composting" | manure$manure_management_systems == "poultry_with_litter" ), ]["direct_ef_n2o_monogastrics"][[1]], 
                                             direct_ef_slurry = manure[ which(manure$manure_management_systems == "liquid_slurry" | manure$manure_management_systems == "liquid_slurry_crust"  | manure$manure_management_systems == "anaerobic_lagoon"), ]["direct_ef_n2o_monogastrics"][[1]],
                                             ef_vol = as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "ef_vol" & inputs_manure_chicken$species == "chicken", ][paste("y_", year, sep = "")][[1]]),
                                             frac_leach_solid = manure[ which(manure$manure_management_systems == "pasture" | manure$manure_management_systems == "solid_storage" | manure$manure_management_systems == "dry_lot"  | manure$manure_management_systems == "pitstorage_less1month"  | manure$manure_management_systems == "pitstorage_greater1month"  | manure$manure_management_systems == "pitstorage_greater1month_chickens" | manure$manure_management_systems == "composting" | manure$manure_management_systems == "poultry_with_litter" ), ]["frac_leach"][[1]]/100, 
                                             frac_leach_slurry = manure[ which(manure$manure_management_systems == "liquid_slurry" | manure$manure_management_systems == "liquid_slurry_crust"  | manure$manure_management_systems == "anaerobic_lagoon"), ]["frac_leach"][[1]]/100, 
                                             ef_leach = as.numeric(inputs_manure_chicken[inputs_manure_chicken$variable == "ef_leach" & inputs_manure_chicken$species == "chicken", ][paste("y_", year, sep = "")][[1]])))
        
      }
    }
  }

  
  
}  
  
  




##Summary of feed requirements, enteric and manure methane emissions
consumption_waste <- data.frame(country = numeric(0), year = numeric(0), species = numeric(0), kcal_consumed = numeric(0), g_consumed = numeric(0), kcal_wasted = numeric(0), g_wasted = numeric(0))


enteric_methane_summary <- data.frame(country = numeric(0), year = numeric(0), species = numeric(0), mgmt = numeric(0), animal = numeric(0), cohort = numeric(0), ch4_emis_kg = numeric(0))
manure_methane_summary <- data.frame(country = numeric(0), year = numeric(0), species = numeric(0), mgmt = numeric(0), animal = numeric(0), cohort = numeric(0), ch4_emis_kg = numeric(0))

manure_n2o_direct_slurry_summary <- data.frame(country = numeric(0), year = numeric(0), species = numeric(0), mgmt = numeric(0), animal = numeric(0), cohort = numeric(0), n2o_emis_kg = numeric(0))
manure_n2o_vol_slurry_summary <- data.frame(country = numeric(0), year = numeric(0), species = numeric(0), mgmt = numeric(0), animal = numeric(0), cohort = numeric(0), n2o_emis_kg = numeric(0))
manure_n2o_leach_slurry_summary <- data.frame(country = numeric(0), year = numeric(0), species = numeric(0), mgmt = numeric(0), animal = numeric(0), cohort = numeric(0), n2o_emis_kg = numeric(0))

manure_n2o_direct_solid_summary <- data.frame(country = numeric(0), year = numeric(0), species = numeric(0), mgmt = numeric(0), animal = numeric(0), cohort = numeric(0), n2o_emis_kg = numeric(0))
manure_n2o_vol_solid_summary <- data.frame(country = numeric(0), year = numeric(0), species = numeric(0), mgmt = numeric(0), animal = numeric(0), cohort = numeric(0), n2o_emis_kg = numeric(0))
manure_n2o_leach_solid_summary <- data.frame(country = numeric(0), year = numeric(0), species = numeric(0), mgmt = numeric(0), animal = numeric(0), cohort = numeric(0), n2o_emis_kg = numeric(0))


manure_nh3_emis_house_slurry_summary <- data.frame(country = numeric(0), year = numeric(0), species = numeric(0), mgmt = numeric(0), animal = numeric(0), cohort = numeric(0), nh3_emis_kg = numeric(0))
manure_nh3_emis_house_solid_summary <- data.frame(country = numeric(0), year = numeric(0), species = numeric(0), mgmt = numeric(0), animal = numeric(0), cohort = numeric(0), nh3_emis_kg = numeric(0))
manure_nh3_emis_yard_summary <- data.frame(country = numeric(0), year = numeric(0), species = numeric(0), mgmt = numeric(0), animal = numeric(0), cohort = numeric(0), nh3_emis_kg = numeric(0))
manure_nh3_emis_storage_slurry_summary <- data.frame(country = numeric(0), year = numeric(0), species = numeric(0), mgmt = numeric(0), animal = numeric(0), cohort = numeric(0), nh3_emis_kg = numeric(0))
manure_nh3_emis_storage_solid_summary <- data.frame(country = numeric(0), year = numeric(0), species = numeric(0), mgmt = numeric(0), animal = numeric(0), cohort = numeric(0), nh3_emis_kg = numeric(0))

manure_no_emis_storage_slurry_summary <- data.frame(country = numeric(0), year = numeric(0), species = numeric(0), mgmt = numeric(0), animal = numeric(0), cohort = numeric(0), no_emis_kg = numeric(0))
manure_no_emis_storage_solid_summary <- data.frame(country = numeric(0), year = numeric(0), species = numeric(0), mgmt = numeric(0), animal = numeric(0), cohort = numeric(0), no_emis_kg = numeric(0))


manure_nitrogen_available_summary <- data.frame(country = numeric(0), year = numeric(0), species = numeric(0), mgmt = numeric(0), animal = numeric(0), cohort = numeric(0), TAN_applic_slurry = numeric(0), TAN_applic_solid = numeric(0), TAN_graze = numeric(0), N_applic_slurry = numeric(0), N_applic_solid = numeric(0), N_graze = numeric(0))


feed_requirements_summary <- data.frame(country = numeric(1), year = numeric(1), species = numeric(1), mgmt = numeric(1), animal = numeric(1), cohort = numeric(1))


for(i in seq(from=1, to=length(if(species_lists$species[count] == "cattle" | species_lists$species[count] == "sheep"){feed_categories_ruminants}else{feed_categories_monogastrics}), by=1)) {
    feed_requirements_summary[paste(if(species_lists$species[count] == "cattle" | species_lists$species[count] == "sheep"){feed_categories_ruminants[i]}else{feed_categories_monogastrics[i]})] <- NA
}
feed_requirements_summary <- feed_requirements_summary[-1,]


#for(i in seq(from=1, to=length(manure_management_systems), by=1)) {
#  manure_nitrogen_available_summary[paste(manure_management_systems[i])] <- NA
#}
#manure_nitrogen_available_summary <- manure_nitrogen_available_summary[-1,]


consumption_waste[(length(consumption_waste$country)+1),]$year = year
consumption_waste[(length(consumption_waste$country)),]$country = country
consumption_waste[(length(consumption_waste$country)),]$species = species_lists$species[count]
consumption_waste[(length(consumption_waste$country)),]$kcal_consumed = kcal_consumed
consumption_waste[(length(consumption_waste$country)),]$g_consumed = g_consumed
consumption_waste[(length(consumption_waste$country)),]$kcal_wasted = kcal_wasted
consumption_waste[(length(consumption_waste$country)),]$g_wasted = g_wasted

if(species_lists$species[count] == "cattle"){

consumption_waste[(length(consumption_waste$country)+1),]$year = year
consumption_waste[(length(consumption_waste$country)),]$country = country
consumption_waste[(length(consumption_waste$country)),]$species = "dairy"
consumption_waste[(length(consumption_waste$country)),]$kcal_consumed = kcal_consumed_dairy
consumption_waste[(length(consumption_waste$country)),]$g_consumed = g_consumed_dairy
consumption_waste[(length(consumption_waste$country)),]$kcal_wasted = kcal_wasted_dairy
consumption_waste[(length(consumption_waste$country)),]$g_wasted = g_wasted_dairy

}
if(species_lists$species[count] == "chicken"){

consumption_waste[(length(consumption_waste$country)+1),]$year = year
consumption_waste[(length(consumption_waste$country)),]$country = country
consumption_waste[(length(consumption_waste$country)),]$species = "eggs"
consumption_waste[(length(consumption_waste$country)),]$kcal_consumed = kcal_consumed_eggs
consumption_waste[(length(consumption_waste$country)),]$g_consumed = g_consumed_eggs
consumption_waste[(length(consumption_waste$country)),]$kcal_wasted = kcal_wasted_eggs
consumption_waste[(length(consumption_waste$country)),]$g_wasted = g_wasted_eggs

}




for(i in seq(from=1, to=length(get(paste("mgmt_", species_lists$species[count], sep = ""))), by=1)) {
  for(j in seq(from=1, to=length(get(paste("animal_", species_lists$species[count], sep = ""))), by=1)) {
    for(k in seq(from=1, to=length(if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))}else{get(paste("cohort_", species_lists$species[count], sep = ""))}), by=1)){

      if(species_lists$species[count] == "chicken"){
        
        enteric_methane_summary[(length(enteric_methane_summary$country)+1),]$year = year
        enteric_methane_summary[(length(enteric_methane_summary$country)),]$country = country
        enteric_methane_summary[(length(enteric_methane_summary$country)),]$species = species_lists$species[count]
        enteric_methane_summary[(length(enteric_methane_summary$country)),]$mgmt = get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
        enteric_methane_summary[(length(enteric_methane_summary$country)),]$animal = get(paste("animal_", species_lists$species[count], sep = ""))[j]
        enteric_methane_summary[(length(enteric_methane_summary$country)),]$cohort = if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]}
        enteric_methane_summary[(length(enteric_methane_summary$country)),]$ch4_emis_kg = 0        
        
      }else{
      
      enteric_methane_summary[(length(enteric_methane_summary$country)+1),]$year = year
      enteric_methane_summary[(length(enteric_methane_summary$country)),]$country = country
      enteric_methane_summary[(length(enteric_methane_summary$country)),]$species = species_lists$species[count]
      enteric_methane_summary[(length(enteric_methane_summary$country)),]$mgmt = get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
      enteric_methane_summary[(length(enteric_methane_summary$country)),]$animal = get(paste("animal_", species_lists$species[count], sep = ""))[j]
      enteric_methane_summary[(length(enteric_methane_summary$country)),]$cohort = if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]}
      enteric_methane_summary[(length(enteric_methane_summary$country)),]$ch4_emis_kg = get(paste("enteric_methane", if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]},
                                                                                                  get(paste("animal_", species_lists$species[count], sep = ""))[j], get(paste("mgmt_", species_lists$species[count], sep = ""))[i], sep = "_"))
}
      
      manure_methane_summary[(length(manure_methane_summary$country)+1),]$year = year
      manure_methane_summary[(length(manure_methane_summary$country)),]$country = country
      manure_methane_summary[(length(manure_methane_summary$country)),]$species = species_lists$species[count]
      manure_methane_summary[(length(manure_methane_summary$country)),]$mgmt = get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
      manure_methane_summary[(length(manure_methane_summary$country)),]$animal = get(paste("animal_", species_lists$species[count], sep = ""))[j]
      manure_methane_summary[(length(manure_methane_summary$country)),]$cohort = if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]}
      manure_methane_summary[(length(manure_methane_summary$country)),]$ch4_emis_kg = get(paste("manure_methane", if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]},
                                                                                                get(paste("animal_", species_lists$species[count], sep = ""))[j], get(paste("mgmt_", species_lists$species[count], sep = ""))[i], sep = "_"))
  
    
      
      manure_n2o_direct_slurry_summary[(length(manure_n2o_direct_slurry_summary$country)+1),]$year = year
      manure_n2o_direct_slurry_summary[(length(manure_n2o_direct_slurry_summary$country)),]$country = country
      manure_n2o_direct_slurry_summary[(length(manure_n2o_direct_slurry_summary$country)),]$species = species_lists$species[count]
      manure_n2o_direct_slurry_summary[(length(manure_n2o_direct_slurry_summary$country)),]$mgmt = get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
      manure_n2o_direct_slurry_summary[(length(manure_n2o_direct_slurry_summary$country)),]$animal = get(paste("animal_", species_lists$species[count], sep = ""))[j]
      manure_n2o_direct_slurry_summary[(length(manure_n2o_direct_slurry_summary$country)),]$cohort = if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]}
      manure_n2o_direct_slurry_summary[(length(manure_n2o_direct_slurry_summary$country)),]$n2o_emis_kg = get(paste("manure_emissions_available", if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]},
                                                                                                                    get(paste("animal_", species_lists$species[count], sep = ""))[j], get(paste("mgmt_", species_lists$species[count], sep = ""))[i], sep = "_"))$n2o_direct_slurry * 44/28
      
      manure_n2o_vol_slurry_summary[(length(manure_n2o_vol_slurry_summary$country)+1),]$year = year
      manure_n2o_vol_slurry_summary[(length(manure_n2o_vol_slurry_summary$country)),]$country = country
      manure_n2o_vol_slurry_summary[(length(manure_n2o_vol_slurry_summary$country)),]$species = species_lists$species[count]
      manure_n2o_vol_slurry_summary[(length(manure_n2o_vol_slurry_summary$country)),]$mgmt = get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
      manure_n2o_vol_slurry_summary[(length(manure_n2o_vol_slurry_summary$country)),]$animal = get(paste("animal_", species_lists$species[count], sep = ""))[j]
      manure_n2o_vol_slurry_summary[(length(manure_n2o_vol_slurry_summary$country)),]$cohort = if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]}
      manure_n2o_vol_slurry_summary[(length(manure_n2o_vol_slurry_summary$country)),]$n2o_emis_kg = get(paste("manure_emissions_available", if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]},
                                                                                                              get(paste("animal_", species_lists$species[count], sep = ""))[j], get(paste("mgmt_", species_lists$species[count], sep = ""))[i], sep = "_"))$n2o_vol_slurry * 44/28
      
      manure_n2o_leach_slurry_summary[(length(manure_n2o_leach_slurry_summary$country)+1),]$year = year
      manure_n2o_leach_slurry_summary[(length(manure_n2o_leach_slurry_summary$country)),]$country = country
      manure_n2o_leach_slurry_summary[(length(manure_n2o_leach_slurry_summary$country)),]$species = species_lists$species[count]
      manure_n2o_leach_slurry_summary[(length(manure_n2o_leach_slurry_summary$country)),]$mgmt = get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
      manure_n2o_leach_slurry_summary[(length(manure_n2o_leach_slurry_summary$country)),]$animal = get(paste("animal_", species_lists$species[count], sep = ""))[j]
      manure_n2o_leach_slurry_summary[(length(manure_n2o_leach_slurry_summary$country)),]$cohort = if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]}
      manure_n2o_leach_slurry_summary[(length(manure_n2o_leach_slurry_summary$country)),]$n2o_emis_kg = get(paste("manure_emissions_available", if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]},
                                                                                                                  get(paste("animal_", species_lists$species[count], sep = ""))[j], get(paste("mgmt_", species_lists$species[count], sep = ""))[i], sep = "_"))$n2o_leach_slurry *44/28
      
      manure_n2o_direct_solid_summary[(length(manure_n2o_direct_solid_summary$country)+1),]$year = year
      manure_n2o_direct_solid_summary[(length(manure_n2o_direct_solid_summary$country)),]$country = country
      manure_n2o_direct_solid_summary[(length(manure_n2o_direct_solid_summary$country)),]$species = species_lists$species[count]
      manure_n2o_direct_solid_summary[(length(manure_n2o_direct_solid_summary$country)),]$mgmt = get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
      manure_n2o_direct_solid_summary[(length(manure_n2o_direct_solid_summary$country)),]$animal = get(paste("animal_", species_lists$species[count], sep = ""))[j]
      manure_n2o_direct_solid_summary[(length(manure_n2o_direct_solid_summary$country)),]$cohort = if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]}
      manure_n2o_direct_solid_summary[(length(manure_n2o_direct_solid_summary$country)),]$n2o_emis_kg = get(paste("manure_emissions_available", if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]},
                                                                                                                  get(paste("animal_", species_lists$species[count], sep = ""))[j], get(paste("mgmt_", species_lists$species[count], sep = ""))[i], sep = "_"))$n2o_direct_solid * 44/28 ##convert kg N to kg N2O emitted
      
      manure_n2o_vol_solid_summary[(length(manure_n2o_vol_solid_summary$country)+1),]$year = year
      manure_n2o_vol_solid_summary[(length(manure_n2o_vol_solid_summary$country)),]$country = country
      manure_n2o_vol_solid_summary[(length(manure_n2o_vol_solid_summary$country)),]$species = species_lists$species[count]
      manure_n2o_vol_solid_summary[(length(manure_n2o_vol_solid_summary$country)),]$mgmt = get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
      manure_n2o_vol_solid_summary[(length(manure_n2o_vol_solid_summary$country)),]$animal = get(paste("animal_", species_lists$species[count], sep = ""))[j]
      manure_n2o_vol_solid_summary[(length(manure_n2o_vol_solid_summary$country)),]$cohort = if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]}
      manure_n2o_vol_solid_summary[(length(manure_n2o_vol_solid_summary$country)),]$n2o_emis_kg = get(paste("manure_emissions_available", if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]},
                                                                                                            get(paste("animal_", species_lists$species[count], sep = ""))[j], get(paste("mgmt_", species_lists$species[count], sep = ""))[i], sep = "_"))$n2o_vol_solid * 44/28 ##convert kg N to kg N2O emitted
      
      manure_n2o_leach_solid_summary[(length(manure_n2o_leach_solid_summary$country)+1),]$year = year
      manure_n2o_leach_solid_summary[(length(manure_n2o_leach_solid_summary$country)),]$country = country
      manure_n2o_leach_solid_summary[(length(manure_n2o_leach_solid_summary$country)),]$species = species_lists$species[count]
      manure_n2o_leach_solid_summary[(length(manure_n2o_leach_solid_summary$country)),]$mgmt = get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
      manure_n2o_leach_solid_summary[(length(manure_n2o_leach_solid_summary$country)),]$animal = get(paste("animal_", species_lists$species[count], sep = ""))[j]
      manure_n2o_leach_solid_summary[(length(manure_n2o_leach_solid_summary$country)),]$cohort = if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]}
      manure_n2o_leach_solid_summary[(length(manure_n2o_leach_solid_summary$country)),]$n2o_emis_kg = get(paste("manure_emissions_available", if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]},
                                                                                                                get(paste("animal_", species_lists$species[count], sep = ""))[j], get(paste("mgmt_", species_lists$species[count], sep = ""))[i], sep = "_"))$n2o_leach_solid *44/28 ##convert kg N to kg N2O emitted
      
    

      manure_nh3_emis_house_slurry_summary[(length(manure_nh3_emis_house_slurry_summary$country)+1),]$year = year
      manure_nh3_emis_house_slurry_summary[(length(manure_nh3_emis_house_slurry_summary$country)),]$country = country
      manure_nh3_emis_house_slurry_summary[(length(manure_nh3_emis_house_slurry_summary$country)),]$species = species_lists$species[count]
      manure_nh3_emis_house_slurry_summary[(length(manure_nh3_emis_house_slurry_summary$country)),]$mgmt = get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
      manure_nh3_emis_house_slurry_summary[(length(manure_nh3_emis_house_slurry_summary$country)),]$animal = get(paste("animal_", species_lists$species[count], sep = ""))[j]
      manure_nh3_emis_house_slurry_summary[(length(manure_nh3_emis_house_slurry_summary$country)),]$cohort = if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]}
      manure_nh3_emis_house_slurry_summary[(length(manure_nh3_emis_house_slurry_summary$country)),]$nh3_emis_kg = get(paste("manure_emissions_available", if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]},
                                                                                                                            get(paste("animal_", species_lists$species[count], sep = ""))[j], get(paste("mgmt_", species_lists$species[count], sep = ""))[i], sep = "_"))$NH3_emis_house_slurry *17/14 ##convert kg N to kg nh3 emitted
      
      manure_nh3_emis_house_solid_summary[(length(manure_nh3_emis_house_solid_summary$country)+1),]$year = year
      manure_nh3_emis_house_solid_summary[(length(manure_nh3_emis_house_solid_summary$country)),]$country = country
      manure_nh3_emis_house_solid_summary[(length(manure_nh3_emis_house_solid_summary$country)),]$species = species_lists$species[count]
      manure_nh3_emis_house_solid_summary[(length(manure_nh3_emis_house_solid_summary$country)),]$mgmt = get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
      manure_nh3_emis_house_solid_summary[(length(manure_nh3_emis_house_solid_summary$country)),]$animal = get(paste("animal_", species_lists$species[count], sep = ""))[j]
      manure_nh3_emis_house_solid_summary[(length(manure_nh3_emis_house_solid_summary$country)),]$cohort = if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]}
      manure_nh3_emis_house_solid_summary[(length(manure_nh3_emis_house_solid_summary$country)),]$nh3_emis_kg = get(paste("manure_emissions_available", if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]},
                                                                                                                          get(paste("animal_", species_lists$species[count], sep = ""))[j], get(paste("mgmt_", species_lists$species[count], sep = ""))[i], sep = "_"))$NH3_emis_house_solid *17/14 ##convert kg N to kg nh3 emitted
      
      manure_nh3_emis_yard_summary[(length(manure_nh3_emis_yard_summary$country)+1),]$year = year
      manure_nh3_emis_yard_summary[(length(manure_nh3_emis_yard_summary$country)),]$country = country
      manure_nh3_emis_yard_summary[(length(manure_nh3_emis_yard_summary$country)),]$species = species_lists$species[count]
      manure_nh3_emis_yard_summary[(length(manure_nh3_emis_yard_summary$country)),]$mgmt = get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
      manure_nh3_emis_yard_summary[(length(manure_nh3_emis_yard_summary$country)),]$animal = get(paste("animal_", species_lists$species[count], sep = ""))[j]
      manure_nh3_emis_yard_summary[(length(manure_nh3_emis_yard_summary$country)),]$cohort = if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]}
      manure_nh3_emis_yard_summary[(length(manure_nh3_emis_yard_summary$country)),]$nh3_emis_kg = get(paste("manure_emissions_available", if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]},
                                                                                                            get(paste("animal_", species_lists$species[count], sep = ""))[j], get(paste("mgmt_", species_lists$species[count], sep = ""))[i], sep = "_"))$NH3_emis_yard *17/14 ##convert kg N to kg nh3 emitted
      
      manure_nh3_emis_storage_slurry_summary[(length(manure_nh3_emis_storage_slurry_summary$country)+1),]$year = year
      manure_nh3_emis_storage_slurry_summary[(length(manure_nh3_emis_storage_slurry_summary$country)),]$country = country
      manure_nh3_emis_storage_slurry_summary[(length(manure_nh3_emis_storage_slurry_summary$country)),]$species = species_lists$species[count]
      manure_nh3_emis_storage_slurry_summary[(length(manure_nh3_emis_storage_slurry_summary$country)),]$mgmt = get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
      manure_nh3_emis_storage_slurry_summary[(length(manure_nh3_emis_storage_slurry_summary$country)),]$animal = get(paste("animal_", species_lists$species[count], sep = ""))[j]
      manure_nh3_emis_storage_slurry_summary[(length(manure_nh3_emis_storage_slurry_summary$country)),]$cohort = if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]}
      manure_nh3_emis_storage_slurry_summary[(length(manure_nh3_emis_storage_slurry_summary$country)),]$nh3_emis_kg = get(paste("manure_emissions_available", if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]},
                                                                                                                                get(paste("animal_", species_lists$species[count], sep = ""))[j], get(paste("mgmt_", species_lists$species[count], sep = ""))[i], sep = "_"))$NH3_storage_slurry *17/14 ##convert kg N to kg nh3 emitted
      
      manure_nh3_emis_storage_solid_summary[(length(manure_nh3_emis_storage_solid_summary$country)+1),]$year = year
      manure_nh3_emis_storage_solid_summary[(length(manure_nh3_emis_storage_solid_summary$country)),]$country = country
      manure_nh3_emis_storage_solid_summary[(length(manure_nh3_emis_storage_solid_summary$country)),]$species = species_lists$species[count]
      manure_nh3_emis_storage_solid_summary[(length(manure_nh3_emis_storage_solid_summary$country)),]$mgmt = get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
      manure_nh3_emis_storage_solid_summary[(length(manure_nh3_emis_storage_solid_summary$country)),]$animal = get(paste("animal_", species_lists$species[count], sep = ""))[j]
      manure_nh3_emis_storage_solid_summary[(length(manure_nh3_emis_storage_solid_summary$country)),]$cohort = if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]}
      manure_nh3_emis_storage_solid_summary[(length(manure_nh3_emis_storage_solid_summary$country)),]$nh3_emis_kg = get(paste("manure_emissions_available", if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]},
                                                                                                                              get(paste("animal_", species_lists$species[count], sep = ""))[j], get(paste("mgmt_", species_lists$species[count], sep = ""))[i], sep = "_"))$NH3_storage_solid *17/14 ##convert kg N to kg nh3 emitted
      
    
      
      manure_no_emis_storage_slurry_summary[(length(manure_no_emis_storage_slurry_summary$country)+1),]$year = year
      manure_no_emis_storage_slurry_summary[(length(manure_no_emis_storage_slurry_summary$country)),]$country = country
      manure_no_emis_storage_slurry_summary[(length(manure_no_emis_storage_slurry_summary$country)),]$species = species_lists$species[count]
      manure_no_emis_storage_slurry_summary[(length(manure_no_emis_storage_slurry_summary$country)),]$mgmt = get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
      manure_no_emis_storage_slurry_summary[(length(manure_no_emis_storage_slurry_summary$country)),]$animal = get(paste("animal_", species_lists$species[count], sep = ""))[j]
      manure_no_emis_storage_slurry_summary[(length(manure_no_emis_storage_slurry_summary$country)),]$cohort = if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]}
      manure_no_emis_storage_slurry_summary[(length(manure_no_emis_storage_slurry_summary$country)),]$no_emis_kg = get(paste("manure_emissions_available", if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]},
                                                                                                                             get(paste("animal_", species_lists$species[count], sep = ""))[j], get(paste("mgmt_", species_lists$species[count], sep = ""))[i], sep = "_"))$NO_storage_slurry *46/14 ##convert kg N to kg no (as NO2) emitted
      
      manure_no_emis_storage_solid_summary[(length(manure_no_emis_storage_solid_summary$country)+1),]$year = year
      manure_no_emis_storage_solid_summary[(length(manure_no_emis_storage_solid_summary$country)),]$country = country
      manure_no_emis_storage_solid_summary[(length(manure_no_emis_storage_solid_summary$country)),]$species = species_lists$species[count]
      manure_no_emis_storage_solid_summary[(length(manure_no_emis_storage_solid_summary$country)),]$mgmt = get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
      manure_no_emis_storage_solid_summary[(length(manure_no_emis_storage_solid_summary$country)),]$animal = get(paste("animal_", species_lists$species[count], sep = ""))[j]
      manure_no_emis_storage_solid_summary[(length(manure_no_emis_storage_solid_summary$country)),]$cohort = if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]}
      manure_no_emis_storage_solid_summary[(length(manure_no_emis_storage_solid_summary$country)),]$no_emis_kg = get(paste("manure_emissions_available", if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]},
                                                                                                                           get(paste("animal_", species_lists$species[count], sep = ""))[j], get(paste("mgmt_", species_lists$species[count], sep = ""))[i], sep = "_"))$NO_storage_solid *46/14 ##convert kg N to kg no (as NO2) emitted
      
    
      
      manure_nitrogen_available_summary[(length(manure_nitrogen_available_summary$country)+1),]$year = year
      manure_nitrogen_available_summary[(length(manure_nitrogen_available_summary$country)),]$country = country
      manure_nitrogen_available_summary[(length(manure_nitrogen_available_summary$country)),]$species = species_lists$species[count]
      manure_nitrogen_available_summary[(length(manure_nitrogen_available_summary$country)),]$mgmt = get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
      manure_nitrogen_available_summary[(length(manure_nitrogen_available_summary$country)),]$animal = get(paste("animal_", species_lists$species[count], sep = ""))[j]
      manure_nitrogen_available_summary[(length(manure_nitrogen_available_summary$country)),]$cohort = if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]}
      manure_nitrogen_available_summary[(length(manure_nitrogen_available_summary$country)),]$TAN_applic_slurry <- get(paste("manure_emissions_available", if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]},
                                                                                                                             get(paste("animal_", species_lists$species[count], sep = ""))[j], get(paste("mgmt_", species_lists$species[count], sep = ""))[i], sep = "_"))$TAN_applic_slurry
      manure_nitrogen_available_summary[(length(manure_nitrogen_available_summary$country)),]$TAN_applic_solid <- get(paste("manure_emissions_available", if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]},
                                                                                                                            get(paste("animal_", species_lists$species[count], sep = ""))[j], get(paste("mgmt_", species_lists$species[count], sep = ""))[i], sep = "_"))$TAN_applic_solid
      manure_nitrogen_available_summary[(length(manure_nitrogen_available_summary$country)),]$N_applic_slurry <- get(paste("manure_emissions_available", if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]},
                                                                                                                           get(paste("animal_", species_lists$species[count], sep = ""))[j], get(paste("mgmt_", species_lists$species[count], sep = ""))[i], sep = "_"))$N_applic_slurry
      manure_nitrogen_available_summary[(length(manure_nitrogen_available_summary$country)),]$N_applic_solid <- get(paste("manure_emissions_available", if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]},
                                                                                                                          get(paste("animal_", species_lists$species[count], sep = ""))[j], get(paste("mgmt_", species_lists$species[count], sep = ""))[i], sep = "_"))$N_applic_solid
      manure_nitrogen_available_summary[(length(manure_nitrogen_available_summary$country)),]$TAN_graze <- get(paste("manure_emissions_available", if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]},
                                                                                                                     get(paste("animal_", species_lists$species[count], sep = ""))[j], get(paste("mgmt_", species_lists$species[count], sep = ""))[i], sep = "_"))$TAN_graze
      manure_nitrogen_available_summary[(length(manure_nitrogen_available_summary$country)),]$N_graze <- get(paste("manure_emissions_available", if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]},
                                                                                                                   get(paste("animal_", species_lists$species[count], sep = ""))[j], get(paste("mgmt_", species_lists$species[count], sep = ""))[i], sep = "_"))$N_graze
      
      
      feed_requirements_summary[(length(feed_requirements_summary$country)+1),]$year = year
      feed_requirements_summary[(length(feed_requirements_summary$country)),]$country = country
      feed_requirements_summary[(length(feed_requirements_summary$country)),]$species = species_lists$species[count]
      feed_requirements_summary[(length(feed_requirements_summary$country)),]$mgmt = get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
      feed_requirements_summary[(length(feed_requirements_summary$country)),]$animal = get(paste("animal_", species_lists$species[count], sep = ""))[j]
      feed_requirements_summary[(length(feed_requirements_summary$country)),]$cohort = if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]}
      feed_requirements_summary[(length(feed_requirements_summary$country)),][7:(length(if(species_lists$species[count] == "cattle" | species_lists$species[count] == "sheep"){feed_categories_ruminants}else{feed_categories_monogastrics})+6)] <- get(paste("feed_requirements", if(species_lists$species[count] == "sheep"){get(paste("cohort_", species_lists$species[count], "_2", sep = ""))[k]}else if(species_lists$species[count] == "chicken"){get(paste("cohort_", species_lists$species[count], "_", mgmt_chicken[i], sep = ""))[k]}else{get(paste("cohort_", species_lists$species[count], sep = ""))[k]},
                                                                                                 get(paste("animal_", species_lists$species[count], sep = ""))[j], get(paste("mgmt_", species_lists$species[count], sep = ""))[i], sep = "_"))
      
}}}

assign(paste("consumption_waste", species_lists$species[count], sep = "_"),  consumption_waste)

assign(paste("enteric_methane_summary", species_lists$species[count], sep = "_"),  enteric_methane_summary)
assign(paste("manure_methane_summary", species_lists$species[count], sep = "_"), manure_methane_summary)
assign(paste("manure_n2o_direct_slurry_summary", species_lists$species[count], sep = "_"),  manure_n2o_direct_slurry_summary)
assign(paste("manure_n2o_vol_slurry_summary", species_lists$species[count], sep = "_"),  manure_n2o_vol_slurry_summary)
assign(paste("manure_n2o_leach_slurry_summary", species_lists$species[count], sep = "_"),  manure_n2o_leach_slurry_summary)
assign(paste("manure_n2o_direct_solid_summary", species_lists$species[count], sep = "_"),  manure_n2o_direct_solid_summary)
assign(paste("manure_n2o_vol_solid_summary", species_lists$species[count], sep = "_"),  manure_n2o_vol_solid_summary)
assign(paste("manure_n2o_leach_solid_summary", species_lists$species[count], sep = "_"),  manure_n2o_leach_solid_summary)
assign(paste("manure_nh3_emis_house_slurry_summary", species_lists$species[count], sep = "_"),  manure_nh3_emis_house_slurry_summary)
assign(paste("manure_nh3_emis_house_solid_summary", species_lists$species[count], sep = "_"),  manure_nh3_emis_house_solid_summary)
assign(paste("manure_nh3_emis_yard_summary", species_lists$species[count], sep = "_"),  manure_nh3_emis_yard_summary)
assign(paste("manure_nh3_emis_storage_slurry_summary", species_lists$species[count], sep = "_"),  manure_nh3_emis_storage_slurry_summary)
assign(paste("manure_nh3_emis_storage_solid_summary", species_lists$species[count], sep = "_"),  manure_nh3_emis_storage_solid_summary)
assign(paste("manure_no_emis_storage_slurry_summary", species_lists$species[count], sep = "_"),  manure_no_emis_storage_slurry_summary)
assign(paste("manure_no_emis_storage_solid_summary", species_lists$species[count], sep = "_"),  manure_no_emis_storage_solid_summary)
assign(paste("manure_nitrogen_available_summary", species_lists$species[count], sep = "_"),  manure_nitrogen_available_summary)
assign(paste("feed_requirements_summary", species_lists$species[count], sep = "_"),  feed_requirements_summary)






}


consumption_waste <- rbind.fill(consumption_waste_cattle, consumption_waste_sheep, consumption_waste_pig, consumption_waste_chicken)


enteric_methane_summary <- rbind.fill(enteric_methane_summary_cattle, enteric_methane_summary_sheep, enteric_methane_summary_pig, enteric_methane_summary_chicken)
manure_methane_summary <- rbind.fill(manure_methane_summary_cattle, manure_methane_summary_sheep, manure_methane_summary_pig, manure_methane_summary_chicken)
manure_n2o_direct_slurry_summary <- rbind.fill(manure_n2o_direct_slurry_summary_cattle, manure_n2o_direct_slurry_summary_sheep, manure_n2o_direct_slurry_summary_pig, manure_n2o_direct_slurry_summary_chicken)
manure_n2o_vol_slurry_summary <- rbind.fill(manure_n2o_vol_slurry_summary_cattle, manure_n2o_vol_slurry_summary_sheep, manure_n2o_vol_slurry_summary_pig, manure_n2o_vol_slurry_summary_chicken)
manure_n2o_leach_slurry_summary <- rbind.fill(manure_n2o_leach_slurry_summary_cattle, manure_n2o_leach_slurry_summary_sheep, manure_n2o_leach_slurry_summary_pig, manure_n2o_leach_slurry_summary_chicken)
manure_n2o_direct_solid_summary <- rbind.fill(manure_n2o_direct_solid_summary_cattle, manure_n2o_direct_solid_summary_sheep, manure_n2o_direct_solid_summary_pig, manure_n2o_direct_solid_summary_chicken)
manure_n2o_vol_solid_summary <- rbind.fill(manure_n2o_vol_solid_summary_cattle, manure_n2o_vol_solid_summary_sheep, manure_n2o_vol_solid_summary_pig, manure_n2o_vol_solid_summary_chicken)
manure_n2o_leach_solid_summary <- rbind.fill(manure_n2o_leach_solid_summary_cattle, manure_n2o_leach_solid_summary_sheep, manure_n2o_leach_solid_summary_pig, manure_n2o_leach_solid_summary_chicken)
manure_nh3_emis_house_slurry_summary <- rbind.fill(manure_nh3_emis_house_slurry_summary_cattle, manure_nh3_emis_house_slurry_summary_sheep, manure_nh3_emis_house_slurry_summary_pig, manure_nh3_emis_house_slurry_summary_chicken)
manure_nh3_emis_house_solid_summary <- rbind.fill(manure_nh3_emis_house_solid_summary_cattle, manure_nh3_emis_house_solid_summary_sheep, manure_nh3_emis_house_solid_summary_pig, manure_nh3_emis_house_solid_summary_chicken)
manure_nh3_emis_yard_summary <- rbind.fill(manure_nh3_emis_yard_summary_cattle, manure_nh3_emis_yard_summary_sheep, manure_nh3_emis_yard_summary_pig, manure_nh3_emis_yard_summary_chicken)
manure_nh3_emis_storage_slurry_summary <- rbind.fill(manure_nh3_emis_storage_slurry_summary_cattle, manure_nh3_emis_storage_slurry_summary_sheep, manure_nh3_emis_storage_slurry_summary_pig, manure_nh3_emis_storage_slurry_summary_chicken)
manure_nh3_emis_storage_solid_summary <- rbind.fill(manure_nh3_emis_storage_solid_summary_cattle, manure_nh3_emis_storage_solid_summary_sheep, manure_nh3_emis_storage_solid_summary_pig, manure_nh3_emis_storage_solid_summary_chicken)
manure_no_emis_storage_slurry_summary <- rbind.fill(manure_no_emis_storage_slurry_summary_cattle, manure_no_emis_storage_slurry_summary_sheep, manure_no_emis_storage_slurry_summary_pig, manure_no_emis_storage_slurry_summary_chicken)
manure_no_emis_storage_solid_summary <- rbind.fill(manure_no_emis_storage_solid_summary_cattle, manure_no_emis_storage_solid_summary_sheep, manure_no_emis_storage_solid_summary_pig, manure_no_emis_storage_solid_summary_chicken)
manure_nitrogen_available_summary <- rbind.fill(manure_nitrogen_available_summary_cattle, manure_nitrogen_available_summary_sheep, manure_nitrogen_available_summary_pig, manure_nitrogen_available_summary_chicken)
feed_requirements_summary <- rbind.fill(feed_requirements_summary_cattle, feed_requirements_summary_sheep, feed_requirements_summary_pig, feed_requirements_summary_chicken)



##Other meat consumption

animal_groups <- c("animal_fats", "seafood", "bees")
animal_fats <- c("Fats, Animals, Raw", "Fish, Body Oil", "Fish, Liver Oil", "Offals, Edible", "Meat, Other")
seafood <- c("Aquatic Animals, Others", "Cephalopods", "Crustaceans", "Demersal Fish", "Freshwater Fish", "Marine Fish, Other", "Molluscs, Other", "Pelagic Fish")
bees <- c("Honey")

for(group in seq(from= 1, to= length(animal_groups), by=1)) {
  
  animal_group <- get(paste(animal_groups[group]))
  
  for(h in seq(from= 1, to= length(animal_group), by=1)) {
    
    
    kcal_consumed <- (as.numeric(inputs_meat[ which(inputs_meat$variable == "Total Food supply"), ][paste("y_", year, sep = "")])) * (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Fraction Food supply consumed ", animal_group[h], sep = "")), ][paste("y_", year, sep = "")]))/
      (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Population", sep = "")), ][paste("y_", year, sep = "")]))/365 ##kcal/person/day consumed (i.e. not wasted)
    
    g_consumed <- kcal_consumed / (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Kcal per tonne ", animal_group[h], sep = "")), ][paste("y_", year, sep = "")]))*1000000 #g/person/day
    
    
    kcal_wasted <- (as.numeric(inputs_meat[ which(inputs_meat$variable == "Total Food supply"), ][paste("y_", year, sep = "")])) * (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Fraction Food supply wasted ", animal_group[h], sep = "")), ][paste("y_", year, sep = "")]))/
      (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Population", sep = "")), ][paste("y_", year, sep = "")]))/365 ##kcal/person/day wasted
    
    g_wasted <- kcal_wasted / (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Kcal per tonne ", animal_group[h], sep = "")), ][paste("y_", year, sep = "")]))*1000000 #g/person/day
    
    
    consumption_waste[(length(consumption_waste$country)+1),]$year = year
    consumption_waste[(length(consumption_waste$country)),]$country = country
    consumption_waste[(length(consumption_waste$country)),]$species = animal_group[h]
    consumption_waste[(length(consumption_waste$country)),]$kcal_consumed = kcal_consumed
    consumption_waste[(length(consumption_waste$country)),]$g_consumed = g_consumed
    consumption_waste[(length(consumption_waste$country)),]$kcal_wasted = kcal_wasted
    consumption_waste[(length(consumption_waste$country)),]$g_wasted = g_wasted
    
  }}









##Calculate Manure N (kg N ha-1 y-1)
##Use N available for application but subtract N applied on grasslands (check what to do with alfalfa and other fodder crops). Lassaletta et al. 2014 includes reference for study that includes values for % N applied to grasslands
##Lui 2010 SI suggests for developing countries that 90% of the manure is applied to croplands and the remaining 10% used for fodder crops
manure_N_kgN_ha_yr <- manure_N_application(manure_N_available_slurry = sum(manure_nitrogen_available_summary$N_applic_slurry, na.rm = TRUE),  
                                           manure_TAN_available_slurry = sum(manure_nitrogen_available_summary$TAN_applic_slurry, na.rm = TRUE), 
                                           manure_N_available_solid  = sum(manure_nitrogen_available_summary$N_applic_solid, na.rm = TRUE),  
                                           manure_TAN_available_solid = sum(manure_nitrogen_available_summary$TAN_applic_solid, na.rm = TRUE),
                                           manure_N_available_graze = sum(manure_nitrogen_available_summary$N_graze, na.rm = TRUE),
                                           manure_TAN_available_graze = sum(manure_nitrogen_available_summary$TAN_graze, na.rm = TRUE),
                                           fraction_cropland_slurry = as.numeric(inputs_crop[which(inputs_crop$variable == "Slurry manure Applied to Cropland"), ][paste("y_", year, sep = "")]), 
                                           fraction_cropland_solid = as.numeric(inputs_crop[which(inputs_crop$variable == "Solid manure Applied to Cropland"), ][paste("y_", year, sep = "")]),  
                                           cropland_ha = as.numeric(inputs_crop[which(inputs_crop$variable == "Cropland area"), ][paste("y_", year, sep = "")]), 
                                           grassland_ha = as.numeric(inputs_crop[which(inputs_crop$variable == "Grassland area"), ][paste("y_", year, sep = "")]))





##Crop Modelling
##Crop and grassland areas
##Crops not included in modelling
crop_names <- names(table(inputs_othercrop$crop))[1:length(names(table(inputs_othercrop$crop)))]

for(i in seq(from=1, to=length(crop_names), by=1)) {
  
  crop_name <- crop_names[i]
  
  if(!is.na(as.numeric(inputs_othercrop[which(inputs_othercrop$crop == crop_name & inputs_othercrop$variable == "Fraction food supply consumed"), ][paste("y_", year, sep = "")]))){
    if(as.numeric(inputs_othercrop[which(inputs_othercrop$crop == crop_name & inputs_othercrop$variable == "Fraction food supply consumed"), ][paste("y_", year, sep = "")]) > 0){
      
      kcal <- as.numeric(inputs_meat[which(inputs_meat$variable == "Total Food supply"), ][paste("y_", year, sep = "")]) * as.numeric(inputs_othercrop[which(inputs_othercrop$crop == crop_name & inputs_othercrop$variable == "Fraction food supply consumed"), ][paste("y_", year, sep = "")])+ #kcal/year
        as.numeric(inputs_meat[which(inputs_meat$variable == "Total Food supply"), ][paste("y_", year, sep = "")]) * as.numeric(inputs_othercrop[which(inputs_othercrop$crop == crop_name & inputs_othercrop$variable == "Fraction food supply wasted"), ][paste("y_", year, sep = "")])
      
      
      kcal_consumed <- (as.numeric(inputs_meat[ which(inputs_meat$variable == "Total Food supply"), ][paste("y_", year, sep = "")])) * as.numeric(inputs_othercrop[which(inputs_othercrop$crop == crop_name & inputs_othercrop$variable == "Fraction food supply consumed"), ][paste("y_", year, sep = "")])/
        (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Population", sep = "")), ][paste("y_", year, sep = "")]))/365 ##kcal/person/day consumed (i.e. not wasted)
      
      g_consumed <- kcal_consumed / as.numeric(inputs_othercrop[which(inputs_othercrop$crop == crop_name & inputs_othercrop$variable == "kcal per tonne"), ][paste("y_", year, sep = "")])*1000000 #g/person/day
      
      
      kcal_wasted <- (as.numeric(inputs_meat[ which(inputs_meat$variable == "Total Food supply"), ][paste("y_", year, sep = "")])) * as.numeric(inputs_othercrop[which(inputs_othercrop$crop == crop_name & inputs_othercrop$variable == "Fraction food supply wasted"), ][paste("y_", year, sep = "")])/
        (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Population", sep = "")), ][paste("y_", year, sep = "")]))/365 ##kcal/person/day wasted
      
      g_wasted <- kcal_wasted / as.numeric(inputs_othercrop[which(inputs_othercrop$crop == crop_name & inputs_othercrop$variable == "kcal per tonne"), ][paste("y_", year, sep = "")])*1000000 #g/person/day
      
      
      consumption_waste[(length(consumption_waste$country)+1),]$year = year
      consumption_waste[(length(consumption_waste$country)),]$country = country
      consumption_waste[(length(consumption_waste$country)),]$species = crop_name
      consumption_waste[(length(consumption_waste$country)),]$kcal_consumed = kcal_consumed
      consumption_waste[(length(consumption_waste$country)),]$g_consumed = g_consumed
      consumption_waste[(length(consumption_waste$country)),]$kcal_wasted = kcal_wasted
      consumption_waste[(length(consumption_waste$country)),]$g_wasted = g_wasted
      
    }
  }
}


crop_names <- names(table(inputs_crop$crop))[2:length(names(table(inputs_crop$crop)))]

for(i in seq(from=1, to=length(crop_names), by=1)) {

  crop_name <- crop_names[i]
  
  if(!is.na(as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "Fraction food supply consumed"), ][paste("y_", year, sep = "")]))){
  if(as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "Fraction food supply consumed"), ][paste("y_", year, sep = "")]) > 0){
  
  kcal <- as.numeric(inputs_meat[which(inputs_meat$variable == "Total Food supply"), ][paste("y_", year, sep = "")]) * as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "Fraction food supply consumed"), ][paste("y_", year, sep = "")])+ #kcal/year
    as.numeric(inputs_meat[which(inputs_meat$variable == "Total Food supply"), ][paste("y_", year, sep = "")]) * as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "Fraction food supply wasted"), ][paste("y_", year, sep = "")])
 
  
  kcal_consumed <- (as.numeric(inputs_meat[ which(inputs_meat$variable == "Total Food supply"), ][paste("y_", year, sep = "")])) * as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "Fraction food supply consumed"), ][paste("y_", year, sep = "")])/
    (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Population", sep = "")), ][paste("y_", year, sep = "")]))/365 ##kcal/person/day consumed (i.e. not wasted)
  
  g_consumed <- kcal_consumed / as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "kcal per tonne"), ][paste("y_", year, sep = "")])*1000000 #g/person/day
  
  
  kcal_wasted <- (as.numeric(inputs_meat[ which(inputs_meat$variable == "Total Food supply"), ][paste("y_", year, sep = "")])) * as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "Fraction food supply wasted"), ][paste("y_", year, sep = "")])/
    (as.numeric(inputs_meat[ which(inputs_meat$variable == paste("Population", sep = "")), ][paste("y_", year, sep = "")]))/365 ##kcal/person/day wasted
  
  g_wasted <- kcal_wasted / as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "kcal per tonne"), ][paste("y_", year, sep = "")])*1000000 #g/person/day
  
  
  consumption_waste[(length(consumption_waste$country)+1),]$year = year
  consumption_waste[(length(consumption_waste$country)),]$country = country
  consumption_waste[(length(consumption_waste$country)),]$species = crop_name
  consumption_waste[(length(consumption_waste$country)),]$kcal_consumed = kcal_consumed
  consumption_waste[(length(consumption_waste$country)),]$g_consumed = g_consumed
  consumption_waste[(length(consumption_waste$country)),]$kcal_wasted = kcal_wasted
  consumption_waste[(length(consumption_waste$country)),]$g_wasted = g_wasted
  
  
  
  
  domestic_consumption <- kcal / as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "kcal per tonne"), ][paste("y_", year, sep = "")]) ##tonnes

  }else{domestic_consumption <- 0}}else{domestic_consumption <- 0}
  
 
  feed2 <- (sum(feed_requirements_summary$GRAINS, na.rm = TRUE) * as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "Fraction grain feed"), ][paste("y_", year, sep = "")])) / 1000 ##tonnes

  
  ##define feed
  if(crop_name == "Maize and products"){
    feed2 <- sum(feed2, (sum(feed_requirements_summary$CORN, na.rm = TRUE) + sum(feed_requirements_summary$MAIZE, na.rm = TRUE)) / 1000, na.rm = TRUE) ##tonnes
  }else if(crop_name == "Barley and products"){
    feed2 <- sum(feed2, (sum(feed_requirements_summary$BARLEY, na.rm = TRUE)) / 1000, na.rm = TRUE) ##tonnes
  }else if(crop_name == "Cassava and products"){
    feed2 <- sum(feed2, (sum(feed_requirements_summary$CASSAVA, na.rm = TRUE)) / 1000, na.rm = TRUE) ##tonnes
  }else if(crop_name == "Wheat and products"){
    feed2 <- sum(feed2, (sum(feed_requirements_summary$WHEAT, na.rm = TRUE)) / 1000, na.rm = TRUE) ##tonnes
  }else if(crop_name == "Millet and products"){
    feed2 <- sum(feed2, (sum(feed_requirements_summary$MILLET, na.rm = TRUE)) / 1000, na.rm = TRUE) ##tonnes
  }else if(crop_name == "Rice and products"){
    feed2 <- sum(feed2, (sum(feed_requirements_summary$RICE, na.rm = TRUE)) / 1000, na.rm = TRUE) ##tonnes
  }else if(crop_name == "Sorghum and products"){
    feed2 <- sum(feed2, (sum(feed_requirements_summary$SORGHUM, na.rm = TRUE)) / 1000, na.rm = TRUE) ##tonnes
  }else if(crop_name == "Soyabeans"){
    feed2 <- sum(feed2, (sum(feed_requirements_summary$SOY, na.rm = TRUE)) / 1000, na.rm = TRUE) ##tonnes
  }else if(crop_name == "Bananas"){
    feed2 <- sum(feed2, (sum(feed_requirements_summary$BNFRUIT, na.rm = TRUE)) / 1000, na.rm = TRUE) ##tonnes
  }else if(crop_name == "Rape and Mustard Oil"){
    feed2 <- sum(feed2, (sum(feed_requirements_summary$RAPESEED, na.rm = TRUE)) / 1000, na.rm = TRUE) ##tonnes
  }else if(crop_name == "Soyabean Oil"){
    feed2 <- sum(feed2, (sum(feed_requirements_summary$SOYOIL, na.rm = TRUE)) / 1000, na.rm = TRUE) ##tonnes
  }else{
    feed2 <- sum(feed2, 0, na.rm = TRUE) ##tonnes
  }
  
  
  
  ##domestic_production
  domestic_production <- domestic_crop_production(feed = feed2, 
                                                  seed = as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "Seed"), ][paste("y_", year, sep = "")]), 
                                                  domestic_consumption = domestic_consumption, 
                                                  loss = as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "Loss"), ][paste("y_", year, sep = "")]),  
                                                  processing = as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "Processing"), ][paste("y_", year, sep = "")]), 
                                                  nonfood = as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "Nonfood"), ][paste("y_", year, sep = "")]), 
                                                  stock_var = as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "Stock var"), ][paste("y_", year, sep = "")]), 
                                                  export = as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "Export"), ][paste("y_", year, sep = "")]), 
                                                  import = as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "Import"), ][paste("y_", year, sep = "")]))##tonnes

  
  if(domestic_production < 0){
   domestic_production <- 0 
  }
  

  crop_yield <-  yield(pctN_in_crop = as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "N pct in crop"), ][paste("y_", year, sep = "")]), 
                       ##ymax = as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "ymax"), ][paste("y_", year, sep = "")]), 
                       land_area = as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "Area Harvested"), ][paste("y_", year, sep = "")]),
                       synthetic_fertiliser_N = as.numeric(inputs_crop[which(inputs_crop$variable == "Fertiliser Applied to Cropland" & inputs_crop$units == "kg N ha-1 y-1"), ][paste("y_", year, sep = "")]), 
                       manure_N = manure_N_kgN_ha_yr$manure_N_application_crops, 
                       fixed_N = as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "Fixed N"), ][paste("y_", year, sep = "")]), 
                       deposition_N = as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "N deposition"), ][paste("y_", year, sep = "")]), 
                       domestic_production = domestic_production*1000, 
                       residue_N = 0)##Change so that residue N is taken as value in previous year
  
  
  ##residue_feed 
  if(crop_name == "Maize and products"){
    residue_feed <- sum(feed_requirements_summary$ZSTOVER, na.rm = TRUE) ##kg
  }else if(crop_name == "Rice and products") {
    residue_feed <- sum(feed_requirements_summary$RSTRAW, na.rm = TRUE) ##kg
  }else if(crop_name == "Wheat and products") {
    residue_feed <- sum(feed_requirements_summary$WSTRAW, na.rm = TRUE) ##kg
  }else if(crop_name == "Barley and products") {
    residue_feed <- sum(feed_requirements_summary$BSTRAW, na.rm = TRUE) ##kg ##Scale by production of oats and barley
  }else if(crop_name == "Oats") {
    residue_feed <- sum(feed_requirements_summary$BSTRAW, na.rm = TRUE) ##kg
  }else if(crop_name == "Millet and products") {
    residue_feed <- sum(feed_requirements_summary$MSTOVER, na.rm = TRUE) ##kg
  }else if(crop_name == "Sorghum and products") {
    residue_feed <- sum(feed_requirements_summary$SSTOVER, na.rm = TRUE) ##kg
  }else if(crop_name == "Sugar cane") {
    residue_feed <- sum(feed_requirements_summary$TOPS, na.rm = TRUE) ##kg
  }else {
    residue_feed <- 0 ##kg
  }
  
  
  
  ##Crop Residue Burning
 
  ##Calculate crop residue based on production to feed in to N inputs calculation
  residue <- crop_residue(production = domestic_production, 
                          dry_matter_fraction = as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "Dry Matter Fraction"), ][paste("y_", year, sep = "")]), 
                          feed = residue_feed/1000, 
                          slope_ag = as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "slope_ag"), ][paste("y_", year, sep = "")]), 
                          intercept_ag = as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "intercept_ag"), ][paste("y_", year, sep = "")]), 
                          residue_ratio_agbg = as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "residue_ratio_agbg"), ][paste("y_", year, sep = "")]), 
                          fuel_pct = as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "fuel_pct"), ][paste("y_", year, sep = "")]), 
                          frac_burned = as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "frac_burned"), ][paste("y_", year, sep = "")]), 
                          fraction_oxidised = as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "Fraction Oxidised"), ][paste("y_", year, sep = "")]), 
                          ef = c(as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "EF CO residue burning"), ][paste("y_", year, sep = "")]),
                                 as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "EF NOx residue burning"), ][paste("y_", year, sep = "")]),
                                 as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "EF SO2 residue burning"), ][paste("y_", year, sep = "")]),
                                 as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "EF NMVOC residue burning"), ][paste("y_", year, sep = "")]),
                                 as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "EF NH3 residue burning"), ][paste("y_", year, sep = "")]),
                                 as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "EF PM10 residue burning"), ][paste("y_", year, sep = "")]),
                                 as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "EF PM2.5 residue burning"), ][paste("y_", year, sep = "")]),
                                 as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "EF BC residue burning"), ][paste("y_", year, sep = "")]),
                                 as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "EF OC residue burning"), ][paste("y_", year, sep = "")]),
                                 as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "EF CH4 residue burning"), ][paste("y_", year, sep = "")])), 
                          ncont_ag = as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "ncont_ag"), ][paste("y_", year, sep = "")]), 
                          ncont_bg = as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "ncont_bg"), ][paste("y_", year, sep = "")]), 
                          crop_area = crop_yield$land_area)##values are in tonnes
  
  
  
  
  machinery_emissions <- machinery_energy(land_area = crop_yield$land_area, fuel_consumption_per_hectare = as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "fuel consumption per hectare"), ][paste("y_", year, sep = "")]), 
                                          ef = c(as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "EF CO2 diesel consumption"), ][paste("y_", year, sep = "")]),
                                                 as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "EF CO diesel consumption"), ][paste("y_", year, sep = "")]),
                                                      as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "EF NOx diesel consumption"), ][paste("y_", year, sep = "")]),
                                                      as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "EF SO2 diesel consumption"), ][paste("y_", year, sep = "")]),
                                                      as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "EF NMVOC diesel consumption"), ][paste("y_", year, sep = "")]),
                                                      as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "EF NH3 diesel consumption"), ][paste("y_", year, sep = "")]),
                                                      as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "EF PM10 diesel consumption"), ][paste("y_", year, sep = "")]),
                                                      as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "EF PM2.5 diesel consumption"), ][paste("y_", year, sep = "")]),
                                                      as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "EF BC diesel consumption"), ][paste("y_", year, sep = "")]),
                                                      as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "EF OC diesel consumption"), ][paste("y_", year, sep = "")]),
                                                      as.numeric(inputs_crop[which(inputs_crop$crop == crop_name & inputs_crop$variable == "EF CH4 diesel consumption"), ][paste("y_", year, sep = "")])))
  
  
  
  
  fertiliser_data <- data.frame(fertiliser = inputs_crop[ which(inputs_crop$units == "fraction total Tonnes N"), ]$variable)
  fertiliser_data$frac <- NA
  fertiliser_data$nh3_ef <- NA
  fertiliser_data$no_ef <- NA
  fertiliser_data$n2o_direct_ef <- NA
  fertiliser_data$n2o_vol_ef <- NA
  fertiliser_data$n2o_leach_ef <- NA
  fertiliser_data$frac_leach <- NA
  
  fertiliser_data$nh3_input <- NA
  fertiliser_data$nitric_acid_req <- NA
  fertiliser_data$urea_req <- NA
  fertiliser_data$ammonium_nitrate_req <- NA
  fertiliser_data$map_req <- NA
  fertiliser_data$dap_req <- NA
  
  fertiliser_data$conversion <- NA ##convert tonnes N to tonnes fertiliser
  
  for(p in seq(from=1, to=length(fertiliser_data$fertiliser), by=1)) {

    fertiliser_data$frac[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "fraction total Tonnes N"), ][paste("y_", year, sep = "")])
    fertiliser_data$nh3_ef[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "NH3 EF kg NH3 kg N-1"), ][paste("y_", year, sep = "")])
    fertiliser_data$no_ef[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "NO EF kg NO2 kg N-1"), ][paste("y_", year, sep = "")])
    fertiliser_data$n2o_direct_ef[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "N2O direct EF kg N2O kg N-1"), ][paste("y_", year, sep = "")])
    fertiliser_data$n2o_vol_ef[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "N2O indirect vol EF kg N2O kg N-1"), ][paste("y_", year, sep = "")])
    fertiliser_data$n2o_leach_ef[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "N2O indirect leach EF kg N2O kg N-1"), ][paste("y_", year, sep = "")])
    fertiliser_data$frac_leach[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "Fraction leached"), ][paste("y_", year, sep = "")])

    fertiliser_data$nh3_input[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "kg NH3 req per tonne fertiliser"), ][paste("y_", year, sep = "")])
    fertiliser_data$nitric_acid_req[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "kg HNO3 req per tonne fertiliser"), ][paste("y_", year, sep = "")])
    fertiliser_data$urea_req[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "kg urea req per tonne fertiliser"), ][paste("y_", year, sep = "")])
    fertiliser_data$ammonium_nitrate_req[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "kg ammonium nitrate req per tonne fertiliser"), ][paste("y_", year, sep = "")])
    fertiliser_data$map_req[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "kg map req per tonne fertiliser"), ][paste("y_", year, sep = "")])
    fertiliser_data$dap_req[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "kg dap req per tonne fertiliser"), ][paste("y_", year, sep = "")])
    
    fertiliser_data$conversion[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "conversion tonnes N to tonnes fertiliser"), ][paste("y_", year, sep = "")])
    
  }  
  
  fertiliser_data$frac[is.na(fertiliser_data$frac)] <- 0
 

  fertiliser_energy_emissions <- fertiliser_energy(application_rate = as.numeric(inputs_crop[which(inputs_crop$variable == "Fertiliser Applied to Cropland" & inputs_crop$units == "kg N ha-1 y-1"), ][paste("y_", year, sep = "")]), 
                                                   crop_area = crop_yield$land_area, 
                                                   fertiliser_pct = fertiliser_data$frac, 
                                                   conversion_N_tonnes = fertiliser_data$conversion,
                                
                                nh3_req = fertiliser_data$nh3_input, 
                                nitric_acid_req =  fertiliser_data$nitric_acid_req,
                                nitric_acid_nh3_req = as.numeric(inputs_crop[which(inputs_crop$variable == "Ammonia inputs for nitric acid production"), ][paste("y_", year, sep = "")]),
                                urea_req = fertiliser_data$urea_req, 
                                urea_nh3_req = as.numeric(fertiliser_data[which(fertiliser_data$fertiliser == "Urea"),]$nh3_input),
                                
                                ammonium_nitrate_req = fertiliser_data$ammonium_nitrate_req,
                                ammonium_nitrate_nh3_req = as.numeric(fertiliser_data[which(fertiliser_data$fertiliser == "Ammonium nitrate (AN)"),]$nh3_input),
                                ammonium_nitrate_nitric_acid_req = as.numeric(fertiliser_data[which(fertiliser_data$fertiliser == "Ammonium nitrate (AN)"),]$nitric_acid_req),
                                
                                map_req = fertiliser_data$map_req,
                                map_nh3_req = as.numeric(fertiliser_data[which(fertiliser_data$fertiliser == "Monoammonium phosphate (MAP)"),]$nh3_input),
                                dap_req = fertiliser_data$dap_req,
                                dap_nh3_req = as.numeric(fertiliser_data[which(fertiliser_data$fertiliser == "Diammonium phosphate (DAP)"),]$nh3_input),
                                
                                
                                ammonia_energy_input = as.numeric(inputs_crop[which(inputs_crop$variable == "NH3 prod energy input"), ][paste("y_", year, sep = "")]),
                                ammonia_fuel_gas_prop = as.numeric(inputs_crop[which(inputs_crop$variable == "NH3 prod gas prop"), ][paste("y_", year, sep = "")]),
                                ammonia_fuel_oil_prop = as.numeric(inputs_crop[which(inputs_crop$variable == "NH3 prod oil prop"), ][paste("y_", year, sep = "")]),
                                ammonia_fuel_coal_prop = as.numeric(inputs_crop[which(inputs_crop$variable == "NH3 prod coal prop"), ][paste("y_", year, sep = "")]),
                                
                                nitric_acid_n2o_ef = as.numeric(inputs_crop[which(inputs_crop$variable == "nitric acid N2O ef"), ][paste("y_", year, sep = "")]),
                                
                                ef_gas = c(as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas CO2 ef"), ][paste("y_", year, sep = "")]),
                                           as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas CO ef"), ][paste("y_", year, sep = "")]),
                                           as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas NOx ef"), ][paste("y_", year, sep = "")]),
                                           as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas SO2 ef"), ][paste("y_", year, sep = "")]),
                                           as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas NMVOC ef"), ][paste("y_", year, sep = "")]),
                                           as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas NH3 ef"), ][paste("y_", year, sep = "")]),
                                           as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas PM10 ef"), ][paste("y_", year, sep = "")]),
                                           as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas PM2.5 ef"), ][paste("y_", year, sep = "")]),
                                           as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas BC ef"), ][paste("y_", year, sep = "")]),
                                           as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas OC ef"), ][paste("y_", year, sep = "")]),
                                           as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas CH4 ef"), ][paste("y_", year, sep = "")])),
                                ef_oil = c(as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil CO2 ef"), ][paste("y_", year, sep = "")]),
                                           as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil CO ef"), ][paste("y_", year, sep = "")]),
                                           as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil NOx ef"), ][paste("y_", year, sep = "")]),
                                           as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil SO2 ef"), ][paste("y_", year, sep = "")]),
                                           as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil NMVOC ef"), ][paste("y_", year, sep = "")]),
                                           as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil NH3 ef"), ][paste("y_", year, sep = "")]),
                                           as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil PM10 ef"), ][paste("y_", year, sep = "")]),
                                           as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil PM2.5 ef"), ][paste("y_", year, sep = "")]),
                                           as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil BC ef"), ][paste("y_", year, sep = "")]),
                                           as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil OC ef"), ][paste("y_", year, sep = "")]),
                                           as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil CH4 ef"), ][paste("y_", year, sep = "")])),
                                ef_coal = c(as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal CO2 ef"), ][paste("y_", year, sep = "")]),
                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal CO ef"), ][paste("y_", year, sep = "")]),
                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal NOx ef"), ][paste("y_", year, sep = "")]),
                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal SO2 ef"), ][paste("y_", year, sep = "")]),
                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal NMVOC ef"), ][paste("y_", year, sep = "")]),
                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal NH3 ef"), ][paste("y_", year, sep = "")]),
                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal PM10 ef"), ][paste("y_", year, sep = "")]),
                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal PM2.5 ef"), ][paste("y_", year, sep = "")]),
                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal BC ef"), ][paste("y_", year, sep = "")]),
                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal OC ef"), ][paste("y_", year, sep = "")]),
                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal CH4 ef"), ][paste("y_", year, sep = "")])))
  
  
  
  
  
  ##Fertiliser Application
  ##Ammonia emissions use EMEP/EEA 2019 Tier 2 approach
  fertiliser_data$nh3_synthetic_fertiliser_kgNH3 <- emis_manure_fertiliser_applied(application_rate = as.numeric(inputs_crop[which(inputs_crop$variable == "Fertiliser Applied to Cropland" & inputs_crop$units == "kg N ha-1 y-1"), ][paste("y_", year, sep = "")]), 
                                                                               crop_area = crop_yield$land_area, 
                                                                               fertiliser_pct = fertiliser_data$frac * 100, 
                                                                               ef = fertiliser_data$nh3_ef)
  
  fertiliser_data$no_synthetic_fertiliser_kgNO2 <- emis_manure_fertiliser_applied(application_rate = as.numeric(inputs_crop[which(inputs_crop$variable == "Fertiliser Applied to Cropland" & inputs_crop$units == "kg N ha-1 y-1"), ][paste("y_", year, sep = "")]), 
                                                                              crop_area = crop_yield$land_area, 
                                                                              fertiliser_pct = fertiliser_data$frac * 100, 
                                                                              ef = fertiliser_data$no_ef) ##EF tier 1 EMEP/EEA 2019 (0.04 kg NO2 kg N-1 applied)
  
  fertiliser_data$n2o_direct_synthetic_fertiliser_kgN2O <- emis_manure_fertiliser_applied(application_rate = as.numeric(inputs_crop[which(inputs_crop$variable == "Fertiliser Applied to Cropland" & inputs_crop$units == "kg N ha-1 y-1"), ][paste("y_", year, sep = "")]), 
                                                                                          crop_area = crop_yield$land_area, 
                                                                                          fertiliser_pct = fertiliser_data$frac * 100, 
                                                                                          ef = (fertiliser_data$n2o_direct_ef)) ##EF tier 1 IPCC 2006 Table 11.1 kg N2O as N kg N-1 applied (conversion to kg N2O applied)
  
  fertiliser_data$n2o_indirect_vol_synthetic_fertiliser_kgN2O <- indirect_n2o_vol_fertiliser(NH3_N_emitted = (fertiliser_data$nh3_synthetic_fertiliser_kgNH3 / 17/14), 
                                                                                             NO_N_emitted = (fertiliser_data$no_synthetic_fertiliser_kgNO2 / 46/14), 
                                                                                             ef = fertiliser_data$n2o_vol_ef)   ##EF tier 1 IPCC 2006 Table 11.3 kg N2O as N kg N-1 applied (conversion to kg N2O applied)
  
  
  fertiliser_data$n2o_indirect_leach_synthetic_fertiliser_kgN2O <- indirect_n2o_leach_fertiliser(application_rate = as.numeric(inputs_crop[which(inputs_crop$variable == "Fertiliser Applied to Cropland" & inputs_crop$units == "kg N ha-1 y-1"), ][paste("y_", year, sep = "")]), 
                                                                                                 crop_area = crop_yield$land_area, 
                                                                                                 frac =  fertiliser_data$frac_leach, 
                                                                                                 fertiliser_pct = fertiliser_data$frac * 100, 
                                                                                                 ef = fertiliser_data$n2o_leach_ef) ##EF tier 1 IPCC 2006 Table 11.3 kg N2O as N kg N-1 applied (conversion to kg N2O applied)
  
  
  ##Emissions manure application
  NH3_manure_application_crop_slurry <- emis_manure_fertiliser_applied(application_rate = manure_N_kgN_ha_yr$manure_TAN_application_crops_slurry, 
                                                                       crop_area = crop_yield$land_area, 
                                                                       ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure slurry NH3 ef"), ][paste("y_", year, sep = "")]), 
                                                                       fertiliser_pct = 100)*17/14 
  
  NH3_manure_application_crop_solid <- emis_manure_fertiliser_applied(application_rate = manure_N_kgN_ha_yr$manure_TAN_application_crops_solid, 
                                                                      crop_area = crop_yield$land_area, 
                                                                      ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure solid NH3 ef"), ][paste("y_", year, sep = "")]), 
                                                                      fertiliser_pct = 100)*17/14
  
  NO_manure_application_crop_slurry <- emis_manure_fertiliser_applied(application_rate = manure_N_kgN_ha_yr$manure_N_application_crops_slurry, 
                                                                      crop_area = crop_yield$land_area, 
                                                                      ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure slurry NO ef"), ][paste("y_", year, sep = "")]), 
                                                                      fertiliser_pct = 100) ##EMEP/EEA TIER 1 Category 3D Table 3.1
  
  NO_manure_application_crop_solid <- emis_manure_fertiliser_applied(application_rate = manure_N_kgN_ha_yr$manure_N_application_crops_solid, 
                                                                     crop_area = crop_yield$land_area, 
                                                                     ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure solid NO ef"), ][paste("y_", year, sep = "")]), 
                                                                     fertiliser_pct = 100) ##EMEP/EEA TIER 1 Category 3D Table 3.1
  
  N2O_direct_manure_application_crop_slurry <- emis_manure_fertiliser_applied(application_rate = manure_N_kgN_ha_yr$manure_N_application_crops_slurry, 
                                                                              crop_area = crop_yield$land_area, 
                                                                              ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure slurry N2O direct ef"), ][paste("y_", year, sep = "")]), 
                                                                              fertiliser_pct = 100)*44/28 
  
  N2O_direct_manure_application_crop_solid <- emis_manure_fertiliser_applied(application_rate = manure_N_kgN_ha_yr$manure_N_application_crops_solid, 
                                                                             crop_area = crop_yield$land_area, 
                                                                             ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure solid N2O direct ef"), ][paste("y_", year, sep = "")]), 
                                                                             fertiliser_pct = 100)*44/28
  
  N2O_indirect_vol_manure_application_crop_slurry <- indirect_n2o_vol_fertiliser(NH3_N_emitted = (NH3_manure_application_crop_slurry / (17/14)), 
                                                                                 NO_N_emitted = (NO_manure_application_crop_slurry / (46/14)), 
                                                                                 ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure slurry N2O vol ef"), ][paste("y_", year, sep = "")])) * 44/28  ##EF tier 1 IPCC 2006 Table 11.3 kg N2O as N kg N-1 applied (conversion to kg N2O applied)
  
  N2O_indirect_vol_manure_application_crop_solid <- indirect_n2o_vol_fertiliser(NH3_N_emitted = (NH3_manure_application_crop_solid / (17/14)), 
                                                                                NO_N_emitted = (NO_manure_application_crop_solid / (46/14)), 
                                                                                ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure solid N2O vol ef"), ][paste("y_", year, sep = "")])) * 44/28  ##EF tier 1 IPCC 2006 Table 11.3 kg N2O as N kg N-1 applied (conversion to kg N2O applied)
  
  
  N2O_indirect_leach_manure_application_crop_slurry <- indirect_n2o_leach_fertiliser(application_rate = manure_N_kgN_ha_yr$manure_N_application_crops_slurry, 
                                                                                     crop_area = crop_yield$land_area, 
                                                                                     ef = (as.numeric(inputs_crop[which(inputs_crop$variable == "Manure slurry N2O leach ef"), ][paste("y_", year, sep = "")])), 
                                                                                     fertiliser_pct = 100, 
                                                                                     frac = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure slurry frac leach"), ][paste("y_", year, sep = "")])) * 44/28 ##EF tier 1 IPCC 2006 Table 11.3 kg N2O as N kg N-1 applied (conversion to kg N2O applied)
  
  N2O_indirect_leach_manure_application_crop_solid <- indirect_n2o_leach_fertiliser(application_rate = manure_N_kgN_ha_yr$manure_N_application_crops_solid, 
                                                                                    crop_area = crop_yield$land_area, 
                                                                                    ef = (as.numeric(inputs_crop[which(inputs_crop$variable == "Manure solid N2O leach ef"), ][paste("y_", year, sep = "")])), 
                                                                                    fertiliser_pct = 100, 
                                                                                    frac = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure solid frac leach"), ][paste("y_", year, sep = "")])) * 44/28 ##EF tier 1 IPCC 2006 Table 11.3 kg N2O as N kg N-1 applied (conversion to kg N2O applied)
  
  
  
  manure_emis <- c(NH3_manure_application_crop_slurry, NH3_manure_application_crop_solid, NO_manure_application_crop_slurry, NO_manure_application_crop_solid, N2O_direct_manure_application_crop_slurry, N2O_direct_manure_application_crop_solid, N2O_indirect_vol_manure_application_crop_slurry, N2O_indirect_vol_manure_application_crop_solid, N2O_indirect_leach_manure_application_crop_slurry, N2O_indirect_leach_manure_application_crop_solid)
  names(manure_emis) <- c("NH3_manure_application_crop_slurry", "NH3_manure_application_crop_solid", "NO_manure_application_crop_slurry", "NO_manure_application_crop_solid", "N2O_direct_manure_application_crop_slurry", "N2O_direct_manure_application_crop_solid", "N2O_indirect_vol_manure_application_crop_slurry", "N2O_indirect_vol_manure_application_crop_solid", "N2O_indirect_leach_manure_application_crop_slurry", "N2O_indirect_leach_manure_application_crop_solid")

  
  
  
  ##Emissions Rice Water Management
  if(crop_name == "Rice and products"){
    ch4_emis_rice <- ch4_rice(land_area = crop_yield$land_area, 
                              irrigated_prop = as.numeric(inputs_crop[which(inputs_crop$variable == "Irrigated rice area prop"),][paste("y_", year, sep = "")]), 
                              baseline_ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Rice Baseline EF"),][paste("y_", year, sep = "")]), 
                              wrsf = c(as.numeric(inputs_crop[which(inputs_crop$variable == "Water Regime Scaling Factor - Irrigated"),][paste("y_", year, sep = "")]), as.numeric(inputs_crop[which(inputs_crop$variable == "Water Regime Scaling Factor - Rainfed"),][paste("y_", year, sep = "")])), 
                              pre_sf = as.numeric(inputs_crop[which(inputs_crop$variable == "Pre Cultivation Water Regime Scaling Factor"),][paste("y_", year, sep = "")]), 
                              cfoa = c(as.numeric(inputs_crop[which(inputs_crop$variable == "Conversion Factor Organic Amendment - Straw"),][paste("y_", year, sep = "")]), as.numeric(inputs_crop[which(inputs_crop$variable == "Conversion Factor Organic Amendment - Manure"),][paste("y_", year, sep = "")]), as.numeric(inputs_crop[which(inputs_crop$variable == "Conversion Factor Organic Amendment - Compost"),][paste("y_", year, sep = "")])), 
                              roa = c(0, 0, 0), 
                              cultivation_period = as.numeric(inputs_crop[which(inputs_crop$variable == "Rice Cultivation Period"),][paste("y_", year, sep = "")]),
                              number_gs = as.numeric(inputs_crop[which(inputs_crop$variable == "Number of rice growing seasons"),][paste("y_", year, sep = "")]))##kg CH4 y-1
  }else{
    ch4_emis_rice <- 0
    
  }
  
  my_list <- list("domestic_production" = domestic_production, 
                  "crop_yield" = crop_yield,
                  "residue_feed" = residue_feed,
                  "residue" = residue, 
                  "fertiliser_data" = fertiliser_data, 
                  "manure_emis" = manure_emis, 
                  "ch4_emis_rice" = ch4_emis_rice,
                  "machinery_emissions " = machinery_emissions,
                  "fertiliser_energy_emissions" = fertiliser_energy_emissions)
  
  assign(paste("outputs_", crop_name, sep = ""), my_list)
  ##assign("consumption_waste", consumption_waste)
  count <- i
  
}




feed_crops <- c("GRAINSIL", "MAIZESIL", "FDDRBEET") ##Other feed crops, e.g. grains and corn, added to feed above for crops

for(i in seq(from=1, to=length(feed_crops), by=1)) {
  
  crop_name <- feed_crops[i]
  

##Feed crops

domestic_production <- sum(feed_requirements_summary[c(crop_name)], na.rm = TRUE)/1000 ##tonnes

crop_yield <-  yield_feedcrop(pctN_in_crop = as.numeric(inputs_feedcrop[which(inputs_feedcrop$crop == crop_name & inputs_feedcrop$variable == "N pct in crop"), ][paste("y_", year, sep = "")]), 
                     ymax = as.numeric(inputs_feedcrop[which(inputs_feedcrop$crop == crop_name & inputs_feedcrop$variable == "ymax"), ][paste("y_", year, sep = "")]), 
                     synthetic_fertiliser_N = as.numeric(inputs_crop[which(inputs_crop$variable == "Fertiliser Applied to Cropland" & inputs_crop$units == "kg N ha-1 y-1"), ][paste("y_", year, sep = "")]), 
                     manure_N = manure_N_kgN_ha_yr$manure_N_application_crops, 
                     fixed_N = as.numeric(inputs_feedcrop[which(inputs_feedcrop$crop == crop_name & inputs_feedcrop$variable == "Fixed N"), ][paste("y_", year, sep = "")]),  ##kg N ha-1 y-1
                     deposition_N = as.numeric(inputs_feedcrop[which(inputs_feedcrop$crop == crop_name & inputs_feedcrop$variable == "N deposition"), ][paste("y_", year, sep = "")]), 
                     domestic_production = domestic_production*1000)


##Fertiliser Application
##Ammonia emissions use EMEP/EEA 2019 Tier 2 approach for 

fertiliser_data <- data.frame(fertiliser = inputs_crop[ which(inputs_crop$units == "fraction total Tonnes N"), ]$variable)
fertiliser_data$frac <- NA
fertiliser_data$nh3_ef <- NA
fertiliser_data$no_ef <- NA
fertiliser_data$n2o_direct_ef <- NA
fertiliser_data$n2o_vol_ef <- NA
fertiliser_data$n2o_leach_ef <- NA
fertiliser_data$frac_leach <- NA

fertiliser_data$nh3_input <- NA
fertiliser_data$nitric_acid_req <- NA
fertiliser_data$urea_req <- NA
fertiliser_data$ammonium_nitrate_req <- NA
fertiliser_data$map_req <- NA
fertiliser_data$dap_req <- NA

fertiliser_data$conversion <- NA ##convert tonnes N to tonnes fertiliser

for(p in seq(from=1, to=length(fertiliser_data$fertiliser), by=1)) {
  
  fertiliser_data$frac[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "fraction total Tonnes N"), ][paste("y_", year, sep = "")])
  fertiliser_data$nh3_ef[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "NH3 EF kg NH3 kg N-1"), ][paste("y_", year, sep = "")])
  fertiliser_data$no_ef[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "NO EF kg NO2 kg N-1"), ][paste("y_", year, sep = "")])
  fertiliser_data$n2o_direct_ef[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "N2O direct EF kg N2O kg N-1"), ][paste("y_", year, sep = "")])
  fertiliser_data$n2o_vol_ef[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "N2O indirect vol EF kg N2O kg N-1"), ][paste("y_", year, sep = "")])
  fertiliser_data$n2o_leach_ef[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "N2O indirect leach EF kg N2O kg N-1"), ][paste("y_", year, sep = "")])
  fertiliser_data$frac_leach[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "Fraction leached"), ][paste("y_", year, sep = "")])
  
  fertiliser_data$nh3_input[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "kg NH3 req per tonne fertiliser"), ][paste("y_", year, sep = "")])
  fertiliser_data$nitric_acid_req[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "kg HNO3 req per tonne fertiliser"), ][paste("y_", year, sep = "")])
  fertiliser_data$urea_req[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "kg urea req per tonne fertiliser"), ][paste("y_", year, sep = "")])
  fertiliser_data$ammonium_nitrate_req[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "kg ammonium nitrate req per tonne fertiliser"), ][paste("y_", year, sep = "")])
  fertiliser_data$map_req[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "kg map req per tonne fertiliser"), ][paste("y_", year, sep = "")])
  fertiliser_data$dap_req[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "kg dap req per tonne fertiliser"), ][paste("y_", year, sep = "")])
  
  fertiliser_data$conversion[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "conversion tonnes N to tonnes fertiliser"), ][paste("y_", year, sep = "")])
  
}  

fertiliser_data$frac[is.na(fertiliser_data$frac)] <- 0


fertiliser_energy_emissions <- fertiliser_energy(application_rate = as.numeric(inputs_crop[which(inputs_crop$variable == "Fertiliser Applied to Cropland" & inputs_crop$units == "kg N ha-1 y-1"), ][paste("y_", year, sep = "")]), 
                                                 crop_area = crop_yield$land_area, 
                                                 fertiliser_pct = fertiliser_data$frac, 
                                                 conversion_N_tonnes = fertiliser_data$conversion,
                                                 
                                                 nh3_req = fertiliser_data$nh3_input, 
                                                 nitric_acid_req =  fertiliser_data$nitric_acid_req,
                                                 nitric_acid_nh3_req = as.numeric(inputs_crop[which(inputs_crop$variable == "Ammonia inputs for nitric acid production"), ][paste("y_", year, sep = "")]),
                                                 urea_req = fertiliser_data$urea_req, 
                                                 urea_nh3_req = as.numeric(fertiliser_data[which(fertiliser_data$fertiliser == "Urea"),]$nh3_input),
                                                 
                                                 ammonium_nitrate_req = fertiliser_data$ammonium_nitrate_req,
                                                 ammonium_nitrate_nh3_req = as.numeric(fertiliser_data[which(fertiliser_data$fertiliser == "Ammonium nitrate (AN)"),]$nh3_input),
                                                 ammonium_nitrate_nitric_acid_req = as.numeric(fertiliser_data[which(fertiliser_data$fertiliser == "Ammonium nitrate (AN)"),]$nitric_acid_req),
                                                 
                                                 map_req = fertiliser_data$map_req,
                                                 map_nh3_req = as.numeric(fertiliser_data[which(fertiliser_data$fertiliser == "Monoammonium phosphate (MAP)"),]$nh3_input),
                                                 dap_req = fertiliser_data$dap_req,
                                                 dap_nh3_req = as.numeric(fertiliser_data[which(fertiliser_data$fertiliser == "Diammonium phosphate (DAP)"),]$nh3_input),
                                                 
                                                 
                                                 ammonia_energy_input = as.numeric(inputs_crop[which(inputs_crop$variable == "NH3 prod energy input"), ][paste("y_", year, sep = "")]),
                                                 ammonia_fuel_gas_prop = as.numeric(inputs_crop[which(inputs_crop$variable == "NH3 prod gas prop"), ][paste("y_", year, sep = "")]),
                                                 ammonia_fuel_oil_prop = as.numeric(inputs_crop[which(inputs_crop$variable == "NH3 prod oil prop"), ][paste("y_", year, sep = "")]),
                                                 ammonia_fuel_coal_prop = as.numeric(inputs_crop[which(inputs_crop$variable == "NH3 prod coal prop"), ][paste("y_", year, sep = "")]),
                                                 
                                                 nitric_acid_n2o_ef = as.numeric(inputs_crop[which(inputs_crop$variable == "nitric acid N2O ef"), ][paste("y_", year, sep = "")]),
                                                 
                                                 ef_gas = c(as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas CO2 ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas CO ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas NOx ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas SO2 ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas NMVOC ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas NH3 ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas PM10 ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas PM2.5 ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas BC ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas OC ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas CH4 ef"), ][paste("y_", year, sep = "")])),
                                                 ef_oil = c(as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil CO2 ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil CO ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil NOx ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil SO2 ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil NMVOC ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil NH3 ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil PM10 ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil PM2.5 ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil BC ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil OC ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil CH4 ef"), ][paste("y_", year, sep = "")])),
                                                 ef_coal = c(as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal CO2 ef"), ][paste("y_", year, sep = "")]),
                                                             as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal CO ef"), ][paste("y_", year, sep = "")]),
                                                             as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal NOx ef"), ][paste("y_", year, sep = "")]),
                                                             as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal SO2 ef"), ][paste("y_", year, sep = "")]),
                                                             as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal NMVOC ef"), ][paste("y_", year, sep = "")]),
                                                             as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal NH3 ef"), ][paste("y_", year, sep = "")]),
                                                             as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal PM10 ef"), ][paste("y_", year, sep = "")]),
                                                             as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal PM2.5 ef"), ][paste("y_", year, sep = "")]),
                                                             as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal BC ef"), ][paste("y_", year, sep = "")]),
                                                             as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal OC ef"), ][paste("y_", year, sep = "")]),
                                                             as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal CH4 ef"), ][paste("y_", year, sep = "")])))



##Fertiliser Application
##Ammonia emissions use EMEP/EEA 2019 Tier 2 approach for 
fertiliser_data$nh3_synthetic_fertiliser_kgNH3 <- emis_manure_fertiliser_applied(application_rate = as.numeric(inputs_crop[which(inputs_crop$variable == "Fertiliser Applied to Cropland" & inputs_crop$units == "kg N ha-1 y-1"), ][paste("y_", year, sep = "")]), 
                                                                                 crop_area = crop_yield$land_area, 
                                                                                 fertiliser_pct = fertiliser_data$frac * 100, 
                                                                                 ef = fertiliser_data$nh3_ef)

fertiliser_data$no_synthetic_fertiliser_kgNO2 <- emis_manure_fertiliser_applied(application_rate = as.numeric(inputs_crop[which(inputs_crop$variable == "Fertiliser Applied to Cropland" & inputs_crop$units == "kg N ha-1 y-1"), ][paste("y_", year, sep = "")]), 
                                                                                crop_area = crop_yield$land_area, 
                                                                                fertiliser_pct = fertiliser_data$frac * 100, 
                                                                                ef = fertiliser_data$no_ef) ##EF tier 1 EMEP/EEA 2019 (0.04 kg NO2 kg N-1 applied)

fertiliser_data$n2o_direct_synthetic_fertiliser_kgN2O <- emis_manure_fertiliser_applied(application_rate = as.numeric(inputs_crop[which(inputs_crop$variable == "Fertiliser Applied to Cropland" & inputs_crop$units == "kg N ha-1 y-1"), ][paste("y_", year, sep = "")]), 
                                                                                        crop_area = crop_yield$land_area, 
                                                                                        fertiliser_pct = fertiliser_data$frac * 100, 
                                                                                        ef = (fertiliser_data$n2o_direct_ef)) ##EF tier 1 IPCC 2006 Table 11.1 kg N2O as N kg N-1 applied (conversion to kg N2O applied)

fertiliser_data$n2o_indirect_vol_synthetic_fertiliser_kgN2O <- indirect_n2o_vol_fertiliser(NH3_N_emitted = (fertiliser_data$nh3_synthetic_fertiliser_kgNH3 / 17/14), 
                                                                                           NO_N_emitted = (fertiliser_data$no_synthetic_fertiliser_kgNO2 / 46/14), 
                                                                                           ef = fertiliser_data$n2o_vol_ef)   ##EF tier 1 IPCC 2006 Table 11.3 kg N2O as N kg N-1 applied (conversion to kg N2O applied)


fertiliser_data$n2o_indirect_leach_synthetic_fertiliser_kgN2O <- indirect_n2o_leach_fertiliser(application_rate = as.numeric(inputs_crop[which(inputs_crop$variable == "Fertiliser Applied to Cropland" & inputs_crop$units == "kg N ha-1 y-1"), ][paste("y_", year, sep = "")]), 
                                                                                               crop_area = crop_yield$land_area, 
                                                                                               frac =  fertiliser_data$frac_leach, 
                                                                                               fertiliser_pct = fertiliser_data$frac * 100, 
                                                                                               ef = fertiliser_data$n2o_leach_ef) ##EF tier 1 IPCC 2006 Table 11.3 kg N2O as N kg N-1 applied (conversion to kg N2O applied)


##Emissions manure application
NH3_manure_application_crop_slurry <- emis_manure_fertiliser_applied(application_rate = manure_N_kgN_ha_yr$manure_TAN_application_crops_slurry, 
                                                                     crop_area = crop_yield$land_area, 
                                                                     ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure slurry NH3 ef"), ][paste("y_", year, sep = "")]), 
                                                                     fertiliser_pct = 100)*17/14 

NH3_manure_application_crop_solid <- emis_manure_fertiliser_applied(application_rate = manure_N_kgN_ha_yr$manure_TAN_application_crops_solid, 
                                                                    crop_area = crop_yield$land_area, 
                                                                    ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure solid NH3 ef"), ][paste("y_", year, sep = "")]), 
                                                                    fertiliser_pct = 100)*17/14

NO_manure_application_crop_slurry <- emis_manure_fertiliser_applied(application_rate = manure_N_kgN_ha_yr$manure_N_application_crops_slurry, 
                                                                    crop_area = crop_yield$land_area, 
                                                                    ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure slurry NO ef"), ][paste("y_", year, sep = "")]), 
                                                                    fertiliser_pct = 100) ##EMEP/EEA TIER 1 Category 3D Table 3.1

NO_manure_application_crop_solid <- emis_manure_fertiliser_applied(application_rate = manure_N_kgN_ha_yr$manure_N_application_crops_solid, 
                                                                   crop_area = crop_yield$land_area, 
                                                                   ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure solid NO ef"), ][paste("y_", year, sep = "")]), 
                                                                   fertiliser_pct = 100) ##EMEP/EEA TIER 1 Category 3D Table 3.1

N2O_direct_manure_application_crop_slurry <- emis_manure_fertiliser_applied(application_rate = manure_N_kgN_ha_yr$manure_N_application_crops_slurry, 
                                                                            crop_area = crop_yield$land_area, 
                                                                            ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure slurry N2O direct ef"), ][paste("y_", year, sep = "")]), 
                                                                            fertiliser_pct = 100)*44/28 

N2O_direct_manure_application_crop_solid <- emis_manure_fertiliser_applied(application_rate = manure_N_kgN_ha_yr$manure_N_application_crops_solid, 
                                                                           crop_area = crop_yield$land_area, 
                                                                           ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure solid N2O direct ef"), ][paste("y_", year, sep = "")]), 
                                                                           fertiliser_pct = 100)*44/28

N2O_indirect_vol_manure_application_crop_slurry <- indirect_n2o_vol_fertiliser(NH3_N_emitted = (NH3_manure_application_crop_slurry / (17/14)), 
                                                                               NO_N_emitted = (NO_manure_application_crop_slurry / (46/14)), 
                                                                               ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure slurry N2O vol ef"), ][paste("y_", year, sep = "")])) * 44/28  ##EF tier 1 IPCC 2006 Table 11.3 kg N2O as N kg N-1 applied (conversion to kg N2O applied)

N2O_indirect_vol_manure_application_crop_solid <- indirect_n2o_vol_fertiliser(NH3_N_emitted = (NH3_manure_application_crop_solid / (17/14)), 
                                                                              NO_N_emitted = (NO_manure_application_crop_solid / (46/14)), 
                                                                              ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure solid N2O vol ef"), ][paste("y_", year, sep = "")])) * 44/28  ##EF tier 1 IPCC 2006 Table 11.3 kg N2O as N kg N-1 applied (conversion to kg N2O applied)


N2O_indirect_leach_manure_application_crop_slurry <- indirect_n2o_leach_fertiliser(application_rate = manure_N_kgN_ha_yr$manure_N_application_crops_slurry, 
                                                                                   crop_area = crop_yield$land_area, 
                                                                                   ef = (as.numeric(inputs_crop[which(inputs_crop$variable == "Manure slurry N2O leach ef"), ][paste("y_", year, sep = "")])), 
                                                                                   fertiliser_pct = 100, 
                                                                                   frac = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure slurry frac leach"), ][paste("y_", year, sep = "")])) * 44/28 ##EF tier 1 IPCC 2006 Table 11.3 kg N2O as N kg N-1 applied (conversion to kg N2O applied)

N2O_indirect_leach_manure_application_crop_solid <- indirect_n2o_leach_fertiliser(application_rate = manure_N_kgN_ha_yr$manure_N_application_crops_solid, 
                                                                                  crop_area = crop_yield$land_area, 
                                                                                  ef = (as.numeric(inputs_crop[which(inputs_crop$variable == "Manure solid N2O leach ef"), ][paste("y_", year, sep = "")])), 
                                                                                  fertiliser_pct = 100, 
                                                                                  frac = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure solid frac leach"), ][paste("y_", year, sep = "")])) * 44/28 ##EF tier 1 IPCC 2006 Table 11.3 kg N2O as N kg N-1 applied (conversion to kg N2O applied)



manure_emis <- c(NH3_manure_application_crop_slurry, NH3_manure_application_crop_solid, NO_manure_application_crop_slurry, NO_manure_application_crop_solid, N2O_direct_manure_application_crop_slurry, N2O_direct_manure_application_crop_solid, N2O_indirect_vol_manure_application_crop_slurry, N2O_indirect_vol_manure_application_crop_solid, N2O_indirect_leach_manure_application_crop_slurry, N2O_indirect_leach_manure_application_crop_solid)
names(manure_emis) <- c("NH3_manure_application_crop_slurry", "NH3_manure_application_crop_solid", "NO_manure_application_crop_slurry", "NO_manure_application_crop_solid", "N2O_direct_manure_application_crop_slurry", "N2O_direct_manure_application_crop_solid", "N2O_indirect_vol_manure_application_crop_slurry", "N2O_indirect_vol_manure_application_crop_solid", "N2O_indirect_leach_manure_application_crop_slurry", "N2O_indirect_leach_manure_application_crop_solid")


my_list <- list("domestic_production" = domestic_production, 
                "crop_yield" = crop_yield,
                "fertiliser_data" = fertiliser_data, 
                "manure_emis" = manure_emis,
                #"machinery_emissions " = machinery_emissions,
                "fertiliser_energy_emissions" = fertiliser_energy_emissions)

assign(paste("outputs_", crop_name, sep = ""), my_list)

count <- i


}








##Pastures (inc. grazing and cultivated grasslands)
pasture_feed <- c("GRASS", "GRASSH", "GRASSLEGF", "GRASSLEGH", "ALFALFAH", "GRASSF") 

pasture_feed_dm <- feed_requirements_summary[c("GRASS", "GRASSH", "GRASSLEGF", "GRASSLEGH", "ALFALFAH", "GRASSF")]
pasture_feed_n <- c(0,0,0,0,0, 0)
names(pasture_feed_n) <- c("GRASS", "GRASSH", "GRASSLEGF", "GRASSLEGH", "ALFALFAH", "GRASSF")

##Calculate N outputs for feed
for(i in seq(from=1, to=length(pasture_feed), by=1)) {
  
  pasture_feed_n[i] <- (sum(pasture_feed_dm[names(pasture_feed_n[i])], na.rm = TRUE) * ##tonnes DM
                          as.numeric(inputs_feedcrop[which(inputs_feedcrop$crop == pasture_feed[i] & inputs_feedcrop$variable == "N pct in crop"), ][paste("y_", year, sep = "")])) / 1000 ##g N kg DM-1 convert to kg N
  
}

fixed_N_kgN_ha_y <- as.numeric(inputs_feedcrop[which(inputs_feedcrop$crop == "Grassland" & inputs_feedcrop$variable == "Fixed N"), ][paste("y_", year, sep = "")]) ##kg N ha y
N_dep <- as.numeric(inputs_feedcrop[which(inputs_feedcrop$crop == "Grassland" & inputs_feedcrop$variable == "N deposition"), ][paste("y_", year, sep = "")])

synthetic_fertiliser_kgN_ha_yr <- as.numeric(inputs_crop[which(inputs_crop$variable == "Fertiliser Applied to Grassland" & inputs_crop$units == "kg N ha-1 y-1"), ][paste("y_", year, sep = "")])

N_inputs_grasslands <- fixed_N_kgN_ha_y + N_dep + as.numeric(manure_N_kgN_ha_yr$manure_N_application_grasslands_slurry) + as.numeric(manure_N_kgN_ha_yr$manure_N_application_grasslands_solid) + as.numeric(manure_N_kgN_ha_yr$manure_N_application_grasslands_graze) + synthetic_fertiliser_kgN_ha_yr ##kg N ha-1 y-1

N_outputs_grasslands <- sum(pasture_feed_n) / as.numeric(inputs_crop[which(inputs_crop$variable == "Grassland area"), ][paste("y_", year, sep = "")]) #kg N ha-1 y-1

N_surplus <-  N_inputs_grasslands - N_outputs_grasslands



##Fertiliser Application
##Ammonia emissions use EMEP/EEA 2019 Tier 2 approach
fertiliser_data <- data.frame(fertiliser = inputs_crop[ which(inputs_crop$units == "fraction total Tonnes N"), ]$variable)
fertiliser_data$frac <- NA
fertiliser_data$nh3_ef <- NA
fertiliser_data$no_ef <- NA
fertiliser_data$n2o_direct_ef <- NA
fertiliser_data$n2o_vol_ef <- NA
fertiliser_data$n2o_leach_ef <- NA
fertiliser_data$frac_leach <- NA

fertiliser_data$nh3_input <- NA
fertiliser_data$nitric_acid_req <- NA
fertiliser_data$urea_req <- NA
fertiliser_data$ammonium_nitrate_req <- NA
fertiliser_data$map_req <- NA
fertiliser_data$dap_req <- NA

fertiliser_data$conversion <- NA ##convert tonnes N to tonnes fertiliser

for(p in seq(from=1, to=length(fertiliser_data$fertiliser), by=1)) {
  
  fertiliser_data$frac[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "fraction total Tonnes N"), ][paste("y_", year, sep = "")])
  fertiliser_data$nh3_ef[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "NH3 EF kg NH3 kg N-1"), ][paste("y_", year, sep = "")])
  fertiliser_data$no_ef[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "NO EF kg NO2 kg N-1"), ][paste("y_", year, sep = "")])
  fertiliser_data$n2o_direct_ef[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "N2O direct EF kg N2O kg N-1"), ][paste("y_", year, sep = "")])
  fertiliser_data$n2o_vol_ef[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "N2O indirect vol EF kg N2O kg N-1"), ][paste("y_", year, sep = "")])
  fertiliser_data$n2o_leach_ef[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "N2O indirect leach EF kg N2O kg N-1"), ][paste("y_", year, sep = "")])
  fertiliser_data$frac_leach[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "Fraction leached"), ][paste("y_", year, sep = "")])
  
  fertiliser_data$nh3_input[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "kg NH3 req per tonne fertiliser"), ][paste("y_", year, sep = "")])
  fertiliser_data$nitric_acid_req[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "kg HNO3 req per tonne fertiliser"), ][paste("y_", year, sep = "")])
  fertiliser_data$urea_req[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "kg urea req per tonne fertiliser"), ][paste("y_", year, sep = "")])
  fertiliser_data$ammonium_nitrate_req[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "kg ammonium nitrate req per tonne fertiliser"), ][paste("y_", year, sep = "")])
  fertiliser_data$map_req[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "kg map req per tonne fertiliser"), ][paste("y_", year, sep = "")])
  fertiliser_data$dap_req[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "kg dap req per tonne fertiliser"), ][paste("y_", year, sep = "")])
  
  fertiliser_data$conversion[p] <- as.numeric(inputs_crop[ which(inputs_crop$variable == fertiliser_data$fertiliser[p] & inputs_crop$units == "conversion tonnes N to tonnes fertiliser"), ][paste("y_", year, sep = "")])
  
}  

fertiliser_data$frac[is.na(fertiliser_data$frac)] <- 0


fertiliser_energy_emissions <- fertiliser_energy(application_rate = synthetic_fertiliser_kgN_ha_yr, 
                                                 crop_area = as.numeric(inputs_crop[which(inputs_crop$variable == "Grassland area"), ][paste("y_", year, sep = "")]), 
                                                 fertiliser_pct = fertiliser_data$frac, 
                                                 conversion_N_tonnes = fertiliser_data$conversion,
                                                 
                                                 nh3_req = fertiliser_data$nh3_input, 
                                                 nitric_acid_req =  fertiliser_data$nitric_acid_req,
                                                 nitric_acid_nh3_req = as.numeric(inputs_crop[which(inputs_crop$variable == "Ammonia inputs for nitric acid production"), ][paste("y_", year, sep = "")]),
                                                 urea_req = fertiliser_data$urea_req, 
                                                 urea_nh3_req = as.numeric(fertiliser_data[which(fertiliser_data$fertiliser == "Urea"),]$nh3_input),
                                                 
                                                 ammonium_nitrate_req = fertiliser_data$ammonium_nitrate_req,
                                                 ammonium_nitrate_nh3_req = as.numeric(fertiliser_data[which(fertiliser_data$fertiliser == "Ammonium nitrate (AN)"),]$nh3_input),
                                                 ammonium_nitrate_nitric_acid_req = as.numeric(fertiliser_data[which(fertiliser_data$fertiliser == "Ammonium nitrate (AN)"),]$nitric_acid_req),
                                                 
                                                 map_req = fertiliser_data$map_req,
                                                 map_nh3_req = as.numeric(fertiliser_data[which(fertiliser_data$fertiliser == "Monoammonium phosphate (MAP)"),]$nh3_input),
                                                 dap_req = fertiliser_data$dap_req,
                                                 dap_nh3_req = as.numeric(fertiliser_data[which(fertiliser_data$fertiliser == "Diammonium phosphate (DAP)"),]$nh3_input),
                                                 
                                                 
                                                 ammonia_energy_input = as.numeric(inputs_crop[which(inputs_crop$variable == "NH3 prod energy input"), ][paste("y_", year, sep = "")]),
                                                 ammonia_fuel_gas_prop = as.numeric(inputs_crop[which(inputs_crop$variable == "NH3 prod gas prop"), ][paste("y_", year, sep = "")]),
                                                 ammonia_fuel_oil_prop = as.numeric(inputs_crop[which(inputs_crop$variable == "NH3 prod oil prop"), ][paste("y_", year, sep = "")]),
                                                 ammonia_fuel_coal_prop = as.numeric(inputs_crop[which(inputs_crop$variable == "NH3 prod coal prop"), ][paste("y_", year, sep = "")]),
                                                 
                                                 nitric_acid_n2o_ef = as.numeric(inputs_crop[which(inputs_crop$variable == "nitric acid N2O ef"), ][paste("y_", year, sep = "")]),
                                                 
                                                 ef_gas = c(as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas CO2 ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas CO ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas NOx ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas SO2 ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas NMVOC ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas NH3 ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas PM10 ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas PM2.5 ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas BC ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas OC ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia gas CH4 ef"), ][paste("y_", year, sep = "")])),
                                                 ef_oil = c(as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil CO2 ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil CO ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil NOx ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil SO2 ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil NMVOC ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil NH3 ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil PM10 ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil PM2.5 ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil BC ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil OC ef"), ][paste("y_", year, sep = "")]),
                                                            as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia oil CH4 ef"), ][paste("y_", year, sep = "")])),
                                                 ef_coal = c(as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal CO2 ef"), ][paste("y_", year, sep = "")]),
                                                             as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal CO ef"), ][paste("y_", year, sep = "")]),
                                                             as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal NOx ef"), ][paste("y_", year, sep = "")]),
                                                             as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal SO2 ef"), ][paste("y_", year, sep = "")]),
                                                             as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal NMVOC ef"), ][paste("y_", year, sep = "")]),
                                                             as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal NH3 ef"), ][paste("y_", year, sep = "")]),
                                                             as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal PM10 ef"), ][paste("y_", year, sep = "")]),
                                                             as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal PM2.5 ef"), ][paste("y_", year, sep = "")]),
                                                             as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal BC ef"), ][paste("y_", year, sep = "")]),
                                                             as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal OC ef"), ][paste("y_", year, sep = "")]),
                                                             as.numeric(inputs_crop[which(inputs_crop$variable == "ammonia coal CH4 ef"), ][paste("y_", year, sep = "")])))


##Fertiliser Application
##Ammonia emissions use EMEP/EEA 2019 Tier 2 approach for 
fertiliser_data$nh3_synthetic_fertiliser_kgNH3 <- emis_manure_fertiliser_applied(application_rate = synthetic_fertiliser_kgN_ha_yr, 
                                                                                 crop_area = as.numeric(inputs_crop[which(inputs_crop$variable == "Grassland area"), ][paste("y_", year, sep = "")]), 
                                                                                 fertiliser_pct = fertiliser_data$frac * 100, 
                                                                                 ef = fertiliser_data$nh3_ef)

fertiliser_data$no_synthetic_fertiliser_kgNO2 <- emis_manure_fertiliser_applied(application_rate = synthetic_fertiliser_kgN_ha_yr, 
                                                                                crop_area = as.numeric(inputs_crop[which(inputs_crop$variable == "Grassland area"), ][paste("y_", year, sep = "")]), 
                                                                                fertiliser_pct = fertiliser_data$frac * 100, 
                                                                                ef = fertiliser_data$no_ef) ##EF tier 1 EMEP/EEA 2019 (0.04 kg NO2 kg N-1 applied)

fertiliser_data$n2o_direct_synthetic_fertiliser_kgN2O <- emis_manure_fertiliser_applied(application_rate = synthetic_fertiliser_kgN_ha_yr, 
                                                                                        crop_area = as.numeric(inputs_crop[which(inputs_crop$variable == "Grassland area"), ][paste("y_", year, sep = "")]), 
                                                                                        fertiliser_pct = fertiliser_data$frac * 100, 
                                                                                        ef = (fertiliser_data$n2o_direct_ef)) ##EF tier 1 IPCC 2006 Table 11.1 kg N2O as N kg N-1 applied (conversion to kg N2O applied)

fertiliser_data$n2o_indirect_vol_synthetic_fertiliser_kgN2O <- indirect_n2o_vol_fertiliser(NH3_N_emitted = (fertiliser_data$nh3_synthetic_fertiliser_kgNH3 / 17/14), 
                                                                                           NO_N_emitted = (fertiliser_data$no_synthetic_fertiliser_kgNO2 / 46/14), 
                                                                                           ef = fertiliser_data$n2o_vol_ef)   ##EF tier 1 IPCC 2006 Table 11.3 kg N2O as N kg N-1 applied (conversion to kg N2O applied)


fertiliser_data$n2o_indirect_leach_synthetic_fertiliser_kgN2O <- indirect_n2o_leach_fertiliser(application_rate = synthetic_fertiliser_kgN_ha_yr, 
                                                                                               crop_area = as.numeric(inputs_crop[which(inputs_crop$variable == "Grassland area"), ][paste("y_", year, sep = "")]), 
                                                                                               frac =  fertiliser_data$frac_leach, 
                                                                                               fertiliser_pct = fertiliser_data$frac * 100, 
                                                                                               ef = fertiliser_data$n2o_leach_ef) ##EF tier 1 IPCC 2006 Table 11.3 kg N2O as N kg N-1 applied (conversion to kg N2O applied)


##Emissions manure application
NH3_manure_application_crop_slurry <- emis_manure_fertiliser_applied(application_rate = manure_N_kgN_ha_yr$manure_TAN_application_grasslands_slurry, 
                                                                     crop_area = as.numeric(inputs_crop[which(inputs_crop$variable == "Grassland area"), ][paste("y_", year, sep = "")]), 
                                                                     ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure slurry NH3 ef"), ][paste("y_", year, sep = "")]), 
                                                                     fertiliser_pct = 100)*17/14 

NH3_manure_application_crop_solid <- emis_manure_fertiliser_applied(application_rate = manure_N_kgN_ha_yr$manure_TAN_application_grasslands_solid, 
                                                                    crop_area = as.numeric(inputs_crop[which(inputs_crop$variable == "Grassland area"), ][paste("y_", year, sep = "")]), 
                                                                    ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure solid NH3 ef"), ][paste("y_", year, sep = "")]), 
                                                                    fertiliser_pct = 100)*17/14

NH3_manure_application_crop_graze <- emis_manure_fertiliser_applied(application_rate = manure_N_kgN_ha_yr$manure_TAN_application_grasslands_graze, 
                                                                    crop_area = as.numeric(inputs_crop[which(inputs_crop$variable == "Grassland area"), ][paste("y_", year, sep = "")]), 
                                                                    ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure graze NH3 ef"), ][paste("y_", year, sep = "")]), 
                                                                    fertiliser_pct = 100)*17/14



NO_manure_application_crop_slurry <- emis_manure_fertiliser_applied(application_rate = manure_N_kgN_ha_yr$manure_N_application_grasslands_slurry, 
                                                                    crop_area = as.numeric(inputs_crop[which(inputs_crop$variable == "Grassland area"), ][paste("y_", year, sep = "")]), 
                                                                    ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure slurry NO ef"), ][paste("y_", year, sep = "")]), 
                                                                    fertiliser_pct = 100) ##EMEP/EEA TIER 1 Category 3D Table 3.1

NO_manure_application_crop_solid <- emis_manure_fertiliser_applied(application_rate = manure_N_kgN_ha_yr$manure_N_application_grasslands_solid, 
                                                                   crop_area =as.numeric(inputs_crop[which(inputs_crop$variable == "Grassland area"), ][paste("y_", year, sep = "")]), 
                                                                   ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure solid NO ef"), ][paste("y_", year, sep = "")]), 
                                                                   fertiliser_pct = 100) ##EMEP/EEA TIER 1 Category 3D Table 3.1

NO_manure_application_crop_graze <- emis_manure_fertiliser_applied(application_rate = manure_N_kgN_ha_yr$manure_N_application_grasslands_graze, 
                                                                   crop_area =as.numeric(inputs_crop[which(inputs_crop$variable == "Grassland area"), ][paste("y_", year, sep = "")]), 
                                                                   ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure graze NO ef"), ][paste("y_", year, sep = "")]), 
                                                                   fertiliser_pct = 100) ##EMEP/EEA TIER 1 Category 3D Table 3.1




N2O_direct_manure_application_crop_slurry <- emis_manure_fertiliser_applied(application_rate = manure_N_kgN_ha_yr$manure_N_application_grasslands_slurry, 
                                                                            crop_area = as.numeric(inputs_crop[which(inputs_crop$variable == "Grassland area"), ][paste("y_", year, sep = "")]), 
                                                                            ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure slurry N2O direct ef"), ][paste("y_", year, sep = "")]), 
                                                                            fertiliser_pct = 100)*44/28 

N2O_direct_manure_application_crop_solid <- emis_manure_fertiliser_applied(application_rate = manure_N_kgN_ha_yr$manure_N_application_grasslands_solid, 
                                                                           crop_area = as.numeric(inputs_crop[which(inputs_crop$variable == "Grassland area"), ][paste("y_", year, sep = "")]), 
                                                                           ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure solid N2O direct ef"), ][paste("y_", year, sep = "")]), 
                                                                           fertiliser_pct = 100)*44/28

N2O_direct_manure_application_crop_graze <- emis_manure_fertiliser_applied(application_rate = manure_N_kgN_ha_yr$manure_N_application_grasslands_graze, 
                                                                           crop_area = as.numeric(inputs_crop[which(inputs_crop$variable == "Grassland area"), ][paste("y_", year, sep = "")]), 
                                                                           ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure graze N2O direct ef"), ][paste("y_", year, sep = "")]), 
                                                                           fertiliser_pct = 100)*44/28



N2O_indirect_vol_manure_application_crop_slurry <- indirect_n2o_vol_fertiliser(NH3_N_emitted = (NH3_manure_application_crop_slurry / (17/14)), 
                                                                               NO_N_emitted = (NO_manure_application_crop_slurry / (46/14)), 
                                                                               ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure slurry N2O vol ef"), ][paste("y_", year, sep = "")])) * 44/28  ##EF tier 1 IPCC 2006 Table 11.3 kg N2O as N kg N-1 applied (conversion to kg N2O applied)

N2O_indirect_vol_manure_application_crop_solid <- indirect_n2o_vol_fertiliser(NH3_N_emitted = (NH3_manure_application_crop_solid / (17/14)), 
                                                                              NO_N_emitted = (NO_manure_application_crop_solid / (46/14)), 
                                                                              ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure solid N2O vol ef"), ][paste("y_", year, sep = "")])) * 44/28  ##EF tier 1 IPCC 2006 Table 11.3 kg N2O as N kg N-1 applied (conversion to kg N2O applied)

N2O_indirect_vol_manure_application_crop_graze <- indirect_n2o_vol_fertiliser(NH3_N_emitted = (NH3_manure_application_crop_graze / (17/14)), 
                                                                              NO_N_emitted = (NO_manure_application_crop_graze / (46/14)), 
                                                                              ef = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure graze N2O vol ef"), ][paste("y_", year, sep = "")])) * 44/28  ##EF tier 1 IPCC 2006 Table 11.3 kg N2O as N kg N-1 applied (conversion to kg N2O applied)


N2O_indirect_leach_manure_application_crop_slurry <- indirect_n2o_leach_fertiliser(application_rate = manure_N_kgN_ha_yr$manure_N_application_grasslands_slurry, 
                                                                                   crop_area = as.numeric(inputs_crop[which(inputs_crop$variable == "Grassland area"), ][paste("y_", year, sep = "")]), 
                                                                                   ef = (as.numeric(inputs_crop[which(inputs_crop$variable == "Manure slurry N2O leach ef"), ][paste("y_", year, sep = "")])), 
                                                                                   fertiliser_pct = 100, 
                                                                                   frac = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure slurry frac leach"), ][paste("y_", year, sep = "")])) * 44/28 ##EF tier 1 IPCC 2006 Table 11.3 kg N2O as N kg N-1 applied (conversion to kg N2O applied)

N2O_indirect_leach_manure_application_crop_solid <- indirect_n2o_leach_fertiliser(application_rate = manure_N_kgN_ha_yr$manure_N_application_grasslands_solid, 
                                                                                  crop_area = as.numeric(inputs_crop[which(inputs_crop$variable == "Grassland area"), ][paste("y_", year, sep = "")]), 
                                                                                  ef = (as.numeric(inputs_crop[which(inputs_crop$variable == "Manure solid N2O leach ef"), ][paste("y_", year, sep = "")])), 
                                                                                  fertiliser_pct = 100, 
                                                                                  frac = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure solid frac leach"), ][paste("y_", year, sep = "")])) * 44/28 ##EF tier 1 IPCC 2006 Table 11.3 kg N2O as N kg N-1 applied (conversion to kg N2O applied)

N2O_indirect_leach_manure_application_crop_graze <- indirect_n2o_leach_fertiliser(application_rate = manure_N_kgN_ha_yr$manure_N_application_grasslands_graze, 
                                                                                  crop_area = as.numeric(inputs_crop[which(inputs_crop$variable == "Grassland area"), ][paste("y_", year, sep = "")]), 
                                                                                  ef = (as.numeric(inputs_crop[which(inputs_crop$variable == "Manure graze N2O leach ef"), ][paste("y_", year, sep = "")])), 
                                                                                  fertiliser_pct = 100, 
                                                                                  frac = as.numeric(inputs_crop[which(inputs_crop$variable == "Manure graze frac leach"), ][paste("y_", year, sep = "")])) * 44/28 ##EF tier 1 IPCC 2006 Table 11.3 kg N2O as N kg N-1 applied (conversion to kg N2O applied)



manure_emis <- c(NH3_manure_application_crop_slurry, NH3_manure_application_crop_solid, NH3_manure_application_crop_graze,
                 NO_manure_application_crop_slurry, NO_manure_application_crop_solid,  NO_manure_application_crop_graze,
                 N2O_direct_manure_application_crop_slurry, N2O_direct_manure_application_crop_solid, N2O_direct_manure_application_crop_graze, 
                 N2O_indirect_vol_manure_application_crop_slurry, N2O_indirect_vol_manure_application_crop_solid, N2O_indirect_vol_manure_application_crop_graze, 
                 N2O_indirect_leach_manure_application_crop_slurry, N2O_indirect_leach_manure_application_crop_solid, N2O_indirect_leach_manure_application_crop_graze)
names(manure_emis) <- c("NH3_manure_application_crop_slurry", "NH3_manure_application_crop_solid", "NH3_manure_application_crop_graze",
                        "NO_manure_application_crop_slurry", "NO_manure_application_crop_solid",  "NO_manure_application_crop_graze",
                        "N2O_direct_manure_application_crop_slurry", "N2O_direct_manure_application_crop_solid", "N2O_direct_manure_application_crop_graze", 
                        "N2O_indirect_vol_manure_application_crop_slurry", "N2O_indirect_vol_manure_application_crop_solid", "N2O_indirect_vol_manure_application_crop_graze", 
                        "N2O_indirect_leach_manure_application_crop_slurry", "N2O_indirect_leach_manure_application_crop_solid", "N2O_indirect_leach_manure_application_crop_graze")


my_list <- list("pasture_feed_n" = pasture_feed_n, 
                "N_inputs_grasslands" = N_inputs_grasslands,
                "N_outputs_grasslands" = N_outputs_grasslands, 
                "N_surplus" = N_surplus,
                "fertiliser_data" = fertiliser_data,
                "manure_emis" = manure_emis,
                "fertiliser_energy_emissions" = fertiliser_energy_emissions)

assign(paste("outputs_grassland", sep = ""), my_list)









##Collate outputs
emissions <- data.frame(country = numeric(0), year = numeric(0), sector = numeric(0), subsector = numeric(0), species = numeric(0), mgmt = numeric(0), animal = numeric(0), pollutant = numeric(0), units = numeric(0), value = numeric(0))

for(count in seq(from=1, to=length(species_lists$species), by=1)) {
  for(i in seq(from=1, to=length(get(paste("mgmt_", species_lists$species[count], sep = ""))), by=1)) {
    for(j in seq(from=1, to=length(get(paste("animal_", species_lists$species[count], sep = ""))), by=1)) {
      
emissions[(length(emissions$country)+1),]$sector <- "livestock"
emissions[(length(emissions$country)),]$subsector <- "enteric fermentation"
emissions[(length(emissions$country)),]$species <- species_lists$species[count]
emissions[(length(emissions$country)),]$mgmt <- get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
emissions[(length(emissions$country)),]$animal <- get(paste("animal_", species_lists$species[count], sep = ""))[j]
emissions[(length(emissions$country)),]$pollutant <- "CH4"
emissions[(length(emissions$country)),]$units <- "kg y-1"
emissions[(length(emissions$country)),]$value <- sum(enteric_methane_summary[ which(enteric_methane_summary$species == species_lists$species[count] & 
                                                                                  enteric_methane_summary$mgmt == get(paste("mgmt_", species_lists$species[count], sep = ""))[i] &
                                                                                    enteric_methane_summary$animal == get(paste("animal_", species_lists$species[count], sep = ""))[j]), ]$ch4_emis_kg, na.rm = TRUE)

}}}


for(count in seq(from=1, to=length(species_lists$species), by=1)) {
  for(i in seq(from=1, to=length(get(paste("mgmt_", species_lists$species[count], sep = ""))), by=1)) {
    for(j in seq(from=1, to=length(get(paste("animal_", species_lists$species[count], sep = ""))), by=1)) {
      
      emissions[(length(emissions$country)+1),]$sector <- "livestock"
      emissions[(length(emissions$country)),]$subsector <- "manure management"
      emissions[(length(emissions$country)),]$species <- species_lists$species[count]
      emissions[(length(emissions$country)),]$mgmt <- get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
      emissions[(length(emissions$country)),]$animal <- get(paste("animal_", species_lists$species[count], sep = ""))[j]
      emissions[(length(emissions$country)),]$pollutant <- "CH4"
      emissions[(length(emissions$country)),]$units <- "kg y-1"
      emissions[(length(emissions$country)),]$value <- sum(manure_methane_summary[ which(manure_methane_summary$species == species_lists$species[count] & 
                                                                                            manure_methane_summary$mgmt == get(paste("mgmt_", species_lists$species[count], sep = ""))[i] &
                                                                                            manure_methane_summary$animal == get(paste("animal_", species_lists$species[count], sep = ""))[j]), ]$ch4_emis_kg, na.rm = TRUE)
      
    }}}


for(count in seq(from=1, to=length(species_lists$species), by=1)) {
  for(i in seq(from=1, to=length(get(paste("mgmt_", species_lists$species[count], sep = ""))), by=1)) {
    for(j in seq(from=1, to=length(get(paste("animal_", species_lists$species[count], sep = ""))), by=1)) {
      
      emissions[(length(emissions$country)+1),]$sector <- "livestock"
      emissions[(length(emissions$country)),]$subsector <- "manure management"
      emissions[(length(emissions$country)),]$species <- species_lists$species[count]
      emissions[(length(emissions$country)),]$mgmt <- get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
      emissions[(length(emissions$country)),]$animal <- get(paste("animal_", species_lists$species[count], sep = ""))[j]
      emissions[(length(emissions$country)),]$pollutant <- "N2O"
      emissions[(length(emissions$country)),]$units <- "kg y-1"
      emissions[(length(emissions$country)),]$value <- sum(manure_n2o_direct_slurry_summary[ which(manure_n2o_direct_slurry_summary$species == species_lists$species[count] & 
                                                                                                     manure_n2o_direct_slurry_summary$mgmt == get(paste("mgmt_", species_lists$species[count], sep = ""))[i] &
                                                                                                     manure_n2o_direct_slurry_summary$animal == get(paste("animal_", species_lists$species[count], sep = ""))[j]), ]$n2o_emis_kg, na.rm = TRUE) +
        sum(manure_n2o_vol_slurry_summary[ which(manure_n2o_vol_slurry_summary$species == species_lists$species[count] & 
                                                      manure_n2o_vol_slurry_summary$mgmt == get(paste("mgmt_", species_lists$species[count], sep = ""))[i] &
                                                      manure_n2o_vol_slurry_summary$animal == get(paste("animal_", species_lists$species[count], sep = ""))[j]), ]$n2o_emis_kg, na.rm = TRUE) +
        sum(manure_n2o_leach_slurry_summary[ which(manure_n2o_leach_slurry_summary$species == species_lists$species[count] & 
                                                      manure_n2o_leach_slurry_summary$mgmt == get(paste("mgmt_", species_lists$species[count], sep = ""))[i] &
                                                      manure_n2o_leach_slurry_summary$animal == get(paste("animal_", species_lists$species[count], sep = ""))[j]), ]$n2o_emis_kg, na.rm = TRUE) +
        sum(manure_n2o_direct_solid_summary[ which(manure_n2o_direct_solid_summary$species == species_lists$species[count] & 
                                                      manure_n2o_direct_solid_summary$mgmt == get(paste("mgmt_", species_lists$species[count], sep = ""))[i] &
                                                      manure_n2o_direct_solid_summary$animal == get(paste("animal_", species_lists$species[count], sep = ""))[j]), ]$n2o_emis_kg, na.rm = TRUE) +
        sum(manure_n2o_vol_solid_summary[ which(manure_n2o_vol_solid_summary$species == species_lists$species[count] & 
                                                   manure_n2o_vol_solid_summary$mgmt == get(paste("mgmt_", species_lists$species[count], sep = ""))[i] &
                                                   manure_n2o_vol_solid_summary$animal == get(paste("animal_", species_lists$species[count], sep = ""))[j]), ]$n2o_emis_kg, na.rm = TRUE) +
        sum(manure_n2o_leach_solid_summary[ which(manure_n2o_leach_solid_summary$species == species_lists$species[count] & 
                                                     manure_n2o_leach_solid_summary$mgmt == get(paste("mgmt_", species_lists$species[count], sep = ""))[i] &
                                                     manure_n2o_leach_solid_summary$animal == get(paste("animal_", species_lists$species[count], sep = ""))[j]), ]$n2o_emis_kg, na.rm = TRUE)
      
    }}}


for(count in seq(from=1, to=length(species_lists$species), by=1)) {
  for(i in seq(from=1, to=length(get(paste("mgmt_", species_lists$species[count], sep = ""))), by=1)) {
    for(j in seq(from=1, to=length(get(paste("animal_", species_lists$species[count], sep = ""))), by=1)) {
      
      emissions[(length(emissions$country)+1),]$sector <- "livestock"
      emissions[(length(emissions$country)),]$subsector <- "manure management"
      emissions[(length(emissions$country)),]$species <- species_lists$species[count]
      emissions[(length(emissions$country)),]$mgmt <- get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
      emissions[(length(emissions$country)),]$animal <- get(paste("animal_", species_lists$species[count], sep = ""))[j]
      emissions[(length(emissions$country)),]$pollutant <- "NH3"
      emissions[(length(emissions$country)),]$units <- "kg y-1"
      emissions[(length(emissions$country)),]$value <- sum(manure_nh3_emis_house_slurry_summary[ which(manure_nh3_emis_house_slurry_summary$species == species_lists$species[count] & 
                                                                                                     manure_nh3_emis_house_slurry_summary$mgmt == get(paste("mgmt_", species_lists$species[count], sep = ""))[i] &
                                                                                                     manure_nh3_emis_house_slurry_summary$animal == get(paste("animal_", species_lists$species[count], sep = ""))[j]), ]$nh3_emis_kg, na.rm = TRUE) +
        sum(manure_nh3_emis_storage_slurry_summary[ which(manure_nh3_emis_storage_slurry_summary$species == species_lists$species[count] & 
                                                   manure_nh3_emis_storage_slurry_summary$mgmt == get(paste("mgmt_", species_lists$species[count], sep = ""))[i] &
                                                   manure_nh3_emis_storage_slurry_summary$animal == get(paste("animal_", species_lists$species[count], sep = ""))[j]), ]$nh3_emis_kg, na.rm = TRUE) +
        sum(manure_nh3_emis_yard_summary[ which(manure_nh3_emis_yard_summary$species == species_lists$species[count] & 
                                                     manure_nh3_emis_yard_summary$mgmt == get(paste("mgmt_", species_lists$species[count], sep = ""))[i] &
                                                     manure_nh3_emis_yard_summary$animal == get(paste("animal_", species_lists$species[count], sep = ""))[j]), ]$nh3_emis_kg, na.rm = TRUE) +
        sum(manure_nh3_emis_house_solid_summary[ which(manure_nh3_emis_house_solid_summary$species == species_lists$species[count] & 
                                                     manure_nh3_emis_house_solid_summary$mgmt == get(paste("mgmt_", species_lists$species[count], sep = ""))[i] &
                                                     manure_nh3_emis_house_solid_summary$animal == get(paste("animal_", species_lists$species[count], sep = ""))[j]), ]$nh3_emis_kg, na.rm = TRUE) +
        sum(manure_nh3_emis_storage_solid_summary[ which(manure_nh3_emis_storage_solid_summary$species == species_lists$species[count] & 
                                                  manure_nh3_emis_storage_solid_summary$mgmt == get(paste("mgmt_", species_lists$species[count], sep = ""))[i] &
                                                  manure_nh3_emis_storage_solid_summary$animal == get(paste("animal_", species_lists$species[count], sep = ""))[j]), ]$nh3_emis_kg, na.rm = TRUE)
      
    }}}


for(count in seq(from=1, to=length(species_lists$species), by=1)) {
  for(i in seq(from=1, to=length(get(paste("mgmt_", species_lists$species[count], sep = ""))), by=1)) {
    for(j in seq(from=1, to=length(get(paste("animal_", species_lists$species[count], sep = ""))), by=1)) {
      
      emissions[(length(emissions$country)+1),]$sector <- "livestock"
      emissions[(length(emissions$country)),]$subsector <- "manure management"
      emissions[(length(emissions$country)),]$species <- species_lists$species[count]
      emissions[(length(emissions$country)),]$mgmt <- get(paste("mgmt_", species_lists$species[count], sep = ""))[i]
      emissions[(length(emissions$country)),]$animal <- get(paste("animal_", species_lists$species[count], sep = ""))[j]
      emissions[(length(emissions$country)),]$pollutant <- "NO"
      emissions[(length(emissions$country)),]$units <- "kg y-1"
      emissions[(length(emissions$country)),]$value <- sum(manure_no_emis_storage_slurry_summary[ which(manure_no_emis_storage_slurry_summary$species == species_lists$species[count] & 
                                                            manure_no_emis_storage_slurry_summary$mgmt == get(paste("mgmt_", species_lists$species[count], sep = ""))[i] &
                                                            manure_no_emis_storage_slurry_summary$animal == get(paste("animal_", species_lists$species[count], sep = ""))[j]), ]$no_emis_kg, na.rm = TRUE) +
       sum(manure_no_emis_storage_solid_summary[ which(manure_no_emis_storage_solid_summary$species == species_lists$species[count] & 
                                                           manure_no_emis_storage_solid_summary$mgmt == get(paste("mgmt_", species_lists$species[count], sep = ""))[i] &
                                                           manure_no_emis_storage_solid_summary$animal == get(paste("animal_", species_lists$species[count], sep = ""))[j]), ]$no_emis_kg, na.rm = TRUE)
      
    }}}






for(i in seq(from=1, to=length(crop_names), by=1)) {
      
      emissions[(length(emissions$country)+1),]$sector <- "crops"
      emissions[(length(emissions$country)),]$subsector <- "manure application"
      emissions[(length(emissions$country)),]$species <- crop_names[i]
      emissions[(length(emissions$country)),]$pollutant <- "NH3"
      emissions[(length(emissions$country)),]$units <- "kg y-1"
      emissions[(length(emissions$country)),]$value <- as.numeric(get(paste("outputs_", crop_names[i], sep = ""))$manure_emis["NH3_manure_application_crop_slurry"] + 
                                                                get(paste("outputs_", crop_names[i], sep = ""))$manure_emis["NH3_manure_application_crop_solid"])
    }


for(i in seq(from=1, to=length(crop_names), by=1)) {
  
  emissions[(length(emissions$country)+1),]$sector <- "crops"
  emissions[(length(emissions$country)),]$subsector <- "manure application"
  emissions[(length(emissions$country)),]$species <- crop_names[i]
  emissions[(length(emissions$country)),]$pollutant <- "NO"
  emissions[(length(emissions$country)),]$units <- "kg y-1"
  emissions[(length(emissions$country)),]$value <- as.numeric(get(paste("outputs_", crop_names[i], sep = ""))$manure_emis["NO_manure_application_crop_slurry"] + 
                                                                get(paste("outputs_", crop_names[i], sep = ""))$manure_emis["NO_manure_application_crop_solid"])
}

for(i in seq(from=1, to=length(crop_names), by=1)) {
  
  emissions[(length(emissions$country)+1),]$sector <- "crops"
  emissions[(length(emissions$country)),]$subsector <- "manure application"
  emissions[(length(emissions$country)),]$species <- crop_names[i]
  emissions[(length(emissions$country)),]$pollutant <- "N2O"
  emissions[(length(emissions$country)),]$units <- "kg y-1"
  emissions[(length(emissions$country)),]$value <- as.numeric(get(paste("outputs_", crop_names[i], sep = ""))$manure_emis["N2O_direct_manure_application_crop_slurry"] + 
                                                                get(paste("outputs_", crop_names[i], sep = ""))$manure_emis["N2O_direct_manure_application_crop_solid"] +
                                                                get(paste("outputs_", crop_names[i], sep = ""))$manure_emis["N2O_indirect_vol_manure_application_crop_slurry"] + 
                                                                get(paste("outputs_", crop_names[i], sep = ""))$manure_emis["N2O_indirect_vol_manure_application_crop_solid"] +
                                                                get(paste("outputs_", crop_names[i], sep = ""))$manure_emis["N2O_indirect_leach_manure_application_crop_slurry"] + 
                                                                get(paste("outputs_", crop_names[i], sep = ""))$manure_emis["N2O_indirect_leach_manure_application_crop_solid"])
}



for(i in seq(from=1, to=length(crop_names), by=1)) {
  
  emissions[(length(emissions$country)+1),]$sector <- "crops"
  emissions[(length(emissions$country)),]$subsector <- "synthetic fertiliser application"
  emissions[(length(emissions$country)),]$species <- crop_names[i]
  emissions[(length(emissions$country)),]$pollutant <- "NH3"
  emissions[(length(emissions$country)),]$units <- "kg y-1"
  emissions[(length(emissions$country)),]$value <- as.numeric(sum(get(paste("outputs_", crop_names[i], sep = ""))$fertiliser_data["nh3_synthetic_fertiliser_kgNH3"], na.rm = TRUE))
}

for(i in seq(from=1, to=length(crop_names), by=1)) {
  
  emissions[(length(emissions$country)+1),]$sector <- "crops"
  emissions[(length(emissions$country)),]$subsector <- "synthetic fertiliser application"
  emissions[(length(emissions$country)),]$species <- crop_names[i]
  emissions[(length(emissions$country)),]$pollutant <- "NO"
  emissions[(length(emissions$country)),]$units <- "kg y-1"
  emissions[(length(emissions$country)),]$value <- as.numeric(sum(get(paste("outputs_", crop_names[i], sep = ""))$fertiliser_data["no_synthetic_fertiliser_kgNO2"], na.rm = TRUE))
}

for(i in seq(from=1, to=length(crop_names), by=1)) {
  
  emissions[(length(emissions$country)+1),]$sector <- "crops"
  emissions[(length(emissions$country)),]$subsector <- "synthetic fertiliser application"
  emissions[(length(emissions$country)),]$species <- crop_names[i]
  emissions[(length(emissions$country)),]$pollutant <- "N2O"
  emissions[(length(emissions$country)),]$units <- "kg y-1"
  emissions[(length(emissions$country)),]$value <- as.numeric(sum(get(paste("outputs_", crop_names[i], sep = ""))$fertiliser_data["n2o_direct_synthetic_fertiliser_kgN2O"], na.rm = TRUE)) + 
                                                    as.numeric(sum(get(paste("outputs_", crop_names[i], sep = ""))$fertiliser_data["n2o_indirect_vol_synthetic_fertiliser_kgN2O"], na.rm = TRUE)) +
                                                    as.numeric(sum(get(paste("outputs_", crop_names[i], sep = ""))$fertiliser_data["n2o_indirect_leach_synthetic_fertiliser_kgN2O"], na.rm = TRUE))
}



emissions[(length(emissions$country)+1),]$sector <- "crops"
emissions[(length(emissions$country)),]$subsector <- "methane from rice production"
emissions[(length(emissions$country)),]$species <- crop_names[40]
emissions[(length(emissions$country)),]$pollutant <- "CH4"
emissions[(length(emissions$country)),]$units <- "kg y-1"
emissions[(length(emissions$country)),]$value <- as.numeric(sum(get(paste("outputs_", crop_names[40], sep = ""))$ch4_emis_rice$ch4_emissions_rice, na.rm = TRUE))





pollutants <- c("CO", "NOx", "SO2", "NMVOC", "NH3", "PM10", "PM2.5", "BC", "OC", "CH4")


for(i in seq(from=1, to=length(crop_names), by=1)) {
  for(j in seq(from=1, to=length(pollutants), by=1)) {
    
  emissions[(length(emissions$country)+1),]$sector <- "crops"
  emissions[(length(emissions$country)),]$subsector <- "crop residue burning"
  emissions[(length(emissions$country)),]$species <- crop_names[i]
  emissions[(length(emissions$country)),]$pollutant <- pollutants[j]
  emissions[(length(emissions$country)),]$units <- "kg y-1"
  emissions[(length(emissions$country)),]$value <- as.numeric(get(paste("outputs_", crop_names[i], sep = ""))$residue$emissions_burning[pollutants[j]])

  }
} 

pollutants <- c("CO2", "CO", "NOx", "SO2", "NMVOC", "NH3", "PM10", "PM2.5", "BC", "OC", "CH4")


for(i in seq(from=1, to=length(crop_names), by=1)) {
  for(j in seq(from=1, to=length(pollutants), by=1)) {
    
    emissions[(length(emissions$country)+1),]$sector <- "crops"
    emissions[(length(emissions$country)),]$subsector <- "machinery diesel consumption"
    emissions[(length(emissions$country)),]$species <- crop_names[i]
    emissions[(length(emissions$country)),]$pollutant <- pollutants[j]
    emissions[(length(emissions$country)),]$units <- "kg y-1"
    emissions[(length(emissions$country)),]$value <- as.numeric(get(paste("outputs_", crop_names[i], sep = ""))$machinery_emissions$emissions[pollutants[j]])
    
  }
} 

for(i in seq(from=1, to=length(crop_names), by=1)) {
  for(j in seq(from=1, to=length(pollutants), by=1)) {
    
    emissions[(length(emissions$country)+1),]$sector <- "crops"
    emissions[(length(emissions$country)),]$subsector <- "ammonia production energy"
    emissions[(length(emissions$country)),]$species <- crop_names[i]
    emissions[(length(emissions$country)),]$pollutant <- pollutants[j]
    emissions[(length(emissions$country)),]$units <- "kg y-1"
    emissions[(length(emissions$country)),]$value <- sum(c(as.numeric(get(paste("outputs_", crop_names[i], sep = ""))$fertiliser_energy_emissions$ammonia_gas_emissions[pollutants[j]]),
                                                           as.numeric(get(paste("outputs_", crop_names[i], sep = ""))$fertiliser_energy_emissions$ammonia_oil_emissions[pollutants[j]]),
                                                           as.numeric(get(paste("outputs_", crop_names[i], sep = ""))$fertiliser_energy_emissions$ammonia_coal_emissions[pollutants[j]])), na.rm = TRUE)
    
  }
} 

for(i in seq(from=1, to=length(crop_names), by=1)) {
    
    emissions[(length(emissions$country)+1),]$sector <- "crops"
    emissions[(length(emissions$country)),]$subsector <- "nitric acid production"
    emissions[(length(emissions$country)),]$species <- crop_names[i]
    emissions[(length(emissions$country)),]$pollutant <- "N2O"
    emissions[(length(emissions$country)),]$units <- "kg y-1"
    emissions[(length(emissions$country)),]$value <- as.numeric(get(paste("outputs_", crop_names[i], sep = ""))$fertiliser_energy_emissions$nitric_acid_n2o_emissions)
    
} 





for(i in seq(from=1, to=length(feed_crops), by=1)) {
  
  emissions[(length(emissions$country)+1),]$sector <- "crops"
  emissions[(length(emissions$country)),]$subsector <- "manure application"
  emissions[(length(emissions$country)),]$species <- feed_crops[i]
  emissions[(length(emissions$country)),]$pollutant <- "NH3"
  emissions[(length(emissions$country)),]$units <- "kg y-1"
  emissions[(length(emissions$country)),]$value <- as.numeric(get(paste("outputs_", feed_crops[i], sep = ""))$manure_emis["NH3_manure_application_crop_slurry"] + 
                                                                get(paste("outputs_", feed_crops[i], sep = ""))$manure_emis["NH3_manure_application_crop_solid"])
}


for(i in seq(from=1, to=length(feed_crops), by=1)) {
  
  emissions[(length(emissions$country)+1),]$sector <- "crops"
  emissions[(length(emissions$country)),]$subsector <- "manure application"
  emissions[(length(emissions$country)),]$species <- feed_crops[i]
  emissions[(length(emissions$country)),]$pollutant <- "NO"
  emissions[(length(emissions$country)),]$units <- "kg y-1"
  emissions[(length(emissions$country)),]$value <- as.numeric(get(paste("outputs_", feed_crops[i], sep = ""))$manure_emis["NO_manure_application_crop_slurry"] + 
                                                                get(paste("outputs_", feed_crops[i], sep = ""))$manure_emis["NO_manure_application_crop_solid"])
}

for(i in seq(from=1, to=length(feed_crops), by=1)) {
  
  emissions[(length(emissions$country)+1),]$sector <- "crops"
  emissions[(length(emissions$country)),]$subsector <- "manure application"
  emissions[(length(emissions$country)),]$species <- feed_crops[i]
  emissions[(length(emissions$country)),]$pollutant <- "N2O"
  emissions[(length(emissions$country)),]$units <- "kg y-1"
  emissions[(length(emissions$country)),]$value <- as.numeric(get(paste("outputs_", feed_crops[i], sep = ""))$manure_emis["N2O_direct_manure_application_crop_slurry"] + 
                                                                get(paste("outputs_", feed_crops[i], sep = ""))$manure_emis["N2O_direct_manure_application_crop_solid"] +
                                                                get(paste("outputs_", feed_crops[i], sep = ""))$manure_emis["N2O_indirect_vol_manure_application_crop_slurry"] + 
                                                                get(paste("outputs_", feed_crops[i], sep = ""))$manure_emis["N2O_indirect_vol_manure_application_crop_solid"] +
                                                                get(paste("outputs_", feed_crops[i], sep = ""))$manure_emis["N2O_indirect_leach_manure_application_crop_slurry"] + 
                                                                get(paste("outputs_", feed_crops[i], sep = ""))$manure_emis["N2O_indirect_leach_manure_application_crop_solid"])
}



for(i in seq(from=1, to=length(feed_crops), by=1)) {
  
  emissions[(length(emissions$country)+1),]$sector <- "crops"
  emissions[(length(emissions$country)),]$subsector <- "synthetic fertiliser application"
  emissions[(length(emissions$country)),]$species <- feed_crops[i]
  emissions[(length(emissions$country)),]$pollutant <- "NH3"
  emissions[(length(emissions$country)),]$units <- "kg y-1"
  emissions[(length(emissions$country)),]$value <- as.numeric(sum(get(paste("outputs_", feed_crops[i], sep = ""))$fertiliser_data["nh3_synthetic_fertiliser_kgNH3"], na.rm = TRUE))
}

for(i in seq(from=1, to=length(feed_crops), by=1)) {
  
  emissions[(length(emissions$country)+1),]$sector <- "crops"
  emissions[(length(emissions$country)),]$subsector <- "synthetic fertiliser application"
  emissions[(length(emissions$country)),]$species <- feed_crops[i]
  emissions[(length(emissions$country)),]$pollutant <- "NO"
  emissions[(length(emissions$country)),]$units <- "kg y-1"
  emissions[(length(emissions$country)),]$value <- as.numeric(sum(get(paste("outputs_", feed_crops[i], sep = ""))$fertiliser_data["no_synthetic_fertiliser_kgNO2"], na.rm = TRUE))
}

for(i in seq(from=1, to=length(feed_crops), by=1)) {
  
  emissions[(length(emissions$country)+1),]$sector <- "crops"
  emissions[(length(emissions$country)),]$subsector <- "synthetic fertiliser application"
  emissions[(length(emissions$country)),]$species <- feed_crops[i]
  emissions[(length(emissions$country)),]$pollutant <- "N2O"
  emissions[(length(emissions$country)),]$units <- "kg y-1"
  emissions[(length(emissions$country)),]$value <- as.numeric(sum(get(paste("outputs_", feed_crops[i], sep = ""))$fertiliser_data["n2o_direct_synthetic_fertiliser_kgN2O"], na.rm = TRUE)) + 
    as.numeric(sum(get(paste("outputs_", feed_crops[i], sep = ""))$fertiliser_data["n2o_indirect_vol_synthetic_fertiliser_kgN2O"], na.rm = TRUE)) +
    as.numeric(sum(get(paste("outputs_", feed_crops[i], sep = ""))$fertiliser_data["n2o_indirect_leach_synthetic_fertiliser_kgN2O"], na.rm = TRUE))
}


pollutants <- c("CO2", "CO", "NOx", "SO2", "NMVOC", "NH3", "PM10", "PM2.5", "BC", "OC", "CH4")



for(i in seq(from=1, to=length(feed_crops), by=1)) {
  for(j in seq(from=1, to=length(pollutants), by=1)) {
    
    emissions[(length(emissions$country)+1),]$sector <- "crops"
    emissions[(length(emissions$country)),]$subsector <- "ammonia production energy"
    emissions[(length(emissions$country)),]$species <- feed_crops[i]
    emissions[(length(emissions$country)),]$pollutant <- pollutants[j]
    emissions[(length(emissions$country)),]$units <- "kg y-1"
    emissions[(length(emissions$country)),]$value <- sum(c(as.numeric(get(paste("outputs_", feed_crops[i], sep = ""))$fertiliser_energy_emissions$ammonia_gas_emissions[pollutants[j]]),
                                                           as.numeric(get(paste("outputs_", feed_crops[i], sep = ""))$fertiliser_energy_emissions$ammonia_oil_emissions[pollutants[j]]),
                                                           as.numeric(get(paste("outputs_", feed_crops[i], sep = ""))$fertiliser_energy_emissions$ammonia_coal_emissions[pollutants[j]])), na.rm = TRUE)
    
  }
} 

for(i in seq(from=1, to=length(feed_crops), by=1)) {
  
  emissions[(length(emissions$country)+1),]$sector <- "crops"
  emissions[(length(emissions$country)),]$subsector <- "nitric acid production"
  emissions[(length(emissions$country)),]$species <- feed_crops[i]
  emissions[(length(emissions$country)),]$pollutant <- "N2O"
  emissions[(length(emissions$country)),]$units <- "kg y-1"
  emissions[(length(emissions$country)),]$value <- as.numeric(get(paste("outputs_", feed_crops[i], sep = ""))$fertiliser_energy_emissions$nitric_acid_n2o_emissions)
  
} 










  
  emissions[(length(emissions$country)+1),]$sector <- "grassland"
  emissions[(length(emissions$country)),]$subsector <- "manure application"
  emissions[(length(emissions$country)),]$pollutant <- "NH3"
  emissions[(length(emissions$country)),]$units <- "kg y-1"
  emissions[(length(emissions$country)),]$value <- as.numeric(get(paste("outputs_grassland", sep = ""))$manure_emis["NH3_manure_application_crop_slurry"] + 
                                                                get(paste("outputs_grassland", sep = ""))$manure_emis["NH3_manure_application_crop_solid"])

  
  emissions[(length(emissions$country)+1),]$sector <- "grassland"
  emissions[(length(emissions$country)),]$subsector <- "manure application"
  emissions[(length(emissions$country)),]$pollutant <- "NO"
  emissions[(length(emissions$country)),]$units <- "kg y-1"
  emissions[(length(emissions$country)),]$value <- as.numeric(get(paste("outputs_grassland", sep = ""))$manure_emis["NO_manure_application_crop_slurry"] + 
                                                                get(paste("outputs_grassland", sep = ""))$manure_emis["NO_manure_application_crop_solid"])

  
  emissions[(length(emissions$country)+1),]$sector <- "grassland"
  emissions[(length(emissions$country)),]$subsector <- "manure application"
  emissions[(length(emissions$country)),]$pollutant <- "N2O"
  emissions[(length(emissions$country)),]$units <- "kg y-1"
  emissions[(length(emissions$country)),]$value <- as.numeric(get(paste("outputs_grassland", sep = ""))$manure_emis["N2O_direct_manure_application_crop_slurry"] + 
                                                                get(paste("outputs_grassland", sep = ""))$manure_emis["N2O_direct_manure_application_crop_solid"] +
                                                                get(paste("outputs_grassland", sep = ""))$manure_emis["N2O_indirect_vol_manure_application_crop_slurry"] + 
                                                                get(paste("outputs_grassland", sep = ""))$manure_emis["N2O_indirect_vol_manure_application_crop_solid"] +
                                                                get(paste("outputs_grassland", sep = ""))$manure_emis["N2O_indirect_leach_manure_application_crop_slurry"] + 
                                                                get(paste("outputs_grassland", sep = ""))$manure_emis["N2O_indirect_leach_manure_application_crop_solid"])

  
  
  emissions[(length(emissions$country)+1),]$sector <- "grassland"
  emissions[(length(emissions$country)),]$subsector <- "graze"
  emissions[(length(emissions$country)),]$pollutant <- "NH3"
  emissions[(length(emissions$country)),]$units <- "kg y-1"
  emissions[(length(emissions$country)),]$value <- as.numeric(get(paste("outputs_grassland", sep = ""))$manure_emis["NH3_manure_application_crop_graze"]) 
  
  
  emissions[(length(emissions$country)+1),]$sector <- "grassland"
  emissions[(length(emissions$country)),]$subsector <- "graze"
  emissions[(length(emissions$country)),]$pollutant <- "NO"
  emissions[(length(emissions$country)),]$units <- "kg y-1"
  emissions[(length(emissions$country)),]$value <- as.numeric(get(paste("outputs_grassland", sep = ""))$manure_emis["NO_manure_application_crop_graze"]) 
  
  
  emissions[(length(emissions$country)+1),]$sector <- "grassland"
  emissions[(length(emissions$country)),]$subsector <- "graze"
  emissions[(length(emissions$country)),]$pollutant <- "N2O"
  emissions[(length(emissions$country)),]$units <- "kg y-1"
  emissions[(length(emissions$country)),]$value <- as.numeric(get(paste("outputs_grassland", sep = ""))$manure_emis["N2O_direct_manure_application_crop_graze"] + 
                                                                get(paste("outputs_grassland", sep = ""))$manure_emis["N2O_indirect_vol_manure_application_crop_graze"] + 
                                                                get(paste("outputs_grassland", sep = ""))$manure_emis["N2O_indirect_leach_manure_application_crop_graze"])
  
  
  
  pollutants <- c("CO2", "CO", "NOx", "SO2", "NMVOC", "NH3", "PM10", "PM2.5", "BC", "OC", "CH4")
  
  
  for(j in seq(from=1, to=length(pollutants), by=1)) {
      
      emissions[(length(emissions$country)+1),]$sector <- "grassland"
      emissions[(length(emissions$country)),]$subsector <- "ammonia production energy"
      emissions[(length(emissions$country)),]$pollutant <- pollutants[j]
      emissions[(length(emissions$country)),]$units <- "kg y-1"
      emissions[(length(emissions$country)),]$value <- sum(c(as.numeric(get(paste("outputs_", crop_names[i], sep = ""))$fertiliser_energy_emissions$ammonia_gas_emissions[pollutants[j]]),
                                                             as.numeric(get(paste("outputs_", crop_names[i], sep = ""))$fertiliser_energy_emissions$ammonia_oil_emissions[pollutants[j]]),
                                                             as.numeric(get(paste("outputs_", crop_names[i], sep = ""))$fertiliser_energy_emissions$ammonia_coal_emissions[pollutants[j]])), na.rm = TRUE)
      
    }
  
    emissions[(length(emissions$country)+1),]$sector <- "grassland"
    emissions[(length(emissions$country)),]$subsector <- "nitric acid production"
    emissions[(length(emissions$country)),]$pollutant <- "N2O"
    emissions[(length(emissions$country)),]$units <- "kg y-1"
    emissions[(length(emissions$country)),]$value <- as.numeric(get(paste("outputs_", crop_names[i], sep = ""))$fertiliser_energy_emissions$nitric_acid_n2o_emissions)
    
  
  
  
  
  emissions[(length(emissions$country)+1),]$sector <- "grassland"
  emissions[(length(emissions$country)),]$subsector <- "synthetic fertiliser application"
  emissions[(length(emissions$country)),]$pollutant <- "NH3"
  emissions[(length(emissions$country)),]$units <- "kg y-1"
  emissions[(length(emissions$country)),]$value <- as.numeric(sum(get(paste("outputs_grassland", sep = ""))$fertiliser_data["nh3_synthetic_fertiliser_kgNH3"], na.rm = TRUE))



  
  emissions[(length(emissions$country)+1),]$sector <- "grassland"
  emissions[(length(emissions$country)),]$subsector <- "synthetic fertiliser application"
  emissions[(length(emissions$country)),]$pollutant <- "NO"
  emissions[(length(emissions$country)),]$units <- "kg y-1"
  emissions[(length(emissions$country)),]$value <- as.numeric(sum(get(paste("outputs_grassland", sep = ""))$fertiliser_data["no_synthetic_fertiliser_kgNO2"], na.rm = TRUE))


  
  emissions[(length(emissions$country)+1),]$sector <- "grassland"
  emissions[(length(emissions$country)),]$subsector <- "synthetic fertiliser application"
  emissions[(length(emissions$country)),]$pollutant <- "N2O"
  emissions[(length(emissions$country)),]$units <- "kg y-1"
  emissions[(length(emissions$country)),]$value <- as.numeric(sum(get(paste("outputs_grassland", sep = ""))$fertiliser_data["n2o_direct_synthetic_fertiliser_kgN2O"], na.rm = TRUE)) + 
    as.numeric(sum(get(paste("outputs_grassland", sep = ""))$fertiliser_data["n2o_indirect_vol_synthetic_fertiliser_kgN2O"], na.rm = TRUE)) +
    as.numeric(sum(get(paste("outputs_grassland", sep = ""))$fertiliser_data["n2o_indirect_leach_synthetic_fertiliser_kgN2O"], na.rm = TRUE))





emissions$country <- country
emissions$year <- year











emissions_summary <- data.frame(country = numeric(0), year = numeric(0), sector = numeric(0), subsector = numeric(0), species = numeric(0), mgmt = numeric(0), animal = numeric(0), pollutant = numeric(0), units = numeric(0), value = numeric(0))


for(count in seq(from=1, to=length(species_lists$species), by=1)) {
      emissions_summary[(length(emissions_summary$country)+1),]$sector <- "livestock"
      emissions_summary[(length(emissions_summary$country)),]$subsector <- "enteric fermentation"
      emissions_summary[(length(emissions_summary$country)),]$species <- species_lists$species[count]
      emissions_summary[(length(emissions_summary$country)),]$pollutant <- "CH4"
      emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
      emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$species == species_lists$species[count] & emissions$subsector == "enteric fermentation"), ]$value, na.rm = TRUE)
      
    }

for(count in seq(from=1, to=length(species_lists$species), by=1)) {
  emissions_summary[(length(emissions_summary$country)+1),]$sector <- "livestock"
  emissions_summary[(length(emissions_summary$country)),]$subsector <- "manure management"
  emissions_summary[(length(emissions_summary$country)),]$species <- species_lists$species[count]
  emissions_summary[(length(emissions_summary$country)),]$pollutant <- "CH4"
  emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
  emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$species == species_lists$species[count] & emissions$subsector == "manure management" & emissions$pollutant == "CH4"), ]$value, na.rm = TRUE)
  
}

for(count in seq(from=1, to=length(species_lists$species), by=1)) {
  emissions_summary[(length(emissions_summary$country)+1),]$sector <- "livestock"
  emissions_summary[(length(emissions_summary$country)),]$subsector <- "manure management"
  emissions_summary[(length(emissions_summary$country)),]$species <- species_lists$species[count]
  emissions_summary[(length(emissions_summary$country)),]$pollutant <- "N2O"
  emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
  emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$species == species_lists$species[count] & emissions$subsector == "manure management" & emissions$pollutant == "N2O"), ]$value, na.rm = TRUE)
  
}

for(count in seq(from=1, to=length(species_lists$species), by=1)) {
  emissions_summary[(length(emissions_summary$country)+1),]$sector <- "livestock"
  emissions_summary[(length(emissions_summary$country)),]$subsector <- "manure management"
  emissions_summary[(length(emissions_summary$country)),]$species <- species_lists$species[count]
  emissions_summary[(length(emissions_summary$country)),]$pollutant <- "NH3"
  emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
  emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$species == species_lists$species[count] & emissions$subsector == "manure management" & emissions$pollutant == "NH3"), ]$value, na.rm = TRUE)
  
}

for(count in seq(from=1, to=length(species_lists$species), by=1)) {
  emissions_summary[(length(emissions_summary$country)+1),]$sector <- "livestock"
  emissions_summary[(length(emissions_summary$country)),]$subsector <- "manure management"
  emissions_summary[(length(emissions_summary$country)),]$species <- species_lists$species[count]
  emissions_summary[(length(emissions_summary$country)),]$pollutant <- "NO"
  emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
  emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$species == species_lists$species[count] & emissions$subsector == "manure management" & emissions$pollutant == "NO"), ]$value, na.rm = TRUE)
  
}

emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "manure application"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "NH3"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "manure application" & emissions$pollutant == "NH3"), ]$value, na.rm = TRUE)

emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "manure application"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "N2O"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "manure application" & emissions$pollutant == "N2O"), ]$value, na.rm = TRUE)

emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "manure application"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "NO"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "manure application" & emissions$pollutant == "NO"), ]$value, na.rm = TRUE)



emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "synthetic fertiliser application"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "NH3"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "synthetic fertiliser application" & emissions$pollutant == "NH3"), ]$value, na.rm = TRUE)

emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "synthetic fertiliser application"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "N2O"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "synthetic fertiliser application" & emissions$pollutant == "N2O"), ]$value, na.rm = TRUE)

emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "synthetic fertiliser application"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "NO"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "synthetic fertiliser application" & emissions$pollutant == "NO"), ]$value, na.rm = TRUE)

emissions_summary[(length(emissions_summary$country)+1),]$sector <- "pasture"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "graze"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "NH3"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "graze" & emissions$pollutant == "NH3"), ]$value, na.rm = TRUE)

emissions_summary[(length(emissions_summary$country)+1),]$sector <- "pasture"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "graze"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "N2O"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "graze" & emissions$pollutant == "N2O"), ]$value, na.rm = TRUE)

emissions_summary[(length(emissions_summary$country)+1),]$sector <- "pasture"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "graze"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "NO"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "graze" & emissions$pollutant == "NO"), ]$value, na.rm = TRUE)



emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "crop residue burning"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "CO"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "crop residue burning" & emissions$pollutant == "CO"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "crop residue burning"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "NOx"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "crop residue burning" & emissions$pollutant == "NOx"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "crop residue burning"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "SO2"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "crop residue burning" & emissions$pollutant == "SO2"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "crop residue burning"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "NMVOC"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "crop residue burning" & emissions$pollutant == "NMVOC"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "crop residue burning"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "NH3"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "crop residue burning" & emissions$pollutant == "NH3"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "crop residue burning"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "PM10"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "crop residue burning" & emissions$pollutant == "PM10"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "crop residue burning"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "PM2.5"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "crop residue burning" & emissions$pollutant == "PM2.5"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "crop residue burning"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "BC"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "crop residue burning" & emissions$pollutant == "BC"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "crop residue burning"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "OC"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "crop residue burning" & emissions$pollutant == "OC"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "crop residue burning"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "CH4"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "crop residue burning" & emissions$pollutant == "CH4"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "methane from rice production"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "CH4"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "methane from rice production"), ]$value, na.rm = TRUE)






emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "machinery diesel consumption"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "CO2"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "machinery diesel consumption" & emissions$pollutant == "CO2"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "machinery diesel consumption"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "CO"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "machinery diesel consumption" & emissions$pollutant == "CO"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "machinery diesel consumption"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "NOx"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "machinery diesel consumption" & emissions$pollutant == "NOx"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "machinery diesel consumption"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "SO2"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "machinery diesel consumption" & emissions$pollutant == "SO2"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "machinery diesel consumption"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "NMVOC"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "machinery diesel consumption" & emissions$pollutant == "NMVOC"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "machinery diesel consumption"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "NH3"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "machinery diesel consumption" & emissions$pollutant == "NH3"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "machinery diesel consumption"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "PM10"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "machinery diesel consumption" & emissions$pollutant == "PM10"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "machinery diesel consumption"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "PM2.5"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "machinery diesel consumption" & emissions$pollutant == "PM2.5"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "machinery diesel consumption"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "BC"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "machinery diesel consumption" & emissions$pollutant == "BC"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "machinery diesel consumption"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "OC"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "machinery diesel consumption" & emissions$pollutant == "OC"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "machinery diesel consumption"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "CH4"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "machinery diesel consumption" & emissions$pollutant == "CH4"), ]$value, na.rm = TRUE)





emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "ammonia production energy"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "CO2"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "ammonia production energy" & emissions$pollutant == "CO2"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "ammonia production energy"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "CO"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "ammonia production energy" & emissions$pollutant == "CO"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "ammonia production energy"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "NOx"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "ammonia production energy" & emissions$pollutant == "NOx"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "ammonia production energy"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "SO2"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "ammonia production energy" & emissions$pollutant == "SO2"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "ammonia production energy"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "NMVOC"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "ammonia production energy" & emissions$pollutant == "NMVOC"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "ammonia production energy"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "NH3"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "ammonia production energy" & emissions$pollutant == "NH3"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "ammonia production energy"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "PM10"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "ammonia production energy" & emissions$pollutant == "PM10"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "ammonia production energy"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "PM2.5"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "ammonia production energy" & emissions$pollutant == "PM2.5"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "ammonia production energy"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "BC"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "ammonia production energy" & emissions$pollutant == "BC"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "ammonia production energy"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "OC"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "ammonia production energy" & emissions$pollutant == "OC"), ]$value, na.rm = TRUE)


emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "ammonia production energy"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "CH4"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "ammonia production energy" & emissions$pollutant == "CH4"), ]$value, na.rm = TRUE)



emissions_summary[(length(emissions_summary$country)+1),]$sector <- "crops"
emissions_summary[(length(emissions_summary$country)),]$subsector <- "nitric acid production"
emissions_summary[(length(emissions_summary$country)),]$pollutant <- "N2O"
emissions_summary[(length(emissions_summary$country)),]$units <- "kg y-1"
emissions_summary[(length(emissions_summary$country)),]$value <- sum(emissions[ which(emissions$subsector == "nitric acid production" & emissions$pollutant == "N2O"), ]$value, na.rm = TRUE)



emissions_summary$country <- country
emissions_summary$year <- year





other_outputs <- data.frame(country = numeric(0), year = numeric(0), sector = numeric(0), subsector = numeric(0), variable = numeric(0), units = numeric(0), value = numeric(0))
for(i in seq(from=1, to=length(crop_names), by=1)) {
    
    other_outputs[(length(other_outputs$country)+1),]$country <- country
    other_outputs[(length(other_outputs$country)),]$year <- year
    other_outputs[(length(other_outputs$country)),]$sector <- crop_names[i]
    other_outputs[(length(other_outputs$country)),]$variable <- "residue fuel"
    other_outputs[(length(other_outputs$country)),]$units <- "kg y-1"
    other_outputs[(length(other_outputs$country)),]$value <- as.numeric(get(paste("outputs_", crop_names[i], sep = ""))$residue$residue_fuel)*1000
    
    other_outputs[(length(other_outputs$country)+1),]$country <- country
    other_outputs[(length(other_outputs$country)),]$year <- year
    other_outputs[(length(other_outputs$country)),]$sector <- crop_names[i]
    other_outputs[(length(other_outputs$country)),]$variable <- "above ground residue"
    other_outputs[(length(other_outputs$country)),]$units <- "kg y-1"
    other_outputs[(length(other_outputs$country)),]$value <- as.numeric(get(paste("outputs_", crop_names[i], sep = ""))$residue$residue_ag)*1000
    
    other_outputs[(length(other_outputs$country)+1),]$country <- country
    other_outputs[(length(other_outputs$country)),]$year <- year
    other_outputs[(length(other_outputs$country)),]$sector <- crop_names[i]
    other_outputs[(length(other_outputs$country)),]$variable <- "below ground residue"
    other_outputs[(length(other_outputs$country)),]$units <- "kg y-1"
    other_outputs[(length(other_outputs$country)),]$value <- as.numeric(get(paste("outputs_", crop_names[i], sep = ""))$residue$residue_bg)*1000
    
    other_outputs[(length(other_outputs$country)+1),]$country <- country
    other_outputs[(length(other_outputs$country)),]$year <- year
    other_outputs[(length(other_outputs$country)),]$sector <- crop_names[i]
    other_outputs[(length(other_outputs$country)),]$variable <- "total residue burned"
    other_outputs[(length(other_outputs$country)),]$units <- "kg y-1"
    other_outputs[(length(other_outputs$country)),]$value <- as.numeric(get(paste("outputs_", crop_names[i], sep = ""))$residue$total_residue_burned)*1000
    
    
    other_outputs[(length(other_outputs$country)+1),]$country <- country
    other_outputs[(length(other_outputs$country)),]$year <- year
    other_outputs[(length(other_outputs$country)),]$sector <- crop_names[i]
    other_outputs[(length(other_outputs$country)),]$variable <- "yield"
    other_outputs[(length(other_outputs$country)),]$units <- "kg ha-1 y-1"
    other_outputs[(length(other_outputs$country)),]$value <- as.numeric(get(paste("outputs_", crop_names[i], sep = ""))$crop_yield$yield)
    
    other_outputs[(length(other_outputs$country)+1),]$country <- country
    other_outputs[(length(other_outputs$country)),]$year <- year
    other_outputs[(length(other_outputs$country)),]$sector <- crop_names[i]
    other_outputs[(length(other_outputs$country)),]$variable <- "land area"
    other_outputs[(length(other_outputs$country)),]$units <- "ha"
    other_outputs[(length(other_outputs$country)),]$value <- as.numeric(get(paste("outputs_", crop_names[i], sep = ""))$crop_yield$land_area)
    
    other_outputs[(length(other_outputs$country)+1),]$country <- country
    other_outputs[(length(other_outputs$country)),]$year <- year
    other_outputs[(length(other_outputs$country)),]$sector <- crop_names[i]
    other_outputs[(length(other_outputs$country)),]$variable <- "N surplus"
    other_outputs[(length(other_outputs$country)),]$units <- "kg N y-1"
    other_outputs[(length(other_outputs$country)),]$value <- as.numeric(get(paste("outputs_", crop_names[i], sep = ""))$crop_yield$N_surplus)
    
    other_outputs[(length(other_outputs$country)+1),]$country <- country
    other_outputs[(length(other_outputs$country)),]$year <- year
    other_outputs[(length(other_outputs$country)),]$sector <- crop_names[i]
    other_outputs[(length(other_outputs$country)),]$variable <- "NUE"
    other_outputs[(length(other_outputs$country)),]$units <- "fraction"
    other_outputs[(length(other_outputs$country)),]$value <- as.numeric(get(paste("outputs_", crop_names[i], sep = ""))$crop_yield$NUE)
    
    other_outputs[(length(other_outputs$country)+1),]$country <- country
    other_outputs[(length(other_outputs$country)),]$year <- year
    other_outputs[(length(other_outputs$country)),]$sector <- crop_names[i]
    other_outputs[(length(other_outputs$country)),]$variable <- "ymax"
    other_outputs[(length(other_outputs$country)),]$units <- "kg N y-1"
    other_outputs[(length(other_outputs$country)),]$value <- as.numeric(get(paste("outputs_", crop_names[i], sep = ""))$crop_yield$ymax)
    
} 


for(i in seq(from=1, to=length(feed_crops), by=1)) {
  
  other_outputs[(length(other_outputs$country)+1),]$country <- country
  other_outputs[(length(other_outputs$country)),]$year <- year
  other_outputs[(length(other_outputs$country)),]$sector <- feed_crops[i]
  other_outputs[(length(other_outputs$country)),]$variable <- "yield"
  other_outputs[(length(other_outputs$country)),]$units <- "kg ha-1 y-1"
  other_outputs[(length(other_outputs$country)),]$value <- as.numeric(get(paste("outputs_", feed_crops[i], sep = ""))$crop_yield$yield)
  
  other_outputs[(length(other_outputs$country)+1),]$country <- country
  other_outputs[(length(other_outputs$country)),]$year <- year
  other_outputs[(length(other_outputs$country)),]$sector <- feed_crops[i]
  other_outputs[(length(other_outputs$country)),]$variable <- "land area"
  other_outputs[(length(other_outputs$country)),]$units <- "ha"
  other_outputs[(length(other_outputs$country)),]$value <- as.numeric(get(paste("outputs_", feed_crops[i], sep = ""))$crop_yield$land_area)
  
  other_outputs[(length(other_outputs$country)+1),]$country <- country
  other_outputs[(length(other_outputs$country)),]$year <- year
  other_outputs[(length(other_outputs$country)),]$sector <- feed_crops[i]
  other_outputs[(length(other_outputs$country)),]$variable <- "N surplus"
  other_outputs[(length(other_outputs$country)),]$units <- "kg N y-1"
  other_outputs[(length(other_outputs$country)),]$value <- as.numeric(get(paste("outputs_", feed_crops[i], sep = ""))$crop_yield$N_surplus)
  
  
  
} 

other_outputs[(length(other_outputs$country)+1),]$country <- country
other_outputs[(length(other_outputs$country)),]$year <- year
other_outputs[(length(other_outputs$country)),]$sector <- "grassland"
other_outputs[(length(other_outputs$country)),]$variable <- "N surplus"
other_outputs[(length(other_outputs$country)),]$units <- "kg N y-1"
other_outputs[(length(other_outputs$country)),]$value <- as.numeric(get(paste("outputs_grassland", sep = ""))$N_surplus)







































##Dietary risks


##Prevalence of population under-nourished
GINI <- inputs_meat[which(inputs_meat$variable == "GINI"),][paste("y_", year, sep = "")][[1]]
GDP_per_capita <- if(inputs_meat[which(inputs_meat$variable == "GDP per capita"),][paste("y_", year, sep = "")][[1]] > 0){inputs_meat[which(inputs_meat$variable == "GDP per capita"),][paste("y_", year, sep = "")][[1]]}else{1E-50}
malnutrition_calories <- inputs_health[which(inputs_health$variable == "Malnutrition Calorie Cutoff"),][paste("y_", year, sep = "")][[1]]
calories_consumed <- sum(consumption_waste$kcal_consumed, na.rm = TRUE) + sum(consumption_waste$kcal_wasted, na.rm = TRUE) 


##associate food types consumed with different food categories
food_parameters <- read.csv("C:\\Users\\cm1479\\Google Drive\\L drive files\\Africa Assessment\\Agriculture\\food groups n content Lassaletta 2014.csv")
food_parameters <- food_parameters[c("Item", "Type_1", "Type_2", "Type_3")]
consumption_waste$Item <- consumption_waste$species
consumption_waste <- merge(consumption_waste, food_parameters, by = c("Item"), all.x = TRUE)

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

inputs_health[is.nan(inputs_health)] <- 0

ages <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+")
ages_2 <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "99.9+")

population <- inputs_meat[which(inputs_meat$variable == "Population"),][paste("y_", year, sep = "")][[1]]
pop_0_frac <- inputs_health[which(inputs_health$age == "0-4" & inputs_health$variable == "Fraction total population"),][paste("y_", year, sep = "")][[1]]
pop_5_frac <- inputs_health[which(inputs_health$age == "5-9" & inputs_health$variable == "Fraction total population"),][paste("y_", year, sep = "")][[1]]
pop_10_frac <- inputs_health[which(inputs_health$age == "10-14" & inputs_health$variable == "Fraction total population"),][paste("y_", year, sep = "")][[1]]
pop_15_frac <- inputs_health[which(inputs_health$age == "15-19" & inputs_health$variable == "Fraction total population"),][paste("y_", year, sep = "")][[1]]
pop_20_frac <- inputs_health[which(inputs_health$age == "20-24" & inputs_health$variable == "Fraction total population"),][paste("y_", year, sep = "")][[1]]
pop_25_frac <- inputs_health[which(inputs_health$age == "25-29" & inputs_health$variable == "Fraction total population"),][paste("y_", year, sep = "")][[1]]
pop_30_frac <- inputs_health[which(inputs_health$age == "30-34" & inputs_health$variable == "Fraction total population"),][paste("y_", year, sep = "")][[1]]
pop_35_frac <- inputs_health[which(inputs_health$age == "35-39" & inputs_health$variable == "Fraction total population"),][paste("y_", year, sep = "")][[1]]
pop_40_frac <- inputs_health[which(inputs_health$age == "40-44" & inputs_health$variable == "Fraction total population"),][paste("y_", year, sep = "")][[1]]
pop_45_frac <- inputs_health[which(inputs_health$age == "45-49" & inputs_health$variable == "Fraction total population"),][paste("y_", year, sep = "")][[1]]
pop_50_frac <- inputs_health[which(inputs_health$age == "50-54" & inputs_health$variable == "Fraction total population"),][paste("y_", year, sep = "")][[1]]
pop_55_frac <- inputs_health[which(inputs_health$age == "55-59" & inputs_health$variable == "Fraction total population"),][paste("y_", year, sep = "")][[1]]
pop_60_frac <- inputs_health[which(inputs_health$age == "60-64" & inputs_health$variable == "Fraction total population"),][paste("y_", year, sep = "")][[1]]
pop_65_frac <- inputs_health[which(inputs_health$age == "65-69" & inputs_health$variable == "Fraction total population"),][paste("y_", year, sep = "")][[1]]
pop_70_frac <- inputs_health[which(inputs_health$age == "70-74" & inputs_health$variable == "Fraction total population"),][paste("y_", year, sep = "")][[1]]
pop_75_frac <- inputs_health[which(inputs_health$age == "75-79" & inputs_health$variable == "Fraction total population"),][paste("y_", year, sep = "")][[1]]
pop_80_frac <- inputs_health[which(inputs_health$age == "80-84" & inputs_health$variable == "Fraction total population"),][paste("y_", year, sep = "")][[1]]
pop_85_frac <- inputs_health[which(inputs_health$age == "85-89" & inputs_health$variable == "Fraction total population"),][paste("y_", year, sep = "")][[1]]
pop_90_frac <- inputs_health[which(inputs_health$age == "90-94" & inputs_health$variable == "Fraction total population"),][paste("y_", year, sep = "")][[1]]
pop_95_frac <- inputs_health[which(inputs_health$age == "95-99" & inputs_health$variable == "Fraction total population"),][paste("y_", year, sep = "")][[1]]
pop_100_frac <- inputs_health[which(inputs_health$age == "100+" & inputs_health$variable == "Fraction total population"),][paste("y_", year, sep = "")][[1]]

pop_0_female_frac <- inputs_health[which(inputs_health$age == "0-4" & inputs_health$variable == "Female Fraction"),][paste("y_", year, sep = "")][[1]]
pop_5_female_frac <- inputs_health[which(inputs_health$age == "5-9" & inputs_health$variable == "Female Fraction"),][paste("y_", year, sep = "")][[1]]
pop_10_female_frac <- inputs_health[which(inputs_health$age == "10-14" & inputs_health$variable == "Female Fraction"),][paste("y_", year, sep = "")][[1]]
pop_15_female_frac <- inputs_health[which(inputs_health$age == "15-19" & inputs_health$variable == "Female Fraction"),][paste("y_", year, sep = "")][[1]]
pop_20_female_frac <- inputs_health[which(inputs_health$age == "20-24" & inputs_health$variable == "Female Fraction"),][paste("y_", year, sep = "")][[1]]
pop_25_female_frac <- inputs_health[which(inputs_health$age == "25-29" & inputs_health$variable == "Female Fraction"),][paste("y_", year, sep = "")][[1]]
pop_30_female_frac <- inputs_health[which(inputs_health$age == "30-34" & inputs_health$variable == "Female Fraction"),][paste("y_", year, sep = "")][[1]]
pop_35_female_frac <- inputs_health[which(inputs_health$age == "35-39" & inputs_health$variable == "Female Fraction"),][paste("y_", year, sep = "")][[1]]
pop_40_female_frac <- inputs_health[which(inputs_health$age == "40-44" & inputs_health$variable == "Female Fraction"),][paste("y_", year, sep = "")][[1]]
pop_45_female_frac <- inputs_health[which(inputs_health$age == "45-49" & inputs_health$variable == "Female Fraction"),][paste("y_", year, sep = "")][[1]]
pop_50_female_frac <- inputs_health[which(inputs_health$age == "50-54" & inputs_health$variable == "Female Fraction"),][paste("y_", year, sep = "")][[1]]
pop_55_female_frac <- inputs_health[which(inputs_health$age == "55-59" & inputs_health$variable == "Female Fraction"),][paste("y_", year, sep = "")][[1]]
pop_60_female_frac <- inputs_health[which(inputs_health$age == "60-64" & inputs_health$variable == "Female Fraction"),][paste("y_", year, sep = "")][[1]]
pop_65_female_frac <- inputs_health[which(inputs_health$age == "65-69" & inputs_health$variable == "Female Fraction"),][paste("y_", year, sep = "")][[1]]
pop_70_female_frac <- inputs_health[which(inputs_health$age == "70-74" & inputs_health$variable == "Female Fraction"),][paste("y_", year, sep = "")][[1]]
pop_75_female_frac <- inputs_health[which(inputs_health$age == "75-79" & inputs_health$variable == "Female Fraction"),][paste("y_", year, sep = "")][[1]]
pop_80_female_frac <- inputs_health[which(inputs_health$age == "80-84" & inputs_health$variable == "Female Fraction"),][paste("y_", year, sep = "")][[1]]
pop_85_female_frac <- inputs_health[which(inputs_health$age == "85-89" & inputs_health$variable == "Female Fraction"),][paste("y_", year, sep = "")][[1]]
pop_90_female_frac <- inputs_health[which(inputs_health$age == "90-94" & inputs_health$variable == "Female Fraction"),][paste("y_", year, sep = "")][[1]]
pop_95_female_frac <- inputs_health[which(inputs_health$age == "95-99" & inputs_health$variable == "Female Fraction"),][paste("y_", year, sep = "")][[1]]
pop_100_female_frac <- inputs_health[which(inputs_health$age == "100+" & inputs_health$variable == "Female Fraction"),][paste("y_", year, sep = "")][[1]]

pop_0_male <- population * pop_0_frac * (1-pop_0_female_frac) 
pop_5_male <- population * pop_5_frac  * (1-pop_5_female_frac) 
pop_10_male <- population * pop_10_frac  * (1-pop_10_female_frac) 
pop_15_male <- population * pop_15_frac  * (1-pop_15_female_frac) 
pop_20_male <- population * pop_20_frac  * (1-pop_20_female_frac) 
pop_25_male <- population * pop_25_frac  * (1-pop_25_female_frac) 
pop_30_male <- population * pop_30_frac  * (1-pop_30_female_frac) 
pop_35_male <- population * pop_35_frac  * (1-pop_35_female_frac) 
pop_40_male <- population * pop_40_frac  * (1-pop_40_female_frac) 
pop_45_male <- population * pop_45_frac  * (1-pop_45_female_frac) 
pop_50_male <- population * pop_50_frac  * (1-pop_50_female_frac) 
pop_55_male <- population * pop_55_frac  * (1-pop_55_female_frac) 
pop_60_male <- population * pop_60_frac  * (1-pop_60_female_frac) 
pop_65_male <- population * pop_65_frac  * (1-pop_65_female_frac) 
pop_70_male <- population * pop_70_frac  * (1-pop_70_female_frac) 
pop_75_male <- population * pop_75_frac  * (1-pop_75_female_frac) 
pop_80_male <- population * pop_80_frac  * (1-pop_80_female_frac) 
pop_85_male <- population * pop_85_frac  * (1-pop_85_female_frac) 
pop_90_male <- population * pop_90_frac  * (1-pop_90_female_frac) 
pop_95_male <- population * pop_95_frac  * (1-pop_95_female_frac) 
pop_99_male <- population * pop_100_frac  * (1-pop_100_female_frac) 



pop_0_female <- population * pop_0_frac * (pop_0_female_frac) 
pop_5_female <- population * pop_5_frac  * (pop_5_female_frac) 
pop_10_female <- population * pop_10_frac  * (pop_10_female_frac) 
pop_15_female <- population * pop_15_frac  * (pop_15_female_frac) 
pop_20_female <- population * pop_20_frac  * (pop_20_female_frac) 
pop_25_female <- population * pop_25_frac  * (pop_25_female_frac) 
pop_30_female <- population * pop_30_frac  * (pop_30_female_frac) 
pop_35_female <- population * pop_35_frac  * (pop_35_female_frac) 
pop_40_female <- population * pop_40_frac  * (pop_40_female_frac) 
pop_45_female <- population * pop_45_frac  * (pop_45_female_frac) 
pop_50_female <- population * pop_50_frac  * (pop_50_female_frac) 
pop_55_female <- population * pop_55_frac  * (pop_55_female_frac) 
pop_60_female <- population * pop_60_frac  * (pop_60_female_frac) 
pop_65_female <- population * pop_65_frac  * (pop_65_female_frac) 
pop_70_female <- population * pop_70_frac  * (pop_70_female_frac) 
pop_75_female <- population * pop_75_frac  * (pop_75_female_frac) 
pop_80_female <- population * pop_80_frac  * (pop_80_female_frac) 
pop_85_female <- population * pop_85_frac  * (pop_85_female_frac) 
pop_90_female <- population * pop_90_frac  * (pop_90_female_frac) 
pop_95_female <- population * pop_95_frac  * (pop_95_female_frac) 
pop_99_female <- population * pop_100_frac  * (pop_100_female_frac) 


sex <- c("male", "female")
sex_2 <- c("Male", "Female")



diet_risk <- diet_risks(GDP_per_capita = GDP_per_capita, 
                        GINI = GINI, 
                        cv_requirement = 0.2, 
                        malnutrition_calories = malnutrition_calories, 
                        calories_consumed = sum(c(consumption_waste$kcal_consumed, consumption_waste$kcal_wasted), na.rm = TRUE),
                        fruit_mean = sum(consumption_waste[ which(consumption_waste$Type_2 == "Fruits"),]$g_consumed, na.rm = TRUE), 
                        fruit_tmrel = 250, 
                        fruit_elasticity = 0.20,
                        vegetable_mean = sum(consumption_waste[ which(consumption_waste$Type_2 == "Vegetables"),]$g_consumed, na.rm = TRUE), 
                        vegetable_tmrel = 360,
                        vegetable_elasticity = 1E-50,
                        legume_mean = sum(consumption_waste[ which(consumption_waste$Type_2 == "Pulses"),]$g_consumed, na.rm = TRUE), 
                        legume_tmrel = 60, 
                        legume_elasticity = 1E-50,
                        grain_mean = sum(consumption_waste[ which(consumption_waste$Type_2 == "Cereals"),]$g_consumed, na.rm = TRUE), 
                        grain_tmrel = 125, 
                        grain_elasticity = 0.22, ##Need to distinguish refined vs whole grains
                        milk_mean = sum(consumption_waste[ which(consumption_waste$species == "dairy"),]$g_consumed, na.rm = TRUE), 
                        milk_tmrel = 435, 
                        milk_elasticity = 0.2,
                        meat_mean = sum(consumption_waste[ which(consumption_waste$species == "cattle" | consumption_waste$species == "pig" | consumption_waste$species == "sheep"),]$g_consumed, na.rm = TRUE), 
                        meat_tmrel = 23,
                        meat_elasticity = 0.27,
                        alpha_max = as.numeric(inputs_meat[which(inputs_meat$variable == "alpha_max"),][paste("y_", year, sep = "")]), 
                        alpha_min = as.numeric(inputs_meat[which(inputs_meat$variable == "alpha_min"),][paste("y_", year, sep = "")]))## Also considering changing cutoffs for consumption. 



##Health impacts
diet_health_summary <- data.frame(country = numeric(0), year = numeric(0), risk = numeric(0), disease = numeric(0), age = numeric(0), sex = numeric(0), mean = numeric(0), median = numeric(0), p2.5 = numeric(0), p97.5 = numeric(0))


##Overweight
bmi_risks <- inputs_risk[ which(inputs_risk$risk == "High body-mass index in adults"),]
bmi_diseases <- as.character(as.data.frame(table(as.character(bmi_risks$disease)))$Var1)

for(i in seq(from=1, to= 2, by=1)) {

for(d in seq(from=1, to= length(bmi_diseases), by=1)) {  

for(age in seq(from=6, to= length(ages), by=1)) {
  
  RR = bmi_risks[(bmi_risks$age == ages[age] & bmi_risks$disease == bmi_diseases[d] & bmi_risks$sex == sex_2[i] & bmi_risks$variable == "Relative Risk"),][paste("y_", year, sep = "")][[1]]
  RR_2.5 = bmi_risks[(bmi_risks$age == ages[age] & bmi_risks$disease == bmi_diseases[d] & bmi_risks$sex == sex_2[i] & bmi_risks$variable == "Relative Risk 2.5"),][paste("y_", year, sep = "")][[1]]
  RR_97.5 = bmi_risks[(bmi_risks$age == ages[age] & bmi_risks$disease == bmi_diseases[d] & bmi_risks$sex == sex_2[i] & bmi_risks$variable == "Relative Risk 97.5"),][paste("y_", year, sep = "")][[1]]
  tmrel = bmi_risks[(bmi_risks$age == ages[age] & bmi_risks$disease == bmi_diseases[d] & bmi_risks$sex == sex_2[i] & bmi_risks$variable == "Relative Risk"),]$tmrel
  increment = bmi_risks[(bmi_risks$age == ages[age] & bmi_risks$disease == bmi_diseases[d] & bmi_risks$sex == sex_2[i] & bmi_risks$variable == "Relative Risk"),]$increment
  exposure = 27.5
  baseline_mortality = inputs_health[(inputs_health$age == ages[age] & inputs_health$disease == bmi_diseases[d] & inputs_health$sex == sex_2[i]),][paste("y_", year, sep = "")][[1]]
  
  overweight_impacts <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$prop_overweight- diet_risk$prop_obese)/100, RR = (1+((RR-1)/increment)*(exposure-tmrel)), RR_95min = (1+((RR_2.5-1)/increment)*(exposure-tmrel)), RR_95max = (1+((RR_97.5-1)/increment)*(exposure-tmrel)), bmort = baseline_mortality, n_it = 1000)
  
  diet_health_summary[(length(diet_health_summary$country)+1),]$risk <- "overweight"
  diet_health_summary[(length(diet_health_summary$country)),]$disease <- bmi_diseases[d]
  diet_health_summary[(length(diet_health_summary$country)),]$age <- ages[age]
  diet_health_summary[(length(diet_health_summary$country)),]$sex <- sex[i]
  diet_health_summary[(length(diet_health_summary$country)),]$mean <-  overweight_impacts$deaths_mean
  diet_health_summary[(length(diet_health_summary$country)),]$median <-  overweight_impacts$deaths_median
  diet_health_summary[(length(diet_health_summary$country)),]$p2.5 <-  overweight_impacts$deaths_2.5
  diet_health_summary[(length(diet_health_summary$country)),]$p97.5 <-  overweight_impacts$deaths_97.5
  
}
}
}


##Obese
for(i in seq(from=1, to= 2, by=1)) {
  
  for(d in seq(from=1, to= length(bmi_diseases), by=1)) {  
    
    for(age in seq(from=6, to= length(ages), by=1)) {
      
      RR = bmi_risks[(bmi_risks$age == ages[age] & bmi_risks$disease == bmi_diseases[d] & bmi_risks$sex == sex_2[i] & bmi_risks$variable == "Relative Risk"),][paste("y_", year, sep = "")][[1]]
      RR_2.5 = bmi_risks[(bmi_risks$age == ages[age] & bmi_risks$disease == bmi_diseases[d] & bmi_risks$sex == sex_2[i] & bmi_risks$variable == "Relative Risk 2.5"),][paste("y_", year, sep = "")][[1]]
      RR_97.5 = bmi_risks[(bmi_risks$age == ages[age] & bmi_risks$disease == bmi_diseases[d] & bmi_risks$sex == sex_2[i] & bmi_risks$variable == "Relative Risk 97.5"),][paste("y_", year, sep = "")][[1]]
      tmrel = bmi_risks[(bmi_risks$age == ages[age] & bmi_risks$disease == bmi_diseases[d] & bmi_risks$sex == sex_2[i] & bmi_risks$variable == "Relative Risk"),]$tmrel
      increment = bmi_risks[(bmi_risks$age == ages[age] & bmi_risks$disease == bmi_diseases[d] & bmi_risks$sex == sex_2[i] & bmi_risks$variable == "Relative Risk"),]$increment
      exposure = 32.5
      baseline_mortality = inputs_health[(inputs_health$age == ages[age] & inputs_health$disease == bmi_diseases[d] & inputs_health$sex == sex_2[i]),][paste("y_", year, sep = "")][[1]]
      
      overweight_impacts <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$prop_obese)/100, RR = (1+((RR-1)/increment)*(exposure-tmrel)), RR_95min = (1+((RR_2.5-1)/increment)*(exposure-tmrel)), RR_95max = (1+((RR_97.5-1)/increment)*(exposure-tmrel)), bmort = baseline_mortality, n_it = 1000)
      
      diet_health_summary[(length(diet_health_summary$country)+1),]$risk <- "obese"
      diet_health_summary[(length(diet_health_summary$country)),]$disease <- bmi_diseases[d]
      diet_health_summary[(length(diet_health_summary$country)),]$age <- ages[age]
      diet_health_summary[(length(diet_health_summary$country)),]$sex <- sex[i]
      diet_health_summary[(length(diet_health_summary$country)),]$mean <-  overweight_impacts$deaths_mean
      diet_health_summary[(length(diet_health_summary$country)),]$median <-  overweight_impacts$deaths_median
      diet_health_summary[(length(diet_health_summary$country)),]$p2.5 <-  overweight_impacts$deaths_2.5
      diet_health_summary[(length(diet_health_summary$country)),]$p97.5 <-  overweight_impacts$deaths_97.5
      
    }
  }
}


##Child stunting - all-cause mortality
for(i in seq(from=1, to= 2, by=1)) {
  
  ##Moderate stunting
  RR = inputs_risk[(inputs_risk$risk == "moderate child stunting" & inputs_risk$age == ages[1] & inputs_risk$disease == "All causes" & inputs_risk$sex == sex_2[i] & inputs_risk$variable == "Relative Risk"),][paste("y_", year, sep = "")][[1]]
  RR_2.5 = inputs_risk[(inputs_risk$risk == "moderate child stunting" & inputs_risk$age == ages[1] & inputs_risk$disease == "All causes" & inputs_risk$sex == sex_2[i] & inputs_risk$variable == "Relative Risk 2.5"),][paste("y_", year, sep = "")][[1]]
  RR_97.5 = inputs_risk[(inputs_risk$risk == "moderate child stunting" & inputs_risk$age == ages[1] & inputs_risk$disease == "All causes" & inputs_risk$sex == sex_2[i] & inputs_risk$variable == "Relative Risk 97.5"),][paste("y_", year, sep = "")][[1]]

  baseline_mortality = inputs_health[(inputs_health$age == ages[1] & inputs_health$disease == "All causes" & inputs_health$sex == sex_2[i]),][paste("y_", year, sep = "")][[1]]
  
  moderate_stunting <- diet_hia(pop = get(paste("pop_0_", sex[i], sep = ""))*(diet_risk$prop_mod_stunting - diet_risk$prop_severe_stunting), RR = RR, RR_95min = RR_2.5, RR_95max = RR_97.5, bmort = baseline_mortality, n_it = 1000)
  
  diet_health_summary[(length(diet_health_summary$country)+1),]$risk <- "moderate child stunting"
  diet_health_summary[(length(diet_health_summary$country)),]$disease <- "Total all causes"
  diet_health_summary[(length(diet_health_summary$country)),]$age <- ages[1]
  diet_health_summary[(length(diet_health_summary$country)),]$sex <- sex[i]
  diet_health_summary[(length(diet_health_summary$country)),]$mean <-  moderate_stunting$deaths_mean
  diet_health_summary[(length(diet_health_summary$country)),]$median <-  moderate_stunting$deaths_median
  diet_health_summary[(length(diet_health_summary$country)),]$p2.5 <-  moderate_stunting$deaths_2.5
  diet_health_summary[(length(diet_health_summary$country)),]$p97.5 <-  moderate_stunting$deaths_97.5
  
  ##Severe stunting
  RR = inputs_risk[(inputs_risk$risk == "severe child stunting" & inputs_risk$age == ages[1] & inputs_risk$disease == "All causes" & inputs_risk$sex == sex_2[i] & inputs_risk$variable == "Relative Risk"),][paste("y_", year, sep = "")][[1]]
  RR_2.5 = inputs_risk[(inputs_risk$risk == "severe child stunting" & inputs_risk$age == ages[1] & inputs_risk$disease == "All causes" & inputs_risk$sex == sex_2[i] & inputs_risk$variable == "Relative Risk 2.5"),][paste("y_", year, sep = "")][[1]]
  RR_97.5 = inputs_risk[(inputs_risk$risk == "severe child stunting" & inputs_risk$age == ages[1] & inputs_risk$disease == "All causes" & inputs_risk$sex == sex_2[i] & inputs_risk$variable == "Relative Risk 97.5"),][paste("y_", year, sep = "")][[1]]
  
  baseline_mortality = inputs_health[(inputs_health$age == ages[1] & inputs_health$disease == "All causes" & inputs_health$sex == sex_2[i]),][paste("y_", year, sep = "")][[1]]
  
  severe_stunting <- diet_hia(pop = get(paste("pop_0_", sex[i], sep = ""))*(diet_risk$prop_severe_stunting), RR = RR, RR_95min = RR_2.5, RR_95max = RR_97.5, bmort = baseline_mortality, n_it = 1000)
  
  diet_health_summary[(length(diet_health_summary$country)+1),]$risk <- "severe child stunting"
  diet_health_summary[(length(diet_health_summary$country)),]$disease <- "Total all causes"
  diet_health_summary[(length(diet_health_summary$country)),]$age <- ages[1]
  diet_health_summary[(length(diet_health_summary$country)),]$sex <- sex[i]
  diet_health_summary[(length(diet_health_summary$country)),]$mean <-  severe_stunting$deaths_mean
  diet_health_summary[(length(diet_health_summary$country)),]$median <-  severe_stunting$deaths_median
  diet_health_summary[(length(diet_health_summary$country)),]$p2.5 <-  severe_stunting$deaths_2.5
  diet_health_summary[(length(diet_health_summary$country)),]$p97.5 <-  severe_stunting$deaths_97.5
  
  
}



##Neonatal deaths - maternal malnutrition
##Blossner 2005 Maternal Malnutirtion methodology
crude_birth_rate <- inputs_health[(inputs_health$variable == "Crude birth rate"),][paste("y_", year, sep = "")][[1]]
lbw_prop <- inputs_health[(inputs_health$variable == "LBW prop"),][paste("y_", year, sep = "")][[1]]
rr_iugr_mm <- inputs_risk[(inputs_risk$risk == "IUGR due to low BMI" & inputs_risk$variable == "Relative Risk"),][paste("y_", year, sep = "")][[1]]

iugr_lbw_births_prop <- -3.2452 + 0.8528 * lbw_prop * 100 ##Section 3.6 Page 21 Blossner 2005
iugr_lbw_births <- iugr_lbw_births_prop/100 * crude_birth_rate * population

attributable_fraction_iugr_mm <- (rr_iugr_mm - 1) / rr_iugr_mm ##mm = maternal malnutrition rr should be 1.8

iugr_lbw_births_mm_prop <- attributable_fraction_iugr_mm * diet_risk$prop_malnourised
iugr_lbw_births_mm <- iugr_lbw_births_mm_prop * iugr_lbw_births

RR = inputs_risk[(inputs_risk$risk == "neonatal mortality" & inputs_risk$age == ages[1] & inputs_risk$disease == "All causes" & inputs_risk$variable == "Relative Risk"),][paste("y_", year, sep = "")][[1]]
RR_2.5 = inputs_risk[(inputs_risk$risk == "neonatal mortality" & inputs_risk$age == ages[1] & inputs_risk$disease == "All causes" & inputs_risk$variable == "Relative Risk 2.5"),][paste("y_", year, sep = "")][[1]]
RR_97.5 = inputs_risk[(inputs_risk$risk == "neonatal mortality" & inputs_risk$age == ages[1] & inputs_risk$disease == "All causes" & inputs_risk$variable == "Relative Risk 97.5"),][paste("y_", year, sep = "")][[1]]

baseline_mortality = inputs_health[(inputs_health$variable == "Mortality Rate" & inputs_health$age == "Neonatal"),][paste("y_", year, sep = "")][[1]]

neonatal_deaths <- diet_hia(pop = iugr_lbw_births_mm, RR = RR, RR_95min = RR_2.5, RR_95max = RR_97.5, bmort = baseline_mortality, n_it = 1000)

diet_health_summary[(length(diet_health_summary$country)+1),]$risk <- "Maternal malnutrition"
diet_health_summary[(length(diet_health_summary$country)),]$disease <- "All causes"
diet_health_summary[(length(diet_health_summary$country)),]$age <- "neonatal"
diet_health_summary[(length(diet_health_summary$country)),]$sex <- "Both"
diet_health_summary[(length(diet_health_summary$country)),]$mean <-  neonatal_deaths$deaths_mean
diet_health_summary[(length(diet_health_summary$country)),]$median <-  neonatal_deaths$deaths_median
diet_health_summary[(length(diet_health_summary$country)),]$p2.5 <-  neonatal_deaths$deaths_2.5
diet_health_summary[(length(diet_health_summary$country)),]$p97.5 <-  neonatal_deaths$deaths_97.5






#Fruits
fruit_risks <- inputs_risk[ which(inputs_risk$risk == "Diet low in fruits"),]
fruit_diseases <- as.character(as.data.frame(table(as.character(fruit_risks$disease)))$Var1)

for(i in seq(from=1, to= 2, by=1)) {
  
  for(d in seq(from=1, to= length(fruit_diseases), by=1)) {  
    
    for(age in seq(from=6, to= length(ages), by=1)) {
      
      RR = fruit_risks[(fruit_risks$age == ages[age] & fruit_risks$disease == fruit_diseases[d] & fruit_risks$sex == sex_2[i] & fruit_risks$variable == "Relative Risk"),][paste("y_", year, sep = "")][[1]]
      RR_2.5 = fruit_risks[(fruit_risks$age == ages[age] & fruit_risks$disease == fruit_diseases[d] & fruit_risks$sex == sex_2[i] & fruit_risks$variable == "Relative Risk 2.5"),][paste("y_", year, sep = "")][[1]]
      RR_97.5 = fruit_risks[(fruit_risks$age == ages[age] & fruit_risks$disease == fruit_diseases[d] & fruit_risks$sex == sex_2[i] & fruit_risks$variable == "Relative Risk 97.5"),][paste("y_", year, sep = "")][[1]]
      tmrel = fruit_risks[(fruit_risks$age == ages[age] & fruit_risks$disease == fruit_diseases[d] & fruit_risks$sex == sex_2[i] & fruit_risks$variable == "Relative Risk"),]$tmrel
      increment = fruit_risks[(fruit_risks$age == ages[age] & fruit_risks$disease == fruit_diseases[d] & fruit_risks$sex == sex_2[i] & fruit_risks$variable == "Relative Risk"),]$increment
      exposure = 0
      baseline_mortality = inputs_health[(inputs_health$age == ages[age] & inputs_health$disease == fruit_diseases[d] & inputs_health$sex == sex_2[i]),][paste("y_", year, sep = "")][[1]]
      
      if(RR == 0){RR <- 1}
      if(RR_2.5 == 0){RR_2.5 <- 1}
      if(RR_97.5 == 0){RR_97.5 <- 1}
      
      fruit_impacts_0_tmrel   <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$fruit_prop_belowtmrel_0.1 - diet_risk$fruit_prop_belowtmrel_0), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.05)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.05)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.05)), bmort = baseline_mortality, n_it = 1000)
      fruit_impacts_0.1_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$fruit_prop_belowtmrel_0.2 - diet_risk$fruit_prop_belowtmrel_0.1), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.15)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.15)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.15)), bmort = baseline_mortality, n_it = 1000)
      fruit_impacts_0.2_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$fruit_prop_belowtmrel_0.3 - diet_risk$fruit_prop_belowtmrel_0.2), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.25)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.25)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.25)), bmort = baseline_mortality, n_it = 1000)
      fruit_impacts_0.3_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$fruit_prop_belowtmrel_0.4 - diet_risk$fruit_prop_belowtmrel_0.3), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.35)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.35)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.35)), bmort = baseline_mortality, n_it = 1000)
      fruit_impacts_0.4_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$fruit_prop_belowtmrel_0.5 - diet_risk$fruit_prop_belowtmrel_0.4), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.45)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.45)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.45)), bmort = baseline_mortality, n_it = 1000)
      fruit_impacts_0.5_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$fruit_prop_belowtmrel_0.6 - diet_risk$fruit_prop_belowtmrel_0.5), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.55)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.55)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.55)), bmort = baseline_mortality, n_it = 1000)
      fruit_impacts_0.6_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$fruit_prop_belowtmrel_0.7 - diet_risk$fruit_prop_belowtmrel_0.6), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.65)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.65)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.65)), bmort = baseline_mortality, n_it = 1000)
      fruit_impacts_0.7_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$fruit_prop_belowtmrel_0.8 - diet_risk$fruit_prop_belowtmrel_0.7), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.75)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.75)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.75)), bmort = baseline_mortality, n_it = 1000)
      fruit_impacts_0.8_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$fruit_prop_belowtmrel_0.9 - diet_risk$fruit_prop_belowtmrel_0.8), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.85)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.85)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.85)), bmort = baseline_mortality, n_it = 1000)
      fruit_impacts_0.9_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$fruit_prop_belowtmrel - diet_risk$fruit_prop_belowtmrel_0.9), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.95)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.95)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.95)), bmort = baseline_mortality, n_it = 1000)
      
      fruit_mean <- sum(fruit_impacts_0_tmrel$deaths_mean, fruit_impacts_0.1_tmrel$deaths_mean, fruit_impacts_0.2_tmrel$deaths_mean, fruit_impacts_0.3_tmrel$deaths_mean, fruit_impacts_0.4_tmrel$deaths_mean, fruit_impacts_0.5_tmrel$deaths_mean, 
                        fruit_impacts_0.6_tmrel$deaths_mean, fruit_impacts_0.7_tmrel$deaths_mean, fruit_impacts_0.8_tmrel$deaths_mean, fruit_impacts_0.9_tmrel$deaths_mean)
      
      fruit_median <- sum(fruit_impacts_0_tmrel$deaths_median, fruit_impacts_0.1_tmrel$deaths_median, fruit_impacts_0.2_tmrel$deaths_median, fruit_impacts_0.3_tmrel$deaths_median, fruit_impacts_0.4_tmrel$deaths_median, fruit_impacts_0.5_tmrel$deaths_median, 
                          fruit_impacts_0.6_tmrel$deaths_median, fruit_impacts_0.7_tmrel$deaths_median, fruit_impacts_0.8_tmrel$deaths_median, fruit_impacts_0.9_tmrel$deaths_median)
      
      fruit_error_1 <- sqrt((fruit_impacts_0_tmrel$deaths_97.5 - fruit_impacts_0_tmrel$deaths_2.5)^2 + (fruit_impacts_0.1_tmrel$deaths_97.5 - fruit_impacts_0.1_tmrel$deaths_2.5)^2 +
                              (fruit_impacts_0.2_tmrel$deaths_97.5 - fruit_impacts_0.2_tmrel$deaths_2.5)^2 + (fruit_impacts_0.3_tmrel$deaths_97.5 - fruit_impacts_0.3_tmrel$deaths_2.5)^2 +
                              (fruit_impacts_0.4_tmrel$deaths_97.5 - fruit_impacts_0.4_tmrel$deaths_2.5)^2 + (fruit_impacts_0.5_tmrel$deaths_97.5 - fruit_impacts_0.5_tmrel$deaths_2.5)^2 +
                              (fruit_impacts_0.6_tmrel$deaths_97.5 - fruit_impacts_0.6_tmrel$deaths_2.5)^2 + (fruit_impacts_0.7_tmrel$deaths_97.5 - fruit_impacts_0.7_tmrel$deaths_2.5)^2 +
                              (fruit_impacts_0.8_tmrel$deaths_97.5 - fruit_impacts_0.8_tmrel$deaths_2.5)^2 + (fruit_impacts_0.9_tmrel$deaths_97.5 - fruit_impacts_0.9_tmrel$deaths_2.5)^2)##absolute error (use for now but see whether propagation gives realistic results)
      
      fruit_error_2 <- sqrt(sum((((fruit_impacts_0_tmrel$deaths_97.5 - fruit_impacts_0_tmrel$deaths_2.5) /2 / fruit_impacts_0_tmrel$deaths_median*100)^2),  
                                (((fruit_impacts_0.1_tmrel$deaths_97.5 - fruit_impacts_0.1_tmrel$deaths_2.5) /2 / fruit_impacts_0.1_tmrel$deaths_median*100)^2), 
                                (((fruit_impacts_0.2_tmrel$deaths_97.5 - fruit_impacts_0.2_tmrel$deaths_2.5) /2 / fruit_impacts_0.2_tmrel$deaths_median*100)^2), 
                                (((fruit_impacts_0.3_tmrel$deaths_97.5 - fruit_impacts_0.3_tmrel$deaths_2.5) /2 / fruit_impacts_0.3_tmrel$deaths_median*100)^2), 
                                (((fruit_impacts_0.4_tmrel$deaths_97.5 - fruit_impacts_0.4_tmrel$deaths_2.5) /2 / fruit_impacts_0.4_tmrel$deaths_median*100)^2), 
                                (((fruit_impacts_0.5_tmrel$deaths_97.5 - fruit_impacts_0.5_tmrel$deaths_2.5) /2 / fruit_impacts_0.5_tmrel$deaths_median*100)^2), 
                                (((fruit_impacts_0.6_tmrel$deaths_97.5 - fruit_impacts_0.6_tmrel$deaths_2.5) /2 / fruit_impacts_0.6_tmrel$deaths_median*100)^2), 
                                (((fruit_impacts_0.7_tmrel$deaths_97.5 - fruit_impacts_0.7_tmrel$deaths_2.5) /2 / fruit_impacts_0.7_tmrel$deaths_median*100)^2), 
                                (((fruit_impacts_0.8_tmrel$deaths_97.5 - fruit_impacts_0.8_tmrel$deaths_2.5) /2 / fruit_impacts_0.8_tmrel$deaths_median*100)^2), 
                                (((fruit_impacts_0.9_tmrel$deaths_97.5 - fruit_impacts_0.9_tmrel$deaths_2.5) /2 / fruit_impacts_0.9_tmrel$deaths_median*100)^2), na.rm = TRUE)) ##percent error
      
      
      
      diet_health_summary[(length(diet_health_summary$country)+1),]$risk <- "Diet low in fruits"
      diet_health_summary[(length(diet_health_summary$country)),]$disease <- fruit_diseases[d]
      diet_health_summary[(length(diet_health_summary$country)),]$age <- ages[age]
      diet_health_summary[(length(diet_health_summary$country)),]$sex <- sex[i]
      diet_health_summary[(length(diet_health_summary$country)),]$mean <-  fruit_mean
      diet_health_summary[(length(diet_health_summary$country)),]$median <-  fruit_median
      diet_health_summary[(length(diet_health_summary$country)),]$p2.5 <-  fruit_median - fruit_error_1/2
      diet_health_summary[(length(diet_health_summary$country)),]$p97.5 <-  fruit_median + fruit_error_1/2
      
      
      
    }
  }
}




##Vegetables
vegetable_risks <- inputs_risk[ which(inputs_risk$risk == "Diet low in vegetables"),]
vegetable_diseases <- as.character(as.data.frame(table(as.character(vegetable_risks$disease)))$Var1)

for(i in seq(from=1, to= 2, by=1)) {
  
  for(d in seq(from=1, to= length(vegetable_diseases), by=1)) {  
    
    for(age in seq(from=6, to= length(ages), by=1)) {
      
      RR = vegetable_risks[(vegetable_risks$age == ages[age] & vegetable_risks$disease == vegetable_diseases[d] & vegetable_risks$sex == sex_2[i] & vegetable_risks$variable == "Relative Risk"),][paste("y_", year, sep = "")][[1]]
      RR_2.5 = vegetable_risks[(vegetable_risks$age == ages[age] & vegetable_risks$disease == vegetable_diseases[d] & vegetable_risks$sex == sex_2[i] & vegetable_risks$variable == "Relative Risk 2.5"),][paste("y_", year, sep = "")][[1]]
      RR_97.5 = vegetable_risks[(vegetable_risks$age == ages[age] & vegetable_risks$disease == vegetable_diseases[d] & vegetable_risks$sex == sex_2[i] & vegetable_risks$variable == "Relative Risk 97.5"),][paste("y_", year, sep = "")][[1]]
      tmrel = vegetable_risks[(vegetable_risks$age == ages[age] & vegetable_risks$disease == vegetable_diseases[d] & vegetable_risks$sex == sex_2[i] & vegetable_risks$variable == "Relative Risk"),]$tmrel
      increment = vegetable_risks[(vegetable_risks$age == ages[age] & vegetable_risks$disease == vegetable_diseases[d] & vegetable_risks$sex == sex_2[i] & vegetable_risks$variable == "Relative Risk"),]$increment
      exposure = 0
      baseline_mortality = inputs_health[(inputs_health$age == ages[age] & inputs_health$disease == vegetable_diseases[d] & inputs_health$sex == sex_2[i]),][paste("y_", year, sep = "")][[1]]
      
      if(RR == 0){RR <- 1}
      if(RR_2.5 == 0){RR_2.5 <- 1}
      if(RR_97.5 == 0){RR_97.5 <- 1}
      
      vegetable_impacts_0_tmrel   <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$vegetable_prop_belowtmrel_0.1 - diet_risk$vegetable_prop_belowtmrel_0), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.05)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.05)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.05)), bmort = baseline_mortality, n_it = 1000)
      vegetable_impacts_0.1_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$vegetable_prop_belowtmrel_0.2 - diet_risk$vegetable_prop_belowtmrel_0.1), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.15)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.15)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.15)), bmort = baseline_mortality, n_it = 1000)
      vegetable_impacts_0.2_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$vegetable_prop_belowtmrel_0.3 - diet_risk$vegetable_prop_belowtmrel_0.2), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.25)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.25)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.25)), bmort = baseline_mortality, n_it = 1000)
      vegetable_impacts_0.3_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$vegetable_prop_belowtmrel_0.4 - diet_risk$vegetable_prop_belowtmrel_0.3), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.35)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.35)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.35)), bmort = baseline_mortality, n_it = 1000)
      vegetable_impacts_0.4_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$vegetable_prop_belowtmrel_0.5 - diet_risk$vegetable_prop_belowtmrel_0.4), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.45)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.45)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.45)), bmort = baseline_mortality, n_it = 1000)
      vegetable_impacts_0.5_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$vegetable_prop_belowtmrel_0.6 - diet_risk$vegetable_prop_belowtmrel_0.5), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.55)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.55)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.55)), bmort = baseline_mortality, n_it = 1000)
      vegetable_impacts_0.6_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$vegetable_prop_belowtmrel_0.7 - diet_risk$vegetable_prop_belowtmrel_0.6), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.65)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.65)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.65)), bmort = baseline_mortality, n_it = 1000)
      vegetable_impacts_0.7_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$vegetable_prop_belowtmrel_0.8 - diet_risk$vegetable_prop_belowtmrel_0.7), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.75)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.75)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.75)), bmort = baseline_mortality, n_it = 1000)
      vegetable_impacts_0.8_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$vegetable_prop_belowtmrel_0.9 - diet_risk$vegetable_prop_belowtmrel_0.8), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.85)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.85)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.85)), bmort = baseline_mortality, n_it = 1000)
      vegetable_impacts_0.9_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$vegetable_prop_belowtmrel - diet_risk$vegetable_prop_belowtmrel_0.9), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.95)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.95)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.95)), bmort = baseline_mortality, n_it = 1000)
      
      vegetable_mean <- sum(vegetable_impacts_0_tmrel$deaths_mean, vegetable_impacts_0.1_tmrel$deaths_mean, vegetable_impacts_0.2_tmrel$deaths_mean, vegetable_impacts_0.3_tmrel$deaths_mean, vegetable_impacts_0.4_tmrel$deaths_mean, vegetable_impacts_0.5_tmrel$deaths_mean, 
                        vegetable_impacts_0.6_tmrel$deaths_mean, vegetable_impacts_0.7_tmrel$deaths_mean, vegetable_impacts_0.8_tmrel$deaths_mean, vegetable_impacts_0.9_tmrel$deaths_mean)
      
      vegetable_median <- sum(vegetable_impacts_0_tmrel$deaths_median, vegetable_impacts_0.1_tmrel$deaths_median, vegetable_impacts_0.2_tmrel$deaths_median, vegetable_impacts_0.3_tmrel$deaths_median, vegetable_impacts_0.4_tmrel$deaths_median, vegetable_impacts_0.5_tmrel$deaths_median, 
                          vegetable_impacts_0.6_tmrel$deaths_median, vegetable_impacts_0.7_tmrel$deaths_median, vegetable_impacts_0.8_tmrel$deaths_median, vegetable_impacts_0.9_tmrel$deaths_median)
      
      vegetable_error_1 <- sqrt((vegetable_impacts_0_tmrel$deaths_97.5 - vegetable_impacts_0_tmrel$deaths_2.5)^2 + (vegetable_impacts_0.1_tmrel$deaths_97.5 - vegetable_impacts_0.1_tmrel$deaths_2.5)^2 +
                              (vegetable_impacts_0.2_tmrel$deaths_97.5 - vegetable_impacts_0.2_tmrel$deaths_2.5)^2 + (vegetable_impacts_0.3_tmrel$deaths_97.5 - vegetable_impacts_0.3_tmrel$deaths_2.5)^2 +
                              (vegetable_impacts_0.4_tmrel$deaths_97.5 - vegetable_impacts_0.4_tmrel$deaths_2.5)^2 + (vegetable_impacts_0.5_tmrel$deaths_97.5 - vegetable_impacts_0.5_tmrel$deaths_2.5)^2 +
                              (vegetable_impacts_0.6_tmrel$deaths_97.5 - vegetable_impacts_0.6_tmrel$deaths_2.5)^2 + (vegetable_impacts_0.7_tmrel$deaths_97.5 - vegetable_impacts_0.7_tmrel$deaths_2.5)^2 +
                              (vegetable_impacts_0.8_tmrel$deaths_97.5 - vegetable_impacts_0.8_tmrel$deaths_2.5)^2 + (vegetable_impacts_0.9_tmrel$deaths_97.5 - vegetable_impacts_0.9_tmrel$deaths_2.5)^2)##absolute error (use for now but see whether propagation gives realistic results)
      
      vegetable_error_2 <- sqrt(sum((((vegetable_impacts_0_tmrel$deaths_97.5 - vegetable_impacts_0_tmrel$deaths_2.5) /2 / vegetable_impacts_0_tmrel$deaths_median*100)^2),  
                                (((vegetable_impacts_0.1_tmrel$deaths_97.5 - vegetable_impacts_0.1_tmrel$deaths_2.5) /2 / vegetable_impacts_0.1_tmrel$deaths_median*100)^2), 
                                (((vegetable_impacts_0.2_tmrel$deaths_97.5 - vegetable_impacts_0.2_tmrel$deaths_2.5) /2 / vegetable_impacts_0.2_tmrel$deaths_median*100)^2), 
                                (((vegetable_impacts_0.3_tmrel$deaths_97.5 - vegetable_impacts_0.3_tmrel$deaths_2.5) /2 / vegetable_impacts_0.3_tmrel$deaths_median*100)^2), 
                                (((vegetable_impacts_0.4_tmrel$deaths_97.5 - vegetable_impacts_0.4_tmrel$deaths_2.5) /2 / vegetable_impacts_0.4_tmrel$deaths_median*100)^2), 
                                (((vegetable_impacts_0.5_tmrel$deaths_97.5 - vegetable_impacts_0.5_tmrel$deaths_2.5) /2 / vegetable_impacts_0.5_tmrel$deaths_median*100)^2), 
                                (((vegetable_impacts_0.6_tmrel$deaths_97.5 - vegetable_impacts_0.6_tmrel$deaths_2.5) /2 / vegetable_impacts_0.6_tmrel$deaths_median*100)^2), 
                                (((vegetable_impacts_0.7_tmrel$deaths_97.5 - vegetable_impacts_0.7_tmrel$deaths_2.5) /2 / vegetable_impacts_0.7_tmrel$deaths_median*100)^2), 
                                (((vegetable_impacts_0.8_tmrel$deaths_97.5 - vegetable_impacts_0.8_tmrel$deaths_2.5) /2 / vegetable_impacts_0.8_tmrel$deaths_median*100)^2), 
                                (((vegetable_impacts_0.9_tmrel$deaths_97.5 - vegetable_impacts_0.9_tmrel$deaths_2.5) /2 / vegetable_impacts_0.9_tmrel$deaths_median*100)^2), na.rm = TRUE)) ##percent error
      
      
      
      diet_health_summary[(length(diet_health_summary$country)+1),]$risk <- "Diet low in vegetables"
      diet_health_summary[(length(diet_health_summary$country)),]$disease <- vegetable_diseases[d]
      diet_health_summary[(length(diet_health_summary$country)),]$age <- ages[age]
      diet_health_summary[(length(diet_health_summary$country)),]$sex <- sex[i]
      diet_health_summary[(length(diet_health_summary$country)),]$mean <-  vegetable_mean
      diet_health_summary[(length(diet_health_summary$country)),]$median <-  vegetable_median
      diet_health_summary[(length(diet_health_summary$country)),]$p2.5 <-  vegetable_median - vegetable_error_1/2
      diet_health_summary[(length(diet_health_summary$country)),]$p97.5 <-  vegetable_median + vegetable_error_1/2
      
      
      
    }
  }
}



##Legumes
legume_risks <- inputs_risk[ which(inputs_risk$risk == "Diet low in legumes"),]
legume_diseases <- as.character(as.data.frame(table(as.character(legume_risks$disease)))$Var1)

for(i in seq(from=1, to= 2, by=1)) {
  
  for(d in seq(from=1, to= length(legume_diseases), by=1)) {  
    
    for(age in seq(from=6, to= length(ages), by=1)) {
      
      RR = legume_risks[(legume_risks$age == ages[age] & legume_risks$disease == legume_diseases[d] & legume_risks$sex == sex_2[i] & legume_risks$variable == "Relative Risk"),][paste("y_", year, sep = "")][[1]]
      RR_2.5 = legume_risks[(legume_risks$age == ages[age] & legume_risks$disease == legume_diseases[d] & legume_risks$sex == sex_2[i] & legume_risks$variable == "Relative Risk 2.5"),][paste("y_", year, sep = "")][[1]]
      RR_97.5 = legume_risks[(legume_risks$age == ages[age] & legume_risks$disease == legume_diseases[d] & legume_risks$sex == sex_2[i] & legume_risks$variable == "Relative Risk 97.5"),][paste("y_", year, sep = "")][[1]]
      tmrel = legume_risks[(legume_risks$age == ages[age] & legume_risks$disease == legume_diseases[d] & legume_risks$sex == sex_2[i] & legume_risks$variable == "Relative Risk"),]$tmrel
      increment = legume_risks[(legume_risks$age == ages[age] & legume_risks$disease == legume_diseases[d] & legume_risks$sex == sex_2[i] & legume_risks$variable == "Relative Risk"),]$increment
      exposure = 0
      baseline_mortality = inputs_health[(inputs_health$age == ages[age] & inputs_health$disease == legume_diseases[d] & inputs_health$sex == sex_2[i]),][paste("y_", year, sep = "")][[1]]
      
      if(RR == 0){RR <- 1}
      if(RR_2.5 == 0){RR_2.5 <- 1}
      if(RR_97.5 == 0){RR_97.5 <- 1}
      
      legume_impacts_0_tmrel   <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$legume_prop_belowtmrel_0.1 - diet_risk$legume_prop_belowtmrel_0), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.05)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.05)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.05)), bmort = baseline_mortality, n_it = 1000)
      legume_impacts_0.1_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$legume_prop_belowtmrel_0.2 - diet_risk$legume_prop_belowtmrel_0.1), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.15)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.15)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.15)), bmort = baseline_mortality, n_it = 1000)
      legume_impacts_0.2_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$legume_prop_belowtmrel_0.3 - diet_risk$legume_prop_belowtmrel_0.2), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.25)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.25)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.25)), bmort = baseline_mortality, n_it = 1000)
      legume_impacts_0.3_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$legume_prop_belowtmrel_0.4 - diet_risk$legume_prop_belowtmrel_0.3), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.35)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.35)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.35)), bmort = baseline_mortality, n_it = 1000)
      legume_impacts_0.4_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$legume_prop_belowtmrel_0.5 - diet_risk$legume_prop_belowtmrel_0.4), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.45)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.45)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.45)), bmort = baseline_mortality, n_it = 1000)
      legume_impacts_0.5_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$legume_prop_belowtmrel_0.6 - diet_risk$legume_prop_belowtmrel_0.5), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.55)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.55)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.55)), bmort = baseline_mortality, n_it = 1000)
      legume_impacts_0.6_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$legume_prop_belowtmrel_0.7 - diet_risk$legume_prop_belowtmrel_0.6), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.65)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.65)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.65)), bmort = baseline_mortality, n_it = 1000)
      legume_impacts_0.7_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$legume_prop_belowtmrel_0.8 - diet_risk$legume_prop_belowtmrel_0.7), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.75)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.75)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.75)), bmort = baseline_mortality, n_it = 1000)
      legume_impacts_0.8_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$legume_prop_belowtmrel_0.9 - diet_risk$legume_prop_belowtmrel_0.8), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.85)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.85)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.85)), bmort = baseline_mortality, n_it = 1000)
      legume_impacts_0.9_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$legume_prop_belowtmrel - diet_risk$legume_prop_belowtmrel_0.9), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.95)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.95)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.95)), bmort = baseline_mortality, n_it = 1000)
      
      legume_mean <- sum(legume_impacts_0_tmrel$deaths_mean, legume_impacts_0.1_tmrel$deaths_mean, legume_impacts_0.2_tmrel$deaths_mean, legume_impacts_0.3_tmrel$deaths_mean, legume_impacts_0.4_tmrel$deaths_mean, legume_impacts_0.5_tmrel$deaths_mean, 
                            legume_impacts_0.6_tmrel$deaths_mean, legume_impacts_0.7_tmrel$deaths_mean, legume_impacts_0.8_tmrel$deaths_mean, legume_impacts_0.9_tmrel$deaths_mean)
      
      legume_median <- sum(legume_impacts_0_tmrel$deaths_median, legume_impacts_0.1_tmrel$deaths_median, legume_impacts_0.2_tmrel$deaths_median, legume_impacts_0.3_tmrel$deaths_median, legume_impacts_0.4_tmrel$deaths_median, legume_impacts_0.5_tmrel$deaths_median, 
                              legume_impacts_0.6_tmrel$deaths_median, legume_impacts_0.7_tmrel$deaths_median, legume_impacts_0.8_tmrel$deaths_median, legume_impacts_0.9_tmrel$deaths_median)
      
      legume_error_1 <- sqrt((legume_impacts_0_tmrel$deaths_97.5 - legume_impacts_0_tmrel$deaths_2.5)^2 + (legume_impacts_0.1_tmrel$deaths_97.5 - legume_impacts_0.1_tmrel$deaths_2.5)^2 +
                                  (legume_impacts_0.2_tmrel$deaths_97.5 - legume_impacts_0.2_tmrel$deaths_2.5)^2 + (legume_impacts_0.3_tmrel$deaths_97.5 - legume_impacts_0.3_tmrel$deaths_2.5)^2 +
                                  (legume_impacts_0.4_tmrel$deaths_97.5 - legume_impacts_0.4_tmrel$deaths_2.5)^2 + (legume_impacts_0.5_tmrel$deaths_97.5 - legume_impacts_0.5_tmrel$deaths_2.5)^2 +
                                  (legume_impacts_0.6_tmrel$deaths_97.5 - legume_impacts_0.6_tmrel$deaths_2.5)^2 + (legume_impacts_0.7_tmrel$deaths_97.5 - legume_impacts_0.7_tmrel$deaths_2.5)^2 +
                                  (legume_impacts_0.8_tmrel$deaths_97.5 - legume_impacts_0.8_tmrel$deaths_2.5)^2 + (legume_impacts_0.9_tmrel$deaths_97.5 - legume_impacts_0.9_tmrel$deaths_2.5)^2)##absolute error (use for now but see whether propagation gives realistic results)
      
      legume_error_2 <- sqrt(sum((((legume_impacts_0_tmrel$deaths_97.5 - legume_impacts_0_tmrel$deaths_2.5) /2 / legume_impacts_0_tmrel$deaths_median*100)^2),  
                                    (((legume_impacts_0.1_tmrel$deaths_97.5 - legume_impacts_0.1_tmrel$deaths_2.5) /2 / legume_impacts_0.1_tmrel$deaths_median*100)^2), 
                                    (((legume_impacts_0.2_tmrel$deaths_97.5 - legume_impacts_0.2_tmrel$deaths_2.5) /2 / legume_impacts_0.2_tmrel$deaths_median*100)^2), 
                                    (((legume_impacts_0.3_tmrel$deaths_97.5 - legume_impacts_0.3_tmrel$deaths_2.5) /2 / legume_impacts_0.3_tmrel$deaths_median*100)^2), 
                                    (((legume_impacts_0.4_tmrel$deaths_97.5 - legume_impacts_0.4_tmrel$deaths_2.5) /2 / legume_impacts_0.4_tmrel$deaths_median*100)^2), 
                                    (((legume_impacts_0.5_tmrel$deaths_97.5 - legume_impacts_0.5_tmrel$deaths_2.5) /2 / legume_impacts_0.5_tmrel$deaths_median*100)^2), 
                                    (((legume_impacts_0.6_tmrel$deaths_97.5 - legume_impacts_0.6_tmrel$deaths_2.5) /2 / legume_impacts_0.6_tmrel$deaths_median*100)^2), 
                                    (((legume_impacts_0.7_tmrel$deaths_97.5 - legume_impacts_0.7_tmrel$deaths_2.5) /2 / legume_impacts_0.7_tmrel$deaths_median*100)^2), 
                                    (((legume_impacts_0.8_tmrel$deaths_97.5 - legume_impacts_0.8_tmrel$deaths_2.5) /2 / legume_impacts_0.8_tmrel$deaths_median*100)^2), 
                                    (((legume_impacts_0.9_tmrel$deaths_97.5 - legume_impacts_0.9_tmrel$deaths_2.5) /2 / legume_impacts_0.9_tmrel$deaths_median*100)^2), na.rm = TRUE)) ##percent error
      
      
      
      diet_health_summary[(length(diet_health_summary$country)+1),]$risk <- "Diet low in legumes"
      diet_health_summary[(length(diet_health_summary$country)),]$disease <- legume_diseases[d]
      diet_health_summary[(length(diet_health_summary$country)),]$age <- ages[age]
      diet_health_summary[(length(diet_health_summary$country)),]$sex <- sex[i]
      diet_health_summary[(length(diet_health_summary$country)),]$mean <-  legume_mean
      diet_health_summary[(length(diet_health_summary$country)),]$median <-  legume_median
      diet_health_summary[(length(diet_health_summary$country)),]$p2.5 <-  legume_median - legume_error_1/2
      diet_health_summary[(length(diet_health_summary$country)),]$p97.5 <-  legume_median + legume_error_1/2
      
      
      
    }
  }
}




##Grains
grain_risks <- inputs_risk[ which(inputs_risk$risk == "Diet low in whole grains"),]
grain_diseases <- as.character(as.data.frame(table(as.character(grain_risks$disease)))$Var1)

for(i in seq(from=1, to= 2, by=1)) {
  
  for(d in seq(from=1, to= length(grain_diseases), by=1)) {  
    
    for(age in seq(from=6, to= length(ages), by=1)) {
      
      RR = grain_risks[(grain_risks$age == ages[age] & grain_risks$disease == grain_diseases[d] & grain_risks$sex == sex_2[i] & grain_risks$variable == "Relative Risk"),][paste("y_", year, sep = "")][[1]]
      RR_2.5 = grain_risks[(grain_risks$age == ages[age] & grain_risks$disease == grain_diseases[d] & grain_risks$sex == sex_2[i] & grain_risks$variable == "Relative Risk 2.5"),][paste("y_", year, sep = "")][[1]]
      RR_97.5 = grain_risks[(grain_risks$age == ages[age] & grain_risks$disease == grain_diseases[d] & grain_risks$sex == sex_2[i] & grain_risks$variable == "Relative Risk 97.5"),][paste("y_", year, sep = "")][[1]]
      tmrel = grain_risks[(grain_risks$age == ages[age] & grain_risks$disease == grain_diseases[d] & grain_risks$sex == sex_2[i] & grain_risks$variable == "Relative Risk"),]$tmrel
      increment = grain_risks[(grain_risks$age == ages[age] & grain_risks$disease == grain_diseases[d] & grain_risks$sex == sex_2[i] & grain_risks$variable == "Relative Risk"),]$increment
      exposure = 0
      baseline_mortality = inputs_health[(inputs_health$age == ages[age] & inputs_health$disease == grain_diseases[d] & inputs_health$sex == sex_2[i]),][paste("y_", year, sep = "")][[1]]
      
      if(RR == 0){RR <- 1}
      if(RR_2.5 == 0){RR_2.5 <- 1}
      if(RR_97.5 == 0){RR_97.5 <- 1}
      
      grain_impacts_0_tmrel   <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$grain_prop_belowtmrel_0.1 - diet_risk$grain_prop_belowtmrel_0), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.05)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.05)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.05)), bmort = baseline_mortality, n_it = 1000)
      grain_impacts_0.1_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$grain_prop_belowtmrel_0.2 - diet_risk$grain_prop_belowtmrel_0.1), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.15)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.15)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.15)), bmort = baseline_mortality, n_it = 1000)
      grain_impacts_0.2_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$grain_prop_belowtmrel_0.3 - diet_risk$grain_prop_belowtmrel_0.2), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.25)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.25)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.25)), bmort = baseline_mortality, n_it = 1000)
      grain_impacts_0.3_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$grain_prop_belowtmrel_0.4 - diet_risk$grain_prop_belowtmrel_0.3), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.35)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.35)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.35)), bmort = baseline_mortality, n_it = 1000)
      grain_impacts_0.4_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$grain_prop_belowtmrel_0.5 - diet_risk$grain_prop_belowtmrel_0.4), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.45)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.45)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.45)), bmort = baseline_mortality, n_it = 1000)
      grain_impacts_0.5_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$grain_prop_belowtmrel_0.6 - diet_risk$grain_prop_belowtmrel_0.5), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.55)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.55)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.55)), bmort = baseline_mortality, n_it = 1000)
      grain_impacts_0.6_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$grain_prop_belowtmrel_0.7 - diet_risk$grain_prop_belowtmrel_0.6), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.65)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.65)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.65)), bmort = baseline_mortality, n_it = 1000)
      grain_impacts_0.7_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$grain_prop_belowtmrel_0.8 - diet_risk$grain_prop_belowtmrel_0.7), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.75)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.75)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.75)), bmort = baseline_mortality, n_it = 1000)
      grain_impacts_0.8_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$grain_prop_belowtmrel_0.9 - diet_risk$grain_prop_belowtmrel_0.8), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.85)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.85)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.85)), bmort = baseline_mortality, n_it = 1000)
      grain_impacts_0.9_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$grain_prop_belowtmrel - diet_risk$grain_prop_belowtmrel_0.9), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.95)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.95)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.95)), bmort = baseline_mortality, n_it = 1000)
      
      grain_mean <- sum(grain_impacts_0_tmrel$deaths_mean, grain_impacts_0.1_tmrel$deaths_mean, grain_impacts_0.2_tmrel$deaths_mean, grain_impacts_0.3_tmrel$deaths_mean, grain_impacts_0.4_tmrel$deaths_mean, grain_impacts_0.5_tmrel$deaths_mean, 
                         grain_impacts_0.6_tmrel$deaths_mean, grain_impacts_0.7_tmrel$deaths_mean, grain_impacts_0.8_tmrel$deaths_mean, grain_impacts_0.9_tmrel$deaths_mean)
      
      grain_median <- sum(grain_impacts_0_tmrel$deaths_median, grain_impacts_0.1_tmrel$deaths_median, grain_impacts_0.2_tmrel$deaths_median, grain_impacts_0.3_tmrel$deaths_median, grain_impacts_0.4_tmrel$deaths_median, grain_impacts_0.5_tmrel$deaths_median, 
                           grain_impacts_0.6_tmrel$deaths_median, grain_impacts_0.7_tmrel$deaths_median, grain_impacts_0.8_tmrel$deaths_median, grain_impacts_0.9_tmrel$deaths_median)
      
      grain_error_1 <- sqrt((grain_impacts_0_tmrel$deaths_97.5 - grain_impacts_0_tmrel$deaths_2.5)^2 + (grain_impacts_0.1_tmrel$deaths_97.5 - grain_impacts_0.1_tmrel$deaths_2.5)^2 +
                               (grain_impacts_0.2_tmrel$deaths_97.5 - grain_impacts_0.2_tmrel$deaths_2.5)^2 + (grain_impacts_0.3_tmrel$deaths_97.5 - grain_impacts_0.3_tmrel$deaths_2.5)^2 +
                               (grain_impacts_0.4_tmrel$deaths_97.5 - grain_impacts_0.4_tmrel$deaths_2.5)^2 + (grain_impacts_0.5_tmrel$deaths_97.5 - grain_impacts_0.5_tmrel$deaths_2.5)^2 +
                               (grain_impacts_0.6_tmrel$deaths_97.5 - grain_impacts_0.6_tmrel$deaths_2.5)^2 + (grain_impacts_0.7_tmrel$deaths_97.5 - grain_impacts_0.7_tmrel$deaths_2.5)^2 +
                               (grain_impacts_0.8_tmrel$deaths_97.5 - grain_impacts_0.8_tmrel$deaths_2.5)^2 + (grain_impacts_0.9_tmrel$deaths_97.5 - grain_impacts_0.9_tmrel$deaths_2.5)^2)##absolute error (use for now but see whether propagation gives realistic results)
      
      grain_error_2 <- sqrt(sum((((grain_impacts_0_tmrel$deaths_97.5 - grain_impacts_0_tmrel$deaths_2.5) /2 / grain_impacts_0_tmrel$deaths_median*100)^2),  
                                 (((grain_impacts_0.1_tmrel$deaths_97.5 - grain_impacts_0.1_tmrel$deaths_2.5) /2 / grain_impacts_0.1_tmrel$deaths_median*100)^2), 
                                 (((grain_impacts_0.2_tmrel$deaths_97.5 - grain_impacts_0.2_tmrel$deaths_2.5) /2 / grain_impacts_0.2_tmrel$deaths_median*100)^2), 
                                 (((grain_impacts_0.3_tmrel$deaths_97.5 - grain_impacts_0.3_tmrel$deaths_2.5) /2 / grain_impacts_0.3_tmrel$deaths_median*100)^2), 
                                 (((grain_impacts_0.4_tmrel$deaths_97.5 - grain_impacts_0.4_tmrel$deaths_2.5) /2 / grain_impacts_0.4_tmrel$deaths_median*100)^2), 
                                 (((grain_impacts_0.5_tmrel$deaths_97.5 - grain_impacts_0.5_tmrel$deaths_2.5) /2 / grain_impacts_0.5_tmrel$deaths_median*100)^2), 
                                 (((grain_impacts_0.6_tmrel$deaths_97.5 - grain_impacts_0.6_tmrel$deaths_2.5) /2 / grain_impacts_0.6_tmrel$deaths_median*100)^2), 
                                 (((grain_impacts_0.7_tmrel$deaths_97.5 - grain_impacts_0.7_tmrel$deaths_2.5) /2 / grain_impacts_0.7_tmrel$deaths_median*100)^2), 
                                 (((grain_impacts_0.8_tmrel$deaths_97.5 - grain_impacts_0.8_tmrel$deaths_2.5) /2 / grain_impacts_0.8_tmrel$deaths_median*100)^2), 
                                 (((grain_impacts_0.9_tmrel$deaths_97.5 - grain_impacts_0.9_tmrel$deaths_2.5) /2 / grain_impacts_0.9_tmrel$deaths_median*100)^2), na.rm = TRUE)) ##percent error
      
      
      
      diet_health_summary[(length(diet_health_summary$country)+1),]$risk <- "Diet low in grains"
      diet_health_summary[(length(diet_health_summary$country)),]$disease <- grain_diseases[d]
      diet_health_summary[(length(diet_health_summary$country)),]$age <- ages[age]
      diet_health_summary[(length(diet_health_summary$country)),]$sex <- sex[i]
      diet_health_summary[(length(diet_health_summary$country)),]$mean <-  grain_mean
      diet_health_summary[(length(diet_health_summary$country)),]$median <-  grain_median
      diet_health_summary[(length(diet_health_summary$country)),]$p2.5 <-  grain_median - grain_error_1/2
      diet_health_summary[(length(diet_health_summary$country)),]$p97.5 <-  grain_median + grain_error_1/2
      
      
      
    }
  }
}



##Milk
milk_risks <- inputs_risk[ which(inputs_risk$risk == "Diet low in milk"),]
milk_diseases <- as.character(as.data.frame(table(as.character(milk_risks$disease)))$Var1)

for(i in seq(from=1, to= 2, by=1)) {
  
  for(d in seq(from=1, to= length(milk_diseases), by=1)) {  
    
    for(age in seq(from=6, to= length(ages), by=1)) {
      
      RR = milk_risks[(milk_risks$age == ages[age] & milk_risks$disease == milk_diseases[d] & milk_risks$sex == sex_2[i] & milk_risks$variable == "Relative Risk"),][paste("y_", year, sep = "")][[1]]
      RR_2.5 = milk_risks[(milk_risks$age == ages[age] & milk_risks$disease == milk_diseases[d] & milk_risks$sex == sex_2[i] & milk_risks$variable == "Relative Risk 2.5"),][paste("y_", year, sep = "")][[1]]
      RR_97.5 = milk_risks[(milk_risks$age == ages[age] & milk_risks$disease == milk_diseases[d] & milk_risks$sex == sex_2[i] & milk_risks$variable == "Relative Risk 97.5"),][paste("y_", year, sep = "")][[1]]
      tmrel = milk_risks[(milk_risks$age == ages[age] & milk_risks$disease == milk_diseases[d] & milk_risks$sex == sex_2[i] & milk_risks$variable == "Relative Risk"),]$tmrel
      increment = milk_risks[(milk_risks$age == ages[age] & milk_risks$disease == milk_diseases[d] & milk_risks$sex == sex_2[i] & milk_risks$variable == "Relative Risk"),]$increment
      exposure = 0
      baseline_mortality = inputs_health[(inputs_health$age == ages[age] & inputs_health$disease == milk_diseases[d] & inputs_health$sex == sex_2[i]),][paste("y_", year, sep = "")][[1]]
      
      if(RR == 0){RR <- 1}
      if(RR_2.5 == 0){RR_2.5 <- 1}
      if(RR_97.5 == 0){RR_97.5 <- 1}
      
      milk_impacts_0_tmrel   <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$milk_prop_belowtmrel_0.1 - diet_risk$milk_prop_belowtmrel_0), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.05)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.05)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.05)), bmort = baseline_mortality, n_it = 1000)
      milk_impacts_0.1_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$milk_prop_belowtmrel_0.2 - diet_risk$milk_prop_belowtmrel_0.1), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.15)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.15)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.15)), bmort = baseline_mortality, n_it = 1000)
      milk_impacts_0.2_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$milk_prop_belowtmrel_0.3 - diet_risk$milk_prop_belowtmrel_0.2), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.25)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.25)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.25)), bmort = baseline_mortality, n_it = 1000)
      milk_impacts_0.3_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$milk_prop_belowtmrel_0.4 - diet_risk$milk_prop_belowtmrel_0.3), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.35)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.35)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.35)), bmort = baseline_mortality, n_it = 1000)
      milk_impacts_0.4_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$milk_prop_belowtmrel_0.5 - diet_risk$milk_prop_belowtmrel_0.4), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.45)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.45)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.45)), bmort = baseline_mortality, n_it = 1000)
      milk_impacts_0.5_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$milk_prop_belowtmrel_0.6 - diet_risk$milk_prop_belowtmrel_0.5), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.55)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.55)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.55)), bmort = baseline_mortality, n_it = 1000)
      milk_impacts_0.6_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$milk_prop_belowtmrel_0.7 - diet_risk$milk_prop_belowtmrel_0.6), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.65)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.65)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.65)), bmort = baseline_mortality, n_it = 1000)
      milk_impacts_0.7_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$milk_prop_belowtmrel_0.8 - diet_risk$milk_prop_belowtmrel_0.7), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.75)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.75)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.75)), bmort = baseline_mortality, n_it = 1000)
      milk_impacts_0.8_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$milk_prop_belowtmrel_0.9 - diet_risk$milk_prop_belowtmrel_0.8), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.85)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.85)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.85)), bmort = baseline_mortality, n_it = 1000)
      milk_impacts_0.9_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$milk_prop_belowtmrel - diet_risk$milk_prop_belowtmrel_0.9), RR = (1+((RR-1)/increment)*(tmrel-tmrel*0.95)), RR_95min = (1+((RR_2.5-1)/increment)*(tmrel-tmrel*0.95)), RR_95max = (1+((RR_97.5-1)/increment)*(tmrel-tmrel*0.95)), bmort = baseline_mortality, n_it = 1000)
      
      milk_mean <- sum(milk_impacts_0_tmrel$deaths_mean, milk_impacts_0.1_tmrel$deaths_mean, milk_impacts_0.2_tmrel$deaths_mean, milk_impacts_0.3_tmrel$deaths_mean, milk_impacts_0.4_tmrel$deaths_mean, milk_impacts_0.5_tmrel$deaths_mean, 
                         milk_impacts_0.6_tmrel$deaths_mean, milk_impacts_0.7_tmrel$deaths_mean, milk_impacts_0.8_tmrel$deaths_mean, milk_impacts_0.9_tmrel$deaths_mean)
      
      milk_median <- sum(milk_impacts_0_tmrel$deaths_median, milk_impacts_0.1_tmrel$deaths_median, milk_impacts_0.2_tmrel$deaths_median, milk_impacts_0.3_tmrel$deaths_median, milk_impacts_0.4_tmrel$deaths_median, milk_impacts_0.5_tmrel$deaths_median, 
                           milk_impacts_0.6_tmrel$deaths_median, milk_impacts_0.7_tmrel$deaths_median, milk_impacts_0.8_tmrel$deaths_median, milk_impacts_0.9_tmrel$deaths_median)
      
      milk_error_1 <- sqrt((milk_impacts_0_tmrel$deaths_97.5 - milk_impacts_0_tmrel$deaths_2.5)^2 + (milk_impacts_0.1_tmrel$deaths_97.5 - milk_impacts_0.1_tmrel$deaths_2.5)^2 +
                               (milk_impacts_0.2_tmrel$deaths_97.5 - milk_impacts_0.2_tmrel$deaths_2.5)^2 + (milk_impacts_0.3_tmrel$deaths_97.5 - milk_impacts_0.3_tmrel$deaths_2.5)^2 +
                               (milk_impacts_0.4_tmrel$deaths_97.5 - milk_impacts_0.4_tmrel$deaths_2.5)^2 + (milk_impacts_0.5_tmrel$deaths_97.5 - milk_impacts_0.5_tmrel$deaths_2.5)^2 +
                               (milk_impacts_0.6_tmrel$deaths_97.5 - milk_impacts_0.6_tmrel$deaths_2.5)^2 + (milk_impacts_0.7_tmrel$deaths_97.5 - milk_impacts_0.7_tmrel$deaths_2.5)^2 +
                               (milk_impacts_0.8_tmrel$deaths_97.5 - milk_impacts_0.8_tmrel$deaths_2.5)^2 + (milk_impacts_0.9_tmrel$deaths_97.5 - milk_impacts_0.9_tmrel$deaths_2.5)^2)##absolute error (use for now but see whether propagation gives realistic results)
      
      milk_error_2 <- sqrt(sum((((milk_impacts_0_tmrel$deaths_97.5 - milk_impacts_0_tmrel$deaths_2.5) /2 / milk_impacts_0_tmrel$deaths_median*100)^2),  
                                 (((milk_impacts_0.1_tmrel$deaths_97.5 - milk_impacts_0.1_tmrel$deaths_2.5) /2 / milk_impacts_0.1_tmrel$deaths_median*100)^2), 
                                 (((milk_impacts_0.2_tmrel$deaths_97.5 - milk_impacts_0.2_tmrel$deaths_2.5) /2 / milk_impacts_0.2_tmrel$deaths_median*100)^2), 
                                 (((milk_impacts_0.3_tmrel$deaths_97.5 - milk_impacts_0.3_tmrel$deaths_2.5) /2 / milk_impacts_0.3_tmrel$deaths_median*100)^2), 
                                 (((milk_impacts_0.4_tmrel$deaths_97.5 - milk_impacts_0.4_tmrel$deaths_2.5) /2 / milk_impacts_0.4_tmrel$deaths_median*100)^2), 
                                 (((milk_impacts_0.5_tmrel$deaths_97.5 - milk_impacts_0.5_tmrel$deaths_2.5) /2 / milk_impacts_0.5_tmrel$deaths_median*100)^2), 
                                 (((milk_impacts_0.6_tmrel$deaths_97.5 - milk_impacts_0.6_tmrel$deaths_2.5) /2 / milk_impacts_0.6_tmrel$deaths_median*100)^2), 
                                 (((milk_impacts_0.7_tmrel$deaths_97.5 - milk_impacts_0.7_tmrel$deaths_2.5) /2 / milk_impacts_0.7_tmrel$deaths_median*100)^2), 
                                 (((milk_impacts_0.8_tmrel$deaths_97.5 - milk_impacts_0.8_tmrel$deaths_2.5) /2 / milk_impacts_0.8_tmrel$deaths_median*100)^2), 
                                 (((milk_impacts_0.9_tmrel$deaths_97.5 - milk_impacts_0.9_tmrel$deaths_2.5) /2 / milk_impacts_0.9_tmrel$deaths_median*100)^2), na.rm = TRUE)) ##percent error
      
      
      
      diet_health_summary[(length(diet_health_summary$country)+1),]$risk <- "Diet low in milks"
      diet_health_summary[(length(diet_health_summary$country)),]$disease <- milk_diseases[d]
      diet_health_summary[(length(diet_health_summary$country)),]$age <- ages[age]
      diet_health_summary[(length(diet_health_summary$country)),]$sex <- sex[i]
      diet_health_summary[(length(diet_health_summary$country)),]$mean <-  milk_mean
      diet_health_summary[(length(diet_health_summary$country)),]$median <-  milk_median
      diet_health_summary[(length(diet_health_summary$country)),]$p2.5 <-  milk_median - milk_error_1/2
      diet_health_summary[(length(diet_health_summary$country)),]$p97.5 <-  milk_median + milk_error_1/2
      
      
      
    }
  }
}







##Meat
meat_risks <- inputs_risk[ which(inputs_risk$risk == "Diet high in red meat"),]
meat_diseases <- as.character(as.data.frame(table(as.character(meat_risks$disease)))$Var1)

for(i in seq(from=1, to= 2, by=1)) {
  
  for(d in seq(from=1, to= length(meat_diseases), by=1)) {  
    
    for(age in seq(from=6, to= length(ages), by=1)) {
    
      RR = meat_risks[(meat_risks$age == ages[age] & meat_risks$disease == meat_diseases[d] & meat_risks$sex == sex_2[i] & meat_risks$variable == "Relative Risk"),][paste("y_", year, sep = "")][[1]]
      RR_2.5 = meat_risks[(meat_risks$age == ages[age] & meat_risks$disease == meat_diseases[d] & meat_risks$sex == sex_2[i] & meat_risks$variable == "Relative Risk 2.5"),][paste("y_", year, sep = "")][[1]]
      RR_97.5 = meat_risks[(meat_risks$age == ages[age] & meat_risks$disease == meat_diseases[d] & meat_risks$sex == sex_2[i] & meat_risks$variable == "Relative Risk 97.5"),][paste("y_", year, sep = "")][[1]]
      tmrel = meat_risks[(meat_risks$age == ages[age] & meat_risks$disease == meat_diseases[d] & meat_risks$sex == sex_2[i] & meat_risks$variable == "Relative Risk"),]$tmrel
      increment = meat_risks[(meat_risks$age == ages[age] & meat_risks$disease == meat_diseases[d] & meat_risks$sex == sex_2[i] & meat_risks$variable == "Relative Risk"),]$increment
      exposure = 0
      baseline_mortality = inputs_health[(inputs_health$age == ages[age] & inputs_health$disease == meat_diseases[d] & inputs_health$sex == sex_2[i]),][paste("y_", year, sep = "")][[1]]
      
      if(RR == 0){RR <- 1}
      if(RR_2.5 == 0){RR_2.5 <- 1}
      if(RR_97.5 == 0){RR_97.5 <- 1}
      
    meat_impacts_1000g_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$meat_prop_below1000g - diet_risk$meat_prop_below900g), RR = (1+((RR-1)/increment)*(950-tmrel)), RR_95min = (1+((RR_2.5-1)/increment)*(950-tmrel)), RR_95max = (1+((RR_97.5-1)/increment)*(950-tmrel)), bmort = baseline_mortality, n_it = 1000)
    meat_impacts_900g_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$meat_prop_below900g - diet_risk$meat_prop_below800g), RR = (1+((RR-1)/increment)*(850-tmrel)), RR_95min = (1+((RR_2.5-1)/increment)*(850-tmrel)), RR_95max = (1+((RR_97.5-1)/increment)*(850-tmrel)), bmort = baseline_mortality, n_it = 1000)
    meat_impacts_800g_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$meat_prop_below800g - diet_risk$meat_prop_below700g), RR = (1+((RR-1)/increment)*(750-tmrel)), RR_95min = (1+((RR_2.5-1)/increment)*(750-tmrel)), RR_95max = (1+((RR_97.5-1)/increment)*(750-tmrel)), bmort = baseline_mortality, n_it = 1000)
    meat_impacts_700g_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$meat_prop_below700g - diet_risk$meat_prop_below600g), RR = (1+((RR-1)/increment)*(650-tmrel)), RR_95min = (1+((RR_2.5-1)/increment)*(650-tmrel)), RR_95max = (1+((RR_97.5-1)/increment)*(650-tmrel)), bmort = baseline_mortality, n_it = 1000)
    meat_impacts_600g_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$meat_prop_below600g - diet_risk$meat_prop_below500g), RR = (1+((RR-1)/increment)*(550-tmrel)), RR_95min = (1+((RR_2.5-1)/increment)*(550-tmrel)), RR_95max = (1+((RR_97.5-1)/increment)*(550-tmrel)), bmort = baseline_mortality, n_it = 1000)
    meat_impacts_500g_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$meat_prop_below500g - diet_risk$meat_prop_below400g), RR = (1+((RR-1)/increment)*(450-tmrel)), RR_95min = (1+((RR_2.5-1)/increment)*(450-tmrel)), RR_95max = (1+((RR_97.5-1)/increment)*(450-tmrel)), bmort = baseline_mortality, n_it = 1000)
    meat_impacts_400g_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$meat_prop_below400g - diet_risk$meat_prop_below350g), RR = (1+((RR-1)/increment)*(375-tmrel)), RR_95min = (1+((RR_2.5-1)/increment)*(375-tmrel)), RR_95max = (1+((RR_97.5-1)/increment)*(375-tmrel)), bmort = baseline_mortality, n_it = 1000)
    meat_impacts_350g_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$meat_prop_below350g - diet_risk$meat_prop_below300g), RR = (1+((RR-1)/increment)*(325-tmrel)), RR_95min = (1+((RR_2.5-1)/increment)*(325-tmrel)), RR_95max = (1+((RR_97.5-1)/increment)*(325-tmrel)), bmort = baseline_mortality, n_it = 1000)
    meat_impacts_300g_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$meat_prop_below300g - diet_risk$meat_prop_below250g), RR = (1+((RR-1)/increment)*(275-tmrel)), RR_95min = (1+((RR_2.5-1)/increment)*(275-tmrel)), RR_95max = (1+((RR_97.5-1)/increment)*(275-tmrel)), bmort = baseline_mortality, n_it = 1000)
    meat_impacts_250g_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$meat_prop_below250g - diet_risk$meat_prop_below200g), RR = (1+((RR-1)/increment)*(225-tmrel)), RR_95min = (1+((RR_2.5-1)/increment)*(225-tmrel)), RR_95max = (1+((RR_97.5-1)/increment)*(225-tmrel)), bmort = baseline_mortality, n_it = 1000)
    meat_impacts_200g_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$meat_prop_below200g - diet_risk$meat_prop_below150g), RR = (1+((RR-1)/increment)*(175-tmrel)), RR_95min = (1+((RR_2.5-1)/increment)*(175-tmrel)), RR_95max = (1+((RR_97.5-1)/increment)*(175-tmrel)), bmort = baseline_mortality, n_it = 1000)
    meat_impacts_150g_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$meat_prop_below150g - diet_risk$meat_prop_below100g), RR = (1+((RR-1)/increment)*(125-tmrel)), RR_95min = (1+((RR_2.5-1)/increment)*(125-tmrel)), RR_95max = (1+((RR_97.5-1)/increment)*(125-tmrel)), bmort = baseline_mortality, n_it = 1000)
    meat_impacts_100g_tmrel <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$meat_prop_below100g - diet_risk$meat_prop_below50g), RR = (1+((RR-1)/increment)*(75-tmrel)), RR_95min = (1+((RR_2.5-1)/increment)*(75-tmrel)), RR_95max = (1+((RR_97.5-1)/increment)*(75-tmrel)), bmort = baseline_mortality, n_it = 1000)
    meat_impacts_50g_tmrel  <- diet_hia(pop = get(paste("pop_", as.numeric(substr(ages_2[age], 1, 2)), "_", sex[i], sep = ""))*(diet_risk$meat_prop_below50g - diet_risk$meat_prop_belowtmrel), RR = (1+((RR-1)/increment)*(((50-(50-tmrel)/2))-tmrel)), RR_95min = (1+((RR_2.5-1)/increment)*(((50-(50-tmrel)/2))-tmrel)), RR_95max = (1+((RR_97.5-1)/increment)*(((50-(50-tmrel)/2))-tmrel)), bmort = baseline_mortality, n_it = 1000)

    
    meat_mean <- sum(meat_impacts_1000g_tmrel$deaths_mean, meat_impacts_900g_tmrel$deaths_mean, meat_impacts_800g_tmrel$deaths_mean, meat_impacts_700g_tmrel$deaths_mean, meat_impacts_600g_tmrel$deaths_mean, meat_impacts_500g_tmrel$deaths_mean, 
                      meat_impacts_400g_tmrel$deaths_mean, meat_impacts_350g_tmrel$deaths_mean, meat_impacts_300g_tmrel$deaths_mean, meat_impacts_250g_tmrel$deaths_mean, meat_impacts_200g_tmrel$deaths_mean, meat_impacts_150g_tmrel$deaths_mean,
                      meat_impacts_100g_tmrel$deaths_mean, meat_impacts_50g_tmrel$deaths_mean)
    
    meat_median <- sum(meat_impacts_1000g_tmrel$deaths_median, meat_impacts_900g_tmrel$deaths_median, meat_impacts_800g_tmrel$deaths_median, meat_impacts_700g_tmrel$deaths_median, meat_impacts_600g_tmrel$deaths_median, meat_impacts_500g_tmrel$deaths_median, 
                     meat_impacts_400g_tmrel$deaths_median, meat_impacts_350g_tmrel$deaths_median, meat_impacts_300g_tmrel$deaths_median, meat_impacts_250g_tmrel$deaths_median, meat_impacts_200g_tmrel$deaths_median, meat_impacts_150g_tmrel$deaths_median,
                     meat_impacts_100g_tmrel$deaths_median, meat_impacts_50g_tmrel$deaths_median)
    

    meat_error_1 <- sqrt((meat_impacts_1000g_tmrel$deaths_97.5 - meat_impacts_1000g_tmrel$deaths_2.5)^2 + (meat_impacts_900g_tmrel$deaths_97.5 - meat_impacts_900g_tmrel$deaths_2.5)^2 +
                            (meat_impacts_800g_tmrel$deaths_97.5 - meat_impacts_800g_tmrel$deaths_2.5)^2 + (meat_impacts_700g_tmrel$deaths_97.5 - meat_impacts_700g_tmrel$deaths_2.5)^2 +
                            (meat_impacts_600g_tmrel$deaths_97.5 - meat_impacts_600g_tmrel$deaths_2.5)^2 + (meat_impacts_500g_tmrel$deaths_97.5 - meat_impacts_500g_tmrel$deaths_2.5)^2 +
                            (meat_impacts_400g_tmrel$deaths_97.5 - meat_impacts_400g_tmrel$deaths_2.5)^2 + (meat_impacts_350g_tmrel$deaths_97.5 - meat_impacts_350g_tmrel$deaths_2.5)^2 +
                            (meat_impacts_300g_tmrel$deaths_97.5 - meat_impacts_300g_tmrel$deaths_2.5)^2 + (meat_impacts_250g_tmrel$deaths_97.5 - meat_impacts_250g_tmrel$deaths_2.5)^2 +
                            (meat_impacts_200g_tmrel$deaths_97.5 - meat_impacts_200g_tmrel$deaths_2.5)^2 + (meat_impacts_150g_tmrel$deaths_97.5 - meat_impacts_150g_tmrel$deaths_2.5)^2 +
                            (meat_impacts_100g_tmrel$deaths_97.5 - meat_impacts_100g_tmrel$deaths_2.5)^2 + (meat_impacts_50g_tmrel$deaths_97.5 - meat_impacts_50g_tmrel$deaths_2.5)^2)##absolute error (use for now but see whether propagation gives realistic results)
    
    meat_error_2 <- sqrt(sum((((meat_impacts_1000g_tmrel$deaths_97.5 - meat_impacts_1000g_tmrel$deaths_2.5) /2 / meat_impacts_1000g_tmrel$deaths_median*100)^2),  
                              (((meat_impacts_900g_tmrel$deaths_97.5 - meat_impacts_900g_tmrel$deaths_2.5) /2 / meat_impacts_900g_tmrel$deaths_median*100)^2), 
                              (((meat_impacts_700g_tmrel$deaths_97.5 - meat_impacts_800g_tmrel$deaths_2.5) /2 / meat_impacts_800g_tmrel$deaths_median*100)^2), 
                              (((meat_impacts_700g_tmrel$deaths_97.5 - meat_impacts_700g_tmrel$deaths_2.5) /2 / meat_impacts_700g_tmrel$deaths_median*100)^2), 
                             (((meat_impacts_600g_tmrel$deaths_97.5 - meat_impacts_600g_tmrel$deaths_2.5) /2 / meat_impacts_600g_tmrel$deaths_median*100)^2), 
                              (((meat_impacts_500g_tmrel$deaths_97.5 - meat_impacts_500g_tmrel$deaths_2.5) /2 / meat_impacts_500g_tmrel$deaths_median*100)^2), 
                              (((meat_impacts_400g_tmrel$deaths_97.5 - meat_impacts_400g_tmrel$deaths_2.5) /2 / meat_impacts_400g_tmrel$deaths_median*100)^2), 
                              (((meat_impacts_350g_tmrel$deaths_97.5 - meat_impacts_350g_tmrel$deaths_2.5) /2 / meat_impacts_350g_tmrel$deaths_median*100)^2), 
                              (((meat_impacts_300g_tmrel$deaths_97.5 - meat_impacts_300g_tmrel$deaths_2.5) /2 / meat_impacts_300g_tmrel$deaths_median*100)^2), 
                              (((meat_impacts_250g_tmrel$deaths_97.5 - meat_impacts_250g_tmrel$deaths_2.5) /2 / meat_impacts_250g_tmrel$deaths_median*100)^2), 
                              (((meat_impacts_200g_tmrel$deaths_97.5 - meat_impacts_200g_tmrel$deaths_2.5) /2 / meat_impacts_200g_tmrel$deaths_median*100)^2), 
                             (((meat_impacts_150g_tmrel$deaths_97.5 - meat_impacts_150g_tmrel$deaths_2.5) /2 / meat_impacts_150g_tmrel$deaths_median*100)^2), 
                             (((meat_impacts_100g_tmrel$deaths_97.5 - meat_impacts_100g_tmrel$deaths_2.5) /2 / meat_impacts_100g_tmrel$deaths_median*100)^2), 
                             (((meat_impacts_50g_tmrel$deaths_97.5 - meat_impacts_50g_tmrel$deaths_2.5) /2 / meat_impacts_50g_tmrel$deaths_median*100)^2), na.rm = TRUE)) ##percent error
    
    
    diet_health_summary[(length(diet_health_summary$country)+1),]$risk <- "Diet high in red meat"
    diet_health_summary[(length(diet_health_summary$country)),]$disease <- meat_diseases[d]
    diet_health_summary[(length(diet_health_summary$country)),]$age <- ages[age]
    diet_health_summary[(length(diet_health_summary$country)),]$sex <- sex[i]
    diet_health_summary[(length(diet_health_summary$country)),]$mean <-  meat_mean
    diet_health_summary[(length(diet_health_summary$country)),]$median <-  meat_median
    diet_health_summary[(length(diet_health_summary$country)),]$p2.5 <-  meat_median - meat_error_1/2
    diet_health_summary[(length(diet_health_summary$country)),]$p97.5 <-  meat_median + meat_error_1/2
    
  }
  
}

}

dr <- as.data.frame(diet_risk)
dr$country <- country
dr$year <- year

setwd(wd)

write.csv(emissions, paste(".\\Outputs\\emissions_", country, "_", year, ".csv", sep = ""), row.names = FALSE)

write.csv(emissions_summary, paste(".\\Outputs\\emissions_summary_", country, "_", year, ".csv", sep = ""), row.names = FALSE)

write.csv(consumption_waste, paste(".\\Outputs\\diet_summary_", country, "_", year, ".csv", sep = ""), row.names = FALSE)

write.csv(other_outputs, paste(".\\Outputs\\other_outputs_", country, "_", year, ".csv", sep = ""), row.names = FALSE)

write.csv(diet_health_summary, paste(".\\Outputs\\diet_health_summary_", country, "_", year, ".csv", sep = ""), row.names = FALSE)

write.csv(dr, paste(".\\Outputs\\diet_risk_summary_", country, "_", year, ".csv", sep = ""), row.names = FALSE)


}
}
