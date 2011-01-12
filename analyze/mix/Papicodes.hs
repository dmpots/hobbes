module Papicodes where

data PapiEvent =
    PAPI_L1_DCM
  | PAPI_L1_DCH
  | PAPI_L1_DCA
  | PAPI_L1_ICM
  | PAPI_L1_ICH
  | PAPI_L1_ICA
  | PAPI_L1_TCA
  | PAPI_L1_TCM
  | PAPI_L1_LDM
  | PAPI_L1_STM
  | PAPI_L2_DCM
  | PAPI_L2_DCH
  | PAPI_L2_DCA
  | PAPI_L2_DCR
  | PAPI_L2_DCW
  | PAPI_L2_ICM
  | PAPI_L2_ICH
  | PAPI_L2_ICA
  | PAPI_L2_TCM
  | PAPI_L2_TCH
  | PAPI_L2_TCA
  | PAPI_L2_TCW
  | PAPI_L2_TCR
  | PAPI_L2_LDM
  | PAPI_L2_STM
  | PAPI_TLB_DM
  | PAPI_TLB_IM
  | PAPI_BR_CN
  | PAPI_BR_TKN
  | PAPI_BR_NTK
  | PAPI_BR_MSP
  | PAPI_BR_PRC
  | PAPI_TOT_INS
  | PAPI_TOT_CYC
  deriving (Eq, Enum, Ord, Read, Show)

papiNormalizer :: PapiEvent -> PapiEvent
papiNormalizer PAPI_L1_DCM  = PAPI_L1_DCA 
papiNormalizer PAPI_L1_DCH  = PAPI_L1_DCA 
papiNormalizer PAPI_L1_DCA  = PAPI_L1_TCA
papiNormalizer PAPI_L1_ICM  = PAPI_L1_ICA
papiNormalizer PAPI_L1_ICH  = PAPI_L1_ICA
papiNormalizer PAPI_L1_ICA  = PAPI_L1_TCA
papiNormalizer PAPI_L1_TCA  = PAPI_L1_TCA
papiNormalizer PAPI_L1_TCM  = PAPI_L1_TCA
papiNormalizer PAPI_L1_LDM  = PAPI_L1_TCM
papiNormalizer PAPI_L1_STM  = PAPI_L1_TCM
papiNormalizer PAPI_L2_DCM  = PAPI_L2_DCA
papiNormalizer PAPI_L2_DCH  = PAPI_L2_DCA
papiNormalizer PAPI_L2_DCA  = PAPI_L2_TCA
papiNormalizer PAPI_L2_DCR  = PAPI_L2_DCA
papiNormalizer PAPI_L2_DCW  = PAPI_L2_DCA
papiNormalizer PAPI_L2_ICM  = PAPI_L2_ICA
papiNormalizer PAPI_L2_ICH  = PAPI_L2_ICA
papiNormalizer PAPI_L2_ICA  = PAPI_L2_TCA
papiNormalizer PAPI_L2_TCM  = PAPI_L2_TCA
papiNormalizer PAPI_L2_TCH  = PAPI_L2_TCA
papiNormalizer PAPI_L2_TCA  = PAPI_L2_TCA
papiNormalizer PAPI_L2_TCR  = PAPI_L2_TCA
papiNormalizer PAPI_L2_TCW  = PAPI_L2_TCA
papiNormalizer PAPI_L2_LDM  = PAPI_L2_TCM
papiNormalizer PAPI_L2_STM  = PAPI_L2_TCM
papiNormalizer PAPI_TLB_DM  = PAPI_TLB_DM
papiNormalizer PAPI_TLB_IM  = PAPI_TLB_IM
papiNormalizer PAPI_BR_CN   = PAPI_BR_CN
papiNormalizer PAPI_BR_TKN  = PAPI_BR_CN
papiNormalizer PAPI_BR_NTK  = PAPI_BR_CN
papiNormalizer PAPI_BR_MSP  = PAPI_BR_CN
papiNormalizer PAPI_BR_PRC  = PAPI_BR_CN
papiNormalizer PAPI_TOT_INS = PAPI_TOT_INS
papiNormalizer PAPI_TOT_CYC = PAPI_TOT_INS -- to calculate CPI


papiOutputFilter :: PapiEvent -> Bool
papiOutputFilter PAPI_L1_DCM  = True
papiOutputFilter PAPI_L1_ICM  = True

papiOutputFilter PAPI_L2_DCM  = True
papiOutputFilter PAPI_L2_ICM  = True

papiOutputFilter PAPI_BR_MSP  = True
papiOutputFilter PAPI_TOT_CYC = True
papiOutputFilter _            = False



