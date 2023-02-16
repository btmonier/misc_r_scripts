library(dplyr)
library(readxl)
library(magrittr)


bmsData <- readxl::read_excel(
    path = "/home/bm646/Downloads/rTASSEL_Demo_wHapMap-1_NOLOC_PLOT_rTASSEL_Demo_wHapMap-PLOTDATA_faperez_AF_20230216_045607.xls",
    sheet = "Observation"
)
bmsData %<>%
    mutate(Taxa = gsub(":.*$", "", DESIGNATION))


rtData <- rTASSEL::readPhenotypeFromPath(
    path = system.file("extdata", "mdp_traits_nomissing.txt", package = "rTASSEL")
) %>%
    rTASSEL::getPhenotypeDF() %>%
    tibble::as_tibble()

covData <- rTASSEL::readPhenotypeFromPath(
    path = system.file("extdata", "mdp_population_structure.txt", package = "rTASSEL")
) %>%
    rTASSEL::getPhenotypeDF() %>%
    tibble::as_tibble()

combData <- left_join(
    x = rtData,
    y = covData
)


bmsData %>%
    relocate(Taxa = DESIGNATION) %>%
    select(-c(EH_M_cm, EDia_M_cm, Dpoll, Q1, Q2, Q3)) %>%
    left_join(combData) %>%
    relocate(
        OBS_UNIT_ID,
        ENTRY_TYPE,
        GID,
        DESIGNATION = Taxa,
        ENTRY_NO,
        PLOT_NO,
        EH_M_cm        = EarHT,
        EDia_M_cm      = EarDia,
        Dpoll = dpoll,
        Q1,
        Q2,
        Q3,
        NPSEL
    ) %>%
    mutate(NPSEL = "YES") %>%
    mutate(
        Q1 = ifelse(is.na(Q1), 0, Q1),
        Q2 = ifelse(is.na(Q2), 0, Q2),
        Q3 = ifelse(is.na(Q3), 0, Q3)
    ) %>%
    arrange(ENTRY_NO) %>%
    write_xlsx(path = "result.xlsx")


