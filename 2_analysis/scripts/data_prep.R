git a# ==========================================================================================
# Data prep
# Author: Manoela do Amaral Ferronato
# Date: 05/2025
# ==========================================================================================

# -------- Loading packages ----------------------------------------------------------------
library(data.table)
library(tidyverse)
library(stringr)
library(readxl)

# ---------------------------------------
# ------- Active-civil-servants data ----
#----------------------------------------
# Reading the files
active_files <- list.files(path = paste0(getwd(), "/0_raw/active-civil-servants"), pattern = ".csv", full.names = TRUE)

# Cleaning and combining the files
active_cs <- rbindlist(
  lapply(active_files, function(f) {
    dt <- fread(f)                                                                               # read 
    dt <- dt[dt$QUADRO_E=="QM",]                                                                 # filtering only teachers
    dt[, DATE := str_extract(basename(f), "(\\d{2})(\\d{2})")]                                   # extracting month and year from file name
    dt[, YEAR := as.integer(paste0("20", str_extract(basename(f), "\\d{4}") |> substr(3, 4)))]   # creating year variable
    setnames(dt, toupper(names(dt)))                                                             # UPPER-CASE the column names
    dt <- select(dt, DATE, YEAR, ID_INTERNO, UAC, CATEG_E, SEXO, ID_COR, DATA_NASCIMENTO,               # selecting columns
                 DATA_INICIO_EXERCICIO_E)
    dt <- unique(dt)                                                                             # remove duplicities 
    dt                                                                                     
  }),
  use.names = TRUE, fill = TRUE         
)

# Fixing date columns
active_cs[, DATA_INICIO_EXERCICIO_E := as.Date(DATA_INICIO_EXERCICIO_E, format = "%d/%m/%Y")]
active_cs[, DATA_NASCIMENTO := as.Date(DATA_NASCIMENTO, format = "%d/%m/%Y")]

# For teachers with multiple contracts, assign the earliest contract start date ("DATA_INICIO_EXERCICIO")
# to identify when each teacher first started working 
active_cs[, DATA_INICIO_EXERCICIO_E := min(DATA_INICIO_EXERCICIO_E, na.rm = TRUE), by = ID_INTERNO]

# Age and years of activity
active_cs[, `:=`(
  ANOS_ATIVIDADE = YEAR - year(DATA_INICIO_EXERCICIO_E),
  IDADE = YEAR - year(DATA_NASCIMENTO),
  ID_COR = fifelse(ID_COR %in% c("", "D"), "S/INFO",
                   fifelse(ID_COR == "N", "P", ID_COR))
)]


# ---- Dealing with inconsistencies: teachers who change race/color or sex
# How frequently do they occur in the data?
active_cs %>% 
  group_by(ID_INTERNO) %>% 
  mutate(
    n_color = n_distinct(ID_COR,           na.rm = TRUE),
    n_sex   = n_distinct(SEXO,             na.rm = TRUE),
    n_birth = n_distinct(DATA_NASCIMENTO,  na.rm = TRUE),
  ) %>% 
  group_by(YEAR) %>%
  summarise(
    total_professores = n(),
    color_change = sum(n_color>1),
    sex_change = sum(n_sex>1),
    date_change = sum(n_birth>1),
    perc_color = round(100 * color_change / total_professores, 2),
    perc_sex = round(100 * sex_change / total_professores, 2),
    perc_date = round(100 * date_change / total_professores, 2)
  )

# Removing these records 
active_cs <- active_cs %>%
  group_by(ID_INTERNO) %>% 
  mutate(
    n_color = n_distinct(ID_COR,           na.rm = TRUE),
    n_sex   = n_distinct(SEXO,             na.rm = TRUE),
    n_birth = n_distinct(DATA_NASCIMENTO,  na.rm = TRUE)
  ) %>% 
  ungroup() %>%
  filter(n_color == 1) %>%
  filter(n_sex == 1) %>%
  filter(n_birth == 1) %>%
  select(-c(n_color, n_sex, n_birth)) 

# ---- Creating column based on type of contract
# Source: https://atendimento.educacao.sp.gov.br/knowledgebase/article/SED-07857/pt-br (acsssed 05/22/2025)
active_cs <- active_cs %>%
  mutate(CATEG_corr = case_when(CATEG_E == "A" ~ "Efetivo",
                                CATEG_E == "P" | CATEG_E == "F" | CATEG_E == "N" ~ "Estável",
                                CATEG_E == "O" ~ "Temporário",
                                CATEG_E == "D" ~ "NI",
                                CATEG_E == "C" ~ "NI",
                                TRUE ~ NA))

# Saving data
#saveRDS(active_cs, file = paste0(getwd(), "/1_data/active_cs.rds"))

# --------------------------------------
# -------- Absenteeism data ------------
#---------------------------------------
# Reading the files names
absence_files <- list.files(path = paste0(getwd(), "/0_raw/absences"), pattern = ".csv", full.names = TRUE, recursive = TRUE)


# Cleaning and combining into a single data frame
absences <- rbindlist(
  lapply(absence_files, function(f) {
    dt <- fread(f)                                                                               # read 
    dt <- dt[dt$QUADRO_EXERC=="QM-DOCENTE" | dt$QUADRO_EXERC=="QM-SUPORTE",]                     # filtering only teachers
    dt <- dt[dt$CIE_ESCOLA!=0,]                                                                  # removing teachers not assigned to schools 
    dt[, DATE := str_extract(basename(f), "(\\d{2})(\\d{2})")]                                   # extracting month and year from file name
    dt[, YEAR := as.integer(paste0("20", str_extract(basename(f), "\\d{4}") %>% substr(3, 4)))]   # creating year variable
    setnames(dt, toupper(names(dt)))                                                             # UPPER-CASE the column names
    dt <- unique(dt)                                                                             # remove duplicities 
    dt <- select(dt, YEAR, DATE, ID_INTERNO, UA_EXERC, CIE_ESCOLA,                               # select columns
                 TT_DIAS_FALTA_MEDICA, TT_DIAS_FALTA_JUST, TT_DIAS_FALTA_INJUST,    
                 TT_DIAS_LIC_PREMIO, TT_DIAS_LIC_GESTANTE, TT_DIAS_LIC_ACID_TRAB,    
                 TT_DIAS_LIC_INTER_PARTIC, TOT_DIAS_AUSENCIAS, TOTAL_DIAS_MES)
    dt                                                                                     
  }),
  use.names = TRUE, fill = TRUE         
)

# ---- Dealing with inconsistencies: teachers who appear more than once per month
# How frequently do they occur in the data?
absences %>%
  group_by(YEAR, DATE, ID_INTERNO, CIE_ESCOLA) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(mais_de_uma_vez = n > 1) %>%
  group_by(YEAR) %>%
  summarise(
    total_professores = n(),
    mais_de_uma_vez = sum(mais_de_uma_vez),
    perc_mais_de_uma_vez = round(100 * mais_de_uma_vez / total_professores, 2)
  )


# Removing these inconsistent records
absences <- absences %>%
  group_by(YEAR, DATE, ID_INTERNO, CIE_ESCOLA) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  rename(UAC = UA_EXERC)


# Saving data
#saveRDS(absences, file = paste0(getwd(), "/1_data/absences.rds"))

# ---------------------------------------
# ------- Teachers' education data ------
#----------------------------------------
# No information for 05-2021

# Reading the files names
education_files <- list.files(path = paste0(getwd(), "/0_raw/teacher-education"), pattern = ".csv", full.names = TRUE, recursive = TRUE)

# Cleaning and combining into a single data frame
education <- rbindlist(
  lapply(education_files, function(f) {
    dt <- fread(f)                                                                               # read 
    dt <- dt[dt$QUADRO_E=="QM",]                                                                 # filtering only teachers
    dt[, YEAR := as.integer(paste0("20", str_extract(basename(f), "\\d{4}") |> substr(3, 4)))]   # creating year variable
    dt[, DATE := str_extract(basename(f), "(\\d{2})(\\d{2})")]                                   # extracting month and year from file name
    dt <- dt[dt$CIE_ESCOLA!=0,]                                                                  # removing teachers not assigned to schools 
    setnames(dt, toupper(names(dt)))                                                             # UPPER-CASE the column names
    dt <- select(dt, YEAR, DATE, ID_INTERNO, CIE_ESCOLA, UA_EXERC, FORMACAO)                                           # selecting columns
    dt <- unique(dt)                                                                             # remove duplicities 
    dt                                                                                     
  }),
  use.names = TRUE, fill = TRUE         
)

# Separating info on education
education <- education %>%
  mutate(FORMACAO = ifelse(FORMACAO=="S/INFO", NA, FORMACAO)) %>%
  separate("FORMACAO", paste("FORMACAO", 1:5, sep="_"), sep= "\\+", extra= "drop") %>%
  mutate(FORMACAO_1 = trimws(FORMACAO_1),
         FORMACAO_2 = trimws(FORMACAO_2),
         FORMACAO_3 = trimws(FORMACAO_3),
         FORMACAO_4 = trimws(FORMACAO_4),
         FORMACAO_5 = trimws(FORMACAO_5)) 

# Creating column of level of education
# Defining levels 
level_names <- c("sem ensino superior",
                 "com ensino superior",
                 "especialização",
                 "mestrado",
                 "doutorado")

educ_levels <- c(
  "ENSINO MÉDIO"              = "sem ensino superior",
  "ENSINO MÉDIO-TÉCNICO"      = "sem ensino superior",
  "APERF/EXTENSÃO CULTURAL"   = "sem ensino superior",
  "LICENCIATURA-CURTA"        = "com ensino superior",
  "LICENCIATURA-PLENA"        = "com ensino superior",
  "BACHARELADO/TECNÓLOGO"     = "com ensino superior",
  "ESPECIALIZAÇÃO"            = "especialização",
  "MESTRADO"                  = "mestrado",
  "DOUTORADO"                 = "doutorado"
)

education <- education %>%
  ## create *_cat versions with the 5-level classification 
  mutate(across(starts_with("FORMACAO_"), ~ educ_levels[.x], .names = "{.col}_cat")) %>%
  
  ## pick the highest education per row
  mutate(
    NIVEL_FORMACAO = factor(
      level_names[
        do.call(
          pmax.int,
          c(                                          # turn each column into its position in lvl_names
            select(., ends_with("_cat")) %>%           # only *_cat columns
              lapply(match, table = level_names),       # fast integer look-up
            list(na.rm = TRUE)
          )
        )
      ],
      levels = level_names,
      ordered = TRUE
    )
  ) %>%
  
  ## drop the helper *_cat columns after QC
  select(-ends_with("_cat")) %>%
  
  # removing duplicates 
  distinct(YEAR, DATE, ID_INTERNO, CIE_ESCOLA, UA_EXERC, FORMACAO_1, FORMACAO_2, FORMACAO_3, FORMACAO_4, FORMACAO_5, NIVEL_FORMACAO, .keep_all = TRUE)

# ---- Dealing with inconsistencies: teachers who appear more than once per month
# How frequently do they occur in the data?
education %>%
  group_by(YEAR, DATE, ID_INTERNO, CIE_ESCOLA) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(mais_de_uma_vez = n > 1) %>%
  group_by(YEAR) %>%
  summarise(
    total_professores = n(),
    mais_de_uma_vez = sum(mais_de_uma_vez),
    perc_mais_de_uma_vez = round(100 * mais_de_uma_vez / total_professores, 2)
  )

# Removing these inconsistent records
education <- education %>%
  group_by(YEAR, DATE, ID_INTERNO, CIE_ESCOLA) %>%
  filter(n() == 1) %>%
  ungroup() 

# ---- Removing records with no information on education
education <- education %>%
  filter(!is.na(FORMACAO_1)) 

# Renaming UAC column 
setnames(education, "UA_EXERC", "UAC")

# Saving data
#saveRDS(education, file = paste0(getwd(), "/1_data/education.rds"))

# ---------------------------------------
# ------------- Dropout rates -----------
#----------------------------------------
# Reading the files names
dropout_files <- list.files(path = paste0(getwd(), "/0_raw/dropout"), pattern = ".xlsx", full.names = TRUE, recursive = TRUE)

# Cleaning and combining into a single data frame
dropout <- rbindlist(
  lapply(dropout_files, function(f) {
    dt <- read_excel(f, skip = 8)                                                                               # read 
    dt <- dt[dt$SG_UF=="SP",]                                                                                   # filtering only SP
    dt <- dt[dt$NO_DEPENDENCIA != "Privada",]                                                                                   # filtering only public schools
    setnames(dt, toupper(names(dt)))                                                                            # UPPER-CASE the column names                                                                     
    dt <- select(dt, NU_ANO_CENSO, CO_MUNICIPIO, CO_ENTIDADE, NO_CATEGORIA, NO_DEPENDENCIA, `3_CAT_FUN`, `3_CAT_MED`)                                           # selecting columns
    setnames(dt, "NU_ANO_CENSO", "YEAR")
    setnames(dt, "CO_ENTIDADE", "CIE_ESCOLA")
    dt$CIE_ESCOLA <- as.character(dt$CIE_ESCOLA)
    dt                                                                                     
  }),
  use.names = TRUE, fill = TRUE         
)

# Fixing numeric columns
dropout$`3_CAT_FUN` <- as.numeric(dropout$`3_CAT_FUN`)
dropout$`3_CAT_MED` <- as.numeric(dropout$`3_CAT_MED`)
dropout$drop_mean <- rowMeans(dropout[, c("3_CAT_FUN", "3_CAT_MED")], na.rm = TRUE)

# Saving data
#saveRDS(dropout, file = paste0(getwd(), "/1_data/dropout.rds"))

# ---------------------------------------
# ------------- School profile ----------
#----------------------------------------
# Reading the files
censo_files <- list.files(path = paste0(getwd(), "/0_raw/censo"), pattern = ".csv", full.names = TRUE)

# Cleaning and combining the files
censo <- rbindlist(
  lapply(censo_files, function(f) {
    dt <- fread(f)                                                                               # read 
    dt <- dt[dt$CO_UF==35,]                                                                      # only SP schools
    dt <- dt[dt$TP_DEPENDENCIA==2,]                                                              # only state schools
    setnames(dt, "NU_ANO_CENSO", "YEAR")
    setnames(dt, "CO_ENTIDADE", "CIE_ESCOLA")
    setnames(dt, toupper(names(dt)))                                                             # UPPER-CASE the column names
    dt[, QT_MAT_FUND_MED := rowSums(dt[, c("QT_MAT_FUND", "QT_MAT_MED")], na.rm = TRUE)]
    dt <- select(dt, YEAR, CIE_ESCOLA, QT_MAT_FUND, QT_MAT_MED, QT_MAT_BAS_PRETA, QT_MAT_FUND_MED)
    dt <- dt[dt$QT_MAT_FUND_MED>0,]                                                              # only primary and secondary schools
    dt$CIE_ESCOLA <- as.character(dt$CIE_ESCOLA)
    dt                                                                                     
  }),
  use.names = TRUE, fill = TRUE         
)

# ---------------------------------------
# ------------- INSE  -------------------
#----------------------------------------
inse <- read_excel("0_raw/inse/INSE_2019_ESCOLAS.xlsx") %>%
  filter(CO_UF == 35 & TP_DEPENDENCIA == 2) %>%
  mutate(YEAR = "2019",
         CIE_ESCOLA = as.character(CO_ESCOLA),
         INSE_VALOR_ABSOLUTO_2019 = INSE_VALOR_ABSOLUTO) %>%
  select(CIE_ESCOLA, INSE_VALOR_ABSOLUTO_2019)

# ---------------------------------------
# ------- Merging final data ------------
#----------------------------------------

# Goal: measure how many active-civil-servant records (active_cs)
#       have matching information in the absences and education tables
active_cs %>%
  left_join(education, by = c("ID_INTERNO", "YEAR", "DATE", "UAC")) %>%
  left_join(absences, by = c("ID_INTERNO", "YEAR",  "DATE", "UAC")) %>%
  mutate(match1 = !is.na(FORMACAO_1),
         match2 = !is.na(TOTAL_DIAS_MES),
         match3 = !is.na(TOTAL_DIAS_MES) & !is.na(FORMACAO_1)) %>%
  group_by(YEAR) %>%
  summarise(
    total_professores = n(),
    education = sum(match1==TRUE),
    perc_education = round(100 * education / total_professores, 2),
    absence = sum(match2==TRUE),
    perc_absence = round(100 * absence / total_professores, 2),
    both = sum(match3==TRUE),
    perc_both = round(100 * both / total_professores, 2)
  )

# ---------------------------------------
# ------- Final teacher-level panel -----
#----------------------------------------

# ------- Unbalanced panel
df_teacher_level_unb <- active_cs %>%
  inner_join(education, by = c("ID_INTERNO", "YEAR", "DATE", "UAC")) %>%            # return only teachers with info on education
  left_join(absences %>% select(-CIE_ESCOLA), by = c("ID_INTERNO", "YEAR",  "DATE", "UAC"))        # return all teachers and info on absences for those who have
  
  
# Removing unused columns
df_teacher_level_unb <- df_teacher_level_unb %>%
  select(-c(CATEG_E, UAC))

# Fixing school codes to match those in INEP databases
setDT(df_teacher_level_unb)

df_teacher_level_unb[, CIE_ESCOLA := substr(
  paste0("00000000", CIE_ESCOLA), 
  nchar(paste0("00000000", CIE_ESCOLA)) - 7, 
  nchar(paste0("00000000", CIE_ESCOLA))
)]

df_teacher_level_unb[, CIE_ESCOLA := sub("^..", "35", CIE_ESCOLA)]

# ------- Joining with dropout and censo info to get information on schools

# Goal: measure how many records in the panel (df_teacher_level_unb)
#       have matching information in the dropout and censo tables
df_teacher_level_unb %>%
  left_join(dropout, by = c("CIE_ESCOLA", "YEAR")) %>%
  left_join(censo, by = c("CIE_ESCOLA", "YEAR")) %>%
  left_join(inse, by = c("CIE_ESCOLA")) %>%
  mutate(match1 = !is.na(CO_MUNICIPIO),
         match2 = !is.na(QT_MAT_FUND_MED),
         match3 = !is.na(INSE_VALOR_ABSOLUTO_2019)) %>%
  group_by(YEAR) %>%
  summarise(
    total_professores = n(),
    dropout = sum(match1==TRUE),
    perc_dropout = round(100 * dropout / total_professores, 2),
    censo = sum(match2==TRUE),
    perc_censo = round(100 * censo / total_professores, 2),
    inse = sum(match3==TRUE),
    perc_inse = round(100 * inse / total_professores, 2))

# Merge dropout and censo with panel data
df_teacher_level_unb <- df_teacher_level_unb %>%
  inner_join(dropout, by = c("CIE_ESCOLA", "YEAR")) %>%
  inner_join(censo, by = c("CIE_ESCOLA", "YEAR")) %>%
  inner_join(inse, by = c("CIE_ESCOLA"))

# Replace info on absences with NA for those teachers who are not in the absences table 
df_teacher_level_unb <- df_teacher_level_unb %>% 
  mutate(across(matches("^(TT|TOT)"), ~ replace_na(.x, 0)))

# Getting number of days in month 
df_teacher_level_unb$TOTAL_DIAS_MES <- days_in_month(as.Date(paste0("01", df_teacher_level_unb$DATE), format = "%d%m%y"))

# Absence rate
df_teacher_level_unb[, ABS_RATE := TOT_DIAS_AUSENCIAS / TOTAL_DIAS_MES]

# Saving data
saveRDS(df_teacher_level_unb, file = paste0(getwd(), "/1_data/df_teacher_level_unb.rds"))

# ------- Balanced panel
# Panel length
panel_length<-n_distinct(df_teacher_level_unb$DATE)

# Keep only teachers who appear in every month
df_teacher_level_bal <- df_teacher_level_unb %>%
  group_by(ID_INTERNO) %>%
  mutate(n = n_distinct(DATE)) %>%
  filter(n == panel_length) %>%
  select(-n) %>%
  ungroup()

# Check 
df_teacher_level_bal %>% 
  group_by(YEAR) %>% 
  summarise(n_teachers = n_distinct(ID_INTERNO))

# Proportion of rows retained after balancing
nrow(df_teacher_level_bal) / nrow(df_teacher_level_unb)

# Saving data
saveRDS(df_teacher_level_bal, file = paste0(getwd(), "/1_data/df_teacher_level_bal.rds"))

# ---------------------------------------
# ------- Final school-level panel ------
#----------------------------------------

# ---- School-level panel unbalanced 
df_school_level_unb <- df_teacher_level_unb %>%
  group_by(YEAR, CIE_ESCOLA, CO_MUNICIPIO, NO_CATEGORIA) %>%
  summarise(
    n_prof          = n_distinct(ID_INTERNO),
    
    ## Ausências
    dias_aus_totais = sum(TOT_DIAS_AUSENCIAS, na.rm = TRUE),
    dias_aus_medio_ano = dias_aus_totais / 365,
    dias_aus_por_prof = dias_aus_totais/n_prof,
    prop_prof_10dias = mean(TOT_DIAS_AUSENCIAS >= 10, na.rm = TRUE),
    abs_rate_medio  = mean(ABS_RATE, na.rm = TRUE),
    
    ## Estrutura do quadro docente 
    n_temp        = n_distinct(ID_INTERNO[CATEG_corr == "Temporário"]),
    n_efet        = n_distinct(ID_INTERNO[CATEG_corr == "Efetivo"]),
    prop_temp       = mean(CATEG_corr == "Temporário",  na.rm = TRUE),
    prop_efetivo    = mean(CATEG_corr == "Efetivo",    na.rm = TRUE),
    idade_media     = mean(IDADE, na.rm = TRUE),
    exp_media       = mean(ANOS_ATIVIDADE, na.rm = TRUE),
    perc_prof_fem        = mean(SEXO == "F", na.rm = TRUE),
    perc_prof_pretos     = mean(ID_COR == "P", na.rm = TRUE),
    perc_prof_ens_sup    = mean(NIVEL_FORMACAO == "com ensino superior", na.rm = TRUE),
    perc_prof_mes        = mean(NIVEL_FORMACAO == "mestrado", na.rm = TRUE),
    perc_prof_dou        = mean(NIVEL_FORMACAO == "doutorado", na.rm = TRUE),
    
    # Dropout 
    drop_mean = mean(drop_mean, na.rm = TRUE),
    qt_mat = mean(QT_MAT_FUND_MED, na.rm = TRUE),
    qt_mat_preta = mean(QT_MAT_BAS_PRETA, na.rm = TRUE),
    per_alu_pretos = qt_mat_preta/qt_mat,
    inse = mean(INSE_VALOR_ABSOLUTO_2019, na.rm = TRUE),
    .groups = "drop"
  )

# Saving data
saveRDS(df_school_level_unb, file = paste0(getwd(), "/1_data/df_school_level_unb.rds"))


# -------- School-level panel balanced

# Keep only schools that appear in every year
df_school_level_bal <- df_school_level_unb %>%
  group_by(CIE_ESCOLA) %>%
  mutate(n = n_distinct(YEAR)) %>%
  filter(n == 3) %>%
  select(-n) %>%
  ungroup()

# Check 
df_school_level_bal %>% 
  group_by(YEAR) %>% 
  summarise(n = n_distinct(CIE_ESCOLA))

# Saving data
saveRDS(df_school_level_bal, file = paste0(getwd(), "/1_data/df_school_level_bal.rds"))


