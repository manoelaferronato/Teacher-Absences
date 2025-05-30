# ==========================================================================================
# Descriptive statistics
# Author: Manoela do Amaral Ferronato
# Date: 05/2025
# ==========================================================================================

# -------- Loading packages ----------------------------------------------------------------
library(data.table)
library(tidyverse)
library(stringr)
library(readxl)
library(kableExtra)
library(knitr)

# -------- Table 1: Descriptive statistics of teachers by contract type --------------------

# Selecting only civil-servant and contract teachers
teacher_level_unb <- readRDS("1_data/teacher_level_unb.rds") %>%
  filter(!CATEG_corr %in% c("NI", "Estável"))

# Function to calculate mean, means difference and standard-error of means differences
stats <- function(var, name = var) {
  dt <- teacher_level_unb[,
    .(
      mean  = mean(eval(parse(text = var)), na.rm = TRUE),
      sd    = sd(eval(parse(text = var)), na.rm = TRUE),
      n     = .N
    ),
    by = CATEG_corr
  ]
  
  dt[, .(
    Var = name,
    Efetivo = mean[CATEG_corr == "Efetivo"],
    Temporário = mean[CATEG_corr == "Temporário"],
    Diff = diff(mean),
    SE_Diff = sqrt(sum((sd^2) / n))
  )]
}

# Selecting variables          
variables <- list(IDADE = "Age",
                  ANOS_ATIVIDADE = "Experience",
                  `as.numeric(SEXO == "F")` = "Women",
                  `as.numeric(NIVEL_FORMACAO %in% "com ensino superior")` = "BA degree",
                  `as.numeric(NIVEL_FORMACAO %in% c("especialização", "mestrado", "doutorado"))` = "MA (or other) degree",
                  ABS_RATE = "Absence rate")

# Execute function in selected variables
tab1 <- rbindlist(
  Map(stats, names(variables), variables)
)

# Calculating proportion of absences by motive in a temp table
tab_temp <- teacher_level_unb[TOT_DIAS_AUSENCIAS > 0, .(
  TT_medica     = sum(TT_DIAS_FALTA_MEDICA,     na.rm = TRUE),
  TT_just       = sum(TT_DIAS_FALTA_JUST,       na.rm = TRUE),
  TT_injust     = sum(TT_DIAS_FALTA_INJUST,     na.rm = TRUE),
  TT_premio     = sum(TT_DIAS_LIC_PREMIO,       na.rm = TRUE),
  TT_gestante   = sum(TT_DIAS_LIC_GESTANTE,     na.rm = TRUE),
  TT_acidente   = sum(TT_DIAS_LIC_ACID_TRAB,    na.rm = TRUE),
  TT_pessoal    = sum(TT_DIAS_LIC_INTER_PARTIC, na.rm = TRUE)
), by = CATEG_corr][,
  total_geral      := rowSums(.SD), .SDcols = patterns("^TT_")][
  , lapply(.SD, function(x) as.numeric(x / total_geral)), 
  by = CATEG_corr][
    , total_geral := NULL]

# Renaming
setnames(tab_temp,
         old = c("TT_medica","TT_just","TT_injust","TT_premio",
                 "TT_gestante","TT_acidente","TT_pessoal"),
         new = c("Sick leave","Justified absences","Unjustified absences",
                 "Seniority leave","Maternity leave","Accident leave","Personal leave"))
# Tidying temp table 
tab_temp <- tab_temp %>%
  pivot_longer(-CATEG_corr, 
                names_to = "Var",
                values_to = "Valor") %>%
  pivot_wider(names_from = CATEG_corr,
              values_from = Valor) 

# Binding main table and temp table
tab1 <- tab1 %>%
  rbind(tab_temp, fill = T) 

# Number of observations
tab1_n <- data.frame(
  Var = "N (teachers)",
  Efetivo = length(unique(teacher_level_unb$ID_INTERNO[
    teacher_level_unb$CATEG_corr == "Efetivo"])),
  Temporário = length(unique(teacher_level_unb$ID_INTERNO[
    teacher_level_unb$CATEG_corr == "Temporário"])),
  Diff = NA,
  SE_Diff = NA
)

# Junta com sua tabela original
tab1 <- rbind(tab1, tab1_n)

# ------- Table to Latex ----------- 

options(knitr.kable.NA = '-')

tab1 <- tab1 %>%
  kable("latex", digits = 2, booktabs = TRUE,
      col.names = c("", "Civil-servant", "Contract", "Difference", "SE"),
      caption = "Descriptive statistics of teachers by contract type",
      align = "lcccccc") %>%
  kable_styling(latex_options = c("scale_down", "hold_position")) %>%
  footnote(general_title = "Note.",
  general = "Statistics were calculated using an unbalanced monthly panel from 2021 to 2023. Data for May 2021 is unavailable. Experience refers to the number of years since the earliest recorded contract. The 'MA (or other) degree' category includes master's, specialization, and doctoral degrees.",
  footnote_as_chunk = TRUE,
  threeparttable = TRUE) %>%
  group_rows("General Characteristics", 1, 3) %>%
  group_rows("Education and Training", 4, 5) %>%
  group_rows("Absences", 6, nrow(tab1)) %>%
  row_spec(13, hline_after = TRUE)

# Saving
writeLines(tab1, "2_analysis/outputs/tab1.txt")
  
# -------- Table 2: Descriptive statistics of schools by location --------------------

# Reading school data
school_level_unb <- readRDS("1_data/school_level_unb.rds")

# Set as dt
setDT(school_level_unb)

# Pupil-teacher ratio
school_level_unb[, aluno_prof := qt_mat/n_prof]

# Function to calculate mean, means difference and standard-error of means differences
stats <- function(var, name = var) {
  dt <- school_level_unb[,
                          .(
                            mean  = mean(get(var), na.rm = TRUE),
                            sd    = sd(get(var), na.rm = TRUE),
                            n     = .N
                          ),
                          by = "NO_CATEGORIA"
  ]
  
  dt[, .(
    Var = name,
    Urban = mean[NO_CATEGORIA == "Urbana"],
    Rural = mean[NO_CATEGORIA == "Rural"],
    Diff = diff(mean),
    SE_Diff = sqrt(sum((sd^2) / n))
  )]
}

# Selecting variables          
variables <- list(n_prof = "Number of teachers",
                  idade_media = "Age",
                  exp_media = "Experience",
                  prop_temp = "Contract teachers",
                  abs_rate_medio = "Absence rate", 
                  prop_prof_10dias = "Proportion of teachers with more than 10 days of absence in a month",
                  qt_mat = "Number of students",
                  aluno_prof = "Pupil-teacher ratio",
                  drop_mean = "Dropout rate",
                  inse = "INSE")

# Execute function in selected variables
tab2 <- rbindlist(
  Map(stats, names(variables), variables)
)

# Number of observations
tab2_n <- data.frame(
  Var   = "N (schools)",
  Urban = length(unique(school_level_unb$CIE_ESCOLA[
    school_level_unb$NO_CATEGORIA == "Urbana"])),
  Rural = length(unique(school_level_unb$CIE_ESCOLA[
    school_level_unb$NO_CATEGORIA == "Rural"])),
  Diff     = NA_real_,
  SE_Diff  = NA_real_
)

# Join with original table
tab2 <- rbind(tab2, tab2_n)

# ------- Table to Latex ----------- 

tab2 <- tab2 %>%
  kable("latex", digits = 2, booktabs = TRUE,
        col.names = c("", "Urban", "Rural", "Difference", "SE"),
        caption = "Descriptive statistics of schools by location",
        align = "lcccccc") %>%
  kable_styling(latex_options = c("scale_down", "hold_position")) %>%
  footnote(general_title = "Note.",
           general = "Statistics were calculated using an unbalanced monthly panel from 2021 to 2023. Data for May 2021 is not available. Teaching experience is measured by the number of years since the earliest recorded contract. The number of students includes both primary/lower secondary and upper secondary levels. Dropout rates represent the average of lower and upper secondary school dropout rates. INSE refers to the Socioeconomic Level Indicator of each school, as calculated by INEP.",
           footnote_as_chunk = TRUE,
           threeparttable = TRUE) %>%
  group_rows("Teacher Characteristics", 1, 4) %>%
  group_rows("Absences", 5, 6) %>%
  group_rows("School Characteristics", 7, nrow(tab2)) %>%
  row_spec(10, hline_after = TRUE)

# Saving
writeLines(tab2, "2_analysis/outputs/tab2.txt")

