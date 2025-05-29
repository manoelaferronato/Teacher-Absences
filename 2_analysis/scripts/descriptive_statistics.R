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
  `Sick leave`     = sum(TT_DIAS_FALTA_MEDICA, na.rm = TRUE),
  `Justified absences`       = sum(TT_DIAS_FALTA_JUST, na.rm = TRUE),
  `Unjustified absences`     = sum(TT_DIAS_FALTA_INJUST, na.rm = TRUE),
  `Seniority leave` = sum(TT_DIAS_LIC_PREMIO, na.rm = TRUE),
  `Maternity leave`   = sum(TT_DIAS_LIC_GESTANTE, na.rm = TRUE),
  `Accident leave`  = sum(TT_DIAS_LIC_ACID_TRAB, na.rm = TRUE),
  `Personal leave` = sum(TT_DIAS_LIC_INTER_PARTIC, na.rm = TRUE),
  total_geral      = sum(TOT_DIAS_AUSENCIAS, na.rm = TRUE)
), by = CATEG_corr][
  , lapply(.SD, function(x) as.numeric(x / total_geral)), 
  by = CATEG_corr][
    , total_geral := NULL]

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
  group_rows("Absences", 6, nrow(tab1)) 

# Saving
writeLines(tab1, "2_analysis/outputs/tab1.txt")
         
         
