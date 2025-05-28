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

df_teacher_level_unb <- readRDS("1_data/df_teacher_level_unb.rds")


df_teacher_level_unb[
  , .(
    n_meses                  = .N,                             # quantos registros no painel
    idade_media              = mean(IDADE, na.rm = TRUE),
    anos_atividade_2024      = max(ANOS_ATIVIDADE, na.rm = TRUE),
    sexo                     = first(na.omit(SEXO)),
    raca                     = first(na.omit(ID_COR)),
    nivel_formacao           = first(na.omit(NIVEL_FORMACAO)),
    contrato                 = first(na.omit(CATEG_corr)),
    media_dias_ausencia      = mean(TOT_DIAS_AUSENCIAS, na.rm = TRUE),
    pct_meses_com_ausencia   = 100 * mean(TOT_DIAS_AUSENCIAS > 0, na.rm = TRUE)
  ),
  by = ID_INTERNO]

