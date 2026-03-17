# Teacher Absences and School Dropout in São Paulo (in progress)

## Overview

This repository contains scripts and documentation for cleaning, merging, and analyzing a database of teacher absences in São Paulo’s state schools. My goal is to estimate the relationship between teacher absenteeism and student dropout. In particular, I aim to address whether the effect differs between permanent civil servants and contract teachers, and between urban and rural schools.

## Research Questions & Hypothesis

* **RQ 1**  Does teacher absenteeism increase school dropout?
* **RQ 2**  Does the magnitude of the effect vary with teacher contract type and school location?
* 
## Data

| Level   | Source                                                                                                                        | Years     | Key fields                                |
| ------- | ----------------------------------------------------------------------------------------------------------------------------- | --------- | ----------------------------------------- |
| Teacher | [Ausências por Servidor](https://dados.educacao.sp.gov.br/dataset/aus%C3%AAncias-por-servidor)                                | 2021–2023 | Monthly absence days by motive            |
| Teacher | [Formação por Servidor](https://dados.educacao.sp.gov.br/dataset/forma%C3%A7%C3%A3o-por-servidor)                             | 2021–2023 | Education credentials                     |
| Teacher | [Servidores Ativos por Unidade](https://dados.educacao.sp.gov.br/dataset/servidores-ativos-por-unidade)                       | 2021–2023 | Employment history and socioeconomic info |
| School  | [INEP](https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/indicadores-educacionais/nivel-socioeconomico)         | 2019      | Socioeconomic index (INSE)                |
| School  | [INEP](https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/indicadores-educacionais/taxas-de-rendimento-escolar)  | 2021–2023 | Lower & upper secondary dropout           |
| School  | [Censo Escolar](https://www.gov.br/inep/pt-br/areas-de-atuacao/pesquisas-estatisticas-e-indicadores/censo-escolar/resultados) | 2021–2023 | Enrollment, location                      |

## Repository Layout

```
.
├── 0_raw/           # Original downloads (not tracked by Git)
├── 1_data/          # Cleaned & merged panel data (only school-level)
├── 2_analysis/            # Reproducible R scripts
│   ├── scripts/
│   ├── outputs/
├── 3_slides/
├── 4_paper/              # Working draft (LaTeX)
└── README.md             # You are here
```
