options(digits = 4)

if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, cowplot, readxl, meta, metafor, fasstr)

sd_from_ci <- function(lower_ci_limit, upper_ci_limit, sample_size){
    round(sqrt(sample_size) * (upper_ci_limit - lower_ci_limit) / 3.92, 2)
}

auroc <- read_excel("data-auroc.xlsx") |>
mutate(
    study = paste0(study, " (", publication_year, ")"),
    auroc_grace = grace_auc,
    sd_auroc_grace = sd_from_ci(grace_lower_ci, grace_upper_ci, n),
    auroc_grace_glucose = grace_glucose_auc,
    sd_auroc_grace_glucose = sd_from_ci(
        grace_glucose_lower_ci, grace_glucose_upper_ci, n
    )
)

model_meta <- metacont(
    data = auroc,
    studlab = study,
    n.e = n, n.c = n,
    mean.e = auroc_grace_glucose, mean.c = auroc_grace,
    sd.e = sd_auroc_grace_glucose, sd.c = sd_auroc_grace
)

rma(
    data = as.data.frame(model_meta),
    yi = TE,
    sei = seTE,
    slab = studlab
) |>
leave1out() |>
as.data.frame() |>
transmute(
    "Estudo" = model_meta$studlab,
    "Diferença Média na ASCROC" = estimate,
    "Erro Padrão" = se,
    "P para Efeito Geral" = pval,
    "P para Heterogeneidade" = Qp,
    "I²" = I2
) |>
fasstr::write_results("tabela-s1-leave-one-out.xlsx", digits = 3)