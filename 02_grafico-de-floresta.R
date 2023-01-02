options(digits = 2)

if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, cowplot, readxl, meta)

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

pdf("figura-3.pdf", width = 12, height = 5)
model_meta |>
forest(
    random = FALSE,
    fixed = TRUE,
    digits = 2,
    digits.pval = 3,
    digits.se = 2,
    sortvar = TE,
    print.tau2 = FALSE,
    colgap.studlab = "7 mm",
    colgap.forest = "7 mm",
    leftlabs = c("Estudo", "Média", "DP", "Total", "Média", "DP", "Total"),
    rightlabs = c("DM", "[IC 95%]", "Peso"),
    label.e = "GRACE-Glicose",
    label.c = "GRACE",
    label.right = "Favorece\nGRACE-Glicose\n",
    label.left = "Favorece\nGRACE\n",
    smlab = "Diferença Média na ASCROC",
    test.overall = TRUE,
    col.square = "#9C89B8",
    col.diamond = "black",
    col.square.lines = NA
)
dev.off()