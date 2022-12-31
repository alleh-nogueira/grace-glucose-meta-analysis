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

meta_model <- metacont(
    data = auroc,
    studlab = study,
    n.e = n, n.c = n,
    mean.e = auroc_grace_glucose, mean.c = auroc_grace,
    sd.e = sd_auroc_grace_glucose, sd.c = sd_auroc_grace
)

pdf("figure-s1.pdf", width = 10, height = 10)
funnel(
    meta_model,
    xlab = expression(bold("Mean Difference in AUROC")),
    ylab = expression(bold("Standard Error")),
    contour = c(0.95, 0.975, 0.99),
    col.contour = c("#3B2E4D", "#6C548C", "#9C89B8"),
    alpha = 0.6
)
legend(
    x = -0.10, y = 0.08,
    c("P < 0.05", "P < 0.025", "P < 0.01"),
    bty = "n",
    fill = c("#3B2E4D", "#6C548C", "#9C89B8")
)
dev.off()