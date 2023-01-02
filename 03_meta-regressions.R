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

metareg(meta_model, risk_of_bias)
plot_rob <- cbind(as.data.frame(meta_model), auroc) |>
ggplot(aes(
    x = risk_of_bias, y = TE, size = w.fixed,
    fill = risk_of_bias, color = risk_of_bias
)) +
geom_violin(
    position = position_dodge(width = 0.8),
    draw_quantiles = c(0.25, 0.5, 0.75),
    alpha = 0.3, adjust = 0.7, size = 1.2
) +
annotate("text", label = paste0("P = 0.67"), x = "Low", y = 0.065) +
labs(
    x = "", y = "Mean Difference in AUROC\n",
    fill = "Overall Risk of Bias", color = "Overall Risk of Bias"
) +
scale_fill_manual(
values = c("#9C89B8", "#6C548C"),
limits = c("Low", "Moderate")
) +
scale_color_manual(
values = c("#9C89B8", "#6C548C"),
limits = c("Low", "Moderate")
) +
theme_cowplot() +
theme(
plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "in"),
plot.background = element_rect(color = "black"),
axis.title = element_text(face = "bold"),
axis.text.x = element_blank(),
axis.ticks.x = element_blank(),
axis.line.x = element_blank(),
legend.position = "bottom",
legend.justification = "center"
) +
scale_y_continuous(
    expand = c(0, 0),
    limits = c(0.00, 0.08),
    breaks = seq(0, 0.08, by = 0.02)
)

metareg(meta_model, prop_diabetes)
plot_dm <- cbind(as.data.frame(meta_model), auroc) |>
ggplot(aes(x = prop_diabetes, y = TE, color = -prop_diabetes, size = w.fixed)) +
geom_point(alpha = 0.5, show.legend = FALSE) +
geom_smooth(method = "lm", se = FALSE, show.legend = FALSE, color = "#3B2E4D") +
scale_color_gradient(low = "#9C89B8", high = "#6C548C") +
annotate(
    "text",
    label = "y = -0.04x + 0.04\nP = 0.47",
    x = 0.75,
    y = 0.04
) +
scale_x_continuous(expand = c(0.035, 0)) +
scale_y_continuous(
    expand = c(0.035, 0),
    breaks = c(0, 0.02, 0.04, 0.06),
    limits = c(0, 0.06)
) +
theme_cowplot() +
theme(
plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "in"),
plot.background = element_rect(color = "black"),
axis.title = element_text(face = "bold"),
legend.position = "top",
legend.justification = "center"
) +
labs(
    x = "\nPrevalence of Diabetes at Admission",
    y = "Mean Difference in AUROC\n"
)

metareg(meta_model, prop_stemi)
plot_stemi <- cbind(as.data.frame(meta_model), auroc) |>
ggplot(aes(x = prop_stemi, y = TE, color = -prop_stemi, size = w.fixed)) +
geom_point(alpha = 0.5, show.legend = FALSE) +
geom_smooth(method = "lm", se = FALSE, show.legend = FALSE, color = "#3B2E4D") +
scale_color_gradient(low = "#9C89B8", high = "#6C548C") +
annotate(
    "text",
    label = "y = 0.05x - 0.01\nP = 0.36",
    x = 0.75,
    y = 0.04
) +
scale_x_continuous(expand = c(0.035, 0)) +
scale_y_continuous(
    expand = c(0.035, 0),
    breaks = c(0, 0.02, 0.04, 0.06),
    limits = c(0, 0.06)
) +
theme_cowplot() +
theme(
plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "in"),
plot.background = element_rect(color = "black"),
axis.title = element_text(face = "bold"),
legend.position = "top",
legend.justification = "center"
) +
labs(
    x = "\nPrevalence of STEMI at Admission",
    y = "Mean Difference in AUROC\n"
)

metareg(meta_model, time_frame_in_days)
plot_time <- cbind(as.data.frame(meta_model), auroc) |>
ggplot(aes(
    x = time_frame_in_days, y = TE,
    color = -time_frame_in_days, size = w.fixed
)) +
geom_point(alpha = 0.5, show.legend = FALSE) +
geom_smooth(method = "lm", se = FALSE, show.legend = FALSE, color = "#3B2E4D") +
scale_color_gradient(low = "#9C89B8", high = "#6C548C") +
annotate(
    "text",
    label = "y = 0x - 0.03\nP = 0.52",
    x = 1500,
    y = 0.04
) +
scale_x_continuous(expand = c(0.035, 0), limits = c(0, 2000)) +
scale_y_continuous(
    expand = c(0.035, 0),
    breaks = c(0, 0.02, 0.04, 0.06),
    limits = c(0, 0.06)
) +
theme_cowplot() +
theme(
plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "in"),
plot.background = element_rect(color = "black"),
axis.title = element_text(face = "bold"),
legend.position = "top",
legend.justification = "center"
) +
labs(
    x = "\nMortality Assessment Time Frame (Days)",
    y = "Mean Difference in AUROC\n"
)

pdf("figure-4.pdf", width = 12, height = 8)
plot_grid(plot_rob, plot_dm, plot_stemi, plot_time, labels = "AUTO")
dev.off()