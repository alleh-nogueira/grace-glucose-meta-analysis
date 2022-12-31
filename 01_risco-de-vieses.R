options(digits = 2)

if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, cowplot, readxl)

quips <- read_excel("data-quips.xlsx") |>
mutate(study = paste0(study, " (", publication_year, ")"), .keep = "unused") |>
pivot_longer(!study, names_to = "domain", values_to = "bias_risk") |>
mutate(
    domain = factor(
        domain,
        labels = c(sapply(1:6, function(x) paste0("D", x)), "Global"),
        levels = c(
            "study_participation",
            "study_attrition",
            "prognostic_factor_measurement",
            "outcome_measurement",
            "study_confouding",
            "statistical_analysis_and_reporting",
            "overall_rob"
        )
    ),
    domain_description = factor(
        domain,
        levels = c(sapply(1:6, function(x) paste0("D", x)), "Global"),
        labels = c(
            "Participação no Estudo (D1)",
            "Atrito no Estudo (D2)",
            "Aferição do Fator Prognóstico (D3)",
            "Aferição do Desfecho (D4)",
            "Confundimento no Estudo (D5)",
            "Análise e Relatório Estatístico (D6)",
            "Risco Global de Vieses"
        )
    ),
    bias_risk = factor(
        bias_risk,
        levels = c("High", "Moderate", "Low", "Unclear"),
        labels = c("Alto", "Moderado", "Baixo", "Obscuro")
    )
)


rob_per_study <- ggplot(quips, aes(x = domain, y = study, fill = bias_risk)) +
geom_tile(colour = "white", size = 1.5, stat = "identity") +
scale_fill_manual(
values = c("#3B2E4D", "#6C548C", "#9C89B8", "#ADB5D7"),
limits = c("Alto", "Moderado", "Baixo", "Obscuro")
) +
scale_x_discrete(position = "top") +
theme_cowplot() +
theme(
panel.background = element_rect(fill = "white"),
plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "in"),
plot.background = element_rect(color = "black"),
axis.title = element_text(face = "bold"),
axis.ticks = element_blank(),
axis.line = element_blank(),
legend.position = "bottom",
legend.justification = "center"
) +
labs(x = "Domínio\n", y = "Estudo\n", fill = "Risco de Vieses")

rob_per_domain <- quips |>
group_by(domain_description, bias_risk) |>
summarise(count = length(study)) |>
group_by(domain_description) |>
mutate(proportion = round(count / sum(count), 2)) |>
ggplot(aes(x = proportion, y = domain_description, fill = bias_risk)) +
geom_col(position = "fill", width = 0.5) +
scale_fill_manual(
values = c("#3B2E4D", "#6C548C", "#9C89B8", "#ADB5D7"),
limits = c("Alto", "Moderado", "Baixo", "Obscuro")
) +
geom_text(
aes(label = paste0(scales::percent(proportion), " (", count, ")")),
position = position_stack(0.5),
color = "white",
fontface = "bold",
size = 4,
show.legend = FALSE
) +
labs(
x = "\nProporção de Estudos (Número)",
y = "Domínio\n", fill = "Risco de Vieses"
) +
scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
scale_y_discrete(limits = rev) +
theme_cowplot() +
theme(
panel.background = element_rect(fill = "white"),
plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "in"),
plot.background = element_rect(color = "black"),
axis.title = element_text(face = "bold"),
axis.ticks.y = element_blank(),
axis.line.y = element_blank(),
legend.position = "top",
legend.justification = "center"
)

plot_grid(
    rob_per_domain, rob_per_study,
    labels = "AUTO", rel_widths = c(1.25, 1)
)
ggsave("figura-2.pdf", height = 9, width = 18)