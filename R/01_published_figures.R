# Figures built only from numbers reported in the published article ---------------
# Source: Luvián-Morales J, et al. Front Nutr 2026;13:1835417 (CC BY 4.0).

# 1. Participant flow (CONSORT style)
box <- function(x, y, w, h, label, fill = "#f8f9fa") {
  list(annotate("rect", xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2,
                fill = fill, colour = "#5f6368"),
       annotate("text", x = x, y = y, label = label, size = 3.4, lineheight = 0.95))
}
arrow_v <- function(x, y1, y2) annotate("segment", x = x, xend = x, y = y1, yend = y2,
                                        arrow = arrow(length = unit(0.18, "cm")), colour = "#5f6368")
arrow_h <- function(x1, x2, y) annotate("segment", x = x1, xend = x2, y = y, yend = y,
                                        arrow = arrow(length = unit(0.18, "cm")), colour = "#5f6368")
p_flow <- ggplot() +
  box(3, 10, 3.4, 0.9, "Assessed for eligibility\nn = 204") +
  box(7.6, 8.6, 4.2, 1.5, "Excluded: n = 68\nimpaired renal function 22%\nunable or unwilling to follow the diet 16.1%\nmissed nutrition consultation 10.2%\nother reasons 51.7%") +
  box(3, 7.2, 3.4, 1.0, "Randomised (blocked by comorbidity)\nn = 136", fill = "#e8f0fe") +
  box(7.6, 5.7, 4.2, 1.1, "Eliminated or did not complete\nall five visits: n = 53") +
  box(1.6, 4.0, 2.8, 1.0, "Anti-inflammatory diet\nanalysed: n = 41", fill = "#fce8e6") +
  box(4.6, 4.0, 2.8, 1.0, "Low-residue diet\nanalysed: n = 42", fill = "#e6f4ea") +
  arrow_v(3, 9.55, 7.7) + arrow_h(3, 5.5, 8.6) + arrow_v(3, 6.7, 5.2) + arrow_h(3, 5.5, 5.7) +
  annotate("segment", x = 1.6, xend = 4.6, y = 5.2, yend = 5.2, colour = "#5f6368") +
  arrow_v(1.6, 5.2, 4.5) + arrow_v(4.6, 5.2, 4.5) +
  coord_cartesian(xlim = c(0, 10), ylim = c(3.3, 10.6)) + theme_void() +
  labs(title = "Participant flow", subtitle = "Numbers as reported in the published article")
ggsave("figures/participant_flow.png", p_flow, width = 8.5, height = 5.6, dpi = 150, bg = "white")

# 2. Effect sizes reported in the article: anti-inflammatory vs low-residue diet
#    Labels use the published values exactly as printed (read as text, no rounding).
es <- read_csv("results/published/effect_sizes.csv", show_col_types = FALSE,
               col_types = cols(.default = col_character())) |>
  mutate(across(c(estimate, lower, upper), as.numeric, .names = "{.col}_num"),
         label = sprintf("%s (%s–%s)   p %s", estimate, lower, upper,
                         ifelse(grepl("^[<>]", p), p, paste("=", p))),
         outcome = factor(outcome, rev(outcome)))
p_left <- ggplot(es, aes(estimate_num, outcome)) +
  geom_vline(xintercept = 1, colour = "#5f6368") +
  geom_errorbar(aes(xmin = lower_num, xmax = upper_num), width = 0.25, orientation = "y", colour = "#1a73e8") +
  geom_point(size = 2.6, colour = "#1a73e8") +
  scale_x_log10(breaks = c(0.1, 0.3, 1, 3)) +
  labs(x = "Relative risk, anti-inflammatory vs low-residue diet (log scale)", y = NULL) +
  theme(panel.grid.minor = element_blank())
p_right <- ggplot(es, aes(0, outcome, label = label)) +
  geom_text(hjust = 0, size = 3.3, colour = "#3c4043") +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  theme_void() + labs(title = "RR (95% CI)") +
  theme(plot.title = element_text(size = 10, colour = "#5f6368"))
p_es <- patchwork::wrap_plots(p_left, p_right, widths = c(1.6, 1)) +
  patchwork::plot_annotation(
    title = "Effect sizes reported in the article",
    subtitle = "RR below 1: lower risk with the anti-inflammatory diet. For complete response, RR above 1 means more responses with it.")
ggsave("figures/published_effect_sizes.png", p_es, width = 10, height = 4.6, dpi = 150, bg = "white")
cat("Published-result figures saved
")
