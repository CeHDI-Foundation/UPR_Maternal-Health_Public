source(here("code", "04a_analysis_MLM_forest_MH.R"))
source(here("code", "04b_analysis_MLM_forest_SBA.R"))
source(here("code", "04c_analysis_MLM_forest_CPR.R"))

p_labels <- forest_plot_data_MLM_MMR |> 
  # filter(model == "Multivariable") |> 
  filter(model == "Univariable") |> 
  ggplot(aes(y = term))+geom_blank()+
  theme_classic()+
  theme(axis.title.y = element_blank(), axis.text.y = element_text(size = 9))

p_MMR <- p_forest_enhanced_MMR_MLM+
  ggtitle("% change\nin MMR*")+
  theme(
    axis.text.y = element_blank(), 
    axis.title.x = element_text(size = 9),
    legend.position = "none", 
    plot.title = element_text(vjust =0, hjust = 0.5, size = 9))
annot_MMR1 <-  forest_plot_data_MLM_MMR |> 
  filter(model == "Univariable") |> 
  ggplot(aes(y = term, color = sig_label))+
  geom_text(aes(x = 0, label = paste0(label_text_uv_p, p_symbol)),
            hjust = 0.5, size = 3, fontface = "plain")+
  labs(title = "Univariable\nEstimate [95% CI]")+
  scale_x_continuous(
    expand = expansion(mult = c(0, 0)) # Expand right side to fit text
  ) +
  scale_color_manual(values = c("Not Significant" = "grey0", "Significant" = "#D55E00")) +
  coord_cartesian(
    clip = "off")+
  theme_void()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 9),
    legend.position = "none"
  )
annot_MMR2 <-  forest_plot_data_MLM_MMR |> 
  filter(model == "Multivariable") |> 
  ggplot(aes(y = term, color = sig_label))+
  geom_text(aes(x = 0, label = paste0(label_text_mv_p, p_symbol)),
            hjust = 0.5, size = 3, fontface = "plain")+
  labs(title = "Multivariable\nEstimate [95% CI]")+
  scale_x_continuous(
    expand = expansion(mult = c(0, 0)) # Expand right side to fit text
  ) +
  scale_color_manual(values = c("Not Significant" = "grey0", "Significant" = "#D55E00")) +
  coord_cartesian(
    clip = "off")+
  theme_void()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 9),
    legend.position = "none"
  )

p_SBA <- p_forest_enhanced_SBA_MLM+
  ggtitle("% change\nin odds of SBA*")+
  theme(axis.text.y = element_blank(), 
        axis.title.x = element_text(size = 9),
        legend.position = "none", plot.title = element_text(vjust = 0, hjust = 0.5, size = 9))
annot_SBA1 <-  forest_plot_data_MLM_SBA |> 
  filter(model == "Univariable") |> 
  ggplot(aes(y = term, color = sig_label))+
  geom_text(aes(x = 0, label = paste0(label_text_uv_p, p_symbol)),
            hjust = 0.5, size = 3, fontface = "plain")+
  labs(title = "Univariable\nEstimate [95% CI]")+
  scale_x_continuous(
    expand = expansion(mult = c(0, 0)) # Expand right side to fit text
  ) +
  scale_color_manual(values = c("Not Significant" = "grey0", "Significant" = "#D55E00")) +
  coord_cartesian(
    clip = "off")+
  theme_void()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 9),
    legend.position = "none"
  )
annot_SBA2 <-  forest_plot_data_MLM_SBA |> 
  filter(model == "Multivariable") |> 
  ggplot(aes(y = term, color = sig_label))+
  geom_text(aes(x = 0, label = paste0(label_text_mv_p, p_symbol)),
            hjust = 0.5, size = 3, fontface = "plain")+
  labs(title = "Multivariable\nEstimate [95% CI]")+
  scale_x_continuous(
    expand = expansion(mult = c(0, 0)) # Expand right side to fit text
  ) +
  scale_color_manual(values = c("Not Significant" = "grey0", "Significant" = "#D55E00")) +
  coord_cartesian(
    clip = "off")+
  theme_void()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 9),
    legend.position = "none"
  )

p_CPR <- p_forest_enhanced_CPR_MLM+
  ggtitle("% change\nin odds of CPR*")+
  theme(axis.text.y = element_blank(), 
        axis.title.x = element_text(size = 9),
        legend.position = "none", plot.title = element_text(vjust = 0, hjust = 0.5, size = 9))
annot_CPR1 <-  forest_plot_data_MLM_CPR |> 
  filter(model == "Univariable") |> 
  ggplot(aes(y = term, color = sig_label))+
  geom_text(aes(x = 0, label = paste0(label_text_uv_p, p_symbol)),
            hjust = 0.5, size = 3, fontface = "plain")+
  labs(title = "Univariable\nEstimate [95% CI]")+
  scale_x_continuous(
    expand = expansion(mult = c(0, 0)) # Expand right side to fit text
  ) +
  scale_color_manual(values = c("Not Significant" = "grey0", "Significant" = "#D55E00")) +
  coord_cartesian(
    clip = "off")+
  theme_void()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 9),
    legend.position = "none"
  )
annot_CPR2 <-  forest_plot_data_MLM_CPR |> 
  filter(model == "Multivariable") |> 
  ggplot(aes(y = term, color = sig_label))+
  geom_text(aes(x = 0, label = paste0(label_text_mv_p, p_symbol)),
            hjust = 0.5, size = 3, fontface = "plain")+
  labs(title = "Multivariable\nEstimate [95% CI]")+
  scale_x_continuous(
    expand = expansion(mult = c(0, 0)) # Expand right side to fit text
  ) +
  scale_color_manual(values = c("Not Significant" = "grey0", "Significant" = "#D55E00")) +
  coord_cartesian(
    clip = "off")+
  theme_void()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 9),
    legend.position = "none"
  )


p_labels+ p_MMR+p_SBA+p_CPR +plot_layout(widths = c(0,2,2,2))

p_labels+ p_MMR + annot_MMR2 +p_SBA + annot_SBA2 + p_CPR + annot_CPR2 + plot_layout(widths = c(0,.8,1,.8,1,.8,1))
ggsave(here("output", "publication_figures", "MLM_forest_plot_recs_all_mv.svg"), 
       width = 10, height = 5, dpi = 300)

p_labels+ p_MMR + annot_MMR1 +p_SBA + annot_SBA1 + p_CPR + annot_CPR1 + plot_layout(widths = c(0,.8,1,.8,1,.8,1))
ggsave(here("output", "publication_figures", "MLM_forest_plot_recs_all_uv.svg"), 
       width = 10, height = 5, dpi = 300)

p_labels+ p_MMR + annot_MMR1 + annot_MMR2 +p_SBA + annot_SBA1 + annot_SBA2 + p_CPR + annot_CPR1 + annot_CPR2 + plot_layout(widths = c(0,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8))+
  plot_annotation(caption = "*per additional recommendation, per year")

ggsave(here("output", "publication_figures", paste0("MLM_forest_plot_recs_all_estimates_", Sys.Date(),".png")), 
       width = 13, height = 4, dpi = 300)
