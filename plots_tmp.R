

library(egg)
library(grid)

p1 = phosp_3m %>% 
  select(matches("eq5d5l_q.*_delta_change$")) %>% 
  gather() %>% 
  group_by(key, value) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  group_by(key) %>% 
  mutate(
    nn = sum(n),
    prop = 100 * n / nn,
    prop = ifelse(value %in% c("No change", "Improvement"), prop * -1, prop)) %>% 
  ungroup() %>% 
  mutate(key = fct_recode(key, 
                          "Mobility" = "eq5d5l_q1_delta_change",
                          "Self-care" = "eq5d5l_q2_delta_change",
                          "Usual activities" = "eq5d5l_q3_delta_change",
                          "Pain / discomfort" = "eq5d5l_q4_delta_change",
                          "Anxiety / Depression " = "eq5d5l_q5_delta_change")
  ) %T>%
  {label_data <<- (.) %>% 
    filter(value == "Worse") %>% 
    mutate(prop_label = paste0(round_tidy(prop, 1), "%"))} %>%
  ggplot_lancet(aes(x = fct_rev(key), y = prop, fill = value)) + 
  geom_col() +
  geom_text(data = label_data, aes(label = prop_label), size = 5, colour = "white", hjust = 1.3) +
  coord_flip() + 
  scale_fill_brewer("", palette = "Blues") +
  labs(y = "Proportion (%)", x = "") +
  theme(legend.position = c(1, 1.25),
        legend.background=element_blank()) +
  ggtitle("Change in EQ5D domains (whole cohort)",
          " ")


p2 = phosp_3m %>% 
  select(crf1a_resp_support_4levels, matches("eq5d5l_q.*_delta_change$")) %>% 
  gather("key", "value", -crf1a_resp_support_4levels) %>% 
  group_by(key, value, crf1a_resp_support_4levels) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  group_by(key, crf1a_resp_support_4levels) %>% 
  mutate(
    nn = sum(n),
    prop = 100 * n / nn,
    prop = ifelse(value %in% c("No change", "Improvement"), prop * -1, prop)
  ) %>% 
  ungroup() %>% 
  mutate(
    key = fct_recode(key, 
                     "Mobility" = "eq5d5l_q1_delta_change",
                     "Self-care" = "eq5d5l_q2_delta_change",
                     "Usual activities" = "eq5d5l_q3_delta_change",
                     "Pain / discomfort" = "eq5d5l_q4_delta_change",
                     "Anxiety / Depression " = "eq5d5l_q5_delta_change")
  ) %T>%
  {label_data <<- (.) %>% 
    filter(value == "Worse") %>% 
    mutate(prop_label = paste0(round_tidy(prop, 1), "%"))} %>%
  ggplot_lancet(aes(x = fct_rev(key), y = prop, fill = value)) + 
  geom_col() +
  geom_text(data = label_data, aes(label = prop_label), size = 4, colour = "white", hjust = 1.1) +
  coord_flip() + 
  scale_fill_brewer("", palette = "Blues") +
  labs(y = "Proportion (%)", x = "") +
  facet_wrap(. ~ crf1a_resp_support_4levels) +
  theme(legend.position = c(1, 1.2),
        legend.background = element_blank()) +
  ggtitle("Change in EQ5D domain (stratified)",
          " ")

p3 = phosp_3m %>% 
  select(eq5d5l_summary_delta, eq5d5l_utility_index_delta, eq5d5l_total_delta) %>% 
  gather() %>% 
  mutate(key = 
           fct_recode(key, 
                      "Change in VAS" = "eq5d5l_summary_delta",
                      "Change in total score" = "eq5d5l_total_delta",
                      "Change in utility index" = "eq5d5l_utility_index_delta"
           )
  ) %>% 
  ggplot_lancet(aes(x = "", y = value)) + 
  geom_boxplot2(aes(fill = key), outlier.shape = NA, width.errorbar = 0) + 
  # geom_jitter(width = 0.4, alpha = 0.05) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  labs(x = "", y = "") +
  facet_grid(. ~ key, scale = "free_x") + 
  theme(legend.position = "none",
        axis.ticks.y = element_blank()) + 
  ggtitle("Change in EQ5D summary metrics (whole cohort)")


p4 = phosp_3m %>% 
  select(eq5d5l_summary_delta, eq5d5l_utility_index_delta, eq5d5l_total_delta, crf1a_resp_support_4levels) %>% 
  gather("key", "value", -crf1a_resp_support_4levels) %>% 
  mutate(key = 
           fct_recode(key, 
                      "Change in VAS" = "eq5d5l_summary_delta",
                      "Change in total score" = "eq5d5l_total_delta",
                      "Change in utility index" = "eq5d5l_utility_index_delta"
           )
  ) %>% 
  ggplot_lancet(aes(x = fct_rev(crf1a_resp_support_4levels), y = value)) + 
  geom_boxplot2(aes(fill = key), outlier.shape = NA, width.errorbar = 0) + 
  # geom_jitter(width = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  labs(x = "", y = "") +
  facet_grid(. ~ key, scale = "free_x") + 
  theme(legend.position = "none") + 
  ggtitle("Change in EQ5D summary metrics (stratified)")


ggarrange(p1, p3, p2, p4, ncol = 2, labels = c("A", "B", "C", "D"), heights = c(1, 2),
               label.args = list(gp=gpar(font=1, cex = 1.5))
               )



phosp_3m %>% 
  select(crf1a_resp_support_4levels, matches("eq5d5l_q.*_delta_change$")) %>% 
  gather("key", "value", -crf1a_resp_support_4levels) %>% 
  group_by(key, value, crf1a_resp_support_4levels) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  group_by(key, crf1a_resp_support_4levels) %>% 
  mutate(
    nn = sum(n),
    prop = 100 * n / nn,
    prop = ifelse(value %in% c("No change", "Improvement"), prop * -1, prop)
    ) %>% 
  ungroup() %>% 
  mutate(
    key = fct_recode(key, 
                     "Mobility" = "eq5d5l_q1_delta_change",
                     "Self-care" = "eq5d5l_q2_delta_change",
                     "Usual activities" = "eq5d5l_q3_delta_change",
                     "Pain / discomfort" = "eq5d5l_q4_delta_change",
                     "Anxiety / Depression " = "eq5d5l_q5_delta_change")
  ) %T>%
  {label_data <<- (.) %>% 
    filter(value == "Worse") %>% 
    mutate(prop_label = paste0(round_tidy(prop, 1), "%"))} %>%
  ggplot_lancet(aes(x = fct_rev(key), y = prop, fill = value)) + 
  geom_col() +
  geom_text(data = label_data, aes(label = prop_label), size = 4, colour = "white", hjust = 1.1) +
  coord_flip() + 
  scale_fill_brewer("", palette = "Blues") +
  labs(y = "Proportion (%)", x = "") +
  facet_wrap(. ~ crf1a_resp_support_4levels) +
  theme(legend.position = c(1, 1.25)) +
  ggtitle("Change in EQ5D domains")





# ----------------------------------------------------------------------------------------------
library(fmsb)
# Define the variable ranges: maximum and minimum
# Demo data
exam_scores <- data.frame(
  row.names = c("Student.1", "Student.2", "Student.3"),
  Biology = c(7.9, 3.9, 9.4),
  Physics = c(10, 20, 0),
  Maths = c(3.7, 11.5, 2.5),
  Sport = c(8.7, 20, 4),
  English = c(7.9, 7.2, 12.4),
  Geography = c(6.4, 10.5, 6.5),
  Art = c(2.4, 0.2, 9.8),
  Programming = c(0, 0, 20),
  Music = c(20, 20, 20)
)
exam_scores
max_min <- data.frame(
  Biology = c(20, 0), Physics = c(20, 0), Maths = c(20, 0),
  Sport = c(20, 0), English = c(20, 0), Geography = c(20, 0),
  Art = c(20, 0), Programming = c(20, 0), Music = c(20, 0)
)
rownames(max_min) <- c("Max", "Min")

# Bind the variable ranges to the data
df <- rbind(max_min, exam_scores)
df
# Plot the data for student 1
library(fmsb)
student1_data <- df[c("Max", "Min", "Student.1"), ]
radarchart(student1_data)


phosp_3m %>% 
  select( matches("eq5d5l_q[12345]+_numeric$")) %>% 
  summarise(
    across(everything(), list(max = max, min = min, mean = mean), na.rm = TRUE)
  )

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

library(data.table)
phosp_3m %>% 
  select( matches("eq5d5l_q[12345]+_numeric$")) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarise(
    max = max(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
  ) %>% 
  transpose(make.names = "key") %>% 
  rename(
    "Mobility" = "eq5d5l_q1_numeric",
    "Self-care" = "eq5d5l_q2_numeric",
    "Usual activities" = "eq5d5l_q3_numeric",
    "Pain / discomfort" = "eq5d5l_q4_numeric",
    "Anxiety / Depression " = "eq5d5l_q5_numeric") %>% 
  radarchart()

after = phosp_3m %>% 
  select( matches("eq5d5l_q[12345]+_numeric$")) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarise(
    max = max(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
  ) %>% 
  transpose(make.names = "key") %>% 
  rename(
    "Mobility" = "eq5d5l_q1_numeric",
    "Self-care" = "eq5d5l_q2_numeric",
    "Usual activities" = "eq5d5l_q3_numeric",
    "Pain / discomfort" = "eq5d5l_q4_numeric",
    "Anxiety / Depression " = "eq5d5l_q5_numeric"
    )

pre = phosp_3m %>% 
  select( matches("eq5d5l_q[12345]+_pre_numeric$")) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarise(
    max = max(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
  ) %>% 
  transpose(make.names = "key") %>% 
  rename(
    "Mobility" = "eq5d5l_q1_pre_numeric",
    "Self-care" = "eq5d5l_q2_pre_numeric",
    "Usual activities" = "eq5d5l_q3_pre_numeric",
    "Pain / discomfort" = "eq5d5l_q4_pre_numeric",
    "Anxiety / Depression " = "eq5d5l_q5_pre_numeric"
  )

plot_data = after %>% 
  bind_rows(pre %>% slice(3))
row.names(plot_data) = c("max", "min", "After COVID-19", "Pre COVID-19")
plot_data

op <- par(mar = c(1, 2, 2, 2))

plot_data %>% 
  create_beautiful_radarchart(caxislabels = c(0, 1, 2, 3, 4),
                              color = c("#00AFBB", "#E7B800"))
# Add an horizontal legend
legend(
  x = "bottom", legend = rownames(plot_data[-c(1,2),]), horiz = TRUE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)



# ----------------------------------------------------------------------

phosp_3m$eq5d5l_total_delta
phosp_3m$eq5d5l_utility_index_delta




