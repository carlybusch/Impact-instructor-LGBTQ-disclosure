#### National instructor LGBTQ+ reveal post-survey
### July 22, 2024

library(brms)
library(dplyr)
library(stringr)
library(lme4)
library(Matrix)
library(lmerTest)
library(ggplot2)
library(nnet)

my_data <- read.csv("lgbtq_post_survey_deid_july2024.csv")

### cronbach's alpha for all scales ----
library(ltm)
cronbach.alpha(my_data[,c("approach1", "approach.2", "approach3")], standardized = T, na.rm = T)
cronbach.alpha(my_data[,c("competence1", "competence2", "competence3")], standardized = T, na.rm = T)
cronbach.alpha(my_data[,c("course_inclusion1", "course_inclusion2", "course_inclusion3")], standardized = T, na.rm = T)
cronbach.alpha(my_data[,c("normalize1", "normalize2", "normalize3")], standardized = T, na.rm = T)
cronbach.alpha(my_data[,c("represent1", "represent2", "represent3")], standardized = T, na.rm = T)
cronbach.alpha(my_data[,c("connect1", "connect2", "connect3")], standardized = T, na.rm = T)
cronbach.alpha(my_data[,c("coursecohesion_sob1", "coursecohesion_sob2", "coursecohesion_sob3")], standardized = T, na.rm = T)
cronbach.alpha(my_data[,c("coursecohesion_mor1", "coursecohesion_mor2", "coursecohesion_mor3")], standardized = T, na.rm = T)
cronbach.alpha(my_data[,c("science_sob1", "science_sob2", "science_sob3")], standardized = T, na.rm = T)
cronbach.alpha(my_data[,c("science_mor1", "science_mor2", "science_mor3")], standardized = T, na.rm = T)

### setting the reference groups ----
my_data$gender3 <- factor(my_data$gender3, 
                          levels = c("man", "not_man"))
my_data$gender3 <- relevel(my_data$gender3, ref = "man")

my_data$religion4 <- relevel(factor(my_data$religion4, ordered = F), ref = "nonreligious")

my_data$race4 <- relevel(factor(my_data$race4, ordered = F), ref = "white")

my_data$anxdep <- relevel(factor(my_data$anxdep, ordered= F), ref = "0")

my_data$lgbtq_accept_uni2 <- relevel(factor(my_data$lgbtq_accept_uni2, levels = c("low", "high")), ref = "low")

my_data$manner <- relevel(factor(my_data$manner, levels = c("elaborate", "brief")), ref = "brief")

my_data$impact <- factor(my_data$impact, levels = c("very negative", "moderately negative", "slightly negative", "none", "slightly positive", "moderately positive", "very positive"))

my_data$appropriate <- factor(my_data$appropriate, levels = c("Strongly disagree", "Disagree","Somewhat disagree","Somewhat agree", "Agree","Strongly agree"))

### demographic table ----

demo_fxn <- function(x){
  tmp <- data.frame(my_data[[x]])
  demo <- merge(data.frame(table(tmp)), 
                data.frame(round((table(tmp)/sum(table(tmp)))*100, 2)), 
                "my_data..x..")
  colnames(demo)<-c("demo","count","perc")
  demo$`Percent (n)` <- paste0(demo$perc, " (", demo$count, ")")
  demo <- arrange(demo, desc(count))
  demo$demo <- stringr::str_to_title(demo$demo)
  demo$name <- x
  return(demo)
}

demo_table <- do.call(rbind, lapply(c("gender", "race", 
                                      "religion", "lgbtq",
                                      "lgbtq_accept_uni2", "anxdep"),
                                    demo_fxn))

### RQ1: Do students recall instructor LGBTQ disclosure? ------
table(my_data$recall2)

recall_reg <- summary(glmer(recall2 ~ lgbtq2 + gender3 + religion4 + race4 + anxdep + 
                                  lgbtq_accept_uni2 + manner + (1|instructor),
                                data = my_data, family=binomial))

### RQ2: Does instructor disclosure affect specific outcomes? ------

means <- do.call(rbind, lapply(c("course_inclusion_mean",
                                 "course_morale_mean", 
                                 "course_belonging_mean", 
                                 "sci_morale_mean",
                                 "sci_belonging_mean", 
                                 "competence_mean",
                                 "connect_mean",
                                 "approach_mean", 
                                 "normalize_mean", 
                                 "represent_mean"),
                               function(x){
                                 tmp <- as.data.frame(mean(my_data[,x], na.rm = T))
                                 colnames(tmp) <- "mean"
                                 rownames(tmp) <- NULL
                                 tmp$outcome <- x
                                 tmp$sd <- sd(my_data[,x], na.rm = T)
                                 tmp <- dplyr::select(tmp, outcome, mean, sd)
                                 return(tmp)
                               }))

mod_out <- do.call(rbind, lapply(c("course_inclusion_mean",
                                   "course_morale_mean", 
                                   "course_belonging_mean", 
                                   "sci_morale_mean",
                                   "sci_belonging_mean", 
                                   "competence_mean",
                                   "connect_mean",
                                   "approach_mean", 
                                   "normalize_mean", 
                                   "represent_mean"),
                                       function(x){
                                         mod_out <- as.data.frame(summary(
                                           lmer(formula = as.formula(
                                             paste0(x, "~ lgbtq2 + gender3 + religion4 + race4 + anxdep + 
                                                    lgbtq_accept_uni2 + manner + (1|instructor)")),
                                             data = my_data, REML = T))$coefficients)
                                         colnames(mod_out) <- c("est", "se", "df", "tval", "pval")
                                         mod_out$outcome <- x
                                         mod_out$predictor <- rownames(mod_out)
                                         rownames(mod_out) <- NULL
                                         mod_out <- dplyr::select(mod_out, outcome, predictor, everything())
                                         return(mod_out)
                                       }))


mod_out |> filter(pval < .05) |>
  filter(predictor != "(Intercept)")

mod_out |> filter(pval < .05 & predictor == "lgbtq2yes")

religion_means <- do.call(rbind, lapply(c("course_inclusion_mean",
                                          "connect_mean",
                                          "approach_mean"),
                                        function(x){
                                          tmp <- as.data.frame(mean(my_data[my_data$religion4 == "christian",x], 
                                                                    na.rm = T))
                                          colnames(tmp) <- "mean_christian"
                                          rownames(tmp) <- NULL
                                          tmp$outcome <- x
                                          tmp$sd_christian <- sd(my_data[my_data$religion4 == "christian",x], 
                                                                 na.rm = T)
                                          tmp$mean_muslim <- mean(my_data[my_data$religion4 == "muslim",x], 
                                                                 na.rm = T)
                                          tmp$sd_muslim <- sd(my_data[my_data$religion4 == "muslim",x], 
                                                                 na.rm = T)
                                          tmp <- dplyr::select(tmp, outcome, mean_christian, sd_christian,
                                                               mean_muslim, sd_muslim)
                                          return(tmp)
                                        }))

### RQ3: How does instructor disclosure affect overall course experience? ----

table(my_data$impact2)

ordinal.impact <- ordinal::clmm(impact ~ lgbtq2 + gender3 + religion4 + race4 + anxdep + 
                                  lgbtq_accept_uni2 + manner + (1|instructor),
                                data = my_data)
summary(ordinal.impact)

table(my_data$impact2)

str_count(my_data$impact_pos_box, "inclusive classroom environment") |> sum()
str_count(my_data$impact_pos_box, "normalize LGBTQ") |> sum()
str_count(my_data$impact_pos_box, "increased the representation of LGBTQ") |> sum()
str_count(my_data$impact_pos_box, "comfortable approaching the instructor") |> sum()
str_count(my_data$impact_pos_box, "instructor more relatable") |> sum()
str_count(my_data$impact_pos_box, "of these options reflect") |> sum()

str_count(my_data$impact_neu_box, "did not change the instructor") |> sum()
str_count(my_data$impact_neu_box, "not relevant to course content") |> sum()
str_count(my_data$impact_neu_box, "familiarity with the LGBTQ") |> sum()
str_count(my_data$impact_neu_box, "of these options reflect") |> sum()

str_count(my_data$impact_neg_box, "distracted from course material") |> sum()
str_count(my_data$impact_neg_box, "feel uncomfortable") |> sum()
str_count(my_data$impact_neg_box, "of these options reflect") |> sum()



### RQ4: Do students believe instructor LGBTQ disclosure is appropriate? ----

table(my_data$appropriate2)

ordinal.appropriateness <- ordinal::clmm(appropriate ~ lgbtq2 + gender3 + religion4 + race4 + anxdep + 
                                           lgbtq_accept_uni2 + manner + (1|instructor),
                                         data = my_data)

summary(ordinal.appropriateness)

str_count(my_data$appropriate_box, "get to know their instructor") |> sum()
str_count(my_data$appropriate_box, "decision whether or not to reveal") |> sum()
str_count(my_data$appropriate_box, "normalizes LGBTQ") |> sum()
str_count(my_data$appropriate_box, "does not impact teaching or learning") |> sum()
str_count(my_data$appropriate_box, "build community and trust") |> sum()
str_count(my_data$appropriate_box, "important part of who they are") |> sum()
str_count(my_data$appropriate_box, "empowers the LGBTQ") |> sum()
str_count(my_data$appropriate_box, "the same as sharing other personal") |> sum()
str_count(my_data$appropriate_box, "of these options reflect") |> sum()

str_count(my_data$notappropriate_box, "not relevant to course material") |> sum()
str_count(my_data$notappropriate_box, "matter whether instructors reveal") |> sum()
str_count(my_data$notappropriate_box, "promotes certain beliefs") |> sum()
str_count(my_data$notappropriate_box, "feel forced") |> sum()
str_count(my_data$notappropriate_box, "make students uncomfortable") |> sum()
str_count(my_data$notappropriate_box, "of these options reflect") |> sum()

### Figure 1 -----

mean_plot <- means |>
  mutate(outcome = forcats::fct_relevel(outcome, "sci_morale_mean",
                                        "sci_belonging_mean",
                                        "represent_mean", 
                                        "normalize_mean",
                                        "course_morale_mean",
                                        "course_belonging_mean",
                                        "course_inclusion_mean",
                                        "competence_mean",
                                        "approach_mean",
                                        "connect_mean")) |>
  ggplot(aes(x = mean, y = outcome)) +
  geom_point(stat = "identity", color = "black", shape = "square", size = 4) +
  geom_errorbarh(aes(xmax = mean + sd, xmin = mean - sd), height = 0, linewidth = 1.25) +
  geom_text(aes(x = mean + sd + 0.2, label = round(mean, 1)),
            vjust = 0.5, size = 3, color = "black") +
  labs(x = "Mean ± SD", y = "") +
  scale_x_continuous(
    limits = c(1, 5), 
    breaks = c(1, 2, 3, 4, 5)) +
  scale_y_discrete(labels = c("Science community\nfeelings of morale",
                                       "Science community\nsense of belonging",
                                       "LGBTQ+ representation\nin science",
                                       "LGBTQ+ normalization\nin science",
                                       "Course feelings of morale",
                                       "Course sense of belonging",
                                       "Inclusive course environment",
                                       "Instructor competence",
                                       "Instructor approachability",
                                       "Instructor connectedness")) +
  theme_classic() +
  theme(axis.ticks.y = element_blank(),
        axis.title.x = element_text(family="Helvetica", color = "black", size = 8),
        axis.text.x = element_text(family="Helvetica", color = "black", size = 8),
        axis.line.y = element_blank(),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 8),
        plot.title = element_blank()) 

mean_plot

fig1b <- mod_out |>
  filter(predictor == "lgbtq2yes") |>
  mutate(psig = ifelse(pval < .05, "sig", "nonsig")) |>
  mutate(outcome = forcats::fct_relevel(outcome, "sci_morale_mean",
                                        "sci_belonging_mean",
                                        "represent_mean", 
                                        "normalize_mean",
                                        "course_morale_mean",
                                        "course_belonging_mean",
                                        "course_inclusion_mean",
                                        "competence_mean",
                                        "approach_mean",
                                        "connect_mean")) %>%
  ggplot(aes(y = outcome)) +
  geom_point(aes(x = est), size = 3.5) +
  geom_errorbarh(aes(xmin = est - 1.96*se, xmax = est + 1.96*se), linewidth = 1.25) +  
  geom_vline(aes(xintercept = 0), linetype="dashed", linewidth=.7, 
             color = "grey15") + 
  labs(x = "Standardized beta ± 95% CI", y = "", title = "LGBTQ+") + 
  scale_y_discrete(labels = c("Science community\nfeelings of morale",
                              "Science community\nsense of belonging",
                              "LGBTQ+ representation\nin science",
                              "LGBTQ+ normalization\nin science",
                              "Course feelings of morale",
                              "Course sense of belonging",
                              "Inclusive course environment",
                              "Instructor competence",
                              "Instructor approachability",
                              "Instructor connectedness")) +
  theme_classic() +
  theme(axis.ticks.y = element_blank(),
        axis.title.x = element_text(family="Helvetica", color = "black", size = 8),
        axis.text.x = element_text(family="Helvetica", color = "black", size = 8),
        axis.line.y = element_blank(),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 8),
        plot.title = element_text(family="Helvetica", color = "black", size = 8, face = "italic", hjust = 0.5),
        legend.position = "none")


fig1b

fig1c <- mod_out |>
  filter(predictor == "gender3not_man") |>
  mutate(psig = ifelse(pval < .05, "sig", "nonsig")) |>
  mutate(outcome = forcats::fct_relevel(outcome, "sci_morale_mean",
                                        "sci_belonging_mean",
                                        "represent_mean", 
                                        "normalize_mean",
                                        "course_morale_mean",
                                        "course_belonging_mean",
                                        "course_inclusion_mean",
                                        "competence_mean",
                                        "approach_mean",
                                        "connect_mean")) %>%
  ggplot(aes(y = outcome)) +
  geom_point(aes(x = est, shape = psig), size = 3.5) +
  geom_errorbarh(aes(xmin = est - 1.96*se, xmax = est + 1.96*se, linewidth = psig)) +  
  geom_vline(aes(xintercept = 0), linetype="dashed", linewidth=.7, 
             color = "grey15") + 
  labs(x = "Standardized beta ± 95% CI", y = "", title = "Women and nonbinary") + 
  scale_shape_manual(values = c(23, 16)) +
  scale_linewidth_manual(values = c(0.5, 1.25)) +
  scale_y_discrete(labels = c("Science community\nfeelings of morale",
                              "Science community\nsense of belonging",
                              "LGBTQ+ representation\nin science",
                              "LGBTQ+ normalization\nin science",
                              "Course feelings of morale",
                              "Course sense of belonging",
                              "Inclusive course environment",
                              "Instructor competence",
                              "Instructor approachability",
                              "Instructor connectedness")) +
  theme_classic() +
  theme(axis.ticks.y = element_blank(),
        axis.title.x = element_text(family="Helvetica", color = "black", size = 8),
        axis.text.x = element_text(family="Helvetica", color = "black", size = 8),
        axis.line.y = element_blank(),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 8),
        plot.title = element_text(family="Helvetica", color = "black", size = 8, face = "italic", hjust = 0.5),
        legend.position = "none")


fig1c


fig1d <- mod_out |>
  filter(predictor == "race4peer") |>
  mutate(psig = ifelse(pval < .05, "sig", "nonsig")) |>
  mutate(outcome = forcats::fct_relevel(outcome, "sci_morale_mean",
                                        "sci_belonging_mean",
                                        "represent_mean", 
                                        "normalize_mean",
                                        "course_morale_mean",
                                        "course_belonging_mean",
                                        "course_inclusion_mean",
                                        "competence_mean",
                                        "approach_mean",
                                        "connect_mean")) %>%
  ggplot(aes(y = outcome)) +
  geom_point(aes(x = est, shape = psig), size = 3.5) +
  geom_errorbarh(aes(xmin = est - 1.96*se, xmax = est + 1.96*se, linewidth = psig)) +  
  geom_vline(aes(xintercept = 0), linetype="dashed", linewidth=.7, 
             color = "grey15") + 
  labs(x = "Standardized beta ± 95% CI", y = "", title = "PEER") + 
  scale_shape_manual(values = c(23, 16)) +
  scale_linewidth_manual(values = c(0.5, 1.25)) +
  scale_y_discrete(labels = c("Science community\nfeelings of morale",
                              "Science community\nsense of belonging",
                              "LGBTQ+ representation\nin science",
                              "LGBTQ+ normalization\nin science",
                              "Course feelings of morale",
                              "Course sense of belonging",
                              "Inclusive course environment",
                              "Instructor competence",
                              "Instructor approachability",
                              "Instructor connectedness")) +
  theme_classic() +
  theme(axis.ticks.y = element_blank(),
        axis.title.x = element_text(family="Helvetica", color = "black", size = 8),
        axis.text.x = element_text(family="Helvetica", color = "black", size = 8),
        axis.line.y = element_blank(),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 8),
        plot.title = element_text(family="Helvetica", color = "black", size = 8, face = "italic", hjust = 0.5),
        legend.position = "none")


fig1d

fig1e <- mod_out |>
  filter(predictor == "race4asian") |>
  mutate(psig = ifelse(pval < .05, "sig", "nonsig")) |>
  mutate(outcome = forcats::fct_relevel(outcome, "sci_morale_mean",
                                        "sci_belonging_mean",
                                        "represent_mean", 
                                        "normalize_mean",
                                        "course_morale_mean",
                                        "course_belonging_mean",
                                        "course_inclusion_mean",
                                        "competence_mean",
                                        "approach_mean",
                                        "connect_mean")) %>%
  ggplot(aes(y = outcome)) +
  geom_point(aes(x = est, shape = psig), size = 3.5) +
  geom_errorbarh(aes(xmin = est - 1.96*se, xmax = est + 1.96*se, linewidth = psig)) +  
  geom_vline(aes(xintercept = 0), linetype="dashed", linewidth=.7, 
             color = "grey15") + 
  labs(x = "Standardized beta ± 95% CI", y = "", title = "Asian") + 
  scale_shape_manual(values = c(23, 16)) +
  scale_linewidth_manual(values = c(0.5, 1.25)) +
  scale_y_discrete(labels = c("Science community\nfeelings of morale",
                              "Science community\nsense of belonging",
                              "LGBTQ+ representation\nin science",
                              "LGBTQ+ normalization\nin science",
                              "Course feelings of morale",
                              "Course sense of belonging",
                              "Inclusive course environment",
                              "Instructor competence",
                              "Instructor approachability",
                              "Instructor connectedness")) +
  theme_classic() +
  theme(axis.ticks.y = element_blank(),
        axis.title.x = element_text(family="Helvetica", color = "black", size = 8),
        axis.text.x = element_text(family="Helvetica", color = "black", size = 8),
        axis.line.y = element_blank(),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 8),
        plot.title = element_text(family="Helvetica", color = "black", size = 8, face = "italic", hjust = 0.5),
        legend.position = "none")


fig1e

fig1f <- mod_out |>
  filter(predictor == "anxdep1") |>
  mutate(psig = ifelse(pval < .05, "sig", "nonsig")) |>
  mutate(outcome = forcats::fct_relevel(outcome, "sci_morale_mean",
                                        "sci_belonging_mean",
                                        "represent_mean", 
                                        "normalize_mean",
                                        "course_morale_mean",
                                        "course_belonging_mean",
                                        "course_inclusion_mean",
                                        "competence_mean",
                                        "approach_mean",
                                        "connect_mean")) %>%
  ggplot(aes(y = outcome)) +
  geom_point(aes(x = est, shape = psig), size = 3.5) +
  geom_errorbarh(aes(xmin = est - 1.96*se, xmax = est + 1.96*se, linewidth = psig)) +  
  geom_vline(aes(xintercept = 0), linetype="dashed", linewidth=.7, 
             color = "grey15") + 
  labs(x = "Standardized beta ± 95% CI", y = "", title = "Anxiety or depression") + 
  scale_shape_manual(values = c(23, 16)) +
  scale_linewidth_manual(values = c(0.5, 1.25)) +
  scale_y_discrete(labels = c("Science community\nfeelings of morale",
                              "Science community\nsense of belonging",
                              "LGBTQ+ representation\nin science",
                              "LGBTQ+ normalization\nin science",
                              "Course feelings of morale",
                              "Course sense of belonging",
                              "Inclusive course environment",
                              "Instructor competence",
                              "Instructor approachability",
                              "Instructor connectedness")) +
  theme_classic() +
  theme(axis.ticks.y = element_blank(),
        axis.title.x = element_text(family="Helvetica", color = "black", size = 8),
        axis.text.x = element_text(family="Helvetica", color = "black", size = 8),
        axis.line.y = element_blank(),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 8),
        plot.title = element_text(family="Helvetica", color = "black", size = 8, face = "italic", hjust = 0.5),
        legend.position = "none")


fig1f

fig1 <- ggpubr::ggarrange(mean_plot, fig1b, fig1c, fig1d, fig1e, fig1f,
                          labels = c("A.", "B.", "C.",
                                     "D.", "E.", "F."), 
                             font.label = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
                          ncol = 2, nrow = 3)

fig1

### Figure 2 -----

fig2 <- mod_out |>
  mutate(psig = ifelse(pval < .05, "sig", "nonsig")) |>
  filter(predictor == "religion4christian" | predictor == "religion4muslim") |>
  mutate(outcome = forcats::fct_relevel(outcome, "sci_morale_mean",
                                        "sci_belonging_mean",
                                        "represent_mean", 
                                        "normalize_mean",
                                        "course_morale_mean",
                                        "course_belonging_mean",
                                        "course_inclusion_mean",
                                        "competence_mean",
                                        "approach_mean",
                                        "connect_mean")) %>%
  ggplot(aes(y = outcome, color = predictor)) +
  geom_point(aes(x = est, shape = psig), size = 1.2, position = position_dodge(0.75)) +
  geom_errorbarh(aes(xmin = est - 1.96*se, xmax = est + 1.96*se, linewidth = psig), 
                 position = position_dodge(0.75)) +  
  geom_vline(aes(xintercept = 0), linetype="dashed", linewidth=.3, 
             color = "grey15") + 
  labs(x = "Standardized beta ± 95% CI", y = "", title = "") + 
  scale_shape_manual(values = c(23, 16)) +
  scale_linewidth_manual(values = c(0.3, .8)) +
  scale_color_manual(values = c("black", "dodgerblue2"), labels = c("Christian", "Muslim")) +
  guides(color = guide_legend(title = "Religious\naffiliation"),
         shape = "none", linewidth = "none") +
  scale_y_discrete(labels = c("Science community feelings of morale",
                              "Science community sense of belonging",
                              "LGBTQ+ representation in science",
                              "LGBTQ+ normalization in science",
                              "Course feelings of morale",
                              "Course sense of belonging",
                              "Inclusive course environment",
                              "Instructor competence",
                              "Instructor approachability",
                              "Instructor connectedness")) +
  theme_classic() +
  theme(axis.ticks.y = element_blank(),
        axis.title.x = element_text(family="Helvetica", color = "black", size = 8),
        axis.text.x = element_text(family="Helvetica", color = "black", size = 8),
        axis.line.y = element_blank(),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 8),
        legend.text = element_text(family="Helvetica", color = "black", size = 8),
        legend.title = element_text(family="Helvetica", color = "black", size = 8),
        plot.title = element_blank(),
        legend.position = "right")


fig2

### Figure 3 -----

fig3 <- mod_out |>
  filter(predictor == "lgbtq_accept_uni2high") |>
  mutate(outcome = forcats::fct_relevel(outcome, "sci_morale_mean",
                                        "sci_belonging_mean",
                                        "represent_mean", 
                                        "normalize_mean",
                                        "course_morale_mean",
                                        "course_belonging_mean",
                                        "course_inclusion_mean",
                                        "competence_mean",
                                        "approach_mean",
                                        "connect_mean")) %>%
  ggplot(aes(y = outcome)) +
  geom_point(aes(x = est), size = 1.2, shape = 23) +
  geom_errorbarh(aes(xmin = est - 1.96*se, xmax = est + 1.96*se), 
                 linewidth = 0.5) +  
  geom_vline(aes(xintercept = 0), linetype="dashed", linewidth=.7, 
             color = "grey15") + 
  labs(x = "Standardized beta ± 95% CI", y = "", title = "") + 
  scale_y_discrete(labels = c("Science community feelings of morale",
                              "Science community sense of belonging",
                              "LGBTQ+ representation in science",
                              "LGBTQ+ normalization in science",
                              "Course feelings of morale",
                              "Course sense of belonging",
                              "Inclusive course environment",
                              "Instructor competence",
                              "Instructor approachability",
                              "Instructor connectedness")) +
  theme_classic() +
  theme(axis.ticks.y = element_blank(),
        axis.title.x = element_text(family="Helvetica", color = "black", size = 8),
        axis.text.x = element_text(family="Helvetica", color = "black", size = 8),
        axis.line.y = element_blank(),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 8),
        plot.title = element_blank(),
        legend.position = "none")


fig3

### Figure 4 -----

impact_df <- data.frame(table(my_data$impact, my_data$lgbtq_accept_uni2)) |>
filter(Var1 != "") |>
  mutate(Var1 = forcats::fct_relevel(Var1, c("very negative", "moderately negative", "slightly negative", "none",
                                             "slightly positive", "moderately positive", "very positive")))

impact_df$denom <- NA
impact_df[impact_df$Var2 == "low",]$denom <- sum(impact_df[impact_df$Var2 == "low",]$Freq)
impact_df[impact_df$Var2 == "high",]$denom <- sum(impact_df[impact_df$Var2 == "high",]$Freq)

impact_df$pct <- impact_df$Freq/impact_df$denom

impact_bar <- impact_df |>
  ggplot(aes(x = Var2, y = pct, fill = Var2)) +
  geom_col() +
  geom_text(aes(label = scales::percent(pct, accuracy = .1)), 
            position=position_dodge(width=0.9), size=2.5, fontface="bold", color = "black", vjust = -.2) +
  scale_y_continuous(expand = c(0,0), limits = c(0, .71), breaks = seq(0:.7,by = .1),
                     labels = scales::label_percent(accuracy = .1)) +
  scale_fill_manual(values = c("lightgray", "black")) +
  labs(x = "", y = "Percent (%)",
       fill = "State LGBTQ+\nacceptance") + 
  facet_wrap(.~Var1, strip.position = "bottom", nrow = 1,
             labeller = labeller(Var1 = c("very negative" = "Very\nnegative",
                                          "moderately negative"  = "Moderately\nnegative",
                                          "slightly negative" = "Slightly\nnegative",
                                          "none" = "Neutral",
                                          "slightly positive"  = "Slightly\npositive",
                                          "moderately positive" = "Moderately\npositive",
                                          "very positive" = "Very\npositive"))) +
  theme_classic() +
  theme(axis.title = element_text(family="Helvetica", color = "black", size = 8),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 8),
        legend.text = element_text(family="Helvetica", color = "black", size = 8),
        legend.title = element_text(family="Helvetica", color = "black", size = 8),
        plot.title = element_blank(),
        strip.text = element_text(family="Helvetica", color = "black", size = 8),
        strip.background = element_blank(),
        strip.placement = "outside")

impact_bar

### Figure 5 -----

approp_df <- data.frame(table(my_data$appropriate, my_data$lgbtq_accept_uni2)) |>
  filter(Var1 != "") |>
  mutate(Var1 = forcats::fct_relevel(Var1, c("Strongly disagree", "Disagree", "Somewhat disagree",
                                             "Somewhat agree", "Agree", "Strongly agree")))

approp_df$denom <- NA
approp_df[approp_df$Var2 == "low",]$denom <- sum(approp_df[approp_df$Var2 == "low",]$Freq)
approp_df[approp_df$Var2 == "high",]$denom <- sum(approp_df[approp_df$Var2 == "high",]$Freq)

approp_df$pct <- approp_df$Freq/approp_df$denom

approp_bar <- approp_df |>
  ggplot(aes(x = Var2, y = pct, fill = Var2)) +
  geom_col() +
  geom_text(aes(label = scales::percent(pct, accuracy = .1)), 
            position=position_dodge(width=0.9), size=2.5, fontface="bold", color = "black", vjust = -.2) +
  scale_y_continuous(expand = c(0,0), limits = c(0, .5), breaks = seq(0:.5,by = .1),
                     labels = scales::label_percent(accuracy = .1)) +
  scale_fill_manual(values = c("lightgray", "black")) +
  labs(x = "", y = "Percent (%)", title = "B. It is completely appropriate for science instructors to reveal their LGBTQ+ identities.",
       fill = "State LGBTQ+\nacceptance") + 
  facet_wrap(.~Var1, strip.position = "bottom", nrow = 1,
             labeller = labeller(Var1 = c("Strongly disagree" = "Strongly\ndisagree",
                                          "Disagree"  = "Disagree",
                                          "Somewhat disagree" = "Somewhat\ndisagree",
                                          "Somewhat agree"  = "Somewhat\nagree",
                                          "Agree" = "Agree",
                                          "Strongly agree" = "Strongly\nagree"))) +
  theme_classic() +
  theme(axis.title = element_text(family="Helvetica", color = "black", size = 8),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 8),
        legend.text = element_text(family="Helvetica", color = "black", size = 8),
        legend.title = element_text(family="Helvetica", color = "black", size = 8),
        plot.title = element_blank(),
        strip.text = element_text(family="Helvetica", color = "black", size = 8),
        strip.background = element_blank(),
        strip.placement = "outside")

approp_bar

###saving figures for submission -----

#as PDFs
ggsave(filename = "~/Desktop/postsurvey_fig1_oct2024.pdf", plot = fig1, device = "pdf", units = "cm",
       width = 17.5, height = 23)
ggsave(filename = "~/Desktop/postsurvey_fig2_oct2024.pdf", plot = fig2, device = "pdf", units = "cm",
       width = 17.5, height = 6)
ggsave(filename = "~/Desktop/postsurvey_fig3_oct2024.pdf", plot = fig3, device = "pdf", units = "cm",
       width = 17.5, height = 6)
ggsave(filename = "~/Desktop/postsurvey_fig4_oct2024.pdf", plot = impact_bar, device = "pdf", units = "cm",
       width = 17.5, height = 8)
ggsave(filename = "~/Desktop/postsurvey_fig5_oct2024.pdf", plot = approp_bar, device = "pdf", units = "cm",
       width = 17.5, height = 6)

#as eps:
ggsave(filename = "~/Desktop/postsurvey_fig1_oct2024.eps", plot = fig1, device = "eps", units = "cm",
       width = 17.5, height = 23)
ggsave(filename = "~/Desktop/postsurvey_fig2_oct2024.eps", plot = fig2, device = "eps", units = "cm",
       width = 17.5, height = 6)
ggsave(filename = "~/Desktop/postsurvey_fig3_oct2024.eps", plot = fig3, device = "eps", units = "cm",
       width = 17.5, height = 6)
ggsave(filename = "~/Desktop/postsurvey_fig4_oct2024.eps", plot = impact_bar, device = "eps", units = "cm",
       width = 17.5, height = 8)
ggsave(filename = "~/Desktop/postsurvey_fig5_oct2024.eps", plot = approp_bar, device = "eps", units = "cm",
       width = 17.5, height = 6)

### SM Table: outcomes disaggregated by instructor -----
#instructors (order in table 1):
#bravo, kilo, quebec, echo, foxtrot, lima, whiskey, golf, hotel

disagg_outcomes <- do.call(rbind, lapply(c("bravo", "kilo", "quebec", 
                                           "echo", "foxtrot", "lima", 
                                           "whiskey", "golf", "hotel"),
                                    function(x){
                                      tmp <- data.frame(table(my_data[my_data$instructor == x,][["recall2"]]))
                                      colnames(tmp) <- c("value", "count")
                                      tmp$outcome <- "recall"
                                      tmp$sum <- sum(tmp$count)
                                      tmp$pct <- round(tmp$count/tmp$sum, 4)
                                      tmp <- tmp |> dplyr::select(outcome, value, count, sum, pct)
                                      tmp$instructor <- x
                                      tmp2 <- data.frame(table(my_data[my_data$instructor == x,][["impact2"]]))
                                      colnames(tmp2) <- c("value", "count")
                                      tmp2$outcome <- "impact"
                                      tmp2$sum <- sum(tmp2$count)
                                      tmp2$pct <- round(tmp2$count/tmp2$sum, 4)
                                      tmp2 <- tmp2 |> dplyr::select(outcome, value, count, sum, pct)
                                      tmp2$instructor <- x
                                      tmp3 <- data.frame(table(my_data[my_data$instructor == x,][["appropriate"]]))
                                      colnames(tmp3) <- c("value", "count")
                                      tmp3$outcome <- "appropriate"
                                      tmp3$sum <- sum(tmp3$count)
                                      tmp3$pct <- round(tmp3$count/tmp3$sum, 4)
                                      tmp3 <- tmp3 |> dplyr::select(outcome, value, count, sum, pct)
                                      tmp3$instructor <- x
                                      tmp4 <- data.frame(table(my_data[my_data$instructor == x,][["appropriate2"]]))
                                      colnames(tmp4) <- c("value", "count")
                                      tmp4$outcome <- "appropriate-binary"
                                      tmp4$sum <- sum(tmp4$count)
                                      tmp4$pct <- round(tmp4$count/tmp4$sum, 4)
                                      tmp4 <- tmp4 |> dplyr::select(outcome, value, count, sum, pct)
                                      tmp4$instructor <- x
                                      out <- rbind(tmp, tmp2, tmp3, tmp4)
                                      out <- out |> dplyr::select(instructor, everything())
                                      return(out)
                                    }))

