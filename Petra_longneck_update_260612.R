# Longneck-Project MAS, Petra
# Juergen Degenfellner
# Code for PUBLICATION based on MAS_thesis: Petra Gmuender

# 2 zeitpunkte
# baseline = T0
# 3 monate = T1

# INFO: ........

# Set working directory to source file location
# (only when running interactively in RStudio; ignored when sourced / via Rscript,
#  where rstudioapi::getActiveDocumentContext() would error "RStudio not running").
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# NOTE: local txt read disabled - the canonical source is the xlsx read below.
#df<- read.table ("E://Work/ZHaW/MAS/2023/Petra Gmünder/2025/Data/longneckPetra2026.txt",
#                                  dec=".",header= TRUE,sep="\t")

# IMPUTE YES NO-----------------------------------------------------------------
impute_TRUE_FALSE <- TRUE

# Functions----
nice_histogram <- function(data, var_name) {
  # Convert the variable name to a symbol
  var_sym <- rlang::ensym(var_name)
  
  # Check if the variable has 10 or fewer unique values
  if (length(unique(data[[as.character(var_sym)]])) <= 10) {
    warning("The number of unique values in the variable is 10 or less; 
            this might not be suitable for a continuous scale.")
  }
  
  p <- ggplot(data, aes(x = {{ var_sym }})) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.7, 
                   color="darkgrey") +
    geom_density(aes(y = after_stat(density)), color = "blue", linewidth=1) +
    geom_boxplot(aes(y = -0.01, x = {{ var_sym }}), width = 0.02, 
                 position = position_nudge(y = -0.00)) +
    geom_point(aes(y = -0.01), 
               position = position_jitter(width = 0.002, height = 0.01), 
               size = 1, alpha = 0.05) +
    ggtitle(paste("Histogram ", as.character(var_sym))) +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
}

# Map regsubsets coefficient names (factor-dummy levels such as "sexfemale" or
# "occupationin training") back to the ORIGINAL predictor variables, so that
# reformulate()/lm() expand the factors themselves. Without this, reformulate()
# fails (e.g. "occupationin training" contains a space -> str2lang parse error,
# or "sexfemale" is not a real column).
coef_to_vars <- function(coef_names, candidate_vars) {
  hits <- vapply(candidate_vars, function(v)
    any(coef_names == v | startsWith(coef_names, v)), logical(1))
  candidate_vars[hits]
}

# R-Packages-----------------------------------------------------------------
library(pacman)
#remotes::install_github("datalorax/equatiomatic")
pacman::p_load(readxl, tidyverse, lubridate, dagitty, ggdag, tictoc,
               writexl, VIM, leaps, DataExplorer, rethinking,
               visdat, caret, equatiomatic, regclass, 
               table1, Hmisc, performance, car, aspace, todor, ggpubr)
library(gtsummary) # otherwise error
library(plotly)
library(RColorBrewer)

# Helpers for the "without baseline" models (best-subset re-selection +
# with/without-baseline comparison). Defines reselect_no_baseline() and
# compare_with_without_baseline(); sourced after leaps is loaded.
source("./no_baseline_reselection.R")

#todor::todor()

# .) READ ----
# NOTE: the data file is NOT part of this repository (see README).
df <- readxl::read_xlsx("./longneckPetra.xlsx",
                        na = "NA",
                        sheet = 1)
dim(df) # 80 (old data set) -> 156 new data set
str(df)
sort(colnames(df))

# .) Select variables for analysis----
df <- df %>% dplyr::select(
  # other info:
  workinghours,
  occupation,
  
  # Outcomes:
  paindetect1_t0, # current pain ranging from 0 to 10
  paindetect2_t0, # strongest pain the last two weeks ranging from 0 to 10
  paindetect3_t0, # average pain severity the last two weeks ranging from 0 to 10
  paindetect1_t1, # current pain ranging from 0 to 10
  paindetect2_t1, # strongest pain the last two weeks ranging from 0 to 10
  paindetect3_t1, # average pain severity the last two weeks ranging from 0 to 10
  paindetectnrs_mean_t0, # mean of the three pain intensities mentioned above (paindetect1, paindetect2, paindetect3), not recommended by Jürgen
  paindetectnrs_mean_t1, # mean of the three pain intensities mentioned above (paindetect1, paindetect2, paindetect3), not recommended by Jürgen
  paindetect_totalscore_t0,
  paindetect_totalscore_t1,
  
  ndiscore_percent_t0, # Neck Disability Index is a self-rated tool assessing neck pain and its associated functional disability, 10 items to rate from 0 (no disability) to 5 (complete disability, not possible), total numeric score varies from 0 to 50, total score is expressed from 0% to 100%, formula in % = (sum/50)*100  
  ndiscore_percent_t1, 
  
  disability_t0, # outcome (in percent of their respective scale...)
  disability_t1, # outcome
  
  # Covariates:
  age, sex, medication, care, # adjustment covariates (other care has too many missings)
  
  cmsz_faults_l,
  cmsz_faults_r, 
  
  cmsz_time_l,
  cmsz_time_r, 
  headeye_number_pos, # for investigating cervical proprioceptive reflexes, five tests of head-eye movement control are applied
  
  rom_flex1,
  rom_flex2, 
  
  rom_ext1,
  rom_ext2, 
  
  rom_rot_r1,
  rom_rot_r2, 
  
  rom_rot_l1,
  rom_rot_l2, 
  
  mc_s_plane, # Movement Control of the upper and lower cervical spine, 0 = negative test, 1 = positive test, total score ranging from 0-4
  
  pin10_c2_l, pin10_c2_r, # mean of both; pin10 = 10 Stimulations (zähler)
  pin2_c2_r, pin2_c2_l, # mean of both; pin2 = 1 Stimulation (nenner)
  # -> WuR_C2_r, WuR_C2_l, 
  
  # Wind up ratio = (10 Stimulations+1)/(1 Stimulation+1), at level M. trapezius descendens right and left:
  pin10_trap_l, pin10_trap_r, # mean of both; pin10 = 10 Stimulations (zähler)
  pin2_trap_r, pin2_trap_l, # mean of both; pin2 = 1 Stimulation (nenner)
  # -> WuR_Trap_r, WuR_Trap_l
  
  # Joint Position Error:
  jpe_l1, jpe_l2, jpe_l3, 
  jpe_r1, jpe_r2, jpe_r3, 
  
  recurrentpain_t0, # (0-4), 0=never, 1=1-2 episodes, 2=3-4 episodes, 4=more than 4 episodes.
  recurrentpain_t1 # (0-4)
)


dim(df) # 156  50

Degree <- function(x)# the function using the inverse tangent and transfer
  # centimeters into radians and later into degrees by dividing through
  # the initial distance of 90cm
{
  atan_d(x/100)
}

# .) Variable Definitions-------------------------------------------------------
# __Absolute error JPE for Rotation left and right ----
# ___RR ----
df$degree_r1 <- Degree(df$jpe_r1) # right rotation
df$degree_r2 <- Degree(df$jpe_r2)
df$degree_r3 <- Degree(df$jpe_r3)
# ___LR ----
df$degree_l1 <- Degree(df$jpe_l1) # left rotation
df$degree_l2 <- Degree(df$jpe_l2)
df$degree_l3 <- Degree(df$jpe_l3)

df$degree_r_AE <- rowMeans(abs(df[,c("degree_r1",
                                     "degree_r2",
                                     "degree_r3")]))
df$degree_l_AE <- rowMeans(abs(df[,c("degree_l1",
                                     "degree_l2",
                                     "degree_l3")]))

t.test(df$degree_l_AE, df$degree_r_AE) #differences are on average 0.3°, not measurable

df$jpe <- df$degree_l_AE+ df$degree_r_AE

df %>%
  pivot_longer(cols = c(degree_r_AE, degree_l_AE),
               names_to = "Side",
               values_to = "Error") %>%
  mutate(Side = case_when(
    Side == "degree_r_AE" ~ "Right",
    Side == "degree_l_AE" ~ "Left"
  )) %>%
  ggplot(aes(x = Side, y = Error, fill = Side)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.7, size = 2) +
  labs(
    title = "JPE°, absolute error\nRot right and left",
    x = "",
    y = "Absolute Error (°)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),   
    legend.position = "none"                  
  )

# __Wind-up Ratios----
# address the zero value issue by adding +1 to each value, 
# then calculate the mean and then the ratio
# check distribution first

t.test(df$pin2_c2_l, df$pin2_c2_r) 
t.test(df$pin2_trap_r, df$pin2_trap_l)
# keep the sides as single variable
# we can change them to one each

df$wur_c2_l <- ((df$pin10_c2_l + 1)/(df$pin2_c2_l + 1))
df$wur_c2_r <- ((df$pin10_c2_r + 1)/(df$pin2_c2_r + 1))

df$wur_c2 <- df$wur_c2_l + df$wur_c2_r

df %>%
  pivot_longer(cols = c(wur_c2_l, wur_c2_r),
               names_to = "Side",
               values_to = "WUR") %>%
  mutate(Side = case_when(
    Side == "wur_c2_l" ~ "Left",
    Side == "wur_c2_r" ~ "Right"
  )) %>%
  ggplot(aes(x = Side, y = WUR, fill = Side)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.7, size = 2) +
  labs(
    title = "Wind-up ratio at C2,\nleft and right",
    x = "",
    y = "Wind-up Ratio"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),  # Titel zentrieren
    legend.position = "none"
  )

df$wur_trap_l <- ((df$pin10_trap_l + 1)/(df$pin2_trap_l + 1))
df$wur_trap_r <- ((df$pin10_trap_r + 1)/(df$pin2_trap_r + 1))

df$wur_trap<- df$wur_trap_l + df$wur_trap_r

df %>%
  dplyr::filter(wur_trap_r < 8) %>% # one very large value
  pivot_longer(cols = c(wur_trap_l, wur_trap_r),
               names_to = "Side",
               values_to = "WUR") %>%
  mutate(Side = case_when(
    Side == "wur_trap_l" ~ "Left",
    Side == "wur_trap_r" ~ "Right"
  )) %>%
  ggplot(aes(x = Side, y = WUR, fill = Side)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.7, size = 2) +
  labs(
    title = "Wind-up ratio at Trapezius,\nleft and right",
    x = "",
    y = "Wind-up Ratio"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.position = "none"                 
  )


# __ROM-------------------------------------------------------------------------
# rom_flex_mean:
df$rom_flex_mean <- rowMeans(df[,c("rom_flex1",
                                   "rom_flex2")], na.rm = TRUE)

# rom_ext_mean:
df$rom_ext_mean <- rowMeans(df[,c("rom_ext1",
                                  "rom_ext2")], na.rm = TRUE)

# rom_rot_r_mean:
df$rom_rot_r_mean <- rowMeans(df[,c("rom_rot_r1",
                                    "rom_rot_r2")], na.rm = TRUE)

# rom_rot_l_mean:
df$rom_rot_l_mean <- rowMeans(df[,c("rom_rot_l1",
                                    "rom_rot_l2")], na.rm = TRUE)

df$sum_rom <- df$rom_ext_mean + df$rom_flex_mean + 
  df$rom_rot_l_mean + df$rom_rot_r_mean


# __CMSZ------------------------------------------------------------------------
df$cmsz_mean_faults <- rowMeans(df[,c("cmsz_faults_l",
                                      "cmsz_faults_r")], na.rm = TRUE)

df$cmsz_mean_time <- rowMeans(df[,c("cmsz_time_l",
                                    "cmsz_time_r")], na.rm = TRUE)

dim(df) # 156  68


# .) Exploratory Data Analysis (EDA)-----
# warnings....
nice_histogram(df, paindetect1_t0)
nice_histogram(df, paindetect2_t0)
nice_histogram(df, paindetect3_t0)
nice_histogram(df, paindetect1_t1)
nice_histogram(df, paindetect2_t1)
nice_histogram(df, paindetect3_t1)

nice_histogram(df, paindetectnrs_mean_t0)
nice_histogram(df, paindetectnrs_mean_t1)
nice_histogram(df, paindetect_totalscore_t0) # right-skewed, but at least rather continuous
nice_histogram(df, paindetect_totalscore_t1) 

nice_histogram(df, disability_t0) # right-skewed
nice_histogram(df, disability_t1) # right-skewed

nice_histogram(df, age) # very right-skewed

# Table 1-----------
Hmisc::label(df$paindetect1_t0) <- "Current pain (0-10) at T0"
Hmisc::label(df$paindetect2_t0) <- "Strongest pain last 2 weeks (0-10) at T0"
Hmisc::label(df$paindetect3_t0) <- "Average pain severity last 2 weeks (0-10) at T0"
Hmisc::label(df$paindetect1_t1) <- "Current pain (0-10) at T1"
Hmisc::label(df$paindetect2_t1) <- "Strongest pain last 2 weeks (0-10) at T1"
Hmisc::label(df$paindetect3_t1) <- "Average pain severity last 2 weeks (0-10) at T1"
Hmisc::label(df$paindetectnrs_mean_t0) <- "Mean pain intensity at T0"
Hmisc::label(df$paindetectnrs_mean_t1) <- "Mean pain intensity at T1"

Hmisc::label(df$paindetect_totalscore_t0) <- "Total PainDetect score at T0"
Hmisc::label(df$paindetect_totalscore_t1) <- "Total PainDetect score at T1"

Hmisc::label(df$disability_t0) <- "Disability T0"
Hmisc::label(df$disability_t1) <- "Disability T1"

# Covariates
Hmisc::label(df$age) <- "Age (years)"

df$sex <- factor(df$sex,
                 levels = c(1, 2),
                 labels = c("male", "female")
)
Hmisc::label(df$sex) <- "Sex (male/female)"

Hmisc::label(df$medication) <- "Medication usage"
Hmisc::label(df$care) <- "Care"

Hmisc::label(df$rom_ext_mean) <- "Range of motion: Extension (mean of 2 measurements)"
Hmisc::label(df$rom_rot_r_mean) <- "Range of motion: Right rotation (mean of 2 measurements)"
Hmisc::label(df$rom_rot_l_mean) <- "Range of motion: Left rotation (mean of 2 measurements)"
Hmisc::label(df$rom_flex_mean) <- "Range of motion: Flexion (mean of 2 measurements)"
Hmisc::label(df$sum_rom)<- " Total sum of Range of motion"

Hmisc::label(df$wur_c2_r) <- "Wind-up ratio at C2 (right)"
Hmisc::label(df$wur_c2_l) <- "Wind-up ratio at C2 (left)"
Hmisc::label(df$wur_c2)<- "Wind-up ratio at C2"

Hmisc::label(df$wur_trap_r) <- "Wind-up ratio at M. trapezius (right)"
Hmisc::label(df$wur_trap_l) <- "Wind-up ratio at M. trapezius (left)"
Hmisc::label(df$wur_trap)<- "Wind-up ratio at M. trapezius"

Hmisc::label(df$recurrentpain_t0) <- "Recurrent pain episodes at T0 (0-4)"
Hmisc::label(df$recurrentpain_t1) <- "Recurrent pain episodes at T1 (0-4)"

Hmisc::label(df$mc_s_plane) <- "Movement Control tests (sagittal plane, 0-4)"

Hmisc::label(df$cmsz_mean_faults) <- "Movement accuracy"
Hmisc::label(df$cmsz_mean_time) <- "Time for movement accuracy test (seconds)"

Hmisc::label(df$headeye_number_pos) <- "Head-eye movement control: Number of positive tests (0-5)"

Hmisc::label(df$degree_r_AE) <- "Joint Position Error: Absolute error in right rotation (degrees)"
Hmisc::label(df$degree_l_AE) <- "Joint Position Error: Absolute error in left rotation (degrees)"


# other info
Hmisc::label(df$workinghours) <- "Working hours per week"
df$occupation <- factor(df$occupation,
                        levels = 1:7,
                        labels = c(
                          "part-time",
                          "full-time",
                          "looking for work",
                          "in training",
                          "housewife/househusband",
                          "disabled/partially disabled",
                          "retired"
                        )
)
Hmisc::label(df$occupation) <- "Occupation"

dim(df) # 156  68/72

table1::table1(~ age + sex + occupation + workinghours + 
                 care + medication + 
                 
                 disability_t0 + disability_t1 + 
                 
                 # NP intensity – PainDETECT:
                 paindetect1_t0 + paindetect2_t0 + paindetect3_t0 + 
                 
                 # recurrent pain episodes:
                 recurrentpain_t0 + recurrentpain_t1 + 
                 
                 # Pinprick: Wind-up ratio:
                 wur_c2_r + wur_c2_l + wur_c2 +
                 wur_trap_r + wur_trap_l + wur_trap+
                 
                 # Cervical movement sense:
                 cmsz_mean_faults + cmsz_mean_time +
                 
                 # Head-eye movement control:
                 headeye_number_pos +
                 
                 # Range of motion:
                 rom_flex_mean + rom_ext_mean + rom_rot_r_mean + rom_rot_l_mean + 
                 sum_rom +
                 
                 # joint position error:
                 degree_r_AE + degree_l_AE + jpe
               
               , data = df) # add variables in the desired order as needed

# many variables are skewed
# -> show 0, 25, 50, 75 and 100% quantile for all continuous variables:

render.quantiles.inline <- function(x) {
  q <- quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  q <- round(q, 2)
  paste(q, collapse = " / ")
}

render.continuous <- function(x) {
  render.quantiles.inline(x)
}

table1::table1( # N=156
  ~ age + sex + occupation + workinghours + 
    care + medication + 
    disability_t0 + disability_t1 +
    paindetect1_t0 + paindetect2_t0 + paindetect3_t0 + 
    recurrentpain_t0 + recurrentpain_t1 +
    wur_c2_r + wur_c2_l + wur_c2 + wur_trap_r + wur_trap_l + wur_trap + 
    cmsz_mean_faults + cmsz_mean_time +
    headeye_number_pos +
    rom_flex_mean + rom_ext_mean + rom_rot_r_mean + rom_rot_l_mean + sum_rom +
    degree_r_AE + degree_l_AE + jpe +
    mc_s_plane,
  data = df,
  render.continuous = render.continuous,
  footnote = "Continuous variables are summarized as: Min. / 25% Quantile / Median / 75% Quantile / Max."
)

# Table 1, with medians and IQR:------
render.continuous.iqr <- function(x) {
  q <- quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  q <- round(q, 2)
  paste0(q[2], " [", q[1], ", ", q[3], "]")
}

# Median + IQR:
table1::table1( # N=156
  ~ age + sex + occupation + workinghours + 
    care + medication + 
    disability_t0 + disability_t1 +
    paindetect1_t0 + paindetect2_t0 + paindetect3_t0 + 
    recurrentpain_t0 + recurrentpain_t1 +
    wur_c2_r + wur_c2_l + wur_trap_r + wur_trap_l +
    cmsz_mean_faults + cmsz_mean_time +
    headeye_number_pos +
    rom_flex_mean + rom_ext_mean + rom_rot_r_mean + rom_rot_l_mean +
    degree_r_AE + degree_l_AE +
    mc_s_plane,
  data = df,
  render.continuous = render.continuous.iqr,
  footnote = "Continuous variables are summarized as: Median [25%, 75%]"
)

dim(df) # 156 72

# Median + IQR stratified by any_missing:
df$any_missing <- ifelse(rowSums(is.na(df)) > 0, 1, 0)
df$any_missing <- factor(df$any_missing,
                         levels = c(0, 1),
                         labels = c("No missing values", "Any missing values")
)
length(df$any_missing) # 156
dim(df) # 156 69
table1::table1( # N=110 und N=46
  ~ age + sex + occupation + workinghours + 
    care + medication + 
    disability_t0 + disability_t1 +
    paindetect1_t0 + paindetect2_t0 + paindetect3_t0 + 
    recurrentpain_t0 + recurrentpain_t1 +
    wur_c2_r + wur_c2_l + wur_trap_r + wur_trap_l +
    cmsz_mean_faults + cmsz_mean_time +
    headeye_number_pos +
    rom_flex_mean + rom_ext_mean + rom_rot_r_mean + rom_rot_l_mean +
    degree_r_AE + degree_l_AE +
    mc_s_plane | any_missing,
  data = df,
  render.continuous = render.continuous.iqr,
  footnote = "Continuous variables are summarized as: Median [25%, 75%]"
)

# Table 1 with means and SD-------------
render.continuous.mean_sd <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  sprintf("%.2f (%.2f)", m, s)
}

table1::table1(
  ~ age + sex + occupation + workinghours + 
    care + medication + 
    disability_t0 + disability_t1 +
    paindetect1_t0 + paindetect2_t0 + paindetect3_t0 + 
    recurrentpain_t0 + recurrentpain_t1 +
    wur_c2_r + wur_c2_l + wur_trap_r + wur_trap_l +
    cmsz_mean_faults + cmsz_mean_time +
    headeye_number_pos +
    rom_flex_mean + rom_ext_mean + rom_rot_r_mean + rom_rot_l_mean +
    degree_r_AE + degree_l_AE +
    mc_s_plane | any_missing,
  data = df,
  render.continuous = render.continuous.mean_sd,
  footnote = "Continuous variables are summarized as: Mean (SD)"
)

# not stratified:
table1::table1(
  ~ age + sex + occupation + workinghours + 
    care + medication + 
    disability_t0 + disability_t1 +
    paindetect1_t0 + paindetect2_t0 + paindetect3_t0 + 
    recurrentpain_t0 + recurrentpain_t1 +
    wur_c2_r + wur_c2_l + wur_trap_r + wur_trap_l +
    cmsz_mean_faults + cmsz_mean_time +
    headeye_number_pos +
    rom_flex_mean + rom_ext_mean + rom_rot_r_mean + rom_rot_l_mean +
    degree_r_AE + degree_l_AE +
    mc_s_plane,
  data = df,
  render.continuous = render.continuous.mean_sd,
  footnote = "Continuous variables are summarized as: Mean (SD)"
)



# .) Missing values-------------------------------------------------------------
# __plot--------------
vis_miss_pretty <- function(df) {
  visdat::vis_miss(df) +
    ggplot2::theme(
      axis.text.x.top  = ggplot2::element_text(
        angle  = 45,
        hjust  = 0,
        vjust  = 0.5,
        margin = ggplot2::margin(b = -20)
      ),
      axis.title.x.top = ggplot2::element_text(
        margin = ggplot2::margin(b = 25)
      ),
      plot.margin = ggplot2::margin(
        t = 10, 
        r = 80,
        b = 10, 
        l = 10
      )
    )
}
vis_miss_pretty(df)
# Overall missingness 4.3%

# __Max column missingness---------
head(sort(colSums(is.na(df))/nrow(df)*100, decreasing = TRUE), 10)
# 23.7%, 16.66%...

# Some individuals have a large proportion of missings.
# __Missingness in rows (obs.) in percent:----------
df$missingness_percent <- rowSums(is.na(df))/ncol(df)*100
hist(df$missingness_percent)
# how many more than 20% missings?
table(df$missingness_percent > 20) # 7 rows
# how many more than 30% missings?
table(df$missingness_percent > 30) # 4 rows
max(df$missingness_percent) # 97%

# __REMOVE rows (obs.) with > 30% missings---------
dim(df)
dim(df %>% dplyr::filter(missingness_percent <= 30)) # -> 4 lost
df <- df %>% dplyr::filter(missingness_percent <= 30)
dim(df) # 152

vis_miss_pretty(df) # missings
# overall 2.5%

# __Impute (kNN)------
if (impute_TRUE_FALSE) {
  df_imp <- kNN(df) # k-nearest-neighbour-imputation
}

vis_miss(df_imp) # no missings if imputed


# .) (Prediction-) Models-------------------------------------------------------
#__----
# __Model 1) disability_t1 as outcome-------------------------------------------
dim(df) # 152
colnames(df_imp)
vis_miss_pretty(df_imp)
sum(is.na(df_imp)) # 0)
dim(df_imp |> droplevels())
# 152

model1 <- lm(disability_t1 ~
               # baseline outcome:
               disability_t0 + 
               # pain history:
               recurrentpain_t0 +
               # demographics:
               age + sex + occupation +
               #medication + # qqPlot error if included
               # range of motion:
               rom_flex_mean + rom_ext_mean + rom_rot_r_mean + rom_rot_l_mean +
               # wind-up ratios:
               wur_c2_r + wur_c2_l + wur_trap_r + wur_trap_l +
               # cervical movement sense:
               cmsz_mean_faults + cmsz_mean_time +
               # head-eye movement control:
               headeye_number_pos +
               # joint position error:
               degree_r_AE + degree_l_AE +
               # movement control:
               mc_s_plane, # 19 predictors
             data = df_imp)
check_model(model1) # PPC could be improved
check_model(model1, check = "pp_check") # not so bad
vif(model1) # 

qqPlot(model1) # perfect
summary(model1) # Adjusted R-squared:  0.4714 
sum(predict(model1) < 0) # 0

# ____Best Subsets---------
# 19
preds <- c(
  "disability_t0", # here with baseline
  "age", "sex",
  "occupation",
  "wur_c2", "wur_trap",
  "cmsz_mean_faults", "cmsz_mean_time",
  "headeye_number_pos",
  "sum_rom",
  "jpe",
  "mc_s_plane",
  "recurrentpain_t0"
)
dat_red <- df_imp[, c("disability_t1", preds)]
colnames(dat_red)

tic()
regfit <- regsubsets(
  disability_t1 ~ .,
  data   = dat_red,  
  nbest = 3,
  nvmax  = length(preds),
)
toc()
# -> 3  linear dependencies found!

X <- model.matrix(disability_t1 ~ ., data = dat_red)[, -1]  # without disability_t1
lin_deps <- findLinearCombos(X)
lin_deps
X[, lin_deps$remove] # check removed columns
#  occupationhousewife/househusband occupationdisabled/partially 
# disabled occupationretired -> do not occur

dat_red_w_lin_dep <- dat_red |>
  dplyr::filter(
    !occupation %in% c(
      "housewife/househusband",
      "disabled/partially disabled",
      "retired"
    )
  ) |>
  droplevels()
dim(dat_red_w_lin_dep) # 152

# repeat best subsets
regfit <- regsubsets(
  disability_t1 ~ .,
  data   = dat_red_w_lin_dep,  
  nbest = 3,
  nvmax  = length(preds),
)

reg_summary <- summary(regfit)
reg_summary

# ____Analyse resulting models-------------
best_id_bic   <- which.min(reg_summary$bic)
best_id_adjr2 <- which.max(reg_summary$adjr2)

coefs_bic  <- coef(regfit, id = best_id_bic)
coefs_bic
# (Intercept)    disability_t0       wur_trap_l recurrentpain_t0 
#  -5.3389862        0.5221082        2.0719736        3.0392899 

vars_bic   <- coef_to_vars(names(coefs_bic)[-1], preds)   # map factor dummies -> original vars

coefs_adjr2 <- coef(regfit, id = best_id_adjr2)
coefs_adjr2

vars_adjr2  <- coef_to_vars(names(coefs_adjr2)[-1], preds)

form_bic   <- reformulate(vars_bic,  response = "disability_t1")
form_adjr2 <- reformulate(vars_adjr2, response = "disability_t1")

form_bic
# disability_t1 ~ disability_t0 + wur_trap + recurrentpain_t0

form_adjr2
# disability_t1 ~ disability_t0 + wur_trap + cmsz_mean_faults + 
# cmsz_mean_time + headeye_number_pos + recurrentpain_t0

model_bic   <- lm(form_bic,   data = dat_red)
model_adjr2 <- lm(form_adjr2, data = dat_red)

summary(model_bic) # Adjusted R-squared:  0.487 
confint(model_bic)
summary(model_adjr2) # Adjusted R-squared:  0.502 
confint(model_adjr2)

check_model(model_bic)
check_model(model_bic, check = "pp_check") # not so bad
qqPlot(model_bic) # perfect

check_model(model_adjr2)
qqPlot(model_adjr2) # perfect

# exploratory! F-test, models are nested.
anova(model_bic, model_adjr2) # 0.063

# still: BIC-model much more parsimonious with almost identical adj.R^2.
# BIC penalizes model complexity more strongly.
# the smaller model provides us the opportunity to visualize the model.

# ____Results of BIC-model---------
tbl_bic <- tbl_regression(
  model_bic,
  intercept = TRUE
) |>
  modify_table_body(
    ~ dplyr::select(.x, -dplyr::any_of("p.value"))
  )
tbl_bic


# ____Visualize BIC-model---------
plot_3d_group <- function(data, model, pain_level, y_scale = 10) {
  
  # --- Grid: voller Wertebereich des Datensatzes ---
  x_seq <- seq(min(data$disability_t0, na.rm = TRUE),
               max(data$disability_t0, na.rm = TRUE),
               length.out = 40)
  
  y_seq <- seq(min(data$wur_trap, na.rm = TRUE),
               max(data$wur_trap, na.rm = TRUE),
               length.out = 40)

  grid <- expand.grid(
    disability_t0 = x_seq,
    wur_trap      = y_seq,   # model_bic now uses the summed wur_trap
    recurrentpain_t0 = pain_level   # FIXIERT für die Fläche!
  )
  
  # --- Ebene aus dem globalen Modell ---
  z_pred <- predict(model, newdata = grid)
  z_mat  <- matrix(z_pred, nrow = length(x_seq), ncol = length(y_seq))
  
  # --- Nur für die Rohpunkte gefiltert nach Pain-Level ---
  df_sub <- data %>%
    filter(recurrentpain_t0 == pain_level)
  
  # y-Achse skalieren (für Punkte und Ebene)
  df_sub$wur_scaled <- df_sub$wur_trap * y_scale
  y_scaled <- y_seq * y_scale
  
  # --- 3D-PLOT ---
  plotly::plot_ly() |>
    plotly::add_markers(
      data = df_sub,
      x = ~disability_t0,
      y = ~wur_scaled,
      z = ~disability_t1,
      marker = list(size = 4),
      name = "Observed"
    ) |>
    plotly::add_surface(
      x = x_seq,
      y = y_scaled,
      z = z_mat,
      opacity = 0.5,
      showscale = FALSE,
      name = "Regression plane"
    ) |>
    plotly::layout(
      title = list(
        text = paste("Conditional regression plane for recurrentpain_t0 =", pain_level),
        x = 0.5
      ),
      scene = list(
        xaxis = list(title = "Disability T0"),
        yaxis = list(title = paste0("Wind-up ratio × ", y_scale)),
        zaxis = list(title = "Disability T1"),
        aspectmode = "cube"
      )
    )
}

data3d <- df_imp %>%
  dplyr::select(disability_t0, disability_t1,
                wur_trap, recurrentpain_t0) %>%
  na.omit()

p0 <- plot_3d_group(data3d, model_bic, pain_level = 0)
p1 <- plot_3d_group(data3d, model_bic, pain_level = 1)
p2 <- plot_3d_group(data3d, model_bic, pain_level = 2)
p3 <- plot_3d_group(data3d, model_bic, pain_level = 3)

p0  
p1
p2
p3

# _____even better:-----------
plot_3d_all_levels <- function(data, model, y_scale = 10) {
  
  pain_levels <- sort(unique(data$recurrentpain_t0))
  
  cols <- brewer.pal(length(pain_levels), "Set1")
  
  x_seq <- seq(min(data$disability_t0, na.rm = TRUE),
               max(data$disability_t0, na.rm = TRUE),
               length.out = 40)
  
  y_seq <- seq(min(data$wur_trap, na.rm = TRUE),
               max(data$wur_trap, na.rm = TRUE),
               length.out = 40)

  p <- plotly::plot_ly()

  for (i in seq_along(pain_levels)) {
    pl    <- pain_levels[i]
    col_i <- cols[i]

    grid <- expand.grid(
      disability_t0    = x_seq,
      wur_trap         = y_seq,
      recurrentpain_t0 = pl
    )
    
    z_pred <- predict(model, newdata = grid)
    z_mat  <- matrix(z_pred, nrow = length(x_seq), ncol = length(y_seq))
    
    y_scaled <- y_seq * y_scale
    
    p <- p |>
      plotly::add_surface(
        x = x_seq,
        y = y_scaled,
        z = z_mat,
        surfacecolor = matrix(1, nrow = length(x_seq), ncol = length(y_seq)),
        colorscale   = list(list(0, col_i), list(1, col_i)),
        opacity      = 0.35,
        showscale    = FALSE,
        showlegend   = FALSE,         
        name         = paste("Plane pain", pl)
      )
  }
  
  data <- data %>%
    mutate(wur_scaled = wur_trap * y_scale)
  
  for (i in seq_along(pain_levels)) {
    pl    <- pain_levels[i]
    col_i <- cols[i]
    
    df_i <- data %>% filter(recurrentpain_t0 == pl)
    
    p <- p |>
      plotly::add_markers(
        data   = df_i,
        x      = ~disability_t0,
        y      = ~wur_scaled,
        z      = ~disability_t1,
        marker = list(size = 4, color = col_i),
        name   = paste("Pain episodes", pl)
      )
  }
  
  p <- p |>
    plotly::layout(
      title = list(
        text = "Regression planes by recurrent pain episodes at T0",
        x = 0.5
      ),
      scene = list(
        xaxis = list(title = "Disability T0"),
        yaxis = list(title = paste0("Wind-up ratio × ", y_scale)),
        zaxis = list(title = "Disability T1"),
        aspectmode = "cube"
      ),
      legend = list(
        title = list(text = "Recurrent pain\nepisodes at T0")
      )
    )
  
  p
}

data3d <- df_imp |>
  dplyr::select(disability_t0, disability_t1, wur_trap, recurrentpain_t0) |>
  na.omit() # caution...

p_all <- plot_3d_all_levels(data3d, model_bic, y_scale = 10)
p_all

# ____Model without disability_t0:--------
# before: disability_t1 ~ disability_t0 + wur_trap + recurrentpain_t0

preds1 <- c(
  "age", "sex",
  "occupation",
  "wur_c2", "wur_trap",
  "cmsz_mean_faults", "cmsz_mean_time",
  "headeye_number_pos",
  "sum_rom",
  "jpe",
  "mc_s_plane",
  "recurrentpain_t0"
)
str(preds1)
class(preds1)

dat_red1 <- df_imp[, c("disability_t1", preds1)]
colnames(dat_red1)
# NOTE: sex is already a factor (set above); no re-factoring needed here.

tic()
regfit <- regsubsets(
  disability_t1 ~ .,
  data   = dat_red1,  
  nbest = 3,
  nvmax  = length(preds1),
)
toc()
# -> 3  linear dependencies found!

X <- model.matrix(disability_t1 ~ ., data = dat_red1)[, -1]  # without disability_t1
lin_deps <- findLinearCombos(X)
lin_deps
X[, lin_deps$remove] # check removed columns
#  occupationhousewife/househusband occupationdisabled/partially 
# disabled occupationretired -> do not occur

dat_red_w_lin_dep1 <- dat_red1 |>
  dplyr::filter(
    !occupation %in% c(
      "housewife/househusband",
      "disabled/partially disabled",
      "retired"
    )
  ) |>
  droplevels()
dim(dat_red_w_lin_dep1) # 152

# repeat best subsets
regfit <- regsubsets(
  disability_t1 ~ .,
  data   = dat_red_w_lin_dep1,  
  nbest = 3,
  nvmax  = length(preds1),
)

reg_summary1 <- summary(regfit)
reg_summary1

# ____Analyse resulting models-------------
# FIX: use reg_summary1 (no-baseline search), NOT reg_summary (with baseline).
best_id_bic   <- which.min(reg_summary1$bic)
best_id_adjr2 <- which.max(reg_summary1$adjr2)

coefs_bic  <- coef(regfit, id = best_id_bic)
coefs_bic
#(Intercept)      wur_trap      cmsz_mean_faults  recurrentpain_t0 
# -0.0007886883     2.8696739879     0.5716562673     4.1114288991 

vars_bic   <- coef_to_vars(names(coefs_bic)[-1], preds1)   # map factor dummies -> original vars

coefs_adjr2 <- coef(regfit, id = best_id_adjr2)
coefs_adjr2

vars_adjr2  <- coef_to_vars(names(coefs_adjr2)[-1], preds1)

form_bic   <- reformulate(vars_bic,  response = "disability_t1")
form_adjr2 <- reformulate(vars_adjr2, response = "disability_t1")

form_bic
# disability_t1 ~ wur_trap + cmsz_mean_faults + recurrentpain_t0

form_adjr2


model_bic   <- lm(form_bic,   data = dat_red1)

model_adjr2 <- lm(disability_t1 ~ sex + wur_trap + cmsz_mean_faults + 
                  mc_s_plane + headeye_number_pos + recurrentpain_t0, data = dat_red1)

summary(model_bic) # Adjusted R-squared:  0.25 
confint(model_bic)
summary(model_adjr2) # Adjusted R-squared:  0.28 
confint(model_adjr2)

check_model(model_bic)
check_model(model_bic, check = "pp_check") # not so bad
qqPlot(model_bic) # perfect

check_model(model_adjr2)
qqPlot(model_adjr2) # perfect

# exploratory! F-test, models are nested.
anova(model_bic, model_adjr2) # 0.056

# still: BIC-model much more parsimonious with almost identical adj.R^2.
# BIC penalizes model complexity more strongly.
# the smaller model provides us the opportunity to visualize the model.

# ____Results of BIC-model---------
tbl_bic <- tbl_regression(
  model_bic,
  intercept = TRUE
) |>
  modify_table_body(
    ~ dplyr::select(.x, -dplyr::any_of("p.value"))
  )
tbl_bic

model1_no_disability_t0 <- lm(disability_t1 ~ 
                                wur_trap + cmsz_mean_faults+
                                recurrentpain_t0,
                              data = df_imp)
check_model(model1_no_disability_t0) # ok
summary(model1_no_disability_t0) # Adjusted R-squared:  0.2573 
confint(model1_no_disability_t0)
#__-----

# __Model 2) paindetect2_t1 (strongest) as outcome------------------------------

colnames(df_imp)
model2 <- lm(paindetect2_t1 ~
               # baseline outcome:
               #disability_t0 + # ...CHECK with other authors....
               paindetect2_t0 +
               # pain history:
               recurrentpain_t0 +
               # demographics:
               age + sex + occupation +
               #medication + # qqPlot error if included
               # range of motion:
               sum_rom +
               # wind-up ratios:
               wur_c2 + wur_trap +
               # cervical movement sense:
               cmsz_mean_faults + cmsz_mean_time +
               # head-eye movement control:
               headeye_number_pos +
               # joint position error:
               jpe +
               # movement control:
               mc_s_plane,
             data = df_imp)

check_model(model2) # PPC could be improved
check_model(model2, check = "pp_check") # a bit off?
vif(model2) # ok
qqPlot(model2) # not perfect... some structure in the middle
summary(model2) # Adjusted R-squared:  0.2171 

# ___Best Subsets---------
preds2 <- c(
  #"disability_t0",
  "paindetect2_t0",
  "age", "sex",
  "occupation",
  "wur_c2", "wur_trap",
  "cmsz_mean_faults", "cmsz_mean_time",
  "headeye_number_pos",
  "sum_rom",
  "jpe",
  "mc_s_plane",
  "recurrentpain_t0"
)
dat_red2 <- df_imp[, c("paindetect2_t1", preds2)]
colnames(dat_red2)
vis_miss_pretty(dat_red2)
sum(is.na(dat_red2)) # 0

regfit2 <- regsubsets(
  paindetect2_t1 ~ .,
  data   = dat_red2,
  nbest = 3,
  nvmax  = length(preds2),
)
# -> 3  linear dependencies found!

X2 <- model.matrix(paindetect2_t1 ~ ., data = dat_red2)[, -1]  # without outcome
lin_deps2 <- findLinearCombos(X2)
lin_deps2
X2[, lin_deps2$remove] # check removed columns
#  occupationhousewife/househusband occupationdisabled/partially
# disabled occupationretired ->

dat_red2_w_lin_dep <- dat_red2 |>
  dplyr::filter(
    !occupation %in% c(
      "housewife/househusband",
      "disabled/partially disabled",
      "retired"
    )
  ) |>
  droplevels()

# repeat best subsets
regfit2 <- regsubsets(
  paindetect2_t1 ~ .,
  data   = dat_red2_w_lin_dep,
  nbest = 3,
  nvmax  = length(preds2),
)

reg_summary2 <- summary(regfit2)

# ___Analyse resulting models-------------
best_id_bic2   <- which.min(reg_summary2$bic)
best_id_adjr22 <- which.max(reg_summary2$adjr2)

coefs_bic2  <- coef(regfit2, id = best_id_bic2)
coefs_bic2
# (Intercept)   paindetect2_t0 cmsz_mean_faults recurrentpain_t0 
# 1.0009859        0.4075076        0.1274705        0.6417801 

vars_bic2   <- coef_to_vars(names(coefs_bic2)[-1], preds2)   # map factor dummies -> original vars

form_bic2   <- reformulate(vars_bic2,  response = "paindetect2_t1")
form_bic2
# paindetect2_t1 ~ paindetect2_t0 + cmsz_mean_faults + recurrentpain_t0

# Parsimonious model (baseline + pain history), consistent with the other
# outcomes and with the visualization below:
model_bic2   <- lm(form_bic2, 
                   data = dat_red2)
check_model(model_bic2)
check_model(model_bic2, check = "pp_check") # a bit off?
summary(model_bic2) # Adjusted R-squared:  0.1926 
qqPlot(model_bic2) # not so perfect, but ok

# ___Results of BIC-model---------
tbl_bic2 <- tbl_regression(
  model_bic2,
  intercept = TRUE
) |>
  modify_table_body(
    ~ dplyr::select(.x, -dplyr::any_of("p.value"))
  )
tbl_bic2

# ___Model without baseline (paindetect2_t0)--------------
# drop the baseline pain predictor, keep the pain history
#model_bic2
# formula = paindetect2_t1 ~ paindetect2_t0 + recurrentpain_t0

model2a <- lm(paindetect2_t1 ~
               # baseline outcome:
               #disability_t0 + # ...CHECK with other authors....
               # pain history:
               recurrentpain_t0 +
               # demographics:
               age + sex + occupation +
               #medication + # qqPlot error if included
               # range of motion:
               sum_rom +
               # wind-up ratios:
               wur_c2 + wur_trap +
               # cervical movement sense:
               cmsz_mean_faults + cmsz_mean_time +
               # head-eye movement control:
               headeye_number_pos +
               # joint position error:
               jpe +
               # movement control:
               mc_s_plane,
             data = df_imp)

check_model(model2a) # PPC could be improved
check_model(model2a, check = "pp_check") # a bit off?
vif(model2a) # ok
qqPlot(model2a) # not perfect... some structure in the middle
summary(model2a) # Adjusted R-squared:  0.13 without baseline variable 

# ___Best Subsets---------
preds2a <- c(
  #"disability_t0",
  "age", "sex",
  "occupation",
  "wur_c2", "wur_trap",
  "cmsz_mean_faults", "cmsz_mean_time",
  "headeye_number_pos",
  "sum_rom",
  "jpe",
  "mc_s_plane",
  "recurrentpain_t0"
)
dat_red2a <- df_imp[, c("paindetect2_t1", preds2a)]
colnames(dat_red2a)
vis_miss_pretty(dat_red2a)
sum(is.na(dat_red2a)) # 0

regfit2 <- regsubsets(
  paindetect2_t1 ~ .,
  data   = dat_red2a,
  nbest = 3,
  nvmax  = length(preds2a),
)
# -> 3  linear dependencies found!

X2a <- model.matrix(paindetect2_t1 ~ ., data = dat_red2a)[, -1]  # without outcome
lin_deps2a <- findLinearCombos(X2a)   # FIX: use X2a, not X2
lin_deps2a
X2a[, lin_deps2a$remove] # check removed columns
#  occupationhousewife/househusband occupationdisabled/partially
# disabled occupationretired ->

dat_red2_w_lin_dep <- dat_red2a |>
  dplyr::filter(
    !occupation %in% c(
      "housewife/househusband",
      "disabled/partially disabled",
      "retired"
    )
  ) |>
  droplevels()

# repeat best subsets
regfit2 <- regsubsets(
  paindetect2_t1 ~ .,
  data   = dat_red2_w_lin_dep,
  nbest = 3,
  nvmax  = length(preds2a),
)

reg_summary2a <- summary(regfit2)

# ___Analyse resulting models-------------
# FIX: use reg_summary2a (no-baseline search), NOT reg_summary2 (with baseline).
best_id_bic2a   <- which.min(reg_summary2a$bic)
best_id_adjr22a <- which.max(reg_summary2a$adjr2)

coefs_bic2a  <- coef(regfit2, id = best_id_bic2a)
coefs_bic2a

vars_bic2a   <- coef_to_vars(names(coefs_bic2a)[-1], preds2a)   # map factor dummies -> original vars

form_bic2a   <- reformulate(vars_bic2a,  response = "paindetect2_t1")
form_bic2a
# Without the baseline predictor, the BIC-optimal model for paindetect2_t1
# reduces to: paindetect2_t1 ~ recurrentpain_t0

model_bic2a   <- lm(form_bic2a,
                   data = dat_red2a)
check_model(model_bic2a)
check_model(model_bic2a, check = "pp_check") # a bit off?
summary(model_bic2a) # Adjusted R-squared:  0.15 
confint(model_bic2a)
qqPlot(model_bic2a) # not so perfect, but ok

#model2_no_paindetect2_t0 <- lm(paindetect2_t1 ~
#                                 recurrentpain_t0,
#                               data = df_imp)
#check_model(model2_no_paindetect2_t0)
#check_model(model2_no_paindetect2_t0, check = "pp_check") # not so perfect
#qqPlot(model2_no_paindetect2_t0)
#summary(model2_no_paindetect2_t0) # Adjusted R-squared:  0.1157 



# ___Visualize BIC-model---------
data3d_pd <- df_imp |>
  dplyr::select(
    paindetect2_t0,
    paindetect2_t1,
    recurrentpain_t0,
    cmsz_mean_faults
  ) |>
  na.omit()

plot_3d_paindetect <- function(data, model, y_scale = 1) {
  
  x_seq <- seq(
    min(data$paindetect2_t0, na.rm = TRUE),
    max(data$paindetect2_t0, na.rm = TRUE),
    length.out = 40
  )
  
  y_seq <- seq(
    min(data$recurrentpain_t0, na.rm = TRUE),
    max(data$recurrentpain_t0, na.rm = TRUE),
    length.out = 40
  )
  
  grid <- expand.grid(
    paindetect2_t0 = x_seq,
    recurrentpain_t0      = y_seq
  )
  # model_bic2 also contains cmsz_mean_faults -> hold it at its mean for the surface
  grid$cmsz_mean_faults <- mean(data$cmsz_mean_faults, na.rm = TRUE)

  z_pred <- predict(model, newdata = grid)
  z_mat  <- matrix(z_pred,
                   nrow = length(x_seq),
                   ncol = length(y_seq))
  
  data$rec_scaled <- data$recurrentpain_t0 * y_scale
  y_scaled        <- y_seq * y_scale
  
  plot_ly() |>
    add_markers(
      data = data,
      x    = ~paindetect2_t0,
      y    = ~rec_scaled,
      z    = ~paindetect2_t1,
      marker = list(size = 4),
      name   = "Observed"
    ) |>
    add_surface(
      x = x_seq,
      y = y_scaled,
      z = z_mat,
      opacity   = 0.5,
      showscale = FALSE,
      name      = "Regression plane"
    ) |>
    layout(
      title = list(
        text = "Regression surface: PainDetect2 at T1",
        x = 0.5
      ),
      scene = list(
        xaxis = list(title = "PainDetect2 at T0"),
        yaxis = list(
          title    = "Recurrent pain episodes at T0",
          tickvals = sort(unique(data$recurrentpain_t0)) * y_scale,
          ticktext = sort(unique(data$recurrentpain_t0))
        ),
        zaxis = list(title = "PainDetect2 at T1"),
        aspectmode = "cube"
      )
    )
}

p_pd <- plot_3d_paindetect(data3d_pd, model_bic2, y_scale = 1)
p_pd # wide spread, sigma large.

# __model 3) paindetect3_t1 (average pain) as outcome---------------------------
# with and without baseline (paindetect3_t0), and (as before) without disability
# as a predictor.

colnames(df_imp)
model3 <- lm(paindetect3_t1 ~
               # baseline outcome:
               paindetect3_t0 +
               # pain history:
               recurrentpain_t0 +
               # demographics:
               age + sex + occupation +
               #medication + # qqPlot error if included
               # range of motion:
               sum_rom +
               # wind-up ratios:
               wur_c2 + wur_trap +
               # cervical movement sense:
               cmsz_mean_faults + cmsz_mean_time +
               # head-eye movement control:
               headeye_number_pos +
               # joint position error:
               jpe +
               # movement control:
               mc_s_plane,
             data = df_imp)
check_model(model3) # PPC could be improved
check_model(model3, check = "pp_check")
vif(model3)
qqPlot(model3) # ok, maybe a bit structure in the middle
summary(model3) # Adjusted R-squared:  0.32 

# ___Best Subsets---------
preds3 <- c(
  "paindetect3_t0",
  "age", "sex",
  "occupation",
  "wur_c2", "wur_trap",
  "cmsz_mean_faults", "cmsz_mean_time",
  "headeye_number_pos",
  "sum_rom",
  "jpe",
  "mc_s_plane",
  "recurrentpain_t0"
)
dat_red3 <- df_imp[, c("paindetect3_t1", preds3)]
colnames(dat_red3)
sum(is.na(dat_red3)) # 0

regfit3 <- regsubsets(
  paindetect3_t1 ~ .,
  data   = dat_red3,
  nbest = 3,
  nvmax  = length(preds3),
)
# -> linear dependencies found (occupation dummies), as for the other outcomes

X3 <- model.matrix(paindetect3_t1 ~ ., data = dat_red3)[, -1]  # without outcome
lin_deps3 <- findLinearCombos(X3)
lin_deps3

dat_red3_w_lin_dep <- dat_red3 |>
  dplyr::filter(
    !occupation %in% c(
      "housewife/househusband",
      "disabled/partially disabled",
      "retired"
    )
  ) |>
  droplevels()

# repeat best subsets
regfit3 <- regsubsets(
  paindetect3_t1 ~ .,
  data   = dat_red3_w_lin_dep,
  nbest = 3,
  nvmax  = length(preds3),
)

reg_summary3 <- summary(regfit3)

# ___Analyse resulting models-------------
best_id_bic3   <- which.min(reg_summary3$bic)
best_id_adjr23 <- which.max(reg_summary3$adjr2)

coefs_bic3  <- coef(regfit3, id = best_id_bic3)
coefs_bic3
# (Intercept)   paindetect3_t0         wur_c2 recurrentpain_t0 
# 0.02525233       0.39291356       0.49968920       0.68895486 

vars_bic3   <- coef_to_vars(names(coefs_bic3)[-1], preds3)   # map factor dummies -> original vars

form_bic3   <- reformulate(vars_bic3,  response = "paindetect3_t1")
form_bic3
# paindetect3_t1 ~ paindetect3_t0 + wur_c2 + recurrentpain_t0

model_bic3 <- lm(form_bic3, data = dat_red3)
check_model(model_bic3)
check_model(model_bic3, check = "pp_check") # not so perfect
summary(model_bic3) # Adjusted R-squared:  0.2935 
qqPlot(model_bic3) # ok

# ___Results of BIC-model---------
tbl_bic3 <- tbl_regression(
  model_bic3,
  intercept = TRUE
) |>
  modify_table_body(
    ~ dplyr::select(.x, -dplyr::any_of("p.value"))
  )
tbl_bic3

colnames(df_imp)
model3a <- lm(paindetect3_t1 ~
               # baseline outcome:
               # pain history:
               recurrentpain_t0 +
               # demographics:
               age + sex + occupation +
               #medication + # qqPlot error if included
               # range of motion:
               sum_rom +
               # wind-up ratios:
               wur_c2 + wur_trap +
               # cervical movement sense:
               cmsz_mean_faults + cmsz_mean_time +
               # head-eye movement control:
               headeye_number_pos +
               # joint position error:
               jpe +
               # movement control:
               mc_s_plane,
             data = df_imp)
check_model(model3a) # PPC could be improved
check_model(model3a, check = "pp_check")
vif(model3a)
qqPlot(model3a) # ok, maybe a bit structure in the middle
summary(model3a) # Adjusted R-squared:  0.22 

# ___Best Subsets---------
preds3a <- c(
  "age", "sex",
  "occupation",
  "wur_c2", "wur_trap",
  "cmsz_mean_faults", "cmsz_mean_time",
  "headeye_number_pos",
  "sum_rom",
  "jpe",
  "mc_s_plane",
  "recurrentpain_t0"
)
dat_red3a <- df_imp[, c("paindetect3_t1", preds3a)]
colnames(dat_red3a)
sum(is.na(dat_red3a)) # 0

regfit3a <- regsubsets(
  paindetect3_t1 ~ .,
  data   = dat_red3a,
  nbest = 3,
  nvmax  = length(preds3a),
)
# -> linear dependencies found (occupation dummies), as for the other outcomes

X3 <- model.matrix(paindetect3_t1 ~ ., data = dat_red3a)[, -1]  # without outcome
lin_deps3 <- findLinearCombos(X3)
lin_deps3

dat_red3_w_lin_dep <- dat_red3a |>
  dplyr::filter(
    !occupation %in% c(
      "housewife/househusband",
      "disabled/partially disabled",
      "retired"
    )
  ) |>
  droplevels()

# repeat best subsets
regfit3a <- regsubsets(
  paindetect3_t1 ~ .,
  data   = dat_red3_w_lin_dep,
  nbest = 3,
  nvmax  = length(preds3a),
)

reg_summary3a <- summary(regfit3a)

# ___Analyse resulting models-------------
best_id_bic3a   <- which.min(reg_summary3a$bic)
best_id_adjr23a <- which.max(reg_summary3a$adjr2)

coefs_bic3a  <- coef(regfit3a, id = best_id_bic3a)
coefs_bic3a
# 

vars_bic3a   <- coef_to_vars(names(coefs_bic3a)[-1], preds3a)   # map factor dummies -> original vars

form_bic3a   <- reformulate(vars_bic3a,  response = "paindetect3_t1")
form_bic3a
# paindetect3_t1 ~ paindetect3_t0 + wur_c2 + recurrentpain_t0

model_bic3a <- lm(form_bic3a, data = dat_red3a)
check_model(model_bic3a)
check_model(model_bic3a, check = "pp_check") # not so perfect
summary(model_bic3a) # Adjusted R-squared:  0.20 
confint(model_bic3a)
qqPlot(model_bic3a) # ok

# ___Results of BIC-model---------
tbl_bic3 <- tbl_regression(
  model_bic3,
  intercept = TRUE
) |>
  modify_table_body(
    ~ dplyr::select(.x, -dplyr::any_of("p.value"))
  )
tbl_bic3


# ___Model WITHOUT baseline (paindetect3_t0)-------------------------------------
model3_no_paindetect3_t0 <- lm(paindetect3_t1 ~
                                 wur_c2 +
                                 recurrentpain_t0,
                               data = df_imp)
check_model(model3_no_paindetect3_t0)
check_model(model3_no_paindetect3_t0, check = "pp_check") # could be better
qqPlot(model3_no_paindetect3_t0) # ~ok
summary(model3_no_paindetect3_t0) # Adjusted R-squared:  0.1884 

# ___Visualize BIC-model---------
# The BIC-model has three predictors (paindetect3_t0 + wur_c2 + recurrentpain_t0),
# so a single surface is not enough. As for Model 1, we draw one regression plane
# over the two continuous predictors (paindetect3_t0 x wur_c2) per recurrentpain
# level, and overlay the observed points coloured by the same level.
data3d_pd3 <- df_imp |>
  dplyr::select(
    paindetect3_t0,
    paindetect3_t1,
    wur_c2,
    recurrentpain_t0
  ) |>
  na.omit()

plot_3d_paindetect3 <- function(data, model, y_scale = 10) {
  
  pain_levels <- sort(unique(data$recurrentpain_t0))
  
  cols <- brewer.pal(max(3, length(pain_levels)), "Set1")
  
  x_seq <- seq(min(data$paindetect3_t0, na.rm = TRUE),
               max(data$paindetect3_t0, na.rm = TRUE),
               length.out = 40)
  
  y_seq <- seq(min(data$wur_c2, na.rm = TRUE),
               max(data$wur_c2, na.rm = TRUE),
               length.out = 40)
  
  p <- plotly::plot_ly()
  
  # one regression plane per recurrentpain level
  for (i in seq_along(pain_levels)) {
    pl    <- pain_levels[i]
    col_i <- cols[i]
    
    grid <- expand.grid(
      paindetect3_t0   = x_seq,
      wur_c2           = y_seq,
      recurrentpain_t0 = pl
    )
    
    z_pred <- predict(model, newdata = grid)
    z_mat  <- matrix(z_pred, nrow = length(x_seq), ncol = length(y_seq))
    
    y_scaled <- y_seq * y_scale
    
    p <- p |>
      plotly::add_surface(
        x = x_seq,
        y = y_scaled,
        z = z_mat,
        surfacecolor = matrix(1, nrow = length(x_seq), ncol = length(y_seq)),
        colorscale   = list(list(0, col_i), list(1, col_i)),
        opacity      = 0.35,
        showscale    = FALSE,
        showlegend   = FALSE,
        name         = paste("Plane pain", pl)
      )
  }
  
  # observed points, coloured by recurrentpain level
  data <- data |>
    dplyr::mutate(wur_scaled = wur_c2 * y_scale)
  
  for (i in seq_along(pain_levels)) {
    pl    <- pain_levels[i]
    col_i <- cols[i]
    
    df_i <- data |> dplyr::filter(recurrentpain_t0 == pl)
    
    p <- p |>
      plotly::add_markers(
        data   = df_i,
        x      = ~paindetect3_t0,
        y      = ~wur_scaled,
        z      = ~paindetect3_t1,
        marker = list(size = 4, color = col_i),
        name   = paste("Pain episodes", pl)
      )
  }
  
  p |>
    plotly::layout(
      title = list(
        text = "Regression planes: PainDetect3 at T1 by recurrent pain episodes at T0",
        x = 0.5
      ),
      scene = list(
        xaxis = list(title = "PainDetect3 at T0"),
        yaxis = list(title = paste0("Wind-up ratio C2 x ", y_scale)),
        zaxis = list(title = "PainDetect3 at T1"),
        aspectmode = "cube"
      ),
      legend = list(
        title = list(text = "Recurrent pain\nepisodes at T0")
      )
    )
}

p_pd3 <- plot_3d_paindetect3(data3d_pd3, model_bic3, y_scale = 10)
p_pd3


# .) Internal validation (bootstrap optimism correction)------------------------
# Ref.: Steyerberg; TRIPOD; JOSPT 2026;13868.
# Because the models are chosen by a data-driven best-subset (BIC) search on a
# small sample, the apparent R^2 is optimistically biased. We estimate the
# optimism by REPEATING THE ENTIRE SELECTION (regsubsets + BIC) in each bootstrap
# sample (Harrell's optimism bootstrap). This is preferred over split-half here:
# split-half would waste ~half of n=152 and be unstable.
#   - r2_corrected      = apparent R^2 - optimism
#   - calibration_slope = shrinkage factor (1 = perfectly calibrated; <1 = the
#                         model is over-fit and predictions are too extreme)

# coef_to_vars() is defined once near the top of the script (factor-dummy mapping).
# drop the empty/near-empty occupation levels (as in the model blocks above)
drop_rare_occ <- function(d)
  droplevels(dplyr::filter(d, !occupation %in%
    c("housewife/househusband", "disabled/partially disabled", "retired")))

# run the full best-subset BIC selection and return the fitted lm
select_bic <- function(data, outcome, cand) {
  d  <- drop_rare_occ(data[, c(outcome, cand)])
  rf <- leaps::regsubsets(reformulate(cand, outcome), data = d,
                          nbest = 1, nvmax = length(cand))
  cf <- coef(rf, id = which.min(summary(rf)$bic))
  vars <- coef_to_vars(names(cf)[-1], cand)
  if (length(vars) == 0) vars <- "1"
  lm(reformulate(vars, outcome), data = d)
}

# robust predict: factor levels unseen during fitting -> reference level
safe_predict <- function(model, newdata) {
  xl <- model$xlevels
  for (v in names(xl)) if (v %in% names(newdata)) {
    x <- as.character(newdata[[v]]); x[!x %in% xl[[v]]] <- xl[[v]][1]
    newdata[[v]] <- factor(x, levels = xl[[v]])
  }
  predict(model, newdata = newdata)
}

r2_of <- function(model, newdata, outcome) {
  y <- newdata[[outcome]]; p <- safe_predict(model, newdata)
  1 - sum((y - p)^2) / sum((y - mean(y))^2)
}

validate_bic <- function(data, outcome, cand, B = 500, seed = 42) {
  set.seed(seed)
  dat <- drop_rare_occ(data)               # resample the analysis set actually used
  n   <- nrow(dat)
  m_app  <- select_bic(dat, outcome, cand)
  r2_app <- r2_of(m_app, dat, outcome)
  opt <- slopes <- rep(NA_real_, B)
  for (b in 1:B) {
    boot <- dat[sample(n, n, replace = TRUE), ]
    m_b  <- tryCatch(select_bic(boot, outcome, cand), error = function(e) NULL)
    if (is.null(m_b)) next
    opt[b]    <- r2_of(m_b, boot, outcome) - r2_of(m_b, dat, outcome)
    p_orig    <- safe_predict(m_b, dat)
    slopes[b] <- tryCatch(coef(lm(dat[[outcome]] ~ p_orig))[2],
                          error = function(e) NA_real_)
  }
  optimism <- mean(opt, na.rm = TRUE)
  data.frame(
    outcome           = outcome,
    final_model       = paste(deparse(formula(m_app)), collapse = ""),
    r2_apparent       = round(r2_app, 3),
    optimism          = round(optimism, 3),
    r2_corrected      = round(r2_app - optimism, 3),
    calibration_slope = round(mean(slopes, na.rm = TRUE), 3),
    B_ok              = sum(!is.na(opt)),
    stringsAsFactors  = FALSE
  )
}

# candidate predictor pools = same as the best-subset searches above (WITH baseline)
cand1 <- c("disability_t0", "age", "sex", "occupation", "wur_c2", "wur_trap",
           "cmsz_mean_faults", "cmsz_mean_time", "headeye_number_pos",
           "sum_rom", "jpe", "mc_s_plane", "recurrentpain_t0")
cand2 <- c("paindetect2_t0", "age", "sex", "occupation", "wur_c2", "wur_trap",
           "cmsz_mean_faults", "cmsz_mean_time", "headeye_number_pos",
           "sum_rom", "jpe", "mc_s_plane", "recurrentpain_t0")
cand3 <- c("paindetect3_t0", "age", "sex", "occupation", "wur_c2", "wur_trap",
           "cmsz_mean_faults", "cmsz_mean_time", "headeye_number_pos",
           "sum_rom", "jpe", "mc_s_plane", "recurrentpain_t0")

validation_results <- rbind(
  validate_bic(df_imp, "disability_t1",  cand1, B = 500),
  validate_bic(df_imp, "paindetect2_t1", cand2, B = 500),
  validate_bic(df_imp, "paindetect3_t1", cand3, B = 500)
)
print(validation_results, row.names = FALSE)
# Typical result (B=500, seed=42):
#        outcome                                                  final_model r2_apparent optimism r2_corrected calibration_slope
#  disability_t1            disability_t1 ~ disability_t0 + wur_trap + recurrentpain_t0       0.475    0.068        0.407             0.946
#  paindetect2_t1   paindetect2_t1 ~ paindetect2_t0 + cmsz_mean_faults + recurrentpain_t0     0.230    0.111        0.119             0.843
#  paindetect3_t1            paindetect3_t1 ~ paindetect3_t0 + wur_c2 + recurrentpain_t0       0.328    0.103        0.225             0.886
# -> disability_t1 validates well (small optimism, slope ~0.95). The two pain
#    outcomes carry substantial optimism (~half of the apparent R^2 for
#    paindetect2_t1) and slopes <0.9 -> report the corrected R^2 and consider
#    shrinkage of the coefficients.

# __Publication-ready HTML table (gt)-------------------------------------------
library(gt)
outcome_labels <- c(disability_t1  = "Disability (T1)",
                    paindetect2_t1 = "Strongest pain, NRS (T1)",
                    paindetect3_t1 = "Average pain, NRS (T1)")

gt_validation <- validation_results |>
  dplyr::mutate(outcome     = outcome_labels[outcome],
                final_model = gsub("~", "=", final_model, fixed = TRUE)) |>
  dplyr::select(outcome, final_model, r2_apparent, optimism,
                r2_corrected, calibration_slope) |>
  gt::gt() |>
  gt::tab_header(
    title    = gt::md("**Internal validation of the prediction models**"),
    subtitle = gt::md("Bootstrap optimism correction (*B* = 500 resamples)")) |>
  gt::cols_label(
    outcome           = gt::md("**Outcome**"),
    final_model       = gt::md("**Selected model**"),
    r2_apparent       = gt::md("*R*&sup2;<br>apparent"),
    optimism          = gt::md("Optimism"),
    r2_corrected      = gt::md("*R*&sup2;<br>corrected"),
    calibration_slope = gt::md("Calibration<br>slope")) |>
  gt::fmt_number(columns = c(r2_apparent, optimism, r2_corrected, calibration_slope),
                 decimals = 3) |>
  gt::cols_align("left",   columns = c(outcome, final_model)) |>
  gt::cols_align("center", columns = c(r2_apparent, optimism, r2_corrected, calibration_slope)) |>
  gt::tab_style(style     = gt::cell_text(weight = "bold"),
                locations = gt::cells_body(columns = r2_corrected)) |>
  gt::tab_spanner(label   = gt::md("**Model performance**"),
                  columns = c(r2_apparent, optimism, r2_corrected, calibration_slope)) |>
  gt::tab_footnote(
    footnote  = "The entire best-subset BIC selection was repeated in every bootstrap sample (Harrell's optimism bootstrap). R-squared corrected = R-squared apparent - optimism.",
    locations = gt::cells_column_spanners(spanners = gt::everything())) |>
  gt::tab_footnote(
    footnote  = "Calibration slope: 1 = perfectly calibrated; < 1 indicates over-fit predictions that are too extreme (shrinkage recommended).",
    locations = gt::cells_column_labels(columns = calibration_slope)) |>
  gt::tab_source_note(
    source_note = gt::md("Method per Steyerberg / TRIPOD; *cf.* JOSPT 2026;13868. *n* = 152, kNN-imputed.")) |>
  gt::tab_options(table.font.size            = gt::px(14),
                  heading.title.font.size    = gt::px(18),
                  column_labels.background.color = "#f2f2f2",
                  table.border.top.style     = "none")

gt_validation
gt::gtsave(gt_validation, "validation_results.html")   # publication-ready HTML
# gt::gtsave(gt_validation, "validation_results.png")  # needs webshot2 + chromote
# gt::gtsave(gt_validation, "validation_results.docx") # for Word manuscripts

