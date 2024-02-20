max_interim_analyses <- 5
max_parameters <- 2
text_delimiter <- ","

info_level_range <- c(0.1, 0.9)
lowest_info_increment <- 0.1

min_events_per_arm <- 3
accrual_increment <- 1

power_range <- c(0.4, 1)
power_include_min <- TRUE
power_include_max <- FALSE

alpha_range <- c(1e-5, 0.5)
alpha_include_min <- TRUE
alpha_include_max <- TRUE

# Kim-DeMets: alpha- and beta-spending parameter range identical
kd_range <- c(0.4, 8)
# Hwang-Shi-DeCani: alpha- and beta-spending parameter range identical
hsd_range <- c(-10, 5)

max_n_threshold <- 10000
