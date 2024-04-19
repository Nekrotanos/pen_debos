# Author: nekrotanos
# Here is a method to exactly calculate the average cost of cronning or hammering accessories.This function could
# easily be modified for uncronned accessories as well. You'd just need to adjust it so the only 2 numbers in the matrix
# were the probability of moving to tne next enhance state (success on enhancment) or the probability of failing and
# resetting to base. That is outside the scope of what I am doing here though, so I won't add a toggle in the function
# for it.
require(pacman)
p_load(tidyverse, ggplot2, cowplot, ggpubr, scales)

#' Calculate the cost of making a deboreka.
#' @param prob_vec A vector of the probabilities to succeed from pri to pen
#' @param cron_cost The cron_costs vector from above
#' @param enh_cost Cost of the enhancement item (in this case the deborekas)
#' @param base_level Numerical representation of the level that you are starting from. Base is 0, Pri is 1, Duo is 2,
#' Tri is 3, Tet is 4
#' @param target_level Numerical representation of the level that you are trying to get. Pri is 1, Duo is 2, Tri is 3,
#' Tet is 4, Pen is 5
#' @param tax Amount of tax to deduct. Defaults to the pretty standard amount of 0.845. This assumes you are grinding
#' the debos yourself. If you are somehow buying them (*cough* carried *cough*) then set it to 1.
#' @param list_out Whether or not to return a list of the cost, debos used, and crons. Each item will be a column vector
#' where each row shows the values of starting from different states. The first row represents starting from base, the
#' second from pri, and so on.
#' @param print_out Whether or not to return a message with some info about the average number of attempts.
calc <- function(prob_vec, cron_cost, base_cost, enh_cost, cron_silv, base_level, target_level, tax=0.845, list_out = FALSE,
                 print_out = TRUE){
  pr <- prob_vec[1]
  du <- prob_vec[2]
  tr <- prob_vec[3]
  te <- prob_vec[4]
  pe <- prob_vec[5]
  prob_mat <- matrix(
    c(
      1-pr, pr, 0, 0, 0, 0,
      0.4*(1-du), 0.6*(1-du), du, 0, 0, 0,
      0, 0.4*(1-tr), 0.6*(1-tr), tr, 0, 0,
      0, 0, 0.4*(1-te), 0.6*(1-te), te, 0,
      0, 0, 0, 0.4*(1-pe), 0.6*(1-pe), pe,
      0, 0, 0, 0, 0, 1
    ),
    ncol = 6,
    byrow=T
  )
  imp <- diag(target_level) - prob_mat[1:target_level, 1:target_level]
  inv <- solve(imp)
  bases <- inv %*% matrix(rep(1,target_level), ncol=1) + 1
  crons <- inv %*% matrix(cron_cost[1:target_level],ncol=1)
  cost <- crons * cron_silv + bases * enh_cost * tax + (base_cost - enh_cost * tax)
  if (print_out) {
    cat("Average bases to reach target level: ", round(bases[(base_level+1),1], 3), "\n",
        "Average crons to reach target level: ", round(crons[(base_level+1),1], 3), "\n",
        "Average cost to reach target level: ", round(cost[(base_level+1),1], 3), "\n", sep="")
  }
  if (list_out) {
    output <- list(bases = bases, crons = crons, cost = cost)
    return(output)
  }
}

# Base to pen
base_cost <- 825000000
cron_costs <- c(95, 288, 865, 2405, 11548)
enh_cost <- 825000000
cron_silv <- 1630000000 / 993
base_level <- 0
target_level <- 5
tax <- 0.65*1.315

# Softcap probabilities (except for pen)
sc_pri <- 0.7
sc_duo <- 0.5
sc_tri <- 0.4
sc_tet <- 0.3
sc_pen <- 0.145

debo_enhance_sc <- c(sc_pri, sc_duo, sc_tri, sc_tet, sc_pen)

# Calculations for example probabilities
cat("================================================================\n")
cat("Average from base to tet with softcap stacks\n")
cat("================================================================\n")
calc(
  prob_vec=debo_enhance_sc,
  cron_cost=cron_costs,
  base_cost = 825000000,
  enh_cost = 825000000,
  cron_silv = cron_silv,
  base_level = 0,
  target_level = 4,
  tax = 0.845
)

# Biohack's stack probabilities
bio_pri <- 0.9
bio_duo <- 0.58
bio_tri <- 0.5040
bio_tet <- 0.31
bio_pen <- 0.1425
debo_enhance_bio <- c(bio_pri, bio_duo, bio_tri, bio_tet, bio_pen)

cat("\n\n================================================================\n")
cat("Average from base to tet with biohack's stacks\n")
cat("================================================================\n")
calc(
  prob_vec=debo_enhance_bio,
  cron_cost=cron_costs,
  base_cost = 825000000,
  enh_cost = 825000000,
  cron_silv = cron_silv,
  base_level = 0,
  target_level = 4,
  tax = 0.845
)

stack_seq <- 250:300

# Enhance probabilities are base prob + 1/10 * base prob * stack number
pen_seq <- 0.005 + 0.0005 * stack_seq

# Graph for making a pen deboreka from base using softcap stacks

# Calculate costs for each stack
sc_enhance_costs <- numeric(length(pen_seq))
sc_enhance_crons <- numeric(length(pen_seq))
sc_enhance_bases <- numeric(length(pen_seq))
for (i in seq_along(pen_seq)) {
  temp_probs <- c(sc_pri, sc_duo, sc_tri, sc_tet, pen_seq[i])
  temp_calc <- calc(
    prob_vec = temp_probs,
    cron_cost = cron_costs,
    base_cost = base_cost,
    enh_cost = enh_cost,
    cron_silv = cron_silv,
    base_level = base_level,
    target_level = target_level,
    tax = tax,
    print_out = FALSE,
    list_out = TRUE
  )
  sc_enhance_costs[i] <- temp_calc$cost[1,1]
  sc_enhance_crons[i] <- temp_calc$crons[1,1]
  sc_enhance_bases[i] <- temp_calc$bases[1,1]
}

# Calculate for hammer, pen chance doesn't matter here so the base-tet probs just have to be right.
sc_hammer_calc <- calc(
    prob_vec = temp_probs,
    cron_cost = cron_costs,
    base_cost = base_cost,
    enh_cost = enh_cost,
    cron_silv = cron_silv,
    base_level = base_level,
    target_level = 4,
    tax = tax,
    print_out = FALSE,
    list_out = TRUE
)

# Calculate as the cost of making the tet + (the average number of attempts to pen * the cost of each attempt).
sc_hammer_costs <- sc_hammer_calc$cost[1,1] + ( 1 / pen_seq) * (27500000000 + enh_cost * tax)
sc_hammer_bases <- sc_hammer_calc$bases[1,1] + ( 1 / pen_seq)

# I'll also calculate the average cost when buying a tet
tet_cost <- 76500000000
bt_hammer_costs <- tet_cost + ( 1 / pen_seq) * (27500000000 + enh_cost * tax)

# I'm lazy and using ggplot, so combine everything into a dataframe.
sc_enhance_data <- tibble(
  Cost = c(sc_enhance_costs, sc_hammer_costs, bt_hammer_costs),
  Stack = c(stack_seq, stack_seq, stack_seq),
  Method = c(rep("Premium Cron", length(pen_seq)), rep("Hammer", length(pen_seq)), rep("Bought Tet", length(pen_seq)))
)

sc_pri_to_pen <- sc_enhance_data %>%
  ggplot(aes(x = Stack, y = Cost, color = Method)) +
  geom_line() +
  scale_y_continuous(limits = c(1.95e11, 3.05e11), labels = unit_format(unit = "B", scale = 1e-9)) +
  ggtitle("Pen debo from base using softcap stacks")

# Do it all over again for biohack's stacks
# Calculate costs for each stack
bio_enhance_costs <- numeric(length(pen_seq))
bio_enhance_crons <- numeric(length(pen_seq))
bio_enhance_bases <- numeric(length(pen_seq))
for (i in seq_along(pen_seq)) {
  temp_probs <- c(bio_pri, bio_duo, bio_tri, bio_tet, pen_seq[i])
  temp_calc <- calc(
    prob_vec = temp_probs,
    cron_cost = cron_costs,
    base_cost = base_cost,
    enh_cost = enh_cost,
    cron_silv = cron_silv,
    base_level = base_level,
    target_level = target_level,
    tax = tax,
    print_out = FALSE,
    list_out = TRUE
  )
  bio_enhance_costs[i] <- temp_calc$cost[1,1]
  bio_enhance_crons[i] <- temp_calc$crons[1, 1]
  bio_enhance_bases[i] <- temp_calc$bases[1, 1]
}

# Calculate for hammer, pen chance doesn't matter here so the base-tet probs just have to be right.
bio_hammer_calc <- calc(
    prob_vec = temp_probs,
    cron_cost = cron_costs,
    base_cost = base_cost,
    enh_cost = enh_cost,
    cron_silv = cron_silv,
    base_level = base_level,
    target_level = 4,
    tax = tax,
    print_out = FALSE,
    list_out = TRUE
)

# Calculate as the cost of making the tet + (the average number of attempts to pen * the cost of each attempt).
bio_hammer_costs <- bio_hammer_calc$cost[1,1] + ( 1 / pen_seq) * (27500000000 + enh_cost * tax)
bio_hammer_bases <- bio_hammer_calc$bases[1,1] + (1 / pen_seq)

# I'm lazy and using ggplot, so combine everything into a dataframe.
bio_enhance_data <- tibble(
  Cost = c(bio_enhance_costs, bio_hammer_costs, bt_hammer_costs),
  Stack = c(stack_seq, stack_seq, stack_seq),
  Method = c(rep("Premium Cron", length(pen_seq)), rep("Hammer", length(pen_seq)), rep("Bought Tet", length(pen_seq)))
)

# Make the plot
bio_pri_to_pen <- bio_enhance_data %>%
  ggplot(aes(x = Stack, y = Cost, color = Method)) +
  geom_line() +
  scale_y_continuous(limits = c(1.95e11, 3.05e11), labels = unit_format(unit = "B", scale = 1e-9)) +
  ggtitle("Pen Debo from base using biohack's stacks")

# Combine the plots into 1 figure
comb_pri_to_pen <- ggarrange(
  sc_pri_to_pen, bio_pri_to_pen,
  ncol = 2,
  legend = "bottom",
  common.legend = TRUE
)

ggsave("base_to_pen_debo_neck.png", comb_pri_to_pen, width = 20, height = 15)
