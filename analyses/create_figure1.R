# -------------------------------------------------
# -------- Load Packages --------------------------
# -------------------------------------------------
library(scales)
library(RColorBrewer)


# -------------------------------------------------
# -------- Load Data ------------------------------
# -------------------------------------------------
dat_final <- readRDS('../data/DataS3_Final.RDS')

# -------------------------------------------------
# -------- Preprocess -----------------------------
# -------------------------------------------------

# We make a list, where each item has one entry with:
# - Item name
# - response scale
# - response data
# Then I later subset from this overall list to get the items I want

# --- Storage ---
l_items <- list()

# --- Aux Function ---

tableN <- function(x) {
  tb <- table(x)
  as.numeric(tb/sum(tb))
}


# ----- The three response formats -----

Response_NotatAll_GreatDeal <- c("Not at all",
                                 "Very little",
                                 "A moderate amount",
                                 "Quite a bit",
                                 "A great deal")

Response_StronglyADA <- c("Strongly Disagree",
                          "Disagree",
                          "Neither agree / disagree",
                          "Agree",
                          "Strongly agree")

Response_Willing <- c("Not willing to",
                      "Willing to",
                      "Already do")


# ----- A. Beliefs about Climate Change & Solutions -----

# Worry
l_items$Worry <- list("Question" = "Overall, how worried are you about climate change?",
                      "Response" = Response_NotatAll_GreatDeal,
                      "Data"= tableN(dat_final[, "Worry_std"]))

# Fundamental changes
l_items$ToC1_4 <- list("Question" = "Fundamental changes to society, politics, and economics required",
                       "Response" = Response_StronglyADA,
                       "Data" = tableN(dat_final[, "ToC1_4_std"]))

# Lifestyle changes
l_items$ToC1_5 <- list("Question" = "Significant changes to personal behavior and lifestyle required",
                       "Response" = Response_StronglyADA,
                       "Data" = tableN(dat_final[, "ToC1_5_std"]))

# Efficacy of activism
l_items$ToC1_9 <- list("Question" = "Environmental activist groups can drive positive change",
                       "Response" = Response_StronglyADA,
                       "Data" = tableN(dat_final[, "ToC1_9_std"]))

# Technology optimism
l_items$ToC1_2 <- list("Question" = "Advances in technology will largely solve climate change",
                       "Response" = Response_StronglyADA,
                       "Data" = tableN(dat_final[, "ToC1_2_std"]))


# ----- B. Responsibility & Role of Scientists and Academics -----

# Responsibility: Institutions
l_items$ResponsibilityInst <- list("Question" = "Scientific or academic institutions are responsible",
                                   "Response" = Response_NotatAll_GreatDeal,
                                   "Data" = tableN(dat_final[, "ResponsibilityInst_std"]))

# Responsibility: scientist
l_items$ResponsibilitySci <- list("Question" = "Feel responsible as a scientist or academic",
                                  "Response" = Response_NotatAll_GreatDeal,
                                  "Data" = tableN(dat_final[, "ResponsibilitySci_std"]))

# Responsibility: Should Advocate
l_items$RoleSci_4 <- list("Question" = "Scientists and academics should engage more in advocacy",
                          "Response" = Response_StronglyADA,
                          "Data" = tableN(dat_final[, "RoleSci_4_std"]))

# Responsibility: Should Protest
l_items$RoleSci_2 <- list("Question" = "Scientists and academics should engage more in legal protest",
                          "Response" = Response_StronglyADA,
                          "Data" = tableN(dat_final[, "RoleSci_2_std"]))

# Responsibility: Diminish Credibility
l_items$Credibility_2 <- list("Question" = "Engaging in advocacy would diminish scientists' credibility",
                              "Response" = Response_StronglyADA,
                              "Data" = tableN(dat_final[, "Credibility_2_std"]))


# ----- C. Lifestyle Behaviors -----

# Reducing car usage
l_items$Beh_incNotApp_1 <- list("Question" = "Reducing car usage",
                                "Response" = Response_Willing,
                                "Data" = tableN(dat_final[, "Beh_incNotApp_1"]))

# Reducing car usage
l_items$Beh_incNotApp_8 <- list("Question" = "Reducing the amount of flying",
                                "Response" = Response_Willing,
                                "Data" = tableN(dat_final[, "Beh_incNotApp_8"]))

# Home energy efficiency
l_items$Beh_incNotApp_3 <- list("Question" = "Increasing energy efficiency at home",
                                "Response" = Response_Willing,
                                "Data" = tableN(dat_final[, "Beh_incNotApp_3"]))

# Vegan/Vegetarian Diet
l_items$Beh_incNotApp_7 <- list("Question" = "Following a mostly vegetarian or vegan diet",
                                "Response" = Response_Willing,
                                "Data" = tableN(dat_final[, "Beh_incNotApp_7"]))

# Having fewer children
l_items$Beh_incNotApp_4 <- list("Question" = "Having fewer or no children",
                                "Response" = Response_Willing,
                                "Data" = tableN(dat_final[, "Beh_incNotApp_4"]))


# ----- D. Advocacy Behaviors -----

# Talking with others
l_items$Beh_incNotApp_5 <- list("Question" = "Talking about climate change with others",
                                "Response" = Response_Willing,
                                "Data" = tableN(dat_final[, "Beh_incNotApp_5"]))

# Engage in Advocacy
l_items$Beh_EngPub <- list("Question" = "Engaging in climate change advocacy",
                           "Response" = Response_Willing,
                           "Data" = tableN(dat_final[, "Beh_EngPub"]))

# Donating money
l_items$Beh_incNotApp_6 <- list("Question" = "Donating to a relevant organization",
                                "Response" = Response_Willing,
                                "Data" = tableN(dat_final[, "Beh_incNotApp_6"]))

# Legal protest
l_items$BehLegal <- list("Question" = "Participating in legal climate change-related protests",
                         "Response" = Response_Willing,
                         "Data" = tableN(dat_final[, "BehLegal"]))

# Civil disobedience
l_items$Beh_others_7 <- list("Question" = "Participating in nonviolent civil disobedience actions",
                             "Response" = Response_Willing,
                             "Data" = tableN(dat_final[, "Beh_others_7"]))


# -------------------------------------------------
# -------- Global Plotting Options ----------------
# -------------------------------------------------

mar=c(0,1,1.25,1)

y_seq <- seq(.9, .1, length=5)
y_h <- 0.045
c_vert_text <- .076
cex_panel_tit <- 1.1
cex_ques <- 1.1
line_panel_tit <- -.1

# Scale
cex_scale <- 0.9
ymin_scale <- 0.1
ymax_scale <- .91

# --- Color schemes ---
# cols_div3 <- alpha(brewer.pal(3, "Set1"), .8)
cols_div3 <- c('#7570B3', '#1B9E77', '#D95F02')[c(3,1,2)]
cols_div5 <- alpha(RColorBrewer::brewer.pal(5, "RdBu"), .8)
# display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE,
#                    colorblindFriendly=FALSE)
cols_div5_leg <- cols_div5
cols_div5_leg[3] <- "lightgrey"

# -------------------------------------------------
# -------- Plotting -------------------------------
# -------------------------------------------------


pdf("../figures/figure1.pdf", width=10, height=8)

par(mfrow=c(2,2))

# ----- A. Beliefs about climate change & solutions -----

# Canvas
par(mar=mar)
plot.new()
plot.window(xlim=c(0,1), ylim=c(0,1))
mtext("   A. Beliefs about climate change & solutions",
      side=3, adj=0, line=line_panel_tit, cex=cex_panel_tit, family = "Times")
# Data
for(i in 1:5) {
  x_seq_i <- c(0, cumsum(l_items[[i]]$Data))
  text(0, y_seq[i]+c_vert_text, labels=l_items[[i]]$Question, cex=cex_ques, adj=0, family = "Times")
  for(j in 1:5) rect(x_seq_i[j], y_seq[i]-y_h, x_seq_i[j+1], y_seq[i]+y_h,  col=cols_div5[j])
} # end for

# Plot scale 1
seq_x_scale5 <- seq(ymin_scale, ymax_scale, length=5)
for(j in 1:5) text(seq_x_scale5[j], 0.825, l_items[[1]]$Response[j],
                   cex=cex_scale, col=cols_div5_leg[j], adj=0.5, family = "Times")

# Plot scale 2
seq_x_scale5 <- seq(ymin_scale, ymax_scale, length=5)
for(j in 1:5) text(seq_x_scale5[j], 0.02, l_items[[2]]$Response[j],
                   cex=cex_scale, col=cols_div5_leg[j], adj=0.5, family = "Times")


# ----- B. Responsibility & Role of Scientists and Academics -----

# Canvas
par(mar=mar)
plot.new()
plot.window(xlim=c(0,1), ylim=c(0,1))
mtext("   B. Responsibility & role of scientists and academics",
      side=3, adj=0, line=line_panel_tit, cex=cex_panel_tit, family = "Times")
# Data
for(i in 1:5) {
  x_seq_i <- c(0, cumsum(l_items[[i+5]]$Data))
  text(0, y_seq[i]+c_vert_text, labels=l_items[[i+5]]$Question, cex=cex_ques, adj=0, family = "Times")
  for(j in 1:5) rect(x_seq_i[j], y_seq[i]-y_h, x_seq_i[j+1], y_seq[i]+y_h,  col=cols_div5[j])
} # end for

# Plot scale 1
seq_x_scale5 <- seq(ymin_scale, ymax_scale, length=5)
for(j in 1:5) text(seq_x_scale5[j], 0.625, l_items[[6]]$Response[j],
                   cex=cex_scale, col=cols_div5_leg[j], adj=0.5, family = "Times")

# Plot scale 2
seq_x_scale5 <- seq(ymin_scale, ymax_scale, length=5)
for(j in 1:5) text(seq_x_scale5[j], 0.015, l_items[[10]]$Response[j],
                   cex=cex_scale, col=cols_div5_leg[j], adj=0.5, family = "Times")


# ----- C. Lifestyle Behaviors -----

# Canvas
par(mar=mar)
plot.new()
plot.window(xlim=c(0,1), ylim=c(0,1))
mtext("   C. Lifestyle behaviors",
      side=3, adj=0, line=line_panel_tit, cex=cex_panel_tit, family = "Times")
# Data
for(i in 1:5) {
  x_seq_i <- c(0, cumsum(l_items[[i+10]]$Data))
  text(0, y_seq[i]+c_vert_text, labels=l_items[[i+10]]$Question, cex=cex_ques, adj=0, family = "Times")
  for(j in 1:3) rect(x_seq_i[j], y_seq[i]-y_h, x_seq_i[j+1], y_seq[i]+y_h,  col=cols_div3[j])
} # end for

# Plot scale 1
seq_x_scale5 <- seq(ymin_scale, ymax_scale, length=3)
for(j in 1:3) text(seq_x_scale5[j], 0.015, l_items[[11]]$Response[j],
                   cex=cex_scale, col=cols_div3[j], adj=0.5, family = "Times")


# ----- D. Advocacy & Activism Behaviors -----

# Canvas
par(mar=mar)
plot.new()
plot.window(xlim=c(0,1), ylim=c(0,1))
mtext("   D. Advocacy & activism behaviors",
      side=3, adj=0, line=line_panel_tit, cex=cex_panel_tit, family = "Times")
# Data
for(i in 1:5) {
  x_seq_i <- c(0, cumsum(l_items[[i+15]]$Data))
  text(0, y_seq[i]+c_vert_text, labels=l_items[[i+15]]$Question, cex=cex_ques, adj=0, family = "Times")
  for(j in 1:3) rect(x_seq_i[j], y_seq[i]-y_h, x_seq_i[j+1], y_seq[i]+y_h,  col=cols_div3[j])
} # end for

# Plot scale 1
seq_x_scale5 <- seq(ymin_scale, ymax_scale, length=3)
for(j in 1:3) text(seq_x_scale5[j], 0.015, l_items[[11]]$Response[j],
                   cex=cex_scale, col=cols_div3[j], adj=0.5, family = "Times")

dev.off()

