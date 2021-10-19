## R scripts for the article "Tweeting and Retweeting: Gender Discrepancies in Discursive Political Engagement and Influence on Twitter"

gender_diff_3var <- function(data) {
  rtf <- dplyr::filter(data, verified == "FALSE", !is.na(gender), prob_bot <= .5)
  rtf$gender <- factor(rtf$gender)
  
  ## tweets sent by women and men
  message("tweets sent by women and men")
  t <- gmodels::CrossTable(rtf$gender)
  
  ## unique women and men users
  rtfu <- rtf[!duplicated(rtf$user_id), ]
  message("unique women and men users")
  gmodels::CrossTable(rtfu$gender)
  
  ## gender and retweet
  message("gender and retweet")
  gmodels::CrossTable(rtf$is_retweet, rtf$gender)
  message("gender and retweet: chi-square test")
  chi1 <- chisq.test(xtabs(~is_retweet + gender, rtf))
  print(chi1)
  message("gender and retweet: effect size")
  es1 <- sqrt(as.numeric(chi1$statistic)/nrow(rtf))
  print(es1)
  
  ## gender and reply
  message("gender and reply")
  gmodels::CrossTable(rtf$is_reply, rtf$gender)
  message("gender and reply: chi-square test")
  chi3 <- chisq.test(xtabs(~is_reply + gender, rtf))
  print(chi3)
  message("gender and reply: effect size")
  es3 <- sqrt(as.numeric(chi3$statistic)/nrow(rtf))
  print(es3)
  
  ## gender and gender of retweet
  message("gender and gender of retweet")
  rtfrv <- filter(rtf, retweet_verified == "FALSE", retweet_gender != "NA")
  gmodels::CrossTable(rtfrv$retweet_gender, rtfrv$gender)
  message("gender and gender of retweet: chi-square test")
  chi4 <- chisq.test(xtabs(~retweet_gender + gender, rtfrv))
  print(chi4)
  message("gender and gender of retweet: effect size")
  es4 <- sqrt(as.numeric(chi4$statistic)/nrow(rtfrv))
  print(es4)
  
  ## gender and gender of reply
  message("gender and gender of reply")
  rtfrpv <- filter(rtf, reply_verified == "FALSE", reply_gender != "NA")
  gmodels::CrossTable(rtfrpv$reply_gender, rtfrpv$gender)
  message("gender and gender of reply: chi-square test")
  chi5 <- chisq.test(xtabs(~reply_gender + gender, rtfrpv))
  print(chi5)
  message("gender and gender of reply: effect size")
  es5 <- sqrt(as.numeric(chi5$statistic)/nrow(rtfrpv))
  print(es5)
  
  ## retweet counts of retweet between gender
  rtfrt <- filter(rtf, is_retweet == "TRUE")
  message("retweet counts of retweet between gender: mean rank")
  rtfrt$retweet_retweet_rank <- rank(rtfrt$retweet_retweet_count)
  rk1 <- rtfrt %>% group_by(gender) %>% summarise(rank = mean(retweet_retweet_rank))
  print(rk1)
  message("retweet counts of retweet between gender: Kruskal-Wallis test")
  ktest1 <- kruskal.test(retweet_retweet_count ~ gender, data = rtfrt)
  print(ktest1)
  message("retweet counts of retweet between gender: effect size") 
  # rstatix::kruskal_effsize()
  # https://rpkgs.datanovia.com/rstatix/reference/kruskal_effsize.html
  # 0.01- < 0.06 (small effect), 0.06 - < 0.14 (moderate effect) and >= 0.14 (large effect)
  # The need to report effect size estimates revisited. An overview of some recommended measures of effect size
  eta2H1 <- 
    (ktest1$statistic - length(levels(rtfrt$gender)) + 1)/(nrow(rtfrt) - length(levels(rtfrt$gender)))
  print(eta2H1)
  
  ## like counts of retweet between gender
  message("like counts of retweet between gender: mean rank")
  rtfrt$retweet_favorite_rank <- rank(rtfrt$retweet_favorite_count)
  rk2 <- rtfrt %>% group_by(gender) %>% summarise(rank = mean(retweet_favorite_rank))
  print(rk2)
  message("retweet counts of retweet between gender: Kruskal-Wallis test")
  ktest2 <- kruskal.test(retweet_favorite_count ~ gender, data = rtfrt)
  print(ktest2)
  message("retweet counts of retweet between gender: effect size")
  eta2H2 <- 
    (ktest2$statistic - length(levels(rtfrt$gender)) + 1)/(nrow(rtfrt) - length(levels(rtfrt$gender)))
  print(eta2H2)
}

gender_diff_3var(data = rt_gender)
