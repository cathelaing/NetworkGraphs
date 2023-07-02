# LMERs

global_pathlength_A_null <- lmerTest::lmer(path_length ~ corpus + numNodes + (1+age|Speaker),
                                           data=subset(SWD_red,
                                                       data_type == "actual")
                                           ,
                                           REML=FALSE)

pathlength_A <- anova(global_pathlength_A, global_pathlength_A_null) # not significant

pathlength_A_df <- pathlength_A$Df[2]
pathlength_A_chisq <- pathlength_A$Chisq[2]
pathlength_A_p.value <- pathlength_A$`Pr(>Chisq)`[2]

# Clustering coefficient

global_clust_model <- lmerTest::lmer(clust_coef_avg ~ age + corpus + numNodes + (1+age|Speaker),
                                     data=subset(SWD_red,
                                                 data_type == "actual"
                                     ),
                                     REML=FALSE)
#summary(global_clust_model)

global_clust_model_null <- lmerTest::lmer(clust_coef_avg ~ corpus + numNodes + (1+age|Speaker),
                                          data=subset(SWD_red,
                                                      data_type == "actual"
                                          ),
                                          REML=FALSE)

clustcoef <- anova(global_clust_model, global_clust_model_null)

clustcoef_df <- clustcoef$Df[2]
clustcoef_chisq <- clustcoef$Chisq[2]
clustcoef_p.value <- clustcoef$`Pr(>Chisq)`[2]


# GAMMs (don't show anything interesting so likely will not use)

```{r GAMM prep, echo=F, message=FALSE, warning=FALSE, comment=F}

SWD_actual <- subset(SWD_red, data_type == "actual")
SWD_target <- subset(SWD_red, data_type == "target")

SWD_actual$start.event <- SWD_actual$session_ordinal == 1 
SWD_target$start.event <- SWD_target$session_ordinal == 1 

SWD_red$start.event <- SWD_red$session_ordinal == 1 

globalthresholds$start.event <- globalthresholds$threshold == 0.01

# CREATE DUMMY VARIABLE FOR ACTUAL VS TARGET  

globalthresholds$IsActual <- (globalthresholds$data_type == "actual")*1

```

```{r GAMM thresholds corr, message=FALSE, warning=FALSE, include=FALSE}

gam.test <- bam(estimate ~ s(threshold) + s(threshold, by=IsActual), data=subset(globalthresholds, corpus == "English"))

summary(gam.test)

plot(gam.test, select=2, shade=TRUE)

globalthresholds$dataO <- as.ordered(globalthresholds$data_type) 
contrasts(globalthresholds$dataO) <- "contr.treatment"

gam.test2 <- bam(estimate ~ dataO + s(threshold) + s(threshold, by=dataO), data=subset(globalthresholds, corpus == "English"))

summary(gam.test2)

plot(gam.test2, select=2, shade=TRUE)
plot_diff(gam.test2, view="threshold", comp=list(dataO=c("target","actual")),
          # main = "Figure 3",
          # ylab = "Est. difference in scaled PAT values",
          # xlab = "Age (months)",
          # xlim = c(10,30),
          hide.label = TRUE)




corr.gamm.base <- bam(estimate ~ 
                         #corpus +
                        data_type +
                         s(threshold, bs = "cr") +  
                          #s(threshold, by=corpus, bs="cr") +
                          s(threshold, by=data_type, bs="cr"),
                       dat=subset(globalthresholds, corpus == "English"), method="ML")

rho <- start_value_rho(corr.gamm.base) 

corr.gamm.1 <- bam(estimate ~ 
                     #corpus +
                     data_type +
                     s(threshold, bs = "cr") +
                      #s(threshold, corpus, bs="fs", m=1, k=9) +
                      s(threshold, data_type, bs="fs", m=1, k=2),  
                   dat=subset(globalthresholds, corpus == "English"), method = "ML", 
                    rho=rho, AR.start=subset(globalthresholds, corpus == "English")$start.event)

corr.gamm.0 <- bam(estimate ~ 
                     #corpus +
                     data_type +
                     s(threshold, bs = "cr") +
                     #s(threshold, corpus, bs="fs", m=1, k=9) +
                     s(threshold, data_type, bs="fs", m=1, k=2),  
                   dat=subset(globalthresholds, corpus == "English"), method = "ML", 
                   rho=rho, AR.start=subset(globalthresholds, corpus == "English")$start.event)

corr.compare <- compareML(corr.gamm.1, corr.gamm.0)
compare1 <- corr.compare$table

corr.compare_summ <- summary(compare1)

plot_diff(corr.gamm.1, view="threshold", comp=list(data_type=c("actual","target")),
          # main = "Figure 3",
          # ylab = "Est. difference in scaled PAT values",
          # xlab = "Age (months)",
          # xlim = c(10,30),
          hide.label = TRUE)


dist.gamm.base <- bam(dist_mean ~ 
                         corpus +
                           s(session_ordinal, bs = "cr") +
                           s(age, bs = "cr")  +                     
                          s(session_ordinal, by=Speaker, bs="cr"),
                        dat=comparison_summary, method="ML")

rdist <- start_value_rho(dist.gamm.base) 

dist.gamm.1 <- bam(dist_mean ~ 
                      corpus +
                      s(session_ordinal, bs = "cr") +
                      s(age, bs = "cr")  +   
                      s(session_ordinal, Speaker, bs="fs", m=1, k=9),  
                    dat=comparison_summary, method = "ML", 
                     rho=rdist, AR.start=comparison_summary$session_ordinal == 1)

dist.gamm.0 <- bam(dist_mean ~ 
                     corpus +
                     s(session_ordinal, bs = "cr"),
                     #s(age, bs = "cr")  +   
                     #s(age, Speaker, bs="fs", m=1, k=9),  
                   dat=comparison_summary, method = "ML", 
                   rho=rdist, AR.start=comparison_summary$session_ordinal == 1)

dist.diff <- compareML(dist.gamm.1, dist.gamm.0)
dist.diff.prep <- dist.diff$table

dist.diff_summ <- summary(dist.diff.prep)

ggplot(comparison_summary, aes(x = age, y = dist_mean)) +
  geom_smooth() +
  theme_bw()

plot_smooth(dist.gamm.1, view="age", rug=FALSE)


```{r GAMM age target MPL, message=FALSE, warning=FALSE, include=FALSE}

MPL.gamm.base_T <- bam(path_length ~ 
                         corpus +
                         s(age, bs = "cr") +                       
                         s(age, by=Speaker, bs="cr") +
                         s(age, by=corpus, bs="cr"),
                       dat=SWD_target, method="ML")

rT <- start_value_rho(MPL.gamm.base_T) 

MPL.gamm.1_T <- bam(path_length ~ 
                      corpus +
                      s(age, bs = "cr") + 
                      s(age, corpus, bs="fs", m=1, k=2) +
                      s(age, Speaker, bs="fs", m=1, k=9),  
                    data = SWD_target, method = "ML", 
                    rho=rT, AR.start=SWD_target$start.event)

MPL.gamm.0_T <- bam(path_length ~ 
                      corpus +
                      #s(age, bs = "cr") + 
                      s(age, corpus, bs="fs", m=1, k=2) +
                      s(age, Speaker, bs="fs", m=1, k=9),  
                    data = SWD_target, method = "ML", 
                    rho=rT, AR.start=SWD_target$start.event)

MPL.gamm.target <- compareML(MPL.gamm.1_T, MPL.gamm.0_T)
MPL.gamm.T <- MPL.gamm.target$table

MPL.gamm.T_summ <- summary(MPL.gamm.T)

```

```{r GAMM age actual CC, message=FALSE, warning=FALSE, include=FALSE}

CC.gamm.base_A <- bam(clust_coef_avg ~ 
                        corpus +
                        s(age, bs = "cr") +                       
                        s(age, by=Speaker, bs="cr") +
                        s(age, by=corpus, bs="cr"),
                      dat=SWD_actual, method="ML")

rA <- start_value_rho(CC.gamm.base_A) 

CC.gamm.1_A <- bam(clust_coef_avg ~ 
                     corpus +
                     s(age, bs = "cr") + 
                     s(age, corpus, bs="fs", m=1, k=2) +
                     s(age, Speaker, bs="fs", m=1, k=9),  
                   data = SWD_actual, method = "ML", 
                   rho=rA, AR.start=SWD_actual$start.event)

CC.gamm.0_A <- bam(clust_coef_avg ~ 
                     corpus +
                     #s(age, bs = "cr") + 
                     s(age, corpus, bs="fs", m=1, k=2) +
                     s(age, Speaker, bs="fs", m=1, k=9),  
                   data = SWD_actual, method = "ML", 
                   rho=rA, AR.start=SWD_actual$start.event)

CC.gamm.actual <- compareML(CC.gamm.1_A, CC.gamm.0_A)
CC.gamm.A <- CC.gamm.actual$table

CC.gamm.A_summ <- summary(CC.gamm.A)

```

```{r GAMM data type actual MPL, message=FALSE, warning=FALSE, include=FALSE}

MPL.gamm.base_DT <- bam(path_length ~ 
                          data_type +
                          corpus +
                          s(age, bs = "cr") +                       
                          s(age, by=Speaker, bs="cr") +
                          s(age, by=data_type, bs="cr") +
                          s(age, by=corpus, bs="cr"),
                        dat=SWD_red, method="ML")

rDT <- start_value_rho(MPL.gamm.base_DT) 

MPL.gamm.1_DT <- bam(path_length ~ 
                       data_type +
                       corpus +
                       s(age, bs = "cr") + 
                       s(age, corpus, bs="fs", m=1, k=2) +
                       s(age, data_type, bs="fs", m=1, k=2) +
                       s(age, Speaker, bs="fs", m=1, k=9),  
                     data = SWD_red, method = "ML", 
                     rho=rDT, AR.start=SWD_red$start.event)

MPL.gamm.0_DT <- bam(path_length ~ 
                       #data_type +
                       corpus +
                       s(age, bs = "cr") + 
                       s(age, corpus, bs="fs", m=1, k=2) +
                       #s(age, data_type, bs="fs", m=1, k=2) +
                       s(age, Speaker, bs="fs", m=1, k=9),  
                     data = SWD_red, method = "ML", 
                     rho=rDT, AR.start=SWD_red$start.event)

MPL.gamm.DT <- compareML(MPL.gamm.1_DT, MPL.gamm.0_DT)
MPL.gamm.DT.prep <- MPL.gamm.DT$table

MPL.gamm.DT_summ <- summary(MPL.gamm.DT.prep)


plot_diff(MPL.gamm.1_DT, view="age", comp=list(data_type=c("target","actual")),
          # main = "Figure 3",
          # ylab = "Est. difference in scaled PAT values",
          xlab = "Age (months)",
          xlim = c(10,30),
          hide.label = TRUE)
```



ggplot(subset(SWD_red, data_type == "actual"),
       mapping = aes(x = age, y = path_length)) +
  geom_smooth() +
  geom_point(shape = 1) +
  theme_bw()

ggplot(subset(SWD_red, data_type == "target"),
       mapping = aes(x = age, y = path_length)) +
  geom_smooth() +  
  geom_point(shape = 1) +
  theme_bw()

ggplot(subset(SWD_red, data_type %in% c("target", "actual")),
       mapping = aes(x = age, y = path_length, colour = data_type)) +
  geom_smooth() +  
  geom_point(shape = 1) +
  theme_bw()

ggplot(subset(SWD_red, data_type %in% c("target", "actual")),
       mapping = aes(x = age, y = clust_coef_avg, colour = data_type)) +
  geom_smooth() +  
  geom_point(shape = 1) +
  theme_bw()