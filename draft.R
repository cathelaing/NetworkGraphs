# GAMMs (don't show anything interesting so likely will not use)

```{r GAMM prep, echo=F, message=FALSE, warning=FALSE, comment=F}

SWD_actual <- subset(SWD_red, data_type == "actual")
SWD_target <- subset(SWD_red, data_type == "target")

SWD_actual$start.event <- SWD_actual$session_ordinal == 1 
SWD_target$start.event <- SWD_target$session_ordinal == 1 

SWD_red$start.event <- SWD_red$session_ordinal == 1 


```

```{r GAMM age actual MPL, message=FALSE, warning=FALSE, include=FALSE}

MPL.gamm.base_A <- bam(path_length ~ 
                         corpus +
                         s(age, bs = "cr") +                       
                         s(age, by=Speaker, bs="cr") +
                         s(age, by=corpus, bs="cr"),
                       dat=SWD_actual, method="ML")

rA <- start_value_rho(MPL.gamm.base_A) 

MPL.gamm.1_A <- bam(path_length ~ 
                      corpus +
                      s(age, bs = "cr") + 
                      s(age, corpus, bs="fs", m=1, k=2) +
                      s(age, Speaker, bs="fs", m=1, k=9),  
                    data = SWD_actual, method = "ML", 
                    rho=rA, AR.start=SWD_actual$start.event)

MPL.gamm.0_A <- bam(path_length ~ 
                      corpus +
                      #s(age, bs = "cr") + 
                      s(age, corpus, bs="fs", m=1, k=2) +
                      s(age, Speaker, bs="fs", m=1, k=9),  
                    data = SWD_actual, method = "ML", 
                    rho=rA, AR.start=SWD_actual$start.event)

MPL.gamm.actual <- compareML(MPL.gamm.1_A, MPL.gamm.0_A)
MPL.gamm.A <- MPL.gamm.actual$table

MPL.gamm.A_summ <- summary(MPL.gamm.A)

```

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