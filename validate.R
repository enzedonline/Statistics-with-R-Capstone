df_validation <- ames_validation %>%
    mutate_at(
        vars(Alley, BsmtFin.Type.1, BsmtFin.Type.2, Bsmt.Qual, Bsmt.Cond, Bsmt.Exposure,
             Fireplace.Qu, Garage.Type, Garage.Finish, Mas.Vnr.Type,
             Garage.Qual, Garage.Cond, Pool.QC, Fence, Misc.Feature), 
        replace_factor_na
    ) %>%
    mutate(log.age = log(2011 - Year.Built)) %>%
    mutate(log.remod.age = log(2011 - Year.Remod.Add))
# drop utilities & sale.condition, only one value in whole db
df_validation <- df_validation %>% select(-Utilities, -Sale.Condition)

# drop month/year sold - doesn't help predict a sale value unless using it to adjust sale price
df_validation <- df_validation %>% select(-Yr.Sold, -Mo.Sold)

#deal with continuous na's 

# 48 observations miss garage age  due to not having a garage
# for purposes of EDA, assume those values are the same as house age
# revisit if garage age is significant influencer
df_validation <- df_validation %>%
    mutate(Garage.Yr.Blt = ifelse(is.na(Garage.Yr.Blt), Year.Built, Garage.Yr.Blt))

# 1 in 7 are missing Lot.Frontage - EDA, found that
# RLot.Frontage looks to be significant influencer
# => strong liner relationship between area and frontage found (after removing outliers)
# => replace NAs with calculated value from area
# <30,000 represents roughly 98% of values

df_validation <- df_validation %>%
    mutate(Lot.Frontage = ifelse(is.na(Lot.Frontage), calc_frontage(Lot.Area), Lot.Frontage))

# Mas.Vnr.Area relates to Masonry veneer. Those that have none will have 0.
df_validation <- df_validation  %>%
    replace_na(list(Mas.Vnr.Area=0))

df_validation <- df_validation  %>%
    replace(is.na(.), 0)

# Perform a basic ANOVA to find influencers to use on outlier validation
ames.lm <- lm(log(price) ~ ., data = df_validation)
ames_anova <- anova(ames.lm)
potential_vars <- (
    ames_anova %>%
        rownames_to_column(var = "Variable") %>%
        slice_max(`Pr(>F)`, n=10) 
)$Variable

n <- nrow(df_validation)
ames_k <- qnorm(0.5 + 0.5 * 0.95 ^ (1 / n))
ames.lm <- lm(log(price) ~ ., data = (df_validation %>% select(price, all_of(potential_vars))))
outliers <- BAS::Bayes.outlier(ames.lm, k = ames_k)
head(sort(outliers$prob.outlier, decreasing=TRUE), 5)

# 3 observations can be counted as outlier
df_validation$outlier = outliers$prob.outlier

#remove outliers from training dataset
df_validation <- df_validation %>%
    filter(!(PID %in% (df_train %>% filter(outlier>0.975))$PID))

# remove unknown factor levels
df_final_validation <- df_validation  %>%
    filter(Foundation != "Wood") %>%
    filter(MS.Zoning != "A (agr)") %>%
    filter(House.Style != "2.5Fin") %>%
    filter(Heating != "Floor") %>%
    filter(!(Exterior.1st %in% c("CBlock", "PreCast")))

predict.validation.final <- predict(model_final, newdata = df_final_validation, estimator = "HPM", se.fit = T)

residuals.validation <- tibble(
        Data = "Validation",
        Observed = df_final_validation$price/1e5,
        Predicted = as.numeric(predict.validation.final$Ypred[1,]),
        `Z Score` = z_score(predict.validation.final$Ypred)[1,]
    ) %>%
    mutate(Predicted = exp(Predicted)/1e5) %>%
    mutate(Residual = Observed - Predicted)

residuals.final.validation <- rbind(residuals.final, residuals.validation)

price98 <- c(quantile(residuals.validation$Observed, .01), quantile(residuals.validation$Observed, .99))
res98 <- c(quantile(residuals.validation$Residual, .01), quantile(residuals.validation$Residual, .99))
observed.mu = mean(residuals.validation$Observed)
observed.sd = sd(residuals.validation$Observed)

residuals.final.validation %>%
    ggplot(aes(x = Observed, y = Residual/Observed, colour=Data)) +
    geom_vline(xintercept = observed.mu, linetype="longdash", alpha=0.5) +
    geom_vline(xintercept = observed.mu-observed.sd, linetype="dotted", alpha=0.4, size=1) +
    geom_vline(xintercept = observed.mu+observed.sd, linetype="dotted", alpha=0.4, size=1) +
    geom_point(alpha = 0.6, size=2) +
    geom_smooth(se = F, size = 1.5) +
    theme_minimal() +
    geom_hline(yintercept = 0) +
    scale_colour_manual(values=c("#33A02B", "#67a9d5", "orange")) +
    theme(plot.title = element_text(hjust = 0.5), plot.margin = margin(20,10,10,10)) +
    labs(
        title = "Residual Values for All Data Sets as proportion of Observed ($'00,000)",
    ) + 
    ylab("Residual") + xlab("Observed") + coord_cartesian(xlim=price98, ylim = c(-0.5,0.5)) +
    labs(
        caption = "Axes cropped to show middle 98% of test data in both directions.
        Vertical Lines are mean and plus/minus one standard devation for the Validation Data only."
    )

residuals.final.validation %>%
    group_by(Data) %>%
    summarise(RMSE = dollar_format()(sqrt(mean((Residual*1e5)^2)))) %>%
    kbl() %>% kable_styling(bootstrap_options = "condensed", full_width = F, position = "left")

train.confint <- exp(confint(predict.train.final, parm = "pred"))/1e5
test.confint <- exp(confint(predict.test.final, parm = "pred"))/1e5
validation.confint <- exp(confint(predict.validation.final, parm = "pred"))/1e5
all_confints <- as.data.frame(Data = "Train", train.confint[,]) %>%
    add_row(
        as.data.frame(Data = "Test", test.confint[,])
    ) %>%
    add_row(
        as.data.frame(Data = "Validation", validation.confint[,])
    ) 
    
cbind(residuals.final.validation, all_confints) %>% 
    mutate(in_ci = ifelse(Observed >= `2.5%` & Observed <= `97.5%`,TRUE,FALSE)) %>% 
    group_by(Data) %>%
    summarize(`% in CI` = round(mean(in_ci)*100,2)) %>%
    kbl() %>% kable_styling(bootstrap_options = "condensed", full_width = F, position = "left")

