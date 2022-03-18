library(MASS)
library(BAS)
library(GGally)
library(ggplot2)
library(ggthemes)
library(kableExtra)
library(moments)
library(RColorBrewer)
library(tidyverse)
library(moments)

load("ames_train.Rdata")

# deal with factor NAs (either NA or empty string) - replace with 'NONE'
replace_factor_na <- function(x){
    x <- as.character(x)
    x <- if_else((is.na(x) | (x=="")), 'NONE', x)
    x <- as.factor(x)
}
df_train <- ames_train %>%
    mutate_at(
        vars(Alley, BsmtFin.Type.1, BsmtFin.Type.2, Bsmt.Qual, Bsmt.Cond, Bsmt.Exposure,
             Fireplace.Qu, Garage.Type, Garage.Finish, Mas.Vnr.Type,
             Garage.Qual, Garage.Cond, Pool.QC, Fence, Misc.Feature), 
        replace_factor_na
    ) 

# drop utilities, only one value in whole db
unique(df_train$Utilities)
df_train <- df_train %>% select(-Utilities)

# drop year sold - doesn't help predict a sale value unless using it to adjust sale price
df_train <- df_train %>% select(-Yr.Sold)

df_train %>% 
    summarise(across(everything(), ~ sum(is.na(.x)))) %>%
    pivot_longer(everything(), names_to = "Variable") %>%
    rename(NAs = value) %>%
    slice_max(order_by = NAs, n=10)

#deal with continuous na's 

# 48 observations miss garage age  due to not having a garage
# for purposes of EDA, assume those values are the same as house age
# revisit if garage age is significant influencer
df_train <- df_train %>%
    mutate(Garage.Yr.Blt = ifelse(is.na(Garage.Yr.Blt), Year.Built, Garage.Yr.Blt))

# 1 in 7 are missing Lot.Frontage - EDA, found that
# RLot.Frontage looks to be significant influencer
# => strong liner relationship between area and frontage found (after removing outliers)
# => replace NAs with calculated value from area
# <30,000 represents roughly 98% of values
df_train %>%
    filter(Lot.Frontage>0 & Lot.Area<30000) %>%
    ggplot(aes(x=Lot.Area, y=Lot.Frontage)) +
    geom_point() +
    geom_smooth(method = "lm")

# find slope (given by covariance) and intercept
slope <- function(x, y){
    return(cov(x, y)/var(x))
}

intercept <- function(x, y, m){
    b <- mean(y) - (m * mean(x))
    return(b)
}

lf.parameters <- df_train %>%
    filter(Lot.Frontage>0 & Lot.Area<30000) %>%
    summarise(
        slope = slope(Lot.Area, Lot.Frontage),
        intercept = intercept(Lot.Area, Lot.Frontage, slope)
    )

# confirm with lm()
lot.frontage.lm <- lm(Lot.Area ~ Lot.Frontage, 
                      data = (df_train %>% filter(Lot.Frontage>0 & Lot.Area<20000)))
summary(lot.frontage.lm)

calc_frontage <- function(LotArea) {
    lf.parameters$intercept + lf.parameters$slope * LotArea
}

df_train <- df_train %>%
    mutate(Lot.Frontage = ifelse(is.na(Lot.Frontage), calc_frontage(Lot.Area), Lot.Frontage))

# Mas.Vnr.Area relates to Masonry veneer. Those that have none will have 0.
df_train <- df_train  %>%
    replace_na(list(Mas.Vnr.Area=0))

df_train %>% select_if(~ any(is.na(.))) %>% filter_at(vars(everything()), any_vars(is.na(.)))

# Only two observations remain with NAs - for the sake of EDA, set these to 0 also
df_train <- df_train  %>%
    replace(is.na(.), 0)

# Perform a basic ANOVA to find influencers to use on outlier test
ames.lm <- lm(log(price) ~ ., data = df_train)
ames_anova <- anova(ames.lm)
potential_vars <- (
        ames_anova %>%
            rownames_to_column(var = "Variable") %>%
            slice_max(`Pr(>F)`, n=10) 
    )$Variable

n <- nrow(df_train)
ames_k <- qnorm(0.5 + 0.5 * 0.95 ^ (1 / n))
ames.lm <- lm(log(price) ~ ., data = (df_train %>% select(price, all_of(potential_vars))))
outliers <- BAS::Bayes.outlier(ames.lm, k = ames_k)
head(sort(outliers$prob.outlier, decreasing=TRUE), 5)

# 3 observations can be counted as outlier
df_train$outlier = outliers$prob.outlier

df_train %>%
    filter(outlier>0.975) %>%
    select(PID, price, Year.Built, Year.Remod.Add, area, Overall.Cond, Exterior.1st, 
           Exter.Cond, Bsmt.Cond, BsmtFin.Type.1, Central.Air, Garage.Cond, Sale.Condition) %>%
    kbl() %>% kable_styling(bootstrap_options = c("condensed"), font_size = 10)

#remove outliers from training dataset
df_train <- df_train %>%
    filter(!(PID %in% (df_train %>% filter(outlier>0.975))$PID))

# filter sale.condition on normal then drop, only one value in whole db
df_train <- df_train %>% 
    filter(Sale.Condition == "Normal") %>%
    select(-Sale.Condition)
#----------------------------------------------------------

df_train %>% 
    ggplot(aes(x=log(area), y=log(price))) +
    geom_point(alpha = 0.4, colour="#B3DE69") +
    geom_smooth(method = "lm", colour = "#80B1D3") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(title = "Log of Sales Price vs Log of House Floor Area", y = "Log of Sale Price", x="Log of Floor Area")

df_log.age <- data.frame(age = log(2011- ames_train$Year.Built)) 
log.age.mu = mean(df_log.age$age)
log.age.sd = sd(df_log.age$age)

plot_age <- df_log.age %>% 
    ggplot(aes(x=age)) +
    geom_histogram(
        alpha = 0.8, 
        bins = 30, 
        aes(y = stat(density, ..scaled..)),
        position="identity", 
        fill="orange", 
        col="darkgrey") +
    stat_function(
        fun = dnorm, 
        aes(colour = "Normal"), 
        size = 1.2, 
        alpha = 0.6,
        args = list(
            mean = log.age.mu, 
            sd = log.age.sd
            )
        ) +
    geom_line(
        aes(y = ..density.., colour = "Observed"), 
        stat = 'density', 
        size = 1.5, 
        alpha = 0.8) + 
    theme_minimal() +
    scale_color_manual(name="Distribution", values=c(Normal="blue", Observed="red")) +
    theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
    labs(title = "Distribution of Log of House Age at 2011", y = "Density", x="Log of House Age")
plot_age

nei_colours <- colorRampPalette(brewer.pal(4, "Set1"))(27)

plot_price_year <- ames_train %>% 
    group_by(Neighborhood, Yr.Sold) %>%
    summarise(Med = median(log(price)))  %>%   
    ggplot(aes(x=Yr.Sold, y=Med, colour=Neighborhood)) +
    geom_point(alpha = 0.8, size=1.5) +
    geom_line(alpha = 0.8, size=1) +
    theme_minimal() +
    scale_color_manual(values = nei_colours) +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(
        title = "Median of Log House Price Sales Each Year by Neighbourhood", 
        y = "Median of Log House Price Sales", 
        x="Year Sold"
        )

    
neighbourhood_price_variation <- ames_train %>% 
    group_by(Neighborhood) %>% 
    summarise(
        SD = sd(price)/1e5, 
    ) %>%
    arrange(desc(SD)) %>%
    mutate(Neighborhood = factor(Neighborhood, unique(Neighborhood))) %>%
    arrange(desc(Neighborhood))

nei_plot <- ames_train %>% 
    select(Neighborhood, price) %>%
    mutate(Neighborhood = factor(Neighborhood, neighbourhood_price_variation$Neighborhood)) %>% 
    ggplot(aes(as.factor(Neighborhood), price/1e5), fill=Neighborhood) +
    theme_minimal() +
    geom_boxplot(aes(fill=Neighborhood)) +
    geom_violin(aes(colour=Neighborhood), width=1.5, alpha=0.3) + 
    coord_flip() +
    theme(
        plot.title = element_text(hjust = 0.5), 
        legend.position="none", 
        plot.caption = element_text(face="italic")
    ) +
    scale_color_manual(values = nei_colours) +
    labs(
        title = "Distribution of House Prices by Neighbourhood", 
        caption = "Neighbourhoods listed in descending order of variability",
        x = NULL, 
        y="Price ($'00,000)"
    )

#---------------------------------------------------------------------------

df_model1 <- df_train %>%
    mutate(log.price = log(price)) %>%
    mutate(log.area = log(area)) %>%
    mutate(log.lot.area = log(Lot.Area)) %>%
    mutate(log.age = log(2011 - Year.Built)) %>%
    mutate(remod.age = 2011 - Year.Remod.Add) %>%
    select(-price, -PID, -area, -Lot.Area, -Year.Built, -Year.Remod.Add)

ames.lm <- lm(log.price ~ ., data = df_model1)
ames_anova <- anova(ames.lm)
ames_anova_tbl <- ames_anova %>%
    add_rownames(var = "Variable") %>%
    arrange(`Pr(>F)`)

df_model1 <- df_model1 %>% 
    select(
        log.price, Neighborhood, Overall.Qual, House.Style, log.area, 
        log.lot.area, Bedroom.AbvGr, log.age, Full.Bath, Kitchen.Qual, MS.Zoning
    )

ames.lm <- lm(log.price ~ ., data = df_model1)
summary(ames.lm) 
ames_anova <- anova(ames.lm)
ames_anova_tbl <- ames_anova %>%
    rownames_to_column(var = "Variable") %>%
    arrange(`Pr(>F)`)


bas_ames.ZS <- bas.lm(log.price ~ ., 
                  data = df_model1,
                  prior = "ZS-null", 
                  modelprior = uniform(),
                  n.models=1000
)
summary(bas_ames.ZS)

models = data.frame(Estimator=character(), 
                    Post.Prob=numeric(), 
                    Vars=integer(),
                    Best.Vars=character()
                    )

#BMA
bas_ames.ZS.BMA = predict(bas_ames.ZS, estimator = "BMA")
models <- models %>% add_row(
    Estimator="BMA", 
    Post.Prob=max(bas_ames.ZS$postprobs[1]), 
    Vars=length(bas_ames.ZS.BMA$best.vars[-1]),
    Best.Vars=toString(bas_ames.ZS.BMA$best.vars[-1])
)

#HPM
bas_ames.ZS.HPM = predict(bas_ames.ZS, estimator = "HPM")
models <- models %>% add_row(
    Estimator="HPM", 
    Post.Prob=max(bas_ames.ZS$postprobs[bas_ames.ZS.HPM$best]), 
    Vars=length(bas_ames.ZS.HPM$best.vars[-1]),
    Best.Vars=toString(bas_ames.ZS.HPM$best.vars[-1])
)

#MPM
bas_ames.ZS.MPM = predict(bas_ames.ZS, estimator = "MPM", se.fit = T)
models <- models %>% add_row(
    Estimator="MPM", 
    Post.Prob=NA, 
    Vars=length(bas_ames.ZS.MPM$best.vars[-1]),
    Best.Vars=toString(bas_ames.ZS.MPM$best.vars[-1])
)

#BPM
bas_ames.ZS.BPM = predict(bas_ames.ZS, estimator = "BPM", se.fit = T)
models <- models %>% add_row(
    Estimator="BPM", 
    Post.Prob=max(bas_ames.ZS$postprobs[bas_ames.ZS.HPM$best]), 
    Vars=length(bas_ames.ZS.BPM$best.vars[-1]),
    Best.Vars=toString(bas_ames.ZS.BPM$best.vars[-1])
)

models %>%
    kbl() %>%
    kable_styling(bootstrap_options = c("condensed"))



BICk <- log(nrow(df_model1))
m_ames_BIC1 <- lm(log.price ~ ., data = df_model1)
ames.lm.BIC1 <- MASS::stepAIC(m_ames_BIC1, direction = "both", k = BICk)
summary(ames.lm.BIC1)


plot(ames.lm.BIC1, which = 1, add.smooth = T, 
     ask = F, pch = 16, sub.caption="", caption="")
abline(a = 0, b = 0, col = "darkgrey", lwd = 2)


plot(bas_ames.JZS, which = 1,  
     ask = F, pch = 16, sub.caption="", caption="")

library(glmnet)
# Loading the data
x_vars <- model.matrix(log.price~. , df_model1)[,-1]
lambda_seq <- 10^seq(2, -2, by = -.1)
# Splitting the data into test and train
ames.lm.cv <- cv.glmnet(x_vars, df_model1$log.price,
                        alpha = 1, lambda = lambda_seq, 
                        nfolds = 5)
# identifying best lamda
best_lam <- ames.lm.cv$lambda.min
best_lam

# Rebuilding the model with best lamda value identified
ames.lasso <- glmnet(x_vars, df_model1$log.price, alpha = 1, lambda = best_lam)
ames.lasso.pred <- predict(ames.lasso, s = best_lam, newx = x_vars)
head(cbind(df_model1$log.price, pred), 10)

residuals <- tibble(
    Observed = df_train$price/1e5,
    Predicted.HPM = as.numeric(bas_ames.ZS.HPM$Ypred[1,]),
    Predicted.BIC = as.numeric(ames.lm.BIC1$fitted.values),
    Predicted.Lasso = as.numeric(ames.lasso.pred)
    ) %>%
    mutate(Predicted.HPM = exp(Predicted.HPM)/1e5) %>%
    mutate(Predicted.BIC = exp(Predicted.BIC)/1e5) %>%
    mutate(Predicted.Lasso = exp(Predicted.Lasso)/1e5) %>%
    mutate(HPM = Observed - Predicted.HPM) %>%
    mutate(BIC = Observed - Predicted.BIC)  %>% 
    mutate(Lasso = Observed - Predicted.Lasso)  %>% 
    pivot_longer(
        cols = c(Lasso, BIC, HPM), 
        values_to = "Residual", 
        names_to = "Model"
    ) %>% 
    mutate(Model = factor(Model, levels = c("Lasso", "BIC", "HPM")))
    
residual.mu = mean(residuals$Residual)
residual.sd = sd(residuals$Residual)

residuals %>% 
    ggplot(aes(x=Residual)) +
    geom_histogram(
        alpha = 0.8, 
        bins = 30, 
        aes(y = stat(density)),
        position="identity", 
        fill="orange", 
        col="darkgrey") +
    stat_function(
        fun = dnorm, 
        aes(colour = "Normal"), 
        size = 1.2, 
        alpha = 0.6,
        args = list(
            mean = residual.mu, 
            sd = residual.sd
        )
    ) +
    geom_line(
        aes(y = ..density.., colour = "Observed"), 
        stat = 'density', 
        size = 1.5, 
        alpha = 0.8) + 
    theme_minimal() +
    scale_color_manual(name="Distribution", values=c(Normal="blue", Observed="red")) +
    theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
    labs(title = "Distribution of Residuals Using HPM", y = "Density", x="Residual ($'00,000)")


price.mu = mean(df_train$price)/1e5
price.sd = sd(df_train$price)/1e5
g_res <- residuals %>%
    ggplot(aes(x = Observed, y = Residual, colour = Model)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = price.mu, linetype="longdash", alpha=0.5) +
    geom_vline(xintercept = price.mu-price.sd, linetype="dotted", alpha=0.4, size=1) +
    geom_vline(xintercept = price.mu+price.sd, linetype="dotted", alpha=0.4, size=1) +
    geom_point(alpha = 0.4, size=2) +
    geom_smooth(se = F, size = 1.2) +
    theme_minimal() +
    scale_color_manual(values = c("#80B1D3", "#B3DE69", "#FF7F00")) +
    theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(face="italic")) +
    labs(
        title = "Residual vs Observed Values for BIC and MPM Models ($'00,000)"
    )
residuals.train.HPM <- residuals %>% filter(Model=="HPM")
price99 <- c(quantile(residuals.train.HPM$Observed, .01), quantile(residuals.train.HPM$Observed, .99))
res99 <- c(quantile(residuals.train.HPM$Residual, .01), quantile(residuals.train.HPM$Residual, .99))
g_res
g_res + coord_cartesian(xlim=price99, ylim = res99) 

#----------------------------------------------------------------
# test data frame
#----------------------------------------------------------------

df_test <- ames_test %>%
    mutate_at(
        vars(Alley, BsmtFin.Type.1, BsmtFin.Type.2, Bsmt.Qual, Bsmt.Cond, Bsmt.Exposure,
             Fireplace.Qu, Garage.Type, Garage.Finish, Mas.Vnr.Type,
             Garage.Qual, Garage.Cond, Pool.QC, Fence, Misc.Feature), 
        replace_factor_na
    ) 

# drop utilities & sale.condition, only one value in whole db
df_test <- df_test %>% select(-Utilities, -Sale.Condition)

# drop month/year sold - doesn't help predict a sale value unless using it to adjust sale price
df_test <- df_test %>% select(-Yr.Sold, -Mo.Sold)

#deal with continuous na's 

# 48 observations miss garage age  due to not having a garage
# for purposes of EDA, assume those values are the same as house age
# revisit if garage age is significant influencer
df_test <- df_test %>%
    mutate(Garage.Yr.Blt = ifelse(is.na(Garage.Yr.Blt), Year.Built, Garage.Yr.Blt))

# 1 in 7 are missing Lot.Frontage - EDA, found that
# RLot.Frontage looks to be significant influencer
# => strong liner relationship between area and frontage found (after removing outliers)
# => replace NAs with calculated value from area
# <30,000 represents roughly 98% of values

df_test <- df_test %>%
    mutate(Lot.Frontage = ifelse(is.na(Lot.Frontage), calc_frontage(Lot.Area), Lot.Frontage))

# Mas.Vnr.Area relates to Masonry veneer. Those that have none will have 0.
df_test <- df_test  %>%
    replace_na(list(Mas.Vnr.Area=0))

df_test <- df_test  %>%
    replace(is.na(.), 0)

# Perform a basic ANOVA to find influencers to use on outlier test
ames.lm <- lm(log(price) ~ ., data = df_test)
ames_anova <- anova(ames.lm)
potential_vars <- (
    ames_anova %>%
        rownames_to_column(var = "Variable") %>%
        slice_max(`Pr(>F)`, n=10) 
)$Variable

n <- nrow(df_test)
ames_k <- qnorm(0.5 + 0.5 * 0.95 ^ (1 / n))
ames.lm <- lm(log(price) ~ ., data = (df_test %>% select(price, all_of(potential_vars))))
outliers <- BAS::Bayes.outlier(ames.lm, k = ames_k)
head(sort(outliers$prob.outlier, decreasing=TRUE), 5)

# 3 observations can be counted as outlier
df_test$outlier = outliers$prob.outlier

#remove outliers from training dataset
df_test <- df_test %>%
    filter(!(PID %in% (df_train %>% filter(outlier>0.975))$PID))

# Remove single entry for Landmrk neighbourhood as this does not exist in training model
# Remove 2 entries with House.Style 2.5Fin not present in training model
df_test <- df_test %>%
    filter(Neighborhood != "Landmrk") %>%
    filter(House.Style != "2.5Fin")

df_model2 <- df_test %>%
    mutate(log.price = log(price)) %>%
    mutate(log.area = log(area)) %>%
    mutate(log.lot.area = log(Lot.Area)) %>%
    mutate(log.age = log(2011 - Year.Built)) %>%
    mutate(remod.age = 2011 - Year.Remod.Add) %>%
    select(
        log.price, Neighborhood, Overall.Qual, House.Style, log.area, 
        log.lot.area, Bedroom.AbvGr, log.age, Full.Bath, Kitchen.Qual, MS.Zoning
    )

HPM.intial <- as.vector(which.matrix(bas_ames.ZS$which[bas_ames.ZS.HPM$best],
                              bas_ames.ZS$n.vars))

model_initial <- bas.lm(log.price ~ ., data = df_model1,
                      prior = "ZS-null",
                      modelprior = uniform(),
                      bestmodel = HPM.intial, n.models = 1)

predict.train.initial <- predict(model_initial, newdata = df_model1, estimator = "HPM")
predict.test.initial <- predict(model_initial, newdata = df_model2, estimator = "HPM")

residuals.intial <- tibble(
    Data = "Train",
    Observed = df_train$price/1e5,
    Predicted = as.numeric(predict.train.initial$Ypred[1,])
) %>%
    add_row(
        Data = "Test",
        Observed = df_test$price/1e5,
        Predicted = as.numeric(predict.test.initial$Ypred[1,])
    ) %>%
    mutate(Predicted = exp(Predicted)/1e5) %>%
    mutate(Residual = Observed - Predicted) 

residuals.intial <- tibble(
    Data = "Train",
    Observed = df_train$price/1e5,
    Predicted = as.numeric(predict.train.initial$Ypred[1,])
) %>%
    add_row(
        Data = "Test",
        Observed = df_test$price/1e5,
        Predicted = as.numeric(predict.test.initial$Ypred[1,])
    ) %>%
    mutate(Predicted = exp(Predicted)/1e5) %>%
    mutate(Residual = Observed - Predicted) 

residuals.intial %>% 
    mutate(Residual = (Residual)*1e5) %>%
    group_by(Data) %>%
    summarise(
        RMSE=sqrt(mean(Residual^2)),
        mean=round(mean(abs(Residual)),2),
        median=median(abs(Residual)), 
        sd=round(sd(abs(Residual)),2),
        max=max(abs(Residual)),
        q25=quantile(abs(Residual), 0.25),
        q75=quantile(abs(Residual),0.75),
        skew=round(skewness(Residual),2)) %>%
    mutate(across(RMSE:q75, dollar_format())) %>%
    kbl(
        caption = "<center><h4><strong>Distribution of Absolute Residuals</strong></h4></center>",
        escape = F
    ) %>% 
    kable_styling(bootstrap_options = c("condensed"))

test.price98 <- c(quantile(residuals.test$Observed, .01), quantile(residuals.test$Observed, .99))
test.res98 <- c(quantile(residuals.test$Residual, .01), quantile(residuals.test$Residual, .99))

residuals.train.test <- residuals %>% 
    filter(Model=="HPM") %>%
    select(Observed, Residual) %>%
    mutate(Data="Train") %>%
    add_row(Observed = residuals.test$Observed, Residual = residuals.test$Residual, Data="Test")

g_test_res <- residuals.train.test %>%
    ggplot(aes(x = Observed, y = Residual, colour=Data)) +
    geom_vline(xintercept = price.mu, linetype="longdash", alpha=0.5) +
    geom_vline(xintercept = price.mu-price.sd, linetype="dotted", alpha=0.4, size=1) +
    geom_vline(xintercept = price.mu+price.sd, linetype="dotted", alpha=0.4, size=1) +
    geom_point(alpha = 0.6, size=2) +
    geom_smooth(se = F, size = 1.5) +
    theme_minimal() +
    geom_hline(yintercept = 0) +
    scale_colour_manual(values=c("#33A02B", "#67a9d5")) +
    theme(plot.title = element_text(hjust = 0.5), plot.margin = margin(20,10,10,10)) +
    labs(
        title = "Residual Values for HPM Model for Train and Test Data ($'00,000)",
    ) + 
    ylab("Residual") + xlab("Observed") 

g_test_res
g_test_res + coord_cartesian(xlim=test.price98, ylim = test.res98) +
    labs(
        caption = "Axes cropped to show middle 98% of test data in both directions."
    )


residuals %>% 
    filter(Model=="HPM") %>%
    mutate(Residual = (Residual)*1e5) %>%
    summarise(
        `Data Set` = "Train",
        RMSE=sqrt(mean(Residual^2)),
        mean=round(mean(abs(Residual)),2),
        median=median(abs(Residual)), 
        sd=round(sd(abs(Residual)),2),
        max=max(abs(Residual)),
        q25=quantile(abs(Residual), 0.25),
        q75=quantile(abs(Residual),0.75),
        skew=round(skewness(Residual),2)) %>%
    add_row(
        `Data Set` = "Test",
        RMSE=sqrt(mean((residuals.test$Residual*1e5)^2)),
        mean=round(mean(abs(residuals.test$Residual)*1e5),2),
        median=median(abs(residuals.test$Residual)*1e5), 
        sd=round(sd(abs(residuals.test$Residual)*1e5),2),
        max=max(abs(residuals.test$Residual)*1e5),
        q25=quantile(abs(residuals.test$Residual)*1e5, 0.25),
        q75=quantile(abs(residuals.test$Residual)*1e5,0.75),
        skew=round(skewness(residuals.test$Residual*1e5),2)
    ) %>%
    mutate(across(RMSE:q75, dollar_format())) %>%
    kbl(
        caption = "<center><h4><strong>Distribution of Absolute Residuals</strong></h4></center>",
        escape = F
        ) %>% 
    kable_styling(
        bootstrap_options = c("condensed"),
        full_width = F,
        position = "left"
    )

residual.mu = mean(residuals.test$Residual)
residual.sd = sd(residuals.test$Residual)
test.res99 <- c(quantile(residuals.test$Residual, .005), quantile(residuals.test$Residual, .995))

residuals.train.test %>%
    ggplot(aes(x=Residual, fill=Data, colour=Data)) +
    geom_histogram(
        alpha = 0.3, 
        bins = 30, 
        aes(y = stat(density)),
        position="identity", 
        col="darkgrey"
        ) +
    geom_vline(
        aes(xintercept = median(Residual[Data=="Test"]), color="Test"), 
        size = 1.2, 
        linetype="longdash"
    ) + 
    geom_vline(
        aes(xintercept = median(Residual[Data=="Train"]), color="Train"), 
        size = 1.2, 
        linetype="longdash"
    ) + 
    stat_function(
        fun = dnorm, 
        aes(colour = "Normal"), 
        size = 1.2, 
        linetype="longdash",
        args = list(
            mean = residual.mu, 
            sd = residual.sd
        )
    ) +
    geom_line(
        aes(y = ..density.., colour = Data), 
        stat = 'density', 
        size = 1.5
        ) + 
    theme_minimal() +
    scale_fill_manual(values=c("Test" = "#33A02B", "Train" = "#1F78B4")) +
    scale_colour_manual(name="Density", values = c("Normal" = "#E3211C", "Test" = "#33A02B", "Train" = "#1F78B4")) + 
    theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
    labs(title = "Distribution of Residuals Using HPM", y = "Density", x="Residual ($'00,000)") + 
    coord_cartesian(xlim=c(-1,1)) 

#--------------------------------------------------------------------
# Final model
#--------------------------------------------------------------------

# Remove price after creating log.price
# Remove two outlier properties identified in initial model testing
# Drop 2 outliers found in initial model testing
df_train <- df_train %>%
    mutate(log.age = log(2011 - Year.Built)) %>%
    mutate(log.remod.age = log(2011 - Year.Remod.Add)) %>%
    filter(!(PID %in% c(908154205, 533350090))) %>%
    filter(Sale.Condition == "Normal")

df_test <- df_test %>%
    mutate(log.age = log(2011 - Year.Built)) %>%
    mutate(log.remod.age = log(2011 - Year.Remod.Add))
    
# Drop colinear and other variable scoring low ANOVA during EDA tests
df_model2 <- df_train %>%
    mutate(log.price = log(price)) %>%
    select(-price, -PID, -Year.Built, -Year.Remod.Add, -Garage.Yr.Blt, -outlier,
           -Overall.Cond, -Bsmt.Cond, -Bsmt.Exposure, -BsmtFin.Type.1, -BsmtFin.SF.1,
           -BsmtFin.Type.2, BsmtFin.SF.2, -Bsmt.Unf.SF, -Total.Bsmt.SF,
           -Exter.Qual, -Pool.QC, -MS.SubClass, -X1st.Flr.SF, -X2nd.Flr.SF,
           -Alley, -Lot.Frontage, -Land.Slope, -Condition.1, Condition.2, -Roof.Matl,
           -Exterior.1st, -Exterior.2nd, -Heating, -Central.Air, -Electrical, -Functional,
           -Garage.Cond, -Garage.Qual, -Misc.Feature)

# re-run ANOVA and select best 20
ames.lm <- lm(log.price ~ ., data = df_model2)
ames_anova <- anova(ames.lm)
ames_anova_tbl <- ames_anova %>%
    add_rownames(var = "Variable") %>%
    slice_min(`Pr(>F)`, n=20) %>%
    arrange(`Pr(>F)`)

# create subset with only those vars
df_model2 <- df_model2 %>% select(log.price, all_of(ames_anova_tbl$Variable))

# create initial BAS model
model_final <- bas.lm(log.price ~ ., 
                      data = df_model2,
                      prior = "ZS-null", 
                      modelprior = uniform(),
                      n.models=1000
)

repeat {

    prior_n <- length(attributes(model_final$x)$names)
    #use HPM to find best vars
    model_final.HPM <- predict(model_final, estimator = "HPM")
    # create vector of best vars and re-run BAS model selecting only those
    model_final$postprobs[model_final.HPM$best]
    HPM <- as.vector(which.matrix(model_final$which[model_final.HPM$best],
                                 model_final$n.vars))
    model_final <- bas.lm(log.price ~ ., data = df_model2,
                            prior = "ZS-null",
                            modelprior = uniform(),
                            bestmodel = HPM, n.models = 1000)
    if(prior_n == length(attributes(model_final$x)$names)){break}
    
}
model_final <- bas.lm(log.price ~ ., data = df_model2,
                      prior = "ZS-null",
                      modelprior = uniform(),
                      bestmodel = HPM, n.models = 1)

coef(model_final, estimator="HPM")
predict.train.final <- predict(model_final, newdata = df_train, estimator = "HPM", se.fit = T)

# bas drops empty levels from model
df_final_test <- df_test %>%
    filter(Foundation != "Wood") %>%
    filter(Exter.Cond != "Po")

predict.test.final <- predict(model_final, newdata = df_final_test, estimator = "HPM", se.fit = T)

z_score <- function(log.prices){
    predicts <- exp(log.prices)
    mu_train <- mean(predicts)
    sd_train <- sd(predicts)
    (predicts - mu_train)/sd_train
}

residuals.final <- tibble(
        Data = "Train",
        Observed = df_train$price/1e5,
        Predicted = as.numeric(predict.train.final$Ypred[1,]),
        `Z Score` = z_score(predict.train.final$Ypred)[1,]
    ) %>%
    add_row(
        Data = "Test",
        Observed = df_final_test$price/1e5,
        Predicted = as.numeric(predict.test.final$Ypred[1,]),
        `Z Score` = z_score(predict.test.final$Ypred)[1,]
    ) %>%
    mutate(Predicted = exp(Predicted)/1e5) %>%
    mutate(Residual = Observed - Predicted) 

price98 <- c(quantile(residuals.final$Observed, .01), quantile(residuals.final$Observed, .99))
res98 <- c(quantile(residuals.final$Residual, .01), quantile(residuals.final$Residual, .99))
#residual.mu = mean(residuals.test$Residual)
#residual.sd = sd(residuals.test$Residual)
#res99 <- c(quantile(residuals.test$Residual, .005), quantile(residuals.test$Residual, .995))
observed.mu = mean(residuals.final$Observed)
observed.sd = sd(residuals.final$Observed)


g_final_res <- residuals.final %>%
    ggplot(aes(x = Observed, y = Residual, colour=Data)) +
    geom_vline(xintercept = observed.mu, linetype="longdash", alpha=0.5) +
    geom_vline(xintercept = observed.mu-observed.sd, linetype="dotted", alpha=0.4, size=1) +
    geom_vline(xintercept = observed.mu+observed.sd, linetype="dotted", alpha=0.4, size=1) +
    geom_point(alpha = 0.6, size=2) +
    geom_smooth(se = F, size = 1.5) +
    theme_minimal() +
    geom_hline(yintercept = 0) +
    scale_colour_manual(values=c("#33A02B", "#67a9d5")) +
    theme(plot.title = element_text(hjust = 0.5), plot.margin = margin(20,10,10,10)) +
    labs(
        title = "Residual Values for Final Model for Train and Test Data ($'00,000)",
    ) + 
    ylab("Residual") + xlab("Observed") 

g_final_res
g_final_res + coord_cartesian(xlim=price98, ylim = res98) +
    labs(
        caption = "Axes cropped to show middle 98% of test data in both directions."
    )

g_final_hist_res <- residuals.final %>%
    ggplot(aes(x=Residual, fill=Data, colour=Data)) +
    geom_histogram(
        alpha = 0.3, 
        bins = 30, 
        aes(y = stat(density)),
        position="identity", 
        col="darkgrey"
    ) +
    geom_vline(
        aes(xintercept = median(Residual[Data=="Test"]), color="Test"), 
        size = 1.2, 
        linetype="longdash"
    ) + 
    geom_vline(
        aes(xintercept = median(Residual[Data=="Train"]), color="Train"), 
        size = 1.2, 
        linetype="longdash"
    ) + 
    stat_function(
        fun = dnorm, 
        aes(colour = "Normal"), 
        size = 1.2, 
        linetype="longdash",
        args = list(
            mean = residual.mu, 
            sd = residual.sd
        )
    ) +
    geom_line(
        aes(y = ..density.., colour = Data), 
        stat = 'density', 
        size = 1.5
    ) + 
    theme_minimal() +
    scale_fill_manual(values=c("Test" = "#33A02B", "Train" = "#1F78B4")) +
    scale_colour_manual(name="Density", values = c("Normal" = "#E3211C", "Test" = "#33A02B", "Train" = "#1F78B4")) + 
    theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
    labs(title = "Distribution of Residuals Using HPM", y = "Density", x="Residual ($'00,000)") + 
    coord_cartesian(xlim=c(-1,1)) 
g_final_hist_res
residuals.final %>% ggplot(aes(colour=Data)) +
    geom_qq(aes(sample = Residual/`Z Score`), alpha = 0.4) +
    scale_color_manual(values=c("Test" = "#33A02B", "Train" = "#1F78B4")) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
    labs(title = "QQ-Plot of Standardised Residuals") + 
    labs(x = "Theoretical quantiles", y = "Standardized residuals") + 
    coord_cartesian(ylim=c(-3,3), xlim=c(-3,3)) 

residuals.final %>%
    group_by(Data) %>%
    summarise(RMSE = dollar_format()(sqrt(mean((Residual*1e5)^2)))) %>%
    kbl() %>% kable_styling(bootstrap_options = "condensed", full_width = F, position = "left")
    


#============================================================
# Non log test
pUL <- 1.25e5
pUL.df_final_train <- df_train %>% filter(price < pUL)
pUL.df_final_test <- df_test %>% filter(price < pUL)
pUL.df_final_validate <- df_validation %>% filter(price < pUL)

# Drop colinear and other variable scoring low ANOVA during EDA tests
df_model3 <- pUL.df_final_train %>%
    select(-PID, -Year.Built, -Year.Remod.Add, -Garage.Yr.Blt, -outlier,
           -Overall.Cond, -Bsmt.Cond, -Bsmt.Exposure, -BsmtFin.Type.1, -BsmtFin.SF.1,
           -BsmtFin.Type.2, BsmtFin.SF.2, -Bsmt.Unf.SF, -Total.Bsmt.SF,
           -Exter.Qual, -Pool.QC, -MS.SubClass, -X1st.Flr.SF, -X2nd.Flr.SF,
           -Alley, -Lot.Frontage, -Land.Slope, -Condition.1, -Condition.2, -Roof.Matl,
           -Exterior.1st, -Exterior.2nd, -Heating, -Central.Air, -Electrical, -Functional,
           -Garage.Cond, -Garage.Qual, -Misc.Feature)

# re-run ANOVA and select best 20
pUL.ames.lm <- lm(price ~ ., data = df_model3)
pUL.ames_anova <- anova(pUL.ames.lm)
pUL.ames_anova_tbl <- pUL.ames_anova %>%
    add_rownames(var = "Variable") %>%
    slice_min(`Pr(>F)`, n=20) %>%
    arrange(`Pr(>F)`)

# create subset with only those vars
df_model3 <- df_model3 %>% select(price, all_of(pUL.ames_anova_tbl$Variable))

# create initial BAS model
pUL.model_final <- bas.lm(price ~ ., 
                      data = df_model3,
                      prior = "ZS-null", 
                      modelprior = uniform(),
                      n.models=1000
)

repeat {
    
    pUL.prior_n <- length(attributes(pUL.model_final$x)$names)
    #use HPM to find best vars
    pUL.model_final.HPM <- predict(pUL.model_final, estimator = "HPM")
    # create vector of best vars and re-run BAS model selecting only those
    pUL.model_final$postprobs[pUL.model_final.HPM$best]
    pUL.HPM <- as.vector(which.matrix(pUL.model_final$which[pUL.model_final.HPM$best],
                                      pUL.model_final$n.vars))
    pUL.model_final <- bas.lm(price ~ ., data = df_model3,
                          prior = "ZS-null",
                          modelprior = uniform(),
                          bestmodel = pUL.HPM, n.models = 1000)
    if(pUL.prior_n == length(attributes(pUL.model_final$x)$names)){break}
    
}

coef(pUL.model_final, estimator="HPM")
pUL.predict.train.final <- predict(pUL.model_final, newdata = pUL.df_final_train, estimator = "HPM", se.fit = T)

# bas drops empty levels from model
pUL.df_final_test <- pUL.df_final_test %>%
    filter(Foundation != "Wood") %>%
    filter(House.Style != "SLvl") %>%
    filter(Exter.Cond != "Po") 
pUL.predict.test.final <- predict(pUL.model_final, newdata = pUL.df_final_test, estimator = "HPM", se.fit = T)

pUL.df_final_validate <- pUL.df_final_validate %>%
    filter(MS.Zoning != "A (agr)") %>%
    filter(Bsmt.Qual != "Ex") %>%
    filter(House.Style != "SLvl") %>%
    filter(Foundation != "Wood") %>%
    filter(Neighborhood != "Gilbert") %>%
    filter(House.Style != "2.5Fin")
pUL.predict.validate.final <- predict(pUL.model_final, newdata = pUL.df_final_validate, estimator = "HPM", se.fit = T)

pUL.residuals.final <- tibble(
    Data = "Train",
    Observed = pUL.df_final_train$price/1e5,
    Predicted = as.numeric(pUL.predict.train.final$Ypred[1,]),
    `Z Score` = z_score(pUL.predict.train.final$Ypred)[1,]
) %>%
    add_row(
        Data = "Test",
        Observed = pUL.df_final_test$price/1e5,
        Predicted = as.numeric(pUL.predict.test.final$Ypred[1,]),
        `Z Score` = z_score(pUL.predict.test.final$Ypred)[1,]
    ) %>%
    add_row(
        Data = "Validate",
        Observed = pUL.df_final_validate$price/1e5,
        Predicted = as.numeric(pUL.predict.validate.final$Ypred[1,]),
        `Z Score` = z_score(pUL.predict.validate.final$Ypred)[1,]
    ) %>%
    mutate(Predicted = (Predicted)/1e5) %>%
    mutate(Residual = Predicted - Observed) 

pUL.price98 <- c(quantile(pUL.residuals.final$Observed, .01), quantile(pUL.residuals.final$Observed, .99))
pUL.res98 <- c(quantile(pUL.residuals.final$Residual, .01), quantile(pUL.residuals.final$Residual, .99))
pUL.observed.mu = mean(pUL.residuals.final$Observed)
pUL.observed.sd = sd(pUL.residuals.final$Observed)


pUL.g_final_res <- pUL.residuals.final %>%
    ggplot(aes(x = Observed, y = Residual/Observed, colour=Data)) +
    geom_vline(xintercept = pUL.observed.mu, linetype="longdash", alpha=0.5) +
    geom_vline(xintercept = pUL.observed.mu-pUL.observed.sd, linetype="dotted", alpha=0.4, size=1) +
    geom_vline(xintercept = pUL.observed.mu+pUL.observed.sd, linetype="dotted", alpha=0.4, size=1) +
    geom_point(alpha = 0.6, size=2) +
    geom_smooth(se = F, size = 1.5) +
    theme_minimal() +
    geom_hline(yintercept = 0) +
    scale_colour_manual(values=c("#33A02B", "#67a9d5", "orange")) +
    theme(plot.title = element_text(hjust = 0.5), plot.margin = margin(20,10,10,10)) +
    labs(
        title = "Residual Values as Proportion of Observed for Trial Model where price < $350,000",
    ) + 
    ylab("Residual ($'00,000)") + xlab("Observed ($'00,000)")  + 
    coord_cartesian(ylim=c(-1,1)) 

pUL.g_final_res

pUL.residuals.final %>%
    group_by(Data) %>%
    summarise(RMSE = dollar_format()(sqrt(mean((Residual*1e5)^2)))) %>%
    kbl() %>% kable_styling(bootstrap_options = "condensed", full_width = F, position = "left")

pUL.train.confint <- confint(pUL.predict.train.final, parm = "pred")/1e5
pUL.test.confint <- confint(pUL.predict.test.final, parm = "pred")/1e5
pUL.validation.confint <- confint(pUL.predict.validate.final, parm = "pred")/1e5
pUL.all_confints <- as.data.frame(Data = "Train", pUL.train.confint[,]) %>%
    add_row(
        as.data.frame(Data = "Test", pUL.test.confint[,])
    ) %>%
    add_row(
        as.data.frame(Data = "Validate", pUL.validation.confint[,])
    ) 

cbind(pUL.residuals.final, pUL.all_confints) %>% 
    mutate(in_ci = ifelse(Observed >= `2.5%` & Observed <= `97.5%`,TRUE,FALSE)) %>% 
    group_by(Data) %>%
    summarize(`% in CI` = round(mean(in_ci)*100,2)) %>%
    kbl() %>% kable_styling(bootstrap_options = "condensed", full_width = F, position = "left")


#---Upper Test
pLL <- 3.2e5
pLL.df_final_train <- df_train %>% filter(price > pLL)
pLL.df_final_test <- df_test %>% filter(price > pLL)
pLL.df_final_validate <- df_validation %>% filter(price > pLL)

# Drop colinear and other variable scoring low ANOVA during EDA tests
df_model4 <- pLL.df_final_train %>%
    select(-PID, -Year.Built, -Year.Remod.Add, -Garage.Yr.Blt, -outlier, -Pool.Area,
           -Overall.Cond, -Bsmt.Cond, -Bsmt.Exposure, -BsmtFin.Type.1, -BsmtFin.SF.1,
           -BsmtFin.Type.2, BsmtFin.SF.2, -Bsmt.Unf.SF, -Total.Bsmt.SF, -Sale.Type, -Misc.Val,
           -Exter.Qual, -Pool.QC, -MS.SubClass, -X1st.Flr.SF, -X2nd.Flr.SF, -Lot.Config,, -Lot.Shape,
           -Alley, -Lot.Frontage, -Land.Slope, -Condition.1, -Condition.2, -Roof.Matl,
           -Exterior.1st, -Exterior.2nd, -Heating, -Central.Air, -Electrical, -Functional,
           -Garage.Cond, -Garage.Qual, -Misc.Feature, -Street, -Paved.Drive, -Bldg.Type,
           -Mas.Vnr.Area, -Exter.Cond, -MS.Zoning, -Roof.Style, -Roof.Matl, -Fireplace.Qu)

# re-run ANOVA and select best 20
pLL.ames.lm <- lm(price ~ ., data = df_model4)
pLL.ames_anova <- anova(pLL.ames.lm)
pLL.ames_anova_tbl <- pLL.ames_anova %>%
    add_rownames(var = "Variable") %>%
    slice_min(`Pr(>F)`, n=20) %>%
    arrange(`Pr(>F)`)

# create subset with only those vars
df_model4 <- df_model4 %>% select(price, all_of(pLL.ames_anova_tbl$Variable))

# create initial BAS model
pLL.model_final <- bas.lm(price ~ ., 
                          data = df_model4,
                          prior = "ZS-null", 
                          modelprior = uniform(),
                          n.models=1000
)

repeat {
    
    pLL.prior_n <- length(attributes(pLL.model_final$x)$names)
    #use HPM to find best vars
    pLL.model_final.HPM <- predict(pLL.model_final, estimator = "HPM")
    # create vector of best vars and re-run BAS model selecting only those
    pLL.model_final$postprobs[pLL.model_final.HPM$best]
    pLL.HPM <- as.vector(which.matrix(pLL.model_final$which[pLL.model_final.HPM$best],
                                      pLL.model_final$n.vars))
    pLL.model_final <- bas.lm(price ~ ., data = df_model4,
                              prior = "ZS-null",
                              modelprior = uniform(),
                              bestmodel = pLL.HPM, n.models = 1000)
    if(pLL.prior_n == length(attributes(pLL.model_final$x)$names)){break}
    
}

coef(pLL.model_final, estimator="HPM")
pLL.predict.train.final <- predict(pLL.model_final, newdata = pLL.df_final_train, estimator = "HPM", se.fit = T)

# bas drops empty levels from model
pLL.df_final_test <- pLL.df_final_test %>%
    filter(!(Neighborhood %in% c("ClearCr", "CollgCr"))) %>%
    filter(Fence != "MnPrv")
pLL.predict.test.final <- predict(pLL.model_final, newdata = pLL.df_final_test, estimator = "HPM", se.fit = T)

pLL.df_final_validate <- pLL.df_final_validate  %>%
    filter(!(House.Style %in% c("2.5Fin", "SLvl"))) %>%
    filter(!(Neighborhood %in% c("ClearCr", "NWAmes", "OldTown"))) %>%
    filter(Lot.Config != "FR2") %>%
    filter(Fence != "MnPrv")

pLL.predict.validate.final <- predict(pLL.model_final, newdata = pLL.df_final_validate, estimator = "HPM", se.fit = T)

pLL.residuals.final <- tibble(
    Data = "Train",
    Observed = pLL.df_final_train$price/1e5,
    Predicted = as.numeric(pLL.predict.train.final$Ypred[1,]),
    `Z Score` = z_score(pLL.predict.train.final$Ypred)[1,]
) %>%
    add_row(
        Data = "Test",
        Observed = pLL.df_final_test$price/1e5,
        Predicted = as.numeric(pLL.predict.test.final$Ypred[1,]),
        `Z Score` = z_score(pLL.predict.test.final$Ypred)[1,]
    ) %>%
    add_row(
        Data = "Validate",
        Observed = pLL.df_final_validate$price/1e5,
        Predicted = as.numeric(pLL.predict.validate.final$Ypred[1,]),
        `Z Score` = z_score(pLL.predict.validate.final$Ypred)[1,]
    ) %>%
    mutate(Predicted = (Predicted)/1e5) %>%
    mutate(Residual = Predicted - Observed) 

pLL.price98 <- c(quantile(pLL.residuals.final$Observed, .01), quantile(pLL.residuals.final$Observed, .99))
pLL.res98 <- c(quantile(pLL.residuals.final$Residual, .01), quantile(pLL.residuals.final$Residual, .99))
pLL.observed.mu = mean(pLL.residuals.final$Observed)
pLL.observed.sd = sd(pLL.residuals.final$Observed)


pLL.g_final_res <- pLL.residuals.final %>%
    ggplot(aes(x = Observed, y = Residual/Observed, colour=Data)) +
    geom_vline(xintercept = pLL.observed.mu, linetype="longdash", alpha=0.5) +
    geom_vline(xintercept = pLL.observed.mu-pLL.observed.sd, linetype="dotted", alpha=0.4, size=1) +
    geom_vline(xintercept = pLL.observed.mu+pLL.observed.sd, linetype="dotted", alpha=0.4, size=1) +
    geom_point(alpha = 0.6, size=2) +
    geom_smooth(se = F, size = 1.5, method = "lm") +
    theme_minimal() +
    geom_hline(yintercept = 0) +
    scale_colour_manual(values=c("#33A02B", "#67a9d5", "orange")) +
    theme(plot.title = element_text(hjust = 0.5), plot.margin = margin(20,10,10,10)) +
    labs(
        title = "Residual Values as Proportion of Observed for Trial Model where price < $350,000",
    ) + 
    ylab("Residual ($'00,000)") + xlab("Observed ($'00,000)") 

pLL.g_final_res
