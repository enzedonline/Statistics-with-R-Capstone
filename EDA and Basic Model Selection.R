ames_train %>% 
    mutate(age_at_2010 = 2010 - Year.Built) %>%
    ggplot(aes(x=age_at_2010)) +
    geom_histogram(alpha = 0.8, bins = 30, aes(y = stat(density)),
                   position="identity", fill="orange", col="darkgrey") +
    geom_line(aes(y = ..density..), colour = 'red', stat = 'density', 
              size = 1.5, alpha = 0.6) + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(title = "Age of Houses at 2010", 
         y = "Proportion of Values", x=NULL)

library(plotly)
ames_train %>% 
    mutate(age_at_2010 = 2010 - Year.Built) %>%
    plot_ly(x = ~age_at_2010,
        type = "histogram",
        histnorm = "probability", 
        marker = list(
            color = "orange",
            line = list(
                color = 'grey',
                width = 1
            )
        )
    )
    
mean_age <- mean(house_ages$age_at_2010)
sd_age <- sd(house_ages$age_at_2010)
house_ages %>% 
    ggplot(aes(x=age_at_2010)) +
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
        args = list(mean = mean_age, sd = sd_age)) +
    geom_line(
        aes(y = ..density.., colour = "Observed"), 
        stat = 'density', 
        size = 1.5, 
        alpha = 0.8) + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
    scale_color_manual(name="Distribution", values=c(Normal="blue", Observed="red")) +
    labs(title = "Age of Houses at 2010", y = "Proportion of Values", x="Age (years)")

qqnorm(house_ages$age_at_2010, main='Normal')
qqline(house_ages$age_at_2010)
shapiro.test(house_ages$age_at_2010)
dispersion_test <- function(x) 
{
    res <- 1-2 * abs((1 - pchisq((sum((x - mean(x))^2)/mean(x)), length(x) - 1))-0.5)
    
    cat("Dispersion test of count data:\n",
        length(x), " data points.\n",
        "Mean: ",mean(x),"\n",
        "Variance: ",var(x),"\n",
        "Probability of being drawn from Poisson distribution: ", 
        round(res, 3),"\n", sep = "")
    
    invisible(res)
}
dispersion_test(house_ages$age_at_2010)

library(RColorBrewer)
nei_colours <- colorRampPalette(brewer.pal(4, "PuOr"))(27)

neighbourhood_price_variation <- ames_train %>% 
    group_by(Neighborhood) %>% 
    summarise(
        Mean = mean(price)/1e5, 
        Median = median(price)/1e5, 
        SD = sd(price)/1e5, 
        `H-Spread` = IQR(price)/1e5,
    ) %>%
    mutate(CV = Mean/Median) %>%
    arrange(desc(SD)) %>%
    mutate(Neighborhood = factor(Neighborhood, unique(Neighborhood))) %>%
    arrange(desc(Neighborhood))

neighbourhood_price <- ames_train %>% 
    select(Neighborhood, price) %>%
    mutate(Neighborhood = factor(Neighborhood, neighbourhood_price_variation$Neighborhood))
    
neighbourhood_price %>% 
    ggplot(aes(as.factor(Neighborhood), price/1e5), fill=Neighborhood) +
    theme_minimal() +
    geom_boxplot(aes(fill=Neighborhood)) +
    geom_violin(aes(colour=Neighborhood), width=1.5, alpha=0.3) + 
    coord_flip() +
    scale_color_manual(values = nei_colours) +
    theme(plot.title = element_text(hjust = 0.5), legend.position="none", plot.caption = element_text(face="italic")) +
    labs(
        title = "Distribution of House Prices by Neighbourhood", 
        caption = "Neighbourhoods listed in descending order of variance",
        x = NULL, 
        y="Price ($'000,000)"
        )


tbl <- neighbourhood_price_variation %>%
    pivot_longer(-Neighborhood,names_to = "what", values_to = "vals") %>%
    mutate(vals = round(vals,2)) %>%
    arrange(desc(Neighborhood)) %>%
    pivot_wider(names_from = "Neighborhood", values_from = "vals", names_sort = T)

tbl1 <- tbl %>% select(1:15) %>%
    kbl() %>% 
    kable_styling(bootstrap_options = c("condensed"), full_width = F)

tbl2 <- tbl %>% select(c(1,16:28)) %>%
    kbl() %>% 
    kable_styling(bootstrap_options = c("condensed"), full_width = F)



ames_train %>% 
    summarise(across(everything(), ~ sum(is.na(.x)))) %>%
    pivot_longer(everything(),names_to = "NAs") %>%
    slice_max(order_by = value, n=5)




df_base <- ames_train %>%
    select(
        price,
        Lot.Area,
        Land.Slope,
        Year.Built,
        Year.Remod.Add,
        Bedroom.AbvGr
        ) %>%
    mutate(price = log(price)) %>%
    drop_na()

library(BAS)

m_ames <- bas.lm(price ~ ., 
                   data = df_base,
                   prior = "ZS-null", 
                   modelprior = uniform()
)
m_ames
ames.BPM = predict(m_ames, estimator = "BPM", se.fit = TRUE)
ames.BPM$best.vars
ames.BPM.conf.fit = confint(ames.BPM, parm = "mean")
ames.BPM.conf.pred = confint(ames.BPM, parm = "pred")
head(cbind(ames.BPM$fit, ames.BPM.conf.fit, ames.BPM.conf.pred), 10)
BPM = as.vector(which.matrix(m_ames$which[ames.BPM$best],
                             m_ames$n.vars))
ames.BPM = bas.lm(price ~ ., data = df_base,
                    prior = "ZS-null",
                    modelprior = uniform(),
                    bestmodel = BPM, n.models = 1)
ames.BPM.coef <- coef(ames.BPM)
ames.BPM.coef
par(mfrow = c(2, 3))
plot(ames.BPM.coef, subset = c(2:7))

par(mfrow = c(1, 1))
plot(m_ames, which = 1, add.smooth = F, 
     ask = F, pch = 16, sub.caption="", caption="")

n <- nrow(df_base)
ames_k <- qnorm(0.5 + 0.5 * 0.95 ^ (1 / n))
ames.lm <- lm(price ~ ., data = df_base)
outliers <- Bayes.outlier(ames.lm, k = ames_k)
max(outliers$prob.outlier)
max_outlier <- ames_train[which.max(outliers$prob.outlier),]

max_outlier %>%
    select(PID, price, Year.Built, Year.Remod.Add, Overall.Cond, Exterior.1st, 
           Exter.Cond, Bsmt.Cond, BsmtFin.Type.1, Central.Air, Garage.Cond, Sale.Condition)


df_base <- ames_train %>%
    select(
        price,
        Lot.Area,
        Land.Slope,
        Year.Built,
        Year.Remod.Add,
        Bedroom.AbvGr
    ) %>%
    mutate(price = log(price)) %>%
    drop_na()

m_ames1 <- bas.lm(price ~ ., 
                  data = df_base,
                  prior = "ZS-null", 
                  modelprior = uniform()
)
m_ames1

ames.BPM1 = predict(m_ames1, estimator = "BPM", se.fit = TRUE)
ames.BPM1$best.vars

BPM1 = as.vector(
    which.matrix(
        m_ames1$which[ames.BPM1$best],
        m_ames1$n.vars)
)
ames.BPM1 = bas.lm(price ~ ., data = df_base,
                   prior = "ZS-null",
                   modelprior = uniform(),
                   bestmodel = BPM1, n.models = 1)
ames.BPM1.coef <- coef(ames.BPM1)

df_base <- ames_train %>%
    select(
        price,
        Lot.Area,
        Land.Slope,
        Year.Built,
        Year.Remod.Add,
        Bedroom.AbvGr
    ) %>%
    mutate(price = log(price), Lot.Area = log(Lot.Area)) %>%
    drop_na()

m_ames2 <- bas.lm(price ~ ., 
                  data = df_base,
                  prior = "ZS-null", 
                  modelprior = uniform()
)
m_ames2

ames.BPM2 = predict(m_ames2, estimator = "BPM", se.fit = TRUE)
ames.BPM2$best.vars

BPM2 = as.vector(
    which.matrix(
        m_ames2$which[ames.BPM2$best],
        m_ames2$n.vars)
)
ames.BPM2 = bas.lm(price ~ ., data = df_base,
                   prior = "ZS-null",
                   modelprior = uniform(),
                   bestmodel = BPM2, n.models = 1)
ames.BPM2.coef <- coef(ames.BPM2)

ames.g_prior1 = bas.lm(log(price) ~ Lot.Area + Year.Built + Year.Remod.Add + Bedroom.AbvGr + Land.Slope, 
                        data = ames_train,
                        alpha=13, 
                        prior="g-prior"
)

prices <- tibble(
        Observed = log(ames_train$price), 
        `Lot Area` = fitted(ames.BPM1), 
        `Log Lot Area` = fitted(ames.BPM2)
    ) %>% 
    pivot_longer(
        cols = c(`Lot Area`,`Log Lot Area`), 
        values_to = "Predicted", 
        names_to = "Model"
    ) %>%
    filter(Observed != min(Observed)) %>%
    mutate(Errors = Predicted - Observed) 

prices %>%
    ggplot(aes(x = Observed, y = Predicted, colour = Model)) +
    geom_point(alpha = 0.4) +
    geom_smooth(se = F, method = "lm", size = 1.2) +
    theme_minimal() +
    scale_color_manual(values = c("#80B1D3", "#B3DE69")) +
    geom_abline(intercept = 0, slope = 1, size = 1, colour = "#FB8072") +
    theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(face="italic")) +
    labs(
        title = "Observed vs Predicted House Prices for Lot Area & Log of Lot Area Models", 
        caption = "Red line represents actual values. Outlier 428 removed."
    )

prices %>%
    ggplot(aes(x = Observed, y = Errors, colour = Model)) +
    geom_point(alpha = 0.4) +
    geom_smooth(se = F, size = 1.2) +
    theme_minimal() +
    scale_color_manual(values = c("#80B1D3", "#B3DE69")) +
    geom_hline(yintercept = 0) +
    theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(face="italic")) +
    labs(
        title = "Errors of Predicted Values for Lot Area & Log of Lot Area Models", 
        caption = "Outlier 428 removed."
    )

prices %>% 
    group_by(Model) %>%
    mutate(Errors = abs(Errors)) %>%
    summarise(
        mean=round(mean(Errors),2),
        median=median(Errors), 
        sd=round(sd(Errors),2),
        max=max(Errors),
        q25=quantile(Errors, 0.25),
        q75=quantile(Errors,0.75),
        skew=round(skewness(Errors),2)) %>%
    kbl() %>% 
    kable_styling(
        bootstrap_options = c("condensed"),
        full_width = F,
        position = "float_left"
        )

prices_median <- median(prices$Observed)
prices %>% 
    mutate(correction = 
               (Predicted + 
                    ((1 - (!(Observed>prices_median))*2) * 
                         0.44*(Observed - prices_median)^2)) - 
               Observed)  %>%
    ggplot(aes(x = Observed, y = correction, colour = Model)) +
    geom_point(alpha = 0.4) +
    geom_smooth(se = F, size = 1.2) +
    theme_minimal() +
    scale_color_manual(values = c("#80B1D3", "#B3DE69")) +
    geom_hline(yintercept = 0) +
    theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(face="italic")) +
    labs(
        title = "Errors of Predicted Values for Lot Area & Log of Lot Area Models", 
        caption = "Outlier 428 removed."
    ) +
    ylab("Errors for Corrected Predicted Values")




