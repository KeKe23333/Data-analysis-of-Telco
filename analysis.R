Telco1 <- read.csv("K:/Master_Life/22T3/MARK5827/project/telecom_churn_28Oct2022.csv", header = TRUE)
names(Telco1)
summary(Telco1)

str(Telco1)


#  Identify the relevant indicator fo the time and the event of interest

#install.packages("survminer")
#install.packages("reshape2")
#install.packages("survival")
#install.packages("rms")
library("survival")
library("survminer")
library("reshape2")
library("car")
library("rms")

#  creat "survial object".

surv.obj <- Surv(Telco1$AccountWeeks,Telco1$Churn)

surv.obj[1:10]

surv.obj[3000:3332]

summary(surv.obj)


summary(Telco1$AccountWeeks)

attach(Telco1)


#Specify survival model

#specify a parametric survival mode(using the weibull diistribution).


# ============================== Task1 ===========================================

mod0 <- survreg(Surv(AccountWeeks, Churn) ~ 1, data = Telco1)  # empty model without any explanatory factors.
summary(mod0)  # summary is here

# plot curve here
surv <- seq(.99, .01, by = -.01)
t <- predict(mod0, type = "quantile", p = 1 - surv, newdata = data.frame(1)) 
surv.df <- data.frame(time = t, surv = surv)
head(surv.df)       # Look at first few lines of the result
surv.df2 <- data.frame(time = t, surv = surv,upper = NA, lower = NA, std.err = NA)
ggsurvplot_df(fit = surv.df2, surv.geom = geom_line, xlim=c(0,250), break.time.by = 50)

#  ============================== Task2 ===========================================

mod1 <- survreg(Surv(AccountWeeks, Churn) ~ DataPlan, data = Telco1)#model without one explanatory factors.
summary(mod1)

factor <- factor(c("0", "1"))
n.dat <- expand.grid(DataPlan = levels(factor))
n.dat

a1 <- predict(mod1, type = "quantile", p = 1-surv, newdata = data.frame(n.dat))
dim(a1)

a2<-cbind(n.dat, a1)

a3 <- melt(a2, id.vars=c("DataPlan"), variable.name="surv_id", value.name="time")
a3
a3$surv <- surv[as.numeric(a3$surv_id)]

ggsurvplot_df(a3, surv.geom = geom_line,
              linetype = "DataPlan", color="DataPlan", legend.title = NULL, xlim=c(0,250), break.time.by = 50)

#  ============================== Task3 ===========================================

mod2 <- survreg(Surv(AccountWeeks, Churn) ~ DataPlan + DataUsage + CustServCalls + DayMins + DayCalls+ MonthlyCharge + OverageFee+ RoamMins, data = Telco1)#model without one explanatory factors.
summary(mod2)

vif(mod2)
#  ============================== Task4 ===========================================

mod3 <- survreg(Surv(AccountWeeks, Churn) ~ DataPlan + DataUsage+ CustServCalls + DayCalls + MonthlyCharge + RoamMins, data = Telco1)#model without one explanatory factors.
summary(mod3)
vif(mod3)
# 
mod4 <- survreg(Surv(AccountWeeks, Churn) ~ DataPlan +  CustServCalls  + MonthlyCharge, data = Telco1)#model without one explanatory factors.
summary(mod4)
vif(mod4)
# 
# mod2
# vif(mod2)
# 
# #  ============================== Task4 ===========================================
# compare and get  final model
mod1_psm <- psm(Surv(AccountWeeks, Churn) ~ DataPlan,data = Telco1, dist="weibull")
mod1_psm

weeks <- seq(1,500, by = +1) 

# define the levels
#n.dat <- expand.grid(DataPlan = levels(DataPlan))  #  don't know why here levels show null. hence create specifiy levels
factor <- factor(c("0", "1"))
n.dat <- expand.grid(DataPlan = levels(factor))
n.dat

#gender <- factor(c("0", "1","0"));    测试 bug. ignore
#test <- expand.grid(gender = levels(gender))


# We ask the model for predictions for 500 weeks ahead.
b1<-survest(mod1_psm, newdata = data.frame(n.dat), time=weeks)
# We rearrange the data such that we can easily use it for the cash flow predictions.
b2<-cbind(n.dat, b1) 
b3<-melt(b2, id.vars=c("DataPlan"), variable.name="time", value.name="surv prob")
b3
write.csv(b3,"~/Downloods/result.csv", row.names = FALSE)



#ggsurvplot_df(b3, surv.geom = geom_line, linetype = "DataPlan", color = "DataPlan", legend.title = NULL, xlim=c(0,250), break.time.by = 10 )


detach(Telco1)
