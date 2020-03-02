library(pacman)
p_load(tidyverse,lpirfs)
coltypes<-cols(.default = col_double(),date = col_datetime(format = ""))

#load data
#Endogen data
#industryproduction, HICP_sl, Uncertainty, DE5Y
data<-read_csv("data.csv", col_types = coltypes) %>% 
  select(-date)
# Conventional and Unconventional monetary surprises from https://doi.org/10.1016/j.jmoneco.2019.08.016
iv<-read_csv("iv.csv", col_types = coltypes) %>% 
  select(-date)

#lp settings
irf_conf<-1.65
irf_steps<-24 
var_lag_order<-4

#state dependent lp (normalized with shock_type = 1)
nl_basic<-lp_nl(endog_data=data,
          trend=0,use_hp=F,shock_type = 1,confint = irf_conf,hor=irf_steps,
          switching = data$DE5Y,gamma=4,lags_endog_nl=var_lag_order,lags_endog_lin=var_lag_order)

#linear lp with iv (two step approach)
linear_iv_twosls_Conv<-lp_lin_iv(endog_data=data,
                     shock = data[,"DE5Y"],
                     instrum = iv[,"Conventional"],
                     lags_endog_lin=var_lag_order,trend=0,confint = irf_conf,hor=irf_steps,use_twosls=T,use_nw=T,adjust_se=T)

linear_iv_twosls_Unconv<-lp_lin_iv(endog_data=data,
                                 shock = data[,"DE5Y"],
                                 instrum = iv[,"Unconventional"],
                                 lags_endog_lin=var_lag_order,trend=0,confint = irf_conf,hor=irf_steps,use_twosls=T,use_nw=T,adjust_se=T)

nl_iv_Conventional<-lp_nl_iv(endog_data=data,
                shock = iv %>% select(Conventional),
                trend=0,use_hp=F,confint = irf_conf,hor=irf_steps,use_nw=T,adjust_se=T,switching = data_f$ciss,
                gamma=4, use_logistic = TRUE,lags_endog_nl=var_lag_order)

nl_iv_Unconventional<-lp_nl_iv(endog_data=data,
                             shock = iv %>% select(Unconventional),
                             trend=0,use_hp=F,confint = irf_conf,hor=irf_steps,use_nw=T,adjust_se=T,switching = data_f$ciss,
                             gamma=4, use_logistic = TRUE,lags_endog_nl=var_lag_order)

#Ich besitze zwei Reihen an Geldpolitikschocks, wo ich gerne vergleichen würde ob sie in verschiedenen Zuständen
#(Bspw. Unsicherheit) verschiedene Effekte auf Makrovariablen haben. Die beiden Schocks sind orthogonal zueinander.

#Beim twosls Ansatz erhalte ich Impulse responses, die jeweils darstellen wie sich 
# a)
plot(linear_iv_twosls_Conv)
#ein Conventioneller Schock, welcher DE5Y um 1% erhöht auf die anderen Variablen auswirkt

# b)
plot(linear_iv_twosls_Unconv)
#ein Unconventioneller Schock, welcher DE5Y um 1% erhöht auf die anderen Variablen auswirkt

# Schätze ich das ganze Zustandsabhängig, dann ist es wesentlich schwieriger die Einzelnen Impulse responses zu vergleichen. 
plot(nl_iv_Conventional)
plot(nl_iv_Unconventional)
#gerne würde ich das Muster von oben ("Ein Schock, welcher DE5Y um 1% erhöht...") auf diesen Fall übertragen
#Im Fall ohne IV ist dies sehr gut möglich:
plot(nl_basic)

#Angedachte Lösung:
#Kann ich das Problem lösen indem ich den "two step approach" dem ganzen vorrausstelle:

#berechnung der "korrigierten" DE5Y Werte:
DE_Conv<-lm(data$DE5Y~iv$Conventional)$fitted
DE_Unconv<-lm(data$DE5Y~iv$Unconventional)$fitted

data_Conv<-data %>% select(-DE5Y) %>% 
  mutate(DE_Conv=DE_Conv)
data_Unconv<-data %>% select(-DE5Y) %>% 
  mutate(DE_Unconv=DE_Unconv)

tsa_conv<-lp_nl_iv(endog_data=data_Conv,
         shock = data_Conv[,"DE_Conv"],
         trend=0,
         use_hp=F,
         confint = irf_conf,
         hor=irf_steps,
         use_nw=T,
         adjust_se=T,
         switching = data_Conv$ciss,
         gamma=4, use_logistic = TRUE,
         lags_endog_nl=var_lag_order)

tsa_unconv<-lp_nl_iv(endog_data=data_Unconv,
              shock = data_Unconv[,"DE_Unconv"],
              trend=0,
              use_hp=F,
              confint = irf_conf,
              hor=irf_steps,
              use_nw=T,
              adjust_se=T,
              switching = data_Unconv$ciss,
              gamma=4, use_logistic = TRUE,
              lags_endog_nl=var_lag_order)

#jetzt sind die Schocks bei 1
tsa_conv$irf_s1_mean
tsa_conv$irf_s2_mean
tsa_unconv$irf_s1_mean
tsa_unconv$irf_s2_mean

plot(tsa_conv)
plot(tsa_unconv)

