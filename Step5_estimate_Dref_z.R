library(qpcR) # for RSS function
library(nlstools)
library(rgl)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(rsample)
library(broom)
library(purrr)
library(directlabels)
library(tidyr)



load(file="DATASET.RData")
data<-data.frame(Temperature=DATASET$Temperature, log10D=DATASET$log10D, Virus=DATASET$Virus, Virus_code=DATASET$Virus_code, Genus=DATASET$Genus)

# Full dataset


fig_all<-ggplot(data, aes(x=Temperature, y=log10D, color=Virus,shape=Genus)) + geom_point(size = 3)
#fig_all+theme_few() + scale_colour_few()
figure_all<-fig_all+theme_bw(base_size=15)+theme(axis.text=element_text(size=12))+xlab("Temperature (°C)") + ylab(bquote(~log[10]~'(D) (D in minutes)'))

figure_all

tiff(file="Figure_all.tiff",width = 7, height = 4, units = 'in',res=300)
figure_all
dev.off()

# Data obtained at low pH were excluded (just the effect of Temperature aims at being modeled)

data<-data[-c(7,8,9,10),]

fig_onlyT<-ggplot(data, aes(x=Temperature, y=log10D, color=Virus,shape=Genus)) + geom_point(size = 3)
figure_onlyT<-fig_onlyT+theme_bw(base_size=15)+theme(axis.text=element_text(size=12))+xlab("Temperature (°C)") + ylab(bquote(~log[10]~'(D) (D in minutes)'))



tiff(file="Figure_onlyT.tiff",width = 7, height = 4, units = 'in',res=300)
figure_onlyT
dev.off()

## 1.3 Prepare
set.seed(21350)
nboot<-1000
boots <- bootstraps(data, times = nboot)



poxvirus_model1<-function(Temp,Tref,log10Tref,zT)
{log10DT<-log10Tref-((Temp-Tref)/zT)
return(log10DT)
}


#### 2. Model #1 - Temperature only (classical Bigelow)

## 2.1 Simple nls fit  
fitmodel1<- nls(log10D~poxvirus_model1(Temperature,Tref=70,log10DTref,zT),
                start = list(log10DTref=2.5,zT=10),    
                upper=c(4,20),
                lower=c(0,0.01),
                data = data,
                algorithm="port",
                trace=TRUE)
RSS(fitmodel1)
fig2a<-ggplot(data, aes(Temperature, log10D)) + geom_point()+geom_line(aes(y = predict(fitmodel1)),size=1.2)
fig2a+theme_bw(base_size=15)+theme(axis.text=element_text(size=12))+xlab("Temperature (Â°C)") + ylab(bquote(~log[10]~'(D) (D in hours)'))


## 2.2 Bootstrap with nlsMicrobio package
fitmodel1.boot<-nlsBoot(fitmodel1,niter=nboot)

plot(fitmodel1.boot)
plot(fitmodel1.boot, type = "boxplot", ask = FALSE)
summary(fitmodel1.boot)

## 2.3 Bootstrap using broom package
fit_nls_on_bootstrap <- function(split) {
  nls(log10D~poxvirus_model1(Temperature,Tref=70,log10DTref,zT), analysis(split), start = list(log10DTref=3.5,zT=10))
}

boot_models <- boots %>% 
  mutate(model = map(splits, fit_nls_on_bootstrap),
         coef_info = map(model, tidy))

boot_coefs <- boot_models %>% 
  unnest(coef_info)

alpha <- .05
boot_coefs %>% 
  group_by(term) %>%
  summarize(low = quantile(estimate, alpha / 2),
            high = quantile(estimate, 1 - alpha / 2))

ggplot(boot_coefs, aes(estimate)) + 
  geom_histogram(binwidth = 0.25) + 
  facet_wrap(~ term, scales = "free")

boot_aug <- boot_models %>% 
  mutate(augmented = map(model, augment)) %>% 
  unnest(augmented)

tiff(file="Figure_fit.tiff",width = 5, height = 4, units = 'in',res=300)
ggplot(boot_aug, aes(Temperature, log10D)) +xlim(25,70)+ylim(-0.5,3.0)+
  geom_point() +  geom_line(aes(y = .fitted, group = id), col="grey", alpha=.2)+theme_bw(base_size=15)+theme(axis.text=element_text(size=12))+xlab("Temperature (°C)")+ ylab(bquote(~log[10]~'(D) (D in minutes)'))
dev.off()

