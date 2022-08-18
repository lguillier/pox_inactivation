library(ggdist)

theme_set(theme_ggdist())

load(file="fitted_parameters.RData")

# Fitted parameters are correlated: important to keep estimates associated for prediction
plot(parameters_f$log10Dref,parameters_f$z)

# Batejat's experimental setting
N0=log10(3.4*10^7) # Virus initial load in medium
LOD=log10(25) # Limit of detection in PFU = 25

max_red<-N0-LOD # Maximal log10 reduction that can be observed in Batejat's paper


# Inactivation prediction 56°C - 30 minutes
T<-56
time<-30
red_56_30=time/10^(parameters_f$log10Dref-((T-70)/parameters_f$z))
hist(red_56_30)
pred_56_30<-N0-red_56_30

# Inactivation prediction 60°C - 15 minutes
T<-60
time<-15
red_60_15=time/10^(parameters_f$log10Dref-((T-70)/parameters_f$z))
hist(red_60_15)
pred_60_15<-N0-red_60_15

# Inactivation prediction 70°C - 5 minutes
T<-70
time<-5
red_70_5=time/10^(parameters_f$log10Dref-((T-70)/parameters_f$z))
hist(red_70_5)

pred_70_5<-N0-red_70_5



dist_df = data.frame(
  group = c(rep("a",(length(pred_56_30))),rep("b",(length(pred_60_15))),rep("c",(length(pred_70_5)))),
  red = c(pred_56_30,pred_60_15,pred_70_5)
)


dist_df %>%
  ggplot(aes(
    x = "Conditions", y = red, fill = group, color = group
  )) +
  stat_dist_halfeye(position = "dodge", color = "black")+
  geom_point(
    aes(y = c(1.4,1,2)),
    # use position_dodgejust here to match the justification
    # of the point to the halfeye when dodging
    position = position_dodgejust(width = 1, justification = 0),
    shape = 1, size = 4, stroke = 1.5
  ) 




dist_df1 = data.frame(red = pred_56_30)


p1<-dist_df1 %>%
  ggplot(aes(
    x = " ", y = red  )) +
  stat_dist_halfeye(position = "dodge", color = "black")+
  geom_point(
    aes(y = c(1.4)),
    # use position_dodgejust here to match the justification
    # of the point to the halfeye when dodging
    position = position_dodgejust(width = 1, justification = 0),
    shape = 0, size = 4, stroke = 1.5
  ) +
  geom_point(
    aes(y = c(1.4)),
    # use position_dodgejust here to match the justification
    # of the point to the halfeye when dodging
    position = position_dodgejust(width = 1, justification = 0),
    shape = 2, size = 4, stroke = 1.5
  ) +
  geom_point(
  aes(y = log10(61)),
  # use position_dodgejust here to match the justification
  # of the point to the halfeye when dodging
  position = position_dodgejust(width = 1, justification = 0),
  shape = 15, size = 4, stroke = 1.5
) +
  geom_point(
    aes(y = log10(2500)),
    # use position_dodgejust here to match the justification
    # of the point to the halfeye when dodging
    position = position_dodgejust(width = 1, justification = 0),
    shape = 17, size = 4, stroke = 1.5
  ) +
  annotate("text", x = 0.77 , y = 1.8, label = "LK strain in VTM")+
  annotate("text", x = 0.72 , y = 1.4, label = "LK and CMIP strains in FCS")+
  annotate("text", x = 0.77 , y = 3.4, label = "CMIP strain in VTM")+
  xlab('Temperature = 56°C - 30 minutes ')+ylab('Final Monkeypox virus titer (log10 PFU/ml)')+ylim(c(-2,6))+theme(axis.line.y = element_line(color = "black"),axis.text.y = element_text(color = "black"))



dist_df2 = data.frame(red = pred_60_15)


p2<-dist_df2 %>%
  ggplot(aes(
    x = " ", y = red  )) +
  stat_dist_halfeye(position = "dodge", color = "black")+
  geom_point(
    aes(y = c(1.4)),
    # use position_dodgejust here to match the justification
    # of the point to the halfeye when dodging
    position = position_dodgejust(width = 1, justification = 1),
    shape = 0, size = 4, stroke = 1.5
  ) +
  geom_point(
    aes(y = c(1.4)),
    # use position_dodgejust here to match the justification
    # of the point to the halfeye when dodging
    position = position_dodgejust(width = 1, justification = 0),
    shape = 2, size = 4, stroke = 1.5
  ) +
  annotate("text", x = 0.5 , y = 1.4, label = "LK and CMIP strains in FCS or VTM")+
  xlab('Temperature = 60°C - 15 minutes ')+ylab('Final Monkeypox virus titer (log10 PFU/ml)')+ylim(c(-2,6))+theme(axis.line.y = element_line(color = "black"),axis.text.y = element_text(color = "black"))


p3<-dist_df3 %>%
  ggplot(aes(
    x = " ", y = red  )) +
  stat_dist_halfeye(position = "dodge", color = "black")+
  geom_point(
    aes(y = c(1.4)),
    # use position_dodgejust here to match the justification
    # of the point to the halfeye when dodging
    position = position_dodgejust(width = 1, justification = 1),
    shape = 0, size = 4, stroke = 1.5
  ) +
  geom_point(
    aes(y = c(1.4)),
    # use position_dodgejust here to match the justification
    # of the point to the halfeye when dodging
    position = position_dodgejust(width = 1, justification = 0),
    shape = 2, size = 4, stroke = 1.5
  ) +
  annotate("text", x = 0.5 , y = 1.4, label = "LK and CMIP strains in FCS or VTM")+
  xlab('Temperature = 60°C - 15 minutes ')+ylab('Final Monkeypox virus titer (log10 PFU/ml)')+ylim(c(-2,6))+theme(axis.line.y = element_line(color = "black"),axis.text.y = element_text(color = "black"))

