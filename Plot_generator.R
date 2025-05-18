#Generates all plots of the paper "An online generalization of the (e-)Benjamini-Hochberg procedure"

library(ggplot2)
library(patchwork)

###Figure 1

load("results/e-val_weak_signal.rda")

p1=ggplot(results_df, aes(pi_A)) + 
  geom_line(aes(y = power_online_e_bh, colour = "1")) + 
  geom_point(aes(y = power_online_e_bh, colour = "1", shape = "1")) +
  geom_line(aes(y = power_e_lond, colour = "2")) + 
  geom_point(aes(y = power_e_lond, colour = "2", shape = "2")) +
  scale_colour_manual(name = "Procedure", values=c("1"="skyblue", "2"="limegreen"), labels=c("1"="online e-BH", "2"="e-LOND"))+
  scale_shape_manual(name = "Procedure", values = c("1"=1, "2"=2), labels=c("1"="online e-BH", "2"="e-LOND"))+
  xlab("Proportion of false hypotheses")+
  ylab("Power")+
  scale_x_continuous(breaks = seq(0.2,0.8,0.2), limits=c(0.1,0.9),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1.2,0.2), limits=c(0,1),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=20), axis.title.x = element_text(size=20),
        legend.text=element_text(size=20), legend.title=element_text(size=20), strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15))

load("results/e-val_strong_signal.rda")

p2=ggplot(results_df, aes(pi_A)) + 
  geom_line(aes(y = power_online_e_bh, colour = "1")) + 
  geom_point(aes(y = power_online_e_bh, colour = "1", shape = "1")) +
  geom_line(aes(y = power_e_lond, colour = "2")) + 
  geom_point(aes(y = power_e_lond, colour = "2", shape = "2")) +
  scale_colour_manual(name = "Procedure", values=c("1"="skyblue", "2"="limegreen"), labels=c("1"="online e-BH", "2"="e-LOND"))+
  scale_shape_manual(name = "Procedure", values = c("1"=1, "2"=2), labels=c("1"="online e-BH", "2"="e-LOND"))+
  xlab("Proportion of false hypotheses")+
  ylab("Power")+
  scale_x_continuous(breaks = seq(0.2,0.8,0.2), limits=c(0.1,0.9),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1.2,0.2), limits=c(0,1),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=20), axis.title.x = element_text(size=20),
        legend.text=element_text(size=20), legend.title=element_text(size=20), strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15))

combined = p1 + p2  & theme(legend.position = "bottom")
combined = combined + plot_layout(guides = "collect")

ggsave("results/Plot_e_lond.pdf", plot=combined, width=12, height=4.5)

###Figure 2

load("results/e-val_strong_signal.rda")


p1=ggplot(results_df, aes(pi_A)) + 
  geom_line(aes(y = power_online_e_bh, colour = "1")) + 
  geom_point(aes(y = power_online_e_bh, colour = "1", shape = "1")) +
  geom_line(aes(y = power_online_e_bh_boosted, colour = "2")) + 
  geom_point(aes(y = power_online_e_bh_boosted, colour = "2", shape = "2")) +
  scale_colour_manual(name = "Online e-BH", values=c("1"="skyblue", "2"="cornflowerblue", "3"="magenta"), 
                      labels=c("1"="non-boosted", "2"="boosted", "3"="boosted under local dependence"))+
  scale_shape_manual(name = "Online e-BH", values = c("1"=1, "2"=6, "3"=8), 
                     labels=c("1"="non-boosted", "2"="boosted", "3"="boosted under local dependence"))+
  xlab("Proportion of false hypotheses")+
  ylab("Power")+
  scale_x_continuous(breaks = seq(0.2,0.8,0.2), limits=c(0.1,0.9),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1.2,0.2), limits=c(0,1),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=20), axis.title.x = element_text(size=20),
        legend.text=element_text(size=20), legend.title=element_text(size=20), strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15))


load("results/e-val_strong_signal.rda")

p2=ggplot(results_df, aes(pi_A)) + 
  geom_line(aes(y = power_online_e_bh, colour = "1")) + 
  geom_point(aes(y = power_online_e_bh, colour = "1", shape = "1")) +
  geom_line(aes(y = power_online_e_bh_boosted, colour = "2")) + 
  geom_point(aes(y = power_online_e_bh_boosted, colour = "2", shape = "2")) +
  scale_colour_manual(name = "Online e-BH", values=c("1"="skyblue", "2"="cornflowerblue", "3"="magenta"), 
                      labels=c("1"="non-boosted", "2"="boosted", "3"="boosted under local dependence"))+
  scale_shape_manual(name = "Online e-BH", values = c("1"=1, "2"=6, "3"=8), 
                     labels=c("1"="non-boosted", "2"="boosted", "3"="boosted under local dependence"))+
  xlab("Proportion of false hypotheses")+
  ylab("Power")+
  scale_x_continuous(breaks = seq(0.2,0.8,0.2), limits=c(0.1,0.9),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1.2,0.2), limits=c(0,1),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=20), axis.title.x = element_text(size=20),
        legend.text=element_text(size=20), legend.title=element_text(size=20), strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15))

combined = p1 + p2  & theme(legend.position = "bottom")
combined = combined + plot_layout(guides = "collect")

ggsave("results/Plot_boosted.pdf", plot=combined, width=12, height=4.5)


###Figure 3

load("results/e-val_weak_signal.rda")

p1=ggplot(results_df, aes(pi_A)) + 
  geom_line(aes(y = power_online_e_bh_boosted_local, colour = "3")) + 
  geom_point(aes(y = power_online_e_bh_boosted_local, colour = "3", shape = "3")) +
  geom_line(aes(y = power_online_e_bh_boosted, colour = "2")) + 
  geom_point(aes(y = power_online_e_bh_boosted, colour = "2", shape = "2")) +
  scale_colour_manual(name = "Online e-BH", values=c("1"="skyblue", "2"="cornflowerblue", "3"="magenta"), 
                      labels=c("1"="non-boosted", "2"="boosted (under arbitrary dependence)", "3"="boosted under local dependence"))+
  scale_shape_manual(name = "Online e-BH", values = c("1"=1, "2"=6, "3"=8), 
                     labels=c("1"="non-boosted", "2"="boosted (under arbitrary dependence)", "3"="boosted under local dependence"))+
  xlab("Proportion of false hypotheses")+
  ylab("Power")+
  scale_x_continuous(breaks = seq(0.2,0.8,0.2), limits=c(0.1,0.9),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1.2,0.2), limits=c(0,1),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=20), axis.title.x = element_text(size=20),
        legend.text=element_text(size=20), legend.title=element_text(size=20), strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15))


load("results/e-val_strong_signal.rda")

p2=ggplot(results_df, aes(pi_A)) + 
  geom_line(aes(y = power_online_e_bh_boosted_local, colour = "3")) + 
  geom_point(aes(y = power_online_e_bh_boosted_local, colour = "3", shape = "3")) +
  geom_line(aes(y = power_online_e_bh_boosted, colour = "2")) + 
  geom_point(aes(y = power_online_e_bh_boosted, colour = "2", shape = "2")) +
  scale_colour_manual(name = "Online e-BH", values=c("1"="skyblue", "2"="cornflowerblue", "3"="magenta"), 
                      labels=c("1"="non-boosted", "2"="boosted (under arbitrary dependence)", "3"="boosted under local dependence"))+
  scale_shape_manual(name = "Online e-BH", values = c("1"=1, "2"=6, "3"=8), 
                     labels=c("1"="non-boosted", "2"="boosted (under arbitrary dependence)", "3"="boosted under local dependence"))+
  xlab("Proportion of false hypotheses")+
  ylab("Power")+
  scale_x_continuous(breaks = seq(0.2,0.8,0.2), limits=c(0.1,0.9),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1.2,0.2), limits=c(0,1),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=20), axis.title.x = element_text(size=20),
        legend.text=element_text(size=20), legend.title=element_text(size=20), strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15))

combined = p1 + p2  & theme(legend.position = "bottom")
combined = combined + plot_layout(guides = "collect")

ggsave("results/Plot_boosted_local.pdf", plot=combined, width=12, height=4.5)


###Figure 4

load("results/p-val_small_q.rda")

p1=ggplot(results_df, aes(pi_A)) + 
  geom_line(aes(y = power_online_bh, colour = "2")) + 
  geom_point(aes(y = power_online_bh, colour = "2", shape = "2")) +
  geom_line(aes(y = power_lord, colour = "3")) + 
  geom_point(aes(y = power_lord, colour = "3", shape = "3")) +
  scale_colour_manual(name = "Procedure", values=c("2"="deepskyblue2", "3"="darkolivegreen3"), 
                      labels=c("2"="Online BH", "3"="LORD"))+
  scale_shape_manual(name = "Procedure", values = c("2"=0, "3"=1), 
                     labels=c("2"="Online BH", "3"="LORD"))+
  xlab("Proportion of false hypotheses")+
  ylab("Power")+
  scale_x_continuous(breaks = seq(0.2,0.8,0.2), limits=c(0.1,0.9),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1.2,0.2), limits=c(0,1),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=20), axis.title.x = element_text(size=20),
        legend.text=element_text(size=20), legend.title=element_text(size=20), strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15))


load("results/p-val_large_q.rda")

p2=ggplot(results_df, aes(pi_A)) + 
  geom_line(aes(y = power_online_bh, colour = "2")) + 
  geom_point(aes(y = power_online_bh, colour = "2", shape = "2")) +
  geom_line(aes(y = power_lord, colour = "3")) + 
  geom_point(aes(y = power_lord, colour = "3", shape = "3")) +
  scale_colour_manual(name = "Procedure", values=c("2"="deepskyblue2", "3"="darkolivegreen3"), 
                      labels=c("2"="Online BH", "3"="LORD"))+
  scale_shape_manual(name = "Procedure", values = c("2"=0, "3"=1), 
                     labels=c("2"="Online BH", "3"="LORD"))+
  xlab("Proportion of false hypotheses")+
  ylab("Power")+
  scale_x_continuous(breaks = seq(0.2,0.8,0.2), limits=c(0.1,0.9),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1.2,0.2), limits=c(0,1),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=20), axis.title.x = element_text(size=20),
        legend.text=element_text(size=20), legend.title=element_text(size=20), strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15))

combined = p1 + p2  & theme(legend.position = "bottom")
combined = combined + plot_layout(guides = "collect")

ggsave("results/Plot_LORD.pdf", plot=combined, width=12, height=4.5)


###Figure 5

load("results/p-val_small_q.rda")

p1=ggplot(results_df, aes(pi_A)) + 
  geom_line(aes(y = power_online_storey_bh, colour = "2")) + 
  geom_point(aes(y = power_online_storey_bh, colour = "2", shape = "2")) +
  geom_line(aes(y = power_saffron, colour = "3")) + 
  geom_point(aes(y = power_saffron, colour = "3", shape = "3")) +
  scale_colour_manual(name = "Procedure", values=c("2"="darkturquoise", "3"="chartreuse3"), 
                      labels=c("2"="Online Storey-BH", "3"="SAFFRON"))+
  scale_shape_manual(name = "Procedure", values = c("2"=0, "3"=1), 
                     labels=c("2"="Online Storey-BH", "3"="SAFFRON"))+
  xlab("Proportion of false hypotheses")+
  ylab("Power")+
  scale_x_continuous(breaks = seq(0.2,0.8,0.2), limits=c(0.1,0.9),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1.2,0.2), limits=c(0,1),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=20), axis.title.x = element_text(size=20),
        legend.text=element_text(size=20), legend.title=element_text(size=20), strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15))


load("results/p-val_large_q.rda")

p2=ggplot(results_df, aes(pi_A)) + 
  geom_line(aes(y = power_online_storey_bh, colour = "2")) + 
  geom_point(aes(y = power_online_storey_bh, colour = "2", shape = "2")) +
  geom_line(aes(y = power_saffron, colour = "3")) + 
  geom_point(aes(y = power_saffron, colour = "3", shape = "3")) +
  scale_colour_manual(name = "Procedure", values=c("2"="darkturquoise", "3"="chartreuse3"), 
                      labels=c("2"="Online Storey-BH", "3"="SAFFRON"))+
  scale_shape_manual(name = "Procedure", values = c("2"=0, "3"=1), 
                     labels=c("2"="Online Storey-BH", "3"="SAFFRON"))+
  xlab("Proportion of false hypotheses")+
  ylab("Power")+
  scale_x_continuous(breaks = seq(0.2,0.8,0.2), limits=c(0.1,0.9),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1.2,0.2), limits=c(0,1),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=20), axis.title.x = element_text(size=20),
        legend.text=element_text(size=20), legend.title=element_text(size=20), strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15))

combined = p1 + p2  & theme(legend.position = "bottom")
combined = combined + plot_layout(guides = "collect")

ggsave("results/Plot_SAFFRON.pdf", plot=combined, width=12, height=4.5)


