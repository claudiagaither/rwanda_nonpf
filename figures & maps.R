#Rwanda non-pf maps
#libraries obvi
library(ggplot2)
library(ggpubr)
library(readxl)
library(survey)
library(writexl)
library(sf)
library(rgdal)
library(viridis)
library(maps)
library(cartography)
library(tidyverse)
library(forcats)


#all species parasitemia histogram
svy_quant<-read_excel("C:/Users/cgait/OneDrive/Desktop/map & figure datasets/svy_quant.xlsx")
rw_hist<-ggplot(data=svy_quant) + geom_histogram(aes(x=quantity, fill=species), bins=40, alpha=0.5) + 
  scale_fill_manual(values=c('pink', 'skyblue2', 'darkgoldenrod1', 'darkgreen'))+ xlim(-5,5)+
  theme(legend.key.size = unit(0.5, 'cm'), 
      legend.key.height = unit(0.5, 'cm'), 
      legend.key.width = unit(0.5, 'cm'), 
      legend.title = element_text(size=14), 
      legend.text = element_text(size=14),
      axis.text.x = element_text(size=12),
      axis.text.y = element_text(size=12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      panel.background = element_blank())
print(rw_hist)
options(scipen = 999)
rw_hist + labs(x="Parasitemia (log scale)", y="Count")+
  scale_x_log10(breaks = c(0.01, 0.10, 1.00, 10, 100, 1000, 10000), minor_breaks = NULL) +theme_test(base_size = 14)

ggsave("histogram.png")

#species categories histograms
#pf_catcolors<-c('pink','magenta','darkmagenta','purple','orange') 
#ggplot(data=pf_quant) + geom_histogram(aes(x=log_parasitemia, fill=pf_cat), alpha=0.5)+ 
#  scale_fill_manual(values=pf_catcolors)
  
#po_catcolors<-c('deepskyblue','orange','green','yellow')
#ggplot(data=po_quant) + geom_histogram(aes(x=log_parasitemia, fill=po_cat), alpha=0.5)+ 
#  scale_fill_manual(values=po_catcolors)

#pv_catcolors<-c('deepskyblue2','aquamarine','darkgreen')
#ggplot(data=pv_quant) + geom_histogram(aes(x=log_parasitemia, fill=pv_cat), alpha=0.5)+ 
#  scale_fill_manual(values=pv_catcolors)

#pm_catcolors<-c('mediumpurple1','violetred3','darkblue','deepskyblue','seagreen2','turquoise4')
#ggplot(data=pm_quant) + geom_histogram(aes(x=log_parasitemia, fill=pm_cat), alpha=0.5)+ 
#  scale_fill_manual(values=pm_catcolors)


#prevalence maps
#https://data.humdata.org/dataset/rwanda-water-bodies

#shapefiles & datasets for maps
svy_pbp <- read_excel("C:/Users/cgait/OneDrive/Desktop/map & figure datasets/svy_pbp.xlsx") 
DHS_cluster <- read_excel("C:/Users/cgait/OneDrive/Desktop/Rwanda nonpf/rw_nonpf data/summary datasets/DHS_cluster.xlsx")
africa_sf<-read_sf("C:/Users/cgait/OneDrive/Desktop/Rwanda nonpf/rw_nonpf data/data downloads/Africa_Boundaries-shp/Africa_Boundaries.shp")
rw_bounds<-read_sf("C:/Users/cgait/OneDrive/Desktop/Rwanda nonpf/rw_nonpf data/data downloads/shapefiles/rw2015_boundary/RWA_adm0.shp")
lake<-read_sf("C:/Users/cgait/OneDrive/Desktop/Rwanda nonpf/rw_nonpf data/data downloads/shapefiles/rwalakes/RWA_Lakes_NISR.shp")
district<-read_sf("C:/Users/cgait/OneDrive/Desktop/Rwanda nonpf/rw_nonpf data/data downloads/shapefiles/rwa_district/District.shp")
svy_district <- merge(district, svy_pbp, by='District')

#background map
bur<-subset(africa_sf, NAME_0=="Burundi")
drc<-subset(africa_sf, NAME_0=="Democratic Republic of the Congo")
tzn<-subset(africa_sf, NAME_0=="Tanzania")
uga<-subset(africa_sf, NAME_0=="Uganda")
bounds<-rbind(bur, drc, tzn, uga)

#ggplot()+geom_sf(data=bounds)+coord_sf(xlim = c(28.5, 31.3), ylim = c(-0.7, -3.2))

#prevalence maps with all CT values
malaria_pr<-ggplot() + geom_sf(data=bounds)+ geom_sf(data=lake, fill="lightblue")+ geom_sf(data=svy_district, aes(fill=malaria_prev)) + 
  scale_fill_viridis(option='plasma') + coord_sf(xlim = c(28.8, 31.0), ylim = c(-0.95, -3.0))+
  theme(legend.key.size = unit(0.5, 'cm'), 
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'), 
        legend.title = element_text(size=16), 
        legend.text = element_text(size=14),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) + labs(fill ="Malaria prevalence")
print(malaria_pr)
ggsave("malaria_prev.png")  

pf_pr<-ggplot() + geom_sf(data=bounds) + geom_sf(data=lake, fill="lightblue")+ geom_sf(data=svy_district, aes(fill=pf_prev))+ 
  coord_sf(xlim = c(28.8, 31.0), ylim = c(-0.95, -3.0))+
  theme(legend.key.size = unit(0.4, 'cm'), 
        legend.key.height = unit(0.4, 'cm'), 
        legend.key.width = unit(0.4, 'cm'), 
        legend.title = element_text(size=11), 
        legend.text = element_text(size=11),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) + labs(fill= expression(italic("P. falciparum")~"prevalence"))
pf_pr<-pf_pr+scale_fill_gradientn(colours = c("white","pink1","pink3","pink4"))
print(pf_pr)
#ggsave("pf_prev.png")

pm_pr<-ggplot() + geom_sf(data=bounds) + geom_sf(data=lake, fill="lightblue")+ geom_sf(data=svy_district, aes(fill=pm_prev))+ 
  coord_sf(xlim = c(28.8, 31.0), ylim = c(-0.95, -3.0))+
  theme(legend.key.size = unit(0.4, 'cm'), 
        legend.key.height = unit(0.4, 'cm'), 
        legend.key.width = unit(0.4, 'cm'), 
        legend.title = element_text(size=11), 
        legend.text = element_text(size=11),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) + labs(fill = expression(italic("P. malariae")~"prevalence")) 
pm_pr<-pm_pr+scale_fill_gradientn(colours = c("white","lightskyblue1","lightskyblue3","lightskyblue4"))
print(pm_pr)
#ggsave("pm_prev.png")

po_pr<-ggplot() + geom_sf(data=bounds) + geom_sf(data=lake, fill="lightblue")+ geom_sf(data=svy_district, aes(fill=po_prev))+ 
  coord_sf(xlim = c(28.8, 31.0), ylim = c(-0.95, -3.0))+
  theme(legend.key.size = unit(0.4, 'cm'), 
        legend.key.height = unit(0.4, 'cm'), 
        legend.key.width = unit(0.4, 'cm'), 
        legend.title = element_text(size=11), 
        legend.text = element_text(size=11),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) + labs(fill = expression(italic("P. ovale")~"prevalence")) 
po_pr<-po_pr+scale_fill_gradientn(colours = c("white","yellow","gold","goldenrod3"))
print(po_pr)
#ggsave("po_prev.png")

pv_pr<-ggplot() + geom_sf(data=bounds) + geom_sf(data=lake, fill="lightblue")+ geom_sf(data=svy_district, aes(fill=pv_prev))+ 
  coord_sf(xlim = c(28.8, 31.0), ylim = c(-0.95, -3.0))+
  theme(legend.key.size = unit(0.4, 'cm'), 
        legend.key.height = unit(0.4, 'cm'), 
        legend.key.width = unit(0.4, 'cm'), 
        legend.title = element_text(size=11), 
        legend.text = element_text(size=11),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) + labs(fill = expression(italic("P. vivax")~"prevalence")) 
pv_pr<-pv_pr+scale_fill_gradientn(colours = c("white","darkolivegreen1","darkolivegreen3","darkolivegreen4"))
print(pv_pr)
#ggsave("pv_prev.png")

ggarrange(pf_pr, pm_pr, po_pr, pv_pr, nrow=2, ncol=2, labels=c("A","B","C","D"))
ggsave("prevalences.png")

ggarrange(pf_pr, po_pr, ncol=2)
ggsave("pf_po_prev.png")

#cluster maps
#low & highpf clusters
cluster_colors<-c('maroon4','turquoise')
shapes<-c(19,1)
ggplot() + geom_sf(data=bounds) + geom_sf(data=lake, fill="lightblue")+
  geom_point(data = DHS_cluster, aes(x=long, y=lat, color=trans_intens, shape=svy_cluster), size=2, alpha=0.5) +
  coord_sf(xlim = c(28.8, 31.0), ylim = c(-0.95, -3.0))+ scale_shape_manual(values=shapes)+
  theme(legend.key.size = unit(0.6, 'cm'), 
        legend.key.height = unit(0.6, 'cm'), 
        legend.key.width = unit(0.5, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(x="",y="",shape="Inclusion in Analysis", color="Transmission Intensity")
ggsave("rw15_clusters.png")
  
#shapes and colors for each species map
pf_shape<-c(16,15,17,17,15)
po_shape<-c(17,15,15,16)
pv_shape<-c(16,17,18)
pm_shape<-c(15,17,17,16,18,18)
full_shape_set<- c(16,16,16,16,15,17,17,15,18,18)

pf_colors <- c('pink', 'maroon1', 'darkmagenta', 'darkblue', 'darkorange1')
po_colors <- c('darkmagenta', 'darkorange1', 'deeppink','darkgoldenrod1')
pv_colors <- c('darkgreen', 'darkblue', 'aquamarine3')
pm_colors<-c("aquamarine","darkmagenta","darkblue","deepskyblue","deeppink", "purple")
colors <- c('pink','deepskyblue','gold','forestgreen','darkorchid1','blue','black','darkorange1','maroon1','maroon4')

#P. falciparum clusters
pf_points<-ggplot()+geom_sf(data=bounds) + geom_sf(data=lake, fill="lightblue")+
  geom_point(data = pf_pos, aes(x=long, y=lat,color=species,shape=species), 
  size = 4, alpha = .45) + scale_shape_manual(values=pf_shape, name="Infection", 
  labels=c("P. falciparum","P. falciparum & P. malariae","P. falciparum, P. malariae & P. ovale", 
  "P. falciparum, P. malariae & P. vivax","P. falciparum & P. ovale")) + 
  scale_color_manual(values=pf_colors, name="Infection", 
  labels=c("P. falciparum","P. falciparum & P. malariae","P. falciparum, P. malariae & P. ovale", 
  "P. falciparum, P. malariae & P. vivax","P. falciparum & P. ovale"))+
  coord_sf(xlim = c(28.8, 31.0), ylim = c(-0.95, -3.0)) +
  theme(legend.key.size = unit(0.4, 'cm'), 
        legend.key.height = unit(0.4, 'cm'), 
        legend.key.width = unit(0.4, 'cm'), 
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + labs(x="", y="")

print(pf_points)
#ggsave("pf_cluster.png")

#P. malariae clusters
pm_points<-ggplot()+geom_sf(data=bounds) + geom_sf(data=lake, fill="lightblue")+
  geom_point(data = pm_pos, aes(x=long, y=lat, color=species, shape=species), 
  size = 4, alpha = .45) + scale_shape_manual(values=pm_shape, name="Infection", 
  labels=c("P. falciparum & P. malariae","P. falciparum, P. malariae & P. ovale","P. falciparum, P. malariae & P. viavax", 
  "P. malariae","P. malariae & P. ovale", "P. malariae & P. vivax")) + 
  scale_color_manual(values=pm_colors, name="Infection", 
  labels=c("P. falciparum & P. malariae","P. falciparum, P. malariae & P. ovale","P. falciparum, P. malariae & P. viavax", 
  "P. malariae","P. malariae & P. ovale", "P. malariae & P. vivax")) +
  coord_sf(xlim = c(28.8, 31.0), ylim = c(-0.95, -3.0)) +
  theme(legend.key.size = unit(0.4, 'cm'), 
        legend.key.height = unit(0.4, 'cm'), 
        legend.key.width = unit(0.4, 'cm'), 
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + labs(x="", y="")
print(pm_points)
#ggsave("pm_cluster.png")

#P. ovale clusters
po_points<-ggplot()+geom_sf(data=bounds) + geom_sf(data=lake, fill="lightblue")+
  geom_point(data = po_pos, aes(x=long, y=lat, color=species, shape=species), 
  size = 4, alpha = .45) + scale_shape_manual(values=po_shape, name="Infection", 
  labels=c("P. falciparum, P. malariae & P. ovale","P. falciparum & P. ovale","P. malariae & P. ovale","P. ovale")) + 
  scale_color_manual(values=po_colors, name="Infection", 
  labels=c("P. falciparum, P. malariae & P. ovale","P. falciparum & P. ovale","P. malariae & P. ovale","P. ovale")) +
  coord_sf(xlim = c(28.8, 31.0), ylim = c(-0.95, -3.0)) +
  theme(legend.key.size = unit(0.4, 'cm'), 
        legend.key.height = unit(0.4, 'cm'), 
        legend.key.width = unit(0.4, 'cm'), 
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + labs(x="", y="")
print(po_points)
#ggsave("po_cluster.png")

#P. vivax clusters
pv_points<-ggplot()+geom_sf(data=bounds) + geom_sf(data=lake, fill="lightblue")+
  geom_point(data = pv_pos, aes(x=long, y=lat, color=species, shape=species), 
  size = 4, alpha = .45) + scale_shape_manual(values=pv_shape, name="Infection", 
  labels=c("P. falciparum, P. malariae & P. vivax","P. malariae & P. vivax","P. vivax")) + 
  scale_color_manual(values=pv_colors, name="Infection",
  labels=c("P. falciparum, P. malariae & P. vivax","P. malariae & P. vivax","P. vivax")) +
  coord_sf(xlim = c(28.8, 31.0), ylim = c(-0.95, -3.0)) +
  theme(legend.key.size = unit(0.4, 'cm'), 
        legend.key.height = unit(0.4, 'cm'), 
        legend.key.width = unit(0.4, 'cm'), 
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + labs(x="", y="")
print(pv_points)
#ggsave("pv_cluster.png")

ggarrange(pf_points, pm_points, po_points, pv_points, nrow=2, ncol=2, labels=c("A","B","C","D"))
ggsave("species_clusters.png")


#forest plots----
all_forest <- read_excel("C:/Users/cgait/OneDrive/Desktop/map & figure datasets/forest.xlsx")
#pf_strat <- read_excel("forest.xlsx", sheet = "pf_strat")
#pf_only <-read_excel("forest.xlsx", sheet = "falciparum")
#pm_only <-read_excel("forest.xlsx", sheet = "malariae")
#po_only <-read_excel("forest.xlsx", sheet = "ovale")

#plot colors
colors<-c("hotpink1","royalblue","goldenrod2")

all_forest$species<-factor(all_forest$species, levels=c("falciparum","malariae","ovale"))

#all samples plot
ggplot() + geom_hline(yintercept=0, linetype='dashed')+
  geom_pointrange(data=all_forest, aes(x=term, y=estimate, ymin=CIL_95, ymax=CIU_95, 
                                       color=species),shape=15, size=1,position=position_dodge2(width=0.9), fatten=0.1)+
  scale_color_manual(values=colors)+
  geom_point(data=all_forest, aes(x=term,y=estimate, color=species),
             shape=19, size=2, position=position_dodge2(width = 1.0),alpha=0.9)+
  coord_flip()+
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14),
        plot.caption = element_text(size=14))+
  labs(y="Prevalence Differences and CIs", x="") + theme_test(base_size = 16)
  #scale_fill_discrete(breaks="ovale","malariae","falciparum")

ggsave("all_species.png")

#single species forest plots
#falciparum
ggplot() + geom_hline(yintercept = 0, linetype='dashed')+
  coord_flip(expand=TRUE)+
  geom_pointrange(data=pf_only, aes(x=term, y=estimate, ymin=CIL_95, ymax=CIU_95, 
                                    color=species), shape=15, size=1, position=position_dodge2(width = 0.9), fatten=0.1)+
  
  geom_point(data=pf_only, aes(x=term, y=estimate, color=species), shape=19, size=2)+
  labs(y="Prevalence Differences and Confidence Intervals for Falciparum Infection")

#ovale
ggplot() + geom_hline(yintercept = 0, linetype='dashed')+
  coord_flip(expand=TRUE)+
  geom_pointrange(data=po_only, aes(x=term, y=estimate, ymin=CIL_95, ymax=CIU_95,
                                    color=species),shape=15, size=1, position=position_dodge2(width = 0.9), fatten=0.1)+
  
  geom_point(data=po_only, aes(x=term, y=estimate, color=species), shape=19, size=2)+
  labs(y="Prevalence Differences and Confidence Intervals for Ovale Infection")

#malariae
ggplot() + geom_hline(yintercept = 0, linetype='dashed')+
  coord_flip(expand=TRUE)+
  geom_pointrange(data=pm_only, aes(x=term, y=estimate, ymin=CIL_95, ymax=CIU_95,
                                    color=species),shape=15, size=1, position=position_dodge2(width = 0.9), fatten=0.1)+
  
  geom_point(data=pm_only, aes(x=term, y=estimate, color=species), shape=19, size=2)+
  labs(y="Prevalence Differences and Confidence Intervals for Malariae Infection")

#stratified by cluster intensity
#P. falciparum trans_int stratified plot
ggplot() + geom_hline(yintercept=0, linetype='dashed')+
  geom_pointrange(data=pf_strat, aes(x=term, y=estimate, ymin=CIL_95, ymax=CIU_95, color=trans_int),
                  shape=15, size=1,position=position_dodge2(width=0.9), fatten=0.1) +
  
  geom_point(data=pf_strat, aes(x=term,y=estimate, color=trans_int),
             shape=19, size=2, position=position_dodge2(width = 1.0),alpha=0.9) +
  coord_flip()+
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=16), 
        legend.text = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14),
        plot.caption = element_text(size=16))
ggsave("pf_strat.png")
