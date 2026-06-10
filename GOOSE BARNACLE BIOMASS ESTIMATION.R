#ESTIMATION OF GOOSE BARNACLE BIOMASS FROM AVERAGE COVER IN CLASSIFICATION MAPS

rm(list=ls())
library(readxl)
library(tidyverse)
library(mgcv)
library(ggtext)

setwd("")
cover_biomass <- read_excel("Coberturas y biomasas mejilla-percebe.xlsx")
cover_biomass<-cover_biomass %>% 
  janitor::clean_names()
cover_biomass
cover_biomass_ok <-cover_biomass%>% 
  select(locality,microhabitat,replicate,rectangle_area ,mussel_area,barnacle_area,fresh_weigth_mussel,
         fresh_weight_barnacle,number_barnacle ,number_mussel) 
cover_biomass_ok<-cover_biomass_ok %>% 
  mutate(rel_cover_m=mussel_area /rectangle_area,
         rel_cover_p=barnacle_area/rectangle_area)
cover_biomass_ok<-cover_biomass_ok %>% 
  mutate(biomass_m_std=fresh_weigth_mussel /(rectangle_area/10000),
         biomass_p_std=fresh_weight_barnacle/(rectangle_area/10000))


cover_biomass_ok<-cover_biomass_ok %>% 
  dplyr::mutate(microhabitat = case_when(
    microhabitat == "E" ~ "Overhang",
    microhabitat == "G" ~ "Crevice",
    microhabitat == "I" ~ "Steep",
    microhabitat=="P"~"Flat"))



barnacle<-cover_biomass_ok %>% 
  select(replicate,locality,microhabitat,rel_cover_p,biomass_p_std)%>% 
  na.omit() 

#remove overhang 
barnacle <-barnacle %>% 
  filter(!microhabitat=="Overhang") %>% 
  filter(rel_cover_p>0)

table(barnacle$microhabitat)
boxplot(barnacle$biomass_p_std)
hist(barnacle$biomass_p_std)
plot(biomass_p_std ~ rel_cover_p, data = barnacle)

glm_gamma_p <- gam(biomass_p_std ~ rel_cover_p, data = barnacle,family = Gamma(link="identity"))
summary(glm_gamma_p)
plot(glm_gamma_p)

library(pscl)
pR2(glm_gamma_p)
empty_df_p<-data.frame(rel_cover_p=seq(min(barnacle$rel_cover_p),max(barnacle$rel_cover_p),length.out=100))

pred_p<- predict(glm_gamma_p, newdata = empty_df_p, type = "response", se.fit = TRUE)
pred_p

newdata_p<-empty_df_p %>% 
  mutate(response=pred_p$fit,
         se.fit=pred_p$se.fit,
         uper_ci=response+(se.fit*1.96),
         lower_ci=response-(se.fit*1.96))
summary(glm_gamma_p)
 


glm_goose_plot<-ggplot(newdata_p) +
  geom_ribbon(aes(x = rel_cover_p, ymax = uper_ci, ymin = lower_ci), alpha = 0.15) +
  geom_line(aes(x = rel_cover_p, y = response)) +
  geom_point(data = barnacle, aes(x = rel_cover_p, y = biomass_p_std, colour = microhabitat, shape = locality), size = 3) +
  theme_classic() +
  theme(     axis.text = element_text(size = 12,color = "black"),axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),legend.title = element_text(size=12),
    axis.text.x = )   +
  labs(    y = expression("Standarized biomass (g fw m"^-2*")"), x = "Relative cover",    colour = "Habitat",    shape = "Locality"  ) +
  scale_colour_brewer(name="Microhabitat",palette = "Dark2") +  
  scale_shape_manual(name="Site", labels=c("Aguncheiro","Rapacarallos"), values = c(16, 17))

 
glm_goose_plot

ggsave("glm_goose_plot.svg",plot=gam_goose_plot,device="svg",
       path="C:/POSDOC XUNTA 2023/MULTISPECTRAL LIDAR/FIGURES",
       width=7,height=5,units="in")







empty_df_p<-data.frame(rel_cover_p=0.22)
empty_df_p<-data.frame(rel_cover_p=0.23)
empty_df_p<-data.frame(rel_cover_p=0.25)

pred<- predict(glm_gamma_p, newdata = empty_df_p, type = "response", se.fit = TRUE)
pred
