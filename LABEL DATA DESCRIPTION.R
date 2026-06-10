 setwd("")
 
 library(tidyverse)
 library(tidyterra)
 library(ggdist)
 library(officer)
 library(flextable)
 library(sf)
 library(glmmTMB)
 library(lme4)

 
 #load labels
 load("OUTPUT/LABELS.RData")
 View(LABELS)
 
 class(LABELS)
 str(LABELS)
 
 LABELS_df <- st_drop_geometry(LABELS)
 table(LABELS_df$class)
 
 summary(LABELS_df$aspect)
 LABELS_df <- LABELS_df[LABELS_df$class != "water", ]
  table(LABELS_df$class)

  
  data<-LABELS_df %>% dplyr::select(slope,aspect, TRI, TPI,class,ID) 
  data
  data$ID<-factor(data$ID)
  data$class<-factor(data$class)
  levels(data$class)
  data$class<-factor(data$class,levels=c("goose_barnacle","other_barnacles","adult_mussels",
                                         "mussel_spat","red_algae","brown_algae","green_algae",
                                         "bare_rock"))
  
  levels(data$class)
  levels(data$ID)
  which(is.na(data))

  
  ##slope  

  data %>% 
    group_by(class) %>% 
    reframe(MEAN=mean(slope),SD=sd(slope),
            max=max(slope),min=min(slope))
  #slope ranges between 0 and 180, tranform to beta distribution with x/180
  
  hist(data$slope) 
  data$slope_t<-(data$slope/180)
  hist(data$slope_t)
  min(data$slope_t)
  max(data$slope_t)
  
  #beta glmm
  model_slope<-glmmTMB(slope_t~class+(1 | ID:class),data,family = beta_family)
  summary(model_slope)
  
  hist(residuals(model_slope))
  plot(fitted(model_slope), residuals(model_slope))#suggests heterokedasticity
  abline(h = 0, lty = 2,col="red")

  res <- simulateResiduals(model_slope)
  plot(res)

   
  model_slope2 <- glmmTMB(
    slope_t ~ class + (1 | ID:class),
    dispformula = ~ class,
    family = beta_family(link = "logit"),
    data = data)
  
  AIC(model_slope,model_slope2)
  anova(model_slope,model_slope2)
  
  summary(model_slope2)
  
  
  sim2 <- simulateResiduals(model_slope2)
  plot(sim2)
  
  confint(model_slope2)
  
  # predict model outcome
  slope_df<-data %>% dplyr::select(slope_t,class,ID)
  
  new_data <- data.frame(class = levels(slope_df$class)) %>% 
    left_join(slope_df,by="class") %>% 
    select(-slope_t) %>% 
    distinct()
  
  head(new_data)
  
  pred<-predict(model_slope2,new_data, se.fit=TRUE,type="response")
  
  
  new_data$fit=pred$fit
  new_data$se=pred$se.fit
  
    pred_slope<-new_data %>% 
      mutate(fit_ok=180*fit,SE_ok=se *180)
      
    
  slope_plot_data<-pred_slope %>% 
    group_by(class) %>% 
    reframe(mean=mean(fit_ok),SE=mean(SE_ok)) 

#PLOT 
 unique(data$class)
 slope_plot<-data %>% 
   group_by(class,slope) %>% 
   ggplot(aes(class,slope,fill=class)) +
   stat_halfeye(interval_size = 0,slab_color="black",alpha=.5,point_size=0)+
   geom_point(data=slope_plot_data,aes(x=class,y=mean),color="black",size=2)+
   geom_errorbar(data=slope_plot_data,aes(x=class,ymin=mean-SE,ymax=mean+SE),
                 inherit.aes = FALSE,width=.3)+
   scale_x_discrete(name="",limits = c("goose_barnacle", "other_barnacles", "adult_mussels","mussel_spat","red_algae","brown_algae",
                               "green_algae","bare_rock"),
                    labels=c( "Goose\nbarnacle","Other\nbarnacles","Adult\nmussels", "Mussel\nspat","Red\nalgae","Brown\nalgae","Green\nalgae",
                              "Bare\nrock"))+
   scale_y_continuous(name = "Slope (%)")+
   theme_classic()+
   theme(text = element_text(size=16,color="black"),
         plot.title=element_text(size=16,color="black"),legend.text = element_text(size=16,color = "black"),
         axis.text.x = element_text(color="black",size=14,angle = -15, hjust = 0),
         axis.text.y = element_text(size=16,color="black"),legend.position = "none",
         axis.title.x = element_text(size=18,color="black"), axis.title.y = element_text(size=18,color="black"))+
   scale_fill_manual(
     values = c("goose_barnacle" = "orange", "other_barnacles" = "pink",
       "adult_mussels" = "blue", "mussel_spat" = "grey3", "red_algae" = "red",
       "brown_algae" = "goldenrod4","green_algae" = "green","bare_rock" = "grey" ))+
   ggtitle("A)")
 
 slope_plot
 
 
 
 
 
 #ASPECT----
 data %>% 
   group_by(class) %>% 
   reframe(MEAN=mean(aspect),SD=sd(aspect), mediana=median(aspect),min=min(aspect),max=max(aspect))
 
 #aspect ranges between -1 an 1, converto to beta distribution with (x+1)/2
 hist(data$aspect)
 data$aspect_t<-(data$aspect+1)/2
 hist(data$aspect_t)
 max(data$aspect_t)
 min(data$aspect_t)
 
 #beta glmm
 model_aspect<-glmmTMB(aspect_t~class+(1 | ID:class),data,family = beta_family)
 summary(model_aspect)
 
 hist(residuals(model_aspect))
 plot(fitted(model_aspect), residuals(model_aspect))#suggests heterokedasticity
 abline(h = 0, lty = 2,col="red")
 
 model_aspect2<-glmmTMB(aspect_t~class+(1 | ID:class),dispformula=~class,
                       data,family = beta_family)

 
 anova(model_aspect,model_aspect2)
 
 summary(model_aspect2)
 
 # predict model outcome
 aspect_df<-data %>% dplyr::select(aspect_t,class,ID)
 
 new_data <- data.frame(class = levels(aspect_df$class)) %>% 
   left_join(aspect_df,by="class") %>% 
   select(-aspect_t) %>% 
   distinct()
 
 head(new_data)
 
 pred<-predict(model_aspect2,new_data, se.fit=TRUE,type="response")
 
 
 new_data$fit=pred$fit
 new_data$se=pred$se.fit
 
 pred_aspect<-new_data %>% 
   mutate(fit_ok=(2*fit)-1,SE_ok=(2*se))
 
 
aspect_plot_data<-pred_aspect %>% 
   group_by(class) %>% 
   reframe(mean=mean(fit_ok),SE=mean(SE_ok)) 
 
 
 aspect_plot<- data %>% 
   group_by(class,aspect) %>% 
   ggplot(aes(class,aspect,fill=class)) +
   stat_halfeye(interval_size = 0,slab_color="black",alpha=.5,point_size=0)+
   geom_point(data=aspect_plot_data,aes(x=class,y=mean),color="black",size=2)+
   geom_errorbar(data=aspect_plot_data,aes(x=class,ymin=mean-SE,ymax=mean+SE),
                 inherit.aes = FALSE,width=.3)+
   
   scale_x_discrete(name="",limits = c("goose_barnacle", "other_barnacles", "adult_mussels","mussel_spat","red_algae","brown_algae",
                                       "green_algae","bare_rock"),
                    labels=c( "Goose\nbarnacle","Other\nbarnacles","Adult\nmussels", "Mussel\nspat","Red\nalgae","Brown\nalgae","Green\nalgae",
                              "Bare\nrock"))+
   scale_y_continuous(name = "Northness")+
   theme_classic()+
   theme(text = element_text(size=16,color="black"),
         plot.title=element_text(size=16,color="black"),legend.text = element_text(size=16,color = "black"),
         axis.text.x = element_text(color="black",size=14,angle = -15, hjust = 0),
         axis.text.y = element_text(size=16,color="black"),legend.position = "none",
         axis.title.x = element_text(size=18,color="black"), axis.title.y = element_text(size=18,color="black"))+
   scale_fill_manual(
     values = c("goose_barnacle" = "orange", "other_barnacles" = "pink",
                "adult_mussels" = "blue", "mussel_spat" = "grey3", "red_algae" = "red",
                "brown_algae" = "goldenrod4","green_algae" = "green","bare_rock" = "grey" ))+
   ggtitle("B)")
 
 
 windows(); aspect_plot
 
 
#TRI-----
 data %>% 
   group_by(class) %>% 
   reframe(MEAN=mean(TRI),SD=sd(TRI), mediana=median(TRI),max=max(TRI),min=min(TRI))
 
 #theoretically, TRI ranges from 0 to inf, so it follows a gamma distribution, no transformation
 hist(data$TRI)
 
 
 #beta glmm
 library(nlme)
 model_tri<-glmmTMB( TRI ~ class + (1 | ID:class), data = data,
                     family = Gamma(link = "log") )
 summary(model_tri)
 
 hist(residuals(model_tri))
 plot(fitted(model_tri), residuals(model_tri)) 
 abline(h = 0, lty = 2,col="red")
 model_tri2 <- glmmTMB( TRI ~ class + (1 | ID:class), dispformula = ~ class,  data = data,
   family = Gamma(link = "log") )
 model_tri3 <- glmmTMB( TRI ~ class + (1 | ID:class), dispformula = ~ class,  data = data,
                        family =beta_family() )
 
 
 anova(model_tri,model_tri2,model_tri3)
 hist(residuals(model_tri2))
 qqnorm(residuals(model_tri2))
 plot(fitted(model_tri2), residuals(model_tri2)) 
 abline(h = 0, lty = 2,col="red")
 
 summary(model_tri2)
 
  # predict model outcome

 
 new_data <- data %>%
   select(class, ID) %>%
   distinct()
 
 head(new_data)
 
 pred<-predict(model_tri2,new_data, se.fit=TRUE,type="response")
 
 
 new_data$fit=pred$fit
 new_data$se=pred$se.fit
 
tri_plot_data<-new_data %>% 
   group_by(class) %>% 
   reframe(mean=mean(fit),SE=mean(se)) 
tri_plot_data
 
tri_plot<-data %>% 
   ggplot(aes(class,TRI,fill=class)) + 
   stat_halfeye(interval_size = 0,slab_color="black",alpha=.5,point_size=0)+
  geom_point(data=tri_plot_data,aes(x=class,y=mean),color="black",size=2)+
  geom_errorbar(data=tri_plot_data,aes(x=class,ymin=mean-SE,ymax=mean+SE),
                inherit.aes = FALSE,width=.3)+
   scale_x_discrete(name="Class",limits = c("goose_barnacle", "other_barnacles", "adult_mussels","mussel_spat","red_algae","brown_algae",
                                            "green_algae","bare_rock"),
                    labels=c( "Goose\nbarnacle","Other\nbarnacles","Adult\nmussels", "Mussel\nspat","Red\nalgae","Brown\nalgae","Green\nalgae",
                              "Bare\nrock"))+
   scale_y_continuous(name = "Topographic ruggedness index")+
   theme_classic()+
   theme(text = element_text(size=16,color="black"),
         plot.title=element_text(size=16,color="black"),legend.text = element_text(size=16,color = "black"),
         axis.text.x = element_text(color="black",size=14,angle = -15, hjust = 0),
         axis.text.y = element_text(size=16,color="black"),legend.position = "none",
         axis.title.x = element_text(size=18,color="black"), axis.title.y = element_text(size=18,color="black"))+
     scale_fill_manual(
     values = c(
       "goose_barnacle" = "orange",
       "other_barnacles" = "pink",
       "adult_mussels" = "blue",
       "mussel_spat" = "grey3",
       "red_algae" = "red",
       "brown_algae" = "goldenrod4",
       "green_algae" = "green",
       "bare_rock" = "grey"))+
   ggtitle("C)")+coord_cartesian(ylim=c(0,0.3))
 
 
windows(); tri_plot
 
 #TPI
 data %>% 
   group_by(class) %>% 
   reframe(MEAN=mean(TPI),SD=sd(TPI), mediana=median(TPI),min=min(TPI),max=max(TPI))
 hist(LABELS_df$TPI)
 
 #TPI can have values between -inf and +inf, so a gaussian distribution, no transformation
 hist(data$TPI)
qqnorm(data$TPI)
 
 #gaussian glmm
 library(nlme)
 
 model_tpi<-lme( TPI ~ class, random = ~1 | ID/class, data = data )

 hist(residuals(model_tpi))
 plot(fitted(model_tpi), residuals(model_tpi)) 
 abline(h = 0, lty = 2,col="red")
 
 model_tpi2 <- lme( TPI ~ class, random = ~1 | ID/class, 
                    weights = varIdent(form = ~1|class),
                     data = data,control = lmeControl( maxIter = 100, msMaxIter = 100))
 
 anova(model_tpi,model_tpi2)
 hist(residuals(model_tpi2))
 qqnorm(residuals(model_tpi2))
 plot(fitted(model_tpi2), residuals(model_tpi2)) 
 abline(h = 0, lty = 2,col="red")
 
 summary(model_tpi2)
 
 # predict model outcome
 
 
 new_data <- data %>%
   select(class, ID) %>%
   distinct()
 
 head(new_data)
 
 pred<-predict(model_tpi2,new_data, se.fit=TRUE,type="response")
 
 pred 
 new_data$fit <- pred
 
 library(emmeans)
 se_data<-emmeans(model_tpi2,~class)
 se_data=as.data.frame(se_data)
 
 tpi_plot_data<-new_data %>% 
   group_by(class) %>% 
   reframe(mean=mean(fit)) %>% 
   left_join(se_data)
 tpi_plot_data
 
 tpi_plot<-data %>% 
    ggplot(aes(class,TPI,fill=class)) + 
   stat_halfeye(interval_size = 0,slab_color="black",alpha=.5,point_size=0)+
   geom_point(data=tpi_plot_data,aes(x=class,y=mean),color="black",size=2)+
   geom_errorbar(data=tpi_plot_data,aes(x=class,ymin=mean-SE,ymax=mean+SE),
                 inherit.aes = FALSE,width=.3)+
   scale_x_discrete(name="Class",limits = c("goose_barnacle", "other_barnacles", "adult_mussels","mussel_spat","red_algae","brown_algae",
                                            "green_algae","bare_rock"),
                    labels=c( "Goose\nbarnacle","Other\nbarnacles","Adult\nmussels", "Mussel\nspat","Red\nalgae","Brown\nalgae","Green\nalgae",
                              "Bare\nrock"))+
   scale_y_continuous(name = "Topographic position index")+
   theme_classic()+
   theme(text = element_text(size=16,color="black"),
         plot.title=element_text(size=16,color="black"),legend.text = element_text(size=16,color = "black"),
         axis.text.x = element_text(color="black",size=14,angle = -15, hjust = 0),
         axis.text.y = element_text(size=16,color="black"),legend.position = "none",
         axis.title.x = element_text(size=18,color="black"), axis.title.y = element_text(size=18,color="black"))+
      scale_fill_manual(
     values = c(
       "goose_barnacle" = "orange",
       "other_barnacles" = "pink",
       "adult_mussels" = "blue",
       "mussel_spat" = "grey3",
       "red_algae" = "red",
       "brown_algae" = "goldenrod4",
       "green_algae" = "green",
       "bare_rock" = "grey"))+
   ggtitle("D)")+
  coord_cartesian(ylim=c(-0.2,0.3))
 
 
 tpi_plot
 
 library(ggpubr)
 topography_plot<-ggarrange(slope_plot,aspect_plot,tri_plot,tpi_plot,
                            nrow=2,ncol=2)
windows();topography_plot   
   
ggsave("FIGURES/topography_descrip.svg",plot=topography_plot,
       height = 9,width =13 ,units = "in")
