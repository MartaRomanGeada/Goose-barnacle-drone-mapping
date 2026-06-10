##INTERTIDAL HABITAT CLASSIFICATION WITH MULTISPECTRAL AND TOPOGRAPHIC DATA
#MARTA ROMÁN

rm(list = ls())
Sys.setenv(LANG = "en")


library(tidyterra)
library(terra)
library(sf)
library(sp)
library(tidyverse)
library(tidymodels)
library(janitor)
library(rsample)
library(DALEX)
library(DALEXtra)
library(ggspatial)
library(stacks)
library(future)
library(doFuture)
library(tune)
library(dplyr)
library(rsample)
library(purrr)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(terra::buffer)
parallel::detectCores(logical = TRUE)
parallel::detectCores(logical = FALSE)

registerDoFuture()
future::plan(sequential)


setwd("")


# https://www.indexdatabase.de/

#MODELS-----

#load labels
load("OUTPUT/LABELS.RData")
LABELS$B5_B4<-(LABELS$b5-LABELS$b4)/(LABELS$b5+LABELS$b4)


LABELS$AUS <- ((560-531)*((LABELS$b3 + LABELS$b4)/2) + 
                 (650-560)*((LABELS$b4 + LABELS$b5)/2) + 
                 (668-650)*((LABELS$b5 + LABELS$b6)/2))
windows();LABELS %>%  
  filter(class=="goose_barnacle"|class=="other_barnacles") %>% 
  ggplot(aes(x=class,y=AUS,colour = class))+
  geom_boxplot()+
  geom_point()

LABELS %>%  
  filter(class=="goose_barnacle"|class=="other_barnacles") %>% 
  group_by(class) %>% 
  reframe(median=median(AUS)) %>% 
  summarise(difference = median[which(class=="other_barnacles")] - median[which(class=="goose_barnacle")])





LABELS %>%  
  filter(class=="goose_barnacle"|class=="other_barnacles") %>% 
  ggplot(aes(x=class,y=B5_B4,colour = class))+
  geom_boxplot()+
  geom_point()

LABELS %>%  
  filter(class=="goose_barnacle"|class=="other_barnacles") %>% 
  ggplot(aes(x=class,y=TPI,colour = class))+
  geom_boxplot()+
  geom_point()

LABELS %>%  
  filter(class=="goose_barnacle"|class=="other_barnacles") %>% 
  ggplot(aes(x=class,y=TRI,colour = class))+
  geom_boxplot()+
  geom_point()

LABELS %>%  
  filter(class=="goose_barnacle"|class=="other_barnacles") %>% 
  ggplot(aes(x=class,y=slope,colour = class))+
  geom_boxplot()+
  geom_point()

LABELS %>%  
  filter(class=="goose_barnacle"|class=="other_barnacles") %>% 
  ggplot(aes(x=class,y=aspect,colour = class))+
  geom_boxplot()+
  geom_point()




data<-LABELS%>%
  dplyr::select(-geometry,-PixelID,-b1,-b2,-b3,-b4,-b5,-b6,-b7,-b8,-b9,-b10) %>% 
  mutate(class=as.factor(class)) %>% 
  na.omit()

data_labels<-LABELS %>% 
  group_by(PixelID) %>% 
  reframe(class=unique(class), geometry=unique(geometry),ID=unique(ID))

table(data$class)

#polygons per class and total
data %>% 
  group_by(ID,class) %>% 
  reframe() %>% 
  group_by(class) %>% 
  count() %>%   
  ungroup() %>%        
  summarise(total = sum(n))

 
LABELS %>% 
  group_by(class) %>% 
  reframe(N=n()) %>% 
  reframe(mean=mean(N),sd=sd(N))

tidymodels_prefer()

#model RF-----

#for fisrt fits, subset a fraction
data_strat <- data %>%
  group_by(class,ID) %>%
  sample_frac(0.05)

data_strat=data #if using all data

data_strat$class<-factor(data_strat$class)

#split data avoiding data leakage


id_data <- data_strat %>%
  distinct(ID, class)

id_split <- initial_split(id_data, strata = class)
id_split
train_ids <- training(id_split) %>% pull(ID)
test_ids  <- testing(id_split) %>% pull(ID)


train <- data_strat %>%
  filter(ID %in% train_ids)

test <- data_strat %>%
  filter(ID %in% test_ids)
folds<-vfold_cv(train,strata = class)


#for last fit
train_idx <- which(data_strat$ID %in% train$ID)
test_idx  <- which(data_strat$ID %in% test$ID)

pixel_split <- make_splits(
  list(analysis = train_idx, assessment = test_idx),
  data = data_strat
)
pixel_split

# load("MODELS/PIXEL_SPLIT.RData")


# remove zero variance

recipe<-recipe(class~.,data=train) %>%
  update_role(ID, new_role = "id") %>%
  step_zv(all_predictors()) 
recipe
print(summary(recipe),n=28)

trained_recipe <- recipe %>%
    prep(training = train)
trained_recipe
summary(trained_recipe)

print( summary(trained_recipe),n=28)
  
bake_recipe<-trained_recipe %>% 
    bake(new_data = train)

bake_recipe
summary(bake_recipe)

#RANDOM FOREST

#tune parameters
tune_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()) %>%
  set_mode("classification") %>%
  set_engine("ranger",importance = "permutation",
             class.weights = c( adult_mussels    = 1,
               bare_rock        = 1,
               brown_algae      = 1,
               goose_barnacle   = 10,   
               green_algae      = 1,
               mussel_spat      = 1,
               other_barnacles  = 0.5,
               red_algae        = 1,
               water            = 1 ))

tune_spec


tune_wf <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(tune_spec)
tune_wf

# #tuning
tune_res <- tune_grid(
  tune_wf,
  resamples = folds,
  grid = 10 )

 

tune_res 
best_auc <- select_best(tune_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

final_rf

final_wf <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(final_rf)

final_wf

final_res <- final_wf %>%
  last_fit(pixel_split)

final_res %>%
  collect_metrics()

final_model <- final_res %>%
  extract_workflow()


metrica<-final_res %>%
  collect_metrics()

confusionmat<-final_res  %>%
  unnest(cols=.predictions) %>%
  conf_mat(class,.pred_class)

kap<-  final_res  %>%
  unnest(cols=.predictions) %>%
  kap(class,.pred_class)


summary(confusionmat)
confusionmat

final_wf %>% extract_spec_parsnip()
final_wf$fit$spec


final_model
# final_res
final_rf
final_wf
wf <- final_res$.workflow[[1]]


wf



#UA AND PA
#USER AND PRODUCER ACCURACIES
matriz <- as.matrix(confusionmat$table)
matriz
total_predicted <- rowSums(matriz)
total_predicted

total_actual<- colSums(matriz)

user_accuracy <- diag(matriz) / total_predicted
user_accuracy

producer_accuracy <- diag(matriz) / total_actual
producer_accuracy


accuracies<-rbind(user_accuracy,producer_accuracy)
accuracies<-as.data.frame(accuracies)


 
F1_scores<-accuracies %>% 
  rownames_to_column() %>% 
  pivot_longer(cols=-rowname,names_to = "class",values_to = "values") %>% 
  group_by(class)%>%
  summarise(
    hmean = n() / sum(1 / values),
    .groups = "drop"
  )

F1_scores




#SAVE MODEL-----

save(pixel_split,file="MODELS/PIXEL_SPLIT.RData")
save(recipe,file="MODELS/RECIPE.RData")

save(tune_spec,file="MODELS/TUNE_SPEC.RData")
save(tune_wf,file="MODELS/TUNE_WF.RData")
save(tune_res,file="MODELS/TUNE_RES.RData")

save(final_rf,file="MODELS/FINAL_RF.RData")
save(final_wf,file="MODELS/FINAL_WF.RData")
save(final_res,file="MODELS/FINAL_RES.RData")
save(final_model,file="MODELS/FINAL_MODEL.RData")
#
#
save(confusionmat ,file="MODELS/CONF_MAT.RData")

#LOAD MODEL-----
load("C:/POSDOC XUNTA 2023/MULTISPECTRAL LIDAR/MODELS/FINAL_MODEL.RData")
load("C:/POSDOC XUNTA 2023/MULTISPECTRAL LIDAR/MODELS/FINAL_RES.RData")
load("C:/POSDOC XUNTA 2023/MULTISPECTRAL LIDAR/MODELS/FINAL_RF.RData")
load("C:/POSDOC XUNTA 2023/MULTISPECTRAL LIDAR/MODELS/FINAL_WF.RData")

#CONFUSION MATRIX-----
load("C:/POSDOC XUNTA 2023/MULTISPECTRAL LIDAR/MODELS/CONF_MAT.RData")


#CONFUSION MATRIX PLOT----
conf_df <- as.data.frame(confusionmat$table)
colnames(conf_df) <- c("class", ".pred_class", "n")
conf_df$class<-factor(conf_df$class,
                      levels=c("goose_barnacle","other_barnacles","adult_mussels","mussel_spat","red_algae",
                               "brown_algae","green_algae","bare_rock","water"),
                      labels=c("Goose barnacle","Other barnacles","Adult mussels","Mussel spat","Red algae",
                               "Brown algae","Green algae","Bare rock","Water"))

conf_df$.pred_class<-factor(conf_df$.pred_class,
                            levels=c("goose_barnacle","other_barnacles","adult_mussels","mussel_spat","red_algae",
                                     "brown_algae","green_algae","bare_rock","water"),
                            labels=c("Goose barnacle","Other barnacles","Adult mussels","Mussel spat","Red algae",
                                     "Brown algae","Green algae","Bare rock","Water"))


conf_df_prop <- conf_df %>%
  group_by(.pred_class) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

#añadir UA y PA

user_acc<-conf_df %>% 
  group_by(class) %>% 
  summarise(correct=sum(n[.pred_class==class]),
            total_pred=sum(n),
            UA=correct/total_pred) %>% 
  select(class, UA)

user_acc


prod_acc<-conf_df %>% 
  group_by(.pred_class) %>% 
  summarise(correct=sum(n[.pred_class==class]),
            total_true=sum(n),
            PA=correct/total_true) %>% 
  rename(class=.pred_class) %>% 
  select(class, PA)

prod_acc

conf_df_prop<-conf_df_prop %>% 
  left_join(user_acc) %>% 
  left_join(prod_acc)


#plot
max_x <- max(as.numeric(factor(conf_df_prop$class)))
max_y <- max(as.numeric(factor(conf_df_prop$.pred_class)))

confusion_matrix_plot_ok<-
  conf_df_prop %>%
  mutate(prop_r=round(prop,2)) %>% 
  mutate( n_label = ifelse(n == 0, NA, round(n, 2))) %>% 
  ggplot(aes(x = class, y = .pred_class, fill = prop_r)) +
  geom_tile() +
  scale_fill_viridis_c(name="Proportion \nof pixels",option="D") +
  theme_bw()+
  labs( x = "True Class", y = "Predicted Class") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  axis.title.y=element_text(color="black",size=12),axis.text.y=element_text(size=12,color="black"),
        axis.title.x=element_text(color="black",size=12),legend.text = element_text(size=12,color="black"),
        legend.title = element_text(color = "black",size=12),
        axis.text.x = element_text(color="black",size=12,angle = 45, hjust = 1))+
  geom_label(aes( label = n_label,fill  = ifelse(is.na(n_label), NA) ), fill="white", color = "black",  label.size = 0,                   # Sin borde
             label.r = unit(0, "lines"),  size = 4,  label.padding = unit(0.2, "lines"),  show.legend = FALSE)



confusion_matrix_plot_ok

ggsave(plot=confusion_matrix_plot_ok,file = "FIGURES/Figure_4.svg",,device= "svg",
       width=8,height=6.5,units="in")





#IMPORTANCE-------

df_train_exp <- train %>%
  dplyr::ungroup() %>%      
  dplyr::select(-class)

class <- train$class 

explainer <-  explain_tidymodels(    final_model,    data = df_train_exp,    y = train$class,
    verbose = FALSE  )


plan()

plan(multisession, workers = 4) 

importance <- variable_importance(explainer)
importance

save(importance,file="MODELS/IMPORTANCE.RData")
save(explainer,file="MODELS/EXPLAINER.RData")

load("MODELS/EXPLAINER.RData")
load("MODELS/IMPORTANCE.RData")

table(importance$variable)
importance<-importance %>%
  mutate(variable = str_replace_all(variable, c(
  "dem" = "DEM",
  "red" = "Red",
  "green"="Green",
  "blue"="Blue",
  "aspect"="Orientation",
  "slope"="Slope")))

  
  
summary<-importance%>%
  filter(!variable=="_baseline_",
         !variable=="ID",
         !variable=="_full_model_")%>%
  group_by(variable)%>%
  reframe(mean=mean(dropout_loss), 
          sd=sd(dropout_loss),
          n=n(),
          se=sd/sqrt(n))%>%
  dplyr::arrange(desc(mean)) %>% 
  dplyr::mutate(variable = variable %>% 
                  gsub("b1_std", "Coastal Blue", .) %>%
                  gsub("b2_std", "Blue 475", .) %>%
                  gsub("b3_std", "Green 531", .) %>%
                  gsub("b4_std", "Green 560", .) %>%
                  gsub("b5_std", "Red 650", .) %>%
                  gsub("b6_std", "Red 668", .) %>%
                  gsub("b7_std", "Red Edge 705", .) %>%
                  gsub("b8_std", "Red Edge 717", .) %>%
                  gsub("b9_std", "Red Edge 740", .) %>%
                  gsub("b10_std", "Near Infrared", .) %>% 
                  gsub("B5_B4","NGRDI",.) %>% 
                  gsub("aspect","Northness",.) %>%
                  gsub("slope","Slope",.))



print(summary,n=25)
importance_plot<-ggplot(summary, aes(x = reorder(variable, mean), y = mean,fill=variable)) +
  geom_col( alpha = 0.7,color="black") +
  geom_errorbar(aes(x=reorder(variable, mean), ymin=mean-se, 
                    ymax=mean+se), 
                width=.25, 
                position = position_dodge(1))+
  theme_classic()+  coord_flip()+ 
  
  scale_x_discrete(name = "Bands") +
  scale_y_continuous(name=("Cross entropy"))+
  scale_fill_manual(values = c("Coastal Blue" = "turquoise", "Blue 475" = "blue",
                               "Green 531" = "yellowgreen", "Green 560" = "darkgreen",
                               "Red 650" = "brown", "Red 668" = "red1", 
                               "Red Edge 705" = "red2", "Red Edge 717" = "red3", 
                               "Red Edge 740" = "red4", "Near Infrared" = "grey",
                               "TRI"="black","TPI"="black","AUS"="white","NGRDI"="white",
                               "Slope"="black","Northness"="black"))+
  theme(text = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        
        legend.position = "none") +
  ggtitle("A) All classes")



importance_plot


#EXPLAINER GOOSE--------



goose<-train%>%
 dplyr::filter(class=="goose_barnacle")
shap_goose <- predict_parts(explainer, new_observation = goose,
                            type = "shap")
save(shap_goose,file="MODELS/SHAPLEY_GOOSE.Rdata")

load("MODELS/SHAPLEY_GOOSE.Rdata")

head(shap_goose)
names(shap_goose)
shap_goose_<- shap_goose[shap_goose$label == "workflow.goose_barnacle", ]

table(shap_goose_$label)
table(shap_goose_$variable_name)


shap_goose_summary<-shap_goose_%>%
  filter(!variable_name=="")%>%
  group_by(variable_name)%>%
  reframe(mean=mean(contribution), 
          sd=sd(contribution),
          n=n(),
          se=sd/sqrt(n))%>%
  dplyr::arrange(desc(mean)) %>%   
  slice_head(n = 26)  
shap_goose_summary

shap_goose_summary <- shap_goose_summary %>%
  dplyr::mutate(variable_name = variable_name %>% 
                  gsub("b1_std", "Coastal Blue", .) %>%
                  gsub("b2_std", "Blue 475", .) %>%
                  gsub("b3_std", "Green 531", .) %>%
                  gsub("b4_std", "Green 560", .) %>%
                  gsub("b5_std", "Red 650", .) %>%
                  gsub("b6_std", "Red 668", .) %>%
                  gsub("b7_std", "Red Edge 705", .) %>%
                  gsub("b8_std", "Red Edge 717", .) %>%
                  gsub("b9_std", "Red Edge 740", .) %>%
                  gsub("b10_std", "Near Infrared", .) %>% 
                  gsub("B5_B4","NGRDI",.) %>% 
                  gsub("aspect","Northness",.) %>%
                  gsub("slope","Slope",.)) %>%
  filter(!variable_name=="ID")



shap_goose_plot<-ggplot(shap_goose_summary, aes(x = reorder(variable_name, mean), y = mean, fill = variable_name)) +
  geom_col(alpha = 0.7,color="black") +
  geom_errorbar(aes(x = reorder(variable_name, mean), ymin = mean - se, 
                    ymax = mean + se), 
                width = .25, 
                position = position_dodge(1)) +
  theme_classic() + 
  coord_flip() + 
  scale_fill_manual(values = c("Coastal Blue" = "turquoise", "Blue 475" = "blue",
                               "Green 531" = "yellowgreen", "Green 560" = "darkgreen",
                               "Red 650" = "brown", "Red 668" = "red1", 
                               "Red Edge 705" = "red2", "Red Edge 717" = "red3", 
                               "Red Edge 740" = "red4", "Near Infrared" = "grey",
                    "TRI"="black","TPI"="black","AUS"="white","NGRDI"="white","Slope"="black","Northness"="black"))+    
  scale_x_discrete(name = "Bands") +
  scale_y_continuous(name = "SHAP values") +   
  theme(text = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        
        legend.position = "none") +
  ggtitle("B) Goose barnacle")

shap_goose_plot





load("C:/POSDOC XUNTA 2023/MULTISPECTRAL LIDAR/MODELS/SHAPLEY_GOOSE.Rdata")
load(file="MODELS/IMPORTANCE.RData")
load(file="MODELS/EXPLAINER.RData")

 
#EXPLAINER OTHER BARNACLES------


barnacles<-train%>%
  dplyr::filter(class=="other_barnacles")

shap_barnacles <- predict_parts(explainer, new_observation = barnacles,
                                type = "shap")
save(shap_barnacles,file="MODELS/SHAPLEY_BARNACLES.Rdata")

load("MODELS/SHAPLEY_BARNACLES.Rdata")

head(shap_barnacles)
names(shap_barnacles)
shap_barnacles_<- shap_barnacles[shap_barnacles$label == "workflow.other_barnacles", ]

table(shap_barnacles_$label)
table(shap_barnacles_$variable_name)

 
shap_barnacles_summary<-shap_barnacles_%>%
  filter(!variable_name=="")%>%
  group_by(variable_name)%>%
  reframe(mean=mean(contribution), 
          sd=sd(contribution),
          n=n(),
          se=sd/sqrt(n))%>%
  dplyr::arrange(desc(mean)) %>%  
  slice_head(n = 26)  
shap_barnacles_summary

shap_barnacles_summary <- shap_barnacles_summary %>%
  dplyr::mutate(variable_name = variable_name %>% 
                  gsub("b1_std", "Coastal Blue", .) %>%
                  gsub("b2_std", "Blue 475", .) %>%
                  gsub("b3_std", "Green 531", .) %>%
                  gsub("b4_std", "Green 560", .) %>%
                  gsub("b5_std", "Red 650", .) %>%
                  gsub("b6_std", "Red 668", .) %>%
                  gsub("b7_std", "Red Edge 705", .) %>%
                  gsub("b8_std", "Red Edge 717", .) %>%
                  gsub("b9_std", "Red Edge 740", .) %>%
                  gsub("b10_std", "Near Infrared", .) %>% 
                  gsub("B5_B4","NGRDI",.) %>% 
                  gsub("aspect","Northness",.) %>%
                  gsub("slope","Slope",.)) %>%
  filter(!variable_name=="ID")


shap_barnacles_plot<-ggplot(shap_barnacles_summary, aes(x = reorder(variable_name, mean), y = mean, fill = variable_name)) +
  geom_col(alpha = 0.7,color="black") +
  geom_errorbar(aes(x = reorder(variable_name, mean), ymin = mean - se, 
                    ymax = mean + se), 
                width = .25, 
                position = position_dodge(1)) +
  theme_classic() + 
  coord_flip() + 
  scale_fill_manual(values = c("Coastal Blue" = "turquoise", "Blue 475" = "blue",
                               "Green 531" = "yellowgreen", "Green 560" = "darkgreen",
                               "Red 650" = "brown", "Red 668" = "red1", 
                               "Red Edge 705" = "red2", "Red Edge 717" = "red3", 
                               "Red Edge 740" = "red4", "Near Infrared" = "grey",
                               "TRI"="black","TPI"="black","AUS"="white","NGRDI"="white","Slope"="black","Northness"="black"))+    
  scale_x_discrete(name = "Bands") +
  scale_y_continuous(name = "SHAP values") +   
  theme(text = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        
        legend.position = "none") +
  ggtitle("C) Other barnacles")

shap_barnacles_plot
 


#EXPLAINER ADULT MUSSEL------



mussel<-train%>%
  dplyr::filter(class=="adult_mussels")

shap_mussel <- predict_parts(explainer, new_observation = mussel,
                           type = "shap")
save(shap_mussel,file="MODELS/SHAPLEY_MUSSEL.Rdata")

load("MODELS/SHAPLEY_MUSSEL.Rdata")

head(shap_mussel)
names(shap_mussel)
shap_mussel_<- shap_mussel[shap_mussel$label == "workflow.adult_mussels", ]

table(shap_mussel_$label)
table(shap_mussel_$variable_name)


shap_mussel_summary<-shap_mussel_%>%
  filter(!variable_name=="")%>%
  group_by(variable_name)%>%
  reframe(mean=mean(contribution), 
          sd=sd(contribution),
          n=n(),
          se=sd/sqrt(n))%>%
  dplyr::arrange(desc(mean)) %>%   
  slice_head(n = 26)  
shap_mussel_summary

shap_mussel_summary <- shap_mussel_summary %>%
  dplyr::mutate(variable_name = variable_name %>% 
                  gsub("b1_std", "Coastal Blue", .) %>%
                  gsub("b2_std", "Blue 475", .) %>%
                  gsub("b3_std", "Green 531", .) %>%
                  gsub("b4_std", "Green 560", .) %>%
                  gsub("b5_std", "Red 650", .) %>%
                  gsub("b6_std", "Red 668", .) %>%
                  gsub("b7_std", "Red Edge 705", .) %>%
                  gsub("b8_std", "Red Edge 717", .) %>%
                  gsub("b9_std", "Red Edge 740", .) %>%
                  gsub("b10_std", "Near Infrared", .) %>% 
                  gsub("B5_B4","NGRDI",.) %>% 
                  gsub("aspect","Northness",.) %>%
                  gsub("slope","Slope",.)) %>%
  filter(!variable_name=="ID")



shap_mussel_plot<-ggplot(shap_mussel_summary, aes(x = reorder(variable_name, mean), y = mean, fill = variable_name)) +
  geom_col(alpha = 0.7,color="black") +
  geom_errorbar(aes(x = reorder(variable_name, mean), ymin = mean - se, 
                    ymax = mean + se), 
                width = .25, 
                position = position_dodge(1)) +
  theme_classic() + 
  coord_flip() + 
  scale_fill_manual(values = c("Coastal Blue" = "turquoise", "Blue 475" = "blue",
                               "Green 531" = "yellowgreen", "Green 560" = "darkgreen",
                               "Red 650" = "brown", "Red 668" = "red1", 
                               "Red Edge 705" = "red2", "Red Edge 717" = "red3", 
                               "Red Edge 740" = "red4", "Near Infrared" = "grey",
                               "TRI"="black","TPI"="black","AUS"="white","NGRDI"="white","Slope"="black","Northness"="black"))+    
  scale_x_discrete(name = "Bands") +
  scale_y_continuous(name = "SHAP values") +   
  theme(text = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        
        legend.position = "none") +
  ggtitle("D) Adult mussels")

shap_mussel_plot
 
#EXPLAINER MUSSEL SPAT------


spat<-train%>%
  dplyr::filter(class=="mussel_spat")

shap_spat <- predict_parts(explainer, new_observation = spat,
                           type = "shap")
save(shap_spat,file="MODELS/SHAPLEY_SPAT.Rdata")

load("MODELS/SHAPLEY_SPAT.Rdata")

head(shap_spat)
names(shap_spat)
shap_spat_<- shap_spat[shap_spat$label == "workflow.mussel_spat", ]

table(shap_spat_$label)
table(shap_spat_$variable_name)


shap_spat_summary<-shap_spat_%>%
  filter(!variable_name=="")%>%
  group_by(variable_name)%>%
  reframe(mean=mean(contribution), 
          sd=sd(contribution),
          n=n(),
          se=sd/sqrt(n))%>%
  dplyr::arrange(desc(mean)) %>%   
  slice_head(n = 26)  
shap_spat_summary

shap_spat_summary <- shap_spat_summary %>%
  dplyr::mutate(variable_name = variable_name %>% 
                  gsub("b1_std", "Coastal Blue", .) %>%
                  gsub("b2_std", "Blue 475", .) %>%
                  gsub("b3_std", "Green 531", .) %>%
                  gsub("b4_std", "Green 560", .) %>%
                  gsub("b5_std", "Red 650", .) %>%
                  gsub("b6_std", "Red 668", .) %>%
                  gsub("b7_std", "Red Edge 705", .) %>%
                  gsub("b8_std", "Red Edge 717", .) %>%
                  gsub("b9_std", "Red Edge 740", .) %>%
                  gsub("b10_std", "Near Infrared", .) %>% 
                  gsub("B5_B4","NGRDI",.) %>% 
                  gsub("aspect","Northness",.) %>%
                  gsub("slope","Slope",.)) %>%
  filter(!variable_name=="ID")



shap_spat_plot<-ggplot(shap_spat_summary, aes(x = reorder(variable_name, mean), y = mean, fill = variable_name)) +
  geom_col(alpha = 0.7,color="black") +
  geom_errorbar(aes(x = reorder(variable_name, mean), ymin = mean - se, 
                    ymax = mean + se), 
                width = .25, 
                position = position_dodge(1)) +
  theme_classic() + 
  coord_flip() + 
  scale_fill_manual(values = c("Coastal Blue" = "turquoise", "Blue 475" = "blue",
                               "Green 531" = "yellowgreen", "Green 560" = "darkgreen",
                               "Red 650" = "brown", "Red 668" = "red1", 
                               "Red Edge 705" = "red2", "Red Edge 717" = "red3", 
                               "Red Edge 740" = "red4", "Near Infrared" = "grey",
                               "TRI"="black","TPI"="black","AUS"="white","NGRDI"="white","Slope"="black","Northness"="black"))+    
  scale_x_discrete(name = "Bands") +
  scale_y_continuous(name = "SHAP values") +   
  theme(text = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        
        legend.position = "none") +
  ggtitle("E) Mussel spat")

shap_spat_plot



#EXPLAINER RED ALGAE------



red<-train%>%
  dplyr::filter(class=="red_algae")

shap_red <- predict_parts(explainer, new_observation = red,
                                type = "shap")
save(shap_red,file="MODELS/SHAPLEY_RED.Rdata")

load("MODELS/SHAPLEY_RED.Rdata")

head(shap_red)
names(shap_red)
shap_red_<- shap_red[shap_red$label == "workflow.red_algae", ]

table(shap_red_$label)
table(shap_red_$variable_name)


shap_red_summary<-shap_red_%>%
  filter(!variable_name=="")%>%
  group_by(variable_name)%>%
  reframe(mean=mean(contribution), 
          sd=sd(contribution),
          n=n(),
          se=sd/sqrt(n))%>%
  dplyr::arrange(desc(mean)) %>%  
  slice_head(n = 26)  
shap_red_summary

shap_red_summary <- shap_red_summary %>%
  dplyr::mutate(variable_name = variable_name %>% 
                  gsub("b1_std", "Coastal Blue", .) %>%
                  gsub("b2_std", "Blue 475", .) %>%
                  gsub("b3_std", "Green 531", .) %>%
                  gsub("b4_std", "Green 560", .) %>%
                  gsub("b5_std", "Red 650", .) %>%
                  gsub("b6_std", "Red 668", .) %>%
                  gsub("b7_std", "Red Edge 705", .) %>%
                  gsub("b8_std", "Red Edge 717", .) %>%
                  gsub("b9_std", "Red Edge 740", .) %>%
                  gsub("b10_std", "Near Infrared", .) %>% 
                  gsub("B5_B4","NGRDI",.) %>% 
                  gsub("aspect","Northness",.) %>%
                  gsub("slope","Slope",.)) %>%
  filter(!variable_name=="ID")


shap_red_plot<-ggplot(shap_red_summary, aes(x = reorder(variable_name, mean), y = mean, fill = variable_name)) +
  geom_col(alpha = 0.7,color="black") +
  geom_errorbar(aes(x = reorder(variable_name, mean), ymin = mean - se, 
                    ymax = mean + se), 
                width = .25, 
                position = position_dodge(1)) +
  theme_classic() + 
  coord_flip() + 
  scale_fill_manual(values = c("Coastal Blue" = "turquoise", "Blue 475" = "blue",
                               "Green 531" = "yellowgreen", "Green 560" = "darkgreen",
                               "Red 650" = "brown", "Red 668" = "red1", 
                               "Red Edge 705" = "red2", "Red Edge 717" = "red3", 
                               "Red Edge 740" = "red4", "Near Infrared" = "grey",
                               "TRI"="black","TPI"="black","AUS"="white","NGRDI"="white","Slope"="black","Northness"="black"))+    
  scale_x_discrete(name = "Bands") +
  scale_y_continuous(name = "SHAP values") +   
  theme(text = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        
        legend.position = "none") +
  ggtitle("F) Red Algae")

shap_red_plot
 


#EXPLAINER BROWN ALGAE------



brown<-train%>%
  dplyr::filter(class=="brown_algae")

shap_brown <- predict_parts(explainer, new_observation = brown,
                          type = "shap")
save(shap_brown,file="MODELS/SHAPLEY_BROWN.Rdata")

load("MODELS/SHAPLEY_BROWN.Rdata")

head(shap_brown)
names(shap_brown)
shap_brown_<- shap_brown[shap_brown$label == "workflow.brown_algae", ]

table(shap_brown_$label)
table(shap_brown_$variable_name)


shap_brown_summary<-shap_brown_%>%
  filter(!variable_name=="")%>%
  group_by(variable_name)%>%
  reframe(mean=mean(contribution), 
          sd=sd(contribution),
          n=n(),
          se=sd/sqrt(n))%>%
  dplyr::arrange(desc(mean)) %>%  
  slice_head(n = 26)  
shap_brown_summary

shap_brown_summary <- shap_brown_summary %>%
  dplyr::mutate(variable_name = variable_name %>% 
                  gsub("b1_std", "Coastal Blue", .) %>%
                  gsub("b2_std", "Blue 475", .) %>%
                  gsub("b3_std", "Green 531", .) %>%
                  gsub("b4_std", "Green 560", .) %>%
                  gsub("b5_std", "Red 650", .) %>%
                  gsub("b6_std", "Red 668", .) %>%
                  gsub("b7_std", "Red Edge 705", .) %>%
                  gsub("b8_std", "Red Edge 717", .) %>%
                  gsub("b9_std", "Red Edge 740", .) %>%
                  gsub("b10_std", "Near Infrared", .) %>% 
                  gsub("B5_B4","NGRDI",.) %>% 
                  gsub("aspect","Northness",.) %>%
                  gsub("slope","Slope",.)) %>%
  filter(!variable_name=="ID")


shap_brown_plot<-ggplot(shap_brown_summary, aes(x = reorder(variable_name, mean), y = mean, fill = variable_name)) +
  geom_col(alpha = 0.7,color="black") +
  geom_errorbar(aes(x = reorder(variable_name, mean), ymin = mean - se, 
                    ymax = mean + se), 
                width = .25, 
                position = position_dodge(1)) +
  theme_classic() + 
  coord_flip() + 
  scale_fill_manual(values = c("Coastal Blue" = "turquoise", "Blue 475" = "blue",
                               "Green 531" = "yellowgreen", "Green 560" = "darkgreen",
                               "Red 650" = "brown", "Red 668" = "red1", 
                               "Red Edge 705" = "red2", "Red Edge 717" = "red3", 
                               "Red Edge 740" = "red4", "Near Infrared" = "grey",
                               "TRI"="black","TPI"="black","AUS"="white","NGRDI"="white","Slope"="black","Northness"="black"))+    
  scale_x_discrete(name = "Bands") +
  scale_y_continuous(name = "SHAP values") +   
  theme(text = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        
        legend.position = "none") +
  ggtitle("G) Brown Algae")

shap_brown_plot


#EXPLAINER GREEN ALGAE------



green<-train%>%
  dplyr::filter(class=="green_algae")

shap_green <- predict_parts(explainer, new_observation = green,
                            type = "shap")
save(shap_green,file="MODELS/SHAPLEY_GREEN.Rdata")

load("MODELS/SHAPLEY_GREEN.Rdata")

head(shap_green)
names(shap_green)
shap_green_<- shap_green[shap_green$label == "workflow.green_algae", ]

table(shap_green_$label)
table(shap_green_$variable_name)


shap_green_summary<-shap_green_%>%
  filter(!variable_name=="")%>%
  group_by(variable_name)%>%
  reframe(mean=mean(contribution), 
          sd=sd(contribution),
          n=n(),
          se=sd/sqrt(n))%>%
  dplyr::arrange(desc(mean)) %>%   
  slice_head(n = 26)  
shap_green_summary

shap_green_summary <- shap_green_summary %>%
  dplyr::mutate(variable_name = variable_name %>% 
                  gsub("b1_std", "Coastal Blue", .) %>%
                  gsub("b2_std", "Blue 475", .) %>%
                  gsub("b3_std", "Green 531", .) %>%
                  gsub("b4_std", "Green 560", .) %>%
                  gsub("b5_std", "Red 650", .) %>%
                  gsub("b6_std", "Red 668", .) %>%
                  gsub("b7_std", "Red Edge 705", .) %>%
                  gsub("b8_std", "Red Edge 717", .) %>%
                  gsub("b9_std", "Red Edge 740", .) %>%
                  gsub("b10_std", "Near Infrared", .) %>% 
                  gsub("B5_B4","NGRDI",.) %>% 
                  gsub("aspect","Northness",.) %>%
                  gsub("slope","Slope",.)) %>%
  filter(!variable_name=="ID")


shap_green_plot<-ggplot(shap_green_summary, aes(x = reorder(variable_name, mean), y = mean, fill = variable_name)) +
  geom_col(alpha = 0.7,color="black") +
  geom_errorbar(aes(x = reorder(variable_name, mean), ymin = mean - se, 
                    ymax = mean + se), 
                width = .25, 
                position = position_dodge(1)) +
  theme_classic() + 
  coord_flip() + 
  scale_fill_manual(values = c("Coastal Blue" = "turquoise", "Blue 475" = "blue",
                               "Green 531" = "yellowgreen", "Green 560" = "darkgreen",
                               "Red 650" = "brown", "Red 668" = "red1", 
                               "Red Edge 705" = "red2", "Red Edge 717" = "red3", 
                               "Red Edge 740" = "red4", "Near Infrared" = "grey",
                               "TRI"="black","TPI"="black","AUS"="white","NGRDI"="white","Slope"="black","Northness"="black"))+    
  scale_x_discrete(name = "Bands") +
  scale_y_continuous(name = "SHAP values") +   
  theme(text = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        
        legend.position = "none") +
  ggtitle("H) Green Algae")

shap_green_plot
shap_spat_plot



library(ggpubr)

figure_5<-ggarrange(importance_plot,shap_goose_plot,
                            shap_barnacles_plot,shap_mussel_plot,
                            shap_spat_plot,shap_red_plot,
                            shap_brown_plot,shap_green_plot,
                            ncol=2,nrow=4,align="hv",
                    widths = c(1,1))


windows();figure_5

library(svglite)

ggsave(plot=figure_5,file = "FIGURES/Figure_5.svg",device= svglite,
       height=16,width =11,units="in")



figure_5<-ggpubr::ggarrange(importance_plot,shap_goose_plot,ncol=2,align="hv")
figure_5




