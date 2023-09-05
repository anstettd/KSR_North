##################################################################################
# Analyses lat, climate, and mean trait data predicting seed number
## Daniel Anstett
## Last updated Jan 7, 2022
##################################################################################

# Clear environment
rm(list = ls())

#import libraries
library(tidyverse)
library(factoextra)
library(RColorBrewer)
library(ggfortify)
library(egg)



#import data
ksr_m <- read.csv("Data/ksr_m.csv", header=T) # Imports individual dataset
ksr_scale <- ksr_m %>% dplyr::select(Flowering_Date,Water_Content,Growth_Rate,Bolt_Date,SLA,Num_Trichomes,
                              Leaf_Totphe,Flower_Totphe,Fruit_Totphe,
                              Leaf_Oenothein_A,Flower_Oenothein_A,Fruit_Oenothein_A) 
ksr_scale <- as.data.frame(scale(ksr_scale))
ksr <- cbind(ksr_m %>% dplyr::select(Pop,Latitude,Longitude,MAT,Seeds),ksr_scale)
ksr <- na.omit(ksr)




##################################################################################

#Compute PCA
pca <- prcomp(ksr[,6:17], scale. = TRUE)


#Make Scree plot

library(ggplot2)


# compute total variance
variance <- pca$sdev^2 / sum(pca$sdev^2)

# Scree plot
qplot(c(1:12), variance) +
  geom_line() + geom_point(size=3)+
  xlab("Principal Component") +  ylab("Variance Explained") +
  scale_x_continuous(breaks=c(2,4,6,8,10,12))+
  ylim(0, 0.5) +  theme_minimal()+
  theme(axis.text.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"))
  ggsave("Single_fig/10_scree_plot.pdf",width=5,height=5,units="in")


# Combine the PCA results with the original data
#pca_data <- as.data.frame(cbind(pca$x, ksr))

#Get summaries
var_explained <- summary(pca)

#Proportion of variation expalined by each axis
var_explained$importance
var_importance<-as.data.frame(var_explained$importance)
var_importance[,13] <- rownames(var_importance)
write_csv(var_importance,"Tables/var_explained.csv")

#Get loadings
pca$rotation[,1:2]
pca_out<-as.data.frame(pca$rotation[,1:2])
pca_out[,3] <- rownames(pca$rotation)
colnames(pca_out) <- c("PC1","PC2","Trait")
pca_out <- pca_out %>% dplyr::select(Trait,PC1,PC2)
write_csv(pca_out,"Tables/loadings.csv")

#Get coordinates for each population
ksr_coor <- cbind(ksr,pca$x[,1:2])

write_csv(ksr_coor,"Data/ksr_coor.csv")




##################################################################################
#Make biplots
bi_lat <- autoplot(pca, data = ksr, fill = 'Latitude', loadings = TRUE, shape=21, color="Black",loadings.label=T,
         size =3,loadings.colour = "black", loadings.label.colour="black", loadings.label.hjust=1.01) +
  scale_fill_gradientn(colours = brewer.pal(11,"RdBu")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + #scale_x_reverse() +
  theme_minimal()+ 
  theme(axis.text.x = element_text(size=12, face="bold"),
        axis.text.y = element_text(size=12,face="bold"),
        axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
bi_lat
#ggsave("Single_fig/10_pca_lat.pdf",width=10,height=8,units="in")

bi_seed <- autoplot(pca, data = ksr, fill = 'Seeds', loadings = TRUE, shape=21, color="Black",loadings.label=T,
         size =3,loadings.colour = "black", loadings.label.colour="black", loadings.label.hjust=1.01) +
  #scale_fill_gradientn(colours = rev(brewer.pal(11,"RdBu"))) +
  scale_fill_gradientn(colours = brewer.pal(9,"Reds")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + #scale_x_reverse() +
  theme_minimal()+ 
  theme(axis.text.x = element_text(size=12, face="bold"),
        axis.text.y = element_text(size=12,face="bold"),
        axis.title.x = element_text(color="black", size=15, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
bi_seed
#ggsave("Single_fig/10_pca_seeds.pdf",width=10,height=8,units="in")



##################################################################################





##################################################################################
#11. Does PC1 and PC2 predict success?

plot(ksr_coor$PC1,ksr_coor$Seeds) # Unlikely 2nd order
plot(ksr_coor$PC2,ksr_coor$Seeds) # No evidence of 2nd order

qu_PC1 <- glm.nb(Seeds ~ PC1 ,data=ksr_coor)
Anova(qu_PC1,type = 3)

qu_PC2 <- glm.nb(Seeds ~ PC2 ,data=ksr_coor)
Anova(qu_PC2,type = 3)


# Nothing significant

plot11a<-visreg(qu_PC1, "PC1", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
  geom_point(size=1)+ scale_y_continuous(name="Seed Number", limits=c(0,100000),
                                         breaks=c(25000,50000,75000,100000))+
  scale_x_continuous(name="PC1")+ theme_classic()
plot11a <- plot11a +   theme(axis.text.x = element_text(size=13, face="bold"),
                             axis.text.y = element_text(size=13,face="bold"),
                             axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                             axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
plot11a
#ggsave("Single_fig/11a_PC1_seeds.pdf",width=7,height=6,units="in")


plot11b<-visreg(qu_PC2, "PC2", scale="response", partial=TRUE, gg=TRUE, line=list(col="black")) +
  geom_point(size=1)+ scale_y_continuous(name="Seed Number", limits=c(0,100000),
                                         breaks=c(25000,50000,75000,100000))+
  scale_x_continuous(name="PC2")+ theme_classic()
plot11b <- plot11b +   theme(axis.text.x = element_text(size=13, face="bold"),
                             axis.text.y = element_text(size=13,face="bold"),
                             axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                             axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.6))
plot11b
#gsave("Single_fig/11a_PC2_seeds.pdf",width=7,height=6,units="in")

## Cowplot Fig S3 export at 4 X 8 inches
plot_grid(plot11a,plot11b)

##################################################################################
#Make unified plot # export 8x10 portrait
plot11<-ggarrange(plot11a,plot11b,ncol=2)
plot_grid(bi_seed,plot11,ncol=1,rel_heights = c(5,2.5))





