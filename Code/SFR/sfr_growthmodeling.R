#Lavaan growth modeling
########################################################################################
#data used 15-12-2020
df_sfr<-read.csv("/home/u034/mcswets/df_sfr20201207-2.csv")

#####################################################################################

#growth mixed modelling  using LCMM package
library(lcmm)
library(LCTMtools)
library(tidyr)
#hlme function for latent class linear mixed models  (=growth mixed models)
#make sfr dataframe into a long dataframe
df_long<-df_sfr[,c(2:7)]
df_long$numeric_id<-seq.int(nrow(df_long))
sfr_long<-gather(df_long, measurement_day, sfr_value, day_0:day_8, factor_key=T)
#hlme function for latent class linear mixed models  (=growth mixed models)
#change measurement day into a number
sfr_long$measurement_day<-gsub("day_","",sfr_long$measurement_day)
sfr_long$measurement_day<-as.numeric(sfr_long$measurement_day)
#set seed and try different number of clusters (=ng variable)
set.seed(2222)
head(sfr_long)
sfr_hlme_1<-hlme(sfr_value ~ measurement_day,
                 subject = "numeric_id", ng = 1, data=sfr_long)
sfr_hlme_2<-hlme(sfr_value ~ measurement_day,
                  mixture= ~ measurement_day,
                  subject = "numeric_id", ng = 2,
                  data=sfr_long, B=sfr_hlme_1)
sfr_hlme_3<-hlme(sfr_value ~ measurement_day,
                  mixture= ~ measurement_day,
                  subject = "numeric_id", ng = 3,
                  data=sfr_long, B=sfr_hlme_1)
sfr_hlme_4<-hlme(sfr_value ~ measurement_day,
                  mixture= ~ measurement_day,
                  subject = "numeric_id", ng = 4,
                  data=sfr_long, B=sfr_hlme_1)
sfr_hlme_5<-hlme(sfr_value ~ measurement_day,
                 mixture= ~ measurement_day,
                 subject = "numeric_id", ng = 5,
                 data=sfr_long, B=sfr_hlme_1)
#pick HLME with lowers BIC value + clinically relevant
summarytable(sfr_hlme_1, sfr_hlme_2, sfr_hlme_3, sfr_hlme_4)
summarytable(sfr_hlme_4)
#check summary for selected number of clusters
summary(sfr_hlme_4)
#https://rstudio-pubs-static.s3.amazonaws.com/522393_3aa7f65898f8426e9c0a92d7971b619d.html
#check APPA for selected number of clusters
LCTMtoolkit(sfr_hlme_4)
#visualize clusters in plot
newdata<-data.frame(measurement_day=c(0,2,4,6,8))
plot_cluster<-predictY(sfr_hlme_4, newdata, var.time="measurement_day", draws=T,
                       interval= "confidence")
plot_cluster
plot(plot_cluster, legend.loc= "topleft", lty=1, 
     xlab="Day of measurement", ylab ="SFR Value")
rpng.off()
#check size of clusters (+more)
lcmm::postprob(sfr_hlme_4)
?predictY

#add to dataframe (df_sfr)
library(dplyr)
df_sfr<-df_sfr %>%
        dplyr::rename(numeric_id= num_id)
gmmclust_dataframe<-sfr_hlme_4$pprob[,1:2]
df_sfr<-left_join(df_sfr, gmmclust_dataframe, by ="numeric_id")
df_sfr<-df_sfr %>%
        dplyr::rename(gmmclust= class)

########################################################################################
df_sfr including GMM class membership
write.csv(df_sfr, "df_sfr_17122020.csv")
df_sfr<-read.csv("/home/u034/mcswets/df_sfr_17122020.csv")
########################################################################################

#plot change in BIC with different clusters
lin<- c(sfr_hlme_1$ng, sfr_hlme_1$BIC)
lin<-rbind(lin, c(sfr_hlme_2$ng, sfr_hlme_2$BIC))
lin<-rbind(lin, c(sfr_hlme_3$ng, sfr_hlme_3$BIC))
lin<-rbind(lin, c(sfr_hlme_4$ng, sfr_hlme_4$BIC))
lin<-rbind(lin, c(sfr_hlme_5$ng, sfr_hlme_5$BIC))
head(lin)
plot(lin, xlab= "Number of clusters",
     ylab= "Bayesian information criteria (BIC)")
rpng.off()

#plot samples from time series for each cluster to see if mean predicted trajectory
# is representative of cluster
cluster1<-subset(df_sfr, df_sfr$gmmclust == 1)
cluster2<-subset(df_sfr, df_sfr$gmmclust == 2)
cluster3<-subset(df_sfr, df_sfr$gmmclust == 3)
cluster4<-subset(df_sfr, df_sfr$gmmclust == 4)
library(ggplot2)
cluster1_long<-gather(cluster1, measurement_day, sfr_value, day_0:day_8, factor_key=T)
violin_1<-ggplot(cluster1_long, aes(x=measurement_day, y=sfr_value))
v1<-violin_1 + geom_violin()+ 
  theme_light()+
  ggtitle("Cluster 1")+
  xlab("Measurement day")+
  ylab("SFR value")
cluster2_long<-gather(cluster2, measurement_day, sfr_value, day_0:day_8, factor_key=T)
violin_2<-ggplot(cluster2_long, aes(x=measurement_day, y=sfr_value))
v2<-violin_2 + geom_violin()+ 
  theme_light()+
  ggtitle("Cluster 2")+
  xlab("Measurement day")+
  ylab("SFR value")
cluster3_long<-gather(cluster3, measurement_day, sfr_value, day_0:day_8, factor_key=T)
violin_3<-ggplot(cluster3_long, aes(x=measurement_day, y=sfr_value))
v3<-violin_3 + geom_violin()+ 
  theme_light()+
  ggtitle("Cluster 3")+
  xlab("Measurement day")+
  ylab("SFR value")
cluster4_long<-gather(cluster4, measurement_day, sfr_value, day_0:day_8, factor_key=T)
violin_4<-ggplot(cluster4_long, aes(x=measurement_day, y=sfr_value))
v4<-violin_4 + geom_violin()+ 
  theme_light()+
  ggtitle("Cluster 4")+
  xlab("Measurement day")+
  ylab("SFR value")
#make a grid of the 4 violin plots using cowplot
install.packages("cowplot",repos="https://cran.rstudio.com/")
library(cowplot)
plot_grid(v1,v2,v3,v4)
########################################################################################
#gmmclust characteristics
tapply(df_sfr$age, df_sfr$gmmclust, quantile, na.rm = TRUE)
tapply(df_sfr$AUC_value, df_sfr$gmmclust, quantile, na.rm = TRUE)
tapply(df_sfr$worst_sfr, df_sfr$gmmclust, quantile, na.rm = TRUE)
tapply(df_sfr$onset2admission, df_sfr$gmmclust, quantile, na.rm = TRUE)
tapply(df_sfr$crp, df_sfr$gmmclust, quantile, na.rm = TRUE)
tapply(df_sfr$rr_vsorres, df_sfr$gmmclust, quantile, na.rm = TRUE)
tapply(df_sfr$hodur, df_sfr$gmmclust, quantile, na.rm = TRUE)
tapply(df_sfr$hosp_dur, df_sfr$gmmclust, quantile, na.rm = TRUE)
table(cluster4$resp_support)
table(cluster4$sex)
colnames(cluster1)
table(cluster3$infiltrates_faorres)
table(cluster4$stroke_ceterm)
table(cluster4$clinical_frailty)
table(cluster4$symptom_cluster)
table(cluster4$ethnicity)
sum(is.na(cluster1$ethnicity))
summary(cluster4_long$sfr_value)

stdErr <- function(x) {sd(x)/ sqrt(length(x))}
day0<-tapply(df_sfr$day_0, df_sfr$gmmclus, mean)
day0se<-tapply(df_sfr$day_0, df_sfr$gmmclus, stdErr)
day2<-tapply(df_sfr$day_2, df_sfr$gmmclus, mean)
day4<-tapply(df_sfr$day_4, df_sfr$gmmclus, mean)
day6<-tapply(df_sfr$day_6, df_sfr$gmmclus, mean)
day8<-tapply(df_sfr$day_8, df_sfr$gmmclus, mean)
day2se<-tapply(df_sfr$day_2, df_sfr$gmmclus, stdErr)
day4se<-tapply(df_sfr$day_4, df_sfr$gmmclus, stdErr)
day6se<-tapply(df_sfr$day_6, df_sfr$gmmclus, stdErr)
day8se<-tapply(df_sfr$day_8, df_sfr$gmmclus, stdErr)
mean_sfr_day<-rbind(day0, day2, day4, day6, day8)
se_sfr_day<-rbind(day0se, day2se, day4se, day6se, day8se)
clusters<-c("1", "2", "3", "4")

#switch columns and rows
mean_sfr_day<-t(mean_sfr_day)
se_sfr_day<-t(se_sfr_day)
mean_sfr_day<-data.frame(mean_sfr_day)
se_sfr_day<-data.frame(se_sfr_day)
mean_sfr_day<-cbind(mean_sfr_day, clusters)
se_sfr_day<-cbind(se_sfr_day, clusters)
meansfrday_long<-gather(mean_sfr_day, 
                        measurement_day, sfr_value, day0:day8, factor_key=T)
sesfrday_long<-gather(se_sfr_day, 
                        measurement_day, SE_value, day0se:day8se, factor_key=T)

library(dplyr)
library(ggplot2)
library(tidyr)
sfr_long_df<-cbind(meansfrday_long, sesfrday_long)
sfr_long_df<-subset(sfr_long_df[,c(1:3,6)])
sfr_long_df

ggplot(sfr_long_df, aes(x=measurement_day, 
                            y=sfr_value, colour=clusters, group= clusters)) +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=sfr_value-SE_value,
                    ymax=sfr_value+SE_value))
#plot mean+ CI non linear function
df_sfr_long<-df_sfr[,c(4:8,150)]
df_sfr_long<-gather(df_sfr_long, measurement_day, sfr_value, day_0:day_8, factor_key=T)
head(df_sfr_long)
ggplot(df_sfr_long, aes(x=measurement_day, 
                        y=sfr_value, colour=clusters, group= clusters))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, 
               fun.args=list(conf.int=0.95), fill="lightblue")+
  stat_summary(geom="line", fun.y=mean, linetype="dashed")+
  stat_summary(geom="point", fun.y=mean, color="red")


#change 0 to NO and 1 to YES for all comorbidities and complcations
df_sfr[,c(64:68,71:74,77:80,114:142,146)][df_sfr[,c(64:68,71:74,77:80,114:142,146)]== 1]<- "YES"
df_sfr[,c(64:68,71:74,77:80,114:142,146)][df_sfr[,c(64:68,71:74,77:80,114:142,146)]== 0]<- "NO"
df_sfr[,c(64:68,71:74,77:80,114:142,146)][is.na(df_sfr[,c(64:68,71:74,77:80,114:142,146)])]<- "UNKNOWN"
#complications + comorbidities
gmm_comp<-(colSums(cluster1[,c(64:68,71:74,77:80,114:142,146)]== "YES"))
gmm_comp<-(colSums(cluster2[,c(64:68,71:74,77:80,114:142,146)]== "YES"))
gmm_comp<-(colSums(cluster3[,c(64:68,71:74,77:80,114:142,146)]== "YES"))
gmm_comp<-(colSums(cluster4[,c(64:68,71:74,77:80,114:142,146)]== "YES"))
gmm_comp
colnames(cluster1)
#number of comorbidities
gmm_com_num<-(rowSums(cluster4[,c(64:68,71:74,77:80,146)]== "YES"))
table(gmm_com_num)
#number of comorbidities
gmm_compl_num<-(rowSums(cluster4[,c(114:116,118,119,122:125,129:134,136,137,139:142)]== "YES"))
table(gmm_compl_num)

####################################significance###########################################
install.packages("chisq.posthoc.test",repos="https://cran.rstudio.com/")
library(chisq.posthoc.test)
install.packages("BiocManager",repos="https://cran.rstudio.com/")
BiocManager::install("mixOmics")
install.packages("RVAideMemoire",repos="https://cran.rstudio.com/")
library(RVAideMemoire)



cardiac_arrest<-as.table(rbind(c(312, 54, 86, 35),c(4473,3677, 2249,3195)))
cardiac_arrest1<-(c(312, 54, 86, 35))
dimnames(cardiac_arrest)<-list(value=c("cardiac arrest", "no cardiac arrest"),
                               group=c("1", "2", "3", "4"))
cardiac_arrest
chisq.test(cardiac_arrest, correct=FALSE)
chisq.test(cardiac_arrest[,c(2,4)])
chisq.posthoc.test(cardiac_arrest, method= "bonferroni")
chisq.multcomp(cardiac_arrest1, p.method= "bonferroni")

test<-rbind(c(18,90),c(6,149))
chisq.test(test)
