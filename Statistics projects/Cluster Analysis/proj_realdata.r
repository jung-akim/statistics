# For mixed data, we compare agglomerative, pam, k-prototypes
# For continuous data, we compare agglomerative, dbscan, k-means, pam, and mixture models
library(cluster)
library(mixture)
library(teigen)
library(EMMIXskew)
library(e1071)
library(dbscan)
library(clustMixType)#kproto
library(FactoMineR)#FAMD
library(factoextra)#fviz_mfa_ind

########### dataset1. Tetragonula ##################################################################
# 15 variables, 236 observations, different cluster size, mixed data(mainly cat)

load('Tetragonula.rdata')# L1-L13 are factors, C1-C2 are locations(cont.), last is label(factor)
Species=Tetragonula$Species

#0) Dim Reduction
Tetragonula=Tetragonula[1:15]
res.famd = FAMD(Tetragonula,graph=F)
# fviz_contrib(res.famd, "var", axes = 1)
fviz_famd_var(res.famd, repel = TRUE,labelsize = 8)+
  theme(axis.title = element_text(size = 25),axis.text = element_text(size = 25))
# fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
              # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              # repel = TRUE)
# fviz_contrib(res.famd,choice='var',axes=1)
# fviz_contrib(res.famd,choice='var',axes=2)

# quanti.var = get_famd_var(res.famd,'quanti.var')
# quali.var <- get_famd_var(res.famd, "quali.var")
# dim1=quali.var$cos2[,1]
# dim2=quali.var$cos2[,2]
# res.famd$eig# variances of the components
# fviz_eig(res.famd)

# 2-component plot of the True labels
# fviz_mfa_ind(res.famd,geom='point',
#              habillage = "Species", # color by groups
#              palette = 'Paired',
#              addEllipses = TRUE, ellipse.type = "confidence"
# )

# Coordinates of the 2-d plot
indv = get_famd_ind(res.famd)
xy=indv$coord[,c(1,2)]
# plot(xy[,1],xy[,2],col=as.numeric(Species)+1,xlab='Dim1',ylab='Dim2',pch=16,cex.main=3,cex.lab=3)

gower.dist=daisy(Tetragonula,metric='gower')

#1) k-prototypes
#lambdaest()# Bias param towards Categorical vars ( zero is same as k-means )
load('Tetragonula.kp.rdata')
# Tetragonula.kp2=kproto(x = Tetragonula[,1:15],k = 2,lambda = lambdaest(Tetragonula[,1:15],outtype='vector'),iter.max = 100,nstart = 20)
# Tetragonula.kp3=kproto(x = Tetragonula[,1:15],k = 3,lambda = lambdaest(Tetragonula[,1:15],outtype='vector'),iter.max = 100,nstart = 20)
# Tetragonula.kp4=kproto(x = Tetragonula[,1:15],k = 4,lambda = lambdaest(Tetragonula[,1:15],outtype='vector'),iter.max = 100,nstart = 20)
# Tetragonula.kp5=kproto(x = Tetragonula[,1:15],k = 5,lambda = lambdaest(Tetragonula[,1:15],outtype='vector'),iter.max = 100,nstart = 20)
# Tetragonula.kp6=kproto(x = Tetragonula[,1:15],k = 6,lambda = lambdaest(Tetragonula[,1:15],outtype='vector'),iter.max = 100,nstart = 20)
# Tetragonula.kp7=kproto(x = Tetragonula[,1:15],k = 7,lambda = lambdaest(Tetragonula[,1:15],outtype='vector'),iter.max = 100,nstart = 20)
# Tetragonula.kp8=kproto(x = Tetragonula[,1:15],k = 8,lambda = lambdaest(Tetragonula[,1:15],outtype='vector'),iter.max = 100,nstart = 20)
# Tetragonula.kp9=kproto(x = Tetragonula[,1:15],k = 9,lambda = lambdaest(Tetragonula[,1:15],outtype='vector'),iter.max = 100,nstart = 20)
# Tetragonula.kp10=kproto(x = Tetragonula[,1:15],k = 10,lambda = lambdaest(Tetragonula[,1:15],outtype='vector'),iter.max = 100,nstart = 20)
# s2=silhouette(Tetragonula.kp2$cluster,dist=gower.dist)
# s3=silhouette(Tetragonula.kp3$cluster,dist=gower.dist)
# s4=silhouette(Tetragonula.kp4$cluster,dist=gower.dist)
# s5=silhouette(Tetragonula.kp5$cluster,dist=gower.dist)
# s6=silhouette(Tetragonula.kp6$cluster,dist=gower.dist)
# s7=silhouette(Tetragonula.kp7$cluster,dist=gower.dist)
# s8=silhouette(Tetragonula.kp8$cluster,dist=gower.dist)
# s9=silhouette(Tetragonula.kp9$cluster,dist=gower.dist)
# s10=silhouette(Tetragonula.kp10$cluster,dist=gower.dist)
# means=c(0,mean(s2[,3]),mean(s3[,3]),mean(s4[,3]),mean(s5[,3]),mean(s6[,3]),mean(s7[,3]),mean(s8[,3]),mean(s9[,3]),mean(s10[,3]))
# which(max(means)==means) #3  0.3459160

#2) Average Agglomerative Clustering
a1 = agnes(gower.dist,method = 'average')#highest coph cor
# a2 = agnes(gower.dist,method = 'ward')
# a3 = agnes(gower.dist,method = 'single')
# a4 = agnes(gower.dist,method = 'complete')
# plot(a1,which.plots=2)
# table(cutree(a1,4))
# cor(cophenetic(a1),gower.dist)#0.960604 highest
# plot(a2,which.plots=2)
# table(cutree(a2,3))
# cor(cophenetic(a2),gower.dist)#0.8807322
# plot(a3,which.plots=2)
# table(cutree(a3,2))
# cor(cophenetic(a3),gower.dist)#0.9222768
# plot(a4,which.plots=2)
# table(cutree(a4,3))
# cor(cophenetic(a4),gower.dist)#0.8861283
agnes.clusters=cutree(a1,4)

#3) PAM
p3=pam(x = gower.dist,k = 3,diss = T)
# plot(silhouette(p3))# best silhouette avg width 0.35
# tb=table(p3$clustering,Species)
# classAgreement(tb)$crand#0.4426006

# PLOTS
graphics.off()
par(mfrow=c(1,4))
load('species.cols.rdata')
# legend('topright',legend = c('noise'),col = c('black'),pch=4,y.intersp=0.5,x.intersp=0.5,text.width=0.05)
plot(xy[,1],xy[,2],col=species.cols,main='9 Species',pch=16,xlab='',ylab='',cex.main=2,cex.axis=2)#9cls
plot(xy[,1],xy[,2],col=Tetragonula.kp$cluster+1,main='KPROTO-3cls',pch=16,xlab='',ylab='',cex.main=2)#3cls
plot(xy[,1],xy[,2],col=agnes.clusters+1,main='AVGAGGM-3cls, 1outlr',pch=16,xlab='',ylab='',cex.main=2)#4cls
plot(xy[,1],xy[,2],col=p3$clustering+1,main='PAM-3cls',pch=16,xlab='',ylab='',cex.main=2)#3cls
plot(xy[,1],xy[,2],col=p3$clustering+1,main='PAM-3cls',pch='',xlab='',ylab='',cex.main=2)#3cls
# text(xy[,1],xy[,2],labels=1:236,cex=.7,font=2) # To find the obs num
classAgreement(table(Tetragonula.kp$cluster,Species))$crand #0.4327512
classAgreement(table(agnes.clusters,Species))$crand #0.4822226
classAgreement(table(p3$clustering,Species))$crand #0.4426006
classAgreement(table(agnes.clusters,p3$clustering))$crand # agnes and pam similar(0.9437937)
classAgreement(table(agnes.clusters,Tetragonula.kp$cluster))$crand # 0.9830351
classAgreement(table(Tetragonula.kp$cluster,p3$clustering))$crand # 0.9792704
# PAM and Agglomerative clusters are very similar
# K-prototypes could not capture outliers and different clusters class ag is very low.
# Agglomerative didn't capture noise. Therefore the classag is also low.

# http://ifcs.boku.ac.at/repository/data/tetragonula_bee/index.html


########### dataset2. Schools --- Mixed Data 445 obs of 5 variables(first is name) ######################

load('school.rdata')
GeneralType=school$General.Type
school = school[,c(2,3,4,6)]

#0) Dim Reduction
res.famd=FAMD(school,graph=F)
indv = get_famd_ind(res.famd)
xy=indv$coord[,c(1,2)]
# plot(xy[,1],xy[,2])

fviz_famd_var(res.famd, repel = TRUE,labelsize=10)+
  theme(axis.title = element_text(size = 25),axis.text = element_text(size = 25))
# fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
              # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              # repel = TRUE)
# quanti.var = get_famd_var(res.famd,'quanti.var')
# quali.var <- get_famd_var(res.famd, "quali.var")
# dim1=quali.var$cos2[,1]
# dim2=quali.var$cos2[,2]
# res.famd$eig# variances of the components
# fviz_eig(res.famd)

gower.dist = daisy(school,metric='gower')

#1) K-prototypes
load('school.kp.rdata')
# school.kp2=kproto(x = school,k = 2,lambda = lambdaest(school,outtype='vector'),iter.max = 100,nstart = 20)
# school.kp3=kproto(x = school,k = 3,lambda = lambdaest(school,outtype='vector'),iter.max = 100,nstart = 20)
# school.kp4=kproto(x = school,k = 4,lambda = lambdaest(school,outtype='vector'),iter.max = 100,nstart = 20)
# school.kp5=kproto(x = school,k = 5,lambda = lambdaest(school,outtype='vector'),iter.max = 100,nstart = 20)
# school.kp6=kproto(x = school,k = 6,lambda = lambdaest(school,outtype='vector'),iter.max = 100,nstart = 20)
# school.kp7=kproto(x = school,k = 7,lambda = lambdaest(school,outtype='vector'),iter.max = 100,nstart = 20)
# school.kp8=kproto(x = school,k = 8,lambda = lambdaest(school,outtype='vector'),iter.max = 100,nstart = 20)
# school.kp9=kproto(x = school,k = 9,lambda = lambdaest(school,outtype='vector'),iter.max = 100,nstart = 20)
# school.kp10=kproto(x = school,k = 10,lambda = lambdaest(school,outtype='vector'),iter.max = 100,nstart = 20)
# s2=silhouette(school.kp2$cluster,dist=gower.dist)
# s3=silhouette(school.kp3$cluster,dist=gower.dist)
# s4=silhouette(school.kp4$cluster,dist=gower.dist)
# s5=silhouette(school.kp5$cluster,dist=gower.dist)
# s6=silhouette(school.kp6$cluster,dist=gower.dist)
# s7=silhouette(school.kp7$cluster,dist=gower.dist)
# s8=silhouette(school.kp8$cluster,dist=gower.dist)
# s9=silhouette(school.kp9$cluster,dist=gower.dist)
# s10=silhouette(school.kp10$cluster,dist=gower.dist)
# means=c(0,mean(s2[,3]),mean(s3[,3]),mean(s4[,3]),mean(s5[,3]),mean(s6[,3]),mean(s7[,3]),mean(s8[,3]),mean(s9[,3]),mean(s10[,3]))
# which(max(means)==means) #5 0.4347429

#2) Average Agglomerative clustering
a1 = agnes(gower.dist,method = 'average')#highest coph cor
# a2 = agnes(gower.dist,method = 'ward')
# a3 = agnes(gower.dist,method = 'single')
# a4 = agnes(gower.dist,method = 'complete')
# plot(a1,which.plots=2)
# table(cutree(a1,4))
# cor(cophenetic(a1),gower.dist)#0.8446409
# plot(a2,which.plots=2)
# table(cutree(a2,4))
# cor(cophenetic(a2),gower.dist)#0.7576289
# plot(a3,which.plots=2)
# table(cutree(a3,3))
# cor(cophenetic(a3),gower.dist)#0.5296785
# plot(a4,which.plots=2)
# table(cutree(a4,4))
# cor(cophenetic(a4),gower.dist)#0.8068432
agnes.clusters=cutree(a1,4)

#3) PAM
p2 = pam(x=gower.dist,k=2,diss=T)
# p2$silinfo$avg.width# 0.4107934
# p3 = pam(x=gower.dist,k=3,diss=T)
# p3$silinfo$avg.width#[1] 0.2886766
# p4 = pam(x=gower.dist,k=4,diss=T)
# p4$silinfo$avg.width#[1] 0.2594983
# p5 = pam(x=gower.dist,k=5,diss=T)
# p5$silinfo$avg.width#[1] 0.2321867

# PLOTS
graphics.off()
par(mfrow=c(1,4))
plot(xy[,1],xy[,2],col=GeneralType,main='General 4 Types',pch=16,xlab='',ylab='',cex.main=2,cex.axis=2)
plot(xy[,1],xy[,2],col=school.kp$cluster+1,main='KPROTO-5cls',pch=16,xlab='',ylab='',cex.main=2)#3cls
plot(xy[,1],xy[,2],col=agnes.clusters+1,main='AVGAGGM-4cls',pch=16,xlab='',ylab='',cex.main=2)#4cls
plot(xy[,1],xy[,2],col=p2$clustering+1,main='PAM-2cls',pch=16,xlab='',ylab='',cex.main=2)#3cls
# text(xy[,1],xy[,2],labels=1:445,cex=.7,font=2)
classAgreement(table(GeneralType,school.kp$cluster))$crand#0.7078361
classAgreement(table(GeneralType,agnes.clusters))$crand#0.5256345
classAgreement(table(GeneralType,p2$clustering))$crand#0.4811647
# AGGLMRT captured the small clusters clearly. Works good in distance-based way
# PAM was not so good in capturing the small cluster. 


########## dataset3. Aircraft_Noise_Complaint_Data --- Mixed 4305 obs of 5 variables #####################

load('air.rdata')# Removed 1486th row where 
# Year Month              Community Total.Complaints Total.Number.of.Callers
# 1486 2016     9 Other Communities               NA                      NA

#0) Dim Reduction
res.famd=FAMD(air,graph=F)
indv = get_famd_ind(res.famd)
xy=indv$coord[,c(1,2)]
# plot(xy[,1],xy[,2])

fviz_famd_var(res.famd, repel = TRUE,labelsize=8)+
  theme(axis.title = element_text(size = 25),axis.text = element_text(size = 25))

# fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
#               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#               repel = TRUE)
# fviz_famd_var(res.famd, "quali.var", col.var = "cos2",
#               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#               repel = TRUE)


gower.dist = daisy(x = air,metric = 'gower')

#1) K-prototypes
load('air.kp.rdata')
# air.kp2=kproto(x = air,k = 2,lambda = lambdaest(air,outtype='vector'),iter.max = 100,nstart = 20)
# air.kp3=kproto(x = air,k = 3,lambda = lambdaest(air,outtype='vector'),iter.max = 100,nstart = 20)
# air.kp4=kproto(x = air,k = 4,lambda = lambdaest(air,outtype='vector'),iter.max = 100,nstart = 20)
# air.kp5=kproto(x = air,k = 5,lambda = lambdaest(air,outtype='vector'),iter.max = 100,nstart = 20)
# air.kp6=kproto(x = air,k = 6,lambda = lambdaest(air,outtype='vector'),iter.max = 100,nstart = 20)
# air.kp7=kproto(x = air,k = 7,lambda = lambdaest(air,outtype='vector'),iter.max = 100,nstart = 20)
# air.kp8=kproto(x = air,k = 8,lambda = lambdaest(air,outtype='vector'),iter.max = 100,nstart = 20)
# air.kp9=kproto(x = air,k = 9,lambda = lambdaest(air,outtype='vector'),iter.max = 100,nstart = 20)
# air.kp10=kproto(x = air,k = 10,lambda = lambdaest(air,outtype='vector'),iter.max = 100,nstart = 20)
# s2=silhouette(air.kp2$cluster,dist=gower.dist)
# s3=silhouette(air.kp3$cluster,dist=gower.dist)
# s4=silhouette(air.kp4$cluster,dist=gower.dist)
# s5=silhouette(air.kp5$cluster,dist=gower.dist)
# s6=silhouette(air.kp6$cluster,dist=gower.dist)
# s7=silhouette(air.kp7$cluster,dist=gower.dist)
# s8=silhouette(air.kp8$cluster,dist=gower.dist)
# s9=silhouette(air.kp9$cluster,dist=gower.dist)
# s10=silhouette(air.kp10$cluster,dist=gower.dist)
# means=c(0,mean(s2[,3]),mean(s3[,3]),mean(s4[,3]),mean(s5[,3]),mean(s6[,3]),mean(s7[,3]),mean(s8[,3]),mean(s9[,3]),mean(s10[,3]))
# which(max(means)==means) #2     0.16587213


#2) Average Agglomerative Clustering
# a1 = agnes(gower.dist,method = 'average')
# a2 = agnes(gower.dist,method = 'ward')
# a3 = agnes(gower.dist,method = 'single')
# a4 = agnes(gower.dist,method = 'complete')
load('air_a1.rdata')
# load('air_a2.rdata')
# load('air_a3.rdata')
# load('air_a4.rdata')
# plot(a1,which.plots=2)
# table(cutree(a1,3))
# cor(cophenetic(a1),gower.dist)#0.4782729 highest coph cor
# plot(a2,which.plots=2)
# table(cutree(a2,14))
# cor(cophenetic(a2),gower.dist)#0.3555197
# plot(a3,which.plots=2)
# table(cutree(a3,1))
# cor(cophenetic(a3),gower.dist)#0.1979882
# plot(a4,which.plots=2)
# table(cutree(a4,2))
# cor(cophenetic(a4),gower.dist)#0.2276106
agnes.clusters=cutree(a1,3)

#3) PAM
p2 = pam(x=gower.dist,k=2,diss=T)
# p3 = pam(x=gower.dist,k=3,diss=T)
# p4 = pam(x=gower.dist,k=4,diss=T)
# p2$silinfo$avg.width#0.1139662 max
# p3$silinfo$avg.width#
# p4$silinfo$avg.width#

# PLOTS
graphics.off()
par(mfrow=c(1,3))
plot(xy[,1],xy[,2],col=air.kp$cluster+1,main='KPROTO-2cls',pch=3,xlab='',ylab='',cex.main=2,cex.axis=2)#2cls
plot(xy[,1],xy[,2],col=agnes.clusters+1,main='AVGAGGM-3cls',pch=2,xlab='',ylab='',cex.main=2)#3cls
plot(xy[,1],xy[,2],col=p2$clustering+1,main='PAM-2cls',xlab='',ylab='',pch=4,cex.main=2)#2cls
# text(xy[,1],xy[,2],labels=1:445,cex=.7,font=2)
classAgreement(table(agnes.clusters,p2$clustering))$crand#-0.01139225
classAgreement(table(agnes.clusters,air.kp$cluster))$crand#-0.01476653
classAgreement(table(p2$clustering,air.kp$cluster))$crand#0.5732219


########### dataset4. travel --- 5456 users(observations) 24 continuous variables #######################

load('travel.rdata')# 2 Na's filled with the average([2713,12], [1348,24])

#0) Dim Reduction
res.pca=princomp(scale(travel))
# plot(res.pca)
# biplot(res.pca, xlabs = rep(".", nrow(travel)),choices = c(1,2))
# summary(res.pca)
xy=res.pca$scores[,c(1,2)]
# plot(xy[,1],xy[,2])


fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), labelsize=6,
             repel = TRUE # Avoid text overlapping
)+theme(axis.title = element_text(size = 17),axis.text = element_text(size = 17))


#1) Mixture Models
load('travel_mst.out.rdata')
load('travel_msn.out.rdata')
load('travel_t.out.rdata')
load('travel_gmm.fit.rdata')
# mst.out <- list()#8
# bic.mst <- numeric(10)
# msn.out <- list()#24
# bic.msn <- numeric(30)
# for(g in 1:30){
#   cat('group',g,'started.\n')
  # mst.out[[g]] <- EmSkew(dat=as.matrix(travel),g=g,distr="mst",debug=F)
  # bic.mst[g] <- mst.out[[g]]$bic
  # msn.out[[g]] <- EmSkew(dat=as.matrix(travel),g=g,distr="msn",debug=F)
  # bic.msn[g] <- msn.out[[g]]$bic
# }
# t.out = teigen(x=as.matrix(travel),Gs=1:10,models="UUUU")#4
# gmm.fit=gpcm(data = as.matrix(travel),G=1:30,mnames = 'VVV')#13

# too much overlapping in plot
# plot(xy[,1],xy[,2],col=mst.out[[8]]$clust)
# plot(xy[,1],xy[,2],col=msn.out[[24]]$clust)# Because of outliers, # of clusters increase in msn.
# plot(xy[,1],xy[,2],col=t.out$classification)
# plot(xy[,1],xy[,2],col=gmm.fit$map)

# classAgreement(table(msn.out[[24]]$clust,mst.out[[8]]$clust))$crand# 0.22
# classAgreement(table(mst.out[[8]]$clust,t.out$classification))$crand# skewed 37.5% agree
#==> skewed-t (8 clusters)

#2) Average Agglomerative Clustering
euc.dist = dist(travel)
# a1 = agnes(euc.dist,method = 'average')
# a2 = agnes(euc.dist,method = 'ward')
# a3 = agnes(euc.dist,method = 'single')
# a4 = agnes(euc.dist,method = 'complete')
load('travel_a1.rdata')
# load('travel_a2.rdata')
# load('travel_a3.rdata')
# load('travel_a4.rdata')

# plot(a1,which.plots=2)
# table(cutree(a1,2))
# cor(cophenetic(a1),euc.dist)# 0.8291193 highest coph cor
# plot(a2,which.plots=2)
# table(cutree(a2,2))
# cor(cophenetic(a2),euc.dist)# 0.7396052
# plot(a3,which.plots=2)
# table(cutree(a3,2))
# cor(cophenetic(a3),euc.dist)# 0.7070676
# plot(a4,which.plots=2)
# table(cutree(a4,2))
# cor(cophenetic(a4),euc.dist)# 0.7787634
agnes.clusters=cutree(a1,2)# 2 clusters

#3) DBSCAN
# kNNdistplot(travel,k=48,cex.axis=2)
# plot(sort(kNNdist(travel,k = 48)),type='l',ylab='48-NN distance',xlab='Points(sample)sorted by distance',cex.lab=1.5,cex.axis=1.5)
# abline(9,0,lty=2)
db=dbscan(travel,eps = 9,minPts =48)
# table(db$cluster)
# plot(x=xy[,1],y=xy[,2],col=db$cluster+1,pch=4)# 2 clusters 13 noises

#4) PAM
load('travel_pam.out.rdata')
# pam.out=list()#2 with max avg.sil
# sil=numeric(10)
# for(g in 2:10){
# cat('pam:',g,'groups..')
# pam.out[[g]]=pam(travel,k=g)
# sil[g]= pam.out[[g]]$silinfo$avg.width
# }

#6) K-MEANS
load('travel_kmeans.out.rdata')
# kmeans.out=list()
# sil.kmeans=numeric(9)#2 with max avg.sil
# for(g in 2:10){
#   kmeans.out[[g]]=kmeans(x = as.matrix(travel),centers = g,iter.max=100,nstart=20)
#   s.vals=silhouette(kmeans.out[[g]]$cluster,dist = dist(travel))
#   sil.kmeans[g]=mean(s.vals[,3])
# }

# PLOTS
graphics.off()
# plot(xy[,1],xy[,2],col=gmm.fit$map+1,main='Normal(13cls)',pch=5,xlab='',ylab='')#13cls
# plot(xy[,1],xy[,2],col=t.out$classification+1,main='t(5cls)',pch=5,xlab='',ylab='')#5cls
# plot(xy[,1],xy[,2],col=msn.out[[24]]$clust+1,main='MSN(24cls)',pch=5,xlab='',ylab='')#24cls
plot(xy[,1],xy[,2],col=mst.out[[8]]$clust+1,main='MST-8cls',pch=5,xlab='',ylab='',cex.main=2,cex.axis=2)#8cls
par(mfrow=c(1,4))
plot(xy[,1],xy[,2],col=agnes.clusters+1,main='AVGAGGM-2cls',pch=2,xlab='',ylab='',cex.main=2,cex.axis=2)#2cls
plot(xy[,1],xy[,2],col=db$cluster+1,main='DBSCAN-2cls&13noise',pch=18,xlab='',ylab='',cex.main=2)#2cls 13 noises
plot(xy[,1],xy[,2],col=pam.out[[2]]$clustering+1,main='PAM-2cls',xlab='',ylab='',cex.main=2)#2cls
plot(xy[,1],xy[,2],col=kmeans.out[[2]]$cluster+1,main='K-MEANS-2cls',pch=8,xlab='',ylab='',cex.main=2)#2cls

s=silhouette(mst.out[[8]]$clust,euc.dist)#-.21
plot(s)
s=silhouette(agnes.clusters,euc.dist)#.68
plot(s)
s=silhouette(db$cluster,euc.dist)#.46
plot(s)
s=silhouette(pam.out[[2]]$clustering,euc.dist)#.68
plot(s)
s=silhouette(kmeans.out[[2]]$cluster,euc.dist)#.68
plot(s)

classAgreement(table(mst.out[[8]]$clust,db$cluster))$crand#0.03527314
classAgreement(table(mst.out[[8]]$clust,agnes.clusters))$crand#0.02011206
classAgreement(table(mst.out[[8]]$clust,pam.out[[2]]$clustering))$crand#0.02033084
classAgreement(table(mst.out[[8]]$clust,kmeans.out[[2]]$cluster))$crand#0.0198204
classAgreement(table(agnes.clusters,db$cluster))$crand#0.4666089
classAgreement(table(agnes.clusters,pam.out[[2]]$clustering))$crand#0.8946439
classAgreement(table(agnes.clusters,kmeans.out[[2]]$cluster))$crand#0.9542283
classAgreement(table(pam.out[[2]]$clustering,db$cluster))$crand#0.5413908
classAgreement(table(pam.out[[2]]$clustering,kmeans.out[[2]]$cluster))$crand#0.8514029
classAgreement(table(kmeans.out[[2]]$cluster,db$cluster))$crand#0.4364427




# DBSCAN captured noise and similar-density clusters.
# Mixture of Skew-T seems plausible compared to others.(overlapped mixture models)
# AGGLMRT, PAM, K-means are equally bad.


