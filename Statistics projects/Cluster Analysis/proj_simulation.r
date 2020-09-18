library(lmf)
library(EMMIXskew)
library(cluster)
library(MixGHD)
library(dbscan)
library(ggplot2)
library(RColorBrewer)
library(mvtnorm)

# To find the epsilon where difference is maximum.
find.eps = function(data, k){# For k=10
  d=sort(apply(kNNdist(data, k=k),1,mean))
  bin = 50
  m=nrow(data)%/%bin
  diff1 = d[(m+1):length(d)]-d[1:(length(d)-m)]
  diff2 = diff1[-1]
  diff1 = diff1[-length(diff1)]
  diff3 = diff2 - diff1
  K = abs(diff3)/(1+(diff2)^2)^(3/2)
  eps = d[which(K==max(K))[1]] # Index for epsilon
  return(eps)
}

kmeans.g = function(data){
  kmeans.out=list()
  sil.kmeans=numeric(10)
  for(g in 1:10){
    kmeans.out[[g]]=kmeans(x = data,centers = g,iter.max=100,nstart=20)
    s.vals=silhouette(kmeans.out[[g]]$cluster,dist = dist(data))
    if(!is.na(s.vals)) sil.kmeans[g]=mean(s.vals[,3])
  }
  return(which(sil.kmeans==max(sil.kmeans)))
}

pam.g = function(data){
  pam.out=list()
  sil.pam=numeric(10)
  for(g in 2:10){
    pam.out[[g]]=pam(x = data,k = g)
    s.val=pam.out[[g]]$silinfo$avg.width
    if(!is.na(s.val)) sil.pam[g]=s.val
  }
  return(which(sil.pam==max(sil.pam)))
}

mst.fcn = function(data){
  out <- list()
  bic.mst <- rep(NA,10)
  for(g in 1:10){
    tryCatch({
      cat('mst: g=',g,'started.\n')
      out[[g]] <- EmSkew(dat=data,g=g,distr="mst",debug=FALSE) 
      bic.mst[g] = out[[g]]$bic
    }, error = function(e) {
      cat('err msg:',e$message,'\n')
    }, finally = {
      next
    })
  }
  g = which(bic.mst==min(bic.mst,na.rm=T))[1]
  return(out[[g]])
}

######## All 12 simulation ARI results are saved here.
load('resARI1.rdata')
load('resARI2.rdata')
load('resARI3.rdata')
load('resARI4.rdata')
load('resARI5.rdata')
load('resARI6.rdata')
load('resARI7.rdata')
load('resARI8.rdata')
load('resARI9.rdata')
load('resARI10.rdata')
load('resARI11.rdata')
load('resARI12.rdata')
######################################################
# 12 Simulations code:

# Simulation #1
# Clearly separated
# clusters: 2
# distributions: (gaussian, gaussian)
# correlation: (none, none)
# dimension: (5, 5)
# sample size: (336, 615)
labA=c(rep(1,336),rep(2,615))

resARI = matrix(NA, 10, 4)
for(i in 1:10){cat('sim1:',i,'th loop started.\n')
  
  c1=rmnorm(336,mean=c(0,1,0,1,5),diag(5))
  c2=rmnorm(615,mean=c(3,4,12,3,2),diag(5))
  data=rbind(c1,c2)
  
  t1=kmeans(data,kmeans.g(data),iter.max=100,nstart=20)
  t2=pam(data,pam.g(data))
  t3=dbscan(data,eps = find.eps(data,k=10),minPts = 10)
  t4=mst.fcn(data)
  
  resARI[i,1]=ARI(t1$cluster,labA)
  resARI[i,2]=ARI(t2$clustering,labA)
  resARI[i,3]=ARI(t3$clust,labA)
  resARI[i,4]=ARI(t4$clust,labA)
}
resARI1 = resARI

# Simulation #2
# Clearly separated
# clusters: 4
# distributions: (gaussian, gaussian, gaussian, t)
# correlation: (none, none, none, none, none)
# dimension: (5, 5, 5, 5)
# sample size: (336, 615, 400, 500)
labB=c(rep(1,336),rep(2,615),rep(3,400),rep(4,500))

resARI = matrix(NA, 10, 4)
for(i in 1:10){cat('sim2:',i,'th loop started.\n')
  c1=rmnorm(336,mean=c(0,1,0,1,5),diag(5))
  c2=rmnorm(615,mean=c(3,4,12,3,2),diag(5))
  c3 = rmnorm(400, mean = c(-5,-2,-1,30,20),diag(5))
  c4 = rmvt(500,sigma = diag(5),df = 3)+10
  data=rbind(c1,c2,c3,c4)
  
  t1=kmeans(data,kmeans.g(data),iter.max=100,nstart=20)
  t2=pam(data,pam.g(data))
  t3=dbscan(data,eps = find.eps(data,k=10),minPts = 10)
  t4=mst.fcn(data)
  
  resARI[i,1]=ARI(t1$cluster,labB)
  resARI[i,2]=ARI(t2$clustering,labB)
  resARI[i,3]=ARI(t3$clust,labB)
  resARI[i,4]=ARI(t4$clust,labB)
}
resARI2 = resARI

# Simulation #3
# Clearly separated
# clusters: 2
# distributions: (gaussian, gaussian)
# correlation: (none, moderately positive)
# dimension: (5, 5)
# sample size: (336, 615)

s=matrix(1,5,5)
s[lower.tri(s)]=c(0.8,0.4,0.5,0.6,0.5,0.8,0.6,0.7,0.4,0.7)
s[upper.tri(s)] = t(s)[upper.tri(s)]

resARI = matrix(NA, 10, 4)
for(i in 1:10){cat('sim3:',i,'th loop started.\n')
  c1=rmnorm(336,mean=c(0,1,0,1,5),diag(5))
  c2=rmnorm(615,mean=c(3,4,12,3,2),s)
  data=rbind(c1,c2)
  
  t1=kmeans(data,kmeans.g(data),iter.max=100,nstart=20)
  t2=pam(data,pam.g(data))
  t3=dbscan(data,eps = find.eps(data,k=10),minPts = 10)
  t4=mst.fcn(data)
  
  resARI[i,1]=ARI(t1$cluster,labA)
  resARI[i,2]=ARI(t2$clustering,labA)
  resARI[i,3]=ARI(t3$clust,labA)
  resARI[i,4]=ARI(t4$clust,labA)
}
resARI3 = resARI


# Simulation #4
# Clearly separated
# clusters: 4
# distributions: (gaussian, gaussian, gaussian, t)
# correlation: (none, moderately positive, moderately positive, moderately positive)
# dimension: (5, 5, 5, 5)
# sample size: (336, 615, 400, 500)

s1=matrix(1,5,5)
s1[lower.tri(s1)]=c(0.6,0.4,0.8,0.4,0.7,0.5,0.6,0.8,0.5,0.7)
s1[upper.tri(s1)] = t(s1)[upper.tri(s1)]
s2=matrix(1,5,5)
s2[lower.tri(s2)]=c(0.7,0.4,0.6,0.5,0.5,0.8,0.4,0.6,0.7,0.8)
s2[upper.tri(s2)] = t(s2)[upper.tri(s2)]

resARI = matrix(NA, 10, 4)
for(i in 1:10){cat('sim4:',i,'th loop started.\n')
  c1=rmnorm(336,mean=c(0,1,0,1,5),diag(5))
  c2=rmnorm(615,mean=c(3,4,12,3,2),s)
  c3=rmnorm(400,mean = c(-5,-2,-2,-10,-1),s1)
  c4=rmvt(500,sigma = s2,df = 3)+10
  data=rbind(c1,c2,c3,c4)
  
  t1=kmeans(data,kmeans.g(data),iter.max=100,nstart=20)
  t2=pam(data,pam.g(data))
  t3=dbscan(data,eps = find.eps(data,k=10),minPts = 10)
  t4=mst.fcn(data)
  
  resARI[i,1]=ARI(t1$cluster,labB)
  resARI[i,2]=ARI(t2$clustering,labB)
  resARI[i,3]=ARI(t3$clust,labB)
  resARI[i,4]=ARI(t4$clust,labB)
}
resARI4 = resARI


# Simulation #5
# Clearly Separated
# clusters: 2
# distributions: (gaussian, skew-gaussian)
# correlation: (none, moderately positive)
# dimension: (5, 5)
# sample size: (336, 615)

resARI = matrix(NA, 10, 4)
for(i in 1:10){cat('sim5:',i,'th loop started.\n')
  c1=rmnorm(336,mean=c(0,1,0,1,5),diag(5))
  c2=rdmsn(615, 5, c(3,4,12,3,2), s, del=c(15,3,1,2,0))
  data=rbind(c1,c2)
  
  t1=kmeans(data,kmeans.g(data),iter.max=100,nstart=20)
  t2=pam(data,pam.g(data))
  t3=dbscan(data,eps = find.eps(data,k=10),minPts = 10)
  t4=mst.fcn(data)
  
  resARI[i,1]=ARI(t1$cluster,labA)
  resARI[i,2]=ARI(t2$clustering,labA)
  resARI[i,3]=ARI(t3$clust,labA)
  resARI[i,4]=ARI(t4$clust,labA)
}
resARI5 = resARI

# Simulation #6
# Clearly Separated
# clusters: 4
# distributions: (gaussian, skew-gaussian, gaussian, t)
# correlation: (none, moderately positive, moderately positive, moderately positive)
# dimension: (5, 5, 5, 5)
# sample size: (336, 615, 400, 500)

resARI = matrix(NA, 10, 4)
for(i in 1:10){cat('sim6:',i,'th loop started.\n')
  c1=rmnorm(336,mean=c(0,1,0,1,5),diag(5))
  c2=rdmsn(615, 5, c(3,4,12,3,2), s, del=c(15,3,1,2,0))
  c3=rmnorm(400,mean = c(-5,-2,-2,-10,-1),s1)
  c4=rmvt(500,sigma = s2,df = 3)-10
  data=rbind(c1,c2,c3,c4)
  
  t1=kmeans(data,kmeans.g(data),iter.max=100,nstart=20)
  t2=pam(data,pam.g(data))
  t3=dbscan(data,eps = find.eps(data,k=10),minPts = 10)
  t4=mst.fcn(data)
  
  resARI[i,1]=ARI(t1$cluster,labB)
  resARI[i,2]=ARI(t2$clustering,labB)
  resARI[i,3]=ARI(t3$clust,labB)
  resARI[i,4]=ARI(t4$clust,labB)
}
resARI6 = resARI


# Simulation #7
# Overlapping
# clusters: 2
# distributions (gaussian, gaussian)
# correlation: (none, none)
# dimension: (5, 5)
# sample size: (336, 615)


resARI = matrix(NA, 10, 4)
for(i in 1:10){cat('sim7:',i,'th loop started.\n')
  c1=rmnorm(336,mean=c(0,1,0,1,5),diag(5))
  c2=rmnorm(615,mean=c(0, 0.5, -0.5, 1, 5),diag(5))
  data=rbind(c1,c2)
  
  t1=kmeans(data,kmeans.g(data),iter.max=100,nstart=20)
  t2=pam(data,pam.g(data))
  t3=dbscan(data,eps = find.eps(data,k=10),minPts = 10)
  t4=mst.fcn(data)
  
  resARI[i,1]=ARI(t1$cluster,labA)
  resARI[i,2]=ARI(t2$clustering,labA)
  resARI[i,3]=ARI(t3$clust,labA)
  resARI[i,4]=ARI(t4$clust,labA)
}
resARI7 = resARI


# Simulation #8
# Overlapping
# clusters: 4
# distributions (gaussian, gaussian, gaussian, skew-t)
# correlation: (none, none, none, none)
# dimension: (5, 5, 5, 5)
# sample size: (336, 615, 400, 500)


resARI = matrix(NA, 10, 4)
for(i in 1:10){cat('sim8:',i,'th loop started.\n')
  c1=rmnorm(336,mean=c(0,1,0,1,5),diag(5))
  c2=rmnorm(615,mean=c(0, 0.5, -0.5, 1, 5),diag(5))
  c3 = rmnorm(400, mean = c(0.5,1,0,0,3),diag(5))
  c4 = rdmst(n = 500,p = 5,mean = c(-1,2,1,0,5), cov = diag(5),nu = 3,del = c(3,5,7,-4,1))
  data=rbind(c1,c2,c3,c4)
  
  t1=kmeans(data,kmeans.g(data),iter.max=100,nstart=20)
  t2=pam(data,pam.g(data))
  t3=dbscan(data,eps = find.eps(data,k=10),minPts = 10)
  t4=mst.fcn(data)
  
  resARI[i,1]=ARI(t1$cluster,labB)
  resARI[i,2]=ARI(t2$clustering,labB)
  resARI[i,3]=ARI(t3$clust,labB)
  resARI[i,4]=ARI(t4$clust,labB)
}
resARI8 = resARI



# Simulation #9
# Overlapping
# clusters: 2
# distributions: (gaussian, gaussian)
# correlation: (none, moderately positive)
# dimension: (5, 5)
# sample size: (336, 615)


resARI = matrix(NA, 10, 4)
for(i in 1:10){cat('sim9:',i,'th loop started.\n')
  c1=rmnorm(336,mean=c(0,1,0,1,5),diag(5))
  c2=rmnorm(615,mean=c(0, 0.5, -0.5, 1, 5),s)
  data=rbind(c1,c2)
  
  t1=kmeans(data,kmeans.g(data),iter.max=100,nstart=20)
  t2=pam(data,pam.g(data))
  t3=dbscan(data,eps = find.eps(data,k=10),minPts = 10)
  t4=mst.fcn(data)
  
  resARI[i,1]=ARI(t1$cluster,labA)
  resARI[i,2]=ARI(t2$clustering,labA)
  resARI[i,3]=ARI(t3$clust,labA)
  resARI[i,4]=ARI(t4$clust,labA)
}
resARI9 = resARI


# Simulation #10
# Overlapping
# clusters: 4
# distributions: (gaussian, gaussian, gaussian, skew-t)
# correlation: (none, moderately positive, moderately positive, moderately positive)
# dimension: (5, 5, 5, 5)
# sample size: (336, 615, 400, 500)


resARI = matrix(NA, 10, 4)
for(i in 1:10){cat('sim10:',i,'th loop started.\n')
  c1=rmnorm(336,mean=c(0,1,0,1,5),diag(5))
  c2=rmnorm(615,mean=c(0, 0.5, -0.5, 1, 5),s)
  c3 = rmnorm(400, mean = c(0.5,1,0,0,3),s1)
  c4 = rdmst(n = 500,p = 5,mean = c(-1,2,1,0,5), cov = s2,nu = 3,del = c(3,5,7,-4,1))
  data=rbind(c1,c2,c3,c4)
  
  t1=kmeans(data,kmeans.g(data),iter.max=100,nstart=20)
  t2=pam(data,pam.g(data))
  t3=dbscan(data,eps = find.eps(data,k=10),minPts = 10)
  t4=mst.fcn(data)
  
  resARI[i,1]=ARI(t1$cluster,labB)
  resARI[i,2]=ARI(t2$clustering,labB)
  resARI[i,3]=ARI(t3$clust,labB)
  resARI[i,4]=ARI(t4$clust,labB)
}
resARI10 = resARI


# Simulation #11
# Overlapping
# clusters: 2
# distributions: (gaussian, skew-gaussian)
# correlation: (none, moderately positive)
# dimension: (5, 5)
# sample size: (336, 615)


resARI = matrix(NA, 10, 4)
for(i in 1:10){cat('sim11:',i,'th loop started.\n')
  c1=rmnorm(336,mean=c(0,1,0,1,5),diag(5))
  c2=rdmsn(615,5,mean=c(0, 0.5, -0.5, 1, 5),s,del=c(15,3,1,2,0))
  data=rbind(c1,c2)
  
  t1=kmeans(data,kmeans.g(data),iter.max=100,nstart=20)
  t2=pam(data,pam.g(data))
  t3=dbscan(data,eps = find.eps(data,k=10),minPts = 10)
  t4=mst.fcn(data)
  
  resARI[i,1]=ARI(t1$cluster,labA)
  resARI[i,2]=ARI(t2$clustering,labA)
  resARI[i,3]=ARI(t3$clust,labA)
  resARI[i,4]=ARI(t4$clust,labA)
}
resARI11 = resARI


# Simulation #12
# Overlapping
# clusters: 4
# distributions: (gaussian, skew-gaussian, gaussian, skew-t)
# correlation: (none, moderately positive, moderately positive, moderately positive)
# dimension: (5, 5, 5, 5)
# sample size: (336, 615, 400, 500)


resARI = matrix(NA, 10, 4)
for(i in 1:10){cat('sim12:',i,'th loop started.\n')
  c1=rmnorm(336,mean=c(0,1,0,1,5),diag(5))
  c2=rdmsn(615,5,mean=c(0, 0.5, -0.5, 1, 5),s,del=c(15,3,1,2,0))
  c3 = rmnorm(400, mean = c(0.5,1,0,0,3),s1)
  c4 = rdmst(n = 500,p = 5,mean = c(-1,2,1,0,5), cov = s2,nu = 3,del = c(3,5,7,-4,1))
  data=rbind(c1,c2,c3,c4)
  
  t1=kmeans(data,kmeans.g(data),iter.max=100,nstart=20)
  t2=pam(data,pam.g(data))
  t3=dbscan(data,eps = find.eps(data,k=10),minPts = 10)
  t4=mst.fcn(data)
  
  resARI[i,1]=ARI(t1$cluster,labB)
  resARI[i,2]=ARI(t2$clustering,labB)
  resARI[i,3]=ARI(t3$clust,labB)
  resARI[i,4]=ARI(t4$clust,labB)
}
resARI12 = resARI