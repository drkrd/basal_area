library(lidR)
library(dplyr)
library(tidyr)
library(stringr)




fid <- lapply(aois, function(aoi)
{
  if (is.null(aoi))
    {
      return(NULL)
    }
  else
    {
      aoi <- lasflightline(aoi)
      ufid <- unique(aoi@data$flightlineID)
      mean_vals <- NULL
      for(i in ufid)
      {
        aoi_tmp <- lasfilter(aoi, flightlineID == i)
        aoi_ar <- lidR::area(aoi_temp)
        aoi_mean <- abs(mean(aoi_tmp@data$ScanAngleRank))
        name <- paste("")
        mean_vals <- c(mean_vals, abs(mean(aoi_tmp@data$ScanAngleRank))) 
        writeLAS()
      }
      return(mean_vals)
    }
}
)


fid <- lapply(fid, `length<-`, max(lengths(fid)))
xy <- as.data.frame(matrix(unlist(fid), nrow=29, byrow=TRUE))
xy[is.na(xy)] <- 99
#xy <- as.data.frame(t(apply(xy, 1, sort)))
xy <- round(xy,2)
rownames(xy) <- names(fid)



ar <- lapply(aois, function(aoi)
{
  {
    if (is.null(aois))
    {
      return(NULL)
    }
    else
    {
      aoi <- lasflightline(aoi)
      ufid <- unique(aoi@data$flightlineID)
      ar_vals <- NULL
      for(i in ufid)
      {
        aoi_tmp <- lasfilter(aoi, flightlineID == i)
        ar_vals <- c(ar_vals, lidR::area(aoi_tmp)) 
      }
      return(ar_vals)
    }
  }
}
)


ar <- lapply(ar, `length<-`, max(lengths(ar)))
ar1 <- as.data.frame(matrix(unlist(ar), nrow=29, byrow=TRUE))
ar1[is.na(ar1)] <- 999
#ar1 <- as.data.frame(t(apply(ar1, 1, sort)))
ar1 <- round(ar1,2)
rownames(ar1) <- names(ar)




















nms <- names(aois)
c=1
for(aoi in aois){
  aoi <- lasflightline(aoi)
  ufid <- unique(aoi@data$flightlineID)
  mean_vals <- NULL
  for(i in ufid)
  {
    aoi_tmp <- lasfilter(aoi, flightlineID == i)
    aoi_ar <- lidR::area(aoi_tmp)
    aoi_mean <- abs(mean(aoi_tmp@data$ScanAngleRank))
    name <- paste("")
    mean_vals <- c(mean_vals, abs(mean(aoi_tmp@data$ScanAngleRank))) 
    nm=paste0(nms[c],'_',
              round(aoi_ar,2),'_',
              min(aoi_tmp@data$ScanAngleRank),'_',
              max(aoi_tmp@data$ScanAngleRank),'_',
              round(abs(mean(aoi_tmp@data$ScanAngleRank)),2),
                  '.las')
    writeLAS(aoi_tmp, nm, index=TRUE)
  }
  c=c+1
}

fls <- list.files("D:\\1_Work\\Dropbox\\3_R_codes\\Projects\\basal_area\\data\\New folder\\", pattern =" *.las")


fls1 <- as.data.frame(fls)
fls1 <- separate(fls1, col = fls, c("Plot", "Area", "Min", "Max", "Mean"), sep = "_")



fls1<- fls1 %>% 
  mutate(Plot = as.numeric(Plot),
         Area = as.numeric(Area),
         Min = as.numeric(Min),
         Max = as.numeric(Max),
         Mean = str_remove(Mean, ".las"),
         Mean = as.numeric(Mean)) %>% 
  filter(Area>695)



fls1 <- fls1 %>% 
  mutate(Class=if_else(Mean<10,"1",
                       if_else(Mean<20,"2",
                               if_else(Mean<30,"3","4"))),
         Class=as.factor(Class))
  


fls_cl1 <- fls1[which(fls1$Class=="1"),]
fls_cl2 <- fls1[which(fls1$Class=="2"),]
fls_cl3 <- fls1[which(fls1$Class=="3" | fls1$Class=="4"),]



fls_cl11 <- fls_cl1 %>%
  mutate(Plot=as.numeric(Plot)) %>% 
  group_by(Plot) %>% 
  slice(which.min(Mean-0)) %>% 
  ungroup()

fls_cl21 <- fls_cl2 %>% 
  group_by(Plot) %>% 
  slice(which.min(Mean-15)) %>% 
  ungroup()

fls_cl31 <- fls_cl3 %>% 
  group_by(Plot) %>% 
  slice(which.min(Mean-25)) %>% 
  ungroup()

  

class1 <- right_join(fls_cl11, tbl_ba, by = c("Plot" = "Id_placette"))

class2 <- right_join(fls_cl21, tbl_ba, by = c("Plot" = "Id_placette"))

class3 <- right_join(fls_cl31, tbl_ba, by = c("Plot" = "Id_placette"))




class1 <- drop_na(class1)
class2 <- drop_na(class2)
class3 <- drop_na(class3)




class1 <- class1 %>%
  mutate(fname = str_c(Plot, Area, Min, Max, Mean, sep = "_")) %>% 
  select(BA,fname)

class2 <- class2 %>%
  mutate(fname = str_c(Plot, Area, Min, Max, Mean, sep = "_")) %>% 
  select(BA,fname)

class3 <- class3 %>%
  mutate(fname = str_c(Plot, Area, Min, Max, Mean, sep = "_")) %>% 
  select(BA,fname)

setwd("D:/1_Work/Dropbox/3_R_codes/Projects/basal_area/data/New folder/")


# class1x <- class1 %>% 
#   select(BA,fname) %>% 
#   mutate(meanch=readfile(fname))
# 
# class2x <- class2 %>% 
#   select(BA,fname) %>% 
#   mutate(meanch=readfile(fname))
# 
# class3x <- class3 %>% 
#   select(BA,fname) %>% 
#   mutate(meanch=readfile(fname))






mch_cl1 <- NULL
vch_cl1 <- NULL
pf_cl1 <- NULL
clad_cl1 <- NULL

for(name in class1$fname)
{
  fn <- paste0("D:/1_Work/Dropbox/3_R_codes/Projects/basal_area/data/New folder/",name,".las")
  tmp <- readLAS(fn)
  mch_cl1 <- c(mch_cl1, func_meanch(tmp))
  vch_cl1 <- c(vch_cl1, func_varch(tmp))
  pf_cl1 <- c(pf_cl1, func_pf(tmp))
  clad_cl1 <- c(clad_cl2, func_cvlad(tmp))
}


# readfile <- function(fn)
# {
#   fn <- paste0("D:/1_Work/Dropbox/3_R_codes/Projects/basal_area/data/New folder/",fn,".las")
#   tmp <- readLAS(fn)
#   return(func_meanch(tmp))
# }















###################################################################
################Functions to calculate metrics#####################


myProfilesLAD = function(Z, Zmax, dZ)
{
  # creating an empty list
  #print (max(Z))
  #print (Zmax)
  #print ("****")
  list_lad =list()
  
  #creating layer names
  
  z_ini=c(0, Zmax)
  lad_ini=LAD(z_ini, dz=dZ, k=0.5)
  
  # initialisation of the list 
  
  for (i in 1:dim(lad_ini)[1])
  {
    list_lad[[i]] = 0
  }
  
  names(list_lad)=lad_ini$z   # adding names
  
  ##### Computation of PAD and converting the result into a list
  
  if (max(Z)>Zmax)
  {
    #print("********")
    #print(max(Z))
    #print("********")
  }
  
  Z=Z[Z<Zmax & Z>0] # to filter outliers and to keep only positive heights
  if(length(Z)>0)
  {
    profil_lad=LAD(Z, dz=dZ, k=0.5)
    if (dim(profil_lad)[1]>0)    # test to identify empty PAD profile (no vegetation above 1 m)
    {
      for (i in 1:dim(profil_lad)[1])
      { 
        list_lad[[i]]=profil_lad$lad[i] 
      }
      
    }
  }
  
  #return the result of the fucntion
  return(list_lad)
}

func_meanch <- function(aoi)
{
  if (is.null(aoi))
  {
    return(NULL)
  }
  else
  {
    aoi_f <- lasfilterfirst(aoi)
    aoi_f_h <- lasfilter(aoi_f, Z > 2 & Z < 60)
    return(mean(aoi_f_h@data$Z))
  }
}

func_varch <- function(aoi)
{
  if (is.null(aoi))
  {
    return(NULL)
  } 
  else
  {
    aoi_f <- lasfilterfirst(aoi)
    aoi_f_h <- lasfilter(aoi_f, Z > 2 & Z < 60)
    return(var(aoi_f_h@data$Z))
  }
}

func_pf <- function(aoi)
{
  if (is.null(aoi))
  {
    return(NULL)
  }
  else
  {
    aoi_f <- lasfilterfirst(aoi)
    num <- lasfilter(aoi_f, Z <= 2)
    return(npoints(num)/npoints(aoi_f))
  }
}

func_cvlad <- function(aoi)
{
  if (is.null(aoi)) 
  {
    return(NULL)
  } 
  else
  {
    v <- NULL
    zm = min(ceiling(max(aoi@data$Z)), 60)

    val = as.vector(unlist(myProfilesLAD(aoi@data$Z, zm, dZ = 1)))
    return(sd(val) / mean(val))
  }
}

func_sdint <- function(aoi)
{
  if (is.null(aoi)) 
  {
    return(NULL)
  } 
  else
  {
    aoi_f <- lasfilterfirst(aoi)
    return(sd(aoi_f@data$Intensity))
  }
  
}

func_meanint <- function(aoi)
{
  if (is.null(aoi)) 
  {
    return(NULL)
  } 
  else
  {
    aoi_f <- lasfilterfirst(aoi)
    return(mean(aoi_f@data$Intensity))
  }
  
}


mets_meanch_ran1 <- list()
mets_varch_ran1 <- list()
mets_gapfr_ran1 <- list()
mets_cvlad_ran1 <- list()


for(i in names(aois_random1))
{
  mets_meanch_ran1[i] <- func_meanch(aois_random1[[i]])
  mets_varch_ran1[i] <- func_varch((aois_random1[[i]]))
  mets_gapfr_ran1[i] <- func_pf((aois_random1[[i]]))
  mets_cvlad_ran1[i] <- func_cvlad((aois_random1[[i]]))
}

mets_cvlad_ran1[21] <- func_cvlad((aois_random1[[23]]))
#####################################################################################
##################################Regression#########################################

model_ba <- lm(log(sum_ba_hec)~log(meanch)+log(varch)+log(gapfr)+log(cvlad), data = field_data_ba_summary)

summary(model_ba)




model_ba_meanch <- lm(sum_ba_hec~gapfr, data = field_data_ba_summary)


for(name in class1$fname)
{
  rn <- paste0(name,'.las')
  x <- readLAS(rn)
  print(func_meanch(x))
}

