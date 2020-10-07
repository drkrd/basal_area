library(lidR)
library(dplyr)
library(ggplot2)


rm(list = ls())
gc()
laspath <-  "C:/1_Work/2_Ciron/Data/ALS/norm/"
lascat <-  readLAScatalog(laspath)



#read csv file
tbl <- read.csv("C:/1_Work/3_R_codes/Projects/basal_area/data/Mesures_placette_Frisbee_all_treeloc_karun.csv")

#summary of basal area
tbl_ba <- tbl %>% 
  select(Id_placette, C130CM, CX, CY, C130CM) %>% 
  mutate(BA = pi * (C130CM/(2*pi*100))^2) %>% 
  group_by(Id_placette, CX, CY) %>% 
  summarise(BA = sum(BA, na.rm = TRUE)) %>% 
  ungroup()

#subset plot id, plot centre x and y coord into another dataframe
clip_data <- as.data.frame(cbind(tbl$Id_placette, tbl$CX, tbl$CY))

#previous step results in duplicates for every plot. Unique plot information will be retained 
clip_data <- distinct(clip_data)

#plot 14 has no coordinate information. It will be dropped
clip_data <- slice(clip_data, -15)

#clip circular plots from
aois <- lasclipCircle(lascat, clip_data$V2, clip_data$V3, 15)
names(aois) <- clip_data$V1

area(aois[[1]])




myProfilesLAD = function(Z, Zmax, dZ)
{
  # creating an empty list
  #print (max(Z))
  print (Zmax)
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

Hmax_lidar <-NULL
Hmax_lidar <- lapply(aois, FUN = function(x)
{
  {
    if (is.null(x))
    {
      return(NULL)
    }
    else
    {
      return(cloud_metrics(x, max(Z)))
    }
  }
})
warnings()

meanCH <- NULL
meanCH <- lapply(aois, FUN = function(x)
{
  if (is.null(x))
  {
    return(NULL)
  }
  else
  {
    return(cloud_metrics(lasfilter(lasfilterfirst(x),
                                Z > 2 & Z < 60),
                      mean(Z)))
  }
})
warnings()

varCH <- NULL
varCH <- lapply(aois, FUN = function(x)
{
  if (is.null(x))
  {
    return(NULL)
  } else
  {
    return(cloud_metrics(lasfilter(lasfilterfirst(x),
                                Z > 2 & Z < 60),
                      var(Z)))
  }
})
warnings()

Pf <- NULL
Pf <- lapply(aois, FUN = function(x)
{
  {
    if (is.null(x))
    {
      return(NULL)
    }
    else
    {
      x <- lasfilterfirst(x)
      num <- lasfilter(x, Z <= 2)
      return(npoints(num)/npoints(x))
    }
  }
})
warnings()

CVlad <- lapply(aois, function(toto)
  {
  if (is.null(toto)) 
    {
    return(NULL)
    } 
  else
    {
    v <- NULL
    zm = min(ceiling(toto@header@PHB$'Max Z'), 60)
    #print("*****")
    print(zm)
    print("..")
    #print(max(toto@data$Z))
    v = as.vector(unlist(cloud_metrics(toto, myProfilesLAD(Z, 
                                                           Zmax = min(ceiling(toto@header@PHB$'Max Z'), 60), 
                                                           dZ = 1))))
    return(sd(v) / mean(v))
    }
  })

warnings()










Hmax_lidar=as.data.frame(unlist(Hmax_lidar))
colnames(Hmax_lidar) <- c("hmax_lidar")
Hmax_lidar$placettes=as.numeric(rownames(Hmax_lidar))

meanCH=as.data.frame(unlist(meanCH))
colnames(meanCH) <- c("meanCH")
meanCH$placettes=as.numeric(rownames(meanCH))

varCH=as.data.frame(unlist(varCH))
colnames(varCH) <- c("varCH")
varCH$placettes=as.numeric(rownames(varCH))

Pf=as.data.frame(unlist(Pf))
colnames(Pf) <- c("Pf")
Pf$placettes=as.numeric(rownames(Pf))

CVlad=as.data.frame(unlist(CVlad))
colnames(CVlad) <- c("CVlad")
CVlad$placettes=as.numeric(rownames(CVlad))

tbl_ba <- tbl_ba %>% 
  left_join(Hmax_lidar, by = c("Id_placette" = "placettes")) %>% 
  left_join(meanCH, by = c("Id_placette" = "placettes")) %>%
  left_join(varCH, by = c("Id_placette" = "placettes")) %>%
  left_join(Pf, by = c("Id_placette" = "placettes")) %>% 
  left_join(CVlad, by = c("Id_placette" = "placettes")) %>% 
  mutate(BA = (BA*10000)/(pi*15*15))

tbl_ba <- tbl_ba %>% 
  mutate(new_ba = exp(-1.71 + 1.75*log(meanCH) + 0.01*log(varCH) + 0.5*log(Pf) + 0.24*log(CVlad)))


model <- lm(log(BA)~log(meanCH)+log(varCH)+log(Pf)+log(CVlad), data = tbl_ba)
summary(model)

ggplot(data = tbl_ba)+
  aes(x=meanCH, y=varCH)+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)

tbl_ba1 <- tbl_ba1 %>% 
  mutate(newBA = )


summary(lm1)


model$fitted.values

lst <- model$fitted.values
lst <- exp(lst)
lst
plot(tbl_ba1$BA,lst)
