##reading a file 

fd <- readxl::read_xlsx("D:/1_Work/Dropbox/3_R_codes/Projects/basal_area/data/Mesures_placette_Frisbee_all_plots.xlsx",
                                sheet = "Arbres")


fd <- as.data.frame(fd)

fd_ba <- fd[,c(1,2,3,10,11,18,19)]

fd_ba_summary <- fd_ba %>%
  mutate(dbh_cm = (cbh_in_cm/pi)) %>% 
  mutate(ba_sqm = pi*((dbh_cm/200)^2)) %>% 
  dplyr::select(c(1,2,3,7,8,9)) %>% 
  group_by(id_placette, cx, cy) %>% 
  summarise(sum_ba=sum(ba_sqm, na.rm = TRUE)) %>% 
  mutate(sum_ba_hec=(10000*sum_ba)/706.8583) %>% 
  ungroup()

plot_locations <- fd_ba_summary[,c(1:3)]

length(fd_ba$Mort[fd_ba$Mort=="x"])
test <- fd_ba[!is.na(fd_ba_smry$Mort),]
print(unique(test$id_placette))
#########################################


xx <- as.data.frame(unlist(r2_lst))
xx <- as.data.frame(cbind(xx, seq_along(1:length(xx[,1]))))
names(xx) <- c("r2", "index")

library(ggplot2)

ggplot(data=xx, aes(x=index, y=r2))+
  geom_line()

##clipping plots based on plot centre coordinates
aois <- list()
for(i in 1:30)
{
  aois[[toString(fd_ba_summary$id_placette[i])]] <- clip_circle(lasc, 
                                                                xcenter = fd_ba_summary$cx[i], 
                                                                ycenter = fd_ba_summary$cy[i], 
                                                                radius = 15 )
}







##computing mean scan angle per flight line
area_calc = function(dfr)
{
  #print(length(dfr$x))
  #str(dfr)
  ch_pts <- chull(dfr$x,dfr$y)
  ch_pts <- c(ch_pts, ch_pts[1])
  dfr <- dfr[ch_pts,]
  dfr <- dfr %>% 
    select(1:2) 
  ch_poly <- Polygon(dfr, hole=F)
  return(ch_poly@area)
}

aois_rand <- list()
for(name in names(aois))
{
  aoi <- lasflightline(aois[[name]])
  flist <- unique(aoi@data$flightlineID)
  meangle_fl_ar <- data.frame()
  for(fl in flist)
  {
    aoi_subset <- lasfilter(aoi, flightlineID == fl)
    meangle <- abs(mean(aoi_subset@data$ScanAngleRank))
    ar <- area_calc(data.frame(x = aoi_subset@data$X, y = aoi_subset@data$Y))
    meangle_fl_ar <- rbind(meangle_fl_ar,c(meangle,fl, ar))
  }
  
  meangle_fl_ar <- meangle_fl_ar[which(meangle_fl_ar[,3]>650),]
  
  
  ##generate a random number between 1 and no. of rows to pick random flid
  x <- sample(1:length(meangle_fl_ar[,1]),1)
  
  ##random flid 
  flid <- meangle_fl_ar[,2][x]
  
  
  ##flid for min mean angle
  #flid <- meangle_fl_ar[,2][which.max(meangle_fl_ar[,1])]
  
  
  ##flid for max mean angle
  #flid <- meangle_fl_ar[,2][which.min(meangle_fl_ar[,1])]
  
  ##extract the point cloud based on the flid and append to a list 
  aoi1 <- lasfilter(aoi, flightlineID == flid)
  aois_rand[[name]] <- aoi1
}

##compute the metrics for the list of point clouds

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
  if (npoints(aoi)==0)
  {
    return(NA_real_)
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
  if (npoints(aoi)==0)
  {
    return(NA_real_)
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
  if (npoints(aoi)==0)
  {
    return(NA_real_)
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
  if (npoints(aoi)==0)
  {
    return(NA_real_)
  }
  else
  {
    v <- NULL
    zm = min(ceiling(max(aoi@data$Z)), 60)
    
    val = as.vector(unlist(myProfilesLAD(aoi@data$Z, zm, dZ = 1)))
    return(sd(val) / mean(val))
  }
}

mets_meanch_rand <- list()
mets_varch_rand <- list()
mets_gapfr_rand <- list()
mets_cvlad_rand <- list()

for(i in names(aois_rand))
{
  mets_meanch_rand[i] <- func_meanch(aois_rand[[i]])
  mets_varch_rand[i] <- func_varch((aois_rand[[i]]))
  mets_gapfr_rand[i] <- func_pf((aois_rand[[i]]))
  mets_cvlad_rand[i] <- func_cvlad((aois_rand[[i]]))
}

all_data_rand <- as.data.frame(cbind(fd_ba_summary[,1:5], 
                                    "meanch_rand" = unlist(mets_meanch_rand),
                                    "varch_rand" = unlist(mets_varch_rand),
                                    "gapfr_rand" = unlist(mets_gapfr_rand),
                                    "cvlad_rand" = unlist(mets_cvlad_rand)))
                                    

model_rand2 <- lm(log(sum_ba_hec)~log(meanch_rand)+log(varch_rand)+log(gapfr_rand)+log(cvlad_rand), 
             data = all_data_rand[-c(21,22),])

summary(model_rand2)







cvlad1 <- rownames_to_column(as.data.frame(matrix(unlist(cvlad), 
                                                  dimnames = list(names(cvlad), "cvlad"))), 
                             'id_placette')
gapfr1 <- rownames_to_column(as.data.frame(matrix(unlist(gapfr), 
                                                  dimnames = list(names(gapfr), "gapfr"))), 
                             'id_placette')
meanch1 <- rownames_to_column(as.data.frame(matrix(unlist(meanch), 
                                                   dimnames = list(names(meanch), "meanch"))), 
                              'id_placette')
varch1 <- rownames_to_column(as.data.frame(matrix(unlist(varch), 
                                                  dimnames = list(names(varch), "varch"))), 
                             'id_placette')

gapfr_corr <- rownames_to_column(as.data.frame(matrix(unlist(gapfr_corr), 
                                                      dimnames = list(names(gapfr_corr), "gapfr_corr"))), 
                                 'id_placette')


all_data <- left_join(fd_ba_summary, , by='id_placette')



ggplot(data = outs1, aes(x=ID, y=value, colour=variable))+
  geom_line()+
  geom_vline(xintercept = which(outs$ID%%2==0), 
             alpha=0.6)+
  geom_vline(xintercept = which(outs$ID%%2!=0), 
             colour='blue', 
             alpha=0.3, 
             linetype='dashed')+
  geom_hline(yintercept = c(65.3,66.7))+
  geom_point()




