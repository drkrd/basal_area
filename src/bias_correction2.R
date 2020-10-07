library(lidR)
library(tibble)
library(dplyr)
library(tidyr)
library(sf)


riparian_region <- st_read("D:/1_Work/2_Ciron/Data/Shape_files/Riparian_region.shp")


func_linmod <- function(met,nm)
{
  #stack the metrics raster and the 95th percentile raster to filter out low vegetation and other noise
  rst_stack <- stack(met, p95)
  #extract only the pixel values that correspond to the riparian
  rst_df <- raster::extract(rst_stack, riparian_region, method='simple', cellnumbers=TRUE) 
  rst_df <- as.data.frame(rbind(rst_df[[1]], rst_df[[2]], rst_df[[3]], rst_df[[4]], rst_df[[5]]))
  rst_vals <- rst_df %>% 
    #dropping all empty rows corresponding to 95th percentile
    drop_na(zq95) %>%  
    
    #dropping cl5 as there are few pixels common between cl5 and others. 
    dplyr::select(-c(cl5)) %>% 
    
    #removing all pixel values which correspond to a locations where the 95th ht. percentile is less than to analyse only high forests
    dplyr::filter(zq95>7) %>%  
    
    #dropping 95th percentile
    dplyr::select(-zq95) %>%   
    
    #renaming all raster values for display
    dplyr::rename("Class1" = cl1, 
                  "Class2" = cl2,
                  "Class3" = cl3,
                  "Class4" = cl4) %>% 
    drop_na()
  
  
  
  model1 <- lm(Class1 ~ Class2, data = rst_vals)
  model2 <- lm(Class1 ~ Class3, data = rst_vals)
  model3 <- lm(Class1 ~ Class4, data = rst_vals)
  
  coeffs <- data.frame("Intercept"=c(model1[["coefficients"]][["(Intercept)"]], 
                                     model2[["coefficients"]][["(Intercept)"]], 
                                     model3[["coefficients"]][["(Intercept)"]]),
                       "Slope"=c(model1[["coefficients"]][["Class1"]],
                                 model2[["coefficients"]][["Class1"]],
                                 model3[["coefficients"]][["Class1"]]))
  return(coeffs)
  }


coeffs_gf <- func_linmod(met_gfrac2, 'gf')




#Simple correction of the linear model
#Subtract the intercept and divid slope
#y = mx + c  =>  (y-c)/m = x


area_calc = function(ls)
{
  #print(length(dfr$x))
  dfr <- data.frame("x"=ls@data$X, "y"=ls@data$Y)
  ch_pts <- chull(dfr$x,dfr$y)
  ch_pts <- c(ch_pts, ch_pts[1])
  dfr <- dfr[ch_pts,]
  dfr <- dfr %>% 
    select(1:2) 
  ch_poly <- Polygon(dfr, hole=F)
  return(ch_poly@area)
}

func_pf <- function(x)
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


scan_corr <- function(x)
{
  x <-lasflightline(x)
  unique_flines <- unique(x@data$flightlineID)
  areas <- NULL
  pfs_w <- NULL
  for (fline_id in unique_flines)
  {
    las_sub <- lasfilter(x, flightlineID == fline_id)
    mean_scan <- abs(mean(las_sub@data$ScanAngleRank))
    las_sub_area <- area_calc(las_sub)
    pf_sub <- func_pf(las_sub)
    pf_sub_corr <- ifelse(mean_scan<20, ((pf_sub-coeffs_gf$Intercept[1])/coeffs_gf$Slope[1]),
                          ifelse(mean_scan<30, ((pf_sub-coeffs_gf$Intercept[2])/coeffs_gf$Slope[2]),
                                 pf_sub-coeffs_gf$Intercept[3])/coeffs_gf$Slope[3])
    
    pfs_w <- c(pfs_w,(pf_sub_corr*las_sub_area))
    
    
    areas <- c(areas, las_sub_area)
  }
  
  mean_pf <- sum(pfs_w, na.rm = TRUE)/sum(areas, na.rm = TRUE)
  return(mean_pf)
}

gapfr_corr <- lapply(aois, scan_corr)


library(lidR)
