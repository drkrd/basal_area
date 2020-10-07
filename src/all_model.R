all_dbs <- data.frame()
for(name in names(aois))
{
  aoi_tmp <- aois[[name]]
  aoi_meanch <- func_meanch(aoi_tmp@data$Z, aoi_tmp@data$ReturnNumber)
  aoi_varch <- func_varch(aoi_tmp@data$Z, aoi_tmp@data$ReturnNumber)
  aoi_pf <- func_pf(aoi_tmp@data$Z, aoi_tmp@data$ReturnNumber)
  aoi_cvlad <- func_cvlad(aoi_tmp@data$Z, aoi_tmp@data$ReturnNumber)
  all_dbs <- as.data.frame(rbind(all_dbs, c(name,
                                            aoi_meanch,
                                            aoi_varch,
                                            aoi_pf,
                                            aoi_cvlad)))
}
names(all_dbs) <- c("id_placette", "meanch", "varch", "pf", "cvlad")
all_dbs$mean_angle <- as.numeric(all_dbs$mean_angle)
all_dbs$meanch <- log(as.numeric(all_dbs$meanch))
all_dbs$varch <- log(as.numeric(all_dbs$varch))
all_dbs$pf <- log(as.numeric(all_dbs$pf))
all_dbs$cvlad <- log(as.numeric(all_dbs$cvlad))
all_dbs2 <- right_join(fdata, all_dbs, by="id_placette")
all_model <- lm(sum_ba_hec~meanch+varch+pf+cvlad, data=all_dbs2[-c(21,22),])
summary(all_model)
