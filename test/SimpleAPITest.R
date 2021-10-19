wd <- getwd()

# temporarily change working directory to allow BGC-Biome using its relative paths to load files
setwd(paste(wd, "/src/Biome-BGC", sep = ""))

res <- bgcExecute("ini/enf_test1_spinup.ini")
if (res != 0) {
  setwd(wd)
  stop(paste("bgcExecute failed with error ", res))
}

res <- bgcExecute("ini/enf_test1.ini")
if (res != 0) {
  setwd(wd)
  stop(paste("bgcExecute failed with error ", res))
}

setwd(wd)
