wd <- getwd()

argv <- "-a"

# temporarily change working directory to allow BGC-Biome using its relative paths to load files
setwd(paste(wd, "/src/Biome-BGC", sep = ""))

# execute reference test run
res <- bgcExecute(argv, c("ini/enf_test1_spinup.ini"))
if (res != 0) {
  setwd(wd)
  stop(paste("bgcExecute failed with error ", res))
}

res <- bgcExecute(argv, c("ini/enf_test1.ini"))
if (res != 0) {
  setwd(wd)
  stop(paste("bgcExecute failed with error ", res))
}

# execute single step test run
res <- bgcExecute(argv, c("ini/enf_test1_singlestep_spinup.ini"))
if (res != 0) {
  setwd(wd)
  stop(paste("bgcExecute failed with error ", res))
}

# enf_test1_singlestep0.ini will not read the restart file
res <- bgcExecute(argv, c("ini/enf_test1_singlestep0.ini"))
for (i in 1:43)
{
  # enf_test1_singlestep.ini will read the restart file
  res <- bgcExecute(argv, c("ini/enf_test1_singlestep.ini"))
  if (res != 0) {
    setwd(wd)
    stop(paste("bgcExecute failed with error ", res))
  }

  file.copy("restart/enf_test1_singlestep_out.endpoint", "restart/enf_test1_singlestep.endpoint", overwrite = TRUE)
}

setwd(wd)
