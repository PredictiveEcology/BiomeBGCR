print(getwd())
ini <- iniRead("inst/inputs/ini/enf_test1.ini")
ini = iniFixPaths(ini)
firstSimYr <- iniGet(ini, "TIME_DEFINE", 3)
ini = iniSet(ini, "TIME_DEFINE", 3, "1666")

iniWrite(ini, "inputs/ini/enf_test1_out.ini")

print(ini)
