args <- commandArgs(trailingOnly = TRUE)
packagePath  <- args[1]
dependencies <- args[2]

# input checks:
if (!file.exists(packagePath)) {
    stop(sprintf("The file (%s) provided does not exist.\nPlease specify 2 arguments.\nThe complete path to the tarball file, and a character string showing the dependencies: `dplyr = (0.5.0), ggplot2 = ()`.", packagePath))
}

if (!require(RVClibrary)) {
    stop("Please install this single tarball manually using somthing like: `install.packages('M:/.../RVClibrary.targz', type = 'source', repos = NULL)`")
}

install.packages_VC_tarball(packagePath, dependencies, parse_dependencies = TRUE)
