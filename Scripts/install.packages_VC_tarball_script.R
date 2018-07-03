args <- commandArgs(trailingOnly = TRUE)
packagePath  <- args[1]
dependencies <- args[2]

# input checks:
if (!file.exists(packagePath)) {
    stop(sprintf("The file (%s) provided does not exist.\nPlease specify 2 arguments.\nThe complete path to the tarball file, and a character string showing the dependencies: `dplyr = (0.5.0), ggplot2 = ()`.", packagePath))
}

if (!require(RVClibrary)) {
    stop("\nPlease install this single tarball manually using something like:\n `install.packages('M:/.../RVClibrary_0.1.0.tar.gz', lib = Sys.getenv('R_LIBS_USER'), type = 'source', repos = NULL)`\n\n")
}

install.packages_VC_tarball(packagePath, dependencies, parse_dependencies = TRUE)
