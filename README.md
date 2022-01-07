# multiversion
`multiversion` adds support for installing multiple versions of an R package in parallel and loading a specific version and its related dependencies. The only package that you need to install is this package providing the support for the complete library management.

`multiversion` helps to solve the problem of reproducibility, but leaves the smallest footprint possible. With practically no overhead (besides the need to install this package, which by the way has no dependencies at all) you can install new versions of a package and keep using the older versions with other applications.

Now you might think, "and what about packrat?", well, next to the major overhead, it does also lack the possibility to load older versions on-the-fly. It would require you to remove all installed packages, and install the older versions again... `multiversion` provides `lib.execute_using_packagelist()` which allows you to execute code with any combination of dependencies that you specify.

Using this library structure (can also be placed on a shared drive) and library management tool (`multiversion`) I am able to build a GUI (shiny app) that can launch a specific version of an analysis as a stand-alone process. It can then monitor it's status by using the log file and launch multiple processes in parallel.
Reproducibility is a major requirement for a company, and there is no package or working method I could find that could provide us with a development method in which we could develop quickly and release rapidly while still maintaining 100% reproducibility by continuously supporting full backwards compatibility.

# Getting started

To get started, install the package:

```r
install.packages("multiversion")  # (<- hopefully possible in the near future, for now a manual installation is required)
library(multiversion)
```

Now you are ready, there are two things you could try to get to know us (me and my package ;P):

1. If you already have some packages installed, you can try to convert your standard library to the structure that is required by `multiversion` to be able to load packages. This can of course be done automatically. Create an empty folder somewhere and run:

```r
dir.create("C:/example_library_location")
lib.convert(normalLibrary = Sys.getenv("R_LIBS_USER"),
            lib_location  = "C:/example_library_location")
```

Now take a look at the library that is created in C:/example_library_location. Every package you had installed before can now be loaded using:

```r
lib.load(YourPackage, lib.location = "C:/example_library_location")
```

2. Alternatively, you can start off fresh and install your favorite packages directly in the new structure. In example 1 we defined "C:/example_library_location", but it is of course not desirable to have to specify this directory over and over again.
`multiversion` provides 2 ways to indicate the multiversion library location, a) you can set it for the duration of the session by running `lib.location()` and providing it your multiversion library location or, b) you can set the `R_MV_LIBRARY_LOCATION` environment variable with that path. The function `lib.location()` is used to define the location of the library throughout the package.

```r
# Check whats configured, or learn how to configure it:
lib.location()

# Configure it (for this session):
lib.location("C:/example_library_location")

# Install a package:
lib.install("devtools")

# Cleanup the temp install dir:
lib.clean_install_dir()
```

Congratulations with installing your first package! Now try to load it:

```r
# Now load your package (offcourse you can ommit the quotes):
lib.load(devtools)  # or also specify a version `lib.load(devtools = '> 1.0.0')`

# Alternatively you could use a convenience wrapper (for `devtools` only):
lib.devtools_load()
```

To list all packages that were installed in the library, try running `lib.installed_packages()`.
For the ones that are familiar with Git, I highly recommend placing the created multiversion library under git version control. Sometimes it can be hard to keep track of what packages were actually installed, or someone accidentally threw away a package which breaks your reproducibility... potentially forever.

I hope I could assist you with getting started. Please contact me if you have any questions!
