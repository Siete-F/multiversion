## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

#### checking if this is a source package ... NOTE
    Found the following apparent object files/libraries:
      tests/test_library/package.a/0.1.0/package.a/R/package.a
      tests/test_library/package.a/0.2.0/package.a/R/package.a
      tests/test_library/package.a/0.3.0/package.a/R/package.a
    Object files/libraries should not be included in a source package.
  
    Subdirectory 'tests/test_library/package.a/0.1.0/package.a' seems to contain an installed version of the package.

I need to include this test library to be able to properly test my package.
If a different approach is necessary for making these tests succeed on non-windows machines, please let me know.

## Downstream dependencies
There are no donwstream dependencies yet.

## test coverage
Currently on 78.09%