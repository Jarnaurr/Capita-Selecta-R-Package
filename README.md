# FinalProject

This is an R package made as part of a university course to learn about the development of such such packages, github version control, C++ integration in R
and many other subjects. To implement these topics we do so by writing several functions on the bootstrapping method of (non-)parametric estimation.

## Bootstrapping

We implement several versions of the bootstrap, each useful in certain situations to reduce the computing time of what might otherwise be computationally heavy loops.
Firstly, there is a "regular" R implemented bootstrapping function which is useful for users who are familiar with the R implementation of such a loop and 
wish to have an interpretable implementation of such a process, saving some time.

Secondly, there is a C++ integrated implementation, which is virtually always superior to the regular R implementation, but might be more difficult
for users to understand as understanding this code would naturally require familiarity with C++ code structure.

Finally, We implement a version which includes both C++ integration and parallel computing which should theoretically be the quickest implementation possible.
During benchmark testing we find that the difference between this, and the regular C++ integrated function is meagre. For smaller bootstrapping projects the overhead
cost of parallel computing will not be worth it, while for larger bootstrap some time can be saved using this implementation.

## User QOL considerations

To make the implementation user friendly we return the results of the aforementioned functions as an S3 object, taking inspiration from the implementation of the "lm()"
function in R to allign this with what users are already familiar with. 

In addition to this we add some validator functions to check that the variables the users inserts in the function are valid, and return appropriate errors if this
is not the case.

Lastly, to easily view the results of the bootstrapping functions, users can use the familiar "summary()" functions to review the output, much like they can for an
object made using "lm()". Additionally, they can use the "print()" function for the raw output. In addition to this, plotting histograms of the output is an
important visual inspection of the bootstrapping result, therefore we also implemented a method to plot the distribution of estimated means and standard errors
using "plot()".

# How to install

There are several ways to install an R package, here we consider a method using the "pak" package

run the following command 

pak::pak("Jarnaurr/Capita-Selecta-R-Package")

Which should install the package.

If Rtools is not installed at your device it will return an error, simply run the command:

pkgbuild::check_build_tools(debug = TRUE)

Which will prompt you to install Rtools after which you can again run the previous command to install this R package from github.
