dswapDST - data base on contaminant removal efficiencies in wastewater treatment
======================================================

What is this?
------------------
This repository provides ``dswapDST``, an add-on package for the [``R ``](https://www.r-project.org/) statistical computing software. The package contains both, the data base of contaminant removal efficiencies reported in the scientific literature and the browser-based interactive user interface.

Prerequisites
------------------

The ``dswapDST`` package currently depends on four other packages, three of which can be installed from CRAN using the ```install.packages()``` command. These CRAN packages are:

- ``shiny`` : needed to build the web-browser interface
- ``DT`` : allows for interactive tables
- ``sqldf`` : required to run SQL queries on the data base

The fourth package called ``tabular`` is needed for the purpose of data base validation. It is currently not available on CRAN but can directly installed from the githib repository as follows:

```
library("devtools")
install_github("dkneis/tabular")

```

For this to work, you need the ``devtools`` package in addition. Since the latter is available from CRAN, you can install it with ``install.packages()`` as usual.

Starting the interface
------------------

The package provides just a single function called ``runDST()``. Call it to start the data base interface in the web browser.

How to report problems or request features
------------------

Write an email to David Kneis (firstname . lastname @ tu-dresden . de).
