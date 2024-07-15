.. _ffi-overview:

########
Overview
########

Idris functions can call Java functions and Java functions can call Idris functions. ``%foreign`` blocks in Idris
allows calling Java functions from Idris. Similarly ``%export`` blocks in Idris allows Idris functions to be
exported as Java static methods and instance methods. Java classes can be created from Idris and Java
interfaces can be implemented using Idris functions. Java annotations can be added in the exported classes, fields and
methods as well. We will see detailed examples of all of these capabilities in the following sections.
