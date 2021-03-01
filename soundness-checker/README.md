# Soundness checker

This project provides methods for validating the result of applying an operation to a temporal feature model. It also makes it possible to convert from a simple feature model evolution plan to a temporal feature model.

Table of Contents
=================

   * [Soundness checker](#soundness-checker)
      * [Build](#build)
      * [Run](#run)
      * [Overview](#overview)

## Build
```sh
stack build
```

## Run
```sh
stack run
```
This outputs an example temporal feature model.

## Overview
See the [`src/`](https://github.com/idamotz/Master/tree/master/soundness-checker/src) folder for the implementation.  
[`src/Types.hs`](https://github.com/idamotz/Master/blob/master/soundness-checker/src/Types.hs) contains all the types used in the project, and provides insight into the structures of the project. [`src/Validate.hs`](https://github.com/idamotz/Master/blob/master/soundness-checker/src/Validate.hs) contains the function `validate` which returns a list of errors resulting from applying an operation to a temporal feature model. [`src/Apply.hs`](https://github.com/idamotz/Master/blob/master/soundness-checker/src/Apply.hs) contains the function `apply`, which applies an operation to a temporal feature model. [`src/Convert.hs`](https://github.com/idamotz/Master/blob/master/soundness-checker/src/Convert.hs) contains the function `convert` which translates a feature model evolution plan into a temporal feature model.
