# Soundness checker

This project provides methods for validating the result of applying an operation to a temporal feature model. It also makes it possible to convert from a simple feature model evolution plan to a temporal feature model.

Table of Contents
=================

   * [Soundness checker](#soundness-checker)
   * [Table of Contents](#table-of-contents)
      * [Build and run](#build-and-run)
         * [Prerequisites](#prerequisites)
      * [Overview](#overview)

## Build and run
### Prerequisites

- [GHC](https://www.haskell.org/platform/mac.html)
- [Stack](https://docs.haskellstack.org/en/stable/README/)


```sh
stack build
```
builds the project.

```sh
stack run
```
This outputs an example temporal feature model.

## Overview
See the [`src/`](https://github.com/idamotz/Master/tree/master/soundness-checker/src) folder for the implementation.  
[`src/Types.hs`](https://github.com/idamotz/Master/blob/master/soundness-checker/src/Types.hs) contains all the types used in the project, and provides insight into the structures of the project. [`src/Validate.hs`](https://github.com/idamotz/Master/blob/master/soundness-checker/src/Validate.hs) contains the function `validate` which returns a list of errors resulting from applying an operation to a temporal feature model. [`src/Apply.hs`](https://github.com/idamotz/Master/blob/master/soundness-checker/src/Apply.hs) contains the function `apply`, which applies an operation to a temporal feature model. [`src/Convert.hs`](https://github.com/idamotz/Master/blob/master/soundness-checker/src/Convert.hs) contains the function `convert` which translates a feature model evolution plan into a temporal feature model. [`src/TreeSequence.hs`](https://github.com/idamotz/Master/blob/master/soundness-checker/src/TreeSequence.hs) contains the function `toTreeSequence` which converts a temporal feature model into a list of time points associated with feature models. This representation can be used to verify correctness of `validate`, as there exists an agreed-upon semantics for feature models.
