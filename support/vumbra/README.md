# umbra

A library of reusable GPU shader functions.

## Overview

This is a library consisting of a collection of useful GPU shader functions, written with
[Shadow](https://git.mfiano.net/mfiano/shadow).

## Install

```lisp
(ql:quickload :umbra)
```

## Usage

The functions contained in this library are meant to be compiled by your GPU, using
[Shadow](https://git.mfiano.net/mfiano/shadow). On their own, they don't do anything, Please consult
the Shadow documentation for usage instructions for inclusion in a shader program.

## License

Copyright © 2017-2022 Michael Fiano <mail@mfiano.net>.

Licensed under the MIT License.
