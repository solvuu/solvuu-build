# solvuu-build release notes

## solvuu-build 0.3.0 2017-03-14
* Support additional OCaml compiler flags, particularly those relating
  to flambda and compilation of C code.
* Improve build times of targets in generated Makefile. See #58.
* :warning: Allow faster building of static files. Backwards
  incompatibility: type of `Project.build_static_file` has
  changed. See #58.
* :warning: Make installation of libs optional: #35. Backwards
  incompatibility: `pkg` argument to make lib is now optional, renamed
  to `install`, and of a different type.
* App installation is now optional. See #66.
* Bug fixes. See #21, #51, #57.

## solvuu-build 0.2.0 2016-12-09
* Add support for Eliom.
* Improve generated .merlin file.
* Support additional OCaml compiler flags.
* Allow projects building only an app.
* Various bug fixes.

## solvuu-build 0.1.0 2016-09-27
* Renamed project to solvuu-build instead of solvuu_build.
* Completely new API.
* Add high level documentation in README.

## solvuu-build 0.0.2 2016-02-23
* Support compilation of C files.
* Installation of each sub-library of a project now goes into its own
  sub-directory.

## solvuu-build 0.0.1 2016-02-15
* Initial release.
