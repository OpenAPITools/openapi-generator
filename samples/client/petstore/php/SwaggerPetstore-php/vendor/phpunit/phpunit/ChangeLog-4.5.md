# Changes in PHPUnit 4.5

## PHPUnit 4.5.0

* Added out-of-the-box support for [Prophecy](https://github.com/phpspec/prophecy)
* Implemented [#137](https://github.com/sebastianbergmann/phpunit/issues/137): Add support for variable number of tests shown per line in default result printer
* Merged [#1478](https://github.com/sebastianbergmann/phpunit/issues/1478): Improve the performance of `PHPUnit_Framework_Constraint_IsEqual` (which is used by `assertEquals()`, for instance) for the most common case
* Fixed [#1474](https://github.com/sebastianbergmann/phpunit/issues/1474): Allow the registration of custom comparators for `assertEquals()` et al. (again)
* [Deprecated](https://github.com/sebastianbergmann/phpunit/commit/7abe7796f77b13fdf3cfc506fb987d6c2ab477f5) the `--strict` commandline option and the XML configuration's `strict` attribute

