1.4.0 / 2015-03-27
==================

  * Fixed errors in return type phpdocs (thanks @sobit)
  * Fixed stringifying of hash containing one value (thanks @avant1)
  * Improved clarity of method call expectation exception (thanks @dantleech)
  * Add ability to specify which argument is returned in willReturnArgument (thanks @coderbyheart)
  * Add more information to MethodNotFound exceptions (thanks @ciaranmcnulty)
  * Support for mocking classes with methods that return references (thanks @edsonmedina)
  * Improved object comparison (thanks @whatthejeff)
  * Adopted '^' in composer dependencies (thanks @GrahamCampbell)
  * Fixed non-typehinted arguments being treated as optional (thanks @whatthejeff)
  * Magic methods are now filtered for keywords (thanks @seagoj)
  * More readable errors for failure when expecting single calls (thanks @dantleech)

1.3.1 / 2014-11-17
==================

  * Fix the edge case when failed predictions weren't recorded for `getCheckedPredictions()`

1.3.0 / 2014-11-14
==================

  * Add a way to get checked predictions with `MethodProphecy::getCheckedPredictions()`
  * Fix HHVM compatibility
  * Remove dead code (thanks @stof)
  * Add support for DirectoryIterators (thanks @shanethehat)

1.2.0 / 2014-07-18
==================

  * Added support for doubling magic methods documented in the class phpdoc (thanks @armetiz)
  * Fixed a segfault appearing in some cases (thanks @dmoreaulf)
  * Fixed the doubling of methods with typehints on non-existent classes (thanks @gquemener)
  * Added support for internal classes using keywords as method names (thanks @milan)
  * Added IdenticalValueToken and Argument::is (thanks @florianv)
  * Removed the usage of scalar typehints in HHVM as HHVM 3 does not support them anymore in PHP code (thanks @whatthejeff)

1.1.2 / 2014-01-24
==================

  * Spy automatically promotes spied method call to an expected one

1.1.1 / 2014-01-15
==================

  * Added support for HHVM

1.1.0 / 2014-01-01
==================

  * Changed the generated class names to use a static counter instead of a random number
  * Added a clss patch for ReflectionClass::newInstance to make its argument optional consistently (thanks @docteurklein)
  * Fixed mirroring of classes with typehints on non-existent classes (thanks @docteurklein)
  * Fixed the support of array callables in CallbackPromise and CallbackPrediction (thanks @ciaranmcnulty)
  * Added support for properties in ObjectStateToken (thanks @adrienbrault)
  * Added support for mocking classes with a final constructor (thanks @ciaranmcnulty)
  * Added ArrayEveryEntryToken and Argument::withEveryEntry() (thanks @adrienbrault)
  * Added an exception when trying to prophesize on a final method instead of ignoring silently (thanks @docteurklein)
  * Added StringContainToken and Argument::containingString() (thanks @peterjmit)
  * Added ``shouldNotHaveBeenCalled`` on the MethodProphecy (thanks @ciaranmcnulty)
  * Fixed the comparison of objects in ExactValuetoken (thanks @sstok)
  * Deprecated ``shouldNotBeenCalled`` in favor of ``shouldNotHaveBeenCalled``

1.0.4 / 2013-08-10
==================

  * Better randomness for generated class names (thanks @sstok)
  * Add support for interfaces into TypeToken and Argument::type() (thanks @sstok)
  * Add support for old-style (method name === class name) constructors (thanks @l310 for report)

1.0.3 / 2013-07-04
==================

  * Support callable typehints (thanks @stof)
  * Do not attempt to autoload arrays when generating code (thanks @MarcoDeBortoli)
  * New ArrayEntryToken (thanks @kagux)

1.0.2 / 2013-05-19
==================

  * Logical `AND` token added (thanks @kagux)
  * Logical `NOT` token added (thanks @kagux)
  * Add support for setting custom constructor arguments
  * Properly stringify hashes
  * Record calls that throw exceptions
  * Migrate spec suite to PhpSpec 2.0

1.0.1 / 2013-04-30
==================

  * Fix broken UnexpectedCallException message
  * Trim AggregateException message

1.0.0 / 2013-04-29
==================

  * Improve exception messages

1.0.0-BETA2 / 2013-04-03
========================

  * Add more debug information to CallTimes and Call prediction exception messages
  * Fix MethodNotFoundException wrong namespace (thanks @gunnarlium)
  * Fix some typos in the exception messages (thanks @pborreli)

1.0.0-BETA1 / 2013-03-25
========================

  * Initial release
