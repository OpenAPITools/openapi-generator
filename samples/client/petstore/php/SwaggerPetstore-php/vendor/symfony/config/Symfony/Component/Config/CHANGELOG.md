CHANGELOG
=========

2.2.0
-----

 * added ArrayNodeDefinition::canBeEnabled() and ArrayNodeDefinition::canBeDisabled()
   to ease configuration when some sections are respectively disabled / enabled
   by default.
 * added a `normalizeKeys()` method for array nodes (to avoid key normalization)
 * added numerical type handling for config definitions
 * added convenience methods for optional configuration sections to ArrayNodeDefinition
 * added a utils class for XML manipulations

2.1.0
-----

 * added a way to add documentation on configuration
 * implemented `Serializable` on resources
 * LoaderResolverInterface is now used instead of LoaderResolver for type
   hinting
