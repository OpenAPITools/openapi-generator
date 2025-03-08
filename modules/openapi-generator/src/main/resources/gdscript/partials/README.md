**Partials** are bits of code that we reuse through templates,
or use once but provide anyway in order to make template customization easier.

For example, if you only want to change the utmost parent class of API classes,
you may override `api_bee_parent_class.handlebars` in this directory.

Note that these are probably not registered as actual partials to Handlebars,
so they are _pseuds-partials_ and won't allow recursion.

> _May the fork be with you, always._
