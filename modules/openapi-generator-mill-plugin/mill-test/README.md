To test the Mill plugin, this project can be used.

> ![Note]
> The `mill-build` folder is only needed to look up the plugin from the local Maven repository for development purposes.

It requires that the current SNAPSHOT version of the plugin is installed in the local Maven repository.
Replace the version in `build.mill` or set the environment variable `$MILL_OPENAPITOOLS_PLUGIN_VERSION` to the desired version.

Run `./mill __.compile` to test if the plugin works or some of the modules tasks like `./mill openapi.validate`.