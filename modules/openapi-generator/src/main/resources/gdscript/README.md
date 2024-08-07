In here are the `handlebars` templates used to generate the `GDScript` client.

All files without the `.handlebars` extension (including this very `README.md` file) are ignored.

You can copy them all (or parts) and override them as needed.


## Domain Overview

### ApiBee

Base class for all Api endpoints classes.
Holds most of the nitty-gritty.


### ApiConfig

Reusable configuration (host, port, etc.) for Apis, injected into their constructor.


### ApiError

Godot does not have an `Exception` (`try / catch`) mechanism, by design.

> It actually makes _some_ sense in the volatile environment that are video games,
> due to their gigantic amount of user inputs and userland data,
> inherent complexity, low stakes, and entertainment value of glitches.

Therefore, whenever there's trouble in paradise, we pass around an `ApiError` object. (a `RefCounted`, don't worry about garbage collection)


### ApiResponse

A wrapper for an API Response, used in callbacks.
Holds the HTTP components of the Response, as well as the deserialized `data` (if any).


## Extending

Most classes can be configured to extend your own class.
Override the `partials/*_parent_class.handlebars` to define them.
