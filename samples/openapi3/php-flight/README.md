## Requirements

PHP 8.1 and later

## Installation & Usage

1. Set up flight as usual - see [Flight documentation](https://docs.flightphp.com/install)
2. Generate using the OpenAPI generator
3. Subclass some/all generated `Abstract*Api` and overwrite the methods you'd like handled. When implementing the `*Stream` methods, make sure to stream the response and not implement the non-stream method.
4. Register routes for your subclassed apis: `RegisterRoutes::registerRoutes(new MyApiHandler());`
