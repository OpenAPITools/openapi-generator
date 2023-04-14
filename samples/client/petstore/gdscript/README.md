# Petstore GDScript Testing Client

## What

- [Godot] `4.x` project
- Made for `--headless` mode
- Returns non-zero exit code upon test failure
- Uses [GUT] as test engine


## Prepare the test server

See https://github.com/OpenAPITools/openapi-generator/wiki/Integration-Tests

> We are using the petstore docker, not the echo server for now.
> Feel free to refactor or duplicate the sample demo to use the new echo server.
> See `bin/configs/gdscript-petstore.yaml`.


## Run

	godot --headless samples/client/petstore/gdscript

The command should return a zero exit code if all tests passed.
You may want to add `--verbose` for more logs when debugging.


## Update

Refresh the generated files, after modifying the gdscript templates or java generator:

	bin/generate-samples.sh bin/configs/gdscript-petstore.yaml


[Godot]: https://godotengine.org
[GUT]: https://github.com/bitwes/Gut
