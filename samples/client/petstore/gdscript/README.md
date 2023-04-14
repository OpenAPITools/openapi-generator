# Petstore GDScript Testing Client

## What

- [Godot] `4.x` project
- Made for `--headless` mode
- Returns non-zero exit code upon test failure
- Uses [GUT] as test engine

## Run

	godot --headless samples/client/petstore/gdscript

## Update

Refresh the generated files, after modifying the gdscript templates or java generator:

	bin/generate-samples.sh bin/configs/gdscript-petstore.yaml


[Godot]: https://godotengine.org
[GUT]: https://github.com/bitwes/Gut
