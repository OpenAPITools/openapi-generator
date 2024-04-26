# REST Service 

This project contains the data models and REST services, generated from the [openapi-generator](https://github.com/OpenAPITools/openapi-generator) project.

The server implementation is based on Li's excellent [cask](https://com-lihaoyi.github.io/cask/) library.

# How to use this code

This code was designed so that it can be packaged up and semantically-versioned alongside your open-api schema.

That approach supports having separate "contract" repositories for microservice projects, where the pipeline for the
contract repo might produce versioned jar artefacts which can then easily be brought in / referenced by a separate service project which simply
implements the business logic.

You can read more about this approach [here](https://github.com/kindservices/contract-first-rest)

# How to implement your business logic
There are a few options for using this code/applying your business logic for the services.

## Option 1 (preferred): Package and publish this boilerplate
Typically, OpenApi templates are written to generate code which co-exists alongside the handwritten business logic.

While that works, it's also not ideal:
 * You have to ensure the generated code isn't checked in
 * Team members, build pipelines, etc all have to regenerate and recompile the same boilerplate code over and over
 * People can encounter IDE issues with generated code

Instead, you have the option of simply packaging/publishing this generated code, and then allowing service implementations
to simply bring in the published code as a dependency.

The steps to do that are:

### Build/Publish
This project is built using [sbt](https://www.scala-sbt.org/download/), so you can run `sbt publish` (or `sbt publishLocal`)

Or, for a zero-install docker build:
```bash
docker run -it --rm -v $(pwd):/app -w /app sbtscala/scala-sbt:eclipse-temurin-17.0.4_1.7.1_3.2.0 sbt publishLocal
```

### Create a new separate implementation project
Once published, you can create your server implementation in a new, clean, separate project based on [the example](./example)

This means all the boilerplate endpoint and model code is brought in as "just another jar", and you're free to
create a greenfield project in whatever language (scala, java, kotlin) and build system of your choosing.

We show a simple, minimalistic example of a starting point in [the example project](./example) 

## Option 2: Extend this generated example
You can configure this project (for instance, setting up your own .gitignore rules and scripts) to leave the generated code as-is 
and provide your implementation alongside the generated code.

The place to start is by providing your own implementation of the Services defined in the `api` package - 
perhaps by creating your 'MyService.scala' code in a new `impl` package.

You then have several options for how to wire those in:

1) Create a new BaseApp instance to create your own Main entry point
Follow the pattern in App.scala, but by passing your own implementations to BaseApp, 
ensuring you call `start` to start the server

```bash
@main def run() = BaseApp(/* your services here/*).start()
```

2) Extend either BaseApp class or mix in the AppRoutes trait
You can create your own main entry point with further control of the main cask app by extending
the BaseApp or otherwise creating your own CaskApp which mixes in the AppRoutes

```bash
object MyApp extends BaseApp(/* your services here/*) {
  // any overrides, new routes, etc here
  start()
}
```


# Customising the generated code

A typical config.yml used to alter the generated code may look like this:
```
groupId: "ex.amp.le"
artifactId: "pets-test"
apiPackage: "ex.ample.api"
modelPackage: "ex.ample.model"
```

Which you would then pass to the generator like this:
```
docker run --rm \
-v ${PWD}:/local openapitools/openapi-generator-cli generate \
-i https://raw.githubusercontent.com/OAI/OpenAPI-Specification/main/examples/v3.0/petstore.yaml \
-g scala-cask \
-c /local/config.yml \
-o /local/path/to/output_dir
```
