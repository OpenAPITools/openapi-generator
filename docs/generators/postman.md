---
title: Documentation for the Postman Generator
---

## METADATA

| Property                            | Value                                         | Notes |
|-------------------------------------|-----------------------------------------------| ----- |
| generator name                      | postman                                       | pass this to the generate command after -g |
| generator stability                 | STABLE                                        | |
| generator type                      | DOCUMENTATION                                 | |
| generator default templating engine | mustache                                      | |
| helpTxt                             | Generates a Postman collection in JSON format | Schema https://schema.postman.com/collection/json/v2.1.0/draft-07/collection.json | |

## CONFIG OPTIONS
These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option                             | Description                                                                                                  | Values          | Default          |
|------------------------------------|--------------------------------------------------------------------------------------------------------------|-----------------|------------------|
| folderStrategy                     | whether to create folders according to the spec’s paths or tags                                              | Paths, Tags     | Paths            |
| pathParamsAsVariables              | boolean, whether to create Postman variables for path parameters                                             | true, false     | true             |
| postmanVariables                   | whether to convert placeholders (i.e. {{VAR_1}}) into Postman variables                                      | true, false     | true             |
| postmanGuid                        | whether to convert placeholders (i.e. {{UNIQUE_REFERENCE}}) into Postman formula ${{guid}}                   | true, false     | true             |
| postmanGuidPlaceholderName         | customise the name of the placeholder (i.e. {{UNIQUE_REFERENCE}}) to replace with Postman formula {{$guid}}" | string          | UNIQUE_REFERENCE |
| postmanIsoTimestamp                | whether to convert placeholders (i.e. {{ISO_TIMESTAMP}}) into Postman formula {{$isoTimestamp}}              | true, false     | true             |
| postmanIsoTimestampPlaceholderName | customise the name of the placeholder (i.e. {{ISO_TIMESTAMP}}) to replace with Postman formula {{$isoTimestamp}}" | string          | ISO_TIMESTAMP |
| requestParameterGeneration         | whether to generate the request parameters based on the schema or the examples                               | Example, Schema | Example          |
