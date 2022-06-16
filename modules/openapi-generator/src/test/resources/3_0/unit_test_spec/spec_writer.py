import dataclasses
import re
import json
import pathlib
import typing

import yaml

"""
Swagger-parser can create one spec from multiple files BUT:
- it must use refs to values in another file
- those refs will be turned into component schemas $refs
- so it will not dereference and inline schemas

Note:
- 3.0.3: schema supports only a single example
- That example could $ref a component example but we have not ability to test negative examples
- Higher spec versions do have examples BUT: examples does not store pass fail info
    - that info could be added as an x-flag

components.examples Re-use solution:
- including an x-valid tag in each example to note if it should pass or fail
- v3.0.3: add x-examples to schema for passing and failing examples
- ref the needed examples
- using examples for passing examples

BUT:
- the parser already supports refing examples to a separate yaml file any way

TODO:
- [DONE] load json file and write it into components/schemas
- [DONE] ensure that examples are being written there
- [DONE] add recursive casting of test examples to ObjectWithTypeBooleans
- [DONE] move processing to defaultCodegen
- [DONE] turn tests on
- add endpoints with those components in:
- request body
- response body
- all parameter types
- add main spec that uses type spec
"""

@dataclasses.dataclass
class JsonSchemaTestCase:
    description: str
    data: typing.Union[str, int, float, bool, None, list, dict]
    valid: bool

@dataclasses.dataclass
class JsonSchemaTestSchema:
    description: str
    schema: dict
    tests: typing.List[JsonSchemaTestCase]


class ExclusionReason:
    v303_does_not_support_array_of_types = 'v3.0.3 does not support type with array of values'
    v303_requires_array_have_items = 'v3.0.3 requires that items MUST be present if the type is array'

json_schema_test_draft = 'draft6'
filepath_to_excluded_case_and_reason = {
    (json_schema_test_draft, 'type.json'): {
        'multiple types can be specified in an array': ExclusionReason.v303_does_not_support_array_of_types,
        'type as array with one item': ExclusionReason.v303_does_not_support_array_of_types,
        'type: array or object': ExclusionReason.v303_does_not_support_array_of_types,
        'type: array, object or null': ExclusionReason.v303_does_not_support_array_of_types,
        'array type matches arrays': ExclusionReason.v303_requires_array_have_items,
    }
}

def get_json_schema_test_schemas(file_path: typing.Tuple[str]) -> typing.List[JsonSchemaTestSchema]:
    json_schema_test_schemas = []
    filename = file_path[-1]
    excluded_case_to_reason = filepath_to_excluded_case_and_reason.get(file_path, {})
    path = pathlib.PurePath(*file_path)
    with open(path) as json_file:
        test_schema_dicts = json.load(json_file)
        for test_schema_dict in test_schema_dicts:
            test_schema_dict['tests'] = [JsonSchemaTestCase(**t) for t in test_schema_dict['tests']]
            json_schema_test_schema = JsonSchemaTestSchema(**test_schema_dict)
            test_case_desc = json_schema_test_schema.description
            excluded_reason = excluded_case_to_reason.get(test_case_desc)
            if excluded_reason:
                print(f'Excluding {test_case_desc} because {excluded_reason}')
                continue

            json_schema_test_schemas.append(json_schema_test_schema)

    return json_schema_test_schemas

openapi_additions = 'openapi_additions'

json_schema_test_file_to_folders = {
    'type.json': (json_schema_test_draft, openapi_additions)
}

openapi_version = '3.0.3'


@dataclasses.dataclass
class OpenApiDocumentInfo:
    title: str
    description: str
    version: str

OpenApiSchema = typing.TypedDict(
    'OpenApiSchema',
    {
        'type': str,
        'x-test-examples': typing.Dict[str, JsonSchemaTestCase],
        'items': 'OpenApiSchema'
    }
)

@dataclasses.dataclass
class OpenApiExample:
    description: str
    value: typing.Union[str, int, float, bool, None, list, dict]

@dataclasses.dataclass
class OpenApiComponents:
    schemas: typing.Dict[str, OpenApiSchema]

@dataclasses.dataclass
class OpenApiDocument:
    openapi: str
    info: OpenApiDocumentInfo
    paths: typing.Dict[str, typing.Any]
    components: OpenApiComponents


def get_new_openapi() -> OpenApiDocument:
    return OpenApiDocument(
        openapi=openapi_version,
        info=OpenApiDocumentInfo(
            title=f"openapi {openapi_version} sample spec",
            description=f"sample spec for testing openapi functionality, built from json schema tests for {json_schema_test_draft}",
            version="0.0.1"
        ),
        paths={},
        components=OpenApiComponents(
            schemas={},
        )
    )

def description_to_component_name(descr: str) -> str:
    res = ''.join(descr.title().split())
    return re.sub(r'[^A-Za-z0-9 ]+', '', res)

def get_test_case_name(test: JsonSchemaTestSchema) -> str:
    res = ''.join(test.description.title().split())
    return re.sub(r'[^A-Za-z0-9 ]+', '', res)

def get_openapi_from_test_file(json_schema_test_file: str, folders: typing.Tuple[str]) -> OpenApiDocument:
     openapi = get_new_openapi()
     for folder in folders:
         file_path_tuple = (folder, json_schema_test_file)
         test_schemas = get_json_schema_test_schemas(file_path_tuple)
         for test_schema in test_schemas:
             component_name = description_to_component_name(test_schema.description)
             kwargs = {
                 'x-test-examples': {}
             }
             schema = OpenApiSchema(
                 **test_schema.schema, **kwargs
             )
             openapi.components.schemas[component_name] = schema
             for test in test_schema.tests:
                 test_case_name = get_test_case_name(test)
                 schema['x-test-examples'][test_case_name] = test
     return openapi

for json_schema_test_file, folders in json_schema_test_file_to_folders.items():
    openapi = get_openapi_from_test_file(json_schema_test_file, folders)
    print(
        yaml.dump(
            dataclasses.asdict(openapi),
            sort_keys=False
        )
    )
    filename_prefix = pathlib.Path(json_schema_test_file).stem
    spec_out = f'{filename_prefix}.yaml'
    with open(spec_out, 'w') as yaml_out:
        yaml_out.write(
            yaml.dump(
                dataclasses.asdict(openapi),
                sort_keys=False
            )
        )