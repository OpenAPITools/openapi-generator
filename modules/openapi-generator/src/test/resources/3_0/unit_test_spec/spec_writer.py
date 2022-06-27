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

JsonSchema = typing.TypedDict(
    'JsonSchema',
    {
        'type': str,
        'properties': typing.Dict[str, 'JsonSchema'],
        'patternProperties': typing.Dict[str, 'JsonSchema'],
        'additionalProperties': typing.Union[bool, 'JsonSchema'],
        'allOf': typing.List['JsonSchema'],
        'anyOf': typing.List['JsonSchema'],
        'oneOf': typing.List['JsonSchema'],
        'not': 'JsonSchema',
        'maxLength': int,
        'minLength': int,
        'required': typing.List[str],
        'maximum': typing.Union[int, float],
        'minimum': typing.Union[int, float],
        'multipleOf': typing.Union[int, float],
    },
    total=False
)

@dataclasses.dataclass
class JsonSchemaTestSchema:
    description: str
    schema: typing.Union[bool, JsonSchema]
    tests: typing.List[JsonSchemaTestCase]


class ExclusionReason:
    v303_does_not_support_array_of_types = 'v3.0.3 does not support type with array of values'
    v303_requires_array_have_items = 'v3.0.3 requires that items MUST be present if the type is array'
    v303_does_not_support_additionalItems = 'v3.0.3 does not support the additionalItems keyword'
    v303_does_not_support_patternProperties = 'v3.0.3 does not support the patternProperties keyword'
    v303_does_not_support_const = 'v3.0.3 does not support the const keyword'
    v303_does_not_support_boolean_schemas_in_location = 'v3.0.3 does not support boolean schemas in location'
    v303_does_not_support_contains = 'v3.0.3 does not support the contains keyword'
    v303_does_not_support_definitions = 'v3.0.3 does not support the definitions keyword'
    v303_does_not_support_dependencies = 'v3.0.3 does not support the dependencies keyword'
    swagger_parser_enum_type_bug = "swagger-parser has a bug where schema type is incorrectly set for an enum, https://github.com/swagger-api/swagger-parser/issues/1761"
    swagger_parser_validation_missing_bug = 'swagger-parser has a bug where validations are unset, https://github.com/swagger-api/swagger-parser/issues/1762'
    swagger_parser_items_type_bug = "swagger-parser has a bug where schema type is incorrectly set with items, https://github.com/swagger-api/swagger-parser/issues/1763"
    v303_does_not_support_id = 'v3.0.3 does not support the $id keyword'

json_schema_test_draft = 'draft6'
FILEPATH_TO_EXCLUDED_CASE_AND_REASON = {
    (json_schema_test_draft, 'type.json'): {
        'multiple types can be specified in an array': ExclusionReason.v303_does_not_support_array_of_types,
        'type as array with one item': ExclusionReason.v303_does_not_support_array_of_types,
        'type: array or object': ExclusionReason.v303_does_not_support_array_of_types,
        'type: array, object or null': ExclusionReason.v303_does_not_support_array_of_types,
        'array type matches arrays': ExclusionReason.v303_requires_array_have_items,
    },
    (json_schema_test_draft, 'enum.json'): {
        'heterogeneous enum validation': ExclusionReason.swagger_parser_enum_type_bug,
        'heterogeneous enum-with-null validation': ExclusionReason.swagger_parser_enum_type_bug,
    },
    (json_schema_test_draft, 'additionalProperties.json'): {
        'non-ASCII pattern with additionalProperties': ExclusionReason.v303_does_not_support_patternProperties,
        'additionalProperties being false does not allow other properties': ExclusionReason.v303_does_not_support_patternProperties,
    },
    (json_schema_test_draft, 'items.json'): {
        'an array of schemas for items': ExclusionReason.v303_does_not_support_array_of_types,
        'items and subitems': ExclusionReason.v303_does_not_support_definitions,
        'items with boolean schema (true)': ExclusionReason.v303_does_not_support_boolean_schemas_in_location,
        'items with boolean schemas': ExclusionReason.v303_does_not_support_boolean_schemas_in_location,
        'items with boolean schema (false)': ExclusionReason.v303_does_not_support_boolean_schemas_in_location,
        'items with boolean schema (false)': ExclusionReason.v303_does_not_support_boolean_schemas_in_location,
        'a schema given for items': ExclusionReason.swagger_parser_items_type_bug,
    },
}
FILEPATH_TO_EXCLUDE_REASON = {
    (json_schema_test_draft, 'additionalItems.json'): ExclusionReason.v303_does_not_support_additionalItems,
    (json_schema_test_draft, 'const.json'): ExclusionReason.v303_does_not_support_const,
    (json_schema_test_draft, 'boolean_schema.json'): ExclusionReason.v303_does_not_support_boolean_schemas_in_location,
    (json_schema_test_draft, 'contains.json'): ExclusionReason.v303_does_not_support_contains,
    (json_schema_test_draft, 'definitions.json'): ExclusionReason.v303_does_not_support_definitions,
    (json_schema_test_draft, 'dependencies.json'): ExclusionReason.v303_does_not_support_dependencies,
    (json_schema_test_draft, 'exclusiveMaximum.json'): ExclusionReason.swagger_parser_validation_missing_bug,
    (json_schema_test_draft, 'exclusiveMinimum.json'): ExclusionReason.swagger_parser_validation_missing_bug,
    (json_schema_test_draft, 'id.json'): ExclusionReason.v303_does_not_support_id,
}

openapi_additions = 'openapi_additions'

JSON_SCHEMA_TEST_FILE_TO_FOLDERS = {
#     'type.json': (json_schema_test_draft, openapi_additions),
#     'additionalItems.json': (json_schema_test_draft,),
#     'additionalProperties.json': (json_schema_test_draft,),
#     'allOf.json': (json_schema_test_draft,),  # activate later after fixing composition processing
#     'anyOf.json': (json_schema_test_draft,),  # activate later after fixing composition processing
#     'boolean_schema.json': (json_schema_test_draft,),
#     'const.json': (json_schema_test_draft,),
#     'contains.json': (json_schema_test_draft,),
#     'default.json': (json_schema_test_draft,),
#     'definitions.json': (json_schema_test_draft,),
#     'dependencies.json': (json_schema_test_draft,),
#     'enum.json': (json_schema_test_draft,),
#     'exclusiveMaximum.json': (json_schema_test_draft,),
#     'exclusiveMinimum.json': (json_schema_test_draft,),
#     'format.json': (json_schema_test_draft,),
#     'id.json': (json_schema_test_draft,),
#     'infinite-loop-detection.json': (json_schema_test_draft,),  # activate after fixing this
#     'items.json': (json_schema_test_draft,),
#     'maximum.json': (json_schema_test_draft,),
#     'maxItems.json': (json_schema_test_draft,),
#     'maxLength.json': (json_schema_test_draft,),
#     'maxProperties.json': (json_schema_test_draft,),
#     'minimum.json': (json_schema_test_draft,),
#     'minItems.json': (json_schema_test_draft,),
#     'minLength.json': (json_schema_test_draft,),
#     'minProperties.json': (json_schema_test_draft,),
    'multipleOf.json': (json_schema_test_draft,),
}

def get_json_schema_test_schemas(file_path: typing.Tuple[str]) -> typing.List[JsonSchemaTestSchema]:
    json_schema_test_schemas = []
    filename = file_path[-1]
    exclude_file_reason = FILEPATH_TO_EXCLUDE_REASON.get(file_path)
    if exclude_file_reason:
        print(f'Excluding {file_path} because {exclude_file_reason}')
        return
    excluded_case_to_reason = FILEPATH_TO_EXCLUDED_CASE_AND_REASON.get(file_path, {})
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
        'items': 'OpenApiSchema',
        'properties': typing.Dict[str, 'OpenApiSchema']
    }
)

@dataclasses.dataclass
class OpenApiExample:
    description: str
    value: typing.Union[str, int, float, bool, None, list, dict]

OpenApiComponents = typing.TypedDict(
    'OpenApiComponents',
    {
        'schemas': typing.Dict[str, typing.Union[bool, OpenApiSchema]],
        'x-schema-test-examples': typing.Dict[str, typing.Dict[str, JsonSchemaTestCase]]
    }
)

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
        components=OpenApiComponents({
            'schemas': {},
            'x-schema-test-examples': {}
        })
    )

def description_to_component_name(descr: str) -> str:
    res = ''.join(descr.title().split())
    return re.sub(r'[^A-Za-z0-9 ]+', '', res)

def get_test_case_name(test: JsonSchemaTestSchema) -> str:
    res = ''.join(test.description.title().split())
    return re.sub(r'[^A-Za-z0-9 ]+', '', res)

def get_component_schemas_and_test_examples(json_schema_test_file: str, folders: typing.Tuple[str]) -> typing.Dict[str, OpenApiSchema]:
    component_schemas = {}
    component_name_to_test_examples = {}
    for folder in folders:
        file_path_tuple = (folder, json_schema_test_file)
        test_schemas = get_json_schema_test_schemas(file_path_tuple)
        if not test_schemas:
            continue
        for test_schema in test_schemas:
            component_name = description_to_component_name(test_schema.description)
            if isinstance(test_schema.schema, bool):
                component_schemas[component_name] = test_schema.schema
            else:
                component_schemas[component_name] = OpenApiSchema(**test_schema.schema)
            for test in test_schema.tests:
                if component_name not in component_name_to_test_examples:
                    component_name_to_test_examples[component_name] = {}
                test_case_name = get_test_case_name(test)
                component_name_to_test_examples[component_name][test_case_name] = test
    return component_schemas, component_name_to_test_examples

def write_openapi_spec():
    openapi = get_new_openapi()
    for json_schema_test_file, folders in JSON_SCHEMA_TEST_FILE_TO_FOLDERS.items():
        component_schemas, component_name_to_test_examples = (
            get_component_schemas_and_test_examples(json_schema_test_file, folders)
        )
        for component_name, schema in component_schemas.items():
            if component_name in openapi.components['schemas']:
                raise ValueError('A component schema with that name is already defined!')
            openapi.components['schemas'][component_name] = schema
        for component_name, test_examples in component_name_to_test_examples.items():
            if component_name in openapi.components['x-schema-test-examples']:
                raise ValueError('A component schema test example map with that name is already defined!')
            openapi.components['x-schema-test-examples'][component_name] = test_examples
    print(
        yaml.dump(
            dataclasses.asdict(openapi),
            sort_keys=False
        )
    )
    spec_out = 'type.yaml'
    with open(spec_out, 'w') as yaml_out:
        yaml_out.write(
            yaml.dump(
                dataclasses.asdict(openapi),
                sort_keys=False
            )
        )

write_openapi_spec()