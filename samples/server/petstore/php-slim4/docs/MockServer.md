# php-base - PHP Slim 4 Server library for OpenAPI Petstore

## Mock Server Documentation

### Mocker Options
To enable mock server uncomment these lines in `index.php` config file:

```php
/**
 * Mocker Middleware options.
 */
$config['mockerOptions'] = [
    'dataMocker' => new OpenApiDataMocker(),

    'getMockResponseCallback' => function (ServerRequestInterface $request, array $responses) {
        // check if client clearly asks for mocked response
        if (
            $request->hasHeader('X-OpenAPIServer-Mock')
            && $request->getHeader('X-OpenAPIServer-Mock')[0] === 'ping'
        ) {
            if (array_key_exists('default', $responses)) {
                return $responses['default'];
            }

            // return first response
            return $responses[array_key_first($responses)];
        }

        return false;
    },

    'afterCallback' => function ($request, $response) {
        // mark mocked response to distinguish real and fake responses
        return $response->withHeader('X-OpenAPIServer-Mock', 'pong');
    },
];
```

* `dataMocker` is mocker class instance. To create custom data mocker extend `OpenAPIServer\Mock\OpenApiDataMockerInterface`.
* `getMockResponseCallback` is callback before mock data generation. Above example shows how to enable mock feature for only requests with `{{X-OpenAPIServer}}-mock: ping` HTTP header. Adjust requests filtering to fit your project requirements. This function must return single response schema from `$responses` array parameter. **Mock feature is disabled when callback returns anything beside array.**
* `afterCallback` is callback executed after mock data generation. Most obvious use case is append specific HTTP headers to distinguish real and fake responses. **This function must always return response instance.**

### Supported features

All data types supported except specific string formats: `email`, `uuid`, `password` which are poorly implemented.

#### Data Types Support

| Data Type | Data Format |      Supported     |
|:---------:|:-----------:|:------------------:|
| `integer` | `int32`     | :white_check_mark: |
| `integer` | `int64`     | :white_check_mark: |
| `number`  | `float`     | :white_check_mark: |
| `number`  | `double`    |                    |
| `string`  | `byte`      | :white_check_mark: |
| `string`  | `binary`    | :white_check_mark: |
| `boolean` |             | :white_check_mark: |
| `string`  | `date`      | :white_check_mark: |
| `string`  | `date-time` | :white_check_mark: |
| `string`  | `password`  | :white_check_mark: |
| `string`  | `email`     | :white_check_mark: |
| `string`  | `uuid`      | :white_check_mark: |

#### Data Options Support

| Data Type   |         Option         |      Supported     |
|:-----------:|:----------------------:|:------------------:|
| `string`    | `minLength`            | :white_check_mark: |
| `string`    | `maxLength`            | :white_check_mark: |
| `string`    | `enum`                 | :white_check_mark: |
| `string`    | `pattern`              |                    |
| `integer`   | `minimum`              | :white_check_mark: |
| `integer`   | `maximum`              | :white_check_mark: |
| `integer`   | `exclusiveMinimum`     | :white_check_mark: |
| `integer`   | `exclusiveMaximum`     | :white_check_mark: |
| `number`    | `minimum`              | :white_check_mark: |
| `number`    | `maximum`              | :white_check_mark: |
| `number`    | `exclusiveMinimum`     | :white_check_mark: |
| `number`    | `exclusiveMaximum`     | :white_check_mark: |
| `array`     | `items`                | :white_check_mark: |
| `array`     | `additionalItems`      |                    |
| `array`     | `minItems`             | :white_check_mark: |
| `array`     | `maxItems`             | :white_check_mark: |
| `array`     | `uniqueItems`          |                    |
| `object`    | `properties`           | :white_check_mark: |
| `object`    | `maxProperties`        |                    |
| `object`    | `minProperties`        |                    |
| `object`    | `patternProperties`    |                    |
| `object`    | `additionalProperties` |                    |
| `object`    | `required`             |                    |
| `*`         | `$ref`                 | :white_check_mark: |
| `*`         | `allOf`                |                    |
| `*`         | `anyOf`                |                    |
| `*`         | `oneOf`                |                    |
| `*`         | `not`                  |                    |

### Known Limitations

Avoid circular refs in your schema. Schema below can cause infinite loop and `Out of Memory` PHP error:
```yml
# ModelA has reference to ModelB while ModelB has reference to ModelA.
# Mock server will produce huge nested JSON example and ended with `Out of Memory` error.
definitions:
  ModelA:
    type: object
    properties:
      model_b:
        $ref: '#/definitions/ModelB'
  ModelB:
    type: array
    items:
      $ref: '#/definitions/ModelA'
```

Don't ref scalar types, because generator will not produce models which mock server can find. So schema below will cause error:
```yml
# generated build contains only `OuterComposite` model class which referenced to not existed `OuterNumber`, `OuterString`, `OuterBoolean` classes
# mock server cannot mock `OuterComposite` model and throws exception
definitions:
  OuterComposite:
    type: object
    properties:
      my_number:
        $ref: '#/definitions/OuterNumber'
      my_string:
        $ref: '#/definitions/OuterString'
      my_boolean:
        $ref: '#/definitions/OuterBoolean'
  OuterNumber:
    type: number
  OuterString:
    type: string
  OuterBoolean:
    type: boolean
```
