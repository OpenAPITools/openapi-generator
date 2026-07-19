# Testing and Purpose

This client doesn't require a server to work. It only tests the regex feature. This comamnd should immediately run it:

```bash
go mod tidy && go vet ./... && go test ./... -v
```

It is intended to test the [issue 20079 : [BUG] Golang pattern validation with regex fails on comma](https://github.com/OpenAPITools/openapi-generator/issues/20079)
that is currently unsolved, for these reasons:

1. For `javaComments` test case, when it should use the `^/\*.*\*/|//[^\\n]*$` pattern, the `org.yaml.snakeyaml.scanner parser` that scans the spec file, wrongly tells that `\*` is an invalid escape code.  
   But attempting to enter `^/\\*.*\\*/|//[^\\n]*$` keeps the `\\*` and generates `^/\\*.*\\*/|//[^\n]*$` instead of the wished `^/\*.*\*/|//[^\n]*$`

2. For `windowsAbsolutePath` test case, Go `validator.package v2` doesn't look handling this regexp well, and tell it having a bad parameter.

## Details

It tests the following regex adaptations for Golang generation:

```yaml
code:
   type: string
   pattern: "^[0-9]{2,}$"

creditCard:
   description: "Visa credit card\n
    matches: 4123 6453 2222 1746\n
    non-matches: 3124 5675 4400 4567, 4123-6453-2222-1746"
   type: string

   pattern: "^4[0-9]{3}\\s[0-9]{4}\\s[0-9]{4}\\s[0-9]{4}$"
   # Original was: 4[0-9]{3}\s[0-9]{4}\s[0-9]{4}\s[0-9]{4}

date:
   description: "Some dates\n
    matches: 31/04/1999, 15/12/4567\n
    non-matches: 31/4/1999, 31/4/99, 1999/04/19, 42/67/25456"
   type: string
   pattern: "^([0-2][0-9]|30|31)/(0[1-9]|1[0-2])/[0-9]{4}$"
   # Original was: ([0-2][0-9]|30|31)/(0[1-9]|1[0-2])/[0-9]{4} : unchanged

windowsAbsolutePath:
   description: "Windows absolute path\n
    matches: \\\\server\\share\\file\n
    non-matches: \\directory\\directory2, /directory2"
   type: string

   # This test case doesn't work due to a problem (?) in validator.v2 (?)
   # it issues an unexpected unknown tag or Bad Parameter.

   # pattern: "^([A-Za-z]:|\\)\\[[:alnum:][:whitespace:]!\"#$%&'()+,-.;=@[]^_`{}~.]*$"
   # Original was: ([A-Za-z]:|\\)\\[[:alnum:][:whitespace:]!"#$%&'()+,-.\\;=@\[\]^_`{}~.]*

email1:
   description: "Email Address 1\n
    matches: abc.123@def456.com, _123@abc.ca\n
    non-matches: abc@dummy, ab*cd@efg.hijkl"
   type: string

   pattern: "^[[:word:]\\-.]+@[[:word:]\\-.]+\\.[[:alpha:]]{2,3}$"
   # Original was: [[:word:]\-.]+@[[:word:]\-.]+\.[[:alpha:]]{2,3}

email2:
   description: "Email Address 2\n
    matches: *@qrstuv@wxyz.12345.com, __1234^%@@abc.def.ghijkl\n
    non-matches: abc.123.*&ca, ^%abcdefg123"
   type: string

   pattern: "^.+@.+\\..+$"
   # Original was: .+@.+\..+

htmlHexadecimalColorCode1:
   description: "HTML Hexadecimal Color Code 1\n
    matches: AB1234, CCCCCC, 12AF3B\n
    non-matches: 123G45, 12-44-CC"
   type: string
   pattern: "^[A-F0-9]{6}$"
   # Original was: [A-F0-9]{6} : unchanged

htmlHexadecimalColorCode2:
   description: "HTML Hexadecimal Color Code 2\n
    matches: AB 11 00, CC 12 D3\n
    non-matches: SS AB CD, AA BB CC DD, 1223AB"
   type: string

   pattern: "^[A-F0-9]{2}\\s[A-F0-9]{2}\\s[A-F0-9]{2}$"
   # Original was: [A-F0-9]{2}\s[A-F0-9]{2}\s[A-F0-9]{2}

ipAddress:
   description: "IP Address\n
    matches: 10.25.101.216\n
    non-matches: 0.0.0, 256.89.457.02"
   type: string

   pattern: "^((2(5[0-5]|[0-4][0-9])|1([0-9][0-9])|([1-9][0-9])|[0-9])\\.){3}(2(5[0-5]|[0-4][0-9])|1([0-9][0-9])|([1-9][0-9])|[0-9])$"
   # Original was: ((2(5[0-5]|[0-4][0-9])|1([0-9][0-9])|([1-9][0-9])|[0-9])\.){3}(2(5[0-5]|[0-4][0-9])|1([0-9][0-9])|([1-9][0-9])|[0-9])

javaComments:
   description: "Java Comments\n
    matches: Matches Java comments that are between /* and */, or one line comments prefaced by //\n
    non-matches: a=1"
   type: string

   # This test case doesn't work due to a problem (?) in validator.v2 (?)
   # org.yaml.snakeyaml.scanner declares \* being an invalid escape code at yaml checking step

   # pattern: "^/\*.*\*/|//[^\\n]*$"
   # Original was: /\*.*\*/|//[^\n]*

money:
   description: "\n
    matches: $1.00, -$97.65
    non-matches: $1, 1.00$, $-75.17"
   type: string

   pattern: "^(\\+|-)?\\$[0-9]*\\.[0-9]{2}$"
   # Original was: (\+|-)?\$[0-9]*\.[0-9]{2}

positiveNegativeDecimalValue:
   description: "Positive, negative numbers, and decimal values\n
    matches: +41, -412, 2, 7968412, 41, +41.1, -3.141592653 
    non-matches: ++41, 41.1.19, -+97.14"
   type: string

   pattern: "^(\\+|-)?[0-9]+(\\.[0-9]+)?$"
   # Original was: (\+|-)?[0-9]+(\.[0-9]+)?

password1:
   description: "Passwords 1\n
    matches: abcd, 1234, A1b2C3d4, 1a2B3\n
    non-matches: abc, *ab12, abcdefghijkl"
   type: string
   pattern: "^[[:alnum:]]{4,10}$"
   # Original was: [[:alnum:]]{4,10} : unchanged

password2:
   description: "Passwords 2\n
    matches: AB_cd, A1_b2c3, a123_\n
    non-matches: *&^g, abc, 1bcd"
   type: string

   pattern: "^[a-zA-Z]\\w{3,7}$"
   # Original was: [a-zA-Z]\w{3,7} : unchanged

phoneNumber:
   description: "Phone Numbers\n
    matches: 519-883-6898, 519 888 6898\n
    non-matches: 888 6898, 5198886898, 519 883-6898"
   type: string

   pattern: "^([2-9][0-9]{2}-[2-9][0-9]{2}-[0-9]{4})|([2-9][0-9]{2}\\s[2-9][0-9]{2}\\s[0-9]{4})$"
   # Original was: ([2-9][0-9]{2}-[2-9][0-9]{2}-[0-9]{4})|([2-9][0-9]{2}\s[2-9][0-9]{2}\s[0-9]{4})

sentence1:
   description: "Sentences 1\n
      matches: Hello, how are you?\n
      non-matches: i am fine"
   type: string

   pattern: "^[A-Z0-9].*(\\.|\\?|!)$"
   # Original was: [A-Z0-9].*(\.|\?|!)

sentence2:
   description: "Sentences 2\n
      matches: Hello, how are you?n
      non-matches: i am fine"
   type: string
   pattern: "^[[:upper:]0-9].*[.?!]$"
   # Original was: [[:upper:]0-9].*[.?!] : unchanged

socialSecurityNumber:
   description: "Social Security Number\n
      matches: 123-45-6789\n
      non-matches: 123 45 6789, 123456789, 1234-56-7891"
   type: string
   pattern: "^[0-9]{3}-[0-9]{2}-[0-9]{4}$"
   # Original was: [0-9]{3}-[0-9]{2}-[0-9]{4} : unchanged

url:
   description: "URL\n
      matches: http://www.sample.com, www.sample.com\n
      non-matches: http://sample.com, http://www.sample.comm"
   type: string

   # \. ==> \\.
   pattern: "^(http://)?www\\.[a-zA-Z0-9]+\\.[a-zA-Z]{2,3}$"
   # Original was: (http://)?www\.[a-zA-Z0-9]+\.[a-zA-Z]{2,3}
```

# Go API client for openapi

No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)

## Overview
This API client was generated by the [OpenAPI Generator](https://openapi-generator.tech) project.  By using the [OpenAPI-spec](https://www.openapis.org/) from a remote server, you can easily generate an API client.

- API version: 1.0
- Package version: 1.0.0
- Generator version: 7.21.0-SNAPSHOT
- Build package: org.openapitools.codegen.languages.GoClientCodegen

## Installation

Install the following dependencies:

```sh
go get github.com/stretchr/testify/assert
go get golang.org/x/net/context
```

Put the package under your project folder and add the following in import:

```go
import openapi "github.com/GIT_USER_ID/GIT_REPO_ID"
```

To use a proxy, set the environment variable `HTTP_PROXY`:

```go
os.Setenv("HTTP_PROXY", "http://proxy_name:proxy_port")
```

## Configuration of Server URL

Default configuration comes with `Servers` field that contains server objects as defined in the OpenAPI specification.

### Select Server Configuration

For using other server than the one defined on index 0 set context value `openapi.ContextServerIndex` of type `int`.

```go
ctx := context.WithValue(context.Background(), openapi.ContextServerIndex, 1)
```

### Templated Server URL

Templated server URL is formatted using default variables from configuration or from context value `openapi.ContextServerVariables` of type `map[string]string`.

```go
ctx := context.WithValue(context.Background(), openapi.ContextServerVariables, map[string]string{
	"basePath": "v2",
})
```

Note, enum values are always validated and all unused variables are silently ignored.

### URLs Configuration per Operation

Each operation can use different server URL defined using `OperationServers` map in the `Configuration`.
An operation is uniquely identified by `"{classname}Service.{nickname}"` string.
Similar rules for overriding default operation server index and variables applies by using `openapi.ContextOperationServerIndices` and `openapi.ContextOperationServerVariables` context maps.

```go
ctx := context.WithValue(context.Background(), openapi.ContextOperationServerIndices, map[string]int{
	"{classname}Service.{nickname}": 2,
})
ctx = context.WithValue(context.Background(), openapi.ContextOperationServerVariables, map[string]map[string]string{
	"{classname}Service.{nickname}": {
		"port": "8443",
	},
})
```

## Documentation for API Endpoints

All URIs are relative to *http://localhost*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*DefaultAPI* | [**FooGet**](docs/DefaultAPI.md#fooget) | **Get** /foo | 


## Documentation For Models

 - [FooGet200Response](docs/FooGet200Response.md)
 - [ImportCode](docs/ImportCode.md)


## Documentation For Authorization

Endpoints do not require authorization.


## Documentation for Utility Methods

Due to the fact that model structure members are all pointers, this package contains
a number of utility functions to easily obtain pointers to values of basic types.
Each of these functions takes a value of the given basic type and returns a pointer to it:

* `PtrBool`
* `PtrInt`
* `PtrInt32`
* `PtrInt64`
* `PtrFloat`
* `PtrFloat32`
* `PtrFloat64`
* `PtrString`
* `PtrTime`

## Author



