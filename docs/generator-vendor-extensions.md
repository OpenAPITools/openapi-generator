---
id: generator-vendor-extensions
title: Vendor Extensions
---

This is a list of [vendor extensions](https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md#specification-extensions) used by different generators.

<!-- TODO: Auto-generate this list using generator metadata -->

The list may not be up-to-date, the best way is to look for "x-" in the mustache templates.

(All examples are in YAML format)

## ObjC
### x-objc-operationId

To customize the method name, you can provide a different name in x-objc-operationId, e.g.
```yaml
summary: Add a new pet to the store
description: ''
operationId: addPet
x-objc-operationId: CreateNewPet
```  

## Java (Feign)
### x-accepts

A single `Accepts` value as the Feign API client needs a single value for `Accepts` header, e.g.
```yaml
consumes:
  - application/json
  - application/xml
x-accepts: application/json
```

### x-content-type

A single "Content-Type" value as the Feign API client needs a single value for `Content-Type` header, e.g.
```yaml
produces:
  - application/xml
  - application/json
x-content-type: application/json
```

## Rust-server

### x-responseId

Each response may specify a unique `x-responseId`. `rust-server` will use this to name the corresponding enum variant in the code. e.g.

```yaml
paths:
  /ping:
    get:
      responses:
        200:
          description: OK
          x-responseId: Pong
```

## MySQL Schema

### x-mysqlSchema

MySQL schema generator creates vendor extensions based on openapi `dataType` and `dataFormat`. When user defined extensions with same key already exists codegen accepts those as is. It means it won't validate properties or correct it for you. Every model in `definitions` can contain table related and column related extensions like in example below: 

```yaml
definitions:
  Order:
    description: This should be most common InnoDB table
    type: object
    properties:
      id:
        description: >-
          This column should be unsigned BIGINT with AUTO_INCREMENT
        type: integer
        format: int64
        x-mysqlSchema:
          columnDefinition:
            colName: id
            colDataType: DECIMAL
            colDataTypeArguments:
              - argumentValue: 16
                isString: false
                hasMore: true
              - argumentValue: 4
                isString: false
                hasMore: false
            colUnsigned: true
            colNotNull: true
            colDefault:
              defaultValue: AUTO_INCREMENT
              isString: false
              isNumeric: false
              isKeyword: true
            colComment: >-
              Column comment. This column should be unsigned BIGINT with AUTO_INCREMENT
    x-mysqlSchema:
      tableDefinition:
        tblName: orders
        tblStorageEngine: InnoDB
        tblComment: >-
          Table comment. This should be most common InnoDB table
```
> :exclamation: There are properties that are not implemented by now(`tblStorageEngine`), but you can see how generator can be enhanced in future.
