---
title: Config Options for mysql-schema
sidebar_label: mysql-schema
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|defaultDatabaseName|Default database name for all MySQL queries| ||
|jsonDataTypeEnabled|Use special JSON MySQL data type for complex model properties. Requires MySQL version 5.7.8. Generates TEXT data type when disabled| |true|
|identifierNamingConvention|Naming convention of MySQL identifiers(table names and column names). This is not related to database name which is defined by defaultDatabaseName option|<dl><dt>**original**</dt><dd>Do not transform original names</dd><dt>**snake_case**</dt><dd>Use snake_case names</dd><dl>|original|
