---
title: Config Options for graphql-schema
sidebar_label: graphql-schema
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|packageName|GraphQL package name (convention: lowercase).| |openapi2graphql|
|packageVersion|GraphQL package version.| |1.0.0|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|LocalDateTime|org.joda.time.*|
|Set|java.util.*|
|LocalTime|org.joda.time.*|
|HashMap|java.util.HashMap|
|ArrayList|java.util.ArrayList|
|URI|java.net.URI|
|Timestamp|java.sql.Timestamp|
|LocalDate|org.joda.time.*|
|BigDecimal|java.math.BigDecimal|
|Date|java.util.Date|
|DateTime|org.joda.time.*|
|Array|java.util.List|
|List|java.util.*|
|UUID|java.util.UUID|
|File|java.io.File|
|Map|java.util.Map|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>Float</li>
<li>null</li>
<li>ID</li>
<li>String</li>
<li>Boolean</li>
<li>Int</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>implements</li>
<li>boolean</li>
<li>null</li>
<li>string</li>
<li>query</li>
<li>id</li>
<li>union</li>
<li>float</li>
<li>type</li>
<li>interface</li>
<li>int</li>
</ul>
