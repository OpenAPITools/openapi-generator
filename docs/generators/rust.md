---
title: Config Options for rust
sidebar_label: rust
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|packageName|Rust package name (convention: lowercase).| |openapi|
|packageVersion|Rust package version.| |1.0.0|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|library|library template (sub-template) to use.|<dl><dt>**hyper**</dt><dd>HTTP client: Hyper.</dd><dt>**reqwest**</dt><dd>HTTP client: Reqwest.</dd><dl>|hyper|

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

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>u8</li>
<li>f32</li>
<li>bool</li>
<li>f64</li>
<li>i64</li>
<li>i32</li>
<li>String</li>
<li>i8</li>
<li>i16</li>
<li>u64</li>
<li>Vec&lt;u8&gt;</li>
<li>u32</li>
<li>char</li>
<li>u16</li>
<li>File</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>struct</li>
<li>mod</li>
<li>use</li>
<li>extern</li>
<li>do</li>
<li>type</li>
<li>while</li>
<li>impl</li>
<li>ref</li>
<li>continue</li>
<li>else</li>
<li>loop</li>
<li>trait</li>
<li>let</li>
<li>priv</li>
<li>if</li>
<li>static</li>
<li>in</li>
<li>sizeof</li>
<li>enum</li>
<li>as</li>
<li>final</li>
<li>true</li>
<li>become</li>
<li>virtual</li>
<li>const</li>
<li>fn</li>
<li>for</li>
<li>box</li>
<li>pure</li>
<li>unsafe</li>
<li>mut</li>
<li>yield</li>
<li>offsetof</li>
<li>where</li>
<li>override</li>
<li>typeof</li>
<li>macro</li>
<li>move</li>
<li>proc</li>
<li>alignof</li>
<li>break</li>
<li>false</li>
<li>match</li>
<li>abstract</li>
<li>crate</li>
<li>super</li>
<li>self</li>
<li>pub</li>
<li>return</li>
<li>unsized</li>
</ul>
