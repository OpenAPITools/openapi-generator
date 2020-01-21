---
title: Config Options for rust
sidebar_label: rust
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|library|library template (sub-template) to use.|<dl><dt>**hyper**</dt><dd>HTTP client: Hyper.</dd><dt>**reqwest**</dt><dd>HTTP client: Reqwest.</dd><dl>|hyper|
|packageName|Rust package name (convention: lowercase).| |openapi|
|packageVersion|Rust package version.| |1.0.0|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|Array|java.util.List|
|ArrayList|java.util.ArrayList|
|BigDecimal|java.math.BigDecimal|
|Date|java.util.Date|
|DateTime|org.joda.time.*|
|File|java.io.File|
|HashMap|java.util.HashMap|
|List|java.util.*|
|LocalDate|org.joda.time.*|
|LocalDateTime|org.joda.time.*|
|LocalTime|org.joda.time.*|
|Map|java.util.Map|
|Set|java.util.*|
|Timestamp|java.sql.Timestamp|
|URI|java.net.URI|
|UUID|java.util.UUID|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>File</li>
<li>String</li>
<li>Vec&lt;u8&gt;</li>
<li>bool</li>
<li>char</li>
<li>f32</li>
<li>f64</li>
<li>i16</li>
<li>i32</li>
<li>i64</li>
<li>i8</li>
<li>u16</li>
<li>u32</li>
<li>u64</li>
<li>u8</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>abstract</li>
<li>alignof</li>
<li>as</li>
<li>become</li>
<li>box</li>
<li>break</li>
<li>const</li>
<li>continue</li>
<li>crate</li>
<li>do</li>
<li>else</li>
<li>enum</li>
<li>extern</li>
<li>false</li>
<li>final</li>
<li>fn</li>
<li>for</li>
<li>if</li>
<li>impl</li>
<li>in</li>
<li>let</li>
<li>loop</li>
<li>macro</li>
<li>match</li>
<li>mod</li>
<li>move</li>
<li>mut</li>
<li>offsetof</li>
<li>override</li>
<li>priv</li>
<li>proc</li>
<li>pub</li>
<li>pure</li>
<li>ref</li>
<li>return</li>
<li>self</li>
<li>sizeof</li>
<li>static</li>
<li>struct</li>
<li>super</li>
<li>trait</li>
<li>true</li>
<li>type</li>
<li>typeof</li>
<li>unsafe</li>
<li>unsized</li>
<li>use</li>
<li>virtual</li>
<li>where</li>
<li>while</li>
<li>yield</li>
</ul>
