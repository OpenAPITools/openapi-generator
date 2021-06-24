---
name: Bug report
about: Create a bug report to help us improve
title: "[BUG] Description"
labels: 'Issue: Bug'
assignees: ''

---

#### Bug Report Checklist

- [ ] Have you provided a full/minimal spec to reproduce the issue?
- [ ] Have you validated the input using an OpenAPI validator ([example](https://apidevtools.org/swagger-parser/online/))?
- [ ] Have you [tested with the latest master](https://github.com/OpenAPITools/openapi-generator/wiki/FAQ#how-to-test-with-the-latest-master-of-openapi-generator) to confirm the issue still exists?
- [ ] Have you searched for related issues/PRs?
- [ ] What's the actual output vs expected output?
- [ ] [Optional] Sponsorship to speed up the bug fix or feature request ([example](https://github.com/OpenAPITools/openapi-generator/issues/6178))

<!--
Please follow the issue template below for bug reports.
Also please indicate in the issue title which language/library is concerned. Eg:  [BUG][JAVA] Bug generating foo with bar 
-->

##### Description

<!-- describe what is the question, suggestion or issue and why this is a problem for you. -->

##### openapi-generator version

<!-- which version of openapi-generator are you using, is it a regression? -->

##### OpenAPI declaration file content or url

<!-- if it is a bug, a json or yaml that produces it.
If you post the code inline, please wrap it with
```yaml
(here your code)
```
(for YAML code) or
```json
(here your code)
```
(for JSON code), so it becomes more readable. If it is longer than about ten lines,
please create a Gist (https://gist.github.com) or upload it somewhere else and
link it here.
  -->

##### Generation Details

<!-- 
    Prefer CLI steps, including the language, libraries and various options. 
    Providing config-based settings allows for simpler testing across CLI and plugins. 
    For examples, see https://github.com/OpenAPITools/openapi-generator/tree/master/bin/configs
-->

##### Steps to reproduce

<!-- unambiguous set of steps to reproduce the bug.-->

##### Related issues/PRs

<!-- has a similar issue/PR been reported/opened before? Please do a search in https://github.com/openapitools/openapi-generator/issues?utf8=%E2%9C%93&q=is%3Aissue%20 -->

##### Suggest a fix

<!-- if you can't fix the bug yourself, perhaps you can point to what might be
  causing the problem (line of code or commit), or simply make a suggestion -->
