---
id: release-summary
title: Release Summary
---

## Versioning

We version artifacts in the common `major.minor.patch` strategy.

We decided versions should be incremented according to the following rules. The examples provided below are not exhaustive.

| Part  | Breaking Changes?   |  Rule | Examples |
| ----: | :-----------------: | :---- | :------- |
| major |  YES                 | breaking changes without fallback | <ul><li>New Features</li><li>Large refactors</li><li>Removal of deprecated code</li><li>Changes to coding interfaces</li><li>Large changes to template bound variables</li></ul> |
| minor | ALLOWED             | breaking changes with fallback | <ul><li>Adding new generator behavior which doesn't affect custom templates (or does, with config option for old behavior)</li><li>Changing generator templates in a way in which switching to custom templates results in old behavior</li><li>Introducing deprecated methods in generators or other shared code</li></ul> |
| patch | NO                  | new features without breaking changes | <ul><li>New generators</li><li>Bug fixes in template or generators</li></ul> |

## Cadence

For patch release (e.g. 3.0.5 to 3.0.6), we plan to do it on a _weekly basis_.

For minor release (e.g. 3.1.6 to 3.2.0), we plan to do it on a _monthly basis_.

For major releases (e.g. 3.3.6 to 4.0.0), we plan to do it on a _quarterly basis_.
