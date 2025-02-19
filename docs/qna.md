---
id: fork-qna
title: "Swagger Codegen Fork: Q&A"
---

This document aims to answer some questions about the fork for historical reference, where these questions don't fit into other documents related to the project itself.

## Why was it decided to fork Swagger Codegen?

There are several reasons:

1. The founding members felt that Swagger Codegen 3.0.0 was diverging too much from the philosophy of Swagger Codegen 2.x.
1. The founding members were concerned that the maintenance overhead of two separate branches (2.x, 3.x) would result in issues similar to those felt in the Python community.
1. The founding members wanted a more rapid [release cycle](https://github.com/OpenAPITools/openapi-generator/releases/) (weekly patch release, monthly minor release) so users do not need to wait for several months to get a stable release.
1. Having a community-driven version allows for innovation, reliability, and a [roadmap](https://github.com/OpenAPITools/openapi-generator/blob/master/docs/roadmap.md) owned by the community.

## Are there any changes to the project license?

No, OpenAPI Generator is still using the [Apache license (version 2)](https://www.apache.org/licenses/LICENSE-2.0).

## I am currently using Swagger Codegen 2.x. How can I upgrade the generator to OpenAPI Generator?

OpenAPI Generator is based on Swagger Codegen `2.4.0-SNAPSHOT` version so the migration should be relatively straightforward. Refer to the [migration guide](./migration-from-swagger-codegen.md) for more information. 

## Who maintains this Q&A page?

This Q&A page is maintained by the [core team members](./core-team.md). It is not maintained by any single person, nor do these Q&As represent the views of any individual or person.
