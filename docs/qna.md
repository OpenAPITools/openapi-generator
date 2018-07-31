## Question and Answer

#### What is the governance structure of the OpenAPI Generator project?

OpenAPI generator (openapi-generator) is managed by the [core team members](../README.md#openapi-generator-core-team).


#### Why was it decided to fork Swagger Codegen and to maintain a community-driven version?

There are several reasons:

1. The founding members came to the conclusion that Swagger Codegen 3.0.0 beta contains too many breaking changes while they strongly believe 3.0.0 release should only focus on one thing: OpenAPI 3.0 support.
1. We had concerns about the development practices, which seemed to be contributing to an unstable and insufficiently tested codebase.
1. We could not agree on an efficient evolutionary strategy.  The founding members felt it was important to move forward with OpenAPI 3.0 support, while maintaining backward compatibility with OpenAPI 2.0 in the same codebase.
1. We found that the enhancements, bug fixes submitted for Swagger Codegen 2.x need to be submitted again for Swagger Codegen 3.0.0 branch. Otherwise, these changes will not appear in the 3.0.0 branch. Having to do the PR twice is not the best use of the community resource.
1. The community-driven version has a much more rapid [release cycle](https://github.com/OpenAPITools/openapi-generator/releases/) (weekly patch release, monthly minor release) so users do not need to wait for several months to get a stable release.
1. Having a community-driven version can bring the project to the next level with reliable releases and a clear [roadmap](https://github.com/OpenAPITools/openapi-generator/blob/master/docs/roadmap.adoc).

#### Has anything been done in attempt to address the issues before deciding to fork Swagger Codegen and maintain a community-driven version?

There were several conversations with SmartBear (Ron, Hugo) via emails, gitter, Skype call and GitHub issues.
But there was no consensus on the next steps and on the direction for Swagger Codegen 3.0.0.

#### Is there any change to the project license?

No, OpenAPI Generator is still using [Apache license (version 2)](https://www.apache.org/licenses/LICENSE-2.0).

#### What is the difference between Swagger Codegen and OpenAPI Generator?

Swagger Codegen is driven by SmartBear while OpenAPI Generator is driven by the community. More than 40 top contributors and template creators of Swagger Codegen have joined OpenAPI Generator as the founding team members.

Swagger is a trademark owned by SmartBear and the use of the term "Swagger" found in this project is for demo purpose (reference) only.

#### I’m currently using Swagger Codegen 2.x. How can I upgrade the generator to OpenAPI Generator?

OpenAPI Generator is based on Swagger Codegen 2.4.0-SNAPSHOT version so the migration should be easy, straightforward and almost seamless. Please refer to the [migration guide](migration-from-swagger-codegen.md) for more information. 

#### May I know who maintains this Q&A page?

This Q&A page is maintained by the [core team members](../README.md#openapi-generator-core-team). It's not maintained by a single person nor do these Q&As represent the view of an individual.
