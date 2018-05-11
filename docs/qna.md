## Question and Answer

#### What is the governance structure of the OpenAPI Generator project?

OpenAPI generator (openapi-generator) is managed by the [core team members](../README.md#openapi-generator-core-team).


#### Why was it decided to fork Swagger Codegen and to maintain a community-driven version?

There are several reasons:

1. The founding members came to the conclusion that Swagger Codegen 3.0.0 beta contains too many breaking changes while they strongly believe 3.0.0 release should only focus on one thing: OpenAPI specification 3.0 support.
1. Swagger Codegen 3.0.0 beta was evaluated as unstable. Changes made directly to 3.0.0 branch without reviews or tests, were breaking the builds from time to time (e.g. a simple `mvn clean package` failed).
1. Reviews of code changes in the 3.0.0 branch highlighted a lot of code block removal without any reason. This might produce regressions for edge cases discovered previously.
1. Most of the test cases in the generators have been commented out as part of the migration to support OpenAPI 3.0. Test cases are the most valuable assets of the project and should be maintained to ensure a good quality.
1. According to SmartBear, [Swagger Codegen 2.x and 3.x should be supported in parallel for a while](https://github.com/swagger-api/swagger-codegen/issues/7754#issuecomment-375039048) without the possibility to work with git branches to merge the fixes from one branch to the next. Having to implement everything twice is not a good idea and the best use of the Swagger Codegen community resources.
1. Having a community-driven version can bring the project to the next level.

#### Has anything been done in attempt to address the issues before deciding to fork Swagger Codegen and maintain a community-driven version?

There was several conversations with SmartBear (Ron, Hugo) via emails, gitter, Skype call and GitHub issues.
But there was no consensus on the next steps and on the direction for Swagger Codegen 3.0.0.

#### Is there any change to the project license?

No, OpenAPI Generator is still using [Apache license (version 2)](https://www.apache.org/licenses/LICENSE-2.0).

#### What is the difference between Swagger Codegen and OpenAPI Generator?

Swagger Codegen is driven by SmartBear while OpenAPI Generator is driven by the community. More than 40 top contributors and template creators of Swagger Codegen have joined OpenAPI Generator as the founding team members.

#### I’m currently using Swagger Codegen 2.x. How can I upggrade the generator to OpenAPI Generator?

OpenAPI Generator is based on Swagger Codegen 2.4.0-SNAPSHOT version so the migration should be easy, straightforward and almost seamless. Please refer to the [migration guide](migration-from-swagger-codegen.md) for more information. 

