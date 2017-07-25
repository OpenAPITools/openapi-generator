# Guidelines For Contributing

## Before submitting an issue

 - If you're not using the latest master to generate API clients or server stubs, please give it another try by pulling the latest master as the issue may have already been addressed. Ref: [Getting Started](https://github.com/swagger-api/swagger-codegen#getting-started)
 - Search the [open issue](https://github.com/swagger-api/swagger-codegen/issues) and [closed issue](https://github.com/swagger-api/swagger-codegen/issues?q=is%3Aissue+is%3Aclosed) to ensure no one else has reported something similar before.
 - File an [issue ticket](https://github.com/swagger-api/swagger-codegen/issues/new) by providing all the required information.
 - Test with the latest master by building the JAR locally to see if the issue has already been addressed.
 - You can also make a suggestion or ask a question by opening an "issue".

## Before submitting a PR

 - Search the [open issue](https://github.com/swagger-api/swagger-codegen/issues) to ensure no one else has reported something similar and no one is actively working on similar proposed change.
 - If no one has suggested something similar, open an ["issue"](https://github.com/swagger-api/swagger-codegen/issues) with your suggestion to gather feedback from the community.
 - It's recommended to **create a new git branch** for the change so that the merge commit message looks nicer in the commit history.

## How to contribute

### git

If you're new to git, you may find the following FAQs useful:

https://github.com/swagger-api/swagger-codegen/wiki/FAQ#git

### Code generators

All the code generators can be found in [modules/swagger-codegen/src/main/java/io/swagger/codegen/languages](https://github.com/swagger-api/swagger-codegen/tree/master/modules/swagger-codegen/src/main/java/io/swagger/codegen/languages)

### Templates

All the templates ([mustache](https://mustache.github.io/)) can be found in [modules/swagger-codegen/src/main/resources](https://github.com/swagger-api/swagger-codegen/tree/master/modules/swagger-codegen/src/main/resources).

For a list of variables available in the template, please refer to this [page](https://github.com/swagger-api/swagger-codegen/wiki/Mustache-Template-Variables)


### Style guide
Code change should conform to the programming style guide of the respective languages:
- Android: https://source.android.com/source/code-style.html
- Bash: https://github.com/bahamas10/bash-style-guide
- C#: https://msdn.microsoft.com/en-us/library/vstudio/ff926074.aspx
- C++: https://google.github.io/styleguide/cppguide.html
- C++ (Tizen): https://wiki.tizen.org/Native_Platform_Coding_Idiom_and_Style_Guide#C.2B.2B_Coding_Style
- Clojure: https://github.com/bbatsov/clojure-style-guide
- Elixir: https://github.com/christopheradams/elixir_style_guide
- Eiffel: https://www.eiffel.org/doc/eiffel/Coding%20Standards
- Erlang: https://github.com/inaka/erlang_guidelines
- Haskell: https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md
- Java: https://google.github.io/styleguide/javaguide.html
- JavaScript: https://github.com/airbnb/javascript/
- Groovy: http://groovy-lang.org/style-guide.html
- Go: https://github.com/golang/go/wiki/CodeReviewComments
- ObjC: https://github.com/NYTimes/objective-c-style-guide
- Perl: http://perldoc.perl.org/perlstyle.html
- PHP: https://github.com/php-fig/fig-standards/blob/master/accepted/PSR-2-coding-style-guide.md
- Python: https://www.python.org/dev/peps/pep-0008/
- Ruby: https://github.com/bbatsov/ruby-style-guide
- Scala: http://docs.scala-lang.org/style/
- Swift: https://developer.apple.com/library/prerelease/ios/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html
- TypeScript: https://github.com/Microsoft/TypeScript/wiki/Coding-guidelines

For other languages, feel free to suggest.

You may find the current code base not 100% conform to the coding style and we welcome contributions to fix those.

For [Vendor Extensions](https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md#vendorExtensions), please follow the naming convention below:
- For general vendor extension, use lower case and hyphen. e.g. `x-is-unique`, `x-content-type`
- For language-specified vendor extension, put it in the form of `x-{lang}-{extension-name}`. e.g. `x-objc-operation-id`, `x-java-feign-retry-limit`
- For a list of existing vendor extensions in use, please refer to https://github.com/swagger-api/swagger-codegen/wiki/Vendor-Extensions. If you've added new vendor extensions as part of your PR, please update the wiki page.

### Testing

To add test cases (optional) covering the change in the code generator, please refer to [modules/swagger-codegen/src/test/java/io/swagger/codegen](https://github.com/swagger-api/swagger-codegen/tree/master/modules/swagger-codegen/src/test/java/io/swagger/codegen)

To test the templates, please perform the following:
- Update the [Petstore](http://petstore.swagger.io/) sample by running the shell script under `bin` folder. For example, run `./bin/ruby-petstore.sh` to update the Ruby PetStore API client under [`samples/client/petstore/ruby`](https://github.com/swagger-api/swagger-codegen/tree/master/samples/client/petstore/ruby) For Windows, the batch files can be found under `bin\windows` folder. (If you find that there are new files generated or unexpected changes as a result of the update, that's not unusual as the test cases are added to the OpenAPI/Swagger spec from time to time. If you've questions or concerns, please open a ticket to start a discussion)
- Run the tests in the sample folder, e.g. in `samples/client/petstore/ruby`, run `mvn integration-test -rf :RubyPetstoreClientTests`. (some languages may not contain unit testing for Petstore and we're looking for contribution from the community to implement those tests)
- Finally, git commit the updated samples files: `git commit -a`
  (`git add -A` if added files with new test cases)

To start the CI tests, you can run `mvn verify -Psamples` (assuming you've all the required tools installed to run tests for different languages) or you can leverage http://travis-ci.org to run the CI tests by adding your own Swagger-Codegen repository.

### Tips
- Smaller changes are easier to review
- [Optional] For bug fixes, provide a OpenAPI Spec to repeat the issue so that the reviewer can use it to confirm the fix
- Add test case(s) to cover the change
- Document the fix in the code to make the code more readable
- Make sure test cases passed after the change (one way is to leverage https://travis-ci.org/ to run the CI tests)
- File a PR with meaningful title, description and commit messages. A good example is [PR-3306](https://github.com/swagger-api/swagger-codegen/pull/3306)
- Recommended git settings
   - `git config --global core.autocrlf input` to tell Git convert CRLF to LF on commit but not the other way around 
- To close an issue (e.g. issue 1542) automatically after a PR is merged, use keywords "fix", "close", "resolve" in the PR description, e.g. `fix #1542`. (Ref: [closing issues using keywords](https://help.github.com/articles/closing-issues-using-keywords/))
