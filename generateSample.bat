set path=c:\dev\src\tools\jdk21\bin
set generatorcli=C:\dev\src\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar
set root=C:\dev\openapi-generator
rem "--include-base-dir" "%root%"
rem set files=s*

rem set files=j*
set files=typescript*

set files=csharp*
set files=kotlin*
set files=groovy*
set files=java-okhttp*
set files=java-restclient-springBoot4-jackson3-jspecif*
set files=java*
set files=spring*

rem set files=dart-dio*
rem set files=java-webclient-sealedInterface*
rem set files=rust*
rem set files=rust-reqwest-oneOf.yaml
rem set files=spring-cloud-3-with-optional*
rem set files=spring-boot-file-delegate-optional
java -ea -server -Duser.timezone=UTC -jar %generatorcli% batch  --fail-fast --includes-base-dir %root% bin/configs/%files%
