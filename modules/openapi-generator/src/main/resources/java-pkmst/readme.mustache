Dear Reader : Thanks for selecting PKMST swagger code generation toolkit for starting your microservice journey.
We believe the hard work we put into this framework will help you kick start your journey sooner and you will find it
easy to scale your service to enterprise grade. Another great thing is you don't need to enable all capabilities as you start,
but can enable as your microservice capabilities needs to be extended to meet strict enterprise standards.

PKMST feature set.
a)Read the Swagger supplied and will create a maven project
b)Create basic controller based on rest verb and path configured in swagger
c)generate default unit test case using junit
d)generate all needed dependency needed to run the microservice on local
e)generate a default manifest file that allows you to push code to cloudfoundry instance ( eg pcf , ibm bluemix etc)

As you mature your knowledge with pkmst, you can leverage vendor extensions feature that will allow you to supply specific configuration
in your swagger and generate respective configuration that will
f)Allow you integrate with registry server (Example Eureka)
g)Allow you to integrate with tracing server (Example Zipkin)
h)Allow you to capture log in your microservice and send to Log management server (Ex ELK or splunk)
i)Allow you to configure Oauth2 security based authorization for your microservice

 Additional Features
 j)generate sample cucumber file and dependency to drive your Behaviour driven development.
 k)generate gatling based performance test, which can be executed via build pipeline like jenkins etc.


Working:
Using OpenAPI specification you can convert any definition to spring boot microservice.
It has the integration with the below services:
eureka registry, zipkin , spring boot admin, circuit breaker.

By default only the circuit breaker annotation is enabled. But one has to write the fallback method. The needed dependency for it is also been enabled. To generate the integration and
related configuration for eureka, zipkin, spring boot admin one has two options:

1) When generating from UI one has to provide vendor extensions inside the OpenAPI specification as below:
swagger: "2.0"
info:
  description: "This is a sample Product Catalogue Server.\
    \  For this sample, you can use the api key `special-key` to test the authorization\
    \ filters."
  version: "1.0.0"
  x-codegen:
    eurekaUri: "http://localhost:8080"
    zipkinUri: "http://localhost:9411"
    springBootAdminUri: "http://localhost:8000"
    pkmstInterceptor: "true"

PLease note the vendor extensions are inside the info tag of the OpenAPI specification. All the tags are case sensitive. Once given all the related configuration and the dependency
will be enabled.

2) When generating from the maven plugin one has to provide configuration inside pom as below:
inside the openapi generator maven plugin under the configuration section

						<configuration>
							<inputSpec>product-openapi.yaml</inputSpec>
							<generatorName>pkmst</generatorName>
							<output>${project.build.directory}/generated-sources</output>
							<configOptions>
								<groupId>com.prokarma</groupId>
								<artifactId>product-catalogue</artifactId>
								<artifactVersion>1.0</artifactVersion>
								<basePackage>com.prokarma.pkmst</basePackage>
								<serviceName>ProductCatalogue</serviceName>
								<eurekaUri></eurekaUri>
								<zipkinUri>http://localhost:9411</zipkinUri>
								<springBootAdminUri>http://localhost:4588</springBootAdminUri>
								<pkmstInterceptor>true</pkmstInterceptor>
								</configOptions>
						</configuration>

 The project has three profiles local, dev, dev-config which can be configured accordingly. Once you have provided the uris you can see the necessary configurations generated inside the local and the dev
 yml files. dev-config is for the config server if you want to use.(also enable the dependency for the config server inside the pom)

 [Note: one has to run the zipkin, eureka, spring boot admin servers to be able to connect from the app. This project assumes that you have all the servers
 up and running.]

 Also provided are the middleware handlers:

1) traceInterceptor:is an id passed in from client and will be unique with an application context. The id will be passed into the backend and return to the consumer for transaction tracing.

2) correlationInterceptor:generates a UUID in the first API/service and pass it to all other APIs/services in the call tree for tracking purpose.

3) rateLimitInterceptor:is a rate limiting handler to limit number of concurrent requests on the server. Once the limit is reached, subsequent requests will be queued for later execution. The size of the queue is configurable.

4) auditInterceptor: audit logs most important info about request and response into audit.log in JSON format with config file that controls which fields to be logged

5) bodyInterceptor:s a body parser middleware that is responsible for parsing the content of the request based on Content-Type in the request header.

To be able to generate the handlers and the necessary configurations one has to provide the pkmstInterceptor key inside the vendor extensions or through
the maven plugin.
Once provided all the handlers are registered in the interceptor registry and can be enabled or disabled through the configuration provided inside
the application yml as below:
interceptor:
   enable:
       audit: true
       body: true
       rateLimit: true
       traceability: true
       correlation: true

For testing we have placeholders for junit test class, integration test class, cucumber sample
feature file(implement according to your needs), gatling load test.

Logstash: Logstash is a tool to collect, process, and forward events and log messages.Sample Logstash configuration file provided that developer can configure to collect wide variety of data.Logstash can dynamically unify data from disparate sources and normalize the data into destinations of their choice.

Ways to run the project:
1) Normal spring boot application

2) Using Dockerfile to run in the container:
dockerfile will be generated inside the project by default. Image can be created using docker cli or through the maven plugin

<build>
  <plugins>
    ...
    <plugin>
      <groupId>com.spotify</groupId>
      <artifactId>docker-maven-plugin</artifactId>
      <version>VERSION GOES HERE</version>
      <configuration>
        <imageName>example</imageName>
        <dockerDirectory>docker</dockerDirectory>
        <resources>
           <resource>
             <targetPath>/</targetPath>
             <directory>${project.build.directory}</directory>
             <include>${project.build.finalName}.jar</include>
           </resource>
        </resources>
      </configuration>
    </plugin>
    ...
  </plugins>
</build>

Use manifest.yml file to push the application to the cloud.

HttpLogging filter is provided for logging in the request and response. Can be found inside the com.prokarma.pkmst.logging package.
Spring security is also provided to secure the resources. Please modify according to your needs.

First run:
Import the project in to the eclipse. Run the app as a spring boot application.The project will run on http://localhost:8008
Swagger ui available on:
http://localhost:8008/swagger-ui.html
If all the configurations have been enabled(depending on the port) below are some of the URls to access:
eureka: http://localhost:8080
zipkin: http://localhost:9411

Please visit below links for more information on PKMST:

Getting started
https://pkmst-getting-started.mybluemix.net

Pkmst examples
https://github.com/ProKarma-Inc/pkmst-getting-started-examples

Pkmst Extensions
https://github.com/ProKarma-Inc/pkmst-extention
