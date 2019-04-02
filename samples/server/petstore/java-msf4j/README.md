# OpenAPI MSF4J generated server


WSO2 Microservices Framework for Java (MSF4J) is a lightweight high performance framework for developing & running microservices. WSO2 MSF4J is one of the highest performing lightweight Java microservices frameworks. Now openapi-generator will generate micro service skeleton from OpenAPI definition. So you can use this project to convert your OpenAPI definitions to micro service quickly. With this approach you can develop complete micro service within seconds from your OpenAPI definition.

MSF4J generator uses java-msf4j as the default library.
Before you build/run service replace .deploy(new PetApi()) with your actual service class name in Application.java file like .deploy(new ApisAPI()) then it will start that service. If you have multiple service classes add them in , separated manner.
```
        new MicroservicesRunner()
                .deploy(new PetsApi())
                .start();
```

To Use-it : in the generated folder try 
```
mvn package 
```

for build jar, then start your server: 
```
java -jar target/java-msf4j-server-1.0.0.jar
```

Java Microservice listening on default port 9090.
Run the following command or simply go to http://127.0.0.1:9090/pet/12 from your browser:

```
    curl http://127.0.0.1:9090/pet/12
```
