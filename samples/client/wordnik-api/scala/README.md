# Wordnik Scala client library

## Overview
This is a full client library for the Wordnik API.  It requires that you have a valid Wordnik API Key--you
can get one for free at http://developer.wordnik.com.

This library is built using the Wordnik [Swagger](http://swagger.wordnik.com) client library generator.  You
can re-generate this library by running ./bin/scala-wordnik-api.sh from the swagger-codegen project

## Usage
Generate the client library with Maven:

```
mvn package -DskipTests=true
```

Run the tests if you like--note, these require you have an active Wordnik API key, username, and password:

```
mvn package -DAPI_KEY={YOUR_API_KEY} -DUSER_NAME={YOUR_USER_NAME} -DPASSWORD={YOUR_PASSWORD}
```

Add the library to your project and you're ready to go:

```scala
import com.wordnik.client.api._
import com.wordnik.client.model._

object Test {
  def main(args: Array[String]) = {
    if(args.length == 0) {
      println("Please pass your API key")
      sys.exit(0)
    }
    val key = args(0)
    val api = new WordApi
    api.addHeader("api_key", key)
    api.getDefinitions("cat", null, null, 10).flatten.foreach(definition => {
      println(definition)
    })
  }
}
```

This project was built with the following minimum requirements:

* Maven 3.0
* Java JDK 6