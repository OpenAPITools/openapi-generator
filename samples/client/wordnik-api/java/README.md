# Wordnik Java client library

## Overview
This is a full client library for the Wordnik API.  It requires that you have a valid Wordnik API Key--you
can get one for free at http://developer.wordnik.com.

This library is built using the Wordnik [Swagger](http://swagger.wordnik.com) client library generator.  You
can re-generate this library by running ./bin/java-wordnik-api.sh from the swagger-codegen project

## Usage
You can use maven central to add this library to your current project:

```xml
<dependency>
  <groupId>com.wordnik</groupId>
  <artifactId>wordnik-java-client</artifactId>
  <version>1.0.0</version>
</dependency>
```
or you can pull the source and re-generate the client library with Maven:

```
mvn package
```

Add the library to your project and you're ready to go:

```java
import com.wordnik.client.api.*;
import com.wordnik.client.model.*;

import com.wordnik.client.common.ApiException;

import java.util.List;

public class Test {
  public static void main(String[] args) {
    if(args.length == 0) {
      System.out.println("Pass your API key as an argument");
      System.exit(0);
    }
    String key = args[0];

    try {
      WordApi api = new WordApi();
      api.getInvoker().addDefaultHeader("api_key", key);
      List<Definition> definitions = api.getDefinitions(
        "Cat",         //  word
        "noun",        //  only get definitions which are "nouns"
        "wiktionary",  //  use wiktionary
        3,             //  fetch only 3 results max
        "true",        //  return related words
        "true",        //  fetch the canonical version of this word (Cat => cat)
        "false"        //  return XML mark-up in response
      );

      for(Definition def : definitions) {
        System.out.print(def);
      }
    }
    catch (ApiException e) {
      e.printStackTrace();
    }
  }  
}
```


This project was built with the following minimum requirements:

* Maven 3.0
* Java JDK 6