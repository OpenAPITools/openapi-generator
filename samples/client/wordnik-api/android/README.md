# Wordnik Android client library

## Overview
This is a full client library for the Wordnik API.  It requires that you have a valid Wordnik API Key--you
can get one for free at http://developer.wordnik.com.

This library is built using the Wordnik [Swagger](http://swagger.wordnik.com) client library generator.  You
can re-generate this library by running ./bin/android-java-wordnik-api.sh from the swagger-codegen project

## Usage
You can use maven central to add this library to your current project:

```xml
<dependency>
  <groupId>com.wordnik</groupId>
  <artifactId>wordnik-android-client</artifactId>
  <version>4.0</version>
</dependency>
```

or with gradle:

```gradle
repositories {
    mavenCentral()
}

dependencies {
    compile 'com.wordnik:wordnik-android-client:4.0'
}
```

or you can pull the source and re-generate the client library with Maven:

```
mvn package
```

Add the library to your project and you're ready to go:

```java
import com.wordnik.client.api.*;
import com.wordnik.client.model.*;

import android.os.AsyncTask;
import android.util.Log;

class WordOfTheDayAsyncTask extends AsyncTask<Void, Void, WordOfTheDay> {
    @Override
    protected WordOfTheDay doInBackground(Void... params) {
        WordsApi api = new WordsApi();
        api.addHeader("api_key", "YOUR_API_KEY");
        try {
            return api.getWordOfTheDay("2014-02-19");
        }
        catch (Exception e) {
            Log.d("WordOfTheDayAsyncTask", e.getMessage());
            return null;
        }
    }

    @Override
    protected void onPostExecute(WordOfTheDay d) {
        Log.d("WordOfTheDayAsyncTask", d.toString());
    }
}
```

You can now invoke the async task anywhere in your app.  Of course you'll want to do
something more in your `onPostExecute` function.

```java
new WordOfTheDayAsyncTask().execute();
```

This project was built with the following minimum requirements:

* Maven 3.0
* Java JDK 6
