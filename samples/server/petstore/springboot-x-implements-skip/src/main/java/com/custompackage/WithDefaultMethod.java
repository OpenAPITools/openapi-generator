package com.custompackage;

public interface WithDefaultMethod {

    default String greet(String name) {
        return "Hello, " + name + "!";
    }
}

