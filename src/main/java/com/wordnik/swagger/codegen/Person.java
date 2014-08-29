package com.wordnik.swagger.codegen;

public class Person {
  public final String name;
  public Person (String name, int age) {
      this.name = name;
      _age = age;
  }
  public int getAge () {
      return _age;
  }
  protected int _age;
}