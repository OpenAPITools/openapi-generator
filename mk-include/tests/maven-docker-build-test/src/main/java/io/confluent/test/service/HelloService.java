/*
 * Copyright [2019 - present] Confluent Inc.
 */

package io.confluent.test.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class HelloService {
  private Logger logger = LoggerFactory.getLogger(getClass());

  public String sayHello(String name) {
    logger.debug("Name : {}", name);
    return "Hello " + name;
  }
}
