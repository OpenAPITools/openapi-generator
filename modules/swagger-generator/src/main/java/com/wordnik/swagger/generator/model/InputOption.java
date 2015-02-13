/**
 *  Copyright 2015 Reverb, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.wordnik.swagger.generator.model;

public class InputOption {
  private String name;
  private String description;
  private Boolean required;
  private String defaultValue;

  public InputOption() {}

  public InputOption(String name, String description, String defaultValue, Boolean required) {
    this.name = name;
    this.description = description;
    this.defaultValue = defaultValue;
    this.required = required;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public String getDescription() {
    return description;
  }

  public void setRequired(Boolean required) {
    this.required = required;
  }

  public Boolean getRequired() {
    return required;
  }

  public void setDefaultValue(String defaultValue) {
    this.defaultValue = defaultValue;
  }

  public String getDefaultValue() {
    return defaultValue;
  }
}
