/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.wordnik.swagger.codegen;

public class CliOption {
  private final String opt;
  private String description;

  public CliOption(String opt, String description) {
    this.opt = opt;
    this.description = description;
  }

  public String getOpt() {
    return opt;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }
}
