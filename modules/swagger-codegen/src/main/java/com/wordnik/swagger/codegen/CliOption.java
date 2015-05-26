/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.wordnik.swagger.codegen;

import org.apache.commons.cli.Option;

public class CliOption extends Option {

  private Boolean langSpecific = false;

  public CliOption(String opt, String description) throws IllegalArgumentException {
    super(opt, description);
  }

  public CliOption(String opt, boolean hasArg, String description) throws IllegalArgumentException {
    super(opt, hasArg, description);
  }

  public CliOption(String opt, String longOpt, boolean hasArg, String description) throws IllegalArgumentException {
    super(opt, longOpt, hasArg, description);
  }

  public CliOption(String opt, String description, Boolean langSpecific) throws IllegalArgumentException {
    this(opt, description);
    this.langSpecific = langSpecific;
  }

  public CliOption(String opt, boolean hasArg, String description, Boolean langSpecific) throws IllegalArgumentException {
    this(opt, hasArg, description);
    this.langSpecific = langSpecific;
  }

  public CliOption(String opt, String longOpt, boolean hasArg, String description, Boolean langSpecific) throws IllegalArgumentException {
    this(opt, longOpt, hasArg, description);
    this.langSpecific = langSpecific;
  }

  public Boolean isLangSpecific() {
    return langSpecific;
  }
}
