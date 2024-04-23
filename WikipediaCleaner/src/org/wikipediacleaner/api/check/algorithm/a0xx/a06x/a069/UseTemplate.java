/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2024  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a0xx.a06x.a069;


class UseTemplate {

  public final String templateName;
  public final boolean includesParenthesis;
  public final boolean includesSmall;

  public UseTemplate(String templateName, boolean includesParenthesis, boolean includeSmall) {
    this.templateName = templateName;
    this.includesParenthesis = includesParenthesis;
    this.includesSmall = includeSmall;
  }

  public static UseTemplate of(String[] parameters) {
    if (parameters == null || parameters.length < 3) {
      return null;
    }
    return new UseTemplate(parameters[0], Boolean.parseBoolean(parameters[1]), Boolean.parseBoolean(parameters[2]));
  }
}
