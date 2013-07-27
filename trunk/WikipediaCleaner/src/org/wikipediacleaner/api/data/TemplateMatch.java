/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;


/**
 * A simple class to list templates to look for when searching for disambiguation links.
 */
public class TemplateMatch {

  private final String name;
  private final String[] parameters;
  private final String defaultParameters;
  private final boolean good;
  private final boolean helpNeeded;
  private final TemplateReplacement[] replacements;

  public TemplateMatch(
      String template,
      String parameter,
      String defaultParameters,
      boolean good,
      boolean helpNeeded) {
    this(template, parameter, defaultParameters, good, helpNeeded, null);
  }

  public TemplateMatch(
      String template,
      String parameter,
      String defaultParameters,
      boolean good,
      boolean helpNeeded,
      TemplateReplacement[] replacements) {
    this.name = template;
    this.parameters = parameter.split(",");
    this.defaultParameters = defaultParameters;
    this.good = good;
    this.helpNeeded = helpNeeded;
    this.replacements = replacements;
  }

  public String getName() {
    return name;
  }

  public int getParametersCount() {
    return (parameters != null) ? parameters.length : 0;
  }

  public String getParameter(int index) {
    return (parameters != null) ? parameters[index] : null;
  }

  public String getDefaultParameters() {
    return defaultParameters;
  }

  public boolean isGood() {
    return good;
  }

  public boolean isHelpNeeded() {
    return helpNeeded;
  }
  
  public TemplateReplacement getReplacement(String parameter) {
    if ((replacements != null) && (parameter != null)) {
      for (TemplateReplacement replacement : replacements) {
        if (parameter.equals(replacement.getOriginalParameter())) {
          return replacement;
        }
      }
    }
    return null;
  }
}
