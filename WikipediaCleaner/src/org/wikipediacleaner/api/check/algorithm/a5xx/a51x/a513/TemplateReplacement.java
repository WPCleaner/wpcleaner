/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a51x.a513;

/** Bean holding configuration for template replacement */
class TemplateReplacement {

  /** Name of the template */
  public final String templateName;

  /** True if the replacement should be full or only the template name */
  public final boolean fullReplacement;

  /** Replacement */
  public final String replacement;

  /** True if replacement can be automatic */
  public final boolean automaticReplacement;

  /** True if the extraction at the end can be automatic */
  public final boolean automaticEndExtraction;

  public static TemplateReplacement build(String[] data) {
    if ((data == null) || (data.length == 0)) {
      return null;
    }
    String name = data[0];
    boolean full = false;
    if (name.startsWith("{{") && name.endsWith("}}")) {
      name = name.substring(2, name.length() - 2);
      full = true;
    }
    return new TemplateReplacement(
        name, full,
        data.length > 1 ? data[1] : null,
        data.length > 2 ? Boolean.parseBoolean(data[2]) : false,
        data.length > 3 ? Boolean.parseBoolean(data[3]) : false);
  }

  private TemplateReplacement(
      String templateName,
      boolean fullReplacement,
      String replacement,
      boolean automaticReplacement,
      boolean automaticEndExtraction) {
    this.templateName = templateName;
    this.fullReplacement = fullReplacement;
    this.replacement = replacement;
    this.automaticReplacement = automaticReplacement;
    this.automaticEndExtraction = automaticEndExtraction;
  }
}
