/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2023  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a50x.a504;

import java.util.List;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.wikipediacleaner.api.data.Page;

class TemplateConfiguration {

  public final String templateName;
  public final String paramName;
  public final boolean automatic;
  @Nullable public final String commentBefore;

  private TemplateConfiguration(
      final String templateName,
      final String paramName,
      final boolean automatic,
      @Nullable final String commentBefore) {
    this.templateName = templateName;
    this.paramName = paramName;
    this.automatic = automatic;
    this.commentBefore = commentBefore;
  }

  public static void parseConfiguration(
      @Nullable List<String[]> rawConfiguration,
      @Nonnull List<TemplateConfiguration> result) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] line : rawConfiguration) {
      parseConfiguration(line, result);
    }
  }

  private static void parseConfiguration(
      @Nullable String[] rawConfiguration,
      @Nonnull List<TemplateConfiguration> result) {
    if ((rawConfiguration == null) || (rawConfiguration.length < 2)) {
      return;
    }
    result.add(new TemplateConfiguration(
        Page.normalizeTitle(rawConfiguration[0]),
        rawConfiguration[1],
        rawConfiguration.length > 2 ? Boolean.parseBoolean(rawConfiguration[2]) : false,
        rawConfiguration.length > 3 ? rawConfiguration[3] : null));
  }
}
