/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2024  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.page;

import java.util.List;

import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;

public final class DenyBot {

  private DenyBot() {
    // Utility class: do nothing
  }

  public static boolean preventAutomaticEdit(final PageAnalysis analysis) {
    WPCConfiguration config = analysis.getWPCConfiguration();
    List<String[]> nobotTemplates = config.getStringArrayList(
        WPCConfigurationStringList.NOBOT_TEMPLATES);
    if (nobotTemplates == null) {
      return false;
    }
    return nobotTemplates.stream().anyMatch(nobotTemplate -> preventAutomaticEdit(analysis, nobotTemplate));
  }

  private static boolean preventAutomaticEdit(final PageAnalysis analysis, final String[] nobotTemplate) {
    if (nobotTemplate.length < 1) {
      return false;
    }
    final String templateName = nobotTemplate[0];
    if (nobotTemplate.length == 1) {
      return analysis.hasTemplate(templateName) != null;
    }
    final String denyParameter = nobotTemplate[1];
    return analysis.getTemplates(templateName)
        .stream()
        .anyMatch(template -> template.getParameterValue(denyParameter) != null);
  }
}
