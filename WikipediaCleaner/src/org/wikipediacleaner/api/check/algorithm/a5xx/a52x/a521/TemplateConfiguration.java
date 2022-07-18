/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a52x.a521;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.check.algorithm.a5xx.TemplateConfigurationGroup;
import org.wikipediacleaner.api.data.Page;

/**
 * Bean for handling configuration for templates.
 */
class TemplateConfiguration {

  @Nonnull private static final Logger log = LoggerFactory.getLogger(TemplateConfiguration.class);

  @Nonnull private final String templateName;

  @Nonnull private final Map<String, List<String>> correctFormatsByParameterName;

  private TemplateConfiguration(@Nonnull String templateName) {
    this.templateName = templateName;
    this.correctFormatsByParameterName = new HashMap<>();
  }

  public @Nonnull String getTemplateName() {
    return templateName;
  }

  public List<String> getCorrectFormats(String parameterName) {
    return correctFormatsByParameterName.get(parameterName);
  }

  /**
   * Add correct formats from the full raw configuration.
   * 
   * @param rawConfiguration Raw configuration for correct formats.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  public static void addCorrectFormats(
      @Nullable List<String[]> rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] line : rawConfiguration) {
      addCorrectFormats(line, configuration, configurationGroup);
    }
  }

  /**
   * Add correct formats from one line of the raw configuration.
   * 
   * @param rawConfiguration Line of the raw configuration for correct formats.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  private static void addCorrectFormats(
      @Nullable String[] rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if ((rawConfiguration == null) || (rawConfiguration.length < 3)) {
      return;
    }
    String initialTemplateName = Page.normalizeTitle(rawConfiguration[0]);
    for (String templateName : configurationGroup.getTemplateNames(initialTemplateName)) {
      TemplateConfiguration templateConfig = configuration.computeIfAbsent(
          templateName,
          k -> new TemplateConfiguration(templateName));
      String parameterName = rawConfiguration[1];
      List<String> correctFormats = templateConfig.correctFormatsByParameterName.computeIfAbsent(
          parameterName, k -> new ArrayList<>());
      for (int paramNum = 2; paramNum < rawConfiguration.length; paramNum++) {
        String correctFormat = rawConfiguration[paramNum].trim();
        correctFormats.add(correctFormat);
      }
    }
  }
}
