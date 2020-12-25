/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a563;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * Bean for handling configuration for templates.
 */
public class TemplateConfiguration {

  @Nonnull private final String templateName;

  @Nullable private Boolean defaultAutomatic;

  @Nonnull private final Map<String, Boolean> automaticByParamName;

  private TemplateConfiguration(@Nonnull String templateName) {
    this.templateName = templateName;
    this.automaticByParamName = new HashMap<>();
  }

  public @Nonnull String getTemplateName() {
    return templateName;
  }

  public @Nonnull Optional<Boolean> isAutomatic(String paramName) {
    return Optional.ofNullable(automaticByParamName.getOrDefault(paramName, defaultAutomatic));
  }

  /**
   * Add configuration from the full raw configuration.
   * 
   * @param rawConfiguration Raw configuration.
   * @param configuration Configuration.
   */
  public static void addConfiguration(
      @Nullable List<String[]> rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration) {
    configuration.clear();
    if (rawConfiguration == null) {
      return;
    }
    for (String[] line : rawConfiguration) {
      addConfiguration(line, configuration);
    }
  }

  /**
   * Add configuration from one line of the raw configuration.
   * 
   * @param rawConfiguration Line of the raw configuration.
   * @param configuration Configuration.
   */
  private static void addConfiguration(
      @Nullable String[] rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration) {
    if ((rawConfiguration == null) || (rawConfiguration.length < 1)) {
      return;
    }
    String[] templates = rawConfiguration[0].split(",");
    Boolean automatic = (rawConfiguration.length > 1) ? Boolean.valueOf(rawConfiguration[1]) : Boolean.FALSE;
    for (String template : templates) {
      if ((template != null) && (template.length() > 0)) {
        String templateName = template.trim();
        TemplateConfiguration templateConfig = configuration.computeIfAbsent(
            templateName,
            k -> new TemplateConfiguration(templateName));
        if (rawConfiguration.length > 2) {
          for (int paramNum = 2; paramNum < rawConfiguration.length; paramNum++) {
            String paramName = rawConfiguration[paramNum].trim();
            templateConfig.automaticByParamName.put(paramName, automatic);
          }
        } else {
          templateConfig.defaultAutomatic = automatic;
        }
      }
    }
  }
}
