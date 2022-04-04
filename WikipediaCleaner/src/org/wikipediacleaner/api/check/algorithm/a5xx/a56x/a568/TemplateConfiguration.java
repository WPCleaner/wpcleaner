/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2022  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a568;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * Bean for handling configuration for templates.
 */
class TemplateConfiguration {

  @Nonnull private final String templateName;
  
  @Nonnull private final Map<String, Boolean> params;

  private TemplateConfiguration(@Nonnull String templateName) {
    this.templateName = templateName;
    this.params = new HashMap<>();
  }

  /**
   * Retrieves the configuration for a given parameter.
   * 
   * @return TRUE if the parameter accepts only integer,
   *         FALSE if the parameter accepts numeric,
   *         null if the parameter is not formatted with formatnum.
   */
  public Boolean getParam(final String name) {
    return params.get(name);
  }

  /**
   * Add template parameters from the full raw configuration.
   * 
   * @param rawConfiguration Raw configuration for template parameters.
   * @param configuration Configuration.
   */
  public static void addTemplateParams(
      @Nullable List<String[]> rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] line : rawConfiguration) {
      addTemplateParams(line, configuration);
    }
  }

  /**
   * Add template parameters from one line of the raw configuration.
   * 
   * @param rawConfiguration Line of the raw configuration for template parameters.
   * @param configuration Configuration.
   */
  public static void addTemplateParams(
      @Nullable String[] rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration) {
    if ((rawConfiguration == null) || (rawConfiguration.length < 3)) {
      return;
    }
    String templateName = rawConfiguration[0];
    Boolean onlyInteger = Boolean.valueOf(rawConfiguration[1]);
    TemplateConfiguration templateConfig = configuration.computeIfAbsent(templateName, k -> new TemplateConfiguration(templateName));
    for (int paramNum = 2; paramNum < rawConfiguration.length; paramNum++) {
      templateConfig.params.put(rawConfiguration[paramNum], onlyInteger);
    }
  }
}
