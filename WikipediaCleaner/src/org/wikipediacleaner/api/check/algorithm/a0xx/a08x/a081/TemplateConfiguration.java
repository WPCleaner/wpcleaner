/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2023  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a0xx.a08x.a081;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.wikipediacleaner.api.check.algorithm.a5xx.TemplateConfigurationGroup;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.utils.SimpleTextProvider;
import org.wikipediacleaner.utils.TextProvider;

/**
 * Bean for handling configuration for templates.
 */
public class TemplateConfiguration {

  @Nonnull private final String templateName;
  @Nonnull private final List<String> titles;

  private TemplateConfiguration(@Nonnull String templateName) {
    this.templateName = templateName;
    this.titles = new ArrayList<>();
  }

  public @Nonnull String getTemplateName() {
    return templateName;
  }

  public List<TextProvider> getTextProviders(PageElementTemplate template) {
    return titles.stream()
        .map(template::getParameterIndex)
        .filter(index -> index >= 0)
        .map(template::getParameter)
        .map(PageElementTemplate.Parameter::getStrippedValue)
        .filter(Objects::nonNull)
        .map(SimpleTextProvider::new)
        .collect(Collectors.toList());
  }

  /**
   * Add parameters for title from the full raw configuration.
   * 
   * @param rawConfiguration Raw configuration for parameters for title.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  public static void addParametersForTitle(
      @Nullable List<String[]> rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] line : rawConfiguration) {
      addParametersForTitle(line, configuration, configurationGroup);
    }
  }

  /**
   * Add parameters for title from one line of the raw configuration.
   * 
   * @param rawConfiguration Line of the raw configuration for parameters for title
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  private static void addParametersForTitle(
      @Nullable String[] rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if ((rawConfiguration == null) || (rawConfiguration.length < 2)) {
      return;
    }
    for (String templateName : configurationGroup.getTemplateNames(rawConfiguration[0])) {
      TemplateConfiguration templateConfig = configuration.computeIfAbsent(
          templateName,
          k -> new TemplateConfiguration(templateName));
      for (int paramNum = 1; paramNum < rawConfiguration.length; paramNum++) {
        templateConfig.titles.add(rawConfiguration[paramNum].trim());
      }
    }
  }
}
