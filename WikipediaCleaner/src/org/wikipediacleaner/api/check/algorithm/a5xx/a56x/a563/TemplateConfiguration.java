/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a563;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.check.algorithm.a5xx.TemplateConfigurationGroup;
import org.wikipediacleaner.api.data.PageElementTemplate;

/**
 * Bean for handling configuration for templates.
 */
class TemplateConfiguration {

  private final static Logger log = LoggerFactory.getLogger(TemplateConfiguration.class);

  @Nonnull private final String templateName;

  @Nullable private Boolean defaultAutomatic;

  @Nullable private Integer unnamedParameterBegin;

  @Nullable private Boolean unnamedParameterAutomatic;

  @Nonnull private final Map<String, Boolean> automaticByParamName;

  @Nonnull private final Set<String> ignoredParams;

  private TemplateConfiguration(@Nonnull String templateName) {
    this.templateName = templateName;
    this.automaticByParamName = new HashMap<>();
    this.ignoredParams = new HashSet<>();
  }

  public @Nonnull String getTemplateName() {
    return templateName;
  }

  /**
   * Analyze a template parameter.
   * 
   * @param template Template.
   * @param paramNum Parameter number.
   * @return Analysis of the parameter:
   *         Optional.empty() is the parameter is not empty or not configured for the detection.
   *         FALSE if the parameter is empty, configured for the detection, but no automatic removal.
   *         TRUE if the parameter is empty, configured for the detection, with automatic removal.
   */
  public @Nonnull Optional<Boolean> isAutomatic(PageElementTemplate template, int paramNum) {
    PageElementTemplate.Parameter param = template.getParameter(paramNum);
    if ((param == null) || StringUtils.isNotEmpty(param.getValue())) {
      return Optional.empty();
    }
    String name = param.getName();
    String computedName = param.getComputedName();
    if (ignoredParams.contains(computedName)) {
      return Optional.empty();
    }
    if (StringUtils.isEmpty(name) && (unnamedParameterBegin != null)) {
      if (StringUtils.isNotEmpty(computedName)) {
        try {
          if (Integer.parseInt(computedName) >= unnamedParameterBegin) {
            for (int paramNumAfter = paramNum + 1; paramNumAfter < template.getParameterCount(); paramNumAfter++) {
              PageElementTemplate.Parameter paramAfter = template.getParameter(paramNumAfter);
              if ((paramAfter != null) &&
                  (paramAfter.getName() == null) &&
                  StringUtils.isNotEmpty(paramAfter.getValue())) {
                log.info(
                    "Prevent removal of unnamed parameter {} in {} because of non-empty unnamed parameter {}",
                    computedName, template.getTemplateName(),
                    paramAfter.getComputedName());
                return Optional.of(Boolean.FALSE);
              }
            }
            return Optional.ofNullable(unnamedParameterAutomatic);
          }
        } catch (NumberFormatException e) {
          log.error("Error parsing parameter computed name {}", computedName);
        }
      }
    }
    return Optional.ofNullable(automaticByParamName.getOrDefault(computedName, defaultAutomatic));
  }

  /**
   * Add configuration for templates from the full raw configuration.
   * 
   * @param rawConfiguration Raw configuration.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  public static void addConfiguration(
      @Nullable List<String[]> rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] line : rawConfiguration) {
      addConfiguration(line, configuration, configurationGroup);
    }
  }

  /**
   * Add configuration for templates from one line of the raw configuration.
   * 
   * @param rawConfiguration Line of the raw configuration.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  private static void addConfiguration(
      @Nullable String[] rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if ((rawConfiguration == null) || (rawConfiguration.length < 1)) {
      return;
    }
    Boolean automatic = (rawConfiguration.length > 1) ? Boolean.valueOf(rawConfiguration[1]) : Boolean.FALSE;
    for (String templateName : configurationGroup.getTemplateNames(rawConfiguration[0])) {
      TemplateConfiguration templateConfig = configuration.computeIfAbsent(
          templateName,
          k -> new TemplateConfiguration(templateName));
      if (rawConfiguration.length > 2) {
        for (int paramNum = 2; paramNum < rawConfiguration.length; paramNum++) {
          String paramName = rawConfiguration[paramNum].trim();
          if (paramName.endsWith("+")) {
            try {
              templateConfig.unnamedParameterBegin =
                  Integer.valueOf(paramName.substring(0, paramName.length() - 1));
              templateConfig.unnamedParameterAutomatic = automatic;
            } catch (NumberFormatException e) {
              log.error(
                  "Incorrect configuration for #563, parameter named {} for template {}",
                  paramName, templateName);
            }
          } else {
            templateConfig.automaticByParamName.put(paramName, automatic);
          }
        }
      } else {
        templateConfig.defaultAutomatic = automatic;
      }
    }
  }

  /**
   * Add configuration for ignored parameters from the full raw configuration.
   * 
   * @param rawConfiguration Raw configuration.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  public static void addIgnoredParameters(
      @Nullable List<String[]> rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] line : rawConfiguration) {
      addIgnoredParameters(line, configuration, configurationGroup);
    }
  }

  /**
   * Add configuration for ignored parameters from one line of the raw configuration.
   * 
   * @param rawConfiguration Line of the raw configuration.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  private static void addIgnoredParameters(
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
        String paramName = rawConfiguration[paramNum].trim();
        templateConfig.ignoredParams.add(paramName);
      }
    }
  }
}
