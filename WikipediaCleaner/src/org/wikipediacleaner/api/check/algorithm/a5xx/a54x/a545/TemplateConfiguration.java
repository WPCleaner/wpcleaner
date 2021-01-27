/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a54x.a545;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.check.algorithm.a5xx.TemplateConfigurationGroup;
import org.wikipediacleaner.api.check.algorithm.a5xx.TemplateParameterSuggestion;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;

/**
 * Bean for handling configuration for templates.
 */
class TemplateConfiguration {

  @Nonnull
  private final String templateName;

  @Nonnull
  private final Map<String, ParameterConfiguration> configByParamName;

  private TemplateConfiguration(@Nonnull String templateName) {
    this.templateName = templateName;
    this.configByParamName = new HashMap<>();
  }

  @Nonnull
  public String getTemplateName() {
    return templateName;
  }

  @Nullable
  public ParameterConfiguration getParamConfiguration(String paramName) {
    return configByParamName.get(paramName);
  }

  /**
   * Analyze a template parameter.
   * 
   * @param analysis Page analysis.
   * @param template Template.
   * @param paramNum Parameter number.
   * @return Suggestions:
   *         Optional.empty() if the parameter is known or not configured for the detection.
   *         A list of suggestions if the parameter is unknown and configured for the detection.
   */
  @Nonnull
  public Optional<List<TemplateParameterSuggestion>> analyzeParam(
      PageAnalysis analysis,
      @Nonnull PageElementTemplate template,
      int paramNum) {

    // Check if an error is present
    PageElementTemplate.Parameter param = template.getParameter(paramNum);
    if (param == null) {
      return Optional.empty();
    }
    String computedName = param.getComputedName();
    ParameterConfiguration paramConfig = configByParamName.get(computedName);
    if (paramConfig == null) {
      return Optional.empty();
    }

    // Check if parameter is unnamed with other parameters after
    boolean unnamedWithOthers = false;
    if (!StringUtils.equals(computedName, param.getName())) {
      for (int paramNum2 = paramNum + 1; paramNum2 < template.getParameterCount(); paramNum2++) {
        PageElementTemplate.Parameter param2 = template.getParameter(paramNum2);
        if (!StringUtils.equals(param2.getComputedName(), param2.getName())) {
          unnamedWithOthers = true;
        }
      }
    }

    // Look for suggestions
    boolean automaticFound = false;
    String contents = analysis.getContents();
    List<TemplateParameterSuggestion> results = new ArrayList<>();
    String existingValue = null;
    if (StringUtils.isNotEmpty(paramConfig.getReplacement())) {
      existingValue = template.getParameterValue(paramConfig.getReplacement());
      boolean automatic = (existingValue == null);
      results.add(TemplateParameterSuggestion.replaceParam(
          contents, param,
          paramConfig.getReplacement(), param.getValue(),
          automatic && !unnamedWithOthers && !automaticFound));
      automaticFound |= automatic;
    }
    boolean automatic =
        StringUtils.isEmpty(param.getValue()) ||
        StringUtils.equals(existingValue, param.getValue());
    results.add(TemplateParameterSuggestion.deleteParam(
        contents, param,
        automatic && !unnamedWithOthers && !automaticFound));
    automaticFound |= automatic;
    results.add(TemplateParameterSuggestion.commentParam(analysis, param, false));
    return Optional.of(results);
  }

  /**
   * Add obsolete parameters from the full raw configuration.
   * 
   * @param rawConfiguration Raw configuration.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  public static void addObsoleteParameters(
      @Nullable List<String[]> rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] line : rawConfiguration) {
      addObsoleteParameters(line, configuration, configurationGroup);
    }
  }

  /**
   * Add obsolete parameters from one line of the raw configuration.
   * 
   * @param rawConfiguration Line of the raw configuration.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  private static void addObsoleteParameters(
      @Nullable String[] rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if ((rawConfiguration == null) || (rawConfiguration.length < 2)) {
      return;
    }
    String[] parameters = rawConfiguration[1].split(",");
    for (String templateName : configurationGroup.getTemplateNames(rawConfiguration[0])) {
      TemplateConfiguration templateConfig = configuration.computeIfAbsent(
          templateName,
          k -> new TemplateConfiguration(templateName));
      for (String parameter : parameters) {
        String parameterName = StringUtils.defaultString(parameter).trim();
        if (StringUtils.isNotEmpty(parameterName)) {
          ParameterConfiguration parameterConfig = templateConfig.configByParamName.computeIfAbsent(
              parameterName,
              k -> new ParameterConfiguration(parameterName));
          if (rawConfiguration.length > 2) {
            parameterConfig.setComment(rawConfiguration[2]);
          }
        }
      }
    }
  }

  /**
   * Add replacement parameters from the full raw configuration.
   * 
   * @param rawConfiguration Raw configuration.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  public static void addReplaceParameters(
      @Nullable List<String[]> rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] line : rawConfiguration) {
      addReplaceParameters(line, configuration, configurationGroup);
    }
  }

  /**
   * Add replacement parameters from one line of the raw configuration.
   * 
   * @param rawConfiguration Line of the raw configuration.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  private static void addReplaceParameters(
      @Nullable String[] rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if ((rawConfiguration == null) || (rawConfiguration.length < 3)) {
      return;
    }
    String[] parameters = rawConfiguration[1].split(",");
    for (String templateName : configurationGroup.getTemplateNames(rawConfiguration[0])) {
      TemplateConfiguration templateConfig = configuration.get(templateName);
      if (templateConfig != null) {
        for (String parameter : parameters) {
          String parameterName = StringUtils.defaultString(parameter).trim();
          if (StringUtils.isNotEmpty(parameterName)) {
            ParameterConfiguration parameterConfig = templateConfig.configByParamName.get(parameterName);
            if (parameterConfig != null) {
              parameterConfig.setReplacement(rawConfiguration[2]);
            }
          }
        }
      }
    }
  }
}
