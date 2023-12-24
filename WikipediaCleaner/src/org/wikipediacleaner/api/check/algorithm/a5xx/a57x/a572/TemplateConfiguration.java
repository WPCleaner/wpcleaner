/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a57x.a572;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.check.algorithm.a5xx.TemplateConfigurationGroup;
import org.wikipediacleaner.api.check.algorithm.a5xx.TemplateParameterSuggestion;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;

/**
 * Bean for handling configuration for templates.
 */
class TemplateConfiguration {

  @Nonnull private static final Logger log = LoggerFactory.getLogger(TemplateConfiguration.class);

  @Nonnull private final String templateName;

  @Nonnull private final Map<String, Set<String>> caseSensitiveValues;
  @Nonnull private final Map<String, Set<String>> caseInsensitiveValues;

  @Nonnull private final Map<String, Map<String, String>> valuesToReplace;

  @Nonnull private final Map<String, List<String>> confusions;

  private TemplateConfiguration(@Nonnull String templateName) {
    this.templateName = templateName;
    this.caseSensitiveValues = new HashMap<>();
    this.caseInsensitiveValues = new HashMap<>();
    this.valuesToReplace = new HashMap<>();
    this.confusions = new HashMap<>();
  }

  public @Nonnull String getTemplateName() {
    return templateName;
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
  public @Nonnull Optional<List<TemplateParameterSuggestion>> analyzeParam(
      PageAnalysis analysis,
      @Nonnull PageElementTemplate template,
      int paramNum) {

    // Check if an error is present
    PageElementTemplate.Parameter param = template.getParameter(paramNum);
    if (param == null) {
      return Optional.empty();
    }
    String computedName = param.getComputedName();
    Set<String> possibleCaseSensitive = caseSensitiveValues.get(computedName);
    Set<String> possibleCaseInsensitive = caseInsensitiveValues.get(computedName);
    if ((possibleCaseSensitive == null) && (possibleCaseInsensitive == null)) {
      return Optional.empty();
    }
    String value = param.getStrippedValue();
    if (value.isEmpty() ||
        (possibleCaseSensitive != null && possibleCaseSensitive.contains(value)) ||
        (possibleCaseInsensitive != null && possibleCaseInsensitive.contains(value.toLowerCase(Locale.ROOT)))) {
      return Optional.empty();
    }

    // Handle parameters with replacements
    String contents = analysis.getContents();
    String newValue = valuesToReplace.getOrDefault(computedName, Collections.emptyMap()).get(value);
    if (newValue != null) {
      return Optional.of(Collections.singletonList(
          TemplateParameterSuggestion.replaceOrDeleteParam(
              contents, template, param, computedName, newValue, true, true)));
    }

    // Default return
    List<TemplateParameterSuggestion> result = new ArrayList<>();
    if (possibleCaseSensitive != null) {
      possibleCaseSensitive
          .stream()
          .map(replacement -> TemplateParameterSuggestion.replaceOrDeleteParam(
              contents, template, param, computedName, replacement, false, false))
          .forEach(result::add);
    }
    if (possibleCaseInsensitive != null) {
      possibleCaseInsensitive
          .stream()
          .map(replacement -> TemplateParameterSuggestion.replaceOrDeleteParam(
              contents, template, param, computedName, replacement, false, false))
          .forEach(result::add);
    }
    return Optional.of(result);
  }

  /**
   * Add known parameters from the full raw configuration.
   * 
   * @param rawConfiguration Raw configuration for known parameters.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  public static void addKnownParameters(
      @Nullable List<String[]> rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] line : rawConfiguration) {
      addKnownParameters(line, configuration, configurationGroup);
    }
  }

  /**
   * Add known parameters from one line of the raw configuration.
   * 
   * @param rawConfiguration Line of the raw configuration for known parameters.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  private static void addKnownParameters(
      @Nullable String[] rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if ((rawConfiguration == null) || (rawConfiguration.length < 3)) {
      return;
    }
    for (String templateName : configurationGroup.getTemplateNames(rawConfiguration[0])) {
      TemplateConfiguration templateConfig = configuration.computeIfAbsent(
          templateName,
          k -> new TemplateConfiguration(templateName));
      String paramName = rawConfiguration[1].trim();
      boolean caseSensitive = Boolean.parseBoolean(rawConfiguration[2].trim());
      Set<String> values = new HashSet<>();
      for (int paramNum = 3; paramNum < rawConfiguration.length; paramNum++) {
        values.add(rawConfiguration[paramNum].trim());
      }
      if (caseSensitive) {
        templateConfig.caseSensitiveValues.put(paramName, values);
      } else {
        templateConfig.caseInsensitiveValues.put(paramName, values);
      }
    }
  }

  /**
   * Add values that can be safely replaced from the full raw configuration.
   * 
   * @param rawConfiguration Raw configuration for values that can be safely replaced.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  public static void addValuesToReplace(
      @Nullable List<String[]> rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] line : rawConfiguration) {
      addValuesToReplace(line, configuration, configurationGroup);
    }
  }

  /**
   * Add values that can be safely replaced from one line of the raw configuration.
   * 
   * @param rawConfiguration Line of the raw configuration for values that can be safely replaced.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  private static void addValuesToReplace(
      @Nullable String[] rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if ((rawConfiguration == null) || (rawConfiguration.length < 4)) {
      return;
    }
    for (String templateName : configurationGroup.getTemplateNames(rawConfiguration[0])) {
      TemplateConfiguration templateConfig = configuration.computeIfAbsent(
          templateName,
          k -> new TemplateConfiguration(templateName));
      String paramName = rawConfiguration[1].trim();
      String oldParamValue = rawConfiguration[2].trim();
      String newParamValue = rawConfiguration[3].trim();
      templateConfig.valuesToReplace.computeIfAbsent(
          paramName,
          k -> new HashMap<>()).put(oldParamValue, newParamValue);
    }
  }

  /**
   * Add possible parameter confusions from the full raw configuration.
   * 
   * @param rawConfiguration Raw configuration for possible parameter confusions.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  public static void addConfusions(
      @Nullable List<String[]> rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] line : rawConfiguration) {
      addConfusions(line, configuration, configurationGroup);
    }
  }

  /**
   * Add possible parameter confusions from one line of the raw configuration.
   * 
   * @param rawConfiguration Line of the raw configuration for possible parameter confusions.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  private static void addConfusions(
      @Nullable String[] rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if ((rawConfiguration == null) || (rawConfiguration.length < 3)) {
      return;
    }
    for (String templateName : configurationGroup.getTemplateNames(rawConfiguration[0])) {
      TemplateConfiguration templateConfig = configuration.computeIfAbsent(
          templateName,
          k -> new TemplateConfiguration(templateName));
      String paramName = rawConfiguration[1].trim();
      List<String> values = new ArrayList<>();
      for (int paramNum = 2; paramNum < rawConfiguration.length; paramNum++) {
        values.add(rawConfiguration[paramNum].trim());
      }
      templateConfig.confusions.put(paramName, values);
    }
  }
  
  public List<String> getPossibleConfusion(@Nonnull String paramName) {
    return confusions.getOrDefault(paramName, Collections.emptyList());
  }
  
  public boolean isPossibleConfusion(@Nonnull String paramName, @Nonnull String otherParamName) {
    return getPossibleConfusion(paramName).contains(otherParamName);
  }
}
