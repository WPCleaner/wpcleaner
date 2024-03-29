/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2022  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a568;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.wikipediacleaner.api.check.algorithm.a5xx.TemplateParameterSuggestion;
import org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a567.Numeric;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;

/**
 * Bean for handling configuration for templates.
 */
class TemplateConfiguration {

  @Nonnull private final String templateName;
  
  @Nonnull private final Map<String, Boolean> params;

  @Nonnull private final Map<String, String> refParams;
  
  @Nonnull private final Map<String, List<String>> removeSuffixes;

  private TemplateConfiguration(@Nonnull String templateName) {
    this.templateName = templateName;
    this.params = new HashMap<>();
    this.refParams = new HashMap<>();
    this.removeSuffixes = new HashMap<>();
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
    final String paramName = param.getComputedName();
    Boolean onlyInteger = params.get(paramName);
    if (onlyInteger == null) {
      return Optional.empty();
    }
    final String paramValue = param.getValue();
    final int valueStartIndex = param.getValueStartIndex();
    if (Numeric.isValidFormatnum(analysis, paramValue, valueStartIndex)) {
      return Optional.empty();
    }

    // Ignore some situations
    int beginIndex = param.getBeginIndex();
    if ((analysis.getSurroundingTag(WikiTagType.NOWIKI, beginIndex) != null) ||
        (analysis.getSurroundingTag(WikiTagType.TEMPLATEDATA, beginIndex) != null)) {
      return Optional.empty();
    }

    // Add suggestion for tweaking the value
    List<TemplateParameterSuggestion> results = new ArrayList<>();
    new NumericTemplateParam(analysis, param, onlyInteger).getSuggestion().map(results::add);

    // Add suggestion for splitting the parameter
    String refParam = refParams.get(paramName);
    if (refParam != null) {
      if (paramValue.endsWith(">")) {
        PageElementTag tag = analysis.isInTag(valueStartIndex + paramValue.length() - 1, WikiTagType.REF);
        if ((tag != null) && tag.isComplete()) {
          String contents = analysis.getContents();
          String newValue = contents.substring(valueStartIndex, tag.getCompleteBeginIndex());
          boolean automatic = Numeric.isValidFormatnum(analysis, newValue, valueStartIndex);
          if (!automatic) {
            String leftValue = contents.substring(valueStartIndex, tag.getCompleteBeginIndex());
            automatic = (leftValue.indexOf('<') < 0) || (leftValue.indexOf('>') < 0);
          }
          results.add(TemplateParameterSuggestion.splitParam(
              contents, template, param, tag.getCompleteBeginIndex(), refParam, automatic, automatic));
        }
      }
    }

    // Add suggestion for removing suffixes
    List<String> suffixes = removeSuffixes.get(paramName);
    if (suffixes != null) {
      boolean found = false;
      for (String suffix : suffixes) {
        if (paramValue.endsWith(suffix)) {
          String contents = analysis.getContents();
          String newValue =
              contents.substring(valueStartIndex, valueStartIndex + paramValue.length() - suffix.length()).trim();
          NumericTemplateParam numeric = new NumericTemplateParam(analysis, param, newValue, onlyInteger && !found);
          numeric.getSuggestion().map(results::add);
          found = true;
        }
      }
    }

    return Optional.of(results); 
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

  /**
   * Add reference parameters from the full raw configuration.
   * 
   * @param rawConfiguration Raw configuration for reference parameters.
   * @param configuration Configuration.
   */
  public static void addRefParams(
      @Nullable List<String[]> rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] line : rawConfiguration) {
      addRefParams(line, configuration);
    }
  }

  /**
   * Add reference parameters from one line of the raw configuration.
   * 
   * @param rawConfiguration Line of the raw configuration for reference parameters.
   * @param configuration Configuration.
   */
  public static void addRefParams(
      @Nullable String[] rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration) {
    if ((rawConfiguration == null) || (rawConfiguration.length < 3)) {
      return;
    }
    String templateName = rawConfiguration[0];
    String initialParam = rawConfiguration[1];
    String refParam = rawConfiguration[2];
    TemplateConfiguration templateConfig = configuration.computeIfAbsent(templateName, k -> new TemplateConfiguration(templateName));
    templateConfig.refParams.put(initialParam, refParam);
  }

  /**
   * Add suffixes to remove from the full raw configuration.
   * 
   * @param rawConfiguration Raw configuration for suffixes to remove.
   * @param configuration Configuration.
   */
  public static void addRemoveSuffixes(
      @Nullable List<String[]> rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] line : rawConfiguration) {
      addRemoveSuffixes(line, configuration);
    }
  }

  /**
   * Add suffixes to remove from one line of the raw configuration.
   * 
   * @param rawConfiguration Line of the raw configuration for suffixes to remove.
   * @param configuration Configuration.
   */
  public static void addRemoveSuffixes(
      @Nullable String[] rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration) {
    if ((rawConfiguration == null) || (rawConfiguration.length < 3)) {
      return;
    }
    String templateName = rawConfiguration[0];
    String paramName = rawConfiguration[1];
    TemplateConfiguration templateConfig = configuration.computeIfAbsent(templateName, k -> new TemplateConfiguration(templateName));
    List<String> suffixes = templateConfig.removeSuffixes.computeIfAbsent(paramName, k -> new ArrayList<>());
    for (int paramNum = 2; paramNum < rawConfiguration.length; paramNum++) {
      suffixes.add(rawConfiguration[paramNum]);
    }
  }
}
