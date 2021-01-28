/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a564;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.similarity.LevenshteinDistance;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.check.algorithm.a5xx.TemplateConfigurationGroup;
import org.wikipediacleaner.api.check.algorithm.a5xx.TemplateParameterSuggestion;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.utils.string.CharacterUtils;

/**
 * Bean for handling configuration for templates.
 */
class TemplateConfiguration {

  @Nonnull private static final Logger log = LoggerFactory.getLogger(TemplateConfiguration.class);

  @Nonnull private final String templateName;

  @Nonnull private final Set<String> knownParams;

  @Nonnull private final Set<String> knownNumericalParameter;

  @Nonnull private final Set<String> paramsToDelete;

  @Nonnull private final Set<String> paramsToComment;

  @Nonnull private final Map<String, Set<String>> paramsToReplaceByName;

  @Nonnull private final LevenshteinDistance levenshteinDistance;

  private TemplateConfiguration(@Nonnull String templateName) {
    this.templateName = templateName;
    this.knownParams = new HashSet<>();
    this.knownNumericalParameter = new HashSet<>();
    this.paramsToDelete = new HashSet<>();
    this.paramsToComment = new HashSet<>();
    this.paramsToReplaceByName = new HashMap<>();
    this.levenshteinDistance = new LevenshteinDistance(4);
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
    if (knownParams.contains(computedName)) {
      return Optional.empty();
    }
    int lastIndex = computedName.length();
    while ((lastIndex > 0) && Character.isDigit(computedName.charAt(lastIndex - 1))) {
      lastIndex--;
    }
    if (lastIndex < computedName.length()) {
      if (knownNumericalParameter.contains(computedName.substring(0, lastIndex))) {
        return Optional.empty();
      }
    }

    // Handle stranded "=" sign
    List<TemplateParameterSuggestion> results = new ArrayList<>();
    String name = param.getName();
    String contents = analysis.getContents();
    if (StringUtils.isEmpty(computedName) && StringUtils.isEmpty(param.getValue())) {
      results.add(TemplateParameterSuggestion.deleteParam(contents, param, true));
      return Optional.of(results);
    }

    // Handle non-breaking white space in the parameter name
    if (name.contains("\u00A0")) {
      String cleanedName = name.replaceAll("\u00A0", " ").trim();
      boolean knownName = false;
      if (knownParams.contains(cleanedName)) {
        knownName = true;
      } else {
        int lastNonDigit = ContentsUtil.moveIndexBackwardWhileFound(
            cleanedName, cleanedName.length() - 1, "0123456789");
        if ((lastNonDigit >= 0) &&
            (lastNonDigit < cleanedName.length() - 1) &&
            (knownNumericalParameter.contains(cleanedName.substring(0, lastNonDigit + 1)))) {
          knownName = true;
        }
      }
      if (knownName) {
        results.add(TemplateParameterSuggestion.replaceOrDeleteParam(
            contents, template, param,
            cleanedName, param.getValue(), true, true));
      }
    }

    // Handle parameters configured for replacement
    Set<String> replacementParams = paramsToReplaceByName.get(computedName);
    if (replacementParams != null) {
      for (String replacementParam : replacementParams) {
        results.add(TemplateParameterSuggestion.replaceOrDeleteParam(
            contents, template, param,
            replacementParam, param.getValue(),
            replacementParams.size() == 1,
            true));
      }
    }

    // Search in the known parameters if one can be used instead
    boolean safeDelete = StringUtils.isEmpty(param.getValue());
    String missingEqualName = null;
    String missingEqualValue = null;
    Map<Integer, List<String>> similarNamesByDistance = new HashMap<>();
    for (String knownParam : knownParams) {

      // Search for mistyped parameters
      if (StringUtils.equalsIgnoreCase(knownParam, name)) {
        results.add(TemplateParameterSuggestion.replaceOrDeleteParam(
            contents, template, param,
            knownParam, param.getValue(), knownParam.length() >= 4, true));
      } else if (StringUtils.equals(name, computedName)) {
        int distance = levenshteinDistance.apply(computedName, knownParam);
        if ((distance >= 0) && (distance <= 3)) {
          similarNamesByDistance.computeIfAbsent(distance, k -> new ArrayList<>()).add(knownParam);
        } else if (StringUtils.isEmpty(param.getValue())) {
          if (StringUtils.startsWith(name, knownParam)) {
            if ((missingEqualName == null) || (knownParam.length() > missingEqualName.length())) {
              missingEqualName = knownParam;
              missingEqualValue = name.substring(knownParam.length()).trim();
            }
          }
        }
      }

      // Search for missing "=" sign
      if (!StringUtils.equals(name, computedName) &&
          StringUtils.startsWith(param.getValue(), knownParam)) {
        if ((missingEqualName == null) || (knownParam.length() > missingEqualName.length())) {
          missingEqualName = knownParam;
          missingEqualValue = param.getValue().substring(knownParam.length()).trim();
        }
      }

      // Handle strange cases with mixed parameter name and value
      if (StringUtils.isEmpty(param.getValue()) && (knownParam.length() < computedName.length())) {
        int index = 0;
        while ((index < knownParam.length()) && (knownParam.charAt(index) == computedName.charAt(index))) {
          index++;
        }
        if (StringUtils.endsWith(computedName, knownParam.substring(index))) {
          safeDelete = false;
        }
      }
    }

    // Handle similar names
    Comparator<String> decreasingSizeComparator = (s1, s2) -> Integer.compare(s2.length(), s1.length());
    for (int distance = 1; distance <= 3; distance++) {
      List<String> listKnownParams = similarNamesByDistance.getOrDefault(distance, Collections.emptyList());
      listKnownParams.sort(decreasingSizeComparator);
      boolean preventAutomatic = false;
      for (String knownParam : listKnownParams) {
        boolean automatic =
            !preventAutomatic &&
            (distance <= 2) &&
            (knownParam.length() >= 5) &&
            (listKnownParams.size() < 2);
        if (automatic) {

          // Check how many characters are equal at the beginning
          int minLength = Math.min(computedName.length(), knownParam.length());
          int beginEquals = 0;
          while ((beginEquals < minLength) && CharacterUtils.equalsIgnoreCase(
              computedName.charAt(beginEquals),
              knownParam.charAt(beginEquals))) {
            beginEquals++;
          }

          // Check how many characters are equal at the end
          int endEquals = 0;
          while ((endEquals < minLength) && CharacterUtils.equalsIgnoreCase(
              computedName.charAt(computedName.length() - 1 - endEquals),
              knownParam.charAt(knownParam.length() - 1 - endEquals))) {
            endEquals++;
          }

          // Check that no digits are in the difference (risk with numbered parameters)
          for (int index = beginEquals; index < computedName.length() - endEquals; index++) {
            automatic &= !Character.isDigit(computedName.charAt(index));
          }
          for (int index = beginEquals; index < knownParam.length() - endEquals; index++) {
            automatic &= !Character.isDigit(knownParam.charAt(index));
          }

          // Check that consecutive characters are inverted for distance of 2
          if (automatic && (distance == 2)) {
            automatic = false;
            if (computedName.length() == beginEquals + 2 + endEquals) {
              if (knownParam.length() == beginEquals + 2 + endEquals) {
                if ((computedName.charAt(beginEquals) == knownParam.charAt(beginEquals + 1)) &&
                    (computedName.charAt(beginEquals + 1) == knownParam.charAt(beginEquals))) {
                  automatic = true;
                }
              }
            }
          }
        }
        results.add(TemplateParameterSuggestion.replaceOrDeleteParam(
            contents, template, param,
            knownParam, param.getValue(),
            automatic, distance <= 2));
      }
      if (!listKnownParams.isEmpty()) {
        preventAutomatic = true;
      }
    }

    // Handle missing "=" sign
    if ((missingEqualName != null) && (missingEqualValue != null)) {
      boolean automatic =
          (missingEqualName.length() >= 5) ||
          ((missingEqualName.length() >= 4) && (missingEqualValue.length() >= 4));
      if (StringUtils.isEmpty(param.getValue())) {
        automatic &=
            CharacterUtils.isClassicLetter(missingEqualValue.charAt(0)) ||
            Character.isDigit(missingEqualValue.charAt(0));
      } else if (missingEqualValue.length() > 1) {
        char firstValue = missingEqualValue.charAt(0);
        char lastName = missingEqualName.charAt(missingEqualName.length() - 1);
        automatic &= (!CharacterUtils.isClassicLetter(firstValue) || !CharacterUtils.isClassicLetter(lastName));
      }
      automatic &=
          (missingEqualValue.length() >= 1) &&
          !CharacterUtils.isPunctuation(missingEqualValue.charAt(0));
      results.add(TemplateParameterSuggestion.replaceOrDeleteParam(
          contents, template, param,
          missingEqualName, missingEqualValue, automatic, true));
    }

    // General suggestions (deletion and comment)
    safeDelete &= results.isEmpty();
    safeDelete |= paramsToDelete.contains(computedName);
    results.add(TemplateParameterSuggestion.deleteParam(contents, param, safeDelete));
    results.add(TemplateParameterSuggestion.commentParam(analysis, param, paramsToComment.contains(computedName)));
    return Optional.of(results);
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
    if ((rawConfiguration == null) || (rawConfiguration.length < 1)) {
      return;
    }
    for (String templateName : configurationGroup.getTemplateNames(rawConfiguration[0])) {
      TemplateConfiguration templateConfig = configuration.computeIfAbsent(
          templateName,
          k -> new TemplateConfiguration(templateName));
      for (int paramNum = 1; paramNum < rawConfiguration.length; paramNum++) {
        String paramName = rawConfiguration[paramNum].trim();
        if (StringUtils.endsWith(paramName, "1+")) {
          paramName = StringUtils.left(paramName, paramName.length() - 2);
          if (templateConfig.knownNumericalParameter.contains(paramName)) {
            log.warn("Numeric parameter {} already defined for template {}", paramName, templateName);
          }
          templateConfig.knownNumericalParameter.add(paramName);
        } else {
          if (templateConfig.knownParams.contains(paramName)) {
            log.warn("Parameter {} already defined for template {}", paramName, templateName);
          }
          templateConfig.knownParams.add(paramName);
        }
      }
    }
  }

  /**
   * Add parameters that can be safely deleted from the full raw configuration.
   * 
   * @param rawConfiguration Raw configuration for parameters that can be safely deleted.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  public static void addParametersToDelete(
      @Nullable List<String[]> rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] line : rawConfiguration) {
      addParametersToDelete(line, configuration, configurationGroup);
    }
  }

  /**
   * Add parameters that can be safely deleted from one line of the raw configuration.
   * 
   * @param rawConfiguration Line of the raw configuration for parameters that can be safely deleted.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  private static void addParametersToDelete(
      @Nullable String[] rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if ((rawConfiguration == null) || (rawConfiguration.length < 1)) {
      return;
    }
    for (String templateName : configurationGroup.getTemplateNames(rawConfiguration[0])) {
      TemplateConfiguration templateConfig = configuration.computeIfAbsent(
          templateName,
          k -> new TemplateConfiguration(templateName));
      for (int paramNum = 1; paramNum < rawConfiguration.length; paramNum++) {
        String paramName = rawConfiguration[paramNum].trim();
        if (templateConfig.paramsToDelete.contains(paramName)) {
          log.warn("Parameter {} already marked as deletable for template {}", paramName, templateName);
        }
        templateConfig.paramsToDelete.add(paramName);
      }
    }
  }

  /**
   * Add parameters that can be safely commented from the full raw configuration.
   * 
   * @param rawConfiguration Raw configuration for parameters that can be safely commented.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  public static void addParametersToComment(
      @Nullable List<String[]> rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] line : rawConfiguration) {
      addParametersToComment(line, configuration, configurationGroup);
    }
  }

  /**
   * Add parameters that can be safely commented from one line of the raw configuration.
   * 
   * @param rawConfiguration Line of the raw configuration for parameters that can be safely commented.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  private static void addParametersToComment(
      @Nullable String[] rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if ((rawConfiguration == null) || (rawConfiguration.length < 1)) {
      return;
    }
    for (String templateName : configurationGroup.getTemplateNames(rawConfiguration[0])) {
      TemplateConfiguration templateConfig = configuration.computeIfAbsent(
          templateName,
          k -> new TemplateConfiguration(templateName));
      for (int paramNum = 1; paramNum < rawConfiguration.length; paramNum++) {
        String paramName = rawConfiguration[paramNum].trim();
        if (templateConfig.paramsToComment.contains(paramName)) {
          log.warn("Parameter {} already marked as commentable for template {}", paramName, templateName);
        }
        templateConfig.paramsToComment.add(paramName);
      }
    }
  }

  /**
   * Add parameters that can be safely replaced from the full raw configuration.
   * 
   * @param rawConfiguration Raw configuration for parameters that can be safely replaced.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  public static void addParametersToReplace(
      @Nullable List<String[]> rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] line : rawConfiguration) {
      addParametersToReplace(line, configuration, configurationGroup);
    }
  }

  /**
   * Add parameters that can be safely replaced from one line of the raw configuration.
   * 
   * @param rawConfiguration Line of the raw configuration for parameters that can be safely replaced.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  private static void addParametersToReplace(
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
      String oldParamName = rawConfiguration[1].trim();
      Set<String> newParamNames = templateConfig.paramsToReplaceByName.computeIfAbsent(oldParamName, k -> new HashSet<>());
      for (int paramNum = 2; paramNum < rawConfiguration.length; paramNum++) {
        newParamNames.add(rawConfiguration[paramNum].trim());
      }
    }
  }
}
