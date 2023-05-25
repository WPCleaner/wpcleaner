/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a564;

import java.text.Normalizer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.regex.Pattern;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.text.similarity.LevenshteinDistance;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.check.algorithm.a5xx.TemplateConfigurationGroup;
import org.wikipediacleaner.api.check.algorithm.a5xx.TemplateParameterSuggestion;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.comment.ContentsComment;
import org.wikipediacleaner.utils.string.CharacterUtils;

/**
 * Bean for handling configuration for templates.
 */
class TemplateConfiguration {

  @Nonnull private static final Logger log = LoggerFactory.getLogger(TemplateConfiguration.class);
  
  private static final Pattern CLEAN_WHITESPACE = Pattern.compile("\\s{2,}");

  @Nonnull private final String templateName;

  @Nonnull private final Set<String> knownParams;
  @Nonnull private final List<String> orderedKnownParams;

  @Nonnull private final Set<String> knownNumericalParameter;
  @Nonnull private final List<String> orderedKnownNumericalParameter;

  @Nonnull private final Set<String> paramsToDelete;

  @Nonnull private final Set<String> unnamedParamsToDelete;

  @Nonnull private final Map<String, Set<String>> valuesToDeleteByParam;

  @Nonnull private final Set<String> paramsToComment;

  @Nonnull private final Map<String, Set<String>> paramsToReplaceByName;

  @Nonnull private final Map<String, Map<String, Pair<String, String>>> paramsToReplaceByValue;

  @Nonnull private final List<String> paramsOk;

  @Nonnull private final LevenshteinDistance levenshteinDistance;

  private TemplateConfiguration(@Nonnull String templateName) {
    this.templateName = templateName;
    this.knownParams = new HashSet<>();
    this.orderedKnownParams = new ArrayList<>();
    this.knownNumericalParameter = new HashSet<>();
    this.orderedKnownNumericalParameter = new ArrayList<>();
    this.paramsToDelete = new HashSet<>();
    this.unnamedParamsToDelete = new HashSet<>();
    this.valuesToDeleteByParam = new HashMap<>();
    this.paramsToComment = new HashSet<>();
    this.paramsToReplaceByName = new HashMap<>();
    this.paramsToReplaceByValue = new HashMap<>();
    this.paramsOk = new ArrayList<>();
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

    // Handle extra whitespace characters
    String contents = analysis.getContents();
    computedName = CLEAN_WHITESPACE.matcher(computedName).replaceAll(" ");
    if (knownParams.contains(computedName)) {
      return Optional.of(Collections.singletonList(
          TemplateParameterSuggestion.replaceOrDeleteParam(
              contents, template, param, computedName, param.getValue(), true, true)));
    }

    // Handle stranded "=" sign
    String name = param.getName();
    if (StringUtils.isEmpty(computedName) && StringUtils.isEmpty(param.getValue())) {
      return Optional.of(Collections.singletonList(
          TemplateParameterSuggestion.deleteParam(contents, param, true)));
    }

    // Handle unnamed parameters
    if (StringUtils.isEmpty(name) && !StringUtils.isEmpty(computedName)) {

      // Handle unnamed parameters that can be deleted
      for (String namedParameter : unnamedParamsToDelete) {
        String otherValue = template.getParameterValue(namedParameter);
        if (otherValue != null) {
          if (otherValue.equalsIgnoreCase(param.getValue()) ||
              otherValue.equalsIgnoreCase("[[" + param.getValue() + "]]")) {
            return Optional.of(Collections.singletonList(
                TemplateParameterSuggestion.deleteParam(contents, param, true)));
          }
        }
      }

      // Handle unnamed parameters that are in fact parameter names
      for (String namedParameter : knownParams) {
        if (namedParameter.equals(param.getValue())) {
          boolean shouldRemove = true;
          if (paramNum + 1 < template.getParameterCount()) {
            PageElementTemplate.Parameter nextParam = template.getParameter(paramNum + 1);
            if (StringUtils.isEmpty(nextParam.getName())) {
              shouldRemove = false;
            }
          }
          if (!shouldRemove && (param.getValue() != null)) {
            for (int paramNumTmp = 0; paramNumTmp < template.getParameterCount(); paramNumTmp++) {
              if (param.getValue().equalsIgnoreCase(template.getParameterName(paramNumTmp))) {
                shouldRemove = true;
              }
            }
          }
          if (shouldRemove) {
            return Optional.of(Collections.singletonList(
                TemplateParameterSuggestion.deleteParam(contents, param, true)));
          }
        }
      }

      // Handle unnamed parameters for which their value also exist in another parameter
      /*for (int paramTmp = 0; paramTmp < template.getParameterCount(); paramTmp++) {
        PageElementTemplate.Parameter otherParam = template.getParameter(paramTmp);
        if (StringUtils.isNotEmpty(otherParam.getName()) &&
            (otherParam.getValue() != null)) {
          if (otherParam.getValue().equalsIgnoreCase(param.getValue()) ||
              otherParam.getValue().equalsIgnoreCase("[[" + param.getValue() + "]]")) {
            System.err.println("Value " + param.getValue() + " exists also in parameter named " + otherParam.getName());
          }
        }
      }*/
    }

    // Handle parameters that can be deleted by configuration
    if (paramsToDelete.contains(computedName)) {
      return Optional.of(Collections.singletonList(
          TemplateParameterSuggestion.deleteParam(contents, param, true)));
    }
    if (valuesToDeleteByParam.containsKey(computedName)) {
      Set<String> valuesToDelete = valuesToDeleteByParam.get(computedName);
      boolean delete = valuesToDelete.contains(param.getValue());
      delete |= valuesToDelete.contains(param.getStrippedValue()) &&
          (StringUtils.isNotBlank(param.getName()) || StringUtils.isNotBlank(param.getStrippedValue()));
      if (delete) {
        return Optional.of(Collections.singletonList(
            TemplateParameterSuggestion.deleteParam(contents, param, true)));
      }
    }

    // Handle non-breaking white space in the parameter name
    List<TemplateParameterSuggestion> results = new ArrayList<>();
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

    // Handle parameters configured for replacement by value
    Map<String, Pair<String, String>> replaceByValue = paramsToReplaceByValue.getOrDefault(computedName, Collections.emptyMap());
    Pair<String, String> replacementValue = replaceByValue.get(param.getValue());
    if ((replacementValue == null) &&
        (param.getValue() != null) &&
        (param.getValue().contains(ContentsComment.START)) &&
        StringUtils.isNotBlank(param.getName()) &&
        StringUtils.isNotBlank(param.getStrippedValue())) {
      replacementValue = replaceByValue.get(param.getStrippedValue());
    }
    if (replacementValue != null) {
      results.add(TemplateParameterSuggestion.replaceOrDeleteParam(
          contents, template, param,
          replacementValue.getLeft(), replacementValue.getRight(),
          true, true));
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
    } else if ((name.isEmpty()) && (param.getValue() != null)) {
      String value = param.getValue();
      for (Entry<String, Set<String>> potentialReplacements : paramsToReplaceByName.entrySet()) {
        String initialParameter = potentialReplacements.getKey();
        if (value.startsWith(initialParameter)) {
          for (String replacementParam : potentialReplacements.getValue()) {
            String newValue = value.substring(initialParameter.length());
            while (newValue.startsWith(" ")) {
              newValue = newValue.substring(1);
            }
            boolean automaticReplacement =
                (potentialReplacements.getValue().size() == 1) &&
                (!value.startsWith(replacementParam));
            results.add(TemplateParameterSuggestion.replaceOrDeleteParam(
                contents, template, param,
                replacementParam, newValue,
                automaticReplacement, true));
          }
        }
      }
    }

    // Handle numerical parameters
    boolean numericalParameter = false;
    if (StringUtils.isNotEmpty(name)) {
      if (CharacterUtils.isClassicDigit(name.charAt(name.length() - 1))) {
        numericalParameter = true;
      }
    }
    String strippedName = name;
    String strippedComputedName = computedName;
    String numericComputedName = StringUtils.EMPTY;
    List<String> correctKnownParams = orderedKnownParams;
    int completionDigitsCount = 0;
    if (numericalParameter) {
      int beginNumeric = computedName.length();
      while ((beginNumeric > 0) &&
             CharacterUtils.isClassicDigit(computedName.charAt(beginNumeric - 1))) {
        beginNumeric--;
      }
      strippedComputedName = computedName.substring(0, beginNumeric);
      numericComputedName = computedName.substring(beginNumeric);
      correctKnownParams = orderedKnownNumericalParameter;
      if (StringUtils.isNotEmpty(name)) {
        beginNumeric = name.length();
        while ((beginNumeric > 0) &&
               CharacterUtils.isClassicDigit(name.charAt(beginNumeric - 1))) {
          beginNumeric--;
        }
        strippedName = name.substring(0, beginNumeric);
        completionDigitsCount = name.length() - beginNumeric;
      }
    }

    // Search in the known parameters if one can be used instead
    boolean safeDelete = StringUtils.isEmpty(param.getValue()) && completionDigitsCount == 0;
    Map<String, String> missingEqualByName = new TreeMap<>(Comparator.comparingInt(String::length).thenComparing(Function.identity()).reversed());
    Map<Integer, List<String>> similarNamesByDistance = new HashMap<>();
    for (String knownParam : correctKnownParams) {
      String completedKnownParam = knownParam + (StringUtils.isNotEmpty(name) ? numericComputedName : "");

      // Search for mistyped parameters
      if (compareWithoutAccents(knownParam, strippedName)) {
        boolean automaticReplacement =
            (knownParam.length() >= 4) ||
            compareWithoutAccents(knownParam, name) ||
            StringUtils.equalsIgnoreCase(knownParam, strippedName); 
        results.add(TemplateParameterSuggestion.replaceOrDeleteParam(
            contents, template, param,
            completedKnownParam, param.getValue(),
            automaticReplacement, true));
      } else if (StringUtils.equals(name, computedName)) {
        int distance = levenshteinDistance.apply(strippedComputedName, knownParam);
        if ((distance >= 0) && (distance <= 3)) {
          similarNamesByDistance.computeIfAbsent(distance, k -> new ArrayList<>()).add(completedKnownParam);
        } else if (StringUtils.isEmpty(param.getValue())) {
          if (StringUtils.startsWith(name, completedKnownParam)) {
            missingEqualByName.put(completedKnownParam, name.substring(completedKnownParam.length()).trim());
          }
        }
      }

      // Search for missing "=" sign
      if (!StringUtils.equals(name, computedName) &&
          StringUtils.startsWith(param.getValue(), completedKnownParam)) {
        String value = param.getValue().substring(completedKnownParam.length()).trim();
        if (value.startsWith(":") || value.startsWith(")")) {
          value = value.substring(1);
        }
        missingEqualByName.put(completedKnownParam, value);
      }

      // Handle strange cases with mixed parameter name and value
      if (StringUtils.isEmpty(param.getValue()) &&
          (knownParam.length() < computedName.length()) &&
          StringUtils.isEmpty(numericComputedName)) {
        int index = 0;
        while ((index < knownParam.length()) && (knownParam.charAt(index) == computedName.charAt(index))) {
          index++;
        }
        if (StringUtils.endsWith(computedName, knownParam.substring(index))) {
          safeDelete = false;
        }
      }
    }

    // Handle various suggestions
    handleSimilarNames(
        results, similarNamesByDistance, param, template, contents,
        completionDigitsCount > 0 && StringUtils.isEmpty(param.getValue()));
    handleMissingEqual(results, missingEqualByName, param, template, contents);
    safeDelete &= results.isEmpty();
    results.add(TemplateParameterSuggestion.deleteParam(contents, param, safeDelete));
    results.add(TemplateParameterSuggestion.commentParam(analysis, param, paramsToComment.contains(computedName)));
    return Optional.of(results);
  }

  private static final Pattern REMOVE_ACCENTS = Pattern.compile("\\p{M}");

  private boolean compareWithoutAccents(final String str1, final String str2) {
    final String modifiedStr1 = REMOVE_ACCENTS.matcher(Normalizer.normalize(str1, Normalizer.Form.NFKD)).replaceAll("");
    final String modifiedStr2 = REMOVE_ACCENTS.matcher(Normalizer.normalize(str2, Normalizer.Form.NFKD)).replaceAll("");
    return StringUtils.equalsIgnoreCase(modifiedStr1, modifiedStr2);
  }

  /**
   * Handle missing equal sign.
   * 
   * @param results List of results to complete.
   * @param missingEqualByName Possible missing equal sign found.
   * @param param Template parameter.
   * @param template Template.
   * @param contents Page contents.
   */
  private void handleMissingEqual(
      List<TemplateParameterSuggestion> results,
      Map<String, String> missingEqualByName,
      PageElementTemplate.Parameter param,
      PageElementTemplate template,
      String contents) {
    boolean missingEqualFound = false;
    for (Map.Entry<String, String> missingEqual : missingEqualByName.entrySet()) {
      String missingEqualName = missingEqual.getKey();
      String missingEqualValue = missingEqual.getValue();
      boolean automatic =
          (missingEqualName.length() >= 5) ||
          ((missingEqualName.length() >= 4) && (missingEqualValue.length() >= 4));
      automatic &= (missingEqualValue.length() == 0) || !Character.isDigit(missingEqualValue.charAt(0));
      automatic |= paramsOk.contains(missingEqualName);
      automatic &= !missingEqualFound;
      for (int nbChars = 1; nbChars < missingEqualValue.length(); nbChars++) {
        automatic &= !knownParams.contains(missingEqualName + missingEqualValue.substring(0, nbChars));
      }
      missingEqualFound = true;
      if (StringUtils.isEmpty(param.getValue())) {
        automatic &=
            CharacterUtils.isClassicLetter(missingEqualValue.charAt(0)) ||
            Character.isDigit(missingEqualValue.charAt(0));
      } else if (missingEqualValue.length() > 1) {
        boolean separator = false;
        separator |= !CharacterUtils.isClassicLetter(missingEqualValue.charAt(0));
        separator |= !CharacterUtils.isClassicLetter(missingEqualName.charAt(missingEqualName.length() - 1));
        separator |=
            param.getValue().startsWith(missingEqualName) &&
            param.getValue().endsWith(missingEqualValue) &&
            (param.getValue().length() > missingEqualName.length() + missingEqualValue.length()) &&
            !CharacterUtils.isClassicLetter(param.getValue().charAt(missingEqualName.length()));
        automatic &= separator;
      }
      automatic &=
          (missingEqualValue.length() >= 1) &&
          !CharacterUtils.isPunctuation(missingEqualValue.charAt(0));
      results.add(TemplateParameterSuggestion.replaceOrDeleteParam(
          contents, template, param,
          missingEqualName, missingEqualValue, automatic, true));
    }
  }

  /**
   * Handle similar name.
   * 
   * @param results List of results to complete.
   * @param similarNamesByDistance Similar names found, organized by their distance.
   * @param param Template parameter.
   * @param template Template.
   * @param contents Page contents.
   */
  private void handleSimilarNames(
      List<TemplateParameterSuggestion> results,
      Map<Integer, List<String>> similarNamesByDistance,
      PageElementTemplate.Parameter param,
      PageElementTemplate template,
      String contents,
      boolean preventAutomatic) {
    Comparator<String> decreasingSizeComparator = (s1, s2) -> Integer.compare(s2.length(), s1.length());
    String computedName = param.getComputedName();
    for (int distance = 1; distance <= 3; distance++) {
      List<String> listKnownParams = similarNamesByDistance.getOrDefault(distance, Collections.emptyList());
      listKnownParams.sort(decreasingSizeComparator);
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
          templateConfig.orderedKnownNumericalParameter.add(paramName);
        } else {
          if (templateConfig.knownParams.contains(paramName)) {
            log.warn("Parameter {} already defined for template {}", paramName, templateName);
          }
          templateConfig.knownParams.add(paramName);
          templateConfig.orderedKnownParams.add(paramName);
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
   * Add parameters for which unnamed parameters can be safely deleted when values are equal from the full raw configuration.
   * 
   * @param rawConfiguration Raw configuration for parameters for which unnamed parameters can be safely deleted.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  public static void addUnnamedParametersToDelete(
      @Nullable List<String[]> rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] line : rawConfiguration) {
      addUnnamedParametersToDelete(line, configuration, configurationGroup);
    }
  }

  /**
   * Add parameters for which unnamed parameters can be safely deleted when values are equal from one line of the raw configuration.
   * 
   * @param rawConfiguration Line of the raw configuration for parameters for which unnamed parameters can be safely deleted.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  private static void addUnnamedParametersToDelete(
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
        if (templateConfig.unnamedParamsToDelete.contains(paramName)) {
          log.warn("Unnamed parameter already marked as deletable when parameter {} as the same value for template {}", paramName, templateName);
        }
        templateConfig.unnamedParamsToDelete.add(paramName);
      }
    }
  }

  /**
   * Add values that can be safely deleted from the full raw configuration.
   * 
   * @param rawConfiguration Raw configuration for values that can be safely deleted.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  public static void addValuesToDelete(
      @Nullable List<String[]> rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] line : rawConfiguration) {
      addValuesToDelete(line, configuration, configurationGroup);
    }
  }

  /**
   * Add values that can be safely deleted from one line of the raw configuration.
   * 
   * @param rawConfiguration Line of the raw configuration for values that can be safely deleted.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  private static void addValuesToDelete(
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
      String value = rawConfiguration[1].trim();
      for (int paramNum = 2; paramNum < rawConfiguration.length; paramNum++) {
        String paramName = rawConfiguration[paramNum].trim();
        Set<String> values = templateConfig.valuesToDeleteByParam.computeIfAbsent(paramName, k -> new HashSet<>());
        if (values.contains(value)) {
          log.warn("Value {} already marked as deletable for template {} and parameter {}", value, templateName, paramName);
        }
        values.add(value);
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
    if ((rawConfiguration == null) || (rawConfiguration.length < 5)) {
      return;
    }
    for (String templateName : configurationGroup.getTemplateNames(rawConfiguration[0])) {
      TemplateConfiguration templateConfig = configuration.computeIfAbsent(
          templateName,
          k -> new TemplateConfiguration(templateName));
      String oldParamName = rawConfiguration[1].trim();
      String oldParamValue = rawConfiguration[2].trim();
      String newParamName = rawConfiguration[3].trim();
      String newParamValue = rawConfiguration[4].trim();
      templateConfig.paramsToReplaceByValue.computeIfAbsent(
          oldParamName,
          k -> new HashMap<>()).put(oldParamValue, new ImmutablePair<>(newParamName, newParamValue));
    }
  }

  /**
   * Add parameters that can be safely used from the full raw configuration.
   * 
   * @param rawConfiguration Raw configuration for parameters that can be safely used.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  public static void addParametersOk(
      @Nullable List<String[]> rawConfiguration,
      @Nonnull Map<String, TemplateConfiguration> configuration,
      @Nonnull TemplateConfigurationGroup configurationGroup) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] line : rawConfiguration) {
      addParametersOk(line, configuration, configurationGroup);
    }
  }

  /**
   * Add parameters that can be safely used from one line of the raw configuration.
   * 
   * @param rawConfiguration Line of the raw configuration for parameters that can be safely used.
   * @param configuration Configuration.
   * @param configurationGroup Configuration of groups of templates.
   */
  private static void addParametersOk(
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
        templateConfig.paramsOk.add(rawConfiguration[paramNum].trim());
      }
    }
  }
}
