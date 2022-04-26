/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a57x.a577;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.Nonnull;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.BasicActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 577 of check wikipedia project.
 * <br>
 * Error 577: Invalid DOI.
 */
public class CheckErrorAlgorithm577 extends CheckErrorAlgorithmBase {

  @Nonnull private static final Logger log = LoggerFactory.getLogger(CheckErrorAlgorithm577.class);

  public CheckErrorAlgorithm577() {
    super("Invalid DOI");
  }

  private static final List<Pattern> OPTIONAL_PREFIXES = Stream.of(
      "/",
      "DOI: *+",
      "doi: *+",
      "doi.org/",
      "http://doi.org/",
      "http://dx.doi.org/",
      "https://doi.org/",
      "https://dx.doi.org/")
      .map(Pattern::compile).collect(Collectors.toList());

  private static final Set<String> DELETE_VALUES = Stream.of("-").collect(Collectors.toSet());

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  @Override
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }

    // Check each template
    List<PageElementTemplate> templates = analysis.getTemplates();
    if ((templates == null) || (templates.isEmpty())) {
      return false;
    }
    boolean result = false;
    for (PageElementTemplate template : templates) {
      result |= analyzeTemplate(analysis, errors, template);
    }

    return result;
  }

  /**
   * Analyze a template to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param template Template.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTemplate(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTemplate template) {

    // Analyze each parameter
    boolean result = false;
    for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {
      result |= analyzeTemplateParam(analysis, errors, template, template.getParameter(paramNum));
    }
    return result;
  }

  /**
   * Analyze a template to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param template Template.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTemplateParam(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTemplate template,
      Parameter param) {

    // Check if the parameter is for a DOI
    if (!StringUtils.equalsIgnoreCase("doi", param.getName())) {
      if (!StringUtils.equalsIgnoreCase("doi", template.getTemplateName()) ||
          (param.getName() != null)) {
        return false;
      }
    }

    // Check the value of the parameter
    String value = param.getValue();
    if (value.isEmpty()) {
      return false;
    }
    if (isValidDOI(value)) {
      return false;
    }

    // Report error
    if (errors == null) {
      return true;
    }
    int beginIndex = param.getBeginIndex();
    int endIndex = param.getEndIndex();
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);

    // Access DOI page
    errorResult.addPossibleAction(
        GT._T("Check incorrect DOI {0}", value),
        new BasicActionProvider(
            new ActionExternalViewer("https://dx.doi.org/" + value)));

    // Delete incorrect values
    if (DELETE_VALUES.contains(value)) {
      errorResult.addReplacement("", true);
    }

    // Prefixes that can be safely deleted
    for (Pattern prefix : deletePrefixes) {
      Matcher matcher = prefix.matcher(value);
      if (matcher.find() && (matcher.start() == 0)) {
        addReplacement(analysis, errorResult, param, value.substring(matcher.end()).trim(), true);
      }
    }

    // Missing beginning
    if (isValidDOI("1" + value)) {
      addReplacement(analysis, errorResult, param, "1" + value, true);
    } else {
      int slashIndex = value.indexOf('/');
      int dotIndex = value.indexOf('.');
      if ((slashIndex > 0) && ((dotIndex < 0) || (dotIndex > slashIndex))) {
        if ((value.indexOf('/', slashIndex + 1) < 0) && isValidDOI("10." + value)) {
          addReplacement(analysis, errorResult, param, "10." + value, false);
        }
      }
    }

    // Incorrect prefix
    int startIndex = value.indexOf("10.");
    if ((startIndex > 0) && isValidDOI(value.substring(startIndex))) {
      addReplacement(analysis, errorResult, param, value.substring(startIndex), false);
    }

    // URL encoded slash
    if (value.contains("%2F")) {
      String newValue = value.replaceAll("%2F", "/");
      if (isValidDOI(newValue)) {
        addReplacement(analysis, errorResult, param, newValue, true);
      }
    }

    errors.add(errorResult);

    return true;
  }

  /**
   * Add a replacement for a DOI.
   * 
   * @param analysis Page analysis.
   * @param errorResult Error.
   * @param param Template parameter.
   * @param newValue New value for the DOI.
   */
  private void addReplacement(
      PageAnalysis analysis,
      CheckErrorResult errorResult,
      Parameter param,
      String newValue,
      boolean automatic) {
    String contents = analysis.getContents();
    String replacement =
        contents.substring(param.getBeginIndex(), param.getValueStartIndex()) +
        newValue +
        contents.substring(param.getValueStartIndex() + param.getValue().length(), param.getEndIndex());
    errorResult.addReplacement(replacement, automatic && isValidDOI(newValue));
    errorResult.addPossibleAction(
        GT._T("Check DOI {0}", newValue),
        new BasicActionProvider(
            new ActionExternalViewer("https://dx.doi.org/" + newValue)));
  }

  /**
   * Check if a DOI has a correct syntax.
   * 
   * @param value DOI value.
   * @return True if the syntax is correct.
   */
  private boolean isValidDOI(String value) {

    // Check if there's an extra prefix
    int currentIndex = 0;
    boolean prefixFound = false;
    for (Pattern prefix : OPTIONAL_PREFIXES) {
      Matcher matcher = prefix.matcher(value);
      if (!prefixFound && matcher.find(currentIndex) && (matcher.start() == currentIndex)) {
        prefixFound = true;
        currentIndex += matcher.end();
      }
    }

    // Check first part of the prefix "10."
    if ((currentIndex >= value.length()) || !value.startsWith("10.", currentIndex)) {
      return false;
    }
    currentIndex += 3;

    // Find the separating slash between prefix and suffix
    int slashIndex = value.indexOf('/', currentIndex);
    if (slashIndex < 0) {
      return false;
    }
    currentIndex++;

    // Check the suffix
    if (currentIndex >= value.length()) {
      return false;
    }

    return true;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    if (!analysis.getPage().isArticle()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Prefixes that can be deleted */
  private static final String PARAMETER_DELETE_PREFIX = "delete_prefix";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_DELETE_PREFIX, true, true, false);
    deletePrefixes.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      for (String tmpElement : tmpList) {
        try {
          deletePrefixes.add(Pattern.compile(tmpElement));
        } catch (PatternSyntaxException e) {
          log.error("Error parsing delete prefix {}", tmpElement);
        }
      }
    }
  }

  /** Prefixes that can be deleted */
  private final List<Pattern> deletePrefixes = new ArrayList<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_DELETE_PREFIX,
        GT._T("Prefixes that can be safely deleted"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "prefix",
                GT._T("Prefix that can be safely deleted"))
        },
        true));
  }
}
