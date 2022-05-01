/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a57x.a576;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 576 of check wikipedia project.
 * <br>
 * Error 576: Bogus image options (see [[Special:LintErrors/bogus-image-options]]) through a template.
 */
public class CheckErrorAlgorithm576 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm576() {
    super("Bogus image options");
  }

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
    boolean result = false;
    for (Entry<String, Map<String, String>> templateConfiguration : configurationByTemplate.entrySet()) {
      List<PageElementTemplate> templates = analysis.getTemplates(templateConfiguration.getKey());
      for (PageElementTemplate template : templates) {
        result |= analyzeTemplate(analysis, errors, template, templateConfiguration.getValue());
      }
    }

    return result;
  }

  /**
   * Analyze a template to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param template Template.
   * @param templateConfiguration Template configuration.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTemplate(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTemplate template,
      Map<String, String> templateConfiguration) {

    boolean result = false;
    for (Entry<String, String> paramConfiguration : templateConfiguration.entrySet()) {
      int paramIndex = template.getParameterIndex(paramConfiguration.getKey());
      if (paramIndex >= 0) {
        Parameter param = template.getParameter(paramIndex);
        switch (paramConfiguration.getValue()) {
        case "upright":
          result |= analyzeUprightParameter(analysis, errors, param);
          break;
        }
      }
    }
    return result;
  }

  private static final Pattern UPRIGHT_PATTERN = Pattern.compile("(?:\\d++(?:\\.\\d++)?|\\.\\d++)");
  private static final Pattern UPRIGHT_SURE_PATTERN_= Pattern.compile("(?:\\d(?:\\.\\d++)?|\\.\\d++)");
  private static final List<Pattern> UPRIGHT_DELETE_PATTERN = Stream
      .of(
          "\\d\\d++ *+px",                           // 12 px
          "\\d++ *+x *+\\d++",                       // 1x1
          "\\d++ *+x *+\\d++ *+px",                  // 1x1 px
          "\\d++\\.\\d++ *+x *+\\d++\\.\\d++ *+px")  // 1.1x1.1 px
      .map(Pattern::compile)
      .collect(Collectors.toList());
  private static final Pattern UPRIGHT_ZERO_PATTERN = Pattern.compile("(?:0++(?:\\.0++)?|\\.0++)");

  /**
   * Analyze a template parameter to check if errors are present for upright image attribute.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param param Template parameter.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeUprightParameter(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      Parameter param) {

    // Check the parameter value
    String value = param.getValue();
    if (value.isEmpty()) {
      return false;
    }
    if (!UPRIGHT_ZERO_PATTERN.matcher(value).matches() && UPRIGHT_PATTERN.matcher(value).matches()) {
      return false;
    }

    // Report error
    if (errors == null) {
      return true;
    }
    int beginIndex = param.getBeginIndex();
    int endIndex = param.getEndIndex();
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);

    // Check for possible replacement
    String contents = analysis.getContents();
    String newValue = value.replaceAll(",", ".").replaceAll("\"", "").replaceAll(" ", "").trim();
    if (newValue.startsWith("O")) {
      newValue = newValue.replaceFirst("O", "0");
    }
    if (!UPRIGHT_ZERO_PATTERN.matcher(newValue).matches() && UPRIGHT_PATTERN.matcher(newValue).matches()) {
      String replacement =
          contents.substring(beginIndex, param.getValueStartIndex()) +
          newValue +
          contents.substring(param.getValueStartIndex() + value.length(), endIndex);
      errorResult.addReplacement(replacement, true);
    }
    if (newValue.endsWith("px")) {
      newValue = newValue.substring(0, newValue.length() - 2);
      if (!UPRIGHT_ZERO_PATTERN.matcher(newValue).matches() && UPRIGHT_SURE_PATTERN_.matcher(newValue).matches()) {
        String replacement =
            contents.substring(beginIndex, param.getValueStartIndex()) +
            newValue +
            contents.substring(param.getValueStartIndex() + value.length(), endIndex);
        errorResult.addReplacement(replacement, true);
      }
    }

    // Check for possible deletion
    boolean delete = newValue.isEmpty() || UPRIGHT_ZERO_PATTERN.matcher(newValue).matches();
    for (Pattern pattern : UPRIGHT_DELETE_PATTERN) {
      if (pattern.matcher(value).matches()) {
        delete = true;
      }
    }
    errorResult.addReplacement("", delete);

    errors.add(errorResult);
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

  /** Templates and parameters that are checked */
  private static final String PARAMETER_TEMPLATES = "templates";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_TEMPLATES, true, true, false);
    configurationByTemplate.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      for (String[] tmpElement : tmpList) {
        if (tmpElement.length > 2) {
          configurationByTemplate
              .computeIfAbsent(tmpElement[0], k -> new HashMap<>())
              .put(tmpElement[1], tmpElement[2]);
        }
      }
    }
  }

  // Templates and parameters that are checked
  private final Map<String, Map<String, String>> configurationByTemplate = new HashMap<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATES,
        GT._T("Templates for which values should be verified"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template",
                GT._T("Name of the template")),
            new AlgorithmParameterElement(
                "param",
                GT._T("Name of the parameter")),
            new AlgorithmParameterElement(
                "image attribut",
                GT._T("Image attribute"))
        },
        true));
  }
}
