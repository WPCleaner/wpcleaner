/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a58x.a580;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;
import java.util.Set;
import java.util.function.Predicate;

import javax.annotation.Nonnull;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 580 of check wikipedia project.
 * <br>
 * Error 580: Redundant templates.
 */
public class CheckErrorAlgorithm580 extends CheckErrorAlgorithmBase {

  @Nonnull private static final Logger log = LoggerFactory.getLogger(CheckErrorAlgorithm580.class);

  public CheckErrorAlgorithm580() {
    super("Redundant templates");
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
    if (analysis.isInNamespace(Namespace.TEMPLATE)) {
      return false;
    }

    // Check templates
    List<PageElementTemplate> templates = analysis.getTemplates();
    if ((templates == null) || (templates.isEmpty())) {
      return false;
    }

    // Check each group of redundant templates
    boolean result = false;
    for (Set<String> redundantTemplates : templateNames) {
      result |= analyzeRedundantTemplates(
          analysis, errors, templates,
          template -> redundantTemplates.contains(template.getTemplateName()));
    }
    for (Set<String> redundantTemplates : unnamedParametersTemplateNames) {
      result |= analyzeRedundantTemplates(
          analysis, errors, templates,
          template -> isUnnamedParametersTemplate(redundantTemplates, template));
    }

    return result;
  }

  private boolean isUnnamedParametersTemplate(Set<String> redundantTemplates, PageElementTemplate template) {
    if (!redundantTemplates.contains(template.getTemplateName())) {
      return false;
    }
    for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {
      if (!"".equals(template.getParameter(paramNum).getName())) {
        return false;
      }
    }
    return true;
  }

  /**
   * Analyze templates to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param templates List of templates in the page.
   * @param filter Filter to use on the list of templates. 
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeRedundantTemplates(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      List<PageElementTemplate> templates,
      Predicate<PageElementTemplate> filter) {

    // Filter templates
    List<PageElementTemplate> filteredTemplates = templates.stream()
        .filter(filter)
        .collect(Collectors.toList());
    if (filteredTemplates.size() <= 1) {
      return false;
    }

    // Report errors
    if (errors == null) {
      return true;
    }
    reportTemplates(analysis, errors, filteredTemplates);
    return true;
  }

  private void reportTemplates(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      List<PageElementTemplate> templates) {
    if (templates.size() <= 1) {
      return;
    }

    // Check if the first two templates are just one after each other
    Set<PageElementTemplate> reportedTemplates = new HashSet<>();
    PageElementTemplate firstTemplate = templates.get(0);
    PageElementTemplate secondTemplate = templates.get(1);
    String contents = analysis.getContents();
    int tmpIndex = ContentsUtil.moveIndexForwardWhileFound(contents, firstTemplate.getEndIndex(), " \n");
    if (tmpIndex == secondTemplate.getBeginIndex()) {
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis, firstTemplate.getBeginIndex(), secondTemplate.getEndIndex());
      boolean canDeleteSecondTemplate = areDuplicates(firstTemplate, secondTemplate, contents);
      if (canDeleteSecondTemplate) {
        errorResult.addReplacement(
            contents.substring(firstTemplate.getBeginIndex(), firstTemplate.getEndIndex()),
            true);
      }
      errors.add(errorResult);
      reportedTemplates.add(firstTemplate);
      reportedTemplates.add(secondTemplate);
    }

    // Report first template as normal
    if (!reportedTemplates.contains(firstTemplate)) {
      errors.add(createCheckErrorResult(
          analysis,
          firstTemplate.getBeginIndex(), firstTemplate.getEndIndex(),
          ErrorLevel.CORRECT));
      reportedTemplates.add(firstTemplate);
    }

    // Report remaining templates
    for (PageElementTemplate template : templates) {
      if (!reportedTemplates.contains(template)) {
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, template.getBeginIndex(), template.getEndIndex());
        if (areDuplicates(firstTemplate, template, contents) &&
            keepFirstDuplicate.contains(firstTemplate.getTemplateName()) &&
            keepFirstDuplicate.contains(secondTemplate.getTemplateName())) {
          errorResult.addReplacement("", true);
        }
        errors.add(errorResult);
      }
    }
  }

  private boolean areDuplicates(
      PageElementTemplate firstTemplate,
      PageElementTemplate secondTemplate,
      String contents) {
    if ((firstTemplate.getParameterCount() == 0) && (secondTemplate.getParameterCount() == 0)) {
      return true;
    }
    if ((firstTemplate.getParameterCount() > 0) && (secondTemplate.getParameterCount() > 0)) {
      String firstValue = contents.substring(firstTemplate.getParameterPipeIndex(0), firstTemplate.getEndIndex());
      String secondValue = contents.substring(secondTemplate.getParameterPipeIndex(0), secondTemplate.getEndIndex());
      if (firstValue.equals(secondValue)) {
        return true;
      }
    }
    return false;
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

  /** Templates that are redundant */
  private static final String PARAMETER_TEMPLATES = "templates";

  /** Templates without named parameters that are redundant */
  private static final String PARAMETER_UNNAMED_TEMPLATES = "templates_unnamed_parameters";

  /** Templates for which we should keep the first duplicate */
  private static final String PARAMETER_KEEP_FIRST_DUPLICATE = "keep_first_duplicate";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_TEMPLATES, true, true, false);
    templateNames.clear();
    if (tmp != null) {
      WPCConfiguration.convertPropertyToStringArrayList(tmp).forEach(elements ->
          templateNames.add(Arrays.stream(elements).map(Page::normalizeTitle).collect(Collectors.toSet())));
    }

    tmp = getSpecificProperty(PARAMETER_UNNAMED_TEMPLATES, true, true, false);
    unnamedParametersTemplateNames.clear();
    if (tmp != null) {
      WPCConfiguration.convertPropertyToStringArrayList(tmp).forEach(elements ->
          unnamedParametersTemplateNames.add(Arrays.stream(elements).map(Page::normalizeTitle).collect(Collectors.toSet())));
    }

    tmp = getSpecificProperty(PARAMETER_KEEP_FIRST_DUPLICATE, true, true, false);
    keepFirstDuplicate.clear();
    if (tmp != null) {
      WPCConfiguration.convertPropertyToStringArrayList(tmp).stream()
          .flatMap(Arrays::stream)
          .map(Page::normalizeTitle)
          .forEach(keepFirstDuplicate::add);
    }
  }

  /** Templates that are redundant */
  private final List<Set<String>> templateNames = new ArrayList<>();

  /** Templates without named parameters that are redundant */
  private final List<Set<String>> unnamedParametersTemplateNames = new ArrayList<>();

  /** Templates for which we should keep only the first duplicate */
  private final Set<String> keepFirstDuplicate = new HashSet<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATES,
        GT._T("Redundant templates"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement("template", GT._T("Template name"), false, true)
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_UNNAMED_TEMPLATES,
        GT._T("Redundant templates when used with only unnamed parameters"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement("template", GT._T("Template name"), false, true)
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_KEEP_FIRST_DUPLICATE,
        GT._T("Templates for which we can keep only the first duplicate"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement("template", GT._T("Template name"), false, true)
        },
        true));
  }
}
