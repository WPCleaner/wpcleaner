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
import java.util.List;
import java.util.stream.Collectors;
import java.util.Set;

import javax.annotation.Nonnull;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
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

    // Check templates
    List<PageElementTemplate> templates = analysis.getTemplates();
    if ((templates == null) || (templates.isEmpty())) {
      return false;
    }

    // Check each group of redundant templates
    boolean result = false;
    for (Set<String> redundantTemplates : templateNames) {
      result |= analyzeRedundantTemplates(analysis, errors, templates, redundantTemplates);
    }

    return result;
  }

  /**
   * Analyze templates to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param templates List of templates in the page.
   * @param redundantTemplates Names of redundant templates. 
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeRedundantTemplates(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      List<PageElementTemplate> templates,
      Set<String> redundantTemplates) {

    // Filter templates
    List<PageElementTemplate> filteredTemplates = templates.stream()
        .filter(template -> redundantTemplates.contains(template.getTemplateName()))
        .collect(Collectors.toList());
    if (filteredTemplates.size() <= 1) {
      return false;
    }
    PageElementTemplate firstTemplate = filteredTemplates.get(0);

    if (errors == null) {
      return true;
    }

    // Report each template
    for (PageElementTemplate template : filteredTemplates) {
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis, template.getBeginIndex(), template.getEndIndex(),
          template == firstTemplate ? ErrorLevel.CORRECT : ErrorLevel.ERROR);
      errors.add(errorResult);
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

  /** Templates that are redundant */
  private static final String PARAMETER_TEMPLATES = "templates";

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
  }

  /** Templates that are redundant */
  private final List<Set<String>> templateNames = new ArrayList<>();

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
  }
}
