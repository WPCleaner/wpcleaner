/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a09x.a095;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 95 of check wikipedia project.
 * Error 95: Editor's signature or link to user space
 */
public class CheckErrorAlgorithm095 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm095() {
    super("Editor's signature or link to user space");
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
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }
    if (!analysis.getPage().isInMainNamespace()) {
      return false;
    }

    boolean result = false;
    result |= analyzeInternalLinks(analysis, errors);
    result |= analyzeTemplates(analysis, errors);

    return result;
  }

  /**
   * Analyze a page to check if errors are present in internal links.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeInternalLinks(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {
    List<PageElementInternalLink> links = analysis.getInternalLinks();
    if (links == null) {
      return false;
    }
    boolean result = false;
    for (PageElementInternalLink link : links) {
      result |= analyzeInternalLink(analysis, errors, link);
    }

    return result;
  }

  /**
   * Analyze a page to check if errors are present in an internal link.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param link Internal link.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeInternalLink(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementInternalLink link) {

    // Check if an error is present
    if (!isInvalidTarget(analysis, link.getLink())) {
      return false;
    }
    PageElementTemplate template = analysis.isInTemplate(link.getBeginIndex());
    if (template != null) {
      PageElementTemplate.Parameter param = template.getParameterAtIndex(link.getBeginIndex());
      if (param != null) {
        Set<String> ignoredParameters = ignoreTemplates.get(template.getTemplateName());
        if (ignoredParameters != null && ignoredParameters.contains(param.getComputedName())) {
          return false;
        }
      }
    }

    // Report error
    if (errors == null) {
      return true;
    }
    int beginIndex = link.getBeginIndex();
    int endIndex = link.getEndIndex();
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, beginIndex, endIndex);
    errors.add(errorResult);
    return true;
  }

  /**
   * Analyze a page to check if errors are present in templates.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTemplates(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {
    List<PageElementTemplate> templates = analysis.getTemplates();
    if (templates == null) {
      return false;
    }
    boolean result = false;
    for (PageElementTemplate template : templates) {
      result |= analyzeTemplate(analysis, errors, template);
    }

    return result;
  }

  /**
   * Analyze a page to check if errors are present in a template.
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

    // Check if an error is present
    if (!isInvalidTarget(analysis, template.getTemplateName())) {
      return false;
    }

    // Report error
    if (errors == null) {
      return true;
    }
    int beginIndex = template.getBeginIndex();
    int endIndex = template.getEndIndex();
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, beginIndex, endIndex);
    errors.add(errorResult);
    return true;
  }

  /**
   * Check if a target is invalid.
   * 
   * @param analysis Page analysis.
   * @param target Target.
   * @return True if the target is invalid.
   */
  private boolean isInvalidTarget(PageAnalysis analysis, String target) {
    if ((target == null) ||
        (target.trim().length() == 0) ||
        (target.indexOf(":") < 0)) {
      return false;
    }
    Page page = DataManager.createSimplePage(analysis.getWikipedia(), target, null, null, null);
    Integer namespace = page.getNamespace();
    if (namespace == null) {
      return false;
    }
    if ((namespace.intValue() != Namespace.USER) &&
        (namespace.intValue() != Namespace.USER_TALK) &&
        (namespace.intValue() != Namespace.DRAFT) &&
        (namespace.intValue() != Namespace.DRAFT_TALK)) {
      return false;
    }
    return true;
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Templates that should be ignored */
  private static final String PARAMETER_IGNORE_TEMPLATES = "ignore_templates";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_IGNORE_TEMPLATES, true, true, false);
    ignoreTemplates.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      if (tmpList != null) {
        for (String[] array : tmpList) {
          if (array.length > 1) {
            Set<String> set = ignoreTemplates.computeIfAbsent(Page.normalizeTitle(array[0]), key -> new HashSet<>());
            for (int index = 1; index < array.length; index++) {
              set.add(array[index]);
            }
          }
        }
      }
    }
  }

  /** Templates that should be ignored */
  private final Map<String, Set<String>> ignoreTemplates = new HashMap<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_IGNORE_TEMPLATES,
        GT._T("Templates that should be ignored"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template name",
                GT._T("Template to be ignored")),
            new AlgorithmParameterElement(
                "template param",
                GT._T("Parameter to be ignored"),
                false,
                true)
        },
        true));
  }
}
