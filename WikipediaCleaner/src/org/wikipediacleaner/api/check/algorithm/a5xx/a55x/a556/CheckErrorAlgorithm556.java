/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a55x.a556;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysisUtils;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.CompleteTagBuilder;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.api.data.contents.template.TemplateBuilder;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 555 of check wikipedia project.
 * Error 556: external link in text.
 */
public class CheckErrorAlgorithm556 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm556() {
    super("external link in text");
  }

  private final List<Pair<String, String>> urlTemplates = new ArrayList<>();

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

    // Global verification
    List<PageElementExternalLink> links = analysis.getExternalLinks();
    if ((links == null) || (links.isEmpty())) {
      return false;
    }
    if (!analysis.getPage().isArticle()) {
      return false;
    }

    // Check each external link
    boolean result = false;
    for (PageElementExternalLink link : links) {
      result |= analyzeLink(analysis, errors, link);
    }

    return result;
  }

  /**
   * Analyze an external link to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param link External link to be analyzed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeLink(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementExternalLink link) {

    // Check if the link is in some constructions
    int beginIndex = link.getBeginIndex();
    if ((analysis.getSurroundingTag(WikiTagType.REF, beginIndex) != null) ||
        (analysis.isInTag(beginIndex, WikiTagType.REFERENCES) != null)) {
      return false;
    }

    // Check if the link is in some sections
    if (!titles.isEmpty()) {
      List<PageElementTitle> currentTitles = PageAnalysisUtils.getCurrentTitles(analysis, beginIndex);
      if (currentTitles != null) {
        for (PageElementTitle title : currentTitles) {
          if (titles.contains(title.getTitle().toUpperCase())) {
            return false;
          }
        }
      }
    }

    // Check if the link is in some templates
    if (!templates.isEmpty() || !templatesParams.isEmpty()) {
      PageElementTemplate template = analysis.isInTemplate(beginIndex);
      if (template != null) {
        // Check if the parameter name is allowed
        Parameter param = template.getParameterAtIndex(beginIndex);
        if ((param != null) &&
            templatesParams.contains(param.getComputedName())) {
          return false;
        }

        // Check if the template allows links in this parameter
        if (param != null) {
          for (String[] possibleTemplate : templates) {
            if (Page.areSameTitle(possibleTemplate[0], template.getTemplateName())) {
              if (possibleTemplate.length < 2) {
                return false;
              }
              for (int paramNum = 1; paramNum < possibleTemplate.length; paramNum++) {
                if (possibleTemplate[paramNum].equals(param.getComputedName())) {
                  return false;
                }
              }
            }
          }
        }
      }
    }

    // Report error
    if (errors == null) {
      return true;
    }
    int endIndex = link.getEndIndex();
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
    String replacement = CompleteTagBuilder.from(
        WikiTagType.REF,
        analysis.getContents().substring(beginIndex, endIndex)).toString();
    errorResult.addReplacement(
        replacement,
        GT._T(
            "Add inside a {0} tag",
            WikiTagType.REF.getFullTag()));
    if (link.hasSquare() && link.hasSecondSquare()) {
      errorResult.addReplacement(link.getDisplayedText());
      errorResult.addReplacement(
          link.getDisplayedText() + replacement,
          GT._T(
              "Keep text and add link inside a {0} tag",
              WikiTagType.REF.getFullTag()));
    }
    if (!link.hasSecondSquare()) {
      for (Pair<String, String> template : urlTemplates) {
        replacement = TemplateBuilder.from(template.getLeft()).addParam(template.getRight(), link.getLink()).toString();
        errorResult.addReplacement(
            replacement,
            GT._T("Use {0}", TemplateBuilder.from(template.getLeft()).toString()));
      }
    }
    errors.add(errorResult);
    return true;
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** List of templates where external links are normal */
  private static final String PARAMETER_TEMPLATES = "templates";

  /** List of templates parameters where external links are normal */
  private static final String PARAMETER_TEMPLATES_PARAMS = "templates_parameters";

  /** List of sections where external links are normal */
  private static final String PARAMETER_TITLES = "titles";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_TEMPLATES, true, true, true);
    templates.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      if (tmpList != null) {
        templates.addAll(tmpList);
      }
    }

    tmp = getSpecificProperty(PARAMETER_TEMPLATES_PARAMS, true, true, true);
    templatesParams.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        templatesParams.addAll(tmpList);
      }
    }

    tmp = getSpecificProperty(PARAMETER_TITLES, true, true, true);
    titles.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        for (String element : tmpList) {
          titles.add(element.toUpperCase());
        }
      }
    }

    urlTemplates.clear();
    List<String[]> tmpList = getWPCConfiguration().getStringArrayList(WPCConfigurationStringList.URL_TEMPLATES);
    if (tmpList != null) {
      for (String[] tmpElement : tmpList) {
        if (tmpElement.length > 1) {
          urlTemplates.add(new ImmutablePair<>(tmpElement[0], tmpElement[1]));
        }
      }
    }
  }

  /** Templates where external links are normal */
  private final List<String[]> templates = new ArrayList<>();

  /** Templates parameters where external links are normal */
  private final Set<String> templatesParams = new HashSet<>();

  /** Sections where external links are normal */
  private final Set<String> titles = new HashSet<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATES,
        GT._T("Templates in which external links are normal"),
        new AlgorithmParameterElement[] {
          new AlgorithmParameterElement(
              "template name",
              GT._T("Template in which external links are normal")),
          new AlgorithmParameterElement(
              "parameter name",
              GT._T("Parameter in which external links are normal"),
              true, true)
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATES_PARAMS,
        GT._T("Template parameters in which external links are normal"),
        new AlgorithmParameterElement[] {
          new AlgorithmParameterElement(
              "parameter name",
              GT._T("Parameter in which external links are normal"))
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_TITLES,
        GT._T("Sections in which external links are normal"),
        new AlgorithmParameterElement(
            "title",
            GT._T("Section in which external links are normal")),
        true));
  }
}
