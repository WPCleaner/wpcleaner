/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a1xx.a11x.a111;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.i18n.GT;

/**
 * Algorithm for analyzing error 111 of check wikipedia project.
 * Error 111: Ref after last reference list
 */
public class CheckErrorAlgorithm111 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm111() {
    super("Ref after last reference list");
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

    // Analyzing text for <ref> tags
    PageElementTag lastRefTag = null;
    List<PageElementTag> refTags = analysis.getTags(WikiTagType.REF);
    if ((refTags != null) && (refTags.size() > 0)) {
      for (int numTag = refTags.size() - 1; (numTag >= 0) && (lastRefTag == null); numTag--) {
        boolean usefulRef = true;
        PageElementTag refTag = refTags.get(numTag);
        if (analysis.getSurroundingTag(WikiTagType.NOWIKI, refTag.getBeginIndex()) != null) {
          usefulRef =  false;
        }
        if (usefulRef) {
          lastRefTag = refTag;
        }
      }
    }
    if (lastRefTag == null) {
      return false;
    }
    boolean referencesFound = false;

    // Analyzing text for <references> tags
    List<PageElementTag> referencesTags = analysis.getTags(WikiTagType.REFERENCES);
    if (referencesTags != null) {
      for (PageElementTag referencesTag : referencesTags) {
        if (referencesTag.isComplete()) {
          if (referencesTag.getCompleteEndIndex() > lastRefTag.getCompleteEndIndex()) {
            return false;
          }
          referencesFound = true;
        }
      }
    }

    // Search for templates like {{References}}
    if (!referencesTemplates.isEmpty()) {
      PageElementTag openTag = null;
      if (lastRefTag.isFullTag() || !lastRefTag.isEndTag()) {
        openTag = lastRefTag;
      } else {
        openTag = lastRefTag.getMatchingTag();
      }
      String groupName = null;
      if (openTag != null) {
        PageElementTag.Parameter paramGroupName = openTag.getParameter("group");
        if (paramGroupName != null) {
          groupName = paramGroupName.getValue();
        }
      }
      List<PageElementTemplate> allTemplates = analysis.getTemplates();
      int templateNum = allTemplates.size();
      while (templateNum > 0) {
        templateNum--;
        PageElementTemplate template = allTemplates.get(templateNum);
        for (String[] referencesTemplate : referencesTemplates) {
          if (Page.areSameTitle(template.getTemplateName(), referencesTemplate[0])) {
            boolean shouldCount = false;
            if (referencesTemplate.length > 1) {
              if ((groupName != null) &&
                  (groupName.equals(referencesTemplate[1]))) {
                shouldCount = true;
              }
            } else {
              shouldCount = true;
            }
            if (shouldCount) {
              if (template.getEndIndex() > lastRefTag.getCompleteEndIndex()) {
                return false;
              }
              referencesFound = true;
            }
          }
        }
      }
    }
    if (!referencesFound) {
      return false;
    }

    // Report error
    if (errors == null) {
      return true;
    }
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis,
        lastRefTag.getCompleteBeginIndex(),
        lastRefTag.getCompleteEndIndex(),
        referencesFound ? ErrorLevel.WARNING : ErrorLevel.ERROR);
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
    return fixUsingAutomaticReplacement(analysis);
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** List of templates */
  private static final String PARAMETER_TEMPLATES = "templates";

  /** List of templates */
  private static final String PARAMETER_REFERENCES_TEMPLATES = "references_templates";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_TEMPLATES, true, true, true);
    if (tmp == null) {
      tmp = getSpecificProperty(PARAMETER_REFERENCES_TEMPLATES, true, true, true);
    }
    referencesTemplates.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      if (tmpList != null) {
        referencesTemplates.addAll(tmpList);
      }
    }
  }

  /** List of templates */
  private final List<String[]> referencesTemplates = new ArrayList<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    //addParameter(new AlgorithmParameter(
    //    PARAMETER_REFERENCES_TEMPLATES,
    //    GT._T("A list of templates resulting in the inclusion of {0}", "&lt;references/&gt;")));
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATES,
        GT._T("A list of templates resulting in the inclusion of {0}", "&lt;references/&gt;"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template name",
                GT._T("Template name")),
            new AlgorithmParameterElement(
                "group parameter",
                GT._T("Parameter for the name of the group"),
                true)
        },
        true));
  }
}
