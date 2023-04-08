/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a55x.a559;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationString;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElement;
import org.wikipediacleaner.api.data.PageElementFullTag;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.IntervalComparator;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 559 of check wikipedia project.
 * Error 559: Force separator between reference tags
 */
public class CheckErrorAlgorithm559 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm559() {
    super("Force separator between reference tags");
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

    // Analyze from the beginning
    List<PageElement> refs = getRefs(analysis);
    if ((refs == null) || (refs.isEmpty())) {
      return false;
    }
    boolean result = false;
    String contents = analysis.getContents();
    int refIndex = 0;
    int maxRefs = refs.size();
    while (refIndex < maxRefs) {

      // Group references separated only by punctuation characters
      int lastRefIndex = PageElement.groupElements(refs, refIndex, contents, ",;.\'′’", separators);
      result |= analyzeGroupOfTags(analysis, contents, errors, refs, refIndex, lastRefIndex);
      refIndex = lastRefIndex + 1;
    }
    return result;
  }

  /**
   * Analyze a group of tags.
   * 
   * @param analysis Page analysis.
   * @param contents Page contents.
   * @param errors Errors found in the page.
   * @param refs List of references.
   * @param firstRefIndex Index of the first reference of the group.
   * @param lastRefIndex Index of the last reference of the group.
   * @return True if the error was found in the group of tags.
   */
  private boolean analyzeGroupOfTags(
      PageAnalysis analysis, String contents,
      Collection<CheckErrorResult> errors,
      List<PageElement> refs,
      int firstRefIndex, int lastRefIndex) {

    if (lastRefIndex == firstRefIndex) {
      return false;
    }
    boolean result = false;
    for (int firstIndex = firstRefIndex; firstIndex < lastRefIndex; firstIndex++) {

      // Check if error should be reported
      PageElement firstRef = refs.get(firstIndex);
      PageElement secondRef = refs.get(firstIndex + 1);
      int beginIndex = firstRef.getEndIndex();
      int endIndex = secondRef.getBeginIndex();
      if (endIndex < beginIndex) {
        // To prevent problems with <ref><ref></ref></ref>...
        return false;
      }
      String separatorText = contents.substring(beginIndex, endIndex).trim();
      boolean shouldReport = !StringUtils.equals(separator, separatorText);
      if ((shouldReport) &&
          (analysis.getSurroundingTag(WikiTagType.REFERENCES, beginIndex) != null)) {
        shouldReport = false;
      }
      if (shouldReport && !referencesTemplates.isEmpty()) {
        PageElementTemplate template = analysis.isInTemplate(beginIndex);
        if (template != null) {
          for (String[] referencesTemplate : referencesTemplates) {
            if ((referencesTemplate.length > 2) &&
                Page.areSameTitle(template.getTemplateName(), referencesTemplate[0])) {
              Parameter param = template.getParameterAtIndex(beginIndex);
              if ((param != null) &&
                  StringUtils.equals(param.getComputedName(), referencesTemplate[2])) {
                shouldReport = false;
              }
            }
          }
        }
      }

      // Report error
      if (shouldReport) {
        if (errors == null) {
          return true;
        }
        int beginSelection = Math.max(beginIndex - 2, 0);
        int endSelection = Math.min(endIndex + 2, contents.length());
        CheckErrorResult errorResult = createCheckErrorResult(analysis, beginSelection, endSelection);
        boolean automatic = true;
        automatic &= canRemoveBetween(contents, firstRef, secondRef);
        automatic &= forceInTemplates || (analysis.isInTemplate(beginIndex) == null);
        String replacement =
            contents.substring(beginSelection, beginIndex) +
            separator +
            contents.substring(endIndex, endSelection);
        errorResult.addReplacement(replacement, automatic);
        errors.add(errorResult);
        result = true;
      }
    }

    return result;
  }

  /**
   * Check if text can be removed between references.
   * 
   * @param contents Page contents.
   * @param previousRef Previous reference.
   * @param nextRef Next reference.
   * @return True if the text between the two references can be safely removed.
   */
  private boolean canRemoveBetween(
      String contents,
      PageElement previousRef,
      PageElement nextRef) {
    String text = contents.substring(previousRef.getEndIndex(), nextRef.getBeginIndex());
    int currentIndex = 0;
    int nbBold = 0;
    int nbItalic = 0;
    while (currentIndex < text.length()) {
      if (text.startsWith("''''", currentIndex)) {
        return false;
      }
      if (text.startsWith("'''", currentIndex)) {
        if (nbItalic > 0) {
          return false;
        }
        nbBold++;
        currentIndex += 3;
      } else if (text.startsWith("''", currentIndex)) {
        if (nbBold > 0) {
          return false;
        }
        nbItalic++;
        currentIndex += 2;
      } else {
        currentIndex++;
      }
    }
    return ((nbBold % 2) == 0) && ((nbItalic % 2) == 0);
  }

  /**
   * @param analysis Page analysis.
   * @return List of references (tags, templates, ...).
   */
  private List<PageElement> getRefs(PageAnalysis analysis) {
    List<PageElement> refs = new ArrayList<>();

    // Retrieve references defined by tags
    List<PageElementTag> refTags = analysis.getCompleteTags(WikiTagType.REF);
    if (refTags != null) {
      for (PageElementTag refTag : refTags) {
        refs.add(new PageElementFullTag(refTag));
      }
    }

    // Retrieve references defined by templates
    if (!templatesName.isEmpty()) {
      List<PageElementTemplate> templates = analysis.getTemplates();
      for (PageElementTemplate template : templates) {
        if (templatesName.contains(template.getTemplateName())) {
          refs.add(template);
        }
      }
    }

    Collections.sort(refs, new IntervalComparator());
    return refs;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    if (!analysis.getPage().isArticle() ||
        !analysis.getPage().isInMainNamespace()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Separator between consecutive tags */
  private static final String PARAMETER_SEPARATOR = "separator";

  /** Templates that can replace a tag */
  private static final String PARAMETER_TEMPLATES = "templates";

  /** Force replacement inside templates */
  private static final String PARAMETER_FORCE_IN_TEMPLATES = "force_in_templates";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    separators.clear();
    separator = getSpecificProperty(PARAMETER_SEPARATOR, true, false, false);
    if (separator == null) {
      separator = getWPCConfiguration().getString(WPCConfigurationString.REF_SEPARATOR);
    }
    if (separator == null) {
      separator = "";
    } else {
      separators.add(separator);
    }
    List<String> tmpList = getWPCConfiguration().getStringList(WPCConfigurationStringList.REF_OTHER_SEPARATORS);
    if (tmpList != null) {
      for (String tmp : tmpList) {
        if (!separators.contains(tmp)) {
          separators.add(tmp);
        }
      }
    }

    String tmp = getSpecificProperty(PARAMETER_TEMPLATES, true, true, false);
    templatesName.clear();
    if (tmp != null) {
      tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      for (String tmpElement : tmpList) {
        templatesName.add(Page.normalizeTitle(tmpElement));
      }
    }

    tmp = getSpecificProperty(PARAMETER_FORCE_IN_TEMPLATES, true, true, false);
    forceInTemplates = false;
    if (tmp != null) {
      forceInTemplates = Boolean.valueOf(tmp);
    }

    referencesTemplates = getWPCConfiguration().getStringArrayList(WPCConfigurationStringList.REFERENCES_TEMPLATES);
  }

  /** Valid separator between consecutive tags */
  private String separator = "";

  /** Separators between consecutive tags */
  private final List<String> separators = new ArrayList<>();

  /** Templates that can replace a tag */
  private final Set<String> templatesName = new HashSet<>();

  /** True to force automatic replacement even in templates */
  private boolean forceInTemplates = false;

  /** Templates containing references */
  private List<String[]> referencesTemplates;

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_FORCE_IN_TEMPLATES,
        GT._T("Option to apply automatic replacement even in templates"),
        new AlgorithmParameterElement(
            "true/false",
            GT._T("Option to apply automatic replacement even in templates"))));
    addParameter(new AlgorithmParameter(
        PARAMETER_SEPARATOR,
        GT._T("Used as a separator between consecutive {0} tags", "&lt;ref&gt;"),
        new AlgorithmParameterElement(
            "text",
            GT._T("Used as a separator between consecutive {0} tags", "&lt;ref&gt;"))));
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATES,
        GT._T("Templates that can be used to replace {0} tags", "&lt;ref&gt;"),
        new AlgorithmParameterElement(
            "template name",
            GT._T("Template that can be used to replace {0} tags", "&lt;ref&gt;")),
        true));
  }
}
