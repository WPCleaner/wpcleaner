/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 33 of check wikipedia project.
 * Error 33: HTML text style element &lt;u&gt;
 */
public class CheckErrorAlgorithm033 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm033() {
    super("HTML text style element <u>");
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
    if (!analysis.getPage().isArticle()) {
      return false;
    }

    // Analyze each tag
    boolean result = false;
    Collection<PageElementTag> tags = analysis.getTags(PageElementTag.TAG_HTML_U);
    String contents = analysis.getContents();
    for (PageElementTag tag : tags) {
      boolean shouldCount = true;

      // Only take the first tag in a pair
      if (shouldCount) {
        if (!tag.isFullTag() && tag.isEndTag() && tag.isComplete()) {
          shouldCount = false;
        }
      }

      // Check that error should be reported
      if (shouldCount) {
        int index = tag.getBeginIndex();
        if ((analysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, index) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_PRE, index) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, index) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT, index) != null)) {
          shouldCount = false;
        }
      }

      // Report error
      if (shouldCount) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis,
            tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
        if (tag.isFullTag()) {
          errorResult.addReplacement("");
        } else if (tag.isComplete()) {
          String value = contents.substring(tag.getValueBeginIndex(), tag.getValueEndIndex());
          errorResult.addReplacement(value);
          if (replacements != null) {
            for (String replacement : replacements) {
              errorResult.addReplacement("{{" + replacement + "|" + value + "}}");
            }
          }
          errorResult.addReplacement("''" + value + "''");
          errorResult.addReplacement("'''" + value + "'''");
        }
        errors.add(errorResult);
      }
    }
    return result;
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Templates that can replace a &lt;u&gt; tag */
  private static final String PARAMETER_TEMPLATES = "templates";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_TEMPLATES, true, true, false);
    replacements.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        replacements.addAll(tmpList);
      }
    }
  }

  /** Templates that can replace a &lt;u&gt; tag */
  private final List<String> replacements = new ArrayList<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATES,
        GT._T("Templates that can be used to replace {0} tags", "&lt;u&gt;"),
        new AlgorithmParameterElement(
            "template name",
            GT._T("Template that can be used to replace {0} tags", "&lt;u&gt;")),
        true));
  }
}
