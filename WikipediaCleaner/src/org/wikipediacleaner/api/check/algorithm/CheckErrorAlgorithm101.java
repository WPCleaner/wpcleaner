/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 101 of check wikipedia project.
 * Error 101: Ordinal numbers found inside &lt;sup&gt; tags
 */
public class CheckErrorAlgorithm101 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm101() {
    super("Ordinal numbers found inside <sup> tags");
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

    // Check every <sup> tag
    List<PageElementTag> supTags = analysis.getCompleteTags(PageElementTag.TAG_HTML_SUP);
    String contents = analysis.getContents();
    boolean result = false;
    for (PageElementTag supTag : supTags) {
      if (supTag.isComplete() && !supTag.isFullTag()) {

        // Check if digit is before supTag
        boolean digitBefore = false;
        int beginIndex = supTag.getBeginIndex();
        while ((beginIndex > 0) &&
               Character.isDigit(contents.charAt(beginIndex - 1))) {
          digitBefore = true;
          beginIndex--;
        }

        // Check if tag content is ordinal
        String value = null;
        boolean ordinal = false;
        if (digitBefore) {
          value = contents.substring(supTag.getValueBeginIndex(), supTag.getValueEndIndex()).trim();
          for (String suffix : listSuffixes) {
            if (suffix.equalsIgnoreCase(value)) {
              ordinal = true;
            }
          }
        }

        // Report error
        if (digitBefore && ordinal) {
          if (errors == null) {
            return true;
          }
          result = true;
          String digits = contents.substring(beginIndex, supTag.getBeginIndex());
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis,
              beginIndex, supTag.getCompleteEndIndex());
          if (replacements != null) {
            String replacement = replacements.get(digits + value);
            if (replacement != null) {
              errorResult.addReplacement(replacement);
            }
          }
          errorResult.addReplacement(digits + value);
          errors.add(errorResult);
        }
      }
    }
    return result;
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** List of ordinal suffixes */
  private static final String PARAMETER_TEMPLATES = "templates";

  /** List of possible replacements */
  private static final String PARAMETER_REPLACEMENTS = "replacements";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_TEMPLATES, true, true, false);
    listSuffixes.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        listSuffixes.addAll(tmpList);
      }
    }
    listSuffixes.add("nd");
    listSuffixes.add("rd");
    listSuffixes.add("st");
    listSuffixes.add("th");

    tmp = getSpecificProperty(PARAMETER_REPLACEMENTS, true, true, false);
    replacements.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        for (String tmpItem : tmpList) {
          int equalIndex = tmpItem.indexOf('=');
          if ((equalIndex > 0) && (equalIndex < tmpItem.length() - 1)) {
            replacements.put(tmpItem.substring(0, equalIndex), tmpItem.substring(equalIndex + 1));
          }
        }
      }
    }
  }

  /** List of ordinal suffixes */
  private final List<String> listSuffixes = new ArrayList<>();

  /** List of possible replacements */
  private final Map<String, String> replacements = new HashMap<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATES,
        GT._T("List of ordinal suffixes"),
        new AlgorithmParameterElement(
            "suffix",
            GT._T("Ordinal suffix")),
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_REPLACEMENTS,
        GT._T("List of possible replacements"),
        new AlgorithmParameterElement(
            "suffix=replacement",
            GT._T("A possible replacemement for one suffix")),
        true));
  }
}
