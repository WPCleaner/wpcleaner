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

    // Retrieve configuration
    String suffixes = getSpecificProperty("templates", true, true, false);
    List<String> listSuffixes = null;
    if (suffixes != null) {
      listSuffixes = WPCConfiguration.convertPropertyToStringList(suffixes);
    } else {
      listSuffixes = new ArrayList<String>();
    }
    listSuffixes.add("nd");
    listSuffixes.add("rd");
    listSuffixes.add("st");
    listSuffixes.add("th");
    Map<String, String> replacements = null;
    String tmpReplacements = getSpecificProperty("replacements", true, true, false);
    if (tmpReplacements != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmpReplacements);
      if (tmpList != null) {
        replacements = new HashMap<String, String>();
        for (String tmp : tmpList) {
          int equalIndex = tmp.indexOf('=');
          if ((equalIndex > 0) && (equalIndex < tmp.length() - 1)) {
            replacements.put(tmp.substring(0, equalIndex), tmp.substring(equalIndex + 1));
          }
        }
      }
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

  /**
   * @return Map of parameters (Name -> description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put("templates", GT._("List of ordinal suffixes"));
    parameters.put("replacements", GT._("List of possible replacements"));
    return parameters;
  }
}
