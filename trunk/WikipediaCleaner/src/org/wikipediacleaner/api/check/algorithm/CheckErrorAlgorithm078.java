/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 78 of check wikipedia project.
 * Error 78: Reference double
 */
public class CheckErrorAlgorithm078 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm078() {
    super("Reference double");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors) {
    if (pageAnalysis == null) {
      return false;
    }

    // Retrieve all <references> tags
    List<PageElementTag> referencesTags = pageAnalysis.getTags(PageElementTag.TAG_WIKI_REFERENCES);
    if ((referencesTags == null) || (referencesTags.size() == 0)) {
      return false;
    }

    // Check all <references> tags
    boolean result = false;
    Map<String, PageElementTag> firstTags = new HashMap<String, PageElementTag>();
    Set<String> tagUsed = new TreeSet<String>();
    for (PageElementTag referencesTag : referencesTags) {

      // Use only beginning tags
      if (referencesTag.isFullTag() || !referencesTag.isEndTag()) {
        // Retrieve "group"
        PageElementTag.Parameter group = referencesTag.getParameter("group");
        String groupName = "";
        if ((group != null) && (group.getValue() != null)) {
          groupName = group.getValue();
        }
  
        // Check if a <references> tag already exist for this group
        PageElementTag firstTag = firstTags.get(groupName);
        if (firstTag == null) {
          firstTags.put(groupName, referencesTag);
        } else {
          if (errors == null) {
            return true;
          }
          result = true;
          if (!tagUsed.contains(groupName)) {
            tagUsed.add(groupName);
            CheckErrorResult errorResult = createCheckErrorResult(
                pageAnalysis.getPage(),
                firstTag.getCompleteBeginIndex(),
                firstTag.getCompleteEndIndex(),
                ErrorLevel.CORRECT);
            errorResult.addReplacement("", GT._("Delete"));
            errors.add(errorResult);
          }
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(),
              referencesTag.getCompleteBeginIndex(),
              referencesTag.getCompleteEndIndex());
          errorResult.addReplacement("", GT._("Delete"));
          errors.add(errorResult);
        }
      }
    }
    return result;
  }
}
