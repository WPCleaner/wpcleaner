/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.CharacterUtils;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementListItem;
import org.wikipediacleaner.api.data.PageElementTag;


/**
 * Algorithm for analyzing error 547 of check wikipedia project.
 * Error 547: Empty list item.
 */
public class CheckErrorAlgorithm547 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm547() {
    super("Empty list item");
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

    // Only in main name space
    if ((analysis.getPage().getNamespace() == null) ||
        (analysis.getPage().getNamespace().intValue() != Namespace.MAIN)) {
      return false;
    }

    // Check if list items are present
    List<PageElementListItem> listItems = analysis.getListItems();
    if ((listItems == null) || (listItems.isEmpty())) {
      return false;
    }

    // Check each list item
    boolean result = false;
    String contents = analysis.getContents();
    for (PageElementListItem listItem : listItems) {

      // Check if list item has text
      boolean shouldReport = true;
      int index = listItem.getBeginIndex() + listItem.getDepth();
      while (shouldReport && (index < listItem.getEndIndex())) {
        shouldReport = CharacterUtils.isWhitespace(contents.charAt(index));
        index++;
      }

      // Filter special cases
      if (shouldReport) {
        if ((analysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, index) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SCORE, index) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, index) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT, index) != null)) {
          shouldReport = false;
        }
      }

      // Report error
      if (shouldReport) {
        result = true;
        if (errors == null) {
          return result;
        }

        // Check if fix can be automatic
        boolean automatic = true;
        if (automatic &&
            (analysis.isInImage(index) != null)) {
          automatic = false;
        }

        // Determine boundaries
        int begin = listItem.getBeginIndex();
        int end = listItem.getEndIndex();
        boolean extended = false;
        if (end + 1 < contents.length()) {
          char nextChar = contents.charAt(end + 1);
          if ((nextChar == '\n') || PageElementListItem.isListIndicator(nextChar)) {
            end++;
            extended = true;
          }
        }
        if (!extended && (begin > 1)) {
          if ((contents.charAt(begin - 1) == '\n') &&
              (contents.charAt(begin - 2) == '\n')) {
            begin--;
            extended = true;
          }
        }

        // Report error
        CheckErrorResult errorResult = createCheckErrorResult(analysis, begin, end);
        errorResult.addReplacement("", automatic);
        errors.add(errorResult);
      }
    }
    return result;
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
}
