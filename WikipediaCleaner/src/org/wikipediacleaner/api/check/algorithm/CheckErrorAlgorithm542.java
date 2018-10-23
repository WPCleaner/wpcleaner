/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 542 of check wikipedia project.
 * Error 542: Empty ref tag.
 */
public class CheckErrorAlgorithm542 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm542() {
    super("Empty ref tag");
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

    // Retrieve separator between several <ref> tags
    String separator = getSpecificProperty(
        "separator", true, false, false);
    if (separator == null) {
      separator = "";
    }

    // Analyze from the beginning
    List<PageElementTag> tags = analysis.getTags(PageElementTag.TAG_WIKI_REF);
    if (tags == null) {
      return false;
    }
    boolean result = false;
    String contents = analysis.getContents();
    int tagIndex = 0;
    int maxTags = tags.size();
    while (tagIndex < maxTags) {

      // Group tags separated only by punctuation characters
      int firstTagIndex = tagIndex;
      int lastTagIndex = PageElementTag.groupTags(tags, firstTagIndex, contents, ",;.\'", separator);
      tagIndex = lastTagIndex + 1;

      // Check for empty ref tags in the group
      int currentTagIndex = firstTagIndex;
      while (currentTagIndex <= lastTagIndex) {
        PageElementTag tmpTag = tags.get(currentTagIndex);
        if (isEmpty(contents, tmpTag)) {
          if (errors == null) {
            return true;
          }
          result = true;

          // Group empty ref tags
          int tmpTagIndex = currentTagIndex;
          currentTagIndex++;
          while ((currentTagIndex <= lastTagIndex) &&
                 (isEmpty(contents, tags.get(currentTagIndex)))) {
            currentTagIndex++;
          }

          // Report error
          int beginIndex = tmpTag.getCompleteBeginIndex();
          if (tmpTagIndex > firstTagIndex) {
            beginIndex = tags.get(firstTagIndex).getCompleteEndIndex();
          }
          int endIndex = tags.get(currentTagIndex - 1).getCompleteEndIndex();
          if ((tmpTagIndex == firstTagIndex) && (currentTagIndex <= lastTagIndex)) {
            endIndex = tags.get(currentTagIndex).getCompleteBeginIndex();
          }
          CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
          errorResult.addReplacement("", false);
          errors.add(errorResult);
        } else {
          currentTagIndex++;
        }
      }
    }
    return result;
  }

  /**
   * @param contents Page contents.
   * @param tag Tag.
   * @return True if the tag is empty.
   */
  private boolean isEmpty(String contents, PageElementTag tag) {
    if ((tag == null) || (contents == null)) {
      return false;
    }
    if (tag.isFullTag()) {
      return true;
    }
    if (!tag.isComplete()) {
      return false;
    }
    for (int index = tag.getValueBeginIndex(); index < tag.getValueEndIndex(); index++) {
      if (contents.charAt(index) != ' ') {
        return false;
      }
    }
    return true;
  }

  /**
   * Return the parameters used to configure the algorithm.
   * 
   * @return Map of parameters (key=name, value=description).
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put(
        "separator",
        GT._T("Used as a separator between consecutive {0} tags", "&lt;ref&gt;"));
    return parameters;
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
}
