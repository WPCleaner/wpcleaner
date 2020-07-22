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
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementListItem;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;


/**
 * Algorithm for analyzing error 555 of check wikipedia project.
 * Error 555: nowiki in text.
 */
public class CheckErrorAlgorithm555 extends CheckErrorAlgorithmBase {

  /** Special texts for which replacement should not be automatic */
  private static final String[] SPECIAL_TEXTS = {
      "ISBN",
      "ISSN",
      "PMID",
      "RFC"
  };

  public CheckErrorAlgorithm555() {
    super("nowiki in text");
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

    // Global verification
    List<PageElementTag> nowikiTags = analysis.getTags(PageElementTag.TAG_WIKI_NOWIKI);
    if ((nowikiTags == null) || (nowikiTags.isEmpty())) {
      return false;
    }

    // Check each nowiki tag
    boolean result = false;
    for (PageElementTag nowikiTag : nowikiTags) {
      result |= analyzeTag(analysis, errors, nowikiTag);
    }

    return result;
  }

  /**
   * Analyze a nowiki tag to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param nowikiTag Nowiki tag to be analyzed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeTag(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTag nowikiTag) {

    // Only work on complete tags
    if ((nowikiTag == null) || !nowikiTag.isComplete()) {
      return false;
    }
    if (!nowikiTag.isFullTag() && nowikiTag.isEndTag()) {
      return false;
    }

    // Check character before the tag
    String contents = analysis.getContents();
    int beginIndex = nowikiTag.getCompleteBeginIndex();
    boolean newLineBefore = false;
    if (beginIndex > 0) {
      beginIndex--;
      if (!isAcceptableOutside(contents.charAt(beginIndex))) {
        return false;
      }
      if (contents.charAt(beginIndex) == '\n') {
        newLineBefore = true;
      }
    } else {
      newLineBefore = true;
    }

    // Check character after the tag
    int endIndex = nowikiTag.getCompleteEndIndex();
    if (endIndex < contents.length()) {
      if (!isAcceptableOutside(contents.charAt(endIndex))) {
        return false;
      }
      endIndex++;
    }

    // Check content inside the tag
    if (!nowikiTag.isFullTag()) {
      for (int index = nowikiTag.getValueBeginIndex(); index < nowikiTag.getValueEndIndex(); index++) {
        if (!isAcceptableInside(contents.charAt(index))) {
          return false;
        }

        // Ignore list items
        if ((index == nowikiTag.getValueBeginIndex()) &&
            (contents.charAt(index) == '*') &&
            newLineBefore) {
          return false;
        }
      }
    }

    // Check if replacement can be automatic
    String internalText = "";
    if (!nowikiTag.isFullTag()) {
      internalText = contents.substring(nowikiTag.getValueBeginIndex(), nowikiTag.getValueEndIndex());
      if (newLineBefore && internalText.trim().isEmpty()) {
        internalText = "";
      }
    }
    boolean automatic = true;
    if (endIndex < contents.length()) {
      // Prevent if there's an external link just after
      PageElementExternalLink eLink = analysis.isInExternalLink(endIndex);
      if ((eLink != null) &&
          (eLink.getBeginIndex() == nowikiTag.getCompleteEndIndex())) {
        automatic = false;
      }
    }
    if (beginIndex > 0) {
      // Prevent if there's an external link just before
      PageElementExternalLink eLink = analysis.isInExternalLink(beginIndex);
      if (eLink != null) {
        if ((eLink.getEndIndex() == nowikiTag.getCompleteBeginIndex()) ||
            (eLink.getBeginIndex() + eLink.getTextOffset() >= nowikiTag.getCompleteBeginIndex())) {
          automatic = false;
        }
      }
    }
    if (beginIndex > 0) {
      // Prevent if the nowiki tag is at the beginning of a template parameter
      PageElementTemplate template = analysis.isInTemplate(beginIndex);
      if (template != null) {
        Parameter param = template.getParameterAtIndex(beginIndex);
        if ((param != null) &&
            (param.getValueStartIndex() >= beginIndex)) {
          automatic = false;
        }
      }
    }
    for (String specialText : SPECIAL_TEXTS) {
      // Prevent for some special texts
      if (internalText.contains(specialText)) {
        automatic = false;
      }
      int tmpIndex = ContentsUtil.moveIndexBeforeWhitespace(contents, beginIndex);
      if (contents.substring(0, tmpIndex + 1).endsWith(specialText)) {
        automatic = false;
      }
    }
    if (newLineBefore) {
      // Prevent if a special character would end up at the beginning of a line
      char firstChar = 0;
      if (internalText.isEmpty()) {
        if (nowikiTag.getCompleteEndIndex() < contents.length()) {
          firstChar = contents.charAt(nowikiTag.getCompleteEndIndex());
        }
      } else {
        firstChar = internalText.charAt(0);
      }
      if (" *".indexOf(firstChar) >= 0) {
        automatic = false;
      }
    }
    if (internalText.indexOf(':') >= 0) {
      PageElementListItem listItem = analysis.isInListItem(beginIndex);
      if (listItem != null) {
        // TODO: Restrict to list items starting with ";"
        automatic = false;
      }
    }

    // Report error
    if (errors == null) {
      return true;
    }
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
    String prefix = contents.substring(beginIndex, nowikiTag.getCompleteBeginIndex());
    String suffix = contents.substring(nowikiTag.getCompleteEndIndex(), endIndex);
    errorResult.addReplacement(prefix + internalText + suffix, automatic);
    errors.add(errorResult);
    return true;
  }

  /**
   * @param character Character to be tested.
   * @return True if the character is acceptable for this error inside the nowiki tag.
   */
  private boolean isAcceptableInside(char character) {
    return
        Character.isAlphabetic(character) ||
        Character.isDigit(character) ||
        Character.isWhitespace(character) ||
        (".,()".indexOf(character) >= 0);
  }

  /**
   * @param character Character to be tested.
   * @return True if the character is acceptable for this error.
   */
  private boolean isAcceptableOutside(char character) {
    return
        isAcceptableInside(character) ||
        (";:*".indexOf(character) >= 0);
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
