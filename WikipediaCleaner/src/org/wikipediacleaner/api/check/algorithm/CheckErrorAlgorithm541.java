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
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 541 of check wikipedia project.
 * Error 541: Obsolete tag (see [[Special:LintErrors/obsolete-tag]])
 */
public class CheckErrorAlgorithm541 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm541() {
    super("Obsolete tag");
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

    // Analyze each kind of obsolete tag
    boolean result = false;
    result |= analyzeTags(analysis, errors, PageElementTag.TAG_HTML_CENTER);
    result |= analyzeTags(analysis, errors, PageElementTag.TAG_HTML_FONT);
    result |= analyzeTags(analysis, errors, PageElementTag.TAG_HTML_STRIKE);
    result |= analyzeTags(analysis, errors, PageElementTag.TAG_HTML_TT);

    return result;
  }

  /**
   * Analyze a page to check if an obsolete tag is present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param tagName Tag name.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTags(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      String tagName) {

    // Analyze contents to find center tags
    List<PageElementTag> tags = analysis.getTags(tagName);
    if (tags.size() == 0) {
      return false;
    }
    if (errors == null) {
      return true;
    }
    for (PageElementTag tag : tags) {

      // Report incomplete tags
      if (!tag.isComplete()) {
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, tag.getBeginIndex(), tag.getEndIndex());
        errors.add(errorResult);
      }

      // Report complete tags
      if (tag.isComplete() && !tag.isEndTag()) {
        CheckErrorResult errorResult = null;
        // TODO: Suggest fixes depending on the tag name
        if (PageElementTag.TAG_HTML_CENTER.equals(tag.getNormalizedName())) {
          errorResult = analyzeCenterTag(analysis, tag);
        } else if (PageElementTag.TAG_HTML_STRIKE.equals(tag.getNormalizedName())) {
          errorResult = analyzeStrikeTag(analysis, tag);
        } else if (PageElementTag.TAG_HTML_TT.equals(tag.getNormalizedName())) {
          errorResult = analyzeTtTag(analysis, tag);
        }
        if (errorResult == null) {
          errorResult = createCheckErrorResult(
              analysis, tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
        }
        errors.add(errorResult);
      }
    }

    return true;
  }

  /**
   * @param analysis Page analysis.
   * @param tag Center tag.
   * @return Error for the center tag.
   */
  private CheckErrorResult analyzeCenterTag(
      PageAnalysis analysis, PageElementTag tag) {
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
    replaceTag(
        analysis, errorResult,
        tag, PageElementTag.TAG_HTML_DIV, "style=\"text-align: center;\"",
        null, false);
    return errorResult;
  }

  /**
   * @param analysis Page analysis.
   * @param tag Strike tag.
   * @return Error for the strike tag.
   */
  private CheckErrorResult analyzeStrikeTag(
      PageAnalysis analysis, PageElementTag tag) {
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
    replaceTag(
        analysis, errorResult,
        tag, PageElementTag.TAG_HTML_DEL, null,
        GT._("for marking an edit"), false);
    replaceTag(
        analysis, errorResult,
        tag, PageElementTag.TAG_HTML_S, null,
        GT._("for anything else"), false);
    return errorResult;
  }

  /**
   * @param analysis Page analysis.
   * @param tag Tt tag.
   * @return Error for the tt tag.
   */
  private CheckErrorResult analyzeTtTag(
      PageAnalysis analysis, PageElementTag tag) {
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
    replaceTag(
        analysis, errorResult,
        tag, PageElementTag.TAG_HTML_CODE, null,
        GT._("preferred for source code"), false);
    replaceTag(
        analysis, errorResult,
        tag, PageElementTag.TAG_HTML_KBD, null,
        GT._("preferred for user input"), false);
    replaceTag(
        analysis, errorResult,
        tag, PageElementTag.TAG_HTML_VAR, null,
        GT._("preferred for variables"), false);
    replaceTag(
        analysis, errorResult,
        tag, PageElementTag.TAG_HTML_SAMP, null,
        GT._("preferred for output, function and tag names, etc."), false);
    replaceTag(
        analysis, errorResult,
        tag, PageElementTag.TAG_HTML_SPAN, "style=\"font-family: monospace;\"",
        GT._("preferred for everything else"), false);
    return errorResult;
  }

  /**
   * @param analysis Page analysis.
   * @param errorResult Error.
   * @param tag Initial tag.
   * @param tagName Replacement tag name.
   * @param options Optional options for the tag.
   * @param comment Optional comment.
   * @param automatic True if the replacement should be automatic.
   */
  private void replaceTag(
      PageAnalysis analysis,
      CheckErrorResult errorResult,
      PageElementTag tag, String tagName, String options,
      String comment, boolean automatic) {
    String openTag = PageElementTag.createTag(
        tagName + (options != null ? " " + options : ""), false, false);
    String closeTag = PageElementTag.createTag(tagName, true, false);
    String replacement =
        openTag +
        analysis.getContents().substring(tag.getValueBeginIndex(), tag.getValueEndIndex()) +
        closeTag;
    String text = openTag + "..." + closeTag;
    if (comment != null) {
      text += " (" + comment + ")";
    }
    errorResult.addReplacement(replacement, text, automatic);
  }

  // ==============================================================================================
  // General functions
  // ==============================================================================================

  /**
   * Automatic fixing of some errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
  }
}
