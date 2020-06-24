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
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Algorithm for analyzing error 042 of check wikipedia project.
 * Error 042: strike tags
 */
public class CheckErrorAlgorithm042 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm042() {
    super("<strike> tags");
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
    Integer ns = analysis.getPage().getNamespace();
    if ((ns == null) || (ns.intValue() != Namespace.MAIN)) {
      return false;
    }

    // Check each tag
    List<PageElementTag> tags = analysis.getTags(PageElementTag.TAG_HTML_STRIKE);
    if ((tags == null) || (tags.isEmpty())) {
      return false;
    }
    if (errors == null) {
      return true;
    }
    for (PageElementTag tag : tags) {
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis, tag.getBeginIndex(), tag.getEndIndex());
      errorResult.addReplacement(PageElementTag.createTag(
          PageElementTag.TAG_HTML_DEL, tag.isEndTag(), false));
      errorResult.addReplacement(PageElementTag.createTag(
          PageElementTag.TAG_HTML_S, tag.isEndTag(), tag.isFullTag()));
      errors.add(errorResult);
    }

    return true;
  }

  /**
   * Bot fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalBotFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
  }
}
