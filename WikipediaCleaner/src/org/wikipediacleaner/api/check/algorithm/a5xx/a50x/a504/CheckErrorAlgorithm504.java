/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a50x.a504;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 504 of check wikipedia project.
 * Error 504: Reference in title
 */
public class CheckErrorAlgorithm504 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm504() {
    super("Reference in title");
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

    // Check every reference
    Collection<PageElementTag> refs = analysis.getCompleteTags(WikiTagType.REF);
    if ((refs == null) || (refs.isEmpty())) {
      return false;
    }
    final Map<PageElementTitle, List<PageElementTag>> titles = refs.stream()
        .filter(ref -> analysis.isInTitle(ref.getCompleteBeginIndex()) != null)
        .collect(Collectors.groupingBy(ref -> analysis.isInTitle(ref.getCompleteBeginIndex())))
;
    if (titles.isEmpty()) {
      return false;
    }

    // Report error
    if (errors == null) {
      return true;
    }

    titles.entrySet().forEach(entry -> reportTitle(analysis, entry.getKey(), entry.getValue(), errors));
    return true;
  }

  private void reportTitle(
      PageAnalysis analysis,
      PageElementTitle title,
      List<PageElementTag> refs,
      Collection<CheckErrorResult> errors) {
    CheckErrorResult error = createCheckErrorResult(analysis, title.getBeginIndex(), title.getEndIndex());
    String contents = analysis.getContents();
    
    // Suggestion to remove references
    String replacement =
        contents.substring(title.getBeginIndex(), refs.get(0).getCompleteBeginIndex()) +
        contents.substring(refs.get(refs.size() - 1).getCompleteEndIndex(), title.getEndIndex());
    error.addReplacement(replacement, GT._T("Remove references"));

    errors.add(error);
  }
}
