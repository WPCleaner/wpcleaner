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
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 509 of check wikipedia project.
 * Error 509: Internal link could be compacted
 */
public class CheckErrorAlgorithm509 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Compact links"),
  };

  public CheckErrorAlgorithm509() {
    super("Internal link could be compacted");
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
    if ((pageAnalysis == null) || (pageAnalysis.getInternalLinks() == null)) {
      return false;
    }

    // Analyze each internal link
    boolean result = false;
    List<PageElementInternalLink> links = pageAnalysis.getInternalLinks();
    String contents = pageAnalysis.getContents();
    int maxLength = contents.length();
    for (PageElementInternalLink link : links) {
      if (link.getText() != null) {
        int endIndex = link.getEndIndex();
        while ((endIndex < maxLength) && (Character.isLetter(contents.charAt(endIndex)))) {
          endIndex++;
        }
        if (endIndex > link.getEndIndex()) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(), link.getBeginIndex(), endIndex);
          String replacement = PageElementInternalLink.createInternalLink(
              link.getLinkNotNormalized(), link.getAnchor(),
              link.getDisplayedTextNotTrimmed() + contents.substring(link.getEndIndex(), endIndex));
          errorResult.addReplacement(replacement, GT._("Compact link"));
          errors.add(errorResult);
        }
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
  public String automaticFix(PageAnalysis analysis) {
    return fix(globalFixes[0], analysis, null);
  }

  /**
   * @return List of possible global fixes.
   */
  @Override
  public String[] getGlobalFixes() {
    return globalFixes;
  }

  /**
   * Fix all the errors in the page.
   * 
   * @param fixName Fix name (extracted from getGlobalFixes()).
   * @param analysis Page analysis.
   * @param textPane Text pane.
   * @return Page contents after fix.
   */
  @Override
  public String fix(String fixName, PageAnalysis analysis, MWPane textPane) {
    return fixUsingFirstReplacement(fixName, analysis);
  }
}
