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
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 85 of check wikipedia project.
 * Error 85: Tag without content
 */
public class CheckErrorAlgorithm085 extends CheckErrorAlgorithmBase {

  private final static String[] interestingTags = {
    PageElementTag.TAG_HTML_CENTER,
    PageElementTag.TAG_WIKI_INCLUDEONLY,
    PageElementTag.TAG_WIKI_GALLERY,
    PageElementTag.TAG_WIKI_NOINCLUDE,
    PageElementTag.TAG_WIKI_REF,
  };

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Delete all tags without content"),
  };

  public CheckErrorAlgorithm085() {
    super("Tag without content");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }

    // Check each tag
    List<PageElementTag> tags = analysis.getTags();
    boolean result = false;
    String contents = analysis.getContents();
    for (PageElementTag tag : tags) {
      if (!tag.isFullTag() && !tag.isEndTag() && tag.isComplete()) {

        // Check if tag can be of interest
        boolean interesting = false;
        for (String tagName : interestingTags) {
          if (tagName.equals(tag.getName())) {
            interesting = true;
          }
        }
  
        // Check tag
        if (interesting) {
          boolean textFound = false;
          int currentIndex = tag.getValueBeginIndex();
          int lastIndex = tag.getValueEndIndex();
          while (!textFound && (currentIndex < lastIndex)) {
            if (!Character.isWhitespace(contents.charAt(currentIndex))) {
              textFound = true;
            }
            currentIndex++;
          }
          if (!textFound) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis,
                tag.getCompleteBeginIndex(),
                tag.getCompleteEndIndex());
            errorResult.addReplacement("", GT._("Delete"));
            errors.add(errorResult);
          }
        }
      }
    }
    return result;
  }

  /**
   * Bot fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalBotFix(PageAnalysis analysis) {
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
