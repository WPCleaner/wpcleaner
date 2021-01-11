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
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementInterwikiLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 54 of check wikipedia project.
 * Error 54: Break in list
 */
public class CheckErrorAlgorithm054 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._T("Remove breaks in list"),
  };

  public CheckErrorAlgorithm054() {
    super("Break in list");
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

    // Check that there are <br> tags in the text
    List<PageElementTag> brTags = analysis.getTags(HtmlTagType.BR);
    if ((brTags == null) || (brTags.isEmpty())) {
      return false;
    }

    // Analyzing the text from the beginning
    boolean result = false;
    int endLineIndex = -1;
    String contents = analysis.getContents();
    StringBuilder suffix = new StringBuilder();
    while (endLineIndex + 1 < contents.length()) {

      // Check if the next line is a list
      int beginLineIndex = endLineIndex + 1;
      boolean isList = (contents.charAt(beginLineIndex) == '*');

      // Searching next end line
      endLineIndex = contents.indexOf("\n", beginLineIndex);
      if (endLineIndex < 0) {
        endLineIndex = contents.length();
      }

      // Checking if the line ends with a <br />
      if (isList) {

        // Search for <br /> at the end of the line
        boolean breakFound = false;
        boolean imageFound = false;
        boolean tagAfter = false;
        int currentPos = endLineIndex - 1;
        int beginError = endLineIndex;
        int endError = endLineIndex;
        boolean automaticBot = true;
        boolean shouldStop = false;
        suffix.setLength(0);
        while (!shouldStop) {
          shouldStop = true;
          while ((currentPos > beginLineIndex) &&
                 (Character.isWhitespace(contents.charAt(currentPos)))) {
            currentPos--;
          }
          if (contents.charAt(currentPos) == '>') {
            PageElementTag tag = analysis.isInTag(currentPos);
            if (tag != null) {
              if (HtmlTagType.BR.equals(tag.getType())) {
                // Limit error if image as been found.
                if (!imageFound) {
                  breakFound = true;
                }
                shouldStop = false;
                beginError = tag.getBeginIndex();
                currentPos = beginError - 1;
                if (tag.getParametersCount() > 0) {
                  automaticBot = false;
                }
              } else if (!breakFound) {
                /*if (PageElementTag.TAG_WIKI_MATH.equals(name)) {
                  tagAfter = true;
                  shouldStop = false;
                  endError = tag.getCompleteBeginIndex();
                  currentPos = endError - 1;
                }*/
              }
            }
          } else if (contents.charAt(currentPos) == ']') {
            boolean linkFound = false;
            if (!linkFound) {
              PageElementInternalLink link = analysis.isInInternalLink(currentPos);
              if ((link != null) && (link.getEndIndex() == currentPos + 1)) {
                currentPos -= 2;
                shouldStop = false;
                automaticBot = false;
                suffix.insert(0, "]]");
                linkFound = true;
              }
            }
            if (!linkFound) {
              PageElementImage image = analysis.isInImage(currentPos);
              if ((image != null) && (image.getEndIndex() == currentPos + 1)) {
                currentPos -= 2;
                shouldStop = false;
                automaticBot = false;
                suffix.insert(0, "]]");
                linkFound = true;
                imageFound = true;
              }
            }
            if (!linkFound) {
              PageElementInterwikiLink link = analysis.isInInterwikiLink(currentPos);
              if ((link != null) && (link.getEndIndex() == currentPos + 1)) {
                currentPos -= 2;
                shouldStop = false;
                automaticBot = false;
                suffix.insert(0, "]]");
                linkFound = true;
              }
            }
            if (!linkFound) {
              PageElementCategory link = analysis.isInCategory(currentPos);
              if ((link != null) && (link.getEndIndex() == currentPos + 1)) {
                currentPos -= 2;
                shouldStop = false;
                automaticBot = false;
                suffix.insert(0, "]]");
                linkFound = true;
              }
            }
            if (!linkFound) {
              PageElementExternalLink link = analysis.isInExternalLink(currentPos);
              if ((link != null) && (link.getEndIndex() == currentPos + 1)) {
                currentPos -= 1;
                shouldStop = false;
                automaticBot = false;
                suffix.insert(0, "]");
                linkFound = true;
              }
            }
          }
        }

        // Report error
        if (breakFound) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, beginError, endError,
              (tagAfter ? ErrorLevel.WARNING : ErrorLevel.ERROR));
          if (!tagAfter) {
            errorResult.addReplacement(suffix.toString(), false, automaticBot);
          }
          errors.add(errorResult);
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
    return fixUsingAutomaticBotReplacement(analysis);
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
