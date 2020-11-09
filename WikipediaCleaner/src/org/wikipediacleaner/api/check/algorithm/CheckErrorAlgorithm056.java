/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.comment.ContentsComment;


/**
 * Algorithm for analyzing error 56 of check wikipedia project.
 * Error 56: Arrow as ASCII art
 */
public class CheckErrorAlgorithm056 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm056() {
    super("Arrow as ASCII art");
  }

  /**
   * Detected arrows and their replacement.
   */
  private final static String[][] allArrows = {
    { "<--->", "↔" },
    { "<-->" , "↔" },
    { "<->"  , "↔" },
    { "<–––>", "↔" },
    { "<––>" , "↔" },
    { "<–>"  , "↔" },
    { "<———>", "↔" },
    { "<——>" , "↔" },
    { "<—>"  , "↔" },

    { "<---" , "←" },
    { "<--"  , "←" },
    { "<-"   , "←" },
    { "<–––" , "←" },
    { "<––"  , "←" },
    { "<–"   , "←" },
    { "<———" , "←" },
    { "<——"  , "←" },
    { "<—"   , "←" },

    { "<===>", "⇔" },
    { "<==>" , "⇔" },
    { "<=>"  , "⇔" },

    { "<===" , "⇐" },
    { "<=="  , "⇐" },
    { "<="   , "⇐", "≤" },

    { "--->", "→" },
    { "-->" , "→" },
    { "->"  , "→" },
    { "–––>", "→" },
    { "––>" , "→" },
    { "–>"  , "→" },
    { "———>", "→" },
    { "——>" , "→" },
    { "—>"  , "→" },

    { "===>", "⇒" },
    { "==>" , "⇒" },
    { "=>"  , "⇒", "≥" },
  };

  /**
   * Tags in which arrows should not be detected. 
   */
  private final static String[] exceptTags = {
    PageElementTag.TAG_HTML_TT,
    PageElementTag.TAG_HTML_CODE,
    PageElementTag.TAG_WIKI_CHEM,
    PageElementTag.TAG_WIKI_HIERO,
    PageElementTag.TAG_WIKI_MATH,
    PageElementTag.TAG_WIKI_MATH_CHEM,
    PageElementTag.TAG_WIKI_NOWIKI,
    PageElementTag.TAG_WIKI_PRE,
    PageElementTag.TAG_WIKI_SCORE,
    PageElementTag.TAG_WIKI_SOURCE,
    PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT,
    PageElementTag.TAG_WIKI_TIMELINE,
  };

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

    // Check each character from the beginning
    boolean result = false;
    int currentIndex = 0;
    String contents = analysis.getContents();
    while (currentIndex < contents.length()) {
      boolean shouldCheck = true;
      int nextIndex = currentIndex + 1;

      // Check each kind of arrow
      int arrowLen = 0;
      String[] arrows = null;
      if (shouldCheck) {
        for (int i = 0; (i < allArrows.length) && (arrowLen == 0); i++) {
          if (contents.startsWith(allArrows[i][0], currentIndex)) {
            arrowLen = allArrows[i][0].length();
            arrows = allArrows[i];
          }
        }
        if (arrowLen == 0) {
          shouldCheck = false;
        }
      }

      // Check if inside a comment
      if (shouldCheck) {
        ContentsComment comment = analysis.comments().getAt(currentIndex);
        if (comment != null) {
          nextIndex = comment.getEndIndex();
          shouldCheck = false;
        }
      }

      // Check if inside a specific tag
      if (shouldCheck) {
        for (String tagName : exceptTags) {
          if (shouldCheck) {
            PageElementTag tag = analysis.getSurroundingTag(tagName, currentIndex);
            if (tag != null) {
              nextIndex = tag.getCompleteEndIndex();
              shouldCheck  = false;
            }
          }
        }
      }
      if (shouldCheck) {
        if (arrows != null) {
          if (errors == null) {
            return true;
          }
          result = true;

          // Check for cases like <ref name=>
          boolean reported = false;
          if ((arrowLen == 2) && (contents.startsWith("=>", currentIndex))) {
            PageElementTag tag = analysis.isInTag(currentIndex);
            if ((tag != null) && (tag.getEndIndex() == currentIndex + 2)) {
              int tmpIndex = currentIndex - 1;
              while ((tmpIndex > 0) && (Character.isLetter(contents.charAt(tmpIndex)))) {
                tmpIndex--;
              }
              if ((tmpIndex > 0) &&
                  (currentIndex > tmpIndex + 1) &&
                  (contents.charAt(tmpIndex) == ' ')) {
                String attributeName = contents.substring(tmpIndex + 1, currentIndex);
                boolean automatic = false;
                if (PageElementTag.TAG_WIKI_REF.equals(tag.getName()) &&
                    attributeName.equals("name")) {
                  automatic = true;
                }
                CheckErrorResult errorResult = createCheckErrorResult(
                    analysis, tmpIndex, currentIndex + arrowLen);
                errorResult.addReplacement(">", automatic);
                errors.add(errorResult);
                reported = true;
              }
            }
          }

          if (!reported) {
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, currentIndex, currentIndex + arrowLen);
            for (int i = 1; i < arrows.length; i++) {
              errorResult.addReplacement(arrows[i]);
            }
            errors.add(errorResult);
          }
        }
        nextIndex = currentIndex + arrowLen;
      }
      currentIndex = nextIndex;
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
    return fixUsingAutomaticReplacement(analysis);
  }
}
