/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 64 of check wikipedia project.
 * Error 64: Link equal to link text
 */
public class CheckErrorAlgorithm064 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Modify all internal links"),
  };

  public CheckErrorAlgorithm064() {
    super("Link equal to linktext");
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

    // Check every internal link
    Collection<PageElementInternalLink> links = analysis.getInternalLinks();
    if ((links == null) || (links.isEmpty())) {
      return false;
    }
    boolean result = false;
    String content = analysis.getContents();
    for (PageElementInternalLink link : links) {

      // Analyze
      String anchor = link.getAnchor();
      String linkName = link.getLink();
      String text = link.getText();
      String cleanedText = (text != null) ? text.replaceAll("\\_", " ") : text;
      String paddingLeft = "";
      String paddingRight = "";
      boolean same = false;
      boolean automatic = false;
      if (((anchor == null) || (anchor.trim().length() == 0)) &&
          (text != null)) {

        // Check for exact same title
        if (!same) {
          if (Page.areSameTitle(linkName, text)) {
            same = true;
            automatic = true;
          } else if (Page.areSameTitle(linkName, cleanedText)) {
            same = true;
          }
        }

        // Check for extra punctuation at the end
        if (!same) {
          int position = text.length();
          while ((position > 0) && (",.".indexOf(text.charAt(position - 1)) >= 0)) {
            position--;
          }
          if (position < text.length()) {
            paddingRight = text.substring(position) + paddingRight;
            text = text.substring(0, position);
            cleanedText = (text != null) ? text.replaceAll("\\_", " ") : text;
            if (Page.areSameTitle(linkName, text)) {
              same = true;
              automatic = true;
            } else if (Page.areSameTitle(linkName, cleanedText)) {
              same = true;
            }
          }
        }

        // Check for surrounding quotes
        if (!same && (text != null)) {
          int countQuoteBefore = 0;
          while ((countQuoteBefore < text.length()) &&
                 (text.charAt(countQuoteBefore) == '\'')) {
            countQuoteBefore++;
          }
          int countQuoteAfter = 0;
          while ((countQuoteAfter < text.length()) &&
                 (text.charAt(text.length() - countQuoteAfter - 1) == '\'')) {
            countQuoteAfter++;
          }
          if ((countQuoteBefore > 1) &&
              (text.length() > countQuoteBefore)) {
            paddingLeft = paddingLeft + text.substring(0, countQuoteBefore);
            paddingRight = text.substring(text.length() - countQuoteAfter) + paddingRight;
            text = text.substring(countQuoteBefore, text.length() - countQuoteAfter);
            cleanedText = (text != null) ? text.replaceAll("\\_", " ") : text;
            if (Page.areSameTitle(linkName, text)) {
              same = true;
              if ((link.getBeginIndex() <= 0) ||
                  (content.charAt(link.getBeginIndex() - 1) != '\'')) {
                if ((link.getEndIndex() >= content.length()) ||
                    (content.charAt(link.getEndIndex()) != '\'')) {
                  automatic = true;
                }
              }
            } else if (Page.areSameTitle(linkName, cleanedText)) {
              same = true;
            }
          }
        }
      }

      // Check for extra punctuation at the end (again, in case punctuation removed)
      if (!same && (text != null)) {
        int position = text.length();
        while ((position > 0) && (",.".indexOf(text.charAt(position - 1)) >= 0)) {
          position--;
        }
        if (position < text.length()) {
          paddingRight = text.substring(position) + paddingRight;
          text = text.substring(0, position);
          cleanedText = (text != null) ? text.replaceAll("\\_", " ") : text;
          if (Page.areSameTitle(linkName, text)) {
            same = true;
            automatic = true;
          } else if (Page.areSameTitle(linkName, cleanedText)) {
            same = true;
          }
        }
      }

      // Report error
      if (same && (text != null)) {
        if (errors == null) {
          return true;
        }
        result = true;

        // Analyze for possible extra modifications
        int beginIndex = link.getBeginIndex();
        int endIndex = link.getEndIndex();
        String extraRight = "";
        String extraFullRight = "";
        if ((endIndex < content.length()) &&
            (content.charAt(endIndex) == '\'')) {
          int countQuoteBefore = 0;
          int tmpIndex = endIndex;
          while ((tmpIndex < content.length() &&
                 (content.charAt(tmpIndex) == '\''))) {
            tmpIndex++;
            countQuoteBefore++;
          }
          while ((tmpIndex < content.length()) &&
                 (" ,.:".indexOf(content.charAt(tmpIndex)) >= 0)) {
            tmpIndex++;
          }
          int countQuoteAfter = 0;
          while ((tmpIndex < content.length() &&
                 (content.charAt(tmpIndex) == '\''))) {
            tmpIndex++;
            countQuoteAfter++;
          }
          if (countQuoteBefore == countQuoteAfter) {
            extraRight = content.substring(endIndex + countQuoteBefore, tmpIndex - countQuoteAfter);
            extraFullRight = content.substring(endIndex, tmpIndex);
            endIndex = tmpIndex;
          }
        }

        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, beginIndex, endIndex);
        List<String> replacements = new ArrayList<>();
        String replacement = null;
        replacement = paddingLeft + PageElementInternalLink.createInternalLink(text, null) + paddingRight + extraRight;
        if (!automatic && !replacements.contains(replacement)) {
          errorResult.addReplacement(replacement);
          replacements.add(replacement);
        }
        replacement = paddingLeft + PageElementInternalLink.createInternalLink(text, null) + paddingRight + extraFullRight;
        if (!replacements.contains(replacement)) {
          errorResult.addReplacement(replacement, automatic);
          replacements.add(replacement);
        }
        replacement = paddingLeft + PageElementInternalLink.createInternalLink(text, null) + paddingRight + extraRight;
        if (!replacements.contains(replacement)) {
          errorResult.addReplacement(replacement);
          replacements.add(replacement);
        }
        replacement = paddingLeft + PageElementInternalLink.createInternalLink(cleanedText, null) + paddingRight + extraFullRight;
        if (!replacements.contains(replacement)) {
          errorResult.addReplacement(replacement);
          replacements.add(replacement);
        }
        replacement = paddingLeft + PageElementInternalLink.createInternalLink(cleanedText, null) + paddingRight + extraRight;
        if (!replacements.contains(replacement)) {
          errorResult.addReplacement(replacement);
          replacements.add(replacement);
        }
        paddingLeft = paddingLeft.replaceAll("\'", "");
        paddingRight = paddingRight.replaceAll("\'", "");
        replacement = paddingLeft + PageElementInternalLink.createInternalLink(text, null) + paddingRight + extraFullRight;
        if (!replacements.contains(replacement)) {
          errorResult.addReplacement(replacement);
          replacements.add(replacement);
        }
        replacement = paddingLeft + PageElementInternalLink.createInternalLink(text, null) + paddingRight + extraRight;
        if (!replacements.contains(replacement)) {
          errorResult.addReplacement(replacement);
          replacements.add(replacement);
        }
        replacement = paddingLeft + PageElementInternalLink.createInternalLink(cleanedText, null) + paddingRight + extraFullRight;
        if (!replacements.contains(replacement)) {
          errorResult.addReplacement(replacement);
          replacements.add(replacement);
        }
        replacement = paddingLeft + PageElementInternalLink.createInternalLink(cleanedText, null) + paddingRight + extraRight;
        if (!replacements.contains(replacement)) {
          errorResult.addReplacement(replacement);
          replacements.add(replacement);
        }
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
    return fixUsingAutomaticReplacement(analysis);
  }
}
