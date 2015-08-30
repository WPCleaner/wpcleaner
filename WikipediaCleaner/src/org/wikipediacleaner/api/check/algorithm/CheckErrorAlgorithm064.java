/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

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
      String paddingLeft = "";
      String simplePaddingLeft = "";
      String paddingRight = "";
      String simplePaddingRight = "";
      boolean same = false;
      boolean automatic = false;
      if (((anchor == null) || (anchor.trim().length() == 0)) &&
          (text != null)) {
        if (!same && Page.areSameTitle(linkName, text)) {
          same = true;
          automatic = true;
        }
        if (!same && Page.areSameTitle(linkName, text.replaceAll("\\_", " "))) {
          same = true;
          automatic = false;
        }
        if (!same) {
          int position = text.length();
          while ((position > 0) && (",.".indexOf(text.charAt(position - 1)) >= 0)) {
            position--;
          }
          if (position < text.length()) {
            paddingRight = text.substring(position);
            simplePaddingRight = paddingRight;
            text = text.substring(0, position);
            if (!same && Page.areSameTitle(linkName, text)) {
              same = true;
              automatic = true;
            }
            if (!same && Page.areSameTitle(linkName, text.replaceAll("\\_", " "))) {
              same = true;
              automatic = false;
            }
          }
        }
        if (!same) {
          int countQuoteBefore = 0;
          while ((countQuoteBefore < text.length()) && (text.charAt(countQuoteBefore) == '\'')) {
            countQuoteBefore++;
          }
          int countQuoteAfter = 0;
          while ((countQuoteAfter < text.length()) && (text.charAt(text.length() - countQuoteAfter - 1) == '\'')) {
            countQuoteAfter++;
          }
          if ((countQuoteBefore > 1) && (countQuoteAfter == countQuoteBefore) && (text.length() > countQuoteBefore)) {
            paddingLeft = paddingLeft + text.substring(0, countQuoteBefore);
            paddingRight = text.substring(text.length() - countQuoteAfter) + paddingRight;
            text = text.substring(countQuoteBefore, text.length() - countQuoteAfter);
            if (Page.areSameTitle(linkName, text) || Page.areSameTitle(linkName, text.replaceAll("\\_", " "))) {
              same = true;
              automatic = true;
              if ((link.getBeginIndex() > 0) && (content.charAt(link.getBeginIndex() - 1) == '\'')) {
                automatic = false;
              }
              if ((link.getEndIndex() < content.length()) && (content.charAt(link.getEndIndex()) == '\'')) {
                automatic = false;
              }
            }
          }
        }
      }

      // Report error
      if (same && (text != null)) {
        if (errors == null) {
          return true;
        }
        result = true;
        String cleanedText = text.replaceAll("\\_", " ");
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis,
            link.getBeginIndex(),
            link.getEndIndex());
        errorResult.addReplacement(
            paddingLeft + PageElementInternalLink.createInternalLink(text, null) + paddingRight,
            automatic);
        if (!text.equals(cleanedText)) {
          errorResult.addReplacement(
              paddingLeft + PageElementInternalLink.createInternalLink(cleanedText, null) + paddingRight,
              false);
        }
        if (!simplePaddingLeft.equals(paddingLeft) || !simplePaddingRight.equals(paddingRight)) {
          errorResult.addReplacement(
              simplePaddingLeft + PageElementInternalLink.createInternalLink(text, null) + simplePaddingRight);
          if (!text.equals(cleanedText)) {
            errorResult.addReplacement(
                simplePaddingLeft + PageElementInternalLink.createInternalLink(cleanedText, null) + simplePaddingRight);
          }
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
