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
import org.wikipediacleaner.api.data.CharacterUtils;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTag;


/**
 * Algorithm for analyzing error 549 of check wikipedia project.
 * Error 549: Split link.
 */
public class CheckErrorAlgorithm549 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm549() {
    super("Splitted link");
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

    // Check each kind of links
    boolean result = false;
    result |= analyzeInternalLinks(analysis, errors);
    //result |= analyzeExternalLinks(analysis, errors);
    //result |= analyzeInterwikiLinks(analysis, errors);

    return result;
  }

  /**
   * Analyze a page to check if errors are present in internal links.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeInternalLinks(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {

    List<PageElementInternalLink> links = analysis.getInternalLinks();
    if (links == null) {
      return false;
    }
    String contents = analysis.getContents();
    boolean result = false;
    int linkNum = 0;
    List<PageElementInternalLink> tmpLinks = new ArrayList<>();
    tmpLinks.add(null);
    while (linkNum < links.size()) {

      // Check for consecutive links with the same target
      PageElementInternalLink firstLink = links.get(linkNum);
      tmpLinks.set(0, firstLink);
      String fullLink = firstLink.getFullLink();
      int endIndex = firstLink.getEndIndex();
      linkNum++;
      boolean finished = false;
      while (!finished && (linkNum < links.size())) {

        // Ignore special characters after the link
        int tmpIndex = endIndex;
        while (!finished && (tmpIndex < contents.length())) {
          char tmpChar = contents.charAt(tmpIndex);
          if (tmpChar == '<') {
            PageElementTag tag = analysis.isInTag(tmpIndex);
            if ((tag != null) &&
                (tag.getBeginIndex() == tmpIndex) &&
                (PageElementTag.TAG_WIKI_NOWIKI.equals(tag.getNormalizedName()))) {
              tmpIndex = tag.getEndIndex();
            } else {
              finished = true;
            }
          } else {
            finished = true;
          }
        }

        // Check if the next link is just after the current link
        finished = false;
        if ((links.get(linkNum).getBeginIndex() == tmpIndex) &&
            (fullLink.equals(links.get(linkNum).getFullLink()))) {
          PageElementInternalLink lastLink = links.get(linkNum);
          tmpLinks.add(lastLink);
          endIndex = lastLink.getEndIndex();
          linkNum++;
        } else {
          finished = true;
        }
      }

      // Report error
      if (tmpLinks.size() > 1) {
        if (errors == null) {
          return true;
        }
        result = true;

        // Construct possible replacement
        StringBuilder buffer = new StringBuilder();
        boolean automatic = true;
        for (PageElementInternalLink link : tmpLinks) {
          String text = link.getDisplayedTextNotTrimmed();
          boolean nowiki = false;
          boolean punctuation = false;
          boolean otherChar = false;
          if (link.getTextOffset() > 0) {
            int charNum = 0;
            while (charNum < text.length()) {
              int currentIndex = link.getBeginIndex() + link.getTextOffset() + charNum;
              char currentChar = contents.charAt(currentIndex);
              if (currentChar == '<') {
                PageElementTag tag = analysis.isInTag(currentIndex);
                if ((tag != null) &&
                    (tag.getBeginIndex() == currentIndex) &&
                    (PageElementTag.TAG_WIKI_NOWIKI.equals(tag.getNormalizedName()))) {
                  nowiki = true;
                  charNum += tag.getEndIndex() - tag.getBeginIndex() - 1;
                } else {
                  otherChar = true;
                }
              } else if (".,;:".indexOf(currentChar) >= 0) {
                punctuation = true;
              } else if (!CharacterUtils.isWhitespace(currentChar)) {
                otherChar = true;
              }
              charNum++;
            }
          } else {
            otherChar = true;
          }
          boolean textEquals = text.equals(buffer.toString());
          if (textEquals) {
            automatic = false;
          } else if (!nowiki || otherChar || punctuation) {
            if (text.startsWith("'") &&
                buffer.toString().endsWith("'")) {
              automatic = false;
            }
            buffer.append(text);
            if (nowiki) {
              automatic = false;
            }
            if (punctuation && !otherChar) {
              automatic = false;
            }
          }
        }
        if (buffer.length() == 0) {
          automatic = false;
        }

        // Report error
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, firstLink.getBeginIndex(), endIndex);
        errorResult.addReplacement(PageElementInternalLink.createInternalLink(fullLink, buffer.toString()), automatic);
        errors.add(errorResult);

        // Restore the temporary list to one element
        tmpLinks.clear();
        tmpLinks.add(null);
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
    if (!analysis.getPage().isArticle() ||
        !analysis.getPage().isInMainNamespace()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }
}
