/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsComment;
import org.wikipediacleaner.api.data.contents.ContentsUtil;


/**
 * Class containing information about a paragraph.
 */
public class PageElementParagraph extends PageElement {

  /**
   * @param analysis Page analysis.
   * @return List of paragraphs.
   */
  public static List<PageElementParagraph> analyzePage(
      PageAnalysis analysis) {
    List<PageElementParagraph> paragraphs = new ArrayList<>();

    // Analyze the page to find paragraphs
    int paragraphBegin = 0;
    String contents = analysis.getContents();
    final int maxLen = contents.length();
    while (paragraphBegin < maxLen) {

      // Move paragraph beginning
      boolean tryMoving = true;
      while (tryMoving) {
        tryMoving = false;

        // Ignore carriage returns
        while ((paragraphBegin < maxLen) &&
               (contents.charAt(paragraphBegin) == '\n')) {
          paragraphBegin++;
          tryMoving = true;
        }

        // Ignore titles
        if ((paragraphBegin < maxLen) &&
            (contents.charAt(paragraphBegin) == '=')) {
          PageElementTitle title = analysis.isInTitle(paragraphBegin);
          if ((title != null) && (title.getBeginIndex() == paragraphBegin)) {
            paragraphBegin = ContentsUtil.moveIndexAfterWhitespace(contents, title.getEndIndex());
            tryMoving = true;
          }
        }

        // Ignore comments
        if ((paragraphBegin < maxLen) &&
            (contents.charAt(paragraphBegin) == '<')) {
          ContentsComment comment = analysis.isInComment(paragraphBegin);
          if ((comment != null) && (comment.getBeginIndex() == paragraphBegin)) {
            paragraphBegin = comment.getEndIndex();
            tryMoving = true;
          }
        }

        // Ignore list items
        if ((paragraphBegin < maxLen) &&
            (PageElementListItem.LIST_INDICATORS.indexOf(contents.charAt(paragraphBegin)) >= 0)) {
          PageElementListItem listItem = analysis.isInListItem(paragraphBegin);
          if ((listItem != null) && (listItem.getBeginIndex() == paragraphBegin)) {
            paragraphBegin = listItem.getEndIndex();
            tryMoving = true;
          }
        }
      }

      // Look for the end of the paragraph
      boolean contentFound = false;
      boolean endFound = false;
      int paragraphEnd = paragraphBegin;
      while ((paragraphEnd < maxLen) && !endFound) {

        // First look for a carriage return
        while ((paragraphEnd < maxLen) &&
               (contents.charAt(paragraphEnd) != '\n')) {
          contentFound = true;
          paragraphEnd++;
        }

        // Check if it's the end of the paragraph
        if (paragraphEnd + 1 < maxLen) {
          char nextChar = contents.charAt(paragraphEnd + 1);
          if (nextChar == '\n') {
            endFound = true;
          } else if (nextChar == '=') {
            PageElementTitle title = analysis.isInTitle(paragraphEnd + 1);
            if ((title != null) && (title.getBeginIndex() == paragraphEnd + 1)) {
              endFound = true;
            }
          } else if (PageElementListItem.LIST_INDICATORS.indexOf(nextChar) >= 0) {
            PageElementListItem listItem = analysis.isInListItem(paragraphEnd + 1);
            if ((listItem != null) && (listItem.getBeginIndex() == paragraphEnd + 1)) {
              endFound = true;
            }
          }
          if (!endFound) {
            paragraphEnd++;
          }
        } else {
          endFound = true;
        }
      }

      // Memorize the paragraph
      if (contentFound && endFound) {
        paragraphs.add(new PageElementParagraph(paragraphBegin, paragraphEnd));
      }
      paragraphBegin = paragraphEnd;
    }

    return paragraphs;
  }

  /**
   * @param index Current index.
   * @param paragraphs List of paragraphs.
   * @return Paragraph if the current index is in a paragraph.
   */
  public static PageElementParagraph isInParagraph(int index, List<PageElementParagraph> paragraphs) {
    if (paragraphs != null) {
      for (PageElementParagraph tmpParagraph : paragraphs) {
        if ((tmpParagraph.getBeginIndex() <= index) &&
            (tmpParagraph.getEndIndex() > index)) {
          return tmpParagraph;
        }
      }
    }
    return null;
  }

  /**
   * @param beginIndex Begin index.
   * @param endIndex End index.
   */
  private PageElementParagraph(
      int beginIndex, int endIndex) {
    super(beginIndex, endIndex);
  }
}
