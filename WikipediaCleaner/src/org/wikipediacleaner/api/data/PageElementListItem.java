/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.data.contents.ContentsUtil;


/**
 * Class containing information about a list item.
 */
public class PageElementListItem extends PageElement {

  public final static String LIST_INDICATORS = "*:#;";

  /**
   * @param character Character to be tested.
   * @return True if the character is a list indicator.
   */
  public static boolean isListIndicator(char character) {
    return LIST_INDICATORS.indexOf(character) >= 0;
  }

  /**
   * @param analysis Page analysis.
   * @return List of tables.
   */
  public static List<PageElementListItem> analyzePage(
      PageAnalysis analysis) {
    List<PageElementListItem> items = new ArrayList<>();

    // Analysis for possible list items
    int index = 0;
    String contents = analysis.getContents();
    while (index < contents.length()) {
      char currentChar = contents.charAt(index);
      if (isListIndicator(currentChar) &&
          ((index == 0) || (contents.charAt(index - 1) == '\n'))) {
        int beginIndex = index;
        index = ContentsUtil.moveIndexForwardWhileFound(contents, index, LIST_INDICATORS);
        int depth = index - beginIndex;
        index = ContentsUtil.moveIndexForwardWhileNotFound(contents, index, "\n");
        items.add(new PageElementListItem(beginIndex, index, depth));
      } else {
        index++;
      }
    }

    return items;
  }

  /**
   * @param index Current index.
   * @param items List of list items.
   * @return True if the current index is already in a table.
   */
  public static PageElementListItem isInListItem(int index, List<PageElementListItem> items) {
    if (items != null) {
      for (PageElementListItem tmpItem : items) {
        if ((tmpItem.getBeginIndex() <= index) &&
            (tmpItem.getEndIndex() > index)) {
          return tmpItem;
        }
      }
    }
    return null;
  }

  /** Depth */
  private final int depth;

  /**
   * @param beginIndex Begin index.
   * @param endIndex End index.
   * @param depth Depth.
   */
  private PageElementListItem(
      int beginIndex, int endIndex, int depth) {
    super(beginIndex, endIndex);
    this.depth = depth;
  }

  /**
   * @return Depth.
   */
  public int getDepth() {
    return depth;
  }
}