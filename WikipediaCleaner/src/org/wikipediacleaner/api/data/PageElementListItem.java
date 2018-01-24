/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.List;


/**
 * Class containing information about a list item.
 */
public class PageElementListItem extends PageElement {

  final static String LIST_INDICATORS = "*:#;";

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
      if ((LIST_INDICATORS.indexOf(currentChar) >= 0) &&
          ((index == 0) || (contents.charAt(index - 1) == '\n'))) {
        int beginIndex = index;
        while ((index < contents.length()) &&
               (LIST_INDICATORS.indexOf(contents.charAt(index)) >= 0)) {
          index++;
        }
        int depth = index - beginIndex;
        while ((index < contents.length()) &&
               (contents.charAt(index) != '\n')) {
          index++;
        }
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