/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.List;


/**
 * Class containing information about an element of the page.
 */
public abstract class PageElement {

  private final int beginIndex;
  private final int endIndex;

  /**
   * @param beginIndex Beginning of the page element.
   * @param endIndex End of the page element.
   */
  public PageElement(int beginIndex, int endIndex) {
    this.beginIndex = beginIndex;
    this.endIndex = endIndex;
  }

  /**
   * @return Beginning of the page element.
   */
  public int getBeginIndex() {
    return beginIndex;
  }

  /**
   * @return End of the page element.
   */
  public int getEndIndex() {
    return endIndex;
  }

  /**
   * Group consecutive elements.
   * 
   * @param elements List of elements.
   * @param firstIndex Index of first element in the list.
   * @param contents Page contents.
   * @param punctuation Possible punctuation between elements.
   * @param separator Possible separator between elements.
   * @return Index of last element in the group of consecutive elements.
   */
  public static int groupElements(
      List<PageElement> elements,
      int firstIndex,
      String contents,
      String punctuation, String separator) {
    if (elements == null) {
      return firstIndex;
    }
    int elementIndex = firstIndex;
    while (elementIndex + 1 < elements.size()) {
      int nextBeginIndex = elements.get(elementIndex + 1).getBeginIndex();
      int currentIndex = elements.get(elementIndex).getEndIndex();
      boolean separatorFound = false;
      while (currentIndex < nextBeginIndex) {
        if (!separatorFound &&
            (separator != null) &&
            contents.startsWith(separator, currentIndex)) {
          separatorFound = true;
          currentIndex += separator.length();
        } else if (contents.startsWith("&nbsp;", currentIndex)) {
          currentIndex += "&nbsp;".length();
        } else if (!Character.isWhitespace(contents.charAt(currentIndex)) &&
            ((punctuation == null) ||
             (punctuation.indexOf(contents.charAt(currentIndex)) < 0))) {
          return elementIndex;
        } else {
          currentIndex++;
        }
      }
      elementIndex++;
    }
    return elementIndex;
  }

  /**
   * Create a textual representation of a list of elements.
   * 
   * @param elements List of elements.
   * @param firstIndex Index of first element in the list.
   * @param lastIndex Index of last element in the list.
   * @param contents Page contents.
   * @param separator Separator.
   * @return Textual representation of a list of elements.
   */
  public static String createListOfElements(
      List<PageElement> elements,
      int firstIndex, int lastIndex,
      String contents, String separator) {
    StringBuilder buffer = new StringBuilder();
    int index = firstIndex;
    while (index <= lastIndex) {
      if ((index > firstIndex) && (separator != null)) {
        buffer.append(separator);
      }
      int beginIndex = elements.get(index).getBeginIndex();
      int endIndex = elements.get(index).getEndIndex();
      index++;
      buffer.append(contents.substring(beginIndex, endIndex));
    }
    return buffer.toString();
  }
}
