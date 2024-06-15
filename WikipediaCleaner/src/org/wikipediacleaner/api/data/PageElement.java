/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.List;

import org.wikipediacleaner.api.data.contents.ContentsElement;


/**
 * Class containing information about an element of the page.
 */
public abstract class PageElement extends ContentsElement {

  /**
   * @param beginIndex Beginning of the page element.
   * @param endIndex End of the page element.
   */
  public PageElement(int beginIndex, int endIndex) {
    super(null, beginIndex, endIndex);
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
