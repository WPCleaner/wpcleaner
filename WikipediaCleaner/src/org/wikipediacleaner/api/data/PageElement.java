/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.Collection;
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
   * Group consecutive elements.
   * 
   * @param elements List of elements.
   * @param firstIndex Index of first element in the list.
   * @param contents Page contents.
   * @param punctuation Possible punctuation between elements.
   * @param separators Possible separators between elements.
   * @return Index of last element in the group of consecutive elements.
   */
  public static int groupElements(
      List<PageElement> elements,
      int firstIndex,
      String contents,
      String punctuation,
      Collection<String> separators) {
    if (elements == null) {
      return firstIndex;
    }
    int elementIndex = firstIndex;
    while (elementIndex + 1 < elements.size()) {
      int nextBeginIndex = elements.get(elementIndex + 1).getBeginIndex();
      int currentIndex = elements.get(elementIndex).getEndIndex();
      if (!canGroup(contents, currentIndex, nextBeginIndex, punctuation, separators)) {
        return elementIndex;
      }
      elementIndex++;
    }
    return elementIndex;
  }

  /**
   * Analyze if text can be between consecutive elements.
   * 
   * @param contents Page contents.
   * @param beginIndex Begin index of the text.
   * @param endIndex End index of the text.
   * @param punctuation Possible punctuation between elements.
   * @param separators Possible separators between elements.
   * @return True if the text can be between consecutive elements.
   */
  private static boolean canGroup(
      final String contents, final int beginIndex, final int endIndex,
      final String punctuation, Collection<String> separators) {
    if (endIndex <= beginIndex) {
      return true;
    }

    // Check for separators
    if (separators != null) {
      for (String separator : separators) {
        if (contents.startsWith(separator, beginIndex)) {
          return canGroup(contents, beginIndex + separator.length(), endIndex, punctuation, separators);
        }
      }
    }

    // Check for unbreakable whitespace
    if (contents.startsWith("&nbsp;", beginIndex)) {
      return canGroup(contents, beginIndex + "&nbsp;".length(), endIndex, punctuation, separators);
    }

    // Check for whitespace
    if (Character.isWhitespace(contents.charAt(beginIndex))) {
      return canGroup(contents, beginIndex + 1, endIndex, punctuation, separators);
    }

    // Check for punctuation
    if ((punctuation != null) && (punctuation.indexOf(contents.charAt(beginIndex))>= 0)) {
      return canGroup(contents, beginIndex + 1, endIndex, punctuation, separators);
    }

    return false;
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
