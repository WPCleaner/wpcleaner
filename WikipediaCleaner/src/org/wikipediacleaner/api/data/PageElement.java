/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsElement;
import org.wikipediacleaner.api.data.contents.tag.TagType;


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
   * @param analysis Page analysis.
   * @param punctuation Possible punctuation between elements.
   * @param separators Possible separators between elements.
   * @param tagTypes Possible tags between elements.
   * @return Index of last element in the group of consecutive elements.
   */
  public static int groupElements(
      List<PageElement> elements,
      int firstIndex,
      PageAnalysis analysis,
      String punctuation,
      Collection<String> separators,
      Collection<TagType> tagTypes) {
    if (elements == null) {
      return firstIndex;
    }
    int elementIndex = firstIndex;
    while (elementIndex + 1 < elements.size()) {
      int nextBeginIndex = elements.get(elementIndex + 1).getBeginIndex();
      int currentIndex = elements.get(elementIndex).getEndIndex();
      if (!canGroup(analysis, currentIndex, nextBeginIndex, punctuation, separators, tagTypes)) {
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
   * @param tagTypes Possible tags between elements.
   * @return True if the text can be between consecutive elements.
   */
  private static boolean canGroup(
      final PageAnalysis analysis, final int beginIndex, final int endIndex,
      final String punctuation, Collection<String> separators, Collection<TagType> tagTypes) {
    if (endIndex <= beginIndex) {
      return true;
    }
    final String contents = analysis.getContents();

    // Check for separators
    if (separators != null) {
      for (String separator : separators) {
        if (contents.startsWith(separator, beginIndex)) {
          return canGroup(analysis, beginIndex + separator.length(), endIndex, punctuation, separators, tagTypes);
        }
      }
    }

    // Check for unbreakable whitespace
    if (contents.startsWith("&nbsp;", beginIndex)) {
      return canGroup(analysis, beginIndex + "&nbsp;".length(), endIndex, punctuation, separators, tagTypes);
    }

    // Check for whitespace
    if (Character.isWhitespace(contents.charAt(beginIndex))) {
      return canGroup(analysis, beginIndex + 1, endIndex, punctuation, separators, tagTypes);
    }

    // Check for punctuation
    if ((punctuation != null) && (punctuation.indexOf(contents.charAt(beginIndex))>= 0)) {
      return canGroup(analysis, beginIndex + 1, endIndex, punctuation, separators, tagTypes);
    }

    // Check for tags
    if ((tagTypes != null) && (contents.charAt(beginIndex) == '<')) {
      PageElementTag tag = analysis.isInTag(beginIndex);
      if ((tag != null) && (tag.isComplete()) && (tagTypes.contains(tag.getType()))) {
        if (tag.isFullTag()) {
          return canGroup(analysis, tag.getCompleteEndIndex(), endIndex, punctuation, separators, tagTypes);
        }
        return canGroup(analysis, tag.getValueBeginIndex(), tag.getValueEndIndex(), punctuation, separators, tagTypes)
            && canGroup(analysis, tag.getCompleteEndIndex(), endIndex, punctuation, separators, tagTypes);
      }
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
