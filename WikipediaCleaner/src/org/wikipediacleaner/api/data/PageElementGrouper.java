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
import org.wikipediacleaner.api.data.contents.tag.TagType;


public class PageElementGrouper {

  private final String punctuation;
  private final Collection<String> separators;
  private final Collection<TagType> tagTypes;

  /**
   * @param punctuation Possible punctuation between elements.
   * @param separators Possible separators between elements.
   * @param tagTypes Possible tags between elements.
   */
  public PageElementGrouper(
      final String punctuation,
      final Collection<String> separators,
      final Collection<TagType> tagTypes) {
    this.punctuation = punctuation;
    this.separators = separators;
    this.tagTypes = tagTypes;
  }

  /**
   * Group consecutive elements.
   * 
   * @param elements List of elements.
   * @param firstIndex Index of first element in the list.
   * @param analysis Page analysis.
   * @return Index of last element in the group of consecutive elements.
   */
  public int groupElements(List<PageElement> elements, int firstIndex, PageAnalysis analysis) {
    if (elements == null) {
      return firstIndex;
    }
    int elementIndex = firstIndex;
    while (elementIndex + 1 < elements.size()) {
      int nextBeginIndex = elements.get(elementIndex + 1).getBeginIndex();
      int currentIndex = elements.get(elementIndex).getEndIndex();
      if (!canGroup(analysis, currentIndex, nextBeginIndex)) {
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
   * @return True if the text can be between consecutive elements.
   */
  private boolean canGroup(final PageAnalysis analysis, final int beginIndex, final int endIndex) {
    if (endIndex <= beginIndex) {
      return true;
    }
    final String contents = analysis.getContents();

    // Check for separators
    if (separators != null) {
      for (String separator : separators) {
        if (contents.startsWith(separator, beginIndex)) {
          return canGroup(analysis, beginIndex + separator.length(), endIndex);
        }
      }
    }

    // Check for unbreakable whitespace
    if (contents.startsWith("&nbsp;", beginIndex)) {
      return canGroup(analysis, beginIndex + "&nbsp;".length(), endIndex);
    }

    // Check for whitespace
    if (Character.isWhitespace(contents.charAt(beginIndex))) {
      return canGroup(analysis, beginIndex + 1, endIndex);
    }

    // Check for punctuation
    if ((punctuation != null) && (punctuation.indexOf(contents.charAt(beginIndex))>= 0)) {
      return canGroup(analysis, beginIndex + 1, endIndex);
    }

    // Check for tags
    if ((tagTypes != null) && (contents.charAt(beginIndex) == '<')) {
      PageElementTag tag = analysis.isInTag(beginIndex);
      if ((tag != null) && (tag.isComplete()) && (tagTypes.contains(tag.getType()))) {
        if (tag.isFullTag()) {
          return canGroup(analysis, tag.getCompleteEndIndex(), endIndex);
        }
        return canGroup(analysis, tag.getValueBeginIndex(), tag.getValueEndIndex())
            && canGroup(analysis, tag.getCompleteEndIndex(), endIndex);
      }
    }

    return false;
  }
}
