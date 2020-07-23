/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents;

import java.util.List;

/**
 * Finder for a type of element.
 */
public class ContentsFinder<T extends ContentsElement> {

  /** List of elements */
  private final List<ContentsFinderElement<T>> elements;

  /**
   * Constructor.
   * 
   * @param builder Builder.
   * @param useSmallest Flag for specifying if the finder should search for the smallest element.
   */
  public ContentsFinder(ContentsFinderBuilder<T> builder, boolean useSmallest) {
    this.elements = builder.createFinderElements(useSmallest);
  }

  /**
   * Find the element at a given index.
   * 
   * @param index Index.
   * @return Element at the index.
   */
  public T findAt(int index) {
    return findAt(index, 0, elements.size());
  }

  /**
   * Find the element at a given index.
   * 
   * @param index Index.
   * @param start Position of the first element.
   * @param end Position of the last element.
   * @return Element at the index
   */
  private T findAt(int index, int start, int end) {
    if (start >= end) {
      return null;
    }
    int position = (start + end) / 2;
    ContentsFinderElement<T> element = elements.get(position);
    if (element.containsIndex(index)) {
      return element.getElement();
    }
    if (index < element.getBeginIndex()) {
      return findAt(index, start, position);
    }
    return findAt(index, position + 1, end);
  }
}
