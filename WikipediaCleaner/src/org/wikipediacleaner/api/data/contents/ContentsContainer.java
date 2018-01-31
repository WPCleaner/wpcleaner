/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents;

import java.util.ArrayList;
import java.util.List;


/**
 * Container for a type of element.
 */
abstract class ContentsContainer<T extends ContentsElement> {

  /** List of elements */
  private final List<T> elements;

  /**
   * @param elements List of elements.
   */
  ContentsContainer(List<T> elements) {
    this.elements = (elements != null) ? elements : new ArrayList<T>();
  }

  /**
   * @return List of elements.
   */
  public List<T> getElements() {
    return elements;
  }

  /**
   * @param index Index in the contents.
   * @return Smallest element containing the provided index.
   */
  public T getSmallestElementAtIndex(int index) {
    T result = null;
    for (T element : this.elements) {
      if (element.containsIndex(index)) {
        if (result == null) {
          result = element;
        } else if (element.getBeginIndex() > result.getBeginIndex()) {
          result = element;
        } else if ((element.getBeginIndex() == result.getBeginIndex()) &&
                   (element.getEndIndex() < result.getEndIndex())) {
          result = element;
        }
      }
    }
    return result;
  }

  /**
   * @param index Index in the contents.
   * @return Largest element containing the provided index.
   */
  public T getLargestElementAtIndex(int index) {
    T result = null;
    for (T element : this.elements) {
      if (element.containsIndex(index)) {
        if (result == null) {
          result = element;
        } else if (element.getBeginIndex() < result.getBeginIndex()) {
          result = element;
        } else if ((element.getBeginIndex() == result.getBeginIndex()) &&
                   (element.getEndIndex() > result.getEndIndex())) {
          result = element;
        }
      }
    }
    return result;
  }
}
