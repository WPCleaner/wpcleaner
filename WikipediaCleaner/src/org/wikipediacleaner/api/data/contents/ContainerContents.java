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
abstract class ContainerContents<T extends ContentsElement> {

  /** List of elements */
  private final List<T> elements;

  /**
   * @param elements List of elements.
   */
  ContainerContents(List<T> elements) {
    this.elements = (elements != null) ? elements : new ArrayList<T>();
  }

  /**
   * @return List of elements.
   */
  public List<T> getAll() {
    return elements;
  }

  /**
   * @param index Index in the contents.
   * @return True if there's an element containing the provided index.
   */
  public boolean isAt(int index) {
    for (T element : this.elements) {
      if (element.containsIndex(index)) {
        return true;
      }
    }
    return false;
  }

  /**
   * @param index Index in the contents.
   * @return Smallest element containing the provided index.
   */
  public T getAt(int index) {
    return getSmallestAt(index);
  }

  /**
   * @param index Index in the contents.
   * @return Smallest element containing the provided index.
   */
  public T getSmallestAt(int index) {
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
  public T getLargestAt(int index) {
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

  /**
   * @param index Index in the contents.
   * @return Element beginning at the provided index.
   */
  public T getBeginsAt(int index) {
    for (T element : this.elements) {
      if (element.getBeginIndex() == index) {
        return element;
      }
    }
    return null;
  }

  /**
   * @param index Index in the contents.
   * @return Element ending at the provided index.
   */
  public T getEndsAt(int index) {
    for (T element : this.elements) {
      if (element.getEndIndex() == index) {
        return element;
      }
    }
    return null;
  }
}
