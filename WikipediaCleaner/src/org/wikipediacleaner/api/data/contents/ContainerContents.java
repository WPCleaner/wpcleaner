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
public abstract class ContainerContents<T extends ContentsElement> {

  /** List of elements */
  private final List<T> elements;

  /** Finder for smallest elements */
  private final ContentsFinder<T> smallestFinder;

  /** Finder for largest elements */
  private final ContentsFinder<T> largestFinder;

  /**
   * @param elements List of elements.
   */
  protected ContainerContents(List<T> elements, ContainerBehavior behavior) {
    this.elements = (elements != null) ? elements : new ArrayList<T>();
    ContentsFinderBuilder<T> finderBuilder = new ContentsFinderBuilder<>();
    finderBuilder.addAll(elements);
    switch (behavior) {
    case SMALLEST_ONLY:
      this.smallestFinder = new ContentsFinder<>(finderBuilder, true);
      this.largestFinder = this.smallestFinder;
      break;
    case LARGEST_ONLY:
      this.largestFinder = new ContentsFinder<>(finderBuilder, false);
      this.smallestFinder = this.largestFinder;
      break;
    default:
      this.smallestFinder = new ContentsFinder<>(finderBuilder, true);
      this.largestFinder = new ContentsFinder<>(finderBuilder, false);
      break;
    }
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
    return (largestFinder.findAt(index) != null);
  }

  /**
   * @param index Index in the contents.
   * @return Smallest element containing the provided index.
   */
  public T getAt(int index) {
    return smallestFinder.findAt(index);
  }

  /**
   * @param index Index in the contents.
   * @return Smallest element containing the provided index.
   */
  public T getSmallestAt(int index) {
    return smallestFinder.findAt(index);
  }

  /**
   * @param index Index in the contents.
   * @return Largest element containing the provided index.
   */
  public T getLargestAt(int index) {
    return largestFinder.findAt(index);
  }

  /**
   * @param index Index in the contents.
   * @return Element beginning at the provided index.
   */
  public T getBeginsAt(int index) {
    T element = smallestFinder.findAt(index);
    if ((element != null) && (element.getBeginIndex() == index)) {
      return element;
    }
    return null;
  }

  /**
   * @param index Index in the contents.
   * @return Element ending at the provided index.
   */
  public T getEndsAt(int index) {
    T element = smallestFinder.findAt(index - 1);
    if ((element != null) && (element.getEndIndex() == index)) {
      return element;
    }
    return null;
  }
}
