/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Builder for a finder for a type of element.
 */
public class ContentsFinderBuilder<T extends ContentsElement> {

  /** Comparator to sort the list of elements */
  private static final IntervalComparator comparator = new IntervalComparator();

  /** Elements */
  private final List<T> elements;

  /**
   * Constructor.
   */
  ContentsFinderBuilder() {
    this.elements = new ArrayList<>();
  }

  /**
   * Add a collection of elements.
   * 
   * @param newElements Collection of new elements.
   * @return Builder.
   */
  ContentsFinderBuilder<T> addAll(List<T> newElements) {
    if (newElements != null) {
      this.elements.addAll(newElements);
    }
    return this;
  }

  /**
   * Create a list of finder elements.
   * 
   * @param useSmallest Flag for specifying if the finder should search for the smallest element.
   * @return List of finder elements.
   */
  List<ContentsFinderElement<T>> createFinderElements(boolean useSmallest) {
    List<ContentsFinderElement<T>> result = new ArrayList<>();
    Collections.sort(elements, comparator);
    if (useSmallest) {
      fillSmallestFinderElement(result);
    } else {
      fillLargestFinderElement(result);
    }
    return result;
  }

  /**
   * Fill the list of finder elements with the smallest elements.
   * 
   * @param result List to be filled.
   */
  private void fillSmallestFinderElement(List<ContentsFinderElement<T>> result) {
    int lastEndIndex = 0;
    for (T element : elements) {
      if (element.getBeginIndex() >= lastEndIndex) {
        if (element.getBeginIndex() > lastEndIndex) {
          result.add(new ContentsFinderElement<>(lastEndIndex, element.getBeginIndex(), (T) null));
        }
        lastEndIndex = element.getEndIndex();
        result.add(new ContentsFinderElement<>(element.getBeginIndex(), lastEndIndex, element));
      } else if (element.getEndIndex() > lastEndIndex) {
        result.add(new ContentsFinderElement<>(lastEndIndex, element.getEndIndex(), element));
        lastEndIndex = element.getEndIndex();
      } else {
        ContentsFinderElement<T> previousElement = result.get(result.size() - 1);
        result.remove(result.size() - 1);
        if (element.getBeginIndex() > previousElement.getBeginIndex()) {
          result.add(new ContentsFinderElement<>(previousElement.getBeginIndex(), element.getBeginIndex(), previousElement.getElement()));
        }
        result.add(new ContentsFinderElement<>(element.getBeginIndex(), element.getEndIndex(), element));
        if (element.getEndIndex() < previousElement.getEndIndex()) {
          result.add(new ContentsFinderElement<>(element.getEndIndex(), previousElement.getEndIndex(), previousElement.getElement()));
        }
      }
      result.add(new ContentsFinderElement<>(lastEndIndex, Integer.MAX_VALUE, (T) null));
    }
  }

  /**
   * Fill the list of finder elements with the largest elements.
   * 
   * @param result List to be filled.
   */
  private void fillLargestFinderElement(List<ContentsFinderElement<T>> result) {
    int lastEndIndex = 0;
    for (T element : elements) {
      if (element.getBeginIndex() >= lastEndIndex) {
        if (element.getBeginIndex() > lastEndIndex) {
          result.add(new ContentsFinderElement<>(lastEndIndex, element.getBeginIndex(), (T) null));
        }
        lastEndIndex = element.getEndIndex();
        result.add(new ContentsFinderElement<>(element.getBeginIndex(), lastEndIndex, element));
      } else if (element.getEndIndex() > lastEndIndex) {
        result.add(new ContentsFinderElement<>(lastEndIndex, element.getEndIndex(), element));
        lastEndIndex = element.getEndIndex();
      }
    }
    result.add(new ContentsFinderElement<>(lastEndIndex, Integer.MAX_VALUE, (T) null));
  }
}
