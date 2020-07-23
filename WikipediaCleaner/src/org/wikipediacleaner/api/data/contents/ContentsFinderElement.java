/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents;


/**
 * Finder element for a type of element.
 */
class ContentsFinderElement<T extends ContentsElement> extends ContentsInterval {

  /** Element for the finder */
  private final T element;

  /**
   * @param beginIndex Begin index of the interval.
   * @param endIndex End index of the interval.
   * @param element Element.
   */
  ContentsFinderElement(int beginIndex, int endIndex, T element) {
    super(beginIndex, endIndex);
    this.element = element;
  }

  /**
   * @return Element.
   */
  public T getElement() {
    return element;
  }
}
