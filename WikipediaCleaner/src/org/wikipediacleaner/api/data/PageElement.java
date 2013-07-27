/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;


/**
 * Class containing information about an element of the page.
 */
public abstract class PageElement {

  private final int beginIndex;
  private final int endIndex;

  /**
   * @param beginIndex Beginning of the page element.
   * @param endIndex End of the page element.
   */
  public PageElement(int beginIndex, int endIndex) {
    this.beginIndex = beginIndex;
    this.endIndex = endIndex;
  }

  /**
   * @return Beginning of the page element.
   */
  public int getBeginIndex() {
    return beginIndex;
  }

  /**
   * @return End of the page element.
   */
  public int getEndIndex() {
    return endIndex;
  }
}
