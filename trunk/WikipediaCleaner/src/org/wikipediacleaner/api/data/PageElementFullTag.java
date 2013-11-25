/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data;


/**
   * Page element for storing a full tag.
 */
public class PageElementFullTag extends PageElement {

  /**
   * First tag of the full tag.
   */
  public final PageElementTag firstTag;

  /**
   * @param tag First tag of the full tag.
   */
  public PageElementFullTag(PageElementTag tag) {
    super(tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
    this.firstTag = tag;
  }
}
