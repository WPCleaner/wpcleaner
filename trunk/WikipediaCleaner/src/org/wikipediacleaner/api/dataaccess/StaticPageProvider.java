/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.dataaccess;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;


/**
 * A static page provider (always returns the same page).
 */
public class StaticPageProvider implements PageProvider {

  /** Page to provide. */
  private final Page page;

  /**
   * @param page Page to provide.
   */
  public StaticPageProvider(Page page) {
    this.page = page;
  }

  /**
   * @return Page.
   * @see org.wikipediacleaner.api.dataaccess.PageProvider#getPage()
   */
  @Override
  public Page getPage() {
    return page;
  }

  /**
   * @return Wiki.
   * @see org.wikipediacleaner.api.dataaccess.WikiProvider#getWiki()
   */
  @Override
  public EnumWikipedia getWiki() {
    if (page != null) {
      return page.getWikipedia();
    }
    return null;
  }

}
