/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.dataaccess;

import java.util.Collections;
import java.util.List;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;


/**
 * A static page provider (always returns the same list of pages).
 */
public class StaticPageListProvider implements PageListProvider {

  /** Page to provide. */
  private final List<Page> pages;

  /**
   * @param wiki Wiki.
   * @param title Title of the page to provide.
   */
  public StaticPageListProvider(EnumWikipedia wiki, String title) {
    this.pages = Collections.singletonList(DataManager.createSimplePage(wiki, title, null, null, null));
  }

  /**
   * @return List of pages.
   * @see org.wikipediacleaner.api.dataaccess.PageListProvider#getPages()
   */
  @Override
  public List<Page> getPages() {
    return pages;
  }

  /**
   * @return Wiki.
   * @see org.wikipediacleaner.api.dataaccess.WikiProvider#getWiki()
   */
  @Override
  public EnumWikipedia getWiki() {
    if ((pages != null) && !pages.isEmpty()) {
      return pages.get(0).getWikipedia();
    }
    return null;
  }

}
