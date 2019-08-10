/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2019  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.pagelist;

import java.util.List;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.dataaccess.PageListProvider;


/**
 * Provides the list of selected pages in a page list table as a PageListProvider.
 */
public class PageListTableSelectedPages implements PageListProvider {

  /** Actual table */
  private final PageListTable table;

  /**
   * Constructor.
   * 
   * @table Actual table.
   */
  public PageListTableSelectedPages(PageListTable table) {
    this.table = table;
  }

  /**
   * @return Wiki.
   * @see org.wikipediacleaner.api.dataaccess.WikiProvider#getWiki()
   */
  @Override
  public EnumWikipedia getWiki() {
    return table.getWiki();
  }

  /**
   * @return List of selected pages.
   * @see org.wikipediacleaner.api.dataaccess.PageListProvider#getPages()
   */
  @Override
  public List<Page> getPages() {
    return table.getSelectedPages();
  }

}
