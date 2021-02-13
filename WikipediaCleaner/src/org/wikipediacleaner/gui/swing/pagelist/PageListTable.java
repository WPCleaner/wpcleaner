/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.pagelist;

import java.util.List;
import java.util.Map;

import javax.swing.JTable;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.ProgressionValue;
import org.wikipediacleaner.api.data.page.PageComment;
import org.wikipediacleaner.api.dataaccess.PageListProvider;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.ProgressionValueCellRenderer;


/**
 * A table containing a list of pages.
 */
public class PageListTable extends JTable implements PageListProvider {

  /** Serialization */
  private static final long serialVersionUID = 9187589516812365281L;

  /** Wiki */
  private final EnumWikipedia wiki;

  /** Underlying model for the table */
  private final PageListTableModel model;

  /**
   * Create a table for a list of pages.
   * 
   * @param wiki Wiki.
   * @param pages List of pages.
   * @param commentsByTitle Page comments indexed by page title.
   * @return Table.
   */
  public static PageListTable createTable(
      EnumWikipedia wiki,
      List<Page> pages,
      Map<String, PageComment> commentsByTitle) {
    PageListTableModel model = new PageListTableModel(wiki, pages, commentsByTitle);
    PageListTable table = new PageListTable(wiki, model);
    table.setDefaultRenderer(ProgressionValue.class, new ProgressionValueCellRenderer());
    model.configureColumnModel(table.getColumnModel());
    Utilities.addRowSorter(table, model);
    table.addMouseListener(new PageListMouseListener());
    return table;
  }

  /**
   * @param wiki Wiki.
   * @param model Underlying model for the table.
   */
  private PageListTable(
      EnumWikipedia wiki,
      PageListTableModel model) {
    super(model);
    this.model = model;
    this.wiki = wiki;
  }

  /**
   * @return Wiki
   * @see org.wikipediacleaner.api.dataaccess.WikiProvider#getWiki()
   */
  @Override
  public EnumWikipedia getWiki() {
    return wiki;
  }

  /**
   * @return List of selected pages
   * @see org.wikipediacleaner.api.dataaccess.PageListProvider#getPages()
   */
  @Override
  public List<Page> getPages() {
    return getSelectedPages();
  }

  /**
   * @return List of selected pages
   */
  public List<Page> getSelectedPages() {
    int[] rows = getSelectedRows();
    for (int i = 0; i < rows.length; i++) {
      rows[i] = Utilities.convertRowIndexToModel(this, rows[i]);
    }
    List<Page> result = model.getPages(rows);
    return result;
  }

  /**
   * Remove pages.
   * 
   * @param pages List of pages.
   */
  public void removePages(List<Page> pages) {
    model.removePages(pages);
  }

  /**
   * @param page Page to be added.
   */
  public void addPage(Page page) {
    model.addPage(page);
  }

  /**
   * Update information about watched pages.
   */
  public void updateWatchedPages() {
    model.updateWatchedPages();
  }
}
