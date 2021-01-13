/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.pagelist;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.ProgressionValue;
import org.wikipediacleaner.gui.swing.component.BooleanIconCellRenderer;
import org.wikipediacleaner.gui.swing.component.IconCellRenderer;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;


/**
 * A table model for a list of pages.
 */
public class PageListTableModel extends AbstractTableModel {

  private static final long serialVersionUID = 6219036518582006787L;

  private EnumWikipedia wiki;

  private List<Page> pages;

  private List<String> watchedPages;

  public final static int COLUMN_WATCHED = 0;
  public final static int COLUMN_PAGE = COLUMN_WATCHED + 1;
  public final static int COLUMN_DISAMBIGUATION = COLUMN_PAGE + 1;
  public final static int COLUMN_REDIRECT = COLUMN_DISAMBIGUATION + 1;
  public final static int COLUMN_BACKLINKS_MAIN = COLUMN_REDIRECT + 1;
  public final static int COLUMN_BACKLINKS_TEMPLATE = COLUMN_BACKLINKS_MAIN + 1;
  public final static int COLUMN_BACKLINKS_OTHER = COLUMN_BACKLINKS_TEMPLATE + 1;
  public final static int COLUMN_COMMENTS_TEXT = COLUMN_BACKLINKS_OTHER + 1;
  public final static int NB_COLUMNS = COLUMN_COMMENTS_TEXT + 1;

  public PageListTableModel(EnumWikipedia wiki, List<Page> pages) {
    this.wiki = wiki;
    this.pages = pages;
    updateWatchedPages();
  }

  /**
   * Configure a column model.
   * 
   * @param model Column model.
   */
  public void configureColumnModel(TableColumnModel model) {
    TableColumn column;

    column = model.getColumn(COLUMN_BACKLINKS_MAIN);
    column.setMinWidth(50);
    column.setPreferredWidth(50);
    column.setMaxWidth(100);

    column = model.getColumn(COLUMN_BACKLINKS_OTHER);
    column.setMinWidth(50);
    column.setPreferredWidth(50);
    column.setMaxWidth(100);

    column = model.getColumn(COLUMN_BACKLINKS_TEMPLATE);
    column.setMinWidth(40);
    column.setPreferredWidth(40);
    column.setMaxWidth(100);

    column = model.getColumn(COLUMN_COMMENTS_TEXT);
    column.setMinWidth(60);

    column = model.getColumn(COLUMN_DISAMBIGUATION);
    column.setMinWidth(20);
    column.setPreferredWidth(20);
    column.setMaxWidth(20);
    column.setCellRenderer(
        new BooleanIconCellRenderer("commons-disambig-colour.png", null));
    column.setHeaderRenderer(
        new IconCellRenderer("commons-disambig-colour.png"));

    column = model.getColumn(COLUMN_REDIRECT);
    column.setMinWidth(20);
    column.setPreferredWidth(20);
    column.setMaxWidth(20);
    column.setCellRenderer(
        new BooleanIconCellRenderer("commons-redirect-arrow-without-text.png", null));
    column.setHeaderRenderer(
        new IconCellRenderer("commons-redirect-arrow-without-text.png"));

    column = model.getColumn(COLUMN_PAGE);
    column.setMinWidth(100);
    column.setPreferredWidth(200);

    column = model.getColumn(COLUMN_WATCHED);
    column.setMinWidth(20);
    column.setPreferredWidth(20);
    column.setMaxWidth(20);
    column.setCellRenderer(
        new BooleanIconCellRenderer("gnome-logviewer.png", null));
    column.setHeaderRenderer(
        new IconCellRenderer("gnome-logviewer.png"));
  }

  public void updateWatchedPages() {
    Configuration config = Configuration.getConfiguration();
    watchedPages = config.getStringList(wiki, Configuration.ARRAY_WATCH_PAGES);
  }

  /**
   * @param rows Rows numbers.
   * @return Corresponding pages.
   */
  public List<Page> getPages(int[] rows) {
    if ((rows != null) && (rows.length > 0)) {
      List<Page> result = new ArrayList<>();
      for (int i = 0; i < rows.length; i++) {
        result.add(pages.get(rows[i]));
      }
      return result;
    }
    return null;
  }

  /**
   * @return List of pages.
   */
  public List<Page> getPages() {
    return pages;
  }

  /**
   * @param row Row number.
   * @return Page.
   */
  public Page getPage(int row) {
    if ((row < 0) || (row >= pages.size())) {
      return null;
    }
    return pages.get(row);
  }
  /**
   * @param pageList List of pages to remove.
   */
  public void removePages(Collection<Page> pageList) {
    for (Page page : pageList) {
      pages.remove(page);
    }
    fireTableDataChanged();
  }

  /**
   * @param page Page to add.
   */
  public void addPage(Page page) {
    pages.add(page);
    Collections.sort(pages);
    fireTableDataChanged();
  }

  /* (non-Javadoc)
   * @see javax.swing.table.TableModel#getColumnCount()
   */
  @Override
  public int getColumnCount() {
    return NB_COLUMNS;
  }

  /* (non-Javadoc)
   * @see javax.swing.table.TableModel#getRowCount()
   */
  @Override
  public int getRowCount() {
    return (pages != null) ? pages.size() : 0;
  }

  /* (non-Javadoc)
   * @see javax.swing.table.TableModel#getValueAt(int, int)
   */
  @Override
  public Object getValueAt(int rowIndex, int columnIndex) {
    if ((pages != null) && (rowIndex >= 0) && (rowIndex < pages.size())) {
      Page page = pages.get(rowIndex);
      switch (columnIndex) {
      case COLUMN_BACKLINKS_MAIN:
        return page.getBacklinksProgressionInMainNamespace();
      case COLUMN_BACKLINKS_TEMPLATE:
        return page.getBacklinksProgressionInTemplateNamespace();
      case COLUMN_BACKLINKS_OTHER:
        return page.getBacklinksProgressionInOtherNamespaces();
      case COLUMN_COMMENTS_TEXT:
        if (page.getComment() != null) {
          return page.getComment().getComment();
        }
        return null;
      case COLUMN_DISAMBIGUATION:
        return page.isDisambiguationPage();
      case COLUMN_PAGE:
        return page.getTitle();
      case COLUMN_REDIRECT:
        return page.getRedirects().isRedirect();
      case COLUMN_WATCHED:
        return watchedPages.contains(page.getTitle());
      }
    }
    return null;
  }

  /* (non-Javadoc)
   * @see javax.swing.table.AbstractTableModel#getColumnName(int)
   */
  @Override
  public String getColumnName(int column) {
    switch (column) {
    case COLUMN_BACKLINKS_MAIN:
      return GT._T("Main");
    case COLUMN_BACKLINKS_TEMPLATE:
      return "{{.}}";
    case COLUMN_BACKLINKS_OTHER:
      return GT._T("Other");
    case COLUMN_COMMENTS_TEXT:
      return GT._T("Comments");
    case COLUMN_DISAMBIGUATION:
      return "D";
    case COLUMN_PAGE:
      return GT._T("Page");
    case COLUMN_REDIRECT:
      return "R";
    case COLUMN_WATCHED:
      return "";
    }
    return super.getColumnName(column);
  }

  /* (non-Javadoc)
   * @see javax.swing.table.AbstractTableModel#getColumnClass(int)
   */
  @Override
  public Class<?> getColumnClass(int columnIndex) {
    switch (columnIndex) {
    case COLUMN_BACKLINKS_MAIN:
      return ProgressionValue.class;
    case COLUMN_BACKLINKS_TEMPLATE:
      return ProgressionValue.class;
    case COLUMN_BACKLINKS_OTHER:
      return ProgressionValue.class;
    case COLUMN_COMMENTS_TEXT:
      return String.class;
    case COLUMN_DISAMBIGUATION:
      return Boolean.class;
    case COLUMN_PAGE:
      return String.class;
    case COLUMN_REDIRECT:
      return Boolean.class;
    case COLUMN_WATCHED:
      return Boolean.class;
    }
    return super.getColumnClass(columnIndex);
  }

}
