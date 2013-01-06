/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.gui.swing;

import java.util.Collections;
import java.util.List;

import javax.swing.table.AbstractTableModel;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.ProgressionValue;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;


/**
 * 
 */
public class PageListTableModel extends AbstractTableModel {

  private static final long serialVersionUID = 6219036518582006787L;

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
    this.pages = pages;
    Configuration config = Configuration.getConfiguration();
    watchedPages = config.getStringList(wiki, Configuration.ARRAY_WATCH_PAGES);
  }

  /**
   * @param rows Rows numbers.
   * @return Corresponding pages.
   */
  public Page[] getPages(int[] rows) {
    if ((rows != null) && (rows.length > 0)) {
      Page[] array = new Page[rows.length];
      for (int i = 0; i < rows.length; i++) {
        array[i] = pages.get(rows[i]);
      }
      return array;
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
  public void removePages(Page[] pageList) {
    for (int i = 0; i < pageList.length; i++) {
      pages.remove(pageList[i]);
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
  public int getColumnCount() {
    return NB_COLUMNS;
  }

  /* (non-Javadoc)
   * @see javax.swing.table.TableModel#getRowCount()
   */
  public int getRowCount() {
    return (pages != null) ? pages.size() : 0;
  }

  /* (non-Javadoc)
   * @see javax.swing.table.TableModel#getValueAt(int, int)
   */
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
        return page.isRedirect();
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
      return GT._("Main");
    case COLUMN_BACKLINKS_TEMPLATE:
      return "{{.}}";
    case COLUMN_BACKLINKS_OTHER:
      return GT._("Other");
    case COLUMN_COMMENTS_TEXT:
      return GT._("Comments");
    case COLUMN_DISAMBIGUATION:
      return "D";
    case COLUMN_PAGE:
      return GT._("Page");
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
