/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import javax.swing.AbstractListModel;
import javax.swing.JLabel;

import org.wikipediacleaner.api.data.CompositeComparator;
import org.wikipediacleaner.api.data.InternalLinkCount;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.i18n.GT;


/**
 * A list model for manipulating MediaWiki pages.
 */
public class PageListModel extends AbstractListModel<Page> {

  private static final long serialVersionUID = 8914341302198128905L;

  private boolean showDisambiguation;
  private boolean showMissing;
  private boolean showOther;
  private boolean showRedirect;
  private List<String> filterNamespaces;

  private PageAnalysis analysis;
  private boolean countDisambiguation;
  private boolean countMissing;
  private boolean countOther;
  private boolean countRedirect;

  private ArrayList<Page> fullList;
  private ArrayList<Page> filteredList;

  private JLabel linkCount;
  private CompositeComparator<Page> comparator;

  /**
   * Construct a list model for pages.
   */
  public PageListModel() {
    super();
    fullList = new ArrayList<Page>();
    filteredList = new ArrayList<Page>();
    showDisambiguation = true;
    showMissing = false;
    showOther = false;
    showRedirect = false;
  }

  /**
   * @param linkCount Label used to display link count.
   */
  public void setLinkCountLabel(JLabel linkCount) {
    this.linkCount = linkCount;
    updateLinkCount();
  }

  /**
   * @param comparator Comparator used to sort the list.
   */
  public void setComparator(CompositeComparator<Page> comparator) {
    this.comparator = comparator;
    Collections.sort(fullList, this.comparator);
    updateStatus();
  }

  /* (non-Javadoc)
   * @see javax.swing.ListModel#getElementAt(int)
   */
  @Override
  public Page getElementAt(int index) {
    if ((index >= 0) && (index < filteredList.size())) {
      return filteredList.get(index);
    }
    return null;
  }

  /* (non-Javadoc)
   * @see javax.swing.ListModel#getSize()
   */
  @Override
  public int getSize() {
    return filteredList.size();
  }

  /**
   * Resets the list to a given list.
   * 
   * @param elements All elements.
   */
  public void setElements(Collection<Page> elements) {
    clear();
    if ((elements == null) || (elements.isEmpty())) {
      return;
    }
    fullList.addAll(elements);
    if (comparator != null) {
      Collections.sort(fullList, comparator);
    }
    for (int i = 0; i < fullList.size(); i++) {
      Page page = fullList.get(i);
      if (filterPage(page)) {
        filteredList.add(page);
      }
    }
    if (!filteredList.isEmpty()) {
      fireIntervalAdded(this, 0, filteredList.size() - 1);
    }
  }

  /**
   * Adds the specified component to the end of this list.
   *  
   * @param element The component to be added.
   */
  public void addElement(Object element) {
    if (element instanceof Page) {
      Page page = (Page) element;
      boolean added = false;
      if (comparator != null) {
        for (int i = 0; !added && (i < fullList.size()); i++) {
          if (comparator.compare(page, fullList.get(i)) < 0) {
            fullList.add(i, page);
            added = true;
          }
        }
      }
      if (!added) {
        fullList.add(page);
      }
      if (filterPage(page)) {
        int index = 0;
        added = false;
        if (comparator != null) {
          for (int i = 0; !added && (i < filteredList.size()); i++) {
            if (comparator.compare(page, filteredList.get(i)) < 0) {
              filteredList.add(i, page);
              index = i;
              added = true;
           }
          }
        }
        if (!added) {
          index = filteredList.size();
          filteredList.add(page);
        }
        fireIntervalAdded(this, index, index);
        updateLinkCount();
      }
    }
  }

  /**
   * Removes all of the elements from this list.  The list will
   * be empty after this call returns (unless it throws an exception).
   */
  public void clear() {
    int index = filteredList.size() - 1;
    filteredList.clear();
    fullList.clear();
    if (index >= 0) {
      fireIntervalRemoved(this, 0, index);
      updateLinkCount();
    }
  }

  /**
   * Filters pages depending on the current options.
   * 
   * @param page Page.
   * @return Flag indicating if the page should be displayed.
   */
  private boolean filterPage(Page page) {
    if (page == null) {
      return false;
    }
    if ((page.getNamespace() != null) && (filterNamespaces != null)) {
      if (filterNamespaces.contains(page.getNamespace().toString())) {
        return false;
      }
    }
    if (!Boolean.FALSE.equals(page.isDisambiguationPage()) && showDisambiguation) {
      return true;
    }
    if (!Boolean.TRUE.equals(page.isExisting()) && showMissing) {
      return true;
    }
    if (!Boolean.FALSE.equals(page.isRedirect()) && showRedirect) {
      return true;
    }
    if (!Boolean.TRUE.equals(page.isDisambiguationPage()) &&
        !Boolean.FALSE.equals(page.isExisting()) &&
        !Boolean.TRUE.equals(page.isRedirect()) &&
        showOther) {
      return true;
    }
    return false;
  }

  public void setPageAnalysis(PageAnalysis analysis) {
    this.analysis = analysis;
  }

  public boolean getShowDisambiguation() {
    return showDisambiguation;
  }

  public void setShowDisambiguation(boolean show) {
    if (show != showDisambiguation) {
      showDisambiguation = show;
      updateStatus();
    }
  }

  public boolean getShowMissing() {
    return showMissing;
  }

  public void setShowMissing(boolean show) {
    if (show != showMissing) {
      showMissing = show;
      updateStatus();
    }
  }

  public boolean getShowOther() {
    return showOther;
  }

  public void setShowOther(boolean show) {
    if (show != showOther) {
      showOther = show;
      updateStatus();
    }
  }

  public boolean getShowRedirect() {
    return showRedirect;
  }

  public void setShowRedirect(boolean show) {
    if (show != showRedirect) {
      showRedirect = show;
      updateStatus();
    }
  }

  public void setFilterNamespace(List<String> filter) {
    this.filterNamespaces = filter;
    updateStatus();
  }

  public boolean getCountDisambiguation() {
    return countDisambiguation;
  }

  public void setCountDisambiguation(boolean count) {
    if (count != countDisambiguation) {
      countDisambiguation = count;
      updateStatus();
    }
  }

  public boolean getCountMissing() {
    return countMissing;
  }

  public void setCountMissing(boolean count) {
    if (count != countMissing) {
      countMissing = count;
      updateStatus();
    }
  }

  public boolean getCountOther() {
    return countOther;
  }

  public void setCountOther(boolean count) {
    if (count != countOther) {
      countOther = count;
      updateStatus();
    }
  }

  public boolean getCountRedirect() {
    return countRedirect;
  }

  public void setCountRedirect(boolean count) {
    if (count != countRedirect) {
      countRedirect = count;
      updateStatus();
    }
  }

  /**
   * Update model depending on the options. 
   */
  void updateStatus() {
    // Clean the existing list
    int index = filteredList.size() - 1;
    filteredList.clear();
    if (index >= 0) {
      fireIntervalRemoved(this, 0, index);
    }

    // Update the new list
    for (int i = 0; i < fullList.size(); i++) {
      if (filterPage(fullList.get(i))) {
        filteredList.add(fullList.get(i));
      }
      if (filteredList.size() > 0) {
        fireIntervalAdded(this, 0, filteredList.size() - 1);
      }
    }

    updateLinkCount();
  }

  /**
   * Update link count.
   */
  public void updateLinkCount() {
    if ((linkCount != null) && (analysis != null)) {
      int totalCount = 0;
      int mainCount = 0;
      analysis.countLinks(filteredList);
      for (Page p : filteredList) {
        if (p != null) {
          InternalLinkCount count = analysis.getLinkCount(p);
          if (count != null) {
            totalCount += count.getTotalLinkCount();
            if ((p.getNamespace() != null) &&
                (Namespace.MAIN == p.getNamespace().intValue())) {
              mainCount += count.getTotalLinkCount();
            }
          }
        }
      }
      if (totalCount > 0) {
        String message =
            GT.__(
                "{0} link in Main Namespace", "{0} links in Main Namespace",
                mainCount, Integer.toString(mainCount)) +
            " / " +
            GT.__(
                "{0} total link", "{0} total links",
                totalCount, Integer.toString(totalCount));
        linkCount.setText(message);
      } else {
        linkCount.setText("");
      }
    }
  }
}
