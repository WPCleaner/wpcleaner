/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import org.wikipediacleaner.api.data.CompositeComparator;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.component.PageListModel;


/**
 * An action listener for modifying sort order.
 */
public class SetComparatorAction implements ActionListener {

  private final PageListModel model;
  private final CompositeComparator<Page> comparator;

  /**
   * @param model Model for page list.
   * @param comparator Comparator.
   */
  public SetComparatorAction(
      PageListModel             model,
      CompositeComparator<Page> comparator) {
    this.model = model;
    this.comparator = comparator;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    if ((model != null) && (comparator != null)) {
      model.setComparator(comparator);
    }
  }

}
