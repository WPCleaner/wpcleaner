/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    if ((model != null) && (comparator != null)) {
      model.setComparator(comparator);
    }
  }

}
