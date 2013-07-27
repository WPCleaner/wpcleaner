/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import javax.swing.JList;
import javax.swing.JPopupMenu;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.menu.BasicMenuCreator;


/**
 * A popup menu listener for Page lists. 
 */
public class BasicPageListPopupListener extends
    AbstractPageListPopupListener {

  /**
   * @param wiki Wiki
   * @param textPane Text pane.
   * @param list List.
   * @param window Window.
   */
  public BasicPageListPopupListener(
      EnumWikipedia wiki,
      MWPane textPane, JList list,
      BasicWindow window) {
    super(wiki, textPane, list, window);
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.component.AbstractPageListPopupListener#createPopup(javax.swing.JPopupMenu, org.wikipediacleaner.api.data.Page)
   */
  @Override
  protected void createPopup(JPopupMenu popup, Page link) {
    BasicMenuCreator menu = new BasicMenuCreator();
    menu.addSeparator(popup);
    menu.addAnalyze(wikipedia, popup, link);
    menu.addView(wikipedia, popup, link, true);
    menu.addDisambiguation(wikipedia, popup, link);
  }

}
