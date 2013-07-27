/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import java.util.Properties;

import javax.swing.JList;
import javax.swing.JPopupMenu;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.menu.DisambiguationPageListMenuCreator;


/**
 * A popup menu listener for Page lists. 
 */
public class DisambiguationPageListPopupListener extends
    AbstractPageListPopupListener {

  private Properties backlinksProperties;

  /**
   * @param wiki Wiki
   * @param textPane Text pane.
   * @param list List.
   * @param window Window.
   */
  public DisambiguationPageListPopupListener(
      EnumWikipedia wiki,
      MWPane textPane, JList list,
      BasicWindow window) {
    super(wiki, textPane, list, window);
  }

  /**
   * @param properties Backlinks properties.
   */
  public void setBackLinksProperties(Properties properties) {
    backlinksProperties = properties;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.component.AbstractPageListPopupListener#createPopup(javax.swing.JPopupMenu, org.wikipediacleaner.api.data.Page)
   */
  @Override
  protected void createPopup(JPopupMenu popup, Page link) {
    DisambiguationPageListMenuCreator menu = new DisambiguationPageListMenuCreator();
    if (backlinksProperties != null) {
      menu.addSeparator(popup);
      menu.addItemsMarkBacklink(wikipedia, popup, page, link, backlinksProperties);
    }
    menu.addSeparator(popup);
    menu.addAnalyze(wikipedia, popup, link);
    menu.addView(wikipedia, popup, link, true);
    menu.addDisambiguation(wikipedia, popup, link);
    menu.addItemPurgeCache(wikipedia, popup, link, window);
  }

}
