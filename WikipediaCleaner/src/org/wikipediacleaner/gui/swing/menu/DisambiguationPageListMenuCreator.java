/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.menu;

import java.util.Properties;

import javax.swing.JPopupMenu;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.action.MarkBacklinkAction;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;


/**
 * A helper class to manage contextual menu.
 */
public class DisambiguationPageListMenuCreator extends BasicMenuCreator {

  /**
   * Add items for marking backlinks.
   * 
   * @param wiki Wiki.
   * @param popup Popup menu.
   * @param page Page.
   * @param link Backlink.
   * @param backlinks Backlinks properties.
   */
  public void addItemsMarkBacklink(
      EnumWikipedia wiki, JPopupMenu popup,
      Page page, Page link, Properties backlinks) {
    if ((page != null) &&
        (link != null) &&
        (wiki != null) &&
        (backlinks != null)) {
      String property = backlinks.getProperty(link.getTitle());
      if (!Configuration.VALUE_PAGE_NORMAL.equals(property)) {
        addItem(
            popup, null, GT._T("Mark backlink as normal"), true,
            new MarkBacklinkAction(wiki, page, link, Configuration.VALUE_PAGE_NORMAL, backlinks));
      }
      if (!Configuration.VALUE_PAGE_HELP_NEEDED.equals(property)) {
        addItem(
            popup, null, GT._T("Mark backlink as needing help"), true,
            new MarkBacklinkAction(wiki, page, link, Configuration.VALUE_PAGE_HELP_NEEDED, backlinks));
      }
      if ((Configuration.VALUE_PAGE_NORMAL.equals(property)) ||
          (Configuration.VALUE_PAGE_HELP_NEEDED.equals(property))) {
        addItem(
            popup, null, GT._T("Remove mark on backlink"), true,
            new MarkBacklinkAction(wiki, page, link, null, backlinks));
      }
    }
  }
}
