/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.ProgressPanel;
import org.wikipediacleaner.i18n.GT;


/**
 * An action listener for reloading members of a category. 
 */
public class ReloadCategoryMembersAction implements ActionListener {

  private final EnumWikipedia wiki;
  private final Page category;
  private final BasicWindow window;

  /**
   * Constructor.
   * 
   * @param wiki Wiki.
   * @param category Category.
   * @param window Window on which the action is done.
   */
  public ReloadCategoryMembersAction(
      EnumWikipedia wiki,
      Page category,
      BasicWindow window) {
    this.wiki = wiki;
    this.category = category;
    this.window = window;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    ProgressPanel progressPanel = (window != null) ? window.getGlassPane() : null;
    try {
      if (progressPanel != null) {
        progressPanel.start();
        progressPanel.setText(GT._("Retrieving MediaWiki API"));
      }
      API api = APIFactory.getAPI();
      if (progressPanel != null) {
        progressPanel.setText(GT._("Retrieving members of a category"));
      }
      api.retrieveCategoryMembers(wiki, category, 0, true, Integer.MAX_VALUE);
    } catch (APIException ex) {
      //
    } finally {
      if (progressPanel != null) {
        progressPanel.stop();
      }
    }
  }
}
