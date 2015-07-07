/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.TemplateListWindow;


/**
 * An action listener for analyzing templates of a page. 
 */
public class TemplatesAnalysisAction implements ActionListener {

  private final Page page;
  private final Page link;
  private final EnumWikipedia wiki;

  public TemplatesAnalysisAction(Page page, Page link, EnumWikipedia wiki) {
    this.page = page;
    this.link = link;
    this.wiki = wiki;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    TemplateListWindow.createTemplateListWindow(page, link, wiki);
  }
}
