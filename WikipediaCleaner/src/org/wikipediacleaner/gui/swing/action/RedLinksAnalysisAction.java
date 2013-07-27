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
import org.wikipediacleaner.gui.swing.Controller;
import org.wikipediacleaner.gui.swing.component.MWPane;


/**
 * An action listener for viewing page. 
 */
public class RedLinksAnalysisAction implements ActionListener {

  private final Page page;
  private final MWPane textPane;
  private final EnumWikipedia wiki;

  public RedLinksAnalysisAction(
      Page page, MWPane textPane, EnumWikipedia wiki) {
    this.page = page;
    this.textPane = textPane;
    this.wiki = wiki;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    Controller.runRedLinksAnalysis(page, textPane, wiki);
  }
}
