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
import org.wikipediacleaner.gui.swing.Controller;


/**
 * An action listener for analyzing a disambiguation page. 
 */
public class DisambiguationAnalysisAction implements ActionListener {

  private final String title;
  private final EnumWikipedia wiki;

  public DisambiguationAnalysisAction(String title, EnumWikipedia wiki) {
    this.title = title;
    this.wiki = wiki;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    Controller.runDisambiguationAnalysis(title, wiki);
  }
}
