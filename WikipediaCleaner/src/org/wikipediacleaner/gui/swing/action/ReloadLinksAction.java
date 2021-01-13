/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.ProgressPanel;
import org.wikipediacleaner.i18n.GT;


/**
 * An action listener for reloading links of a page. 
 */
public class ReloadLinksAction implements ActionListener {

  private final EnumWikipedia wikipedia;
  private final Page page;
  private final BasicWindow window;

  /**
   * Constructor.
   * 
   * @param wikipedia Wikipedia.
   * @param page Wikipedia page.
   * @param window Window on which the action is done.
   */
  public ReloadLinksAction(
      EnumWikipedia wikipedia,
      Page page,
      BasicWindow window) {
    this.wikipedia = wikipedia;
    this.page = page;
    this.window = window;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    ProgressPanel progressPanel = (window != null) ? window.getGlassPane() : null;
    try {
      if (progressPanel != null) {
        progressPanel.start();
        progressPanel.setText(GT._T("Retrieving MediaWiki API"));
      }
      API api = APIFactory.getAPI();
      if (progressPanel != null) {
        progressPanel.setText(GT._T("Analyzing links for redirect pages"));
      }
      ArrayList<Page> pages = new ArrayList<>();
      pages.add(page);
      api.initializeRedirect(wikipedia, pages);
      if (progressPanel != null) {
        progressPanel.setText(GT._T("Analyzing links for disambiguation pages"));
      }
      api.initializeDisambiguationStatus(wikipedia, pages, false);
      Iterator<Page> iter = page.getRedirects().getIteratorWithPage();
      while (iter.hasNext()) {
        Page tmp = iter.next();
        if (progressPanel != null) {
          progressPanel.setText(GT._T(
              "Retrieving possible disambiguations for {0}",
              new Object[] { tmp.getTitle() } ));
        }
        api.retrieveLinks(wikipedia, Collections.singletonList(tmp));
      }
    } catch (APIException ex) {
      //
    } finally {
      if (progressPanel != null) {
        progressPanel.stop();
      }
    }
  }
}
