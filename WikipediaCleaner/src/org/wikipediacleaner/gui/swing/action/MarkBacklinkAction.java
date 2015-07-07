/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Properties;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.utils.Configuration;


/**
 * An action listener for analyzing templates of a page. 
 */
public class MarkBacklinkAction implements ActionListener {

  private final EnumWikipedia wikipedia;
  private final Page page;
  private final Page link;
  private final String mark;
  private final Properties backlinksProperties;

  /**
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param link Backlink.
   * @param mark Kind of mark.
   * @param backlinksProperties Backlinks properties.
   */
  public MarkBacklinkAction(
      EnumWikipedia wikipedia,
      Page page, Page link, String mark,
      Properties backlinksProperties) {
    this.wikipedia = wikipedia;
    this.page = page;
    this.link = link;
    this.mark = mark;
    this.backlinksProperties = backlinksProperties;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    if (mark == null) {
      backlinksProperties.remove(link.getTitle());
    } else {
      backlinksProperties.put(link.getTitle(), mark);
    }
    Configuration configuration = Configuration.getConfiguration();
    configuration.setSubProperties(
        wikipedia, Configuration.PROPERTIES_BACKLINKS, page.getTitle(), backlinksProperties);
  }
}
