/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import java.awt.event.ActionEvent;

import org.wikipediacleaner.api.data.PageElementTitle;


/**
 * An Action that opens the Table of Contents pane.
 */
public class MWPaneEditTocAction extends MWPaneAction {

  /** Serialization */
  private static final long serialVersionUID = -8884273905628744049L;

  /** Title to be selected when opening the TOC */
  private final PageElementTitle title;

  /**
   * Constructor.
   * 
   * @param title Title to be selected.
   */
  public MWPaneEditTocAction(PageElementTitle title) {
    super("MWPaneEditTocAction");
    this.title = title;
  }

  /**
   * @param e
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    MWPane textPane = getMWPane(e);
    if (textPane == null) {
      return;
    }
    textPane.displayToc(title);
  }
}
