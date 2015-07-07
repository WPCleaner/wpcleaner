/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import java.awt.event.ActionEvent;


/**
 * An Action that opens the Table of Contents pane.
 */
public class MWPaneEditTocAction extends MWPaneAction {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = -8884273905628744049L;

  /**
   * Constructor.
   */
  public MWPaneEditTocAction() {
    super("MWPaneEditTocAction");
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
    textPane.displayToc();
  }
}
