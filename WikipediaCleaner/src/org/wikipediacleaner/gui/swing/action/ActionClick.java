/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.AbstractButton;


/**
 * Action for clicking a button.
 */
public class ActionClick extends AbstractAction {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = -1854239918495666556L;

  /**
   * Button.
   */
  private final AbstractButton button;

  /**
   * @param button Button.
   */
  public ActionClick(AbstractButton button) {
    this.button = button;
  }

  /**
   * @param e Event.
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if (button != null) {
      button.doClick();
    }
  }
}
