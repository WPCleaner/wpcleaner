/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check;

import javax.swing.Action;
import javax.swing.JTextPane;
import javax.swing.text.Element;

import org.wikipediacleaner.gui.swing.action.ReplaceTextAction;


/**
 * An action provider for ReplaceTextAction.
 */
class ReplaceTextActionProvider implements ActionProvider {

  private final String newText;

  private final boolean automatic;

  /**
   * @param newText New text.
   */
  ReplaceTextActionProvider(String newText) {
    this(newText, false);
  }

  /**
   * @param newText New text.
   * @param automatic True if the replacement can be done automatically.
   */
  ReplaceTextActionProvider(String newText, boolean automatic) {
    this.newText = newText;
    this.automatic = automatic;
  }

  /**
   * @return New text.
   */
  public String getNewText() {
    return newText;
  }

  /**
   * @return True if the replacement can be done automatically.
   */
  public boolean isAutomatic() {
    return automatic;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.ActionProvider#getAction(javax.swing.text.Element, javax.swing.JTextPane)
   */
  public Action getAction(Element element, JTextPane textPane) {
    return new ReplaceTextAction(newText, element, textPane);
  }

  /**
   * @param text New text.
   * @return True if this action can give this new text.
   */
  public boolean isPossibleReplacement(String text) {
    return (text != null) && (text.equals(newText));
  }
}
