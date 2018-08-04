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


/**
 * A basic ActionProvider created with an action
 */
public class BasicActionProvider implements ActionProvider {

  private final Action action;

  /**
   * Constructor.
   * 
   * @param action Action.
   */
  public BasicActionProvider(Action action) {
    this.action = action;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.ActionProvider#getAction(javax.swing.text.Element, javax.swing.JTextPane)
   */
  @Override
  public Action getAction(
      @SuppressWarnings("unused") Element element,
      @SuppressWarnings("unused") JTextPane textPane) {
    return action;
  }

  /**
   * @param text New text.
   * @return True if this action can give this new text.
   */
  @Override
  public boolean isPossibleReplacement(String text) {
    return false;
  }

}
