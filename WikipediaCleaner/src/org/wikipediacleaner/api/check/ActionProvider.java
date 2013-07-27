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
 * Provides an action.
 */
interface ActionProvider {

  /**
   * @param element Text element.
   * @param textPane Text component.
   * @return Action.
   */
  Action getAction(Element element, JTextPane textPane);

  /**
   * @param text New text.
   * @return True if this action can give this new text.
   */
  public boolean isPossibleReplacement(String text);
}
