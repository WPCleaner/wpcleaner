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
 * An action provider doing nothing.
 */
public class NullActionProvider implements ActionProvider {

  /**
   * @param element Text element.
   * @param textPane Text component.
   * @return Action.
   */
  @Override
  public Action getAction(Element element, JTextPane textPane) {
    return null;
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
