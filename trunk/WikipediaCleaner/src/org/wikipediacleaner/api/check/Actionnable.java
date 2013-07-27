/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check;

import java.util.List;

import javax.swing.Action;
import javax.swing.JTextPane;
import javax.swing.text.Element;


/**
 * A generic interface for grouping possible actions.
 */
public interface Actionnable {

  /**
   * @return Action name.
   */
  public String getName();

  /**
   * @return Flag indicating if it is composed action.
   */
  public boolean isCompositeAction();

  /**
   * @param newText New text.
   * @return True if this action can give this new text.
   */
  public boolean isPossibleReplacement(String newText);

  /**
   * @param element Text element.
   * @param textPane Text component.
   * @return Action.
   */
  public Action getAction(Element element, JTextPane textPane);

  /**
   * @return Actions.
   */
  public List<Actionnable> getActions();
}
