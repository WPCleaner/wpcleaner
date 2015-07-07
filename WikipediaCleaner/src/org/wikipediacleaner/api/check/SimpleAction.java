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
 * A simple action.
 */
public class SimpleAction implements Actionnable {

  private final String name;
  private final ActionProvider actionProvider;
  private final Action action;

  /**
   * @param name Action name.
   * @param actionProvider Action provider.
   */
  SimpleAction(String name, ActionProvider actionProvider) {
    this.name = name;
    this.actionProvider = actionProvider;
    this.action = null;
  }

  /**
   * @param name Action name.
   * @param action Action.
   */
  public SimpleAction(String name, Action action) {
    this.name = name;
    this.actionProvider = null;
    this.action = action;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.Actionnable#getAction(javax.swing.text.Element, javax.swing.JTextPane)
   */
  @Override
  public Action getAction(Element element, JTextPane textPane) {
    if (actionProvider != null) {
      return actionProvider.getAction(element, textPane);
    }
    return action;
  }

  /**
   * @return Action provider.
   */
  public ActionProvider getActionProvider() {
    return actionProvider;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.Actionnable#getActions()
   */
  @Override
  public List<Actionnable> getActions() {
    return null;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.Actionnable#getName()
   */
  @Override
  public String getName() {
    return name;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.Actionnable#isCompositeAction()
   */
  @Override
  public boolean isCompositeAction() {
    return false;
  }

  /**
   * @param newText New text.
   * @return True if this action can give this new text.
   */
  @Override
  public boolean isPossibleReplacement(String newText) {
    if (actionProvider == null) {
      return false;
    }
    return actionProvider.isPossibleReplacement(newText);
  }
}
