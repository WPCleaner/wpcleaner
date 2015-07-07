/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check;

import java.util.ArrayList;
import java.util.List;

import javax.swing.Action;
import javax.swing.JTextPane;
import javax.swing.text.Element;


/**
 * A composite action.
 */
public class CompositeAction implements Actionnable {

  private final String name;
  private final List<Actionnable> actions;

  /**
   * @param name Action name.
   */
  public CompositeAction(String name) {
    this(name, new ArrayList<Actionnable>());
  }

  /**
   * @param name Action name.
   * @param actions Actions.
   */
  public CompositeAction(String name, List<Actionnable> actions) {
    this.name = name;
    this.actions = actions;
  }

  /**
   * Add an action.
   * 
   * @param action Action.
   */
  public void addAction(Actionnable action) {
    if (action != null) {
      actions.add(action);
    }
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.Actionnable#getAction(javax.swing.text.Element, javax.swing.JTextPane)
   */
  @Override
  public Action getAction(
      @SuppressWarnings("unused") Element element,
      @SuppressWarnings("unused") JTextPane textPane) {
    return null;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.Actionnable#getActions()
   */
  @Override
  public List<Actionnable> getActions() {
    return actions;
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
    return true;
  }

  /**
   * @param newText New text.
   * @return True if this action can give this new text.
   */
  @Override
  public boolean isPossibleReplacement(String newText) {
    if (actions == null) {
      return false;
    }
    for (Actionnable action : actions) {
      if (action.isPossibleReplacement(newText)) {
        return true;
      }
    }
    return false;
  }
}
